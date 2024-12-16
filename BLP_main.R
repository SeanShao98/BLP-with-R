# EIO: BLP
# May 19, 22
# Author: GitHub@SeanShao98
# initializing ------------------------------------------------
setwd("~/***")
require(dplyr)
require(tidyr)
require(data.table)
require(magrittr)
require(stargazer)
require(readxl)
require(MASS)

rm(list = ls())
options(scipen = 10)
# Note: a_i = a + pi * inc_i + sigma * v_i; beta is constant para
# i.e., only income in D and 1 random alpha, thus one v;
#       pi and sigma are both scalar;
#       i in (1:R); j in (1:J); t in (1:T)
# R, J, nmarket are global variables

# -1: data & variables ------------------------------------------------
load("beer.RData") # data from "logit_main.R"
demo <- read_xlsx("DEMO.xlsx", sheet = 1) %>%
  `[`(!is.na(.$income), c(8, 16, 17)) %>%
  `names<-`(c("store", "inc", "inc_sd")) %>%
  setDT()
market <- expand(beer, store, week) %>% setDT() # 936 store * week
market <- demo[market, on = "store"]

# 0: draw consumers for each t (store * week) --------------------------------
R <- 20 #   GLOBALLY        |    draw: 20 consumers (R = 20)
J <- 11 #     SET           | product: 11
nmarket <- dim(market)[1] # |  market: T = 936

D <- apply(market, MARGIN = 1, function(x) {
  rnorm(R, x[2], x[3])
}) %>% t()
V <- matrix(rnorm(nmarket * R, 0, 1), ncol = R)
draws_beer <- list("D" = D, "V" = V)
# D_i (1*1): income
# V_i (1*1): only one unobserved variable

# D and V: 936 markets, 20 draws (consumers)
# If more than one variables in D, make D a list.

# 1: share calculation ------------------------------------------------
# 936 * 11 <-- 936 * 20 |--->>> for t in 936: 20 i choose 11 j
# R, J, nmarket are GLOBALLY SET above
share_cal <- 
  function(delta, rc, chrs){
    # delta (J*1); rc (K*R); chrs (J*K)                  <<== MATRIX
    # return: s_t (J*1). Given t, the predicted share.   ==>> VEC
    sapply(c(1:J), function(j) {
      exp(delta[j] + as.matrix(chrs[j, ]) %*% as.matrix(rc)) 
                                          # 1*R, exp component
      }) %>% t() %>%                      # J*R, exp component
      apply(MARGIN = 2, function(i) {
        i / (sum(i) + 1)}) %>%            # J*R, i's simulated prob of j
      apply(MARGIN = 1, mean) %>%         # J*1, j's simulated share at t
      return()
  }

share_cal_loopT <- 
  function(delta, coefs, draws, Chrs_T) {
    # nmarket = T; delta (JT*1); Chrs_T (JT * K);
    # coefs: Pi (K*d), Sigma (K*K); draws: D_d (T*R), V_k (T*R)  <<== MATRIX
    # return: simulated share of all t (JT*1)                    ==>> VEC
    sapply(c(1:nmarket), function(t) {
      share_cal(
        delta = as.matrix(delta[c((J * (t - 1) + 1):(t * J)), ]),
        rc = coefs[["Pi"]] %*% draws[["D"]][t, ] + # if K,D > 1, "D"=>"D_1"...
          coefs[["Sigma"]] %*% draws[["V"]][t, ],  # i.e. all D_d, V_k
        chrs = as.matrix(Chrs_T[c((J * (t - 1) + 1):(t * J)), ])
      )
    }, simplify = T) %>% c() %>%
      return()
  }
# Noted: the model here only allow K = D = 1.

# 2: contract mapping ------------------------------------------------
delta_cal <- 
  function(delta0, coefs, draws, share, Chrs_T){
    # for all j and t:
    # delta (JT*1); coefs; draws; Chrs_T   <<== MATRIX
    # return: delta_hat (JT*1)             ==>> VEC
    epsilon <- 100
    delta_old <- delta0
    h <- 1
    while (epsilon > exp(-2) & h <= 10000) {
      delta_new <- delta_old + log(share) - log(
        share_cal_loopT(delta_old,
                        coefs  = coefs,
                        draws  = draws,
                        Chrs_T = Chrs_T)
      )
      epsilon <- max(abs(delta_new - delta_old))
      delta_old <- delta_new
      h <- h + 1
    }
    if (h > 10000) {
      stop("No convergence found")
    } else {
      return(delta_new)
    }
  }

# 3: NLLS ------------------------------------------------
xi_sqr <-
  function(coef_vec, delta0, draws, Chrs_T, share, X, Z, edg) {
    # coef_vec ("Pi", "Sigma") (Kd + KK) :=> coefs (list);
    # X: more chrs, NULL if empty; Z: IVs    <<== MATRIX
    # return: xi^2 => sum => .               ==>> SCALAR
    K <- dim(Chrs_T)[2]
    L <- length(coef_vec)
    coefs <- list("Pi" = matrix(coef_vec[1:(L - K^2)], nrow = K),
                  "Sigma" = matrix(coef_vec[-c(1:(L - K^2))], nrow = K))
    delta_hat <- delta_cal(delta0, coefs, draws, share, Chrs_T)
    # 2SLS to obtain linear parameters
    # delta_hat (JT*1); Chrs_T (JT*K); X (JT*H); Z (JT*M); TSLS (K+H)
    # no constant term in X_f
    X_f <- cbind(Chrs_T, X)             # names of TSLS == names of X_f
    Z_f <- cbind(Chrs_T[, -edg], Z, X)  # endogenous columns in edg
    TSLS <<- ginv(t(X_f) %*% Z_f %*% ginv(t(Z_f) %*% Z_f) %*% t(Z_f) %*% X_f) %*%
      t(X_f) %*% Z_f %*% ginv(t(Z_f) %*% Z_f) %*% t(Z_f) %*% delta_hat
    xi <- delta_hat - X_f %*% TSLS
    message(paste0("Linear coefficient: ", paste(TSLS, collapse = ", ")))
    return(sum(xi ^ 2))
  }

# example: beer case ------------------------------------------------
t0 <- Sys.time()
BLP <- optim(
  c(1, 1), xi_sqr,
  delta0 = as.matrix(beer$logit_y),
  draws = draws_beer,
  Chrs_T = as.matrix(beer$price),
  share = as.matrix(beer$share),
  X = as.matrix(beer[, grep("^upc([1-9]|10)$", names(beer)), with = F]),
  Z = as.matrix(beer$wholesale),
  edg = 1L
)
fun_time <- difftime(Sys.time(), t0, units = "secs") %>% as.numeric()
BLP_coefs <- list(
  "Linear coefficients" = c(
    "Price" = TSLS[1], {TSLS[-1] %>% `names<-`(paste0("upc", c(1:10)))}
  ),
  "Nonlinear coefficients" = c(
    "$\\pi$" = BLP$par[1], "$\\sigma$" = BLP$par[2]
  )
)
# example: casual test ------------------------------------------------
R <- 3 # K = D = 1
J <- 2 # J = T = 2
nmarket <- 2
Chrs_T <- c(1, 1.3, 10, 0.3) %>% as.matrix()
draws <- list("D" = matrix(c(1, 3, 2, 1.8, 3.1, 0.99), ncol = 3), 
              "V" = matrix(c(1.1, 1.9, 1.8, 2.8, 0.1, 0.7), ncol = 3))
coefs <- list("Pi" = {data.table(c(1.6)) %>% as.matrix()}, 
              "Sigma" = {data.table(c(1.2)) %>% as.matrix()})
delta0 <- c(1.5, 1.2, 0.2, 1.7) %>% as.matrix()
share <- c(0.2, 0.8, 0.3, 0.7)
X <- matrix(c(1, 4, 2, 6, 4, 10, 10.7, 0.9), ncol = 2) # H = 2
Z <- c(14, 7.7, 6.3, 17) %>% as.matrix() # M = 1
edg <- c(1)
coef_vec <- unlist(coefs)

delta_1 <- delta_cal(delta0, coefs, draws, share, Chrs_T)
BLP <- optim(
  c(1, 1), xi_sqr,
  delta0 = delta0, draws = draws, Chrs_T = Chrs_T, share = share,
  X = X, Z = Z, edg = edg
)

tf <- function(x) {
  out <- (x[1] - 6) ^ 2 + (x[2] - 3) ^ 2
  x1 <<- x[1] - 4
  return(out)
}
tf_r <- optim(c(1, 1), tf)
