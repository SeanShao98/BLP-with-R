# EIO: Logit
# May 12, 22
# Author: GitHub@SeanShao98
# initializing ------------------------------------------------
setwd("~/***")
require(dplyr)
require(tidyr)
require(data.table)
require(magrittr)
require(stargazer)
require(readxl)

rm(list = ls())
options(scipen = 10)
beer <- read_xlsx("WBER.xlsx", sheet = 1) %>%
  `[`(, c(1:9)) %>%
  setDT()
# data ------------------------------------------------
beer[, upc := as.numeric(factor(upc))]
beer <- class.ind(beer$upc) %>%
  as.data.table() %>%
  `names<-`(paste0("upc", names(.))) %$%
  cbind(beer, .)
beer[, move_ttl := sum(move), by = c("store", "week")]
beer %<>%
  mutate(
    share = move/custcoun,
    share0 = 1 - move_ttl/custcoun,
    logit_y = log(share) - log(share0),
    wholesale = price * (1 - profit * 0.01)
  )
summary(beer[, c("move", "price", "profit", "custcoun")])
# reg ------------------------------------------------
logit_formula <-
  paste0("upc", c(1:10)) %>%
  paste(collapse = " + ") %>%
  paste0("logit_y ~ price +", .) %>%
  as.formula()
# logit <- lm(logit_formula, data = beer)
logit <- lm(
  logit_y ~ price + upc1 + upc2 + upc3 + upc4 + upc5 +
    upc6 + upc7 + upc8 + upc9 + upc10,
  data = beer)
logit_iv <- ivreg(
  logit_y ~ price + upc1 + upc2 + upc3 + upc4 + upc5 +
    upc6 + upc7 + upc8 + upc9 + upc10 | wholesale + 
    upc1 + upc2 + upc3 + upc4 + upc5 +
    upc6 + upc7 + upc8 + upc9 + upc10,
  data = beer
)
# I/O ------------------------------------------------
stargazer(
  beer[, c("move", "price", "profit", "custcoun")],
  summary = T, type = "latex", style = "aer", label = "summary",
  float = F,
  out = "./out/out_summary.tex"
)
stargazer(
  logit, logit_iv, 
  type = "latex", title = "Logit Results", align = T,
  float = F, label = "logit", no.space = T,
  dep.var.labels = "$\\ln(s_j)-\\ln(s_0)$",
  keep = "price",
  model.numbers = F, model.names = T,
  out = "./out/out_logit.tex"
)
save(beer, file = "beer.RData")
