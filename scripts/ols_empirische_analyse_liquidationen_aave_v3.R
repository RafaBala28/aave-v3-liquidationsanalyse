##############################################################
#   AAVE V3 – Liquidationsanalyse: OLS-Regression
#   Bachelorarbeit – Empirische Analyse
#   Autor: Rafael Balasteguim da Silva
#   Datum: 05.01.2026
##############################################################

##############################################################
# 0) LIBRARIES
##############################################################

library(tidyverse)
library(lubridate)
library(lmtest)
library(sandwich)
library(zoo)
library(modelsummary)
library(here)

##############################################################
# 1) BASISPFAD
##############################################################

setwd("C:/Users/rafae/Documents/UniBas/Bachelor Arbeit/aave-v3-liquidationsanalyse")

BASE_DIR <- getwd()
DATA_DIR <- "data"

##############################################################
# 2) LIQUIDATIONSDATEN LADEN UND AUFBEREITEN
##############################################################

liq_raw <- read_csv(
  file.path(DATA_DIR, "liquidations_master.csv"),
  col_types = cols(
    block = col_character(),
    timestamp = col_double(),
    collateralAsset = col_character(),
    debtAsset = col_character(),
    user = col_character(),
    liquidator = col_character(),
    collateralOut = col_double(),
    debtToCover = col_double(),
    collateralSymbol = col_character(),
    debtSymbol = col_character(),
    collateral_value_usd = col_double(),
    debt_value_usd = col_double(),
    gas_used = col_double(),
    gas_price_gwei = col_double(),
    eth_price_usd_at_block = col_double()
  )
)



##############################################################
# 3) ETH-PREISDATEN (CHAINLINK, DAILY)
##############################################################

eth_daily <- read_csv(
  file.path(DATA_DIR, "eth_chainlink_daily_aave_v3.csv")
) %>%
  mutate(date = as.Date(date_utc)) %>%
  arrange(date) %>%
  mutate(
    ret     = (eth_price_usd - lag(eth_price_usd)) / lag(eth_price_usd),
    neg_ret = pmin(ret, 0),
    pos_ret = pmax(ret, 0),
    vol_7d  = zoo::rollapply(ret, 7, sd, fill = NA, align = "right"),
    vol_14d = zoo::rollapply(ret, 14, sd, fill = NA, align = "right")
  )


##############################################################
# 4) TÄGLICHE AGGREGATION DER LIQUIDATIONEN
##############################################################

liq_raw <- liq_raw %>%
  mutate(
    date = as.Date(
      as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC")
    )
  )

liq_raw <- liq_raw %>%
  filter(date >= start_date)

liq_daily <- liq_raw %>%
  count(date, name = "liquidations")


##############################################################
# 5) MERGE: LIQUIDATIONEN & PREISDATEN
##############################################################

panel_daily <- liq_daily %>%
  left_join(eth_daily, by = "date") %>%
  filter(!is.na(ret)) %>%
  mutate(
    log_liquidations = log(liquidations + 1)
  )

##############################################################
# 6) HILFSFUNKTION: ROBUSTE STANDARDERRORS
##############################################################

robust_se <- function(model) {
  coeftest(model, vcov = vcovHC(model, type = "HC1"))
}

##############################################################
# 7) BASISMODELL
##############################################################

m_base <- lm(
  log_liquidations ~ neg_ret + pos_ret,
  data = panel_daily
)

robust_se(m_base)

##############################################################
# 8) VOLATILITÄTSMODELLE
##############################################################

m_vol7 <- lm(
  log_liquidations ~ neg_ret + pos_ret + vol_7d,
  data = panel_daily
)

m_vol14 <- lm(
  log_liquidations ~ neg_ret + pos_ret + vol_14d,
  data = panel_daily
)

robust_se(m_vol7)
robust_se(m_vol14)

##############################################################
# 9) ROBUSTNESS: VERZÖGERTE PREISÄNDERUNGEN
##############################################################

panel_lag <- panel_daily %>%
  arrange(date) %>%
  mutate(
    neg_ret_lag1 = lag(neg_ret),
    pos_ret_lag1 = lag(pos_ret)
  )

m_lag <- lm(
  log_liquidations ~ neg_ret + pos_ret +
    neg_ret_lag1 + pos_ret_lag1,
  data = panel_lag
)

robust_se(m_lag)

##############################################################
# 10) PREISSCHOCKS (> 5 %)
##############################################################

panel_shock <- panel_daily %>%
  mutate(shock_5pct_down = neg_ret < -0.05)

m_shock <- lm(
  log_liquidations ~ neg_ret + pos_ret + shock_5pct_down,
  data = panel_shock
)

m_interaction <- lm(
  log_liquidations ~ neg_ret * shock_5pct_down + pos_ret,
  data = panel_shock
)

robust_se(m_shock)
robust_se(m_interaction)

##############################################################
# 11) DEBT-STRUKTUR: STABIL VS. VOLATIL
##############################################################

stable_debt_tokens <- c(
  "DAI", "USDC", "USDT", "LUSD", "GHO", "crvUSD",
  "FRAX", "PYUSD", "USDe", "USDS", "USDtb", "EURC", "RLUSD"
)

liq_debt <- liq_raw %>%
  mutate(
    volatile_debt = !debtSymbol %in% stable_debt_tokens
  ) %>%
  count(date, volatile_debt, name = "liquidations") %>%
  left_join(eth_daily, by = "date") %>%
  filter(!is.na(ret), !is.na(vol_7d)) %>%
  mutate(
    log_liquidations = log(liquidations + 1)
  )

m_debt_level <- lm(
  log_liquidations ~ neg_ret + pos_ret + volatile_debt,
  data = liq_debt
)

m_debt_interact <- lm(
  log_liquidations ~ neg_ret + pos_ret * volatile_debt,
  data = liq_debt
)

robust_se(m_debt_level)
robust_se(m_debt_interact)

##############################################################
# 12) ERGEBNISTABELLEN (LATEX)
##############################################################

modelsummary(
  list(
    "Baseline"              = m_base,
    "Volatilität (7 Tage)"  = m_vol7,
    "Volatilität (14 Tage)" = m_vol14
  ),
  vcov = "HC1",
  stars = TRUE,
  statistic = "({std.error})",
  gof_map = c("nobs", "adj.r.squared"),
)

modelsummary(
  list(
    "Baseline (ohne Lags)" = m_base,
    "Baseline (mit Lags)"  = m_lag
  ),
  vcov = "HC1",
  stars = TRUE,
  statistic = "({std.error})",
  gof_map = c("nobs", "adj.r.squared"),
)

modelsummary(
  list(
    "Debt-Struktur"            = m_debt_level,
    "Debt-Struktur (Inter.)"  = m_debt_interact
  ),
  vcov = "HC1",
  stars = TRUE,
  statistic = "({std.error})",
  gof_map = c("nobs", "adj.r.squared"),
)

##############################################################
# ENDE DES SKRIPTS
##############################################################
