##############################################################
#   AAVE V3 – Liquidationsanalyse: Deskriptive Analyse
#   Bachelorarbeit – Empirische Analyse
#   Autor: Rafael Balasteguim da Silva
#   Datum: 05.01.2026
##############################################################

##############################################################
# 0) SETUP & LIBRARIES
##############################################################

rm(list = ls())
set.seed(123)

options(
  stringsAsFactors = FALSE,
  scipen = 999
)


##############################################################
# 0) SETUP & LIBRARIES
##############################################################

rm(list = ls())
set.seed(123)

options(
  stringsAsFactors = FALSE,
  scipen = 999
)

library(tidyverse)   
library(lubridate)   
library(scales)      
library(ineq)        
library(patchwork)   
library(ggplot2)
library(here)        


##############################################################
# 1) BASIS-PFADE
##############################################################

setwd("")

BASE_DIR <- getwd()
DATA_DIR <- "data"
FIG_DIR  <- "figures"

dir.create(FIG_DIR, showWarnings = FALSE)


##############################################################
# 2) GLOBALER PLOT-STIL (PAPER)
##############################################################

theme_paper <- function() {
  theme_minimal(base_size = 10) +
    theme(
      text = element_text(family = "serif"),
      axis.title = element_text(size = 9),
      axis.text  = element_text(size = 8),
      
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
      
      plot.title = element_blank(),
      
      legend.position = "bottom",
      legend.title    = element_blank(),
      legend.text     = element_text(size = 8),
      
      plot.background = element_rect(fill = "white", color = NA)
    )
}

# Für tagbellen:

options(modelsummary_format_numeric_latex = "plain")

fmt_ch <- function(x) {
  label_number(
    big.mark = "'",
    decimal.mark = ".",
    accuracy = 1
  )(x)
}

# Einheitliche Linien- & Balkenstärke
update_geom_defaults("line", list(linewidth = 0.6))
update_geom_defaults("step", list(linewidth = 0.6))
update_geom_defaults("col",  list(fill = "grey70"))

##############################################################
# 3) EXPORT-FUNKTION (PDF + PNG)
##############################################################

save_plot <- function(plot, name) {
  
  dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)
  
  # PDF (Standard, einspaltig)
  ggsave(
    filename = file.path(FIG_DIR, paste0(name, ".pdf")),
    plot     = plot,
    width    = 6.3,
    height   = 4.5,
    units    = "in",
    device = "pdf"
  )
  
  # PNG (Preview)
  ggsave(
    filename = file.path(FIG_DIR, paste0(name, ".png")),
    plot     = plot,
    width    = 8,
    height   = 5,
    units    = "in",
    dpi      = 300
  )
}



##############################################################
# 4) CSV EINLESEN
##############################################################
df <- read.csv(
  file.path(DATA_DIR, "liquidations_master.csv"),
  stringsAsFactors = FALSE,
  colClasses = c(
    block = "character",
    timestamp = "numeric",
    datetime_utc = "character",
    collateralAsset = "character",
    debtAsset = "character",
    user = "character",
    liquidator = "character",
    collateralOut = "numeric",
    debtToCover = "numeric",
    receiveAToken = "logical",
    collateralSymbol = "character",
    debtSymbol = "character",
    collateral_price_usd_at_block = "numeric",
    debt_price_usd_at_block = "numeric",
    collateral_value_usd = "numeric",
    debt_value_usd = "numeric",
    tx = "character",
    block_builder = "character",
    gas_used = "numeric",
    gas_price_gwei = "numeric",
    eth_price_usd_at_block = "numeric"
  )
)

glimpse(df)    # Datencheck

##############################################################
# 5) ZEITLICHER CUT
##############################################################

cut_date <- as_datetime("2025-11-30 23:59:59", tz = "UTC")

df <- df %>%
  mutate(
    datetime = as_datetime(timestamp, origin = "1970-01-01", tz = "UTC")
  ) %>%
  filter(datetime <= cut_date)


##############################################################
# 6) PROFIT- & GASKOSTENBERECHNUNG
##############################################################
 
df <- df %>%
  mutate(
    # Gaspreis von Gwei → ETH
    gas_price_eth = gas_price_gwei * 1e-9,
    
    # Gaskosten in ETH
    gas_cost_eth  = gas_used * gas_price_eth,
    
    # Gaskosten in USD
    gas_cost_usd  = gas_cost_eth * eth_price_usd_at_block,
    
    # Brutto-Profit (Collateral minus Debt)
    gross_profit_usd = collateral_value_usd - debt_value_usd,
    
    # Netto-Profit (abzüglich Gas)
    net_profit_usd   = gross_profit_usd - gas_cost_usd
  )


summary(df$gas_cost_usd)
summary(df$gross_profit_usd)
summary(df$net_profit_usd)

##############################################################
# 6.1) BASISSTATISTIK: VOLUMEN & AKTEURE
##############################################################

# Gesamt-Liquidationsvolumen (USD)
total_liquidation_volume <- df %>%
  summarise(
    total_volume_usd = sum(debt_value_usd, na.rm = TRUE)
  )

total_liquidation_volume

# Anzahl Liquidationen & Liquidatoren
summary_liquidations <- df %>%
  summarise(
    liquidations = n(),
    liquidators  = n_distinct(liquidator)
  )

summary_liquidations

# Überblick über Liquidatoren
summary(df$liquidator)

##############################################################
# 6.2) AGGREGATION PRO LIQUIDATOR
##############################################################

liq_profit_by_liquidator <- df %>%
  group_by(liquidator) %>%
  summarise(
    liquidations   = n(),
    net_profit_usd = sum(net_profit_usd, na.rm = TRUE),
    .groups = "drop"
  )

##############################################################
# 6.3) DESKRIPTIVE STATISTIK:
#      LIQUIDATIONEN PRO LIQUIDATOR
##############################################################

summary_liquidations_liq <- liq_profit_by_liquidator %>%
  summarise(
    Min    = min(liquidations),
    Q1     = quantile(liquidations, 0.25),
    Median = median(liquidations),
    Mean   = mean(liquidations),
    Q3     = quantile(liquidations, 0.75),
    Max    = max(liquidations)
  )

summary_liquidations_liq

##############################################################
# 6.4) DESKRIPTIVE STATISTIK:
#      KUMULIERTER NETTO-PROFIT PRO LIQUIDATOR
##############################################################

summary_net_profit <- liq_profit_by_liquidator %>%
  summarise(
    Min    = min(net_profit_usd),
    Q1     = quantile(net_profit_usd, 0.25),
    Median = median(net_profit_usd),
    Mean   = mean(net_profit_usd),
    Q3     = quantile(net_profit_usd, 0.75),
    Max    = max(net_profit_usd)
  )

summary_net_profit

##############################################################
# 6.5) KOMBINIERTES SUMMARY-TABLE
##############################################################

summary_table <- tibble(
  Kennzahl = c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max"),
  
  `Liquidationen pro Liquidator` = c(
    min(liq_profit_by_liquidator$liquidations),
    quantile(liq_profit_by_liquidator$liquidations, 0.25),
    median(liq_profit_by_liquidator$liquidations),
    mean(liq_profit_by_liquidator$liquidations),
    quantile(liq_profit_by_liquidator$liquidations, 0.75),
    max(liq_profit_by_liquidator$liquidations)
  ),
  
  `Kumulierter Netto-Profit (USD)` = c(
    min(liq_profit_by_liquidator$net_profit_usd),
    quantile(liq_profit_by_liquidator$net_profit_usd, 0.25),
    median(liq_profit_by_liquidator$net_profit_usd),
    mean(liq_profit_by_liquidator$net_profit_usd),
    quantile(liq_profit_by_liquidator$net_profit_usd, 0.75),
    max(liq_profit_by_liquidator$net_profit_usd)
  )
)

summary_table


##############################################################
# 7.1) TOP-10 COLLATERAL ASSETS (ANZAHL LIQUIDATIONEN)
##############################################################

p_collateralTOP10 <- df %>%
  count(collateralSymbol, sort = TRUE) %>%
  slice_head(n = 10) %>%
  ggplot(aes(
    x = reorder(collateralSymbol, n),
    y = n
  )) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Collateral-Asset",
    y = "Number of liquidations"
  ) +
  theme_paper()

# anzeigen
p_collateralTOP10

# speichern
save_plot(p_collateralTOP10, "fig1_top10_collateral")


top_collateral_table <- df %>%
  group_by(collateralSymbol) %>%
  summarise(
    liquidations      = n(),
    total_volume_usd  = sum(collateral_value_usd, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(liquidations)) %>%
  slice_head(n = 10)

top_collateral_table
##############################################################
# 7.2) TOP-10 DEBT ASSETS (ANZAHL LIQUIDATIONEN)
##############################################################

p_debtTOP10 <- df %>%
  count(debtSymbol, sort = TRUE) %>%
  slice_head(n = 10) %>%
  ggplot(aes(
    x = reorder(debtSymbol, n),
    y = n
  )) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Debt-Asset",
    y = "Number of liquidations"
  ) +
  theme_paper()

# anzeigen
p_debtTOP10

# speichern
save_plot(p_debtTOP10, "fig2_top10_debt")

##############################################################
# 7.2a) TOP-10 DEBT ASSETS – TABELLE
##############################################################

top_debt_table <- df %>%
  group_by(debtSymbol) %>%
  summarise(
    liquidations      = n(),
    total_volume_usd  = sum(debt_value_usd, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(liquidations)) %>%
  slice_head(n = 10)

top_debt_table

##############################################################
# 7.3.1) TOP-10 ASSETS EXTRAHIEREN
##############################################################

top10_collateral <- top_collateral_table$collateralSymbol
top10_debt       <- top_debt_table$debtSymbol

##############################################################
# 7.3.2) COLLATERAL × DEBT PAARE (ANZAHL)
##############################################################

pair_top10 <- df %>%
  filter(
    collateralSymbol %in% top10_collateral,
    debtSymbol       %in% top10_debt
  ) %>%
  group_by(collateralSymbol, debtSymbol) %>%
  summarise(
    liquidations = n(),
    .groups = "drop"
  )

##############################################################
# 7.3.3) SORTIERREIHENFOLGEN (ANZAHL)
##############################################################

# Collateral: nach Gesamt-Liquidationen
collateral_order <- pair_top10 %>%
  group_by(collateralSymbol) %>%
  summarise(total_liq = sum(liquidations), .groups = "drop") %>%
  arrange(desc(total_liq)) %>%
  pull(collateralSymbol)

# Debt: nach Gesamt-Liquidationen
debt_order <- pair_top10 %>%
  group_by(debtSymbol) %>%
  summarise(total_liq = sum(liquidations), .groups = "drop") %>%
  arrange(desc(total_liq)) %>%
  pull(debtSymbol)

##############################################################
# 7.3.4) HEATMAP: ANZAHL LIQUIDATIONEN
##############################################################

p_heat_top10 <- ggplot(
  pair_top10,
  aes(
    x    = factor(debtSymbol,       levels = debt_order),
    y    = factor(collateralSymbol, levels = collateral_order),
    fill = liquidations
  )
) +
  geom_tile(color = NA) +
  
  scale_fill_gradient(
    low    = "#f7fbff",
    high   = "black",
    breaks = c(0, 1000, 2000, 3000),
    labels = scales::label_number(accuracy = 1),
    name   = "Anzahl Liquidationen",
    limits = c(0, NA)
  ) +
  
  guides(
    fill = guide_colorbar(
      barwidth       = 14,
      barheight      = 0.6,
      ticks          = TRUE,
      title.position = "top"
    )
  ) +
  
  labs(
    title = "A. Anzahl Liquidationen",
    x = "Debt-Asset",
    y = "Collateral-Asset"
  ) +
  
  theme_paper() +
  theme(
    plot.title = element_text(
      size   = 10,
      face   = "bold",
      hjust  = 0.5,
      margin = margin(b = 6)
    ),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid  = element_blank(),
    legend.title = element_text(size = 9, hjust = 0.5),
    legend.text  = element_text(size = 8)
  )

# anzeigen
p_heat_top10

# speichern
save_plot(p_heat_top10, "fig3_heatmap_top10_pairs_")


##############################################################
# 7.4.1) COLLATERAL × DEBT PAARE (VOLUMEN)
##############################################################

pair_top10_volume <- df %>%
  filter(
    collateralSymbol %in% top10_collateral,
    debtSymbol       %in% top10_debt
  ) %>%
  group_by(collateralSymbol, debtSymbol) %>%
  summarise(
    volume_usd = sum(debt_value_usd, na.rm = TRUE),
    .groups = "drop"
  )

##############################################################
# 7.4.2) SORTIERREIHENFOLGEN (VOLUMEN)
##############################################################

collateral_order_vol <- pair_top10_volume %>%
  group_by(collateralSymbol) %>%
  summarise(total_vol = sum(volume_usd), .groups = "drop") %>%
  arrange(desc(total_vol)) %>%
  pull(collateralSymbol)

debt_order_vol <- pair_top10_volume %>%
  group_by(debtSymbol) %>%
  summarise(total_vol = sum(volume_usd), .groups = "drop") %>%
  arrange(desc(total_vol)) %>%
  pull(debtSymbol)

##############################################################
# 7.4.3) HEATMAP: LIQUIDATIONSVOLUMEN (USD)
##############################################################

p_heat_volume_top10 <- ggplot(
  pair_top10_volume,
  aes(
    x    = factor(debtSymbol,       levels = debt_order_vol),
    y    = factor(collateralSymbol, levels = collateral_order_vol),
    fill = volume_usd
  )
) +
  geom_tile(color = NA) +
  
  scale_fill_gradient(
    low  = "#f7fbff",
    high = "black",
    breaks = c(0, 50e6, 100e6, 150e6, 200e6, 250e6),
    labels = scales::label_number(
      scale   = 1e-6,
      suffix  = " Mio.",
      accuracy = 1
    ),
    name   = "Liquidationsvolumen in Mio. USD",
    limits = c(0, NA)
  ) +
  
  guides(
    fill = guide_colorbar(
      barwidth       = 14,
      barheight      = 0.6,
      ticks          = TRUE,
      title.position = "top"
    )
  ) +
  
  labs(
    title = "B. Liquidationsvolumen",
    x = "Debt-Asset",
    y = "Collateral-Asset"
  ) +
  
  theme_paper() +
  theme(
    plot.title = element_text(
      size   = 10,
      face   = "bold",
      hjust  = 0.5,
      margin = margin(b = 6)
    ),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid  = element_blank(),
    legend.title = element_text(size = 9, hjust = 0.5),
    legend.text  = element_text(size = 8)
  )

# anzeigen
p_heat_volume_top10

# speichern
save_plot(p_heat_volume_top10, "fig4_heatmap_top10_pairs_volume")

##############################################################
# 7.5) COMBINED HEATMAP:
#      ANZAHL (A) + VOLUMEN (B)
##############################################################

p_heat_combined <- (p_heat_top10 + p_heat_volume_top10) +
  plot_layout(widths = c(1, 1)) &
  theme(
    plot.margin = margin(
      t = 5.5,
      r = 2,
      b = 5.5,
      l = 5.5,
      unit = "pt"
    )
  )

# anzeigen
p_heat_combined

# speichern 
save_plot(
  p_heat_combined,
  "fig5_heatmap_pairs_count_volume"
)


##############################################################
# 8.1) ZEITRAUM
##############################################################

start_date <- as.Date("2023-01-01")
end_date   <- as.Date("2025-11-30")

##############################################################
# 8.2) MONATLICHE AGGREGATION
##############################################################

df_volume_monthly <- df %>%
  filter(
    datetime >= start_date,
    datetime <= as.POSIXct(end_date + 1, tz = "UTC") - 1
  ) %>%
  mutate(
    month_date = floor_date(datetime, "month")
  ) %>%
  group_by(month_date) %>%
  summarise(
    liquidationsvolumen_mio =
      sum(debt_value_usd, na.rm = TRUE) / 1e6,
    eth_preis_usd =
      mean(eth_price_usd_at_block, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(month_date)

##############################################################
# 8.3) SKALENFAKTOR (ETH-PREIS)
##############################################################

scale_factor <-
  max(df_volume_monthly$liquidationsvolumen_mio, na.rm = TRUE) /
  max(df_volume_monthly$eth_preis_usd, na.rm = TRUE)

##############################################################
# 8.4) ZEITREIHENPLOT
##############################################################

p_volume_monthly <- ggplot(df_volume_monthly, aes(x = month_date)) +
  
  # Balken: Liquidationsvolumen (Mio. USD)
  geom_col(
    aes(y = liquidationsvolumen_mio),
    fill  = "grey70",
    alpha = 0.6,
    width = 25
  ) +
  
  # Linie: ETH-Preis (sekundäre Achse)
  geom_line(
    aes(y = eth_preis_usd * scale_factor),
    color = "black",
    linewidth = 0.7
  ) +
  
  # Beschriftung ETH-Preis (letzter Beobachtungspunkt)
  geom_text(
    data = df_volume_monthly %>% slice_tail(n = 1),
    aes(
      x = month_date + 25,
      y = eth_preis_usd * scale_factor + 5,
      label = "ETH-Preis"
    ),
    hjust = 0,
    vjust = 0,
    size  = 3,
    color = "black"
  ) +
  
  scale_y_continuous(
    name = "Liquidationsvolumen (Mio. USD)",
    breaks = c(0, 50, 100, 150, 200, 250),
    sec.axis = sec_axis(
      ~ . / scale_factor,
      name   = "ETH-Preis (USD)",
      labels = label_number(big.mark = "'", accuracy = 1)
    )
  ) +
  
  scale_x_date(
    limits      = c(start_date, end_date),
    date_breaks = "3 months",
    date_labels = "%m.%Y"
  ) +
  
  labs(x = "Monat") +
  
  theme_paper() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  ) +
  
  coord_cartesian(clip = "off")


##############################################################
# 8.5) ANZEIGEN & SPEICHERN
##############################################################

p_volume_monthly

save_plot(
  p_volume_monthly,
  "fig6_volume_eth_monthly"
)

##############################################################
# 9.1) PROFITE PRO LIQUIDATOR (NUR POSITIVE)
##############################################################

df_liq_profit <- df %>%
  group_by(liquidator) %>%
  summarise(
    total_profit = sum(net_profit_usd, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(total_profit > 0)

##############################################################
# 9.2) GINI-KOEFFIZIENT
##############################################################

gini_value <- ineq(
  df_liq_profit$total_profit,
  type = "Gini"
)

##############################################################
# 9.3) LORENZ-KURVE
##############################################################

lorenz_obj <- Lc(df_liq_profit$total_profit)

lorenz_df <- data.frame(
  p = lorenz_obj$p,
  L = lorenz_obj$L
)

##############################################################
# 9.4) PLOT: LORENZ-KURVE (PROFITE)
##############################################################

p_lorenz <- ggplot(lorenz_df, aes(x = p, y = L)) +
  
  # Lorenz-Kurve
  geom_line(
    color = "black",
    linewidth = 0.6
  ) +
  
  # Gleichverteilung
  geom_abline(
    slope     = 1,
    intercept = 0,
    linetype  = "dashed",
    color     = "grey50",
    linewidth = 0.4
  ) +
  
  # Gini-Annotation
  annotate(
    "text",
    x     = 0.05,
    y     = 0.95,
    label = paste0("Gini: ", round(gini_value, 2)),
    hjust = 0,
    size  = 3,
    family = "serif"
  ) +
  
  labs(
    x = "Kumulativer Anteil der Liquidatoren",
    y = "Kumulativer Anteil der Gesamtprofite"
  ) +
  
  coord_equal(
    xlim = c(0, 1),
    ylim = c(0, 1)
  ) +
  
  theme_paper()

p_lorenz

save_plot(
  p_lorenz,
  "fig7_lorenz_liquidators"
)


##############################################################
# BOXPLOT: GAS-KOSTEN PRO LIQUIDATION
##############################################################

p_gas_box <- ggplot(df, aes(y = gas_cost_usd)) +
  
  geom_boxplot(
    fill = "grey80",
    color = "black",
    linewidth = 0.5,
    outlier.alpha = 0.2
  ) +
  
  scale_y_continuous(
    labels = scales::label_number(
      accuracy = 1,
      big.mark = "'"
    ),
    name = "Gaskosten pro Liquidation (USD)"
  ) +
  
  labs(x = NULL) +
  
  theme_paper() +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_blank()
  )

# anzeigen
p_gas_box

# speichern
save_plot(p_gas_box, "fig8_gas_cost_boxplot")

##############################################################
# 11.1) TOP-10 LIQUIDATOREN NACH NETTO-GEWINN
##############################################################

top10_liquidators <- df %>%
  group_by(liquidator) %>%
  summarise(
    liquidations     = n(),
    gross_profit_usd = sum(gross_profit_usd, na.rm = TRUE),
    gas_cost_usd     = sum(gas_cost_usd, na.rm = TRUE),
    net_profit_usd   = sum(net_profit_usd, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(net_profit_usd)) %>%
  slice_head(n = 10)

##############################################################
# 11.1) TOP-10 LIQUIDATOREN NACH NETTO-GEWINN
##############################################################

top10_liquidators <- df %>%
  group_by(liquidator) %>%
  summarise(
    liquidations     = n(),
    gross_profit_usd = sum(gross_profit_usd, na.rm = TRUE),
    gas_cost_usd     = sum(gas_cost_usd, na.rm = TRUE),
    net_profit_usd   = sum(net_profit_usd, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(net_profit_usd)) %>%
  slice_head(n = 10)

##############################################################
# 11.2) GERUNDETE ÜBERSICHT (TOP-10)
##############################################################

top10_liquidators_clean <- top10_liquidators %>%
  mutate(
    gross_profit_usd = round(gross_profit_usd, 0),
    gas_cost_usd     = round(gas_cost_usd, 0),
    net_profit_usd   = round(net_profit_usd, 0)
  )

print(top10_liquidators_clean, n = Inf, width = Inf)


##############################################################
# 11.3) ANTEIL DER TOP-10 AM GESAMTGEWINN
##############################################################

top10_share <- top10_liquidators %>%
  summarise(
    share_top10 =
      sum(net_profit_usd) /
      sum(df$net_profit_usd, na.rm = TRUE)
  )

top10_share

##############################################################
# 11.4) GAS-KOSTEN PRO LIQUIDATOR
##############################################################

liquidator_gas_stats <- df %>%
  group_by(liquidator) %>%
  summarise(
    liquidations  = n(),
    total_gas_usd = sum(gas_cost_usd, na.rm = TRUE),
    avg_gas_usd   = mean(gas_cost_usd, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(avg_gas_usd)

liquidator_gas_stats_clean <- liquidator_gas_stats %>%
  mutate(
    total_gas_usd = round(total_gas_usd, 0),
    avg_gas_usd   = round(avg_gas_usd, 2)
  )

print(liquidator_gas_stats_clean, n = Inf, width = Inf)

##############################################################
# 11.5) GRÖSSTE EINZEL-LIQUIDATIONEN
##############################################################

top20_largest_liquidations <- df %>%
  arrange(desc(net_profit_usd)) %>%
  mutate(
    asset_pair = paste(collateralSymbol, "/", debtSymbol)
  ) %>%
  select(
    liquidator,
    asset_pair,
    gross_profit_usd,
    gas_cost_usd,
    net_profit_usd
  ) %>%
  slice_head(n = 25)

top20_largest_liquidations

##############################################################
# 11.5) GRÖSSTE EINZEL-LIQUIDATIONEN
##############################################################

top20_largest_liquidations <- df %>%
  arrange(desc(net_profit_usd)) %>%
  mutate(
    asset_pair = paste(collateralSymbol, "/", debtSymbol)
  ) %>%
  select(
    liquidator,
    asset_pair,
    gross_profit_usd,
    gas_cost_usd,
    net_profit_usd
  ) %>%
  slice_head(n = 25)

top20_largest_liquidations

##############################################################
# 11.6) UNPROFITABLE LIQUIDATOREN
##############################################################

top25_unprofitable_liquidators <- df %>%
  group_by(liquidator) %>%
  summarise(
    liquidations     = n(),
    gross_profit_usd = sum(gross_profit_usd, na.rm = TRUE),
    gas_cost_usd     = sum(gas_cost_usd, na.rm = TRUE),
    net_profit_usd   = sum(net_profit_usd, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(net_profit_usd <= 0) %>%
  arrange(net_profit_usd) %>%
  slice_head(n = 25) %>%
  mutate(
    gross_profit_usd = round(gross_profit_usd, 0),
    gas_cost_usd     = round(gas_cost_usd, 0),
    net_profit_usd   = round(net_profit_usd, 0)
  )

print(top25_unprofitable_liquidators, n = Inf)

##############################################################
# ENDE
##############################################################





##############################################################
# 1) BASIS-PFADE
##############################################################

setwd("C:/Users/rafae/Documents/UniBas/Bachelor Arbeit/aave-v3-liquidationsanalyse")

BASE_DIR <- getwd()
DATA_DIR <- "data"
FIG_DIR  <- "figures"
TAB_DIR  <- "tables"

dir.create(FIG_DIR, showWarnings = FALSE)
dir.create(TAB_DIR, showWarnings = FALSE)


##############################################################
# 2) GLOBALER PLOT-STIL (PAPER)
##############################################################

theme_paper <- function() {
  theme_minimal(base_size = 10) +
    theme(
      text = element_text(family = "serif"),
      axis.title = element_text(size = 9),
      axis.text  = element_text(size = 8),
      
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
      
      plot.title = element_blank(),
      
      legend.position = "bottom",
      legend.title    = element_blank(),
      legend.text     = element_text(size = 8),
      
      plot.background = element_rect(fill = "white", color = NA)
    )
}

# Für tagbellen:

options(modelsummary_format_numeric_latex = "plain")

fmt_ch <- function(x) {
  label_number(
    big.mark = "'",
    decimal.mark = ".",
    accuracy = 1
  )(x)
}

# Einheitliche Linien- & Balkenstärke
update_geom_defaults("line", list(linewidth = 0.6))
update_geom_defaults("step", list(linewidth = 0.6))
update_geom_defaults("col",  list(fill = "grey70"))

##############################################################
# 3) EXPORT-FUNKTION (PDF + PNG)
##############################################################

save_plot <- function(plot, name) {
  
  dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)
  
  # PDF (Standard, einspaltig)
  ggsave(
    filename = file.path(FIG_DIR, paste0(name, ".pdf")),
    plot     = plot,
    width    = 6.3,
    height   = 4.5,
    units    = "in",
    device = "pdf"
  )
  
  # PNG (Preview)
  ggsave(
    filename = file.path(FIG_DIR, paste0(name, ".png")),
    plot     = plot,
    width    = 8,
    height   = 5,
    units    = "in",
    dpi      = 300
  )
}



##############################################################
# 4) CSV EINLESEN
##############################################################
df <- read.csv(
  file.path(DATA_DIR, "liquidations_master.csv"),
  stringsAsFactors = FALSE,
  colClasses = c(
    block = "character",
    timestamp = "numeric",
    datetime_utc = "character",
    collateralAsset = "character",
    debtAsset = "character",
    user = "character",
    liquidator = "character",
    collateralOut = "numeric",
    debtToCover = "numeric",
    receiveAToken = "logical",
    collateralSymbol = "character",
    debtSymbol = "character",
    collateral_price_usd_at_block = "numeric",
    debt_price_usd_at_block = "numeric",
    collateral_value_usd = "numeric",
    debt_value_usd = "numeric",
    tx = "character",
    block_builder = "character",
    gas_used = "numeric",
    gas_price_gwei = "numeric",
    eth_price_usd_at_block = "numeric"
  )
)

glimpse(df)    # Datencheck

##############################################################
# 5) ZEITLICHER CUT
##############################################################

cut_date <- as_datetime("2025-11-30 23:59:59", tz = "UTC")

df <- df %>%
  mutate(
    datetime = as_datetime(timestamp, origin = "1970-01-01", tz = "UTC")
  ) %>%
  filter(datetime <= cut_date)


##############################################################
# 6) PROFIT- & GASKOSTENBERECHNUNG
##############################################################
 
df <- df %>%
  mutate(
    # Gaspreis von Gwei → ETH
    gas_price_eth = gas_price_gwei * 1e-9,
    
    # Gaskosten in ETH
    gas_cost_eth  = gas_used * gas_price_eth,
    
    # Gaskosten in USD
    gas_cost_usd  = gas_cost_eth * eth_price_usd_at_block,
    
    # Brutto-Profit (Collateral minus Debt)
    gross_profit_usd = collateral_value_usd - debt_value_usd,
    
    # Netto-Profit (abzüglich Gas)
    net_profit_usd   = gross_profit_usd - gas_cost_usd
  )


summary(df$gas_cost_usd)
summary(df$gross_profit_usd)
summary(df$net_profit_usd)

##############################################################
# 6.1) BASISSTATISTIK: VOLUMEN & AKTEURE
##############################################################

# Gesamt-Liquidationsvolumen (USD)
total_liquidation_volume <- df %>%
  summarise(
    total_volume_usd = sum(debt_value_usd, na.rm = TRUE)
  )

total_liquidation_volume

# Anzahl Liquidationen & Liquidatoren
summary_liquidations <- df %>%
  summarise(
    liquidations = n(),
    liquidators  = n_distinct(liquidator)
  )

summary_liquidations

# Überblick über Liquidatoren
summary(df$liquidator)

##############################################################
# 6.2) AGGREGATION PRO LIQUIDATOR
##############################################################

liq_profit_by_liquidator <- df %>%
  group_by(liquidator) %>%
  summarise(
    liquidations   = n(),
    net_profit_usd = sum(net_profit_usd, na.rm = TRUE),
    .groups = "drop"
  )

##############################################################
# 6.3) DESKRIPTIVE STATISTIK:
#      LIQUIDATIONEN PRO LIQUIDATOR
##############################################################

summary_liquidations_liq <- liq_profit_by_liquidator %>%
  summarise(
    Min    = min(liquidations),
    Q1     = quantile(liquidations, 0.25),
    Median = median(liquidations),
    Mean   = mean(liquidations),
    Q3     = quantile(liquidations, 0.75),
    Max    = max(liquidations)
  )

summary_liquidations_liq

##############################################################
# 6.4) DESKRIPTIVE STATISTIK:
#      KUMULIERTER NETTO-PROFIT PRO LIQUIDATOR
##############################################################

summary_net_profit <- liq_profit_by_liquidator %>%
  summarise(
    Min    = min(net_profit_usd),
    Q1     = quantile(net_profit_usd, 0.25),
    Median = median(net_profit_usd),
    Mean   = mean(net_profit_usd),
    Q3     = quantile(net_profit_usd, 0.75),
    Max    = max(net_profit_usd)
  )

summary_net_profit

##############################################################
# 6.5) KOMBINIERTES SUMMARY-TABLE
##############################################################

summary_table <- tibble(
  Kennzahl = c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max"),
  
  `Liquidationen pro Liquidator` = c(
    min(liq_profit_by_liquidator$liquidations),
    quantile(liq_profit_by_liquidator$liquidations, 0.25),
    median(liq_profit_by_liquidator$liquidations),
    mean(liq_profit_by_liquidator$liquidations),
    quantile(liq_profit_by_liquidator$liquidations, 0.75),
    max(liq_profit_by_liquidator$liquidations)
  ),
  
  `Kumulierter Netto-Profit (USD)` = c(
    min(liq_profit_by_liquidator$net_profit_usd),
    quantile(liq_profit_by_liquidator$net_profit_usd, 0.25),
    median(liq_profit_by_liquidator$net_profit_usd),
    mean(liq_profit_by_liquidator$net_profit_usd),
    quantile(liq_profit_by_liquidator$net_profit_usd, 0.75),
    max(liq_profit_by_liquidator$net_profit_usd)
  )
)

summary_table


##############################################################
# 7.1) TOP-10 COLLATERAL ASSETS (ANZAHL LIQUIDATIONEN)
##############################################################

p_collateralTOP10 <- df %>%
  count(collateralSymbol, sort = TRUE) %>%
  slice_head(n = 10) %>%
  ggplot(aes(
    x = reorder(collateralSymbol, n),
    y = n
  )) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Collateral-Asset",
    y = "Number of liquidations"
  ) +
  theme_paper()

# anzeigen
p_collateralTOP10

# speichern
save_plot(p_collateralTOP10, "fig1_top10_collateral")


top_collateral_table <- df %>%
  group_by(collateralSymbol) %>%
  summarise(
    liquidations      = n(),
    total_volume_usd  = sum(collateral_value_usd, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(liquidations)) %>%
  slice_head(n = 10)

top_collateral_table
##############################################################
# 7.2) TOP-10 DEBT ASSETS (ANZAHL LIQUIDATIONEN)
##############################################################

p_debtTOP10 <- df %>%
  count(debtSymbol, sort = TRUE) %>%
  slice_head(n = 10) %>%
  ggplot(aes(
    x = reorder(debtSymbol, n),
    y = n
  )) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Debt-Asset",
    y = "Number of liquidations"
  ) +
  theme_paper()

# anzeigen
p_debtTOP10

# speichern
save_plot(p_debtTOP10, "fig2_top10_debt")

##############################################################
# 7.2a) TOP-10 DEBT ASSETS – TABELLE
##############################################################

top_debt_table <- df %>%
  group_by(debtSymbol) %>%
  summarise(
    liquidations      = n(),
    total_volume_usd  = sum(debt_value_usd, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(liquidations)) %>%
  slice_head(n = 10)

top_debt_table

##############################################################
# 7.3.1) TOP-10 ASSETS EXTRAHIEREN
##############################################################

top10_collateral <- top_collateral_table$collateralSymbol
top10_debt       <- top_debt_table$debtSymbol

##############################################################
# 7.3.2) COLLATERAL × DEBT PAARE (ANZAHL)
##############################################################

pair_top10 <- df %>%
  filter(
    collateralSymbol %in% top10_collateral,
    debtSymbol       %in% top10_debt
  ) %>%
  group_by(collateralSymbol, debtSymbol) %>%
  summarise(
    liquidations = n(),
    .groups = "drop"
  )

##############################################################
# 7.3.3) SORTIERREIHENFOLGEN (ANZAHL)
##############################################################

# Collateral: nach Gesamt-Liquidationen
collateral_order <- pair_top10 %>%
  group_by(collateralSymbol) %>%
  summarise(total_liq = sum(liquidations), .groups = "drop") %>%
  arrange(desc(total_liq)) %>%
  pull(collateralSymbol)

# Debt: nach Gesamt-Liquidationen
debt_order <- pair_top10 %>%
  group_by(debtSymbol) %>%
  summarise(total_liq = sum(liquidations), .groups = "drop") %>%
  arrange(desc(total_liq)) %>%
  pull(debtSymbol)

##############################################################
# 7.3.4) HEATMAP: ANZAHL LIQUIDATIONEN
##############################################################

p_heat_top10 <- ggplot(
  pair_top10,
  aes(
    x    = factor(debtSymbol,       levels = debt_order),
    y    = factor(collateralSymbol, levels = collateral_order),
    fill = liquidations
  )
) +
  geom_tile(color = NA) +
  
  scale_fill_gradient(
    low    = "#f7fbff",
    high   = "black",
    breaks = c(0, 1000, 2000, 3000),
    labels = scales::label_number(accuracy = 1),
    name   = "Anzahl Liquidationen",
    limits = c(0, NA)
  ) +
  
  guides(
    fill = guide_colorbar(
      barwidth       = 14,
      barheight      = 0.6,
      ticks          = TRUE,
      title.position = "top"
    )
  ) +
  
  labs(
    title = "A. Anzahl Liquidationen",
    x = "Debt-Asset",
    y = "Collateral-Asset"
  ) +
  
  theme_paper() +
  theme(
    plot.title = element_text(
      size   = 10,
      face   = "bold",
      hjust  = 0.5,
      margin = margin(b = 6)
    ),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid  = element_blank(),
    legend.title = element_text(size = 9, hjust = 0.5),
    legend.text  = element_text(size = 8)
  )

# anzeigen
p_heat_top10

# speichern
save_plot(p_heat_top10, "fig3_heatmap_top10_pairs_")


##############################################################
# 7.4.1) COLLATERAL × DEBT PAARE (VOLUMEN)
##############################################################

pair_top10_volume <- df %>%
  filter(
    collateralSymbol %in% top10_collateral,
    debtSymbol       %in% top10_debt
  ) %>%
  group_by(collateralSymbol, debtSymbol) %>%
  summarise(
    volume_usd = sum(debt_value_usd, na.rm = TRUE),
    .groups = "drop"
  )

##############################################################
# 7.4.2) SORTIERREIHENFOLGEN (VOLUMEN)
##############################################################

collateral_order_vol <- pair_top10_volume %>%
  group_by(collateralSymbol) %>%
  summarise(total_vol = sum(volume_usd), .groups = "drop") %>%
  arrange(desc(total_vol)) %>%
  pull(collateralSymbol)

debt_order_vol <- pair_top10_volume %>%
  group_by(debtSymbol) %>%
  summarise(total_vol = sum(volume_usd), .groups = "drop") %>%
  arrange(desc(total_vol)) %>%
  pull(debtSymbol)

##############################################################
# 7.4.3) HEATMAP: LIQUIDATIONSVOLUMEN (USD)
##############################################################

p_heat_volume_top10 <- ggplot(
  pair_top10_volume,
  aes(
    x    = factor(debtSymbol,       levels = debt_order_vol),
    y    = factor(collateralSymbol, levels = collateral_order_vol),
    fill = volume_usd
  )
) +
  geom_tile(color = NA) +
  
  scale_fill_gradient(
    low  = "#f7fbff",
    high = "black",
    breaks = c(0, 50e6, 100e6, 150e6, 200e6, 250e6),
    labels = scales::label_number(
      scale   = 1e-6,
      suffix  = " Mio.",
      accuracy = 1
    ),
    name   = "Liquidationsvolumen in Mio. USD",
    limits = c(0, NA)
  ) +
  
  guides(
    fill = guide_colorbar(
      barwidth       = 14,
      barheight      = 0.6,
      ticks          = TRUE,
      title.position = "top"
    )
  ) +
  
  labs(
    title = "B. Liquidationsvolumen",
    x = "Debt-Asset",
    y = "Collateral-Asset"
  ) +
  
  theme_paper() +
  theme(
    plot.title = element_text(
      size   = 10,
      face   = "bold",
      hjust  = 0.5,
      margin = margin(b = 6)
    ),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid  = element_blank(),
    legend.title = element_text(size = 9, hjust = 0.5),
    legend.text  = element_text(size = 8)
  )

# anzeigen
p_heat_volume_top10

# speichern
save_plot(p_heat_volume_top10, "fig4_heatmap_top10_pairs_volume")

##############################################################
# 7.5) COMBINED HEATMAP:
#      ANZAHL (A) + VOLUMEN (B)
##############################################################

p_heat_combined <- (p_heat_top10 + p_heat_volume_top10) +
  plot_layout(widths = c(1, 1)) &
  theme(
    plot.margin = margin(
      t = 5.5,
      r = 2,
      b = 5.5,
      l = 5.5,
      unit = "pt"
    )
  )

# anzeigen
p_heat_combined

# speichern 
save_plot(
  p_heat_combined,
  "fig5_heatmap_pairs_count_volume"
)


##############################################################
# 8.1) ZEITRAUM
##############################################################

start_date <- as.Date("2023-01-01")
end_date   <- as.Date("2025-11-30")

##############################################################
# 8.2) MONATLICHE AGGREGATION
##############################################################

df_volume_monthly <- df %>%
  filter(
    datetime >= start_date,
    datetime <= as.POSIXct(end_date + 1, tz = "UTC") - 1
  ) %>%
  mutate(
    month_date = floor_date(datetime, "month")
  ) %>%
  group_by(month_date) %>%
  summarise(
    liquidationsvolumen_mio =
      sum(debt_value_usd, na.rm = TRUE) / 1e6,
    eth_preis_usd =
      mean(eth_price_usd_at_block, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(month_date)

##############################################################
# 8.3) SKALENFAKTOR (ETH-PREIS)
##############################################################

scale_factor <-
  max(df_volume_monthly$liquidationsvolumen_mio, na.rm = TRUE) /
  max(df_volume_monthly$eth_preis_usd, na.rm = TRUE)

##############################################################
# 8.4) ZEITREIHENPLOT
##############################################################

p_volume_monthly <- ggplot(df_volume_monthly, aes(x = month_date)) +
  
  # Balken: Liquidationsvolumen (Mio. USD)
  geom_col(
    aes(y = liquidationsvolumen_mio),
    fill  = "grey70",
    alpha = 0.6,
    width = 25
  ) +
  
  # Linie: ETH-Preis (sekundäre Achse)
  geom_line(
    aes(y = eth_preis_usd * scale_factor),
    color = "black",
    linewidth = 0.7
  ) +
  
  # Beschriftung ETH-Preis (letzter Beobachtungspunkt)
  geom_text(
    data = df_volume_monthly %>% slice_tail(n = 1),
    aes(
      x = month_date + 25,
      y = eth_preis_usd * scale_factor + 5,
      label = "ETH-Preis"
    ),
    hjust = 0,
    vjust = 0,
    size  = 3,
    color = "black"
  ) +
  
  scale_y_continuous(
    name = "Liquidationsvolumen (Mio. USD)",
    breaks = c(0, 50, 100, 150, 200, 250),
    sec.axis = sec_axis(
      ~ . / scale_factor,
      name   = "ETH-Preis (USD)",
      labels = label_number(big.mark = "'", accuracy = 1)
    )
  ) +
  
  scale_x_date(
    limits      = c(start_date, end_date),
    date_breaks = "3 months",
    date_labels = "%m.%Y"
  ) +
  
  labs(x = "Monat") +
  
  theme_paper() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  ) +
  
  coord_cartesian(clip = "off")


##############################################################
# 8.5) ANZEIGEN & SPEICHERN
##############################################################

p_volume_monthly

save_plot(
  p_volume_monthly,
  "fig6_volume_eth_monthly"
)

##############################################################
# 9.1) PROFITE PRO LIQUIDATOR (NUR POSITIVE)
##############################################################

df_liq_profit <- df %>%
  group_by(liquidator) %>%
  summarise(
    total_profit = sum(net_profit_usd, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(total_profit > 0)

##############################################################
# 9.2) GINI-KOEFFIZIENT
##############################################################

gini_value <- ineq(
  df_liq_profit$total_profit,
  type = "Gini"
)

##############################################################
# 9.3) LORENZ-KURVE
##############################################################

lorenz_obj <- Lc(df_liq_profit$total_profit)

lorenz_df <- data.frame(
  p = lorenz_obj$p,
  L = lorenz_obj$L
)

##############################################################
# 9.4) PLOT: LORENZ-KURVE (PROFITE)
##############################################################

p_lorenz <- ggplot(lorenz_df, aes(x = p, y = L)) +
  
  # Lorenz-Kurve
  geom_line(
    color = "black",
    linewidth = 0.6
  ) +
  
  # Gleichverteilung
  geom_abline(
    slope     = 1,
    intercept = 0,
    linetype  = "dashed",
    color     = "grey50",
    linewidth = 0.4
  ) +
  
  # Gini-Annotation
  annotate(
    "text",
    x     = 0.05,
    y     = 0.95,
    label = paste0("Gini: ", round(gini_value, 2)),
    hjust = 0,
    size  = 3,
    family = "serif"
  ) +
  
  labs(
    x = "Kumulativer Anteil der Liquidatoren",
    y = "Kumulativer Anteil der Gesamtprofite"
  ) +
  
  coord_equal(
    xlim = c(0, 1),
    ylim = c(0, 1)
  ) +
  
  theme_paper()

p_lorenz

save_plot(
  p_lorenz,
  "fig7_lorenz_liquidators"
)


##############################################################
# BOXPLOT: GAS-KOSTEN PRO LIQUIDATION
##############################################################

p_gas_box <- ggplot(df, aes(y = gas_cost_usd)) +
  
  geom_boxplot(
    fill = "grey80",
    color = "black",
    linewidth = 0.5,
    outlier.alpha = 0.2
  ) +
  
  scale_y_continuous(
    labels = scales::label_number(
      accuracy = 1,
      big.mark = "'"
    ),
    name = "Gaskosten pro Liquidation (USD)"
  ) +
  
  labs(x = NULL) +
  
  theme_paper() +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_blank()
  )

# anzeigen
p_gas_box

# speichern
save_plot(p_gas_box, "fig8_gas_cost_boxplot")

##############################################################
# 11.1) TOP-10 LIQUIDATOREN NACH NETTO-GEWINN
##############################################################

top10_liquidators <- df %>%
  group_by(liquidator) %>%
  summarise(
    liquidations     = n(),
    gross_profit_usd = sum(gross_profit_usd, na.rm = TRUE),
    gas_cost_usd     = sum(gas_cost_usd, na.rm = TRUE),
    net_profit_usd   = sum(net_profit_usd, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(net_profit_usd)) %>%
  slice_head(n = 10)

##############################################################
# 11.1) TOP-10 LIQUIDATOREN NACH NETTO-GEWINN
##############################################################

top10_liquidators <- df %>%
  group_by(liquidator) %>%
  summarise(
    liquidations     = n(),
    gross_profit_usd = sum(gross_profit_usd, na.rm = TRUE),
    gas_cost_usd     = sum(gas_cost_usd, na.rm = TRUE),
    net_profit_usd   = sum(net_profit_usd, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(net_profit_usd)) %>%
  slice_head(n = 10)

##############################################################
# 11.2) GERUNDETE ÜBERSICHT (TOP-10)
##############################################################

top10_liquidators_clean <- top10_liquidators %>%
  mutate(
    gross_profit_usd = round(gross_profit_usd, 0),
    gas_cost_usd     = round(gas_cost_usd, 0),
    net_profit_usd   = round(net_profit_usd, 0)
  )

print(top10_liquidators_clean, n = Inf, width = Inf)


##############################################################
# 11.3) ANTEIL DER TOP-10 AM GESAMTGEWINN
##############################################################

top10_share <- top10_liquidators %>%
  summarise(
    share_top10 =
      sum(net_profit_usd) /
      sum(df$net_profit_usd, na.rm = TRUE)
  )

top10_share

##############################################################
# 11.4) GAS-KOSTEN PRO LIQUIDATOR
##############################################################

liquidator_gas_stats <- df %>%
  group_by(liquidator) %>%
  summarise(
    liquidations  = n(),
    total_gas_usd = sum(gas_cost_usd, na.rm = TRUE),
    avg_gas_usd   = mean(gas_cost_usd, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(avg_gas_usd)

liquidator_gas_stats_clean <- liquidator_gas_stats %>%
  mutate(
    total_gas_usd = round(total_gas_usd, 0),
    avg_gas_usd   = round(avg_gas_usd, 2)
  )

print(liquidator_gas_stats_clean, n = Inf, width = Inf)

##############################################################
# 11.5) GRÖSSTE EINZEL-LIQUIDATIONEN
##############################################################

top20_largest_liquidations <- df %>%
  arrange(desc(net_profit_usd)) %>%
  mutate(
    asset_pair = paste(collateralSymbol, "/", debtSymbol)
  ) %>%
  select(
    liquidator,
    asset_pair,
    gross_profit_usd,
    gas_cost_usd,
    net_profit_usd
  ) %>%
  slice_head(n = 25)

top20_largest_liquidations

##############################################################
# 11.5) GRÖSSTE EINZEL-LIQUIDATIONEN
##############################################################

top20_largest_liquidations <- df %>%
  arrange(desc(net_profit_usd)) %>%
  mutate(
    asset_pair = paste(collateralSymbol, "/", debtSymbol)
  ) %>%
  select(
    liquidator,
    asset_pair,
    gross_profit_usd,
    gas_cost_usd,
    net_profit_usd
  ) %>%
  slice_head(n = 25)

top20_largest_liquidations

##############################################################
# 11.6) UNPROFITABLE LIQUIDATOREN
##############################################################

top25_unprofitable_liquidators <- df %>%
  group_by(liquidator) %>%
  summarise(
    liquidations     = n(),
    gross_profit_usd = sum(gross_profit_usd, na.rm = TRUE),
    gas_cost_usd     = sum(gas_cost_usd, na.rm = TRUE),
    net_profit_usd   = sum(net_profit_usd, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(net_profit_usd <= 0) %>%
  arrange(net_profit_usd) %>%
  slice_head(n = 25) %>%
  mutate(
    gross_profit_usd = round(gross_profit_usd, 0),
    gas_cost_usd     = round(gas_cost_usd, 0),
    net_profit_usd   = round(net_profit_usd, 0)
  )

print(top25_unprofitable_liquidators, n = Inf)

##############################################################
# ENDE
##############################################################

