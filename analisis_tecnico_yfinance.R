##############################################################
#   ANÁLISIS TÉCNICO — 5 ACTIVOS FINANCIEROS
#   Medias Móviles (SMA10, SMA20, SMA50) + RSI (14 períodos)
#   Horizonte: próximos 7 días
#
#   Paquetes necesarios (instalar una sola vez):
#   install.packages(c("quantmod","TTR","ggplot2","gridExtra",
#                      "scales","dplyr","patchwork","grid",
#                      "grDevices","knitr"))
##############################################################

suppressMessages({
  library(quantmod)   # descarga de datos financieros
  library(TTR)        # indicadores técnicos (SMA, RSI)
  library(ggplot2)    # gráficos
  library(dplyr)      # manipulación de datos
  library(patchwork)  # combinar paneles de ggplot
  library(scales)     # formateo de ejes
  library(grid)       # elementos gráficos base
  library(grDevices)  # dispositivos gráficos (PNG, PDF)
})

# ─────────────────────────────────────────────
# CONFIGURACIÓN
# ─────────────────────────────────────────────
TICKERS <- list(
  "AAPL"     = list(nombre = "Apple Inc.",   tipo = "Acción",        moneda = "USD"),
  "BTC-USD"  = list(nombre = "Bitcoin",      tipo = "Criptomoneda",  moneda = "USD"),
  "GC=F"     = list(nombre = "Gold Futures", tipo = "Materia Prima", moneda = "USD"),
  "EURUSD=X" = list(nombre = "Euro / USD",   tipo = "Divisa",        moneda = "USD"),
  "TSLA"     = list(nombre = "Tesla Inc.",   tipo = "Acción",        moneda = "USD")
)

END_DATE   <- Sys.Date()
START_DATE <- END_DATE - 180
OUTPUT_DIR <- "."

PALETA <- list(
  price = "#2c3e50", sma10 = "#3498db", sma20 = "#e67e22",
  sma50 = "#8e44ad", buy   = "#27ae60", sell  = "#e74c3c",
  rsi   = "#2980b9", bg    = "#f8f9fa", grid  = "#dfe6e9"
)

cat(strrep("=", 60), "\n")
cat("  Descargando datos via quantmod (Yahoo Finance)...\n")
cat(strrep("=", 60), "\n")

# ─────────────────────────────────────────────
# 1. DESCARGA DE DATOS
# ─────────────────────────────────────────────
assets <- list()

for (ticker in names(TICKERS)) {
  meta <- TICKERS[[ticker]]
  cat(sprintf("  → %s (%s)... ", ticker, meta$nombre))

  tryCatch({
    raw <- suppressWarnings(
      getSymbols(ticker, src = "yahoo",
                 from = START_DATE, to = END_DATE,
                 auto.assign = FALSE, warnings = FALSE)
    )
    # Quedarse solo con el precio de cierre ajustado
    close_col <- grep("Adjusted|Close", colnames(raw), value = TRUE)[1]
    df <- data.frame(
      Date  = as.Date(index(raw)),
      Close = as.numeric(raw[, close_col])
    )
    df <- df[!is.na(df$Close), ]

    meta$df <- df
    assets[[ticker]] <- meta
    cat(sprintf("OK — %d sesiones\n", nrow(df)))
  }, error = function(e) {
    cat(sprintf("ERROR: %s\n", e$message))
  })
}
cat("\n")

# ─────────────────────────────────────────────
# 2. INDICADORES TÉCNICOS
# ─────────────────────────────────────────────
calc_indicators <- function(df) {
  df <- df[order(df$Date), ]
  n  <- nrow(df)
  cl <- df$Close

  df$SMA10 <- as.numeric(SMA(cl, n = 10))
  df$SMA20 <- as.numeric(SMA(cl, n = 20))
  df$SMA50 <- as.numeric(SMA(cl, n = 50))

  # RSI Wilder (14 períodos)
  df$RSI <- as.numeric(RSI(cl, n = 14, maType = "EMA"))

  # Señales cruce SMA10/SMA20
  df$Signal    <- ifelse(df$SMA10 > df$SMA20, 1, -1)
  lag_signal   <- c(NA, df$Signal[-n])
  df$CrossUp   <- !is.na(lag_signal) & df$Signal == 1  & lag_signal == -1
  df$CrossDown <- !is.na(lag_signal) & df$Signal == -1 & lag_signal == 1

  df
}

for (ticker in names(assets)) {
  assets[[ticker]]$df <- calc_indicators(assets[[ticker]]$df)
}

# ─────────────────────────────────────────────
# 3. RECOMENDACIÓN COMPUESTA
# ─────────────────────────────────────────────
get_recommendation <- function(df) {
  last  <- tail(df, 1)
  rsi   <- last$RSI
  sma10 <- last$SMA10
  sma20 <- last$SMA20
  sma50 <- last$SMA50
  close <- last$Close
  score   <- 0
  reasons <- character(0)

  recent <- tail(df, 3)
  if (any(recent$CrossUp, na.rm = TRUE)) {
    score <- score + 2
    reasons <- c(reasons, "Cruce alcista SMA10/SMA20 reciente")
  }
  if (any(recent$CrossDown, na.rm = TRUE)) {
    score <- score - 2
    reasons <- c(reasons, "Cruce bajista SMA10/SMA20 reciente")
  }

  if (!is.na(sma50)) {
    if (sma10 > sma20 && sma20 > sma50) {
      score <- score + 2
      reasons <- c(reasons, "Alineación alcista SMA10>SMA20>SMA50")
    } else if (sma10 < sma20 && sma20 < sma50) {
      score <- score - 2
      reasons <- c(reasons, "Alineación bajista SMA10<SMA20<SMA50")
    }
  }

  if (!is.na(close) && !is.na(sma20)) {
    if (close > sma20) {
      score <- score + 1; reasons <- c(reasons, "Precio sobre SMA20")
    } else {
      score <- score - 1; reasons <- c(reasons, "Precio bajo SMA20")
    }
  }

  if (!is.na(rsi)) {
    if (rsi < 30) {
      score <- score + 3
      reasons <- c(reasons, sprintf("RSI sobrevendido (%.1f)", rsi))
    } else if (rsi > 70) {
      score <- score - 3
      reasons <- c(reasons, sprintf("RSI sobrecomprado (%.1f)", rsi))
    } else if (rsi > 60) {
      score <- score + 1
      reasons <- c(reasons, sprintf("RSI momentum alcista (%.1f)", rsi))
    } else if (rsi < 40) {
      score <- score - 1
      reasons <- c(reasons, sprintf("RSI momentum bajista (%.1f)", rsi))
    }
  }

  if (score >= 3) {
    rec <- "COMPRA";  color <- "#27ae60"
  } else if (score <= -3) {
    rec <- "VENTA";   color <- "#e74c3c"
  } else {
    rec <- "NEUTRAL"; color <- "#f39c12"
  }

  list(rec = rec, color = color, score = score, reasons = reasons,
       rsi_val = rsi, sma10_val = sma10, sma20_val = sma20)
}

for (ticker in names(assets)) {
  res <- get_recommendation(assets[[ticker]]$df)
  assets[[ticker]] <- c(assets[[ticker]], res)
}

# ─────────────────────────────────────────────
# 4. GRÁFICOS (PNG por activo)
# ─────────────────────────────────────────────
chart_files <- list()

for (ticker in names(assets)) {
  info <- assets[[ticker]]
  df   <- tail(info$df, 65)   # últimas 65 sesiones
  rec  <- info$rec
  rc   <- info$color

  # — Panel 1: Precio + SMAs —
  buy_pts  <- df[!is.na(df$CrossUp)  & df$CrossUp,  ]
  sell_pts <- df[!is.na(df$CrossDown) & df$CrossDown, ]

  p1 <- ggplot(df, aes(x = Date)) +
    geom_rect(aes(xmin = min(Date), xmax = max(Date),
                  ymin = min(Close, na.rm=TRUE)*0.995,
                  ymax = max(Close, na.rm=TRUE)*1.005),
              fill = rc, alpha = 0.04) +
    geom_line(aes(y = Close), color = PALETA$price, linewidth = 1.2) +
    geom_line(aes(y = SMA10), color = PALETA$sma10,  linewidth = 1.0, linetype = "dashed") +
    geom_line(aes(y = SMA20), color = PALETA$sma20,  linewidth = 1.0, linetype = "dotdash") +
    geom_line(aes(y = SMA50), color = PALETA$sma50,  linewidth = 1.0, linetype = "dotted") +
    { if (nrow(buy_pts)  > 0) geom_point(data = buy_pts,
        aes(y = Close), shape = 24, fill = PALETA$buy,  color = PALETA$buy,  size = 3) } +
    { if (nrow(sell_pts) > 0) geom_point(data = sell_pts,
        aes(y = Close), shape = 25, fill = PALETA$sell, color = PALETA$sell, size = 3) } +
    annotate("label", x = max(df$Date), y = max(df$Close, na.rm=TRUE),
             label = paste0("  ", rec, "  "), hjust = 1, vjust = 1,
             fill = rc, color = "white", fontface = "bold", size = 5,
             label.padding = unit(0.4, "lines"), label.size = 0) +
    scale_y_continuous(labels = label_comma()) +
    scale_x_date(date_labels = "%b %Y") +
    labs(title = sprintf("%s  (%s)  |  %s", info$nombre, ticker, info$tipo),
         y = sprintf("Precio (%s)", info$moneda), x = NULL) +
    theme_minimal(base_size = 10) +
    theme(
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = PALETA$bg, color = NA),
      panel.grid.major = element_line(color = PALETA$grid, linewidth = 0.4),
      panel.grid.minor = element_blank(),
      plot.title       = element_text(face = "bold", size = 12),
      axis.text.x      = element_blank(),
      axis.ticks.x     = element_blank()
    )

  # — Panel 2: RSI —
  p2 <- ggplot(df, aes(x = Date, y = RSI)) +
    geom_line(color = PALETA$rsi, linewidth = 1.2) +
    geom_ribbon(aes(ymin = ifelse(RSI >= 70, 70, NA), ymax = RSI),
                fill = PALETA$sell, alpha = 0.25, na.rm = TRUE) +
    geom_ribbon(aes(ymin = RSI, ymax = ifelse(RSI <= 30, 30, NA)),
                fill = PALETA$buy,  alpha = 0.25, na.rm = TRUE) +
    geom_hline(yintercept = c(30, 50, 70),
               linetype = c("dashed","dotted","dashed"),
               color    = c(PALETA$buy, "#95a5a6", PALETA$sell),
               linewidth = 0.8) +
    annotate("text", x = df$Date[2], y = 72,
             label = "Sobrecomprado (70)", hjust = 0, size = 2.8,
             color = PALETA$sell) +
    annotate("text", x = df$Date[2], y = 22,
             label = "Sobrevendido (30)", hjust = 0, size = 2.8,
             color = PALETA$buy) +
    scale_y_continuous(limits = c(0, 100)) +
    scale_x_date(date_labels = "%b %Y") +
    labs(y = "RSI (14)", x = NULL) +
    theme_minimal(base_size = 10) +
    theme(
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = PALETA$bg, color = NA),
      panel.grid.major = element_line(color = PALETA$grid, linewidth = 0.4),
      panel.grid.minor = element_blank(),
      axis.text.x      = element_blank(),
      axis.ticks.x     = element_blank()
    )

  # — Panel 3: Spread SMA10 − SMA20 —
  df$Spread      <- df$SMA10 - df$SMA20
  df$SpreadColor <- ifelse(!is.na(df$Spread) & df$Spread >= 0,
                           PALETA$buy, PALETA$sell)

  p3 <- ggplot(df, aes(x = Date, y = Spread, fill = SpreadColor)) +
    geom_col(alpha = 0.75, width = 0.8) +
    geom_hline(yintercept = 0, color = "#2c3e50", linewidth = 0.6) +
    scale_fill_identity() +
    scale_x_date(date_labels = "%b %Y") +
    labs(y = "SMA10 − SMA20", x = NULL) +
    theme_minimal(base_size = 10) +
    theme(
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = PALETA$bg, color = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = PALETA$grid, linewidth = 0.4),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 30, hjust = 1, size = 7)
    )

  # Combinar paneles con patchwork
  combined <- p1 / p2 / p3 +
    plot_layout(heights = c(3, 1.2, 1.2)) &
    theme(plot.margin = margin(2, 8, 2, 8))

  fname <- file.path(OUTPUT_DIR,
    paste0(gsub("[=\\-]", "_", ticker), "_chart.png"))
  ggsave(fname, plot = combined, width = 14, height = 9,
         dpi = 140, bg = "white")
  chart_files[[ticker]] <- fname
  cat(sprintf("  ✓ Gráfico: %s\n", fname))
}

# ─────────────────────────────────────────────
# 5. TABLA RESUMEN EN CONSOLA
# ─────────────────────────────────────────────
summary_data <- data.frame()

for (ticker in names(assets)) {
  info <- assets[[ticker]]
  df   <- info$df
  p0   <- df$Close[1]
  pf   <- df$Close[nrow(df)]
  rent <- (pf / p0 - 1) * 100

  summary_data <- rbind(summary_data, data.frame(
    Ticker    = ticker,
    Nombre    = info$nombre,
    Tipo      = info$tipo,
    P_Inicial = round(p0, 4),
    P_Final   = round(pf, 4),
    SMA10     = round(as.numeric(info$sma10_val), 4),
    SMA20     = round(as.numeric(info$sma20_val), 4),
    RSI       = round(as.numeric(info$rsi_val),   1),
    Rent_pct  = round(rent, 2),
    Rec       = info$rec,
    Score     = info$score,
    stringsAsFactors = FALSE
  ))
}

cat("\n", strrep("=", 95), "\n", sep="")
cat(sprintf("%-12s %-18s %11s %11s %11s %11s %7s %8s  %s\n",
            "TICKER", "NOMBRE", "P.INICIAL", "P.FINAL",
            "SMA10", "SMA20", "RSI", "RENT%", "RECOM."))
cat(strrep("=", 95), "\n")

for (i in seq_len(nrow(summary_data))) {
  r <- summary_data[i, ]
  cat(sprintf("%-12s %-18s %11.4f %11.4f %11.4f %11.4f %7.1f %7.2f%%  %s\n",
              r$Ticker, r$Nombre,
              r$P_Inicial, r$P_Final,
              r$SMA10, r$SMA20,
              r$RSI, r$Rent_pct, r$Rec))
}
cat(strrep("=", 95), "\n\n")

# ─────────────────────────────────────────────
# 6. GENERACIÓN DEL PDF
# ─────────────────────────────────────────────
output_pdf <- file.path(OUTPUT_DIR, "analisis_tecnico_R.pdf")
pdf(output_pdf, width = 8.27, height = 11.69)   # A4 en pulgadas

# ── PORTADA ──
grid.newpage()
grid.rect(gp = gpar(fill = "#1a252f", col = NA))
grid.rect(y = 0.63, height = 0.28, gp = gpar(fill = "#2980b9", col = NA))
grid.rect(y = 0.36, height = 0.004, gp = gpar(fill = "#27ae60", col = NA))
grid.rect(y = 0.905, height = 0.004, gp = gpar(fill = "#f39c12", col = NA))

grid.text("ANÁLISIS TÉCNICO",
          y = 0.72, gp = gpar(col = "white", fontface = "bold", cex = 2.4))
grid.text("5 Activos Financieros",
          y = 0.67, gp = gpar(col = "white", fontface = "bold", cex = 1.7))
grid.text("SMA10 · SMA20 · SMA50  +  RSI (14)  |  Datos via quantmod / Yahoo Finance",
          y = 0.615, gp = gpar(col = "#ecf0f1", cex = 0.9))
grid.text(sprintf("Análisis al %s  |  Horizonte: próximos 7 días",
                  format(Sys.Date(), "%d de %B de %Y")),
          y = 0.58, gp = gpar(col = "#bdc3c7", cex = 0.85))
grid.text("Este informe tiene fines educativos. No constituye asesoría financiera.",
          y = 0.04, gp = gpar(col = "#7f8c8d", cex = 0.7))

# Función para encabezado/pie de página en páginas internas
page_header_footer <- function(page_num) {
  grid.rect(y = 0.97, height = 0.03,
            gp = gpar(fill = "#2c3e50", col = NA))
  grid.text("Análisis Técnico — 5 Activos Financieros",
            x = 0.05, y = 0.985, just = "left",
            gp = gpar(col = "white", cex = 0.65, fontface = "bold"))
  grid.text(format(Sys.Date(), "%d/%m/%Y"),
            x = 0.95, y = 0.985, just = "right",
            gp = gpar(col = "white", cex = 0.65))
  grid.lines(x = c(0.03, 0.97), y = c(0.025, 0.025),
             gp = gpar(col = "#bdc3c7", lwd = 0.5))
  grid.text(sprintf("Página %d", page_num),
            x = 0.97, y = 0.013, just = "right",
            gp = gpar(col = "#95a5a6", cex = 0.6))
  grid.text(sprintf("Análisis Técnico · 5 Activos · %s",
                    format(Sys.Date(), "%d/%m/%Y")),
            x = 0.03, y = 0.013, just = "left",
            gp = gpar(col = "#95a5a6", cex = 0.6))
}

# ── PÁGINA 1: INTRODUCCIÓN Y METODOLOGÍA ──
grid.newpage()
page_header_footer(1)
vp <- viewport(x=0.5, y=0.5, width=0.9, height=0.88,
               just=c("centre","centre"))
pushViewport(vp)

grid.text("1. Introducción y Metodología",
          x = 0, y = 0.97, just = "left",
          gp = gpar(fontface = "bold", cex = 1.1, col = "#2c3e50"))
grid.lines(x = c(0, 1), y = c(0.94, 0.94),
           gp = gpar(col = "#2980b9", lwd = 2))

intro_text <- paste0(
  "Este informe descarga datos reales de mercado vía quantmod (Yahoo Finance) ",
  "y aplica análisis técnico con medias móviles simples (SMA10, SMA20, SMA50) ",
  "y el RSI de 14 períodos. La recomendación de compra/venta/neutral se calcula ",
  "mediante una puntuación compuesta que integra cruces de medias, alineación ",
  "de tendencia y niveles del RSI. Datos descargados el ",
  format(Sys.Date(), "%d/%m/%Y"), "."
)
grid.text(intro_text, x = 0, y = 0.89, just = c("left","top"),
          gp = gpar(cex = 0.78, col = "#1a252f"),
          vp = viewport(width = 1))

# Tabla de indicadores
ind_y    <- 0.74
row_h    <- 0.055
col_x    <- c(0, 0.24, 0.42, 1)
headers  <- c("Indicador", "Período", "Descripción")
ind_rows <- list(
  c("SMA 10", "10 sesiones", "Momentum de corto plazo / señales de cruce"),
  c("SMA 20", "20 sesiones", "Soporte/resistencia dinámico de medio plazo"),
  c("SMA 50", "50 sesiones", "Referencia de tendencia estructural"),
  c("RSI",    "14 sesiones", "Sobrecompra >70 · Neutral 30-70 · Sobreventa <30")
)

# encabezado tabla
grid.rect(x = 0.5, y = ind_y, width = 1, height = row_h,
          gp = gpar(fill = "#2c3e50", col = NA))
for (j in seq_along(headers)) {
  grid.text(headers[j],
            x = col_x[j] + 0.01, y = ind_y,
            just = "left",
            gp = gpar(col = "white", fontface = "bold", cex = 0.72))
}
for (i in seq_along(ind_rows)) {
  ry   <- ind_y - i * row_h
  fill <- if (i %% 2 == 0) "#ecf0f1" else "white"
  grid.rect(x = 0.5, y = ry, width = 1, height = row_h,
            gp = gpar(fill = fill, col = "#bdc3c7", lwd = 0.4))
  for (j in 1:3) {
    grid.text(ind_rows[[i]][j],
              x = col_x[j] + 0.01, y = ry,
              just = "left",
              gp = gpar(col = "#1a252f", cex = 0.70))
  }
}

popViewport()

# ── UNA PÁGINA POR ACTIVO ──
rsi_label <- function(r) {
  if (is.na(r)) return("N/D")
  if (r < 30)   return("Sobrevendido — posible rebote")
  if (r > 70)   return("Sobrecomprado — posible corrección")
  if (r > 60)   return("Momentum alcista moderado")
  if (r < 40)   return("Momentum bajista moderado")
  "Zona neutra"
}
rec_desc <- list(
  COMPRA  = "Los indicadores técnicos apuntan a tendencia alcista. Se recomienda considerar posición larga durante la próxima semana.",
  VENTA   = "Los indicadores reflejan presión bajista. Se recomienda posición corta o liquidar posiciones largas.",
  NEUTRAL = "Sin señal clara. Se recomienda esperar confirmación antes de entrar al mercado."
)
REC_COLORS <- list(COMPRA="#27ae60", VENTA="#e74c3c", NEUTRAL="#f39c12")

for (i in seq_len(nrow(summary_data))) {
  row    <- summary_data[i, ]
  ticker <- row$Ticker
  info   <- assets[[ticker]]
  rc_hex <- REC_COLORS[[row$Rec]]

  grid.newpage()
  page_header_footer(i + 1)

  vp2 <- viewport(x=0.5, y=0.48, width=0.9, height=0.88,
                  just=c("centre","centre"))
  pushViewport(vp2)

  # Título del activo
  grid.text(sprintf("2.%d  %s  (%s)", i, info$nombre, ticker),
            x=0, y=0.97, just="left",
            gp=gpar(fontface="bold", cex=1.05, col="#2c3e50"))
  grid.lines(x=c(0,1), y=c(0.94,0.94),
             gp=gpar(col="#2980b9", lwd=2))

  # Sub-cabecera: Clase / Moneda / Recomendación
  subh_y <- 0.91; subh_h <- 0.045
  cols_sh <- c(0, 0.28, 0.48, 0.73, 1)
  labels_sh <- c(sprintf("Clase: %s", info$tipo),
                 sprintf("Moneda: %s", info$moneda),
                 "Recomendación:", row$Rec)
  for (j in seq_along(labels_sh)) {
    fill_j <- if (j == 4) rc_hex else "transparent"
    col_j  <- if (j == 4) "white" else "#1a252f"
    fw_j   <- if (j == 4) "bold" else "plain"
    if (j == 4) {
      grid.rect(x = (cols_sh[j]+cols_sh[j+1])/2,
                y = subh_y,
                width  = cols_sh[j+1]-cols_sh[j],
                height = subh_h,
                gp = gpar(fill=fill_j, col=NA))
    }
    grid.text(labels_sh[j],
              x = (cols_sh[j]+cols_sh[j+1])/2, y = subh_y,
              gp = gpar(col=col_j, fontface=fw_j, cex=0.78))
  }

  # Gráfico del activo
  img_file <- chart_files[[ticker]]
  if (!is.null(img_file) && file.exists(img_file)) {
    img <- png::readPNG(img_file)
    grid.raster(img, x=0.5, y=0.60, width=1, height=0.42)
    grid.text(
      sprintf("Figura %d. Precio, SMA10/20/50 y RSI (14) para %s. Triángulos = señales de cruce. Histograma = spread SMA10-SMA20.", i, ticker),
      x=0.5, y=0.375,
      gp=gpar(col="#7f8c8d", cex=0.65, fontface="italic"))
  }

  # Tabla de métricas
  grid.text("Métricas del período", x=0, y=0.345, just="left",
            gp=gpar(fontface="bold", cex=0.88, col="#2980b9"))
  grid.lines(x=c(0,1), y=c(0.325,0.325),
             gp=gpar(col="#2980b9", lwd=1))

  met_rows <- list(
    c("Métrica",       "Valor",                           "Interpretación"),
    c("Precio Inicial",sprintf("$%s", formatC(row$P_Inicial, format="f", digits=4, big.mark=",")), "Primer precio del período descargado"),
    c("Precio Final",  sprintf("$%s", formatC(row$P_Final,   format="f", digits=4, big.mark=",")), "Último precio de cierre disponible"),
    c("Rentabilidad",  sprintf("%+.2f%%", row$Rent_pct),   "Variación % acumulada en el período"),
    c("SMA 10",        sprintf("$%s", formatC(row$SMA10,    format="f", digits=4, big.mark=",")), "Media móvil corto plazo"),
    c("SMA 20",        sprintf("$%s", formatC(row$SMA20,    format="f", digits=4, big.mark=",")), "Media móvil medio plazo"),
    c("RSI (14)",      sprintf("%.1f", row$RSI),           rsi_label(row$RSI)),
    c("Score técnico", sprintf("%+d / 8", row$Score),      ">+3=COMPRA · <-3=VENTA · entre=NEUTRAL")
  )

  met_col_x <- c(0, 0.27, 0.46, 1)
  met_y0    <- 0.31
  met_rh    <- 0.038

  for (k in seq_along(met_rows)) {
    ry   <- met_y0 - (k-1)*met_rh
    fill <- if (k == 1) "#2c3e50" else if (k %% 2 == 0) "#ecf0f1" else "white"
    col  <- if (k == 1) "white" else "#1a252f"
    fw   <- if (k == 1) "bold" else "plain"
    grid.rect(x=0.5, y=ry, width=1, height=met_rh,
              gp=gpar(fill=fill, col="#bdc3c7", lwd=0.4))
    for (j in 1:3) {
      # Color especial para Rentabilidad y RSI
      txt_col <- col
      txt_fw  <- fw
      if (k == 4 && j == 2) {  # Rentabilidad valor
        txt_col <- if (row$Rent_pct >= 0) "#27ae60" else "#e74c3c"
        txt_fw  <- "bold"
      }
      if (k == 7 && j == 2) {  # RSI valor
        txt_col <- if (row$RSI > 70) "#e74c3c" else if (row$RSI < 30) "#27ae60" else "#1a252f"
        txt_fw  <- "bold"
      }
      grid.text(met_rows[[k]][j],
                x=met_col_x[j]+0.01, y=ry,
                just="left",
                gp=gpar(col=txt_col, fontface=txt_fw, cex=0.68))
    }
  }

  # Interpretación
  interp_y <- met_y0 - length(met_rows)*met_rh - 0.02
  grid.text("Interpretación", x=0, y=interp_y, just="left",
            gp=gpar(fontface="bold", cex=0.88, col="#2980b9"))
  grid.text(rec_desc[[row$Rec]],
            x=0, y=interp_y-0.04, just=c("left","top"),
            gp=gpar(col="#1a252f", cex=0.74),
            vp=viewport(width=1))

  popViewport()
}

# ── RESUMEN COMPARATIVO ──
grid.newpage()
page_header_footer(nrow(summary_data) + 2)

vp3 <- viewport(x=0.5, y=0.48, width=0.9, height=0.88, just=c("centre","centre"))
pushViewport(vp3)

grid.text("3. Resumen Comparativo — 5 Activos",
          x=0, y=0.97, just="left",
          gp=gpar(fontface="bold", cex=1.05, col="#2c3e50"))
grid.lines(x=c(0,1), y=c(0.94,0.94), gp=gpar(col="#2980b9", lwd=2))
grid.text(sprintf("Consolidado de indicadores técnicos al %s. Horizonte: próximos 7 días.",
                  format(Sys.Date(),"%d/%m/%Y")),
          x=0, y=0.91, just="left",
          gp=gpar(col="#1a252f", cex=0.78))

tbl_headers <- c("Activo","Nombre","Tipo","P.Inicial","P.Final","SMA10","SMA20","RSI","Rent.%","Recom.")
col_xr <- c(0, 0.09, 0.24, 0.38, 0.50, 0.62, 0.73, 0.82, 0.88, 0.94, 1)
tbl_y0 <- 0.875
tbl_rh <- 0.052

# Header
grid.rect(x=0.5, y=tbl_y0, width=1, height=tbl_rh,
          gp=gpar(fill="#2c3e50", col=NA))
for (j in seq_along(tbl_headers)) {
  grid.text(tbl_headers[j],
            x=col_xr[j]+0.005, y=tbl_y0, just="left",
            gp=gpar(col="white", fontface="bold", cex=0.60))
}

for (i in seq_len(nrow(summary_data))) {
  row  <- summary_data[i, ]
  ry   <- tbl_y0 - i*tbl_rh
  fill <- if (i %% 2 == 0) "#ecf0f1" else "white"
  grid.rect(x=0.5, y=ry, width=1, height=tbl_rh,
            gp=gpar(fill=fill, col="#bdc3c7", lwd=0.4))

  vals <- c(
    row$Ticker,
    substr(row$Nombre, 1, 14),
    substr(row$Tipo, 1, 12),
    sprintf("$%.2f", row$P_Inicial),
    sprintf("$%.2f", row$P_Final),
    sprintf("$%.2f", row$SMA10),
    sprintf("$%.2f", row$SMA20),
    sprintf("%.1f",  row$RSI),
    sprintf("%+.2f%%", row$Rent_pct),
    ""  # Rec se pinta aparte
  )
  for (j in seq_along(vals)) {
    col_j <- "#1a252f"
    fw_j  <- "plain"
    if (j == 9) col_j <- if (row$Rent_pct >= 0) "#27ae60" else "#e74c3c"; fw_j <- "bold"
    grid.text(vals[j], x=col_xr[j]+0.005, y=ry, just="left",
              gp=gpar(col=col_j, fontface=fw_j, cex=0.58))
  }
  # Celda Recomendación coloreada
  rc_hex <- REC_COLORS[[row$Rec]]
  mid_x  <- (col_xr[10]+col_xr[11])/2
  grid.rect(x=mid_x, y=ry,
            width=col_xr[11]-col_xr[10], height=tbl_rh,
            gp=gpar(fill=rc_hex, col=NA))
  grid.text(row$Rec, x=mid_x, y=ry,
            gp=gpar(col="white", fontface="bold", cex=0.58))
}

popViewport()

# ── NOTAS ──
grid.newpage()
page_header_footer(nrow(summary_data) + 3)
vp4 <- viewport(x=0.5, y=0.48, width=0.9, height=0.88, just=c("centre","centre"))
pushViewport(vp4)

grid.text("4. Notas", x=0, y=0.97, just="left",
          gp=gpar(fontface="bold", cex=1.05, col="#2c3e50"))
grid.lines(x=c(0,1), y=c(0.94,0.94), gp=gpar(col="#2980b9", lwd=2))

notas <- c(
  "Datos descargados en tiempo real via quantmod / Yahoo Finance. Los precios pueden diferir levemente de distintas fuentes.",
  "El análisis técnico identifica patrones históricos pero no garantiza rentabilidad futura.",
  "RSI y medias móviles son indicadores rezagados. Combínelos con análisis fundamental.",
  "Establezca niveles de stop-loss antes de ejecutar cualquier operación.",
  "ESTE INFORME TIENE FINES EDUCATIVOS Y NO CONSTITUYE ASESORÍA FINANCIERA PROFESIONAL."
)
for (k in seq_along(notas)) {
  fw <- if (k == 5) "bold" else "plain"
  grid.text(paste0("⚠  ", notas[k]),
            x=0.01, y=0.90 - (k-1)*0.07,
            just=c("left","top"),
            gp=gpar(col="#1a252f", cex=0.76, fontface=fw),
            vp=viewport(width=0.98))
}

grid.text(sprintf("Informe generado el %s", format(Sys.time(), "%d/%m/%Y %H:%M")),
          x=0.5, y=0.45,
          gp=gpar(col="#7f8c8d", cex=0.72, fontface="italic"))

popViewport()

dev.off()

cat(sprintf("\n✓ PDF generado: %s\n", output_pdf))
cat("  Ábrelo con cualquier visor de PDF (Acrobat, etc.)\n")
