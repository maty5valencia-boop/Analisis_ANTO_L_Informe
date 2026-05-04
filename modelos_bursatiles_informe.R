# ============================================================
# MODELOS BURSATILES — IPSA Y MERCADOS REGIONALES
# Genera 3 gráficos PNG + informe PDF automáticamente
# ============================================================
# Instalar paquetes si no están disponibles:
# install.packages(c("readxl","dplyr","tidyr","lubridate",
#                    "ggplot2","gridExtra","grid","scales","patchwork"))
# ============================================================

library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(grid)
library(scales)
library(patchwork)

# ============================================================
# CONFIGURACIÓN
# ============================================================
ARCHIVO    <- r"(C:\Users\crist\Downloads\Datos_bursatiles_704551.xlsx)"
DIR_SALIDA <- dirname(normalizePath(ARCHIVO, mustWork = FALSE))

IMG1       <- file.path(DIR_SALIDA, "modelo1_ipsa_fundamentales.png")
IMG2       <- file.path(DIR_SALIDA, "modelo2_capm_residuos.png")
IMG3       <- file.path(DIR_SALIDA, "modelo3_fed_model.png")
PDF_SALIDA <- file.path(DIR_SALIDA, "informe_modelos_bursatiles.pdf")

# Colores corporativos
AZUL    <- "#1B3A6B"
ROJO    <- "#E74C3C"
NARANJA <- "#E67E22"
VERDE   <- "#27AE60"
GRIS    <- "#2C3E50"
GRIS_CL <- "#F5F5F5"

cat("=======================================================\n")
cat("MODELOS BURSATILES — Inicio del proceso\n")
cat("=======================================================\n\n")

# ============================================================
# CARGA DE DATOS
# ============================================================
cat("Cargando datos...\n")

# --- Bolsas (moneda local) ---
bolsas_raw <- read_excel(ARCHIVO, sheet = "Bolsas", col_names = FALSE)
countries  <- as.character(bolsas_raw[10, 2:ncol(bolsas_raw)])
bolsas_raw2 <- bolsas_raw[11:nrow(bolsas_raw), ]
colnames(bolsas_raw2) <- c("Fecha", countries)
bolsas <- bolsas_raw2 %>%
  mutate(Fecha = as.Date(as.numeric(Fecha), origin = "1899-12-30")) %>%
  filter(!is.na(Fecha)) %>%
  mutate(across(-Fecha, as.numeric)) %>%
  arrange(Fecha)

# --- Bolsas USD ---
bolsas_usd_raw <- read_excel(ARCHIVO, sheet = "Bolsas (USD)", col_names = FALSE)
bolsas_usd_raw2 <- bolsas_usd_raw[11:nrow(bolsas_usd_raw), ]
colnames(bolsas_usd_raw2) <- c("Fecha", countries)
bolsas_usd <- bolsas_usd_raw2 %>%
  mutate(Fecha = as.Date(as.numeric(Fecha), origin = "1899-12-30")) %>%
  filter(!is.na(Fecha)) %>%
  mutate(across(-Fecha, as.numeric)) %>%
  arrange(Fecha)

# --- Otros Financieros ---
fin_raw <- read_excel(ARCHIVO, sheet = "Otros Financieros", col_names = FALSE)
fin_raw2 <- fin_raw[11:nrow(fin_raw), 1:4]
colnames(fin_raw2) <- c("Fecha", "BCU10", "PE_IPSA", "MXEF")
fin <- fin_raw2 %>%
  mutate(Fecha = as.Date(as.numeric(Fecha), origin = "1899-12-30")) %>%
  filter(!is.na(Fecha)) %>%
  mutate(across(-Fecha, as.numeric)) %>%
  arrange(Fecha)

# --- Macro ---
macro_raw <- read_excel(ARCHIVO, sheet = "Otros macroeconomicos", col_names = FALSE)
macro_raw2 <- macro_raw[7:nrow(macro_raw), 1:4]
colnames(macro_raw2) <- c("Fecha", "Cobre", "Petroleo", "IMACEC")
macro <- macro_raw2 %>%
  mutate(Fecha = as.Date(as.numeric(Fecha), origin = "1899-12-30")) %>%
  filter(!is.na(Fecha)) %>%
  mutate(across(-Fecha, as.numeric)) %>%
  arrange(Fecha)

cat("  Datos cargados correctamente.\n\n")

# ============================================================
# FUNCIÓN OLS
# ============================================================
ols_fit <- function(y, X_mat) {
  Xm     <- cbind(1, X_mat)
  beta   <- as.numeric(solve(t(Xm) %*% Xm) %*% t(Xm) %*% y)
  fitted <- as.numeric(Xm %*% beta)
  resid  <- y - fitted
  n      <- nrow(Xm); k <- ncol(Xm)
  s2     <- sum(resid^2) / (n - k)
  se     <- sqrt(s2)
  se_b   <- sqrt(diag(solve(t(Xm) %*% Xm)) * s2)
  t_stat <- beta / se_b
  t_crit <- qt(0.975, df = n - k)
  list(beta = beta, fitted = fitted, resid = resid,
       se = se, t_stat = t_stat, t_crit = t_crit, n = n, k = k)
}

# ============================================================
# MODELO 1: IPSA ~ COBRE + PETROLEO + IMACEC (log-log)
# ============================================================
cat("=======================================================\n")
cat("MODELO 1 — IPSA por Fundamentales Economicos\n")
cat("=======================================================\n")

# Mensualizar IPSA (ultimo valor del mes)
ipsa_m <- bolsas %>%
  select(Fecha, Chile) %>%
  filter(!is.na(Chile)) %>%
  mutate(YM = floor_date(Fecha, "month")) %>%
  group_by(YM) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  rename(Fecha = YM, IPSA = Chile) %>%
  select(Fecha, IPSA)

macro_m <- macro %>%
  mutate(YM = floor_date(Fecha, "month")) %>%
  group_by(YM) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  rename(Fecha = YM) %>%
  select(Fecha, Cobre, Petroleo, IMACEC)

df1 <- inner_join(ipsa_m, macro_m, by = "Fecha") %>%
  filter(!is.na(IPSA), !is.na(Cobre), !is.na(Petroleo), !is.na(IMACEC)) %>%
  mutate(
    log_ipsa   = log(IPSA),
    log_cobre  = log(Cobre),
    log_petro  = log(Petroleo),
    log_imacec = log(IMACEC)
  )

X1   <- cbind(df1$log_cobre, df1$log_petro, df1$log_imacec)
m1   <- ols_fit(df1$log_ipsa, X1)

df1$fitted  <- m1$fitted
df1$resid   <- m1$resid
df1$fitted_nivel <- exp(m1$fitted)
df1$ci_up   <- exp(m1$fitted + 1.96 * m1$se)
df1$ci_dn   <- exp(m1$fitted - 1.96 * m1$se)

# R²
ss_res1 <- sum(m1$resid^2)
ss_tot1 <- sum((df1$log_ipsa - mean(df1$log_ipsa))^2)
r2_m1   <- 1 - ss_res1 / ss_tot1

# Diagnóstico últimos 12 meses
last12_1  <- tail(df1$resid, 12)
t_rec1    <- mean(last12_1) / (m1$se / sqrt(12))
sig_rec1  <- abs(t_rec1) > m1$t_crit
dir1      <- ifelse(mean(last12_1) > 0, "SOBRE", "BAJO")
color_rec1 <- ifelse(sig_rec1, ROJO, NARANJA)

nombres1 <- c("Constante", "log(Cobre)", "log(Petroleo)", "log(IMACEC)")
for (i in seq_along(m1$beta)) {
  sig_txt <- ifelse(abs(m1$t_stat[i]) > m1$t_crit, "✓ sig", "✗ no sig")
  cat(sprintf("  %-18s  b = %+.4f   t = %+.2f   %s\n",
              nombres1[i], m1$beta[i], m1$t_stat[i], sig_txt))
}
cat(sprintf("\n  Ultimos 12 meses:\n"))
cat(sprintf("    Residuo promedio : %+.4f\n", mean(last12_1)))
cat(sprintf("    t-estadistico    : %+.2f\n", t_rec1))
cat(sprintf("    Conclusion       : IPSA %s valor fundamental\n", dir1))
cat(sprintf("    Significancia 5%% : %s\n\n",
            ifelse(sig_rec1, "✓ SI es significativo", "✗ NO es significativo")))

fecha_rec1_ini <- tail(df1$Fecha, 12)[1]
fecha_rec1_fin <- tail(df1$Fecha, 1)

# Gráfico Modelo 1
p1a <- ggplot(df1, aes(x = Fecha)) +
  annotate("rect", xmin = fecha_rec1_ini, xmax = fecha_rec1_fin,
           ymin = -Inf, ymax = Inf, fill = color_rec1, alpha = 0.08) +
  geom_ribbon(aes(ymin = ci_dn, ymax = ci_up), fill = ROJO, alpha = 0.12) +
  geom_line(aes(y = IPSA, color = "IPSA Observado"), linewidth = 0.9) +
  geom_line(aes(y = fitted_nivel, color = "IPSA Estimado"),
            linewidth = 0.8, linetype = "dashed") +
  scale_color_manual(values = c("IPSA Observado" = AZUL, "IPSA Estimado" = ROJO)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = comma) +
  labs(title = "Nivel Observado vs. Estimado",
       y = "Puntos", x = NULL, color = NULL) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title    = element_text(color = GRIS, face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90")
  )

bar_colors1 <- ifelse(df1$resid > 0, ROJO, AZUL)
p1b <- ggplot(df1, aes(x = Fecha, y = resid)) +
  annotate("rect", xmin = fecha_rec1_ini, xmax = fecha_rec1_fin,
           ymin = -Inf, ymax = Inf, fill = color_rec1, alpha = 0.08) +
  geom_col(fill = bar_colors1, alpha = 0.75, width = 20) +
  geom_hline(yintercept = 0,           color = "black",  linewidth = 0.6) +
  geom_hline(yintercept =  1.96*m1$se, color = NARANJA, linewidth = 0.8, linetype = "dashed") +
  geom_hline(yintercept = -1.96*m1$se, color = NARANJA, linewidth = 0.8, linetype = "dashed") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = sprintf("Residuos  |  Ult.12m: IPSA %s fundamental  |  t=%.2f  |  %s",
                    dir1, t_rec1, ifelse(sig_rec1, "Sig. 5%", "No sig. 5%")),
    y = "Residuo (log)", x = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title       = element_text(color = color_rec1, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90")
  )

fig1 <- p1a / p1b +
  plot_annotation(
    title    = "Modelo 1: IPSA — Determinacion por Fundamentales Economicos",
    subtitle = "log(IPSA) = a + b1*log(Cobre) + b2*log(Petroleo) + b3*log(IMACEC SA)",
    theme    = theme(
      plot.title    = element_text(color = AZUL, face = "bold", size = 13),
      plot.subtitle = element_text(color = GRIS, size = 10)
    )
  )

ggsave(IMG1, fig1, width = 13, height = 9, dpi = 150, bg = "white")
cat(sprintf("  Guardado: %s\n\n", IMG1))

# ============================================================
# MODELO 2: CAPM REGIONAL
# ============================================================
cat("=======================================================\n")
cat("MODELO 2 — CAPM Regional (vs. MSCI Emergentes USD)\n")
cat("=======================================================\n")

mxef_m <- fin %>%
  select(Fecha, MXEF) %>%
  filter(!is.na(MXEF)) %>%
  mutate(YM = floor_date(Fecha, "month")) %>%
  group_by(YM) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  rename(Fecha = YM) %>%
  select(Fecha, MXEF)

paises <- c("Chile", "Argentina", "Brasil", "Colombia",
            "Mexico", "Peru", "EE.UU. (S&P 500)",
            "China", "India", "Corea", "Alemania", "Japon")

# Variantes con tildes
paises_check <- sapply(paises, function(p) {
  if (p %in% names(bolsas_usd)) return(p)
  alts <- c("México"="Mexico","Perú"="Peru","Japón"="Japon")
  rev_alts <- setNames(names(alts), alts)
  if (p %in% names(rev_alts) && rev_alts[p] %in% names(bolsas_usd))
    return(rev_alts[p])
  return(NA)
}, USE.NAMES = FALSE)
paises_validos <- paises_check[!is.na(paises_check)]

capm_pais <- function(pais_col) {
  idx_m <- bolsas_usd %>%
    select(Fecha, all_of(pais_col)) %>%
    rename(idx = all_of(pais_col)) %>%
    filter(!is.na(idx)) %>%
    mutate(YM = floor_date(Fecha, "month")) %>%
    group_by(YM) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    rename(Fecha = YM) %>%
    select(Fecha, idx)

  df <- inner_join(idx_m, mxef_m, by = "Fecha") %>%
    filter(!is.na(idx), !is.na(MXEF)) %>%
    mutate(log_idx = log(idx), log_ref = log(MXEF))

  if (nrow(df) < 30) return(NULL)

  m <- ols_fit(df$log_idx, matrix(df$log_ref, ncol = 1))
  df$resid  <- m$resid
  t_last    <- tail(m$resid, 1) / m$se
  sig       <- abs(t_last) > m$t_crit
  ss_res    <- sum(m$resid^2)
  ss_tot    <- sum((df$log_idx - mean(df$log_idx))^2)
  r2        <- 1 - ss_res / ss_tot
  direction <- ifelse(tail(m$resid, 1) > 0, "Sobrevalorado", "Subvalorado")

  list(beta = m$beta, resid_df = df[, c("Fecha","resid")],
       se = m$se, t_last = t_last, sig = sig,
       direction = direction, r2 = r2)
}

resultados_capm <- list()
for (p in paises_validos) {
  r <- capm_pais(p)
  if (!is.null(r)) {
    resultados_capm[[p]] <- r
    sig_str <- ifelse(r$sig, "✓ sig 5%", "✗ no sig")
    cat(sprintf("  %-22s  b=%.3f  t_ultimo=%+.2f  %-15s  %s\n",
                p, r$beta[2], r$t_last, r$direction, sig_str))
  }
}

# Gráfico Modelo 2 — panel de subplots
n_paises <- length(resultados_capm)
ncols    <- 3
nrows    <- ceiling(n_paises / ncols)

plots_m2 <- lapply(names(resultados_capm), function(pais) {
  r      <- resultados_capm[[pais]]
  df_r   <- r$resid_df
  se     <- r$se
  bar_colors2 <- ifelse(df_r$resid > 0, ROJO, AZUL)
  sig_str <- ifelse(r$sig, "Sig 5%", "No sig")

  ggplot(df_r, aes(x = Fecha, y = resid)) +
    geom_ribbon(aes(ymin = -1.96*se, ymax = 1.96*se),
                fill = "grey70", alpha = 0.06) +
    geom_col(fill = bar_colors2, alpha = 0.65, width = 20) +
    geom_hline(yintercept = 0,          color = "black",  linewidth = 0.5) +
    geom_hline(yintercept =  1.96*se,   color = NARANJA,  linewidth = 0.7, linetype = "dashed") +
    geom_hline(yintercept = -1.96*se,   color = NARANJA,  linewidth = 0.7, linetype = "dashed") +
    geom_point(data = tail(df_r, 1), aes(x = Fecha, y = resid),
               color = ifelse(tail(df_r$resid, 1) > 0, ROJO, AZUL), size = 2.5) +
    scale_x_date(date_breaks = "4 years", date_labels = "%y") +
    labs(
      title = sprintf("%s\nb=%.2f | %s | %s",
                      pais, r$beta[2], r$direction, sig_str),
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 7.5) +
    theme(
      plot.title       = element_text(color = AZUL, face = "bold", size = 7.5, lineheight = 1.2),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey92")
    )
})

fig2 <- wrap_plots(plots_m2, ncol = ncols) +
  plot_annotation(
    title    = "Modelo 2: Componente No Explicado por MSCI Emergentes (USD)",
    subtitle = "log(Indice USD) = a + b*log(MSCI EM)  |  Rojo: Sobrevaloración  |  Azul: Subvaloración",
    theme    = theme(
      plot.title    = element_text(color = AZUL, face = "bold", size = 13),
      plot.subtitle = element_text(color = GRIS, size = 9)
    )
  )

ggsave(IMG2, fig2, width = 16, height = nrows * 3.8 + 1, dpi = 150, bg = "white")
cat(sprintf("\n  Guardado: %s\n\n", IMG2))

# ============================================================
# MODELO 3: FED MODEL
# ============================================================
cat("=======================================================\n")
cat("MODELO 3 — Fed Model (BCU10 vs. Earnings Yield IPSA)\n")
cat("=======================================================\n")

df3 <- fin %>%
  select(Fecha, PE_IPSA, BCU10) %>%
  filter(!is.na(PE_IPSA), !is.na(BCU10)) %>%
  mutate(
    UP     = (1 / PE_IPSA) * 100,
    spread = BCU10 - UP
  )

mu3  <- mean(df3$spread, na.rm = TRUE)
std3 <- sd(df3$spread,   na.rm = TRUE)
ult3 <- tail(df3$spread, 1)

if (ult3 > mu3 + std3) {
  zona3       <- "SOBRE el rango: acciones caras relativo a bonos"
  color_zona3 <- ROJO
} else if (ult3 < mu3 - std3) {
  zona3       <- "BAJO el rango: acciones baratas relativo a bonos"
  color_zona3 <- VERDE
} else {
  zona3       <- "DENTRO del rango +/-1s: valoracion neutra"
  color_zona3 <- NARANJA
}

cat(sprintf("  Media historica  : %.2f%%\n", mu3))
cat(sprintf("  Desv. estandar   : %.2f%%\n", std3))
cat(sprintf("  Rango +/-1s      : [%.2f%%, %.2f%%]\n", mu3 - std3, mu3 + std3))
cat(sprintf("  Ultimo spread    : %.2f%%\n", ult3))
cat(sprintf("  Conclusion       : %s\n\n", zona3))

p3a <- ggplot(df3, aes(x = Fecha)) +
  geom_line(aes(y = BCU10, color = "BCU 10 años (%)"), linewidth = 0.9) +
  geom_line(aes(y = UP,    color = "Earnings Yield IPSA (%)"),
            linewidth = 0.9, linetype = "dashed") +
  scale_color_manual(values = c("BCU 10 años (%)" = AZUL,
                                "Earnings Yield IPSA (%)" = ROJO)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(title = "Tasa Real (BCU10) y Earnings Yield del IPSA",
       y = "% anual", x = NULL, color = NULL) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title       = element_text(color = GRIS, face = "bold"),
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90")
  )

ultimo_punto <- tail(df3, 1)
p3b <- ggplot(df3, aes(x = Fecha, y = spread)) +
  geom_ribbon(aes(ymin = mu3 - std3, ymax = mu3 + std3),
              fill = NARANJA, alpha = 0.10) +
  geom_line(color = "#8E44AD", linewidth = 0.9) +
  geom_hline(yintercept = mu3,          color = "black",  linewidth = 0.8) +
  geom_hline(yintercept = mu3 + std3,   color = NARANJA,  linewidth = 0.8, linetype = "dashed") +
  geom_hline(yintercept = mu3 - std3,   color = NARANJA,  linewidth = 0.8, linetype = "dashed") +
  geom_point(data = ultimo_punto, aes(x = Fecha, y = spread),
             color = color_zona3, size = 3.5, shape = 16) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = sprintf("Spread y Bandas +/-1s  |  %s", zona3),
    y = "Diferencia (p.p.)", x = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title       = element_text(color = color_zona3, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90")
  )

fig3 <- p3a / p3b +
  plot_annotation(
    title    = "Modelo 3: Fed Model — BCU 10 años vs. Earnings Yield (U/P) del IPSA",
    theme    = theme(
      plot.title = element_text(color = AZUL, face = "bold", size = 13)
    )
  )

ggsave(IMG3, fig3, width = 13, height = 9, dpi = 150, bg = "white")
cat(sprintf("  Guardado: %s\n\n", IMG3))

# ============================================================
# INFORME PDF
# ============================================================
cat("Generando informe PDF...\n")

# Intentar cargar paquetes PDF; instalar si faltan
for (pkg in c("grid", "gridExtra", "png")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

fecha_hoy <- format(Sys.Date(), "%d de %B de %Y")

# R² Modelo 2 Chile
r2_chile <- ifelse("Chile" %in% names(resultados_capm),
                   resultados_capm[["Chile"]]$r2, NA)
t_chile  <- ifelse("Chile" %in% names(resultados_capm),
                   resultados_capm[["Chile"]]$t_last, NA)
dir_chile <- ifelse("Chile" %in% names(resultados_capm),
                    resultados_capm[["Chile"]]$direction, "N/D")
sig_chile <- ifelse("Chile" %in% names(resultados_capm),
                    resultados_capm[["Chile"]]$sig, FALSE)

# Helpers para construir páginas con grid
nueva_pagina <- function() grid.newpage()

texto_titulo <- function(txt, y, size = 14, color = AZUL, bold = TRUE, just = "centre") {
  grid.text(txt, x = 0.5, y = y, gp = gpar(
    fontsize = size, col = color,
    fontface = ifelse(bold, "bold", "plain")
  ), just = just)
}

texto_body <- function(txt, x, y, size = 9, color = GRIS, just = "left", width = 0.88) {
  grid.text(txt, x = x, y = y,
            gp = gpar(fontsize = size, col = color),
            just = just)
}

linea_hr <- function(y, color = AZUL) {
  grid.lines(x = c(0.06, 0.94), y = c(y, y),
             gp = gpar(col = color, lwd = 1.5))
}

caja_color <- function(y, h, color = "#FEF3E2", borde = NARANJA) {
  grid.rect(x = 0.5, y = y, width = 0.88, height = h,
            gp = gpar(fill = color, col = borde, lwd = 1.5))
}

tabla_grid <- function(df_tab, y_top, col_widths, row_h = 0.032,
                        header_bg = AZUL, header_col = "white",
                        stripe_col = GRIS_CL) {
  nr <- nrow(df_tab); nc <- ncol(df_tab)
  x_starts <- c(0.06, 0.06 + cumsum(col_widths[-length(col_widths)]))
  # header
  grid.rect(x = 0.5, y = y_top - row_h/2, width = 0.88, height = row_h,
            gp = gpar(fill = header_bg, col = NA))
  for (j in 1:nc) {
    grid.text(names(df_tab)[j],
              x = x_starts[j] + col_widths[j]/2,
              y = y_top - row_h/2,
              gp = gpar(fontsize = 8, col = header_col, fontface = "bold"),
              just = "centre")
  }
  for (i in 1:nr) {
    y_row <- y_top - row_h * i - row_h/2
    bg    <- ifelse(i %% 2 == 0, stripe_col, "white")
    grid.rect(x = 0.5, y = y_row, width = 0.88, height = row_h,
              gp = gpar(fill = bg, col = "grey85", lwd = 0.4))
    for (j in 1:nc) {
      grid.text(as.character(df_tab[i, j]),
                x = x_starts[j] + col_widths[j]/2,
                y = y_row,
                gp = gpar(fontsize = 7.5, col = GRIS),
                just = "centre")
    }
  }
}

imagen_en_pagina <- function(path, y_centre, alto = 0.42) {
  img <- png::readPNG(path)
  grid.raster(img, x = 0.5, y = y_centre,
              width = 0.88, height = alto,
              interpolate = TRUE)
}

# ── Abrir PDF ────────────────────────────────────────────────
pdf(PDF_SALIDA, width = 8.27, height = 11.69, onefile = TRUE) # A4

# ── PÁGINA 1: PORTADA ────────────────────────────────────────
nueva_pagina()
grid.rect(gp = gpar(fill = "white", col = NA))

texto_titulo("INFORME DE VALORACION BURSATIL", y = 0.88, size = 20)
texto_titulo("Analisis Cuantitativo del IPSA y Mercados Regionales",
             y = 0.82, size = 12, bold = FALSE, color = GRIS)
linea_hr(0.78)
linea_hr(0.775)

meta <- data.frame(
  Campo = c("Fecha de elaboracion", "Modelos aplicados",
            "Mercado objetivo", "Benchmark regional", "Fuente de datos"),
  Valor = c(fecha_hoy,
            "Fundamentales Economicos | CAPM Regional | Fed Model",
            "IPSA (Chile) y Indices Regionales en USD",
            "MSCI Emerging Markets (MXEF)",
            basename(ARCHIVO))
)
y_meta <- 0.72
row_h_meta <- 0.05
for (i in 1:nrow(meta)) {
  bg <- ifelse(i %% 2 == 0, GRIS_CL, "white")
  grid.rect(x = 0.5, y = y_meta - (i-1)*row_h_meta,
            width = 0.88, height = row_h_meta,
            gp = gpar(fill = bg, col = "grey85", lwd = 0.3))
  grid.text(meta$Campo[i], x = 0.10, y = y_meta - (i-1)*row_h_meta,
            gp = gpar(fontsize = 9.5, col = AZUL, fontface = "bold"), just = "left")
  grid.text(meta$Valor[i], x = 0.40, y = y_meta - (i-1)*row_h_meta,
            gp = gpar(fontsize = 9, col = GRIS), just = "left")
}

linea_hr(0.46)

intro <- paste0(
  "El presente informe aplica tres modelos cuantitativos complementarios para evaluar el nivel ",
  "de valoracion del IPSA chileno y de los principales indices bursatiles regionales. ",
  "Cada modelo aborda una dimension distinta del analisis: los fundamentales macroeconomicos (M1), ",
  "la prima de riesgo relativa al mercado emergente global (M2) y el atractivo relativo ",
  "frente a tasas reales de largo plazo (M3)."
)
grid.text(intro, x = 0.5, y = 0.40, gp = gpar(fontsize = 9.5, col = GRIS),
          just = "centre", hjust = 0.5,
          vp = viewport(x = 0.5, y = 0.40, width = 0.88, height = 0.12),
          default.units = "npc")

grid.text(paste("Pagina 1  |  Informe de Valoracion Bursatil  |", fecha_hoy),
          x = 0.5, y = 0.03, gp = gpar(fontsize = 7.5, col = "grey60"), just = "centre")

# ── PÁGINA 2: MODELO 1 ───────────────────────────────────────
nueva_pagina()
grid.rect(gp = gpar(fill = "white", col = NA))

texto_titulo("Modelo 1: IPSA por Fundamentales Economicos", y = 0.96, size = 14)
linea_hr(0.935)

texto_titulo("Especificacion", y = 0.915, size = 10, color = GRIS)
grid.text(
  "log(IPSA) = a + b1*log(Cobre) + b2*log(Petroleo) + b3*log(IMACEC)",
  x = 0.5, y = 0.888,
  gp = gpar(fontsize = 9.5, col = AZUL, fontface = "bold"), just = "centre"
)

texto_titulo("Coeficientes Estimados", y = 0.862, size = 10, color = GRIS)

tab1 <- data.frame(
  Variable      = nombres1,
  Coeficiente   = sprintf("%+.4f", m1$beta),
  `t-stat`      = sprintf("%+.2f", m1$t_stat),
  `Sig. 5pct`   = ifelse(abs(m1$t_stat) > m1$t_crit, "SI", "NO"),
  check.names   = FALSE
)
tabla_grid(tab1, y_top = 0.845,
           col_widths = c(0.22, 0.22, 0.22, 0.22))

# Caja diagnóstico
resid_prom1 <- mean(last12_1)
bg_box1  <- ifelse(sig_rec1, "#FDECEA", "#FEF3E2")
brd_box1 <- ifelse(sig_rec1, ROJO, NARANJA)
caja_color(y = 0.725, h = 0.042, color = bg_box1, borde = brd_box1)
grid.text(
  sprintf("Residuo prom. ult. 12m: %+.4f  |  t = %+.2f  |  IPSA %s fundamental  |  %s",
          resid_prom1, t_rec1, dir1,
          ifelse(sig_rec1, "Sig. 5%", "No sig. 5%")),
  x = 0.5, y = 0.725,
  gp = gpar(fontsize = 9, col = brd_box1, fontface = "bold"), just = "centre"
)

texto_titulo("Grafico — Nivel Observado, Estimado y Residuos", y = 0.695, size = 10, color = GRIS)
imagen_en_pagina(IMG1, y_centre = 0.495, alto = 0.39)
grid.text(
  "Figura 1: IPSA observado vs. estimado por fundamentales (panel superior) y residuos mensuales (panel inferior).",
  x = 0.5, y = 0.29, gp = gpar(fontsize = 7.5, col = "grey50", fontface = "italic"), just = "centre"
)

# Interpretacion
if (sig_rec1 && dir1 == "SOBRE") {
  interp1 <- paste0("El IPSA muestra una desviacion positiva y estadisticamente significativa ",
                    "respecto de su valor fundamental en los ultimos 12 meses. Las condiciones macroeconomicas ",
                    "(cobre, petroleo e IMACEC) no justificarian el nivel actual del indice, lo que representa ",
                    "una senal de cautela para posiciones de largo plazo.")
} else if (sig_rec1 && dir1 == "BAJO") {
  interp1 <- paste0("El IPSA cotiza por debajo de lo que justifican los fundamentales macroeconomicos ",
                    "de manera estadisticamente significativa, lo que podria interpretarse como una ",
                    "oportunidad de entrada desde una perspectiva de valor fundamental.")
} else {
  interp1 <- paste0("La desviacion del IPSA respecto de su valor fundamental en los ultimos 12 meses ",
                    "no es estadisticamente significativa al 5%. El indice se encuentra en un rango coherente ",
                    "con los determinantes macroeconomicos (cobre, petroleo e IMACEC).")
}
grid.text(interp1, x = 0.5, y = 0.24,
          gp = gpar(fontsize = 8.5, col = GRIS),
          just = "centre", hjust = 0.5,
          vp = viewport(x = 0.5, y = 0.24, width = 0.88, height = 0.08))

grid.text(paste("Pagina 2  |  Informe de Valoracion Bursatil  |", fecha_hoy),
          x = 0.5, y = 0.03, gp = gpar(fontsize = 7.5, col = "grey60"), just = "centre")

# ── PÁGINA 3: MODELO 2 ───────────────────────────────────────
nueva_pagina()
grid.rect(gp = gpar(fill = "white", col = NA))

texto_titulo("Modelo 2: CAPM Regional vs. MSCI Emergentes (USD)", y = 0.96, size = 14)
linea_hr(0.935)

texto_titulo("Especificacion", y = 0.915, size = 10, color = GRIS)
grid.text("log(Indice USD) = a + b*log(MSCI EM)  —  Residuo = Componente no explicado por el mercado global",
          x = 0.5, y = 0.888, gp = gpar(fontsize = 9.5, col = AZUL, fontface = "bold"), just = "centre")

texto_titulo("Resultados por Pais — Ultimo Periodo", y = 0.862, size = 10, color = GRIS)

tab2_data <- lapply(names(resultados_capm), function(p) {
  r <- resultados_capm[[p]]
  data.frame(
    Pais       = p,
    Beta       = sprintf("%.3f", r$beta[2]),
    `t-ultimo` = sprintf("%+.2f", r$t_last),
    Estado     = r$direction,
    `Sig.5pct` = ifelse(r$sig, "SI", "NO"),
    R2         = sprintf("%.3f", r$r2),
    check.names = FALSE
  )
})
tab2 <- do.call(rbind, tab2_data)
tabla_grid(tab2, y_top = 0.845,
           col_widths = c(0.185, 0.12, 0.12, 0.175, 0.11, 0.11))

texto_titulo("Grafico — Residuos por Pais", y = 0.665, size = 10, color = GRIS)
imagen_en_pagina(IMG2, y_centre = 0.468, alto = 0.38)
grid.text(
  "Figura 2: Residuos mensuales vs. MSCI EM. Rojo = sobrevalorado, Azul = subvalorado. Bandas naranja: +/-1.96 desv. est.",
  x = 0.5, y = 0.27, gp = gpar(fontsize = 7.5, col = "grey50", fontface = "italic"), just = "centre"
)

# Interpretacion Chile
if (!is.na(t_chile)) {
  sig_cl_txt <- ifelse(sig_chile, "estadisticamente significativa", "no estadisticamente significativa")
  interp2 <- sprintf(
    paste0("Para Chile, el residuo actual indica que el IPSA se encuentra %s respecto del MSCI Emergentes, ",
           "con una desviacion %s (t = %+.2f). Su beta de %.2f refleja la sensibilidad historica ",
           "del mercado chileno ante el ciclo global emergente."),
    tolower(dir_chile), sig_cl_txt, t_chile,
    resultados_capm[["Chile"]]$beta[2]
  )
  grid.text(interp2, x = 0.5, y = 0.22,
            gp = gpar(fontsize = 8.5, col = GRIS),
            just = "centre", hjust = 0.5,
            vp = viewport(x = 0.5, y = 0.22, width = 0.88, height = 0.08))
}

grid.text(paste("Pagina 3  |  Informe de Valoracion Bursatil  |", fecha_hoy),
          x = 0.5, y = 0.03, gp = gpar(fontsize = 7.5, col = "grey60"), just = "centre")

# ── PÁGINA 4: MODELO 3 ───────────────────────────────────────
nueva_pagina()
grid.rect(gp = gpar(fill = "white", col = NA))

texto_titulo("Modelo 3: Fed Model — Bonos vs. Acciones", y = 0.96, size = 14)
linea_hr(0.935)

texto_titulo("Especificacion", y = 0.915, size = 10, color = GRIS)
grid.text("Spread = BCU10 (%) - Earnings Yield (%) = BCU10 - (1/PE)*100",
          x = 0.5, y = 0.888, gp = gpar(fontsize = 9.5, col = AZUL, fontface = "bold"), just = "centre")

texto_titulo("Estadisticos del Spread Historico", y = 0.862, size = 10, color = GRIS)

tab3 <- data.frame(
  Estadistico = c("Media historica", "Desv. estandar", "Limite superior (+1s)",
                  "Limite inferior (-1s)", "Ultimo spread"),
  Valor       = c(sprintf("%.2f%%", mu3), sprintf("%.2f%%", std3),
                  sprintf("%.2f%%", mu3 + std3), sprintf("%.2f%%", mu3 - std3),
                  sprintf("%.2f%%", ult3))
)
tabla_grid(tab3, y_top = 0.845, col_widths = c(0.5, 0.38))

caja_color(y = 0.748, h = 0.042,
           color = ifelse(ult3 > mu3+std3, "#FDECEA",
                          ifelse(ult3 < mu3-std3, "#EAFAF1", "#FEF3E2")),
           borde = color_zona3)
grid.text(zona3, x = 0.5, y = 0.748,
          gp = gpar(fontsize = 9.5, col = color_zona3, fontface = "bold"), just = "centre")

texto_titulo("Grafico — BCU10 vs. Earnings Yield y Spread", y = 0.718, size = 10, color = GRIS)
imagen_en_pagina(IMG3, y_centre = 0.518, alto = 0.39)
grid.text(
  "Figura 3: BCU10 vs. Earnings Yield (panel superior) y spread con bandas historicas +/-1s (panel inferior).",
  x = 0.5, y = 0.315, gp = gpar(fontsize = 7.5, col = "grey50", fontface = "italic"), just = "centre"
)

if (ult3 > mu3 + std3) {
  interp3 <- paste0("El spread actual supera la banda superior (+1s), indicando que los bonos reales BCU10 ",
                    "ofrecen mayor rentabilidad relativa que las acciones. Bajo el Fed Model, el IPSA luce caro ",
                    "relativo a la renta fija real chilena.")
} else if (ult3 < mu3 - std3) {
  interp3 <- paste0("El spread actual esta bajo la banda inferior (-1s), indicando que el earnings yield del IPSA ",
                    "supera ampliamente la tasa BCU10. El Fed Model sugiere que las acciones estan baratas ",
                    "relativo a los bonos reales chilenos.")
} else {
  interp3 <- paste0("El spread se encuentra dentro de la banda historica +/-1s, reflejando una relacion ",
                    "neutra entre la rentabilidad implicita de las acciones y la tasa real de largo plazo. ",
                    "El Fed Model no muestra desalineacion significativa en este momento.")
}
grid.text(interp3, x = 0.5, y = 0.264,
          gp = gpar(fontsize = 8.5, col = GRIS),
          just = "centre", hjust = 0.5,
          vp = viewport(x = 0.5, y = 0.264, width = 0.88, height = 0.07))

grid.text(paste("Pagina 4  |  Informe de Valoracion Bursatil  |", fecha_hoy),
          x = 0.5, y = 0.03, gp = gpar(fontsize = 7.5, col = "grey60"), just = "centre")

# ── PÁGINA 5: CONCLUSIONES ───────────────────────────────────
nueva_pagina()
grid.rect(gp = gpar(fill = "white", col = NA))

texto_titulo("Sintesis y Conclusiones Integradas", y = 0.96, size = 14)
linea_hr(0.935)

estado_m3 <- ifelse(ult3 > mu3+std3, "Acciones caras vs bonos",
                    ifelse(ult3 < mu3-std3, "Acciones baratas vs bonos", "Valoracion neutra"))

tab_conclu <- data.frame(
  Modelo     = c("M1: Fundamentales", "M2: CAPM Regional", "M3: Fed Model"),
  `Señal`    = c(paste("IPSA", dir1, "fundamental"),
                 ifelse(is.na(dir_chile), "N/D", dir_chile),
                 estado_m3),
  `Sig. 5pct`= c(ifelse(sig_rec1, "SI", "NO"),
                 ifelse(sig_chile, "SI", "NO"),
                 ifelse(ult3 > mu3+std3 | ult3 < mu3-std3, "SI", "NO")),
  Observacion = c(
    sprintf("t=%.2f | Resid.prom: %+.4f", t_rec1, resid_prom1),
    ifelse(is.na(t_chile), "N/D", sprintf("t=%+.2f | beta=%.2f", t_chile,
           resultados_capm[["Chile"]]$beta[2])),
    sprintf("Spread: %.2f%% | Media: %.2f%%", ult3, mu3)
  ),
  check.names = FALSE
)
tabla_grid(tab_conclu, y_top = 0.905,
           col_widths = c(0.21, 0.23, 0.13, 0.31), row_h = 0.038)

linea_hr(0.80)
texto_titulo("Consideraciones Finales", y = 0.78, size = 11, color = GRIS)

cons1 <- paste0(
  "Ningun modelo por si solo determina el nivel correcto de un mercado. Los tres enfoques son ",
  "complementarios: M1 captura los fundamentales macro, M2 el posicionamiento relativo al ciclo ",
  "global emergente, y M3 el atractivo frente a la renta fija real. La convergencia de señales ",
  "otorga mayor robustez a las conclusiones."
)
grid.text(cons1, x = 0.5, y = 0.73,
          gp = gpar(fontsize = 9, col = GRIS),
          just = "centre", hjust = 0.5,
          vp = viewport(x = 0.5, y = 0.73, width = 0.88, height = 0.09))

cons2 <- paste0(
  "Este informe tiene fines exclusivamente analiticos y no constituye una recomendacion de inversion. ",
  "Las decisiones de cartera deben considerar adicionalmente el perfil de riesgo del inversionista, ",
  "el horizonte temporal, la liquidez de los instrumentos y el contexto macroeconomico global vigente."
)
grid.text(cons2, x = 0.5, y = 0.645,
          gp = gpar(fontsize = 8.5, col = "grey55", fontface = "italic"),
          just = "centre", hjust = 0.5,
          vp = viewport(x = 0.5, y = 0.645, width = 0.88, height = 0.08))

grid.text(paste("Pagina 5  |  Informe de Valoracion Bursatil  |", fecha_hoy),
          x = 0.5, y = 0.03, gp = gpar(fontsize = 7.5, col = "grey60"), just = "centre")

dev.off()
cat(sprintf("  Informe PDF generado: %s\n\n", PDF_SALIDA))
cat("=======================================================\n")
cat("FIN — Se generaron 3 graficos PNG + 1 informe PDF\n")
cat("=======================================================\n")
