# =============================================================================
# EJERCICIO 1 — Average Treatment Effect (ATE)
# Campaña push countdown timing vs. push estándar
# Dataset: cai_exp_data.csv
# PRODUCE (global env para render_*.R):
#   df, tabla_balance, tabla_balance_export, balance_ok, chisq_pvals,
#   lm_check_summary, f_val, f_pval,
#   m_conv, m_rev, m_conv_ancova, m_rev_ancova, m_ticket,
#   resumen_ate, resumen_ate_comp,
#   ate_conv, ate_rev, ate_tick, p_conv, p_rev, p_tick,
#   sig_conv, sig_rev, sig_tick
# =============================================================================

if (!exists("root")) root <- getwd()
source(file.path(root, "code", "00_setup.R"))

df <- df_exp

# -----------------------------------------------------------------------------
# 1. ESTADÍSTICA DESCRIPTIVA
# -----------------------------------------------------------------------------

cat("\n=== DISTRIBUCIÓN DEL TRATAMIENTO ===\n")
df |>
  count(treatment) |>
  mutate(
    grupo = if_else(treatment == 1, "Tratado (countdown)", "Control (push estándar)"),
    pct   = scales::percent(n / sum(n), accuracy = 0.1)
  ) |>
  select(grupo, n, pct) |>
  print()

cat("\n=== RESUMEN OUTCOMES POR GRUPO ===\n")
df |>
  group_by(treatment) |>
  summarise(
    n              = n(),
    conversion_pct = mean(conversion) * 100,
    revenue_medio  = mean(revenue),
    revenue_sd     = sd(revenue),
    .groups = "drop"
  ) |>
  print()

# -----------------------------------------------------------------------------
# 2. TABLA DE BALANCE
# -----------------------------------------------------------------------------

labels_cont <- c(
  recency_days        = "Recencia (dias)",
  frequency           = "Frecuencia (visitas)",
  avg_session_min     = "Duracion de sesion (min)",
  satisfaction_score  = "Puntaje de satisfaccion",
  device_age_years    = "Antiguedad del dispositivo (anos)"
)
vars_continuas <- names(labels_cont)

balance_continuas <- map_dfr(vars_continuas, function(var) {
  media_ctrl <- mean(df[[var]][df$treatment == 0])
  media_trat <- mean(df[[var]][df$treatment == 1])
  sd_ctrl    <- sd(df[[var]][df$treatment == 0])
  sd_trat    <- sd(df[[var]][df$treatment == 1])
  dif        <- media_trat - media_ctrl
  p_val      <- t.test(df[[var]] ~ df$treatment)$p.value
  unidad     <- gsub(".*\\((.*)\\).*", "\\1", labels_cont[[var]])
  tibble(
    Variable     = labels_cont[[var]],
    `Control`    = sprintf("%.2f", media_ctrl),
    `Control SD` = sprintf("%.2f", sd_ctrl),
    `Tratado`    = sprintf("%.2f", media_trat),
    `Tratado SD` = sprintf("%.2f", sd_trat),
    `Diferencia` = sprintf("%+.3f %s", dif, unidad),
    `p-valor`    = round(p_val, 3)
  )
})

labels_categ <- c(
  canal        = "Canal de adquisicion",
  platform     = "Plataforma",
  loyalty_tier = "Nivel de lealtad"
)
vars_categ <- names(labels_categ)

chisq_pvals <- map_chr(vars_categ, function(var) {
  p <- chisq.test(table(df[[var]], df$treatment))$p.value
  sprintf("%s: p = %.3f", labels_categ[[var]], p)
}) |> set_names(vars_categ)

balance_categ <- map_dfr(vars_categ, function(var) {
  categorias <- sort(unique(as.character(df[[var]])))
  header <- tibble(
    Variable     = labels_categ[[var]],
    `Control`    = NA_character_, `Control SD` = NA_character_,
    `Tratado`    = NA_character_, `Tratado SD` = NA_character_,
    `Diferencia` = NA_character_, `p-valor`    = NA_real_
  )
  filas <- map_dfr(categorias, function(cat) {
    dummy     <- as.integer(df[[var]] == cat)
    prop_ctrl <- mean(dummy[df$treatment == 0])
    prop_trat <- mean(dummy[df$treatment == 1])
    p_val     <- t.test(dummy ~ df$treatment)$p.value
    tibble(
      Variable     = paste0("  ", cat),
      `Control`    = sprintf("%.1f%%", prop_ctrl * 100),
      `Control SD` = NA_character_,
      `Tratado`    = sprintf("%.1f%%", prop_trat * 100),
      `Tratado SD` = NA_character_,
      `Diferencia` = sprintf("%+.2f pp", (prop_trat - prop_ctrl) * 100),
      `p-valor`    = round(p_val, 3)
    )
  })
  bind_rows(header, filas)
})

tabla_balance        <- bind_rows(balance_continuas, balance_categ)
tabla_balance_export <- tabla_balance |> select(-`Control SD`, -`Tratado SD`)
balance_ok           <- all(tabla_balance$`p-valor` > 0.05, na.rm = TRUE)

# Regresión auxiliar: D ~ X (R² ≈ 0 confirma aleatorización)
lm_check         <- lm(
  treatment ~ recency_days + frequency + avg_session_min +
    satisfaction_score + device_age_years + canal + platform + loyalty_tier,
  data = df
)
lm_check_summary <- summary(lm_check)
fstat  <- lm_check_summary$fstatistic
f_val  <- round(fstat[["value"]], 4)
f_pval <- round(pf(fstat[["value"]], fstat[["numdf"]], fstat[["dendf"]], lower.tail = FALSE), 4)

cat("\nR2 regresion treatment ~ covariables:", round(lm_check_summary$r.squared, 4), "\n")
cat("F-test conjunto: F =", f_val, "| p-valor =", f_pval, "\n")

# -----------------------------------------------------------------------------
# 3. MODELOS DE REGRESIÓN
# -----------------------------------------------------------------------------

covariables <- c("recency_days", "frequency", "avg_session_min",
                 "satisfaction_score", "device_age_years",
                 "canal", "platform", "loyalty_tier")

m_conv <- lm(conversion ~ treatment, data = df)
m_rev  <- lm(revenue   ~ treatment, data = df)

cat("\n=== Conversion rate — SE robustos HC2 ===\n")
print(coeftest(m_conv, vcov = vcovHC(m_conv, type = "HC2")))

cat("\n=== Revenue — SE robustos HC2 ===\n")
print(coeftest(m_rev, vcov = vcovHC(m_rev, type = "HC2")))

formula_ancova <- function(outcome) {
  as.formula(paste(outcome, "~ treatment +", paste(covariables, collapse = " + ")))
}
m_conv_ancova <- lm(formula_ancova("conversion"), data = df)
m_rev_ancova  <- lm(formula_ancova("revenue"),    data = df)
m_ticket      <- lm(revenue ~ treatment,          data = filter(df, conversion == 1))

ate_summary <- function(modelo, label) {
  ct <- coeftest(modelo, vcov = vcovHC(modelo, type = "HC2"))
  ci <- coefci(modelo,   vcov = vcovHC(modelo, type = "HC2"), level = 0.95)
  tibble(
    Modelo       = label,
    ATE          = round(ct["treatment", "Estimate"], 4),
    `IC 95% inf` = round(ci["treatment", 1], 4),
    `IC 95% sup` = round(ci["treatment", 2], 4),
    `p-valor`    = round(ct["treatment", "Pr(>|t|)"], 4),
    N            = nobs(modelo)
  )
}

resumen_ate <- bind_rows(
  ate_summary(m_conv, "Conversion rate (LPM)"),
  ate_summary(m_rev,  "Revenue")
)
resumen_ate_comp <- bind_rows(
  ate_summary(m_conv_ancova, "Conversion rate — ANCOVA"),
  ate_summary(m_rev_ancova,  "Revenue — ANCOVA"),
  ate_summary(m_ticket,      "Ticket promedio (solo converters)")
)

cat("\n=== ATE — MODELOS PRINCIPALES ===\n");    print(resumen_ate)
cat("\n=== ATE — MODELOS COMPLEMENTARIOS ===\n"); print(resumen_ate_comp)

# Escalares para los renders
ate_conv <- resumen_ate$ATE[resumen_ate$Modelo == "Conversion rate (LPM)"]
ate_rev  <- resumen_ate$ATE[resumen_ate$Modelo == "Revenue"]
ate_tick <- resumen_ate_comp$ATE[resumen_ate_comp$Modelo == "Ticket promedio (solo converters)"]
p_conv   <- resumen_ate$`p-valor`[resumen_ate$Modelo == "Conversion rate (LPM)"]
p_rev    <- resumen_ate$`p-valor`[resumen_ate$Modelo == "Revenue"]
p_tick   <- resumen_ate_comp$`p-valor`[resumen_ate_comp$Modelo == "Ticket promedio (solo converters)"]
sig_conv <- ifelse(p_conv < 0.05, "estadísticamente significativo", "no significativo")
sig_rev  <- ifelse(p_rev  < 0.05, "estadísticamente significativo", "no significativo")
sig_tick <- ifelse(p_tick < 0.05, "estadísticamente significativo", "no significativo")

cat(sprintf("\nATE conversión: %+.1f pp (p = %.4f)\n", ate_conv * 100, p_conv))
cat(sprintf("ATE revenue:    +$%.2f  (p = %.4f)\n", ate_rev, p_rev))
cat(sprintf("ATE ticket:     +$%.2f  (p = %.4f)\n", ate_tick, p_tick))
