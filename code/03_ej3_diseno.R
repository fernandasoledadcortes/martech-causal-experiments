# =============================================================================
# EJERCICIO 3 — Diseño Experimental: Rediseño del Checkout Flow
# Ejercicio teórico con cálculo de poder estadístico empírico
# Baseline p0 extraído del grupo control de cai_exp_data.csv (Ejercicio 1)
# PRODUCE (global env para render_*.R):
#   p0, p1, alpha, poder, MDE, z_alpha, z_beta, p_bar,
#   n_formula, n_r, tabla_sens_simple, tabla_duracion,
#   daily_traffic, dias_estadist, dias_recom
# =============================================================================

if (!exists("root")) root <- getwd()
source(file.path(root, "code", "00_setup.R"))

df <- df_exp

# -----------------------------------------------------------------------------
# 1. BASELINE EMPÍRICO (grupo control, Ejercicio 1)
# -----------------------------------------------------------------------------

p0 <- mean(df$conversion[df$treatment == 0])
cat(sprintf("Tasa de conversion baseline (grupo control): %.4f (%.1f%%)\n", p0, p0 * 100))

# -----------------------------------------------------------------------------
# 2. CÁLCULO DE PODER ESTADÍSTICO
# -----------------------------------------------------------------------------

alpha  <- 0.05
poder  <- 0.80
MDE    <- 0.02
p1     <- p0 + MDE

z_alpha <- qnorm(1 - alpha / 2)
z_beta  <- qnorm(poder)
p_bar   <- (p0 + p1) / 2

n_formula <- ceiling(2 * (z_alpha + z_beta)^2 * p_bar * (1 - p_bar) / MDE^2)

resultado_power <- power.prop.test(
  p1 = p0, p2 = p1, sig.level = alpha, power = poder, alternative = "two.sided"
)
n_r <- ceiling(resultado_power$n)

cat(sprintf("\nParámetros:\n  p0=%.4f | p1=%.4f | MDE=%.2f pp | alpha=%.2f | poder=%.2f\n",
            p0, p1, MDE * 100, alpha, poder))
cat(sprintf("  z_alpha/2=%.4f | z_beta=%.4f | p_bar=%.4f\n", z_alpha, z_beta, p_bar))
cat(sprintf("\nTamaño de muestra (fórmula):        %d por grupo | %d total\n",
            n_formula, n_formula * 2))
cat(sprintf("Tamaño de muestra (power.prop.test): %d por grupo | %d total\n",
            n_r, n_r * 2))

# -----------------------------------------------------------------------------
# 3. ANÁLISIS DE SENSIBILIDAD
# -----------------------------------------------------------------------------

p0_vals  <- c(0.10, 0.20, 0.28, 0.35, 0.50)
mde_vals <- c(0.01, 0.02, 0.03, 0.05)

tabla_sens_simple <- expand_grid(p0 = p0_vals, MDE = mde_vals) |>
  mutate(
    p1 = p0 + MDE,
    n  = map2_int(p0, p1, ~ ceiling(
      power.prop.test(p1 = .x, p2 = .y, sig.level = 0.05, power = 0.80)$n
    )),
    p0  = sprintf("%.0f%%", p0 * 100),
    MDE = sprintf("%.0f pp", MDE * 100)
  ) |>
  select(`Baseline (p0)` = p0, MDE, `n por grupo` = n) |>
  pivot_wider(names_from = MDE, values_from = `n por grupo`)

cat("\n=== SENSIBILIDAD: n por grupo ===\n")
print(tabla_sens_simple)

# -----------------------------------------------------------------------------
# 4. TABLA DE DURACIÓN
# -----------------------------------------------------------------------------

daily_traffic  <- c(200, 500, 1000, 2000, 5000)
dias_estadist  <- ceiling((n_formula * 2) / daily_traffic)
dias_recom     <- pmax(dias_estadist, 14)

tabla_duracion <- tibble(
  `Usuarios/día en checkout` = format(daily_traffic, big.mark = ","),
  `Días (estadístico)`       = dias_estadist,
  `Días recomendados`        = dias_recom,
  `Restricción activa`       = ifelse(dias_estadist >= 14, "Tráfico", "Ciclo semanal")
)

cat("\n=== DURACIÓN ESTIMADA DEL EXPERIMENTO ===\n")
print(tabla_duracion)
