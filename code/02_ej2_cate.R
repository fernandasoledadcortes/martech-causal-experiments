# =============================================================================
# EJERCICIO 2 — Conditional Average Treatment Effect (CATE)
# Heterogeneidad del efecto por canal de adquisición
# Dataset: cai_exp_data.csv
# PRODUCE (global env para render_*.R):
#   m_cate, cate_por_canal, cate_vals, canal_max, canal_min,
#   f_het, p_het, hay_het
# =============================================================================

if (!exists("root")) root <- getwd()
source(file.path(root, "code", "00_setup.R"))

df <- df_exp

# -----------------------------------------------------------------------------
# 2. MODELO CATE — REGRESIÓN CON INTERACCIONES
# -----------------------------------------------------------------------------
# y_i = β₀ + β₁·D_i + β₂·canal_i + β₃·(D_i × canal_i) + ε_i
# Categoría base: organic
# CATE(k) = β₁ + β₃_k

m_cate <- lm(revenue ~ treatment * canal, data = df)

cat("\n=== MODELO CATE — Coeficientes (SE robustos HC2) ===\n")
print(coeftest(m_cate, vcov = vcovHC(m_cate, type = "HC2")))

# -----------------------------------------------------------------------------
# 3. CATE POR CANAL — EFECTOS MARGINALES PROMEDIO
# -----------------------------------------------------------------------------

cate_por_canal <- avg_slopes(
  m_cate,
  variables  = "treatment",
  by         = "canal",
  vcov       = "HC2"
) |>
  as_tibble() |>
  select(canal, CATE = estimate, SE = std.error,
         `IC 95% inf` = conf.low, `IC 95% sup` = conf.high,
         `p-valor` = p.value) |>
  mutate(across(where(is.numeric), ~ round(.x, 4)))

cat("\n=== CATE POR CANAL ===\n")
print(cate_por_canal)

# Test conjunto de heterogeneidad
lht <- linearHypothesis(
  m_cate,
  c("treatment:canalpaid_search = 0", "treatment:canalsocial = 0"),
  vcov = vcovHC(m_cate, type = "HC2")
)
f_het   <- round(lht$F[2], 3)
p_het   <- round(lht$`Pr(>F)`[2], 4)
hay_het <- p_het < 0.05

cat("\n=== TEST DE HETEROGENEIDAD CONJUNTA ===\n")
cat("H0: el efecto del tratamiento es igual en todos los canales\n")
cat("F =", f_het, "| p-valor =", p_het, "\n")
cat(ifelse(hay_het,
  "Se rechaza H0: hay heterogeneidad significativa del efecto por canal.\n",
  "No se rechaza H0: no hay evidencia de heterogeneidad significativa.\n"))

# Escalares para los renders
cate_vals <- setNames(cate_por_canal$CATE, cate_por_canal$canal)
canal_max <- names(which.max(cate_vals))
canal_min <- names(which.min(cate_vals))
