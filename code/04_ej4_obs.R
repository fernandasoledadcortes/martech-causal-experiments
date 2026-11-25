# =============================================================================
# EJERCICIO 4 (BONUS) — Difference-in-Differences (DiD)
# Efecto del programa de fidelización sobre el revenue semanal
# Dataset: cai_obs_data.csv
# PRODUCE (global env para render_*.R):
#   df (df_obs), df_user, n_e0, n_e1, rev_mean_e0, rev_mean_e1,
#   tabla_balance_obs, chi_region, chi_appver,
#   m_trends, f_trends, p_trends, tendencias_paralelas,
#   m_did_fe, m_did_twfe, att, att_se, att_p, att_ci_l, att_ci_u, att_twfe,
#   medias, rev_e1_post, rev_e1_pre, rev_e0_post, rev_e0_pre, did_manual,
#   tabla_did_manual, path_fig_did
# =============================================================================

if (!exists("root")) root <- getwd()
source(file.path(root, "code", "00_setup.R"))

df <- df_obs

# -----------------------------------------------------------------------------
# 1. TABLA DE BALANCE — COVARIABLES PRE-TRATAMIENTO POR GRUPO
# -----------------------------------------------------------------------------

df_user <- df |>
  filter(week == 1) |>
  select(client_id, enrolled, revenue_pre_avg, tenure_months,
         nps_score, segment, region, app_version)

n_e0 <- sum(df_user$enrolled == 0)
n_e1 <- sum(df_user$enrolled == 1)

rev_mean_e0 <- mean(df_user$revenue_pre_avg[df_user$enrolled == 0], na.rm = TRUE)
rev_mean_e1 <- mean(df_user$revenue_pre_avg[df_user$enrolled == 1], na.rm = TRUE)

labels_cont_obs <- c(
  revenue_pre_avg = "Revenue pre-período ($)",
  tenure_months   = "Antigüedad (meses)",
  nps_score       = "NPS score"
)

balance_cont_obs <- map_dfr(names(labels_cont_obs), function(var) {
  x0    <- df_user[[var]][df_user$enrolled == 0]
  x1    <- df_user[[var]][df_user$enrolled == 1]
  p_val <- t.test(x1, x0)$p.value
  tibble(
    Variable      = labels_cont_obs[[var]],
    `Control`     = sprintf("%.2f", mean(x0, na.rm = TRUE)),
    `Control SD`  = sprintf("%.2f", sd(x0, na.rm = TRUE)),
    `Tratados`    = sprintf("%.2f", mean(x1, na.rm = TRUE)),
    `Tratados SD` = sprintf("%.2f", sd(x1, na.rm = TRUE)),
    `Diferencia`  = sprintf("%+.2f", mean(x1, na.rm = TRUE) - mean(x0, na.rm = TRUE)),
    `p-valor`     = round(p_val, 4)
  )
})

balance_seg <- {
  header <- tibble(
    Variable = "Segmento", `Control` = NA_character_, `Control SD` = NA_character_,
    `Tratados` = NA_character_, `Tratados SD` = NA_character_,
    `Diferencia` = NA_character_, `p-valor` = NA_real_
  )
  categorias <- levels(df_user$segment)
  filas <- map_dfr(categorias, function(cat) {
    dummy     <- as.integer(as.character(df_user$segment) == cat)
    prop_ctrl <- mean(dummy[df_user$enrolled == 0])
    prop_trat <- mean(dummy[df_user$enrolled == 1])
    p_val     <- t.test(dummy ~ df_user$enrolled)$p.value
    tibble(
      Variable      = paste0("  ", cat),
      `Control`     = sprintf("%.1f%%", prop_ctrl * 100),
      `Control SD`  = NA_character_,
      `Tratados`    = sprintf("%.1f%%", prop_trat * 100),
      `Tratados SD` = NA_character_,
      `Diferencia`  = sprintf("%+.2f pp", (prop_trat - prop_ctrl) * 100),
      `p-valor`     = round(p_val, 4)
    )
  })
  bind_rows(header, filas)
}

tabla_balance_obs <- bind_rows(balance_cont_obs, balance_seg)

chi_region <- chisq.test(table(df_user$region,      df_user$enrolled))
chi_appver <- chisq.test(table(df_user$app_version, df_user$enrolled))

cat("\n=== TABLA DE BALANCE — ENROLLED=0 vs ENROLLED=1 ===\n")
cat(sprintf("N: control=%s | tratados=%s\n",
            format(n_e0, big.mark=","), format(n_e1, big.mark=",")))
print(tabla_balance_obs)
cat(sprintf("Chi-cuadrado Región: p = %.4f | App version: p = %.4f\n",
            chi_region$p.value, chi_appver$p.value))

# -----------------------------------------------------------------------------
# 2. TEST DE TENDENCIAS PARALELAS (pre-período: semanas 1–9)
# -----------------------------------------------------------------------------

df_pre    <- df |> filter(week <= 9)
m_trends  <- lm(revenue ~ week * enrolled, data = df_pre)

cat("\n=== TEST DE TENDENCIAS PARALELAS (pre-período) ===\n")
print(coeftest(m_trends, vcov = vcovCL(m_trends, cluster = ~client_id)))

lht_trends <- linearHypothesis(
  m_trends, "week:enrolled = 0",
  vcov = vcovCL(m_trends, cluster = ~client_id)
)
f_trends            <- round(lht_trends$F[2], 3)
p_trends            <- round(lht_trends$`Pr(>F)`[2], 4)
tendencias_paralelas <- p_trends >= 0.05

cat(sprintf("\nF = %.3f | p = %.4f\n", f_trends, p_trends))
cat(ifelse(tendencias_paralelas,
  "No se rechaza H0: tendencias paralelas compatibles.\n",
  "ADVERTENCIA: se rechaza H0 — tendencias pre-tratamiento difieren entre grupos.\n"))

# -----------------------------------------------------------------------------
# 3. MODELOS DiD
# -----------------------------------------------------------------------------

m_did_fe <- feols(revenue ~ enrolled + enrolled:post | week,
                  data = df, cluster = ~client_id)

m_did_twfe <- feols(revenue ~ enrolled:post | client_id + week,
                    data = df, cluster = ~client_id)

cat("\n=== (1) DiD + FE SEMANA ===\n");  print(m_did_fe)
cat("\n=== (2) TWFE ===\n");             print(m_did_twfe)

att      <- round(coef(m_did_fe)["enrolled:post"], 2)
att_se   <- round(se(m_did_fe)["enrolled:post"], 2)
att_p    <- round(pvalue(m_did_fe)["enrolled:post"], 4)
att_ci_l <- round(att - 1.96 * se(m_did_fe)["enrolled:post"], 2)
att_ci_u <- round(att + 1.96 * se(m_did_fe)["enrolled:post"], 2)
att_twfe <- round(coef(m_did_twfe)["enrolled:post"], 2)

cat(sprintf("\nATT FE semana: $%.2f (SE=%.2f)\n", att, att_se))
cat(sprintf("ATT TWFE:      $%.2f\n", att_twfe))
cat(sprintf("IC 95%%: [$%.2f, $%.2f] | p=%.4f\n", att_ci_l, att_ci_u, att_p))

# -----------------------------------------------------------------------------
# 4. DESCOMPOSICIÓN MANUAL Y TABLA RESUMEN
# -----------------------------------------------------------------------------

medias <- df |>
  group_by(enrolled, post) |>
  summarise(rev_mean = mean(revenue, na.rm = TRUE), .groups = "drop")

rev_e1_post <- medias$rev_mean[medias$enrolled == 1 & medias$post == 1]
rev_e1_pre  <- medias$rev_mean[medias$enrolled == 1 & medias$post == 0]
rev_e0_post <- medias$rev_mean[medias$enrolled == 0 & medias$post == 1]
rev_e0_pre  <- medias$rev_mean[medias$enrolled == 0 & medias$post == 0]
did_manual  <- (rev_e1_post - rev_e1_pre) - (rev_e0_post - rev_e0_pre)

tabla_did_manual <- tibble(
  Grupo      = c("Enrolled = 1", "Enrolled = 0", "Diferencia (DiD)"),
  `Pre ($)`  = c(round(rev_e1_pre,  1), round(rev_e0_pre,  1), "—"),
  `Post ($)` = c(round(rev_e1_post, 1), round(rev_e0_post, 1), "—"),
  `Δ ($)`    = c(round(rev_e1_post - rev_e1_pre, 1),
                  round(rev_e0_post - rev_e0_pre, 1),
                  round(did_manual, 2))
)

# -----------------------------------------------------------------------------
# 5. FIGURA — TENDENCIAS DE REVENUE POR SEMANA Y GRUPO
# -----------------------------------------------------------------------------

trends_data <- df |>
  group_by(week, enrolled) |>
  summarise(rev_mean = mean(revenue, na.rm = TRUE), .groups = "drop") |>
  mutate(grupo = ifelse(enrolled == 1, "Inscriptos (enrolled=1)",
                                       "No inscriptos (enrolled=0)"))

fig_trends <- ggplot(trends_data, aes(x = week, y = rev_mean,
                                      color = grupo, group = grupo)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  geom_vline(xintercept = 9.5, linetype = "dashed", color = "grey40") +
  annotate("text", x = 9.5, y = max(trends_data$rev_mean) * 0.96,
           label = "Inicio\npost-período", hjust = -0.1, size = 3, color = "grey40") +
  annotate("rect", xmin = 1,   xmax = 9.5, ymin = -Inf, ymax = Inf,
           fill = "#F5F5F5", alpha = 0.4) +
  annotate("rect", xmin = 9.5, xmax = 18,  ymin = -Inf, ymax = Inf,
           fill = "#E8F4F8", alpha = 0.4) +
  scale_color_manual(values = c("Inscriptos (enrolled=1)"   = "#2563EB",
                                 "No inscriptos (enrolled=0)" = "#DC2626")) +
  scale_x_continuous(breaks = 1:18) +
  labs(
    title    = "Revenue promedio semanal por grupo (DiD)",
    subtitle = "Gris: pre-período (sem. 1-9) | Azul: post-período (sem. 10-18)",
    x = "Semana", y = "Revenue promedio ($)", color = NULL,
    caption  = "Fuente: cai_obs_data.csv."
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank())

path_fig_did <- file.path(path_figs, "fig_did_trends.png")
ggsave(path_fig_did, fig_trends, width = 9, height = 5, dpi = 150)
cat("\nFigura guardada:", path_fig_did, "\n")
