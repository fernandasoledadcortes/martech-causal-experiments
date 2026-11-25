# =============================================================================
# 00_setup.R — Configuración global
# Carga librerías, define paths y limpia ambas bases de datos.
#
# USO: al inicio de cada script agregar las dos líneas:
#   root <- getwd()   # <- única línea a editar
#   source(file.path(root, "code", "00_setup.R"))
# =============================================================================
# REQUIERE: `root` definido en el script que llama a source()
# PRODUCE:
#   df_exp  — cai_exp_data.csv limpio (50,000 filas × 9 cols)
#   df_obs  — cai_obs_data.csv limpio (360,000 filas × 14 cols)
#   paths   — path_input_exp, path_input_obs, path_figs,
#             path_reporte, path_pres
# =============================================================================

Sys.setlocale("LC_ALL", "en_US.UTF-8")
options(encoding = "UTF-8")

# -----------------------------------------------------------------------------
# 1. PAQUETES
# -----------------------------------------------------------------------------

pkgs <- c("tidyverse", "modelsummary", "sandwich", "lmtest",
          "kableExtra", "marginaleffects", "car", "pwr", "ggplot2", "fixest")
install.packages(setdiff(pkgs, rownames(installed.packages())),
                 repos = "https://cloud.r-project.org")

library(tidyverse)
library(modelsummary)
library(sandwich)
library(lmtest)
library(kableExtra)
library(marginaleffects)
library(car)
library(pwr)
library(ggplot2)
library(fixest)

# -----------------------------------------------------------------------------
# 2. PATHS (todos derivados de `root`)
# -----------------------------------------------------------------------------

path_input_exp <- file.path(root, "inputs", "cai_exp_data.csv")
path_input_obs <- file.path(root, "inputs", "cai_obs_data.csv")
path_figs      <- file.path(root, "outputs", "figures")
path_reporte   <- file.path(root, "outputs", "reporte_resultados.html")
path_pres      <- file.path(root, "outputs", "presentacion.html")

# -----------------------------------------------------------------------------
# 3. LIMPIEZA — cai_exp_data.csv → df_exp
# -----------------------------------------------------------------------------
# Problemas detectados en exploración:
#   - 12,500 filas duplicadas exactas  → distinct()
#   - canal: "socal" (typo) → "social"; capitalización inconsistente
#   - loyalty_tier: strings → factor ordenado (bronze < silver < gold)

if (!exists("df_exp")) {
  df_exp <- read_csv(path_input_exp, show_col_types = FALSE) |>
    distinct() |>
    mutate(
      across(where(is.character), ~ str_trim(str_to_lower(.x))),
      canal = case_when(canal == "socal" ~ "social", TRUE ~ canal),
      loyalty_tier = factor(loyalty_tier, levels = c("bronze", "silver", "gold")),
      canal        = factor(canal),
      platform     = factor(platform),
      treatment    = as.integer(treatment)
    )
  cat(sprintf("[setup] df_exp: %d filas | duplicados eliminados: %d\n",
              nrow(df_exp),
              nrow(read_csv(path_input_exp, show_col_types = FALSE)) - nrow(df_exp)))
} else {
  cat("[setup] df_exp ya cargado, se omite.\n")
}

# -----------------------------------------------------------------------------
# 4. LIMPIEZA — cai_obs_data.csv → df_obs
# -----------------------------------------------------------------------------
# Problemas detectados en exploración:
#   - Sin duplicados exactos
#   - segment: valores sucios ("BASIC","basico","PREMIUM","Premium","standar")
#   - tenure_months: 25 filas con -999 (sentinel de missing), 1 por usuario
#     → se imputa con el valor correcto del mismo usuario en sus otras semanas
#   - nps_score: 888 usuarios (4.4%) con valores > 10 (fuera de escala 0–10)
#     → se flaggea pero no se corrige (no entra al modelo DiD)
#   - engagement_index: valores negativos → válidos (índice compuesto)
#   - revenue: 7,034 NAs (1.95%) y 1,739 ceros → válidos
#   - survey_response: 14.7% NAs → no entra al modelo

if (!exists("df_obs")) {
  tenure_correcto <- read_csv(path_input_obs, show_col_types = FALSE) |>
    filter(tenure_months != -999) |>
    group_by(client_id) |>
    summarise(tenure_ok = first(tenure_months), .groups = "drop")

  df_obs <- read_csv(path_input_obs, show_col_types = FALSE) |>
    left_join(tenure_correcto, by = "client_id") |>
    mutate(
      segment       = str_trim(str_to_lower(segment)),
      region        = str_trim(str_to_lower(region)),
      segment       = case_when(
        segment %in% c("basico", "basic")     ~ "basic",
        segment == "premium"                  ~ "premium",
        segment %in% c("standar", "standard") ~ "standard",
        TRUE ~ segment
      ),
      tenure_months = if_else(tenure_months == -999, tenure_ok, tenure_months),
      enrolled      = as.integer(enrolled),
      post          = as.integer(week >= 10),
      week_fac      = factor(week),
      segment       = factor(segment, levels = c("basic", "standard", "premium")),
      region        = factor(region),
      app_version   = factor(app_version)
    ) |>
    select(-tenure_ok)

  cat(sprintf("[setup] df_obs: %d filas | %d usuarios | %d semanas | panel balanceado: %s\n",
              nrow(df_obs),
              n_distinct(df_obs$client_id),
              n_distinct(df_obs$week),
              all(count(df_obs, client_id)$n == 18)))
} else {
  cat("[setup] df_obs ya cargado, se omite.\n")
}
