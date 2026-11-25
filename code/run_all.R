# =============================================================================
# run_all.R — Script maestro
# Corre todos los ejercicios en orden y genera los outputs HTML completos.
#
# INSTRUCCIONES:
#   1. Cambiar `root` a la ruta del proyecto en tu equipo (única línea a editar)
#   2. Correr este script completo
#
# OUTPUTS generados:
#   outputs/reporte_resultados.html  — documento ejecutivo en español
#   outputs/presentacion.html        — presentación reveal.js en español
#   outputs/report_results.html      — executive report in English
#   outputs/presentation.html        — reveal.js presentation in English
#   outputs/figures/fig_did_trends.png
# =============================================================================

root <- getwd()   # <- ÚNICA LÍNEA A MODIFICAR

# -----------------------------------------------------------------------------
# Setup (librerías, paths, datos) — se carga una sola vez
# -----------------------------------------------------------------------------

source(file.path(root, "code", "00_setup.R"))

# -----------------------------------------------------------------------------
# Ejercicios — producen objetos en el global env
# -----------------------------------------------------------------------------

cat("\n========== EJ 1: ATE ==========\n")
source(file.path(root, "code", "01_ej1_ate.R"))

cat("\n========== EJ 2: CATE ==========\n")
source(file.path(root, "code", "02_ej2_cate.R"))

cat("\n========== EJ 3: DISEÑO ==========\n")
source(file.path(root, "code", "03_ej3_diseno.R"))

cat("\n========== EJ 4: DiD (BONUS) ==========\n")
source(file.path(root, "code", "04_ej4_obs.R"))

# -----------------------------------------------------------------------------
# Render — genera los HTMLs a partir de los objetos calculados
# -----------------------------------------------------------------------------

cat("\n========== RENDER: ESPAÑOL ==========\n")
source(file.path(root, "code", "render_es.R"))

cat("\n========== RENDER: ENGLISH ==========\n")
source(file.path(root, "code", "render_en.R"))

cat("\n==============================================\n")
cat("Listo. Outputs en:\n")
cat("  [ES] ", file.path(root, "outputs", "reporte_resultados.html"), "\n")
cat("  [ES] ", file.path(root, "outputs", "presentacion.html"), "\n")
cat("  [EN] ", file.path(root, "outputs", "report_results.html"), "\n")
cat("  [EN] ", file.path(root, "outputs", "presentation.html"), "\n")
cat("==============================================\n")
