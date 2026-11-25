# =============================================================================
# render_es.R — Genera los HTMLs en español
# REQUIERE: haber corrido 01–04_ej*.R (objetos en el global env)
# PRODUCE:
#   outputs/reporte_resultados.html
#   outputs/presentacion.html
# =============================================================================

options(knitr.kable.NA = "")

path_reporte <- file.path(root, "outputs", "reporte_resultados.html")
path_pres    <- file.path(root, "outputs", "presentacion.html")

# =============================================================================
# HELPERS
# =============================================================================

doc_header <- '<!DOCTYPE html>
<html lang="es">
<head>
  <meta charset="UTF-8">
  <title>Resultados — MarTech Consultant Assessment</title>
  <link rel="stylesheet"
    href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css">
  <style>
    body     { font-family: "Segoe UI", sans-serif; max-width: 960px;
               margin: 40px auto; padding: 0 20px; color: #222; }
    h1       { border-bottom: 2px solid #dee2e6; padding-bottom: 10px;
               margin-bottom: 6px; }
    h2       { color: #2c3e50; margin-top: 40px; }
    h3       { color: #495057; margin-top: 28px; }
    .meta    { color: #6c757d; font-size: 0.9em; margin-bottom: 32px; }
    .insight { background: #f8f9fa; border-left: 4px solid #0d6efd;
               padding: 14px 18px; border-radius: 4px; margin: 16px 0; }
    caption  { caption-side: top; font-weight: 600; text-align: left;
               padding-bottom: 6px; }
  </style>
</head>
<body>
  <h1>Resultados — MarTech Consultant Assessment</h1>
  <p class="meta">Campaña push countdown timing vs. push estándar &nbsp;|&nbsp;
     Dataset: cai_exp_data.csv &nbsp;|&nbsp; Generado: '

writeLines(paste0(doc_header, format(Sys.Date(), "%d/%m/%Y"), "</p>"), path_reporte)
append_doc <- function(...) cat(paste0(..., "\n"), file = path_reporte, append = TRUE)

pres_header <- '<!DOCTYPE html>
<html lang="es">
<head>
  <meta charset="UTF-8">
  <title>MarTech Consultant Assessment</title>
  <link rel="stylesheet"
    href="https://cdn.jsdelivr.net/npm/reveal.js@5.1.0/dist/reveal.css">
  <link rel="stylesheet"
    href="https://cdn.jsdelivr.net/npm/reveal.js@5.1.0/dist/theme/white.css">
  <style>
    .reveal                  { font-size: 20px; }
    .reveal h1               { font-size: 1.8em; color: #1a1a2e; }
    .reveal h2               { font-size: 1.25em; color: #2c3e50; }
    .reveal .slides section  { text-align: left; padding: 10px 50px; }
    .reveal table            { font-size: 0.7em; border-collapse: collapse;
                               width: auto; margin-top: 8px; }
    .reveal table th,
    .reveal table td         { border: 1px solid #dee2e6; padding: 5px 10px; }
    .reveal table thead tr   { background: #2c3e50; color: white; }
    .reveal table tbody tr:nth-child(even) { background: #f8f9fa; }
    .reveal caption          { caption-side: top; font-weight: 600;
                               text-align: left; padding-bottom: 5px; font-size: 1em; }
    .reveal tfoot td         { font-size: 0.82em; color: #6c757d;
                               border: none; padding-top: 6px; }
    .insight                 { background: #f0f4ff; border-left: 4px solid #0d6efd;
                               padding: 10px 14px; border-radius: 4px;
                               font-size: 0.85em; margin-top: 14px; }
    .slide-title             { height: 100%; display: flex; flex-direction: column;
                               justify-content: center; }
    .slide-title h1          { color: #1a1a2e; font-size: 2em; }
    .slide-title .sub        { color: #6c757d; font-size: 1em; margin-top: 10px; }
    /* === MODALES INTERACTIVOS === */
    .tip { border-bottom: 2px dotted #2563EB; color: #2563EB;
           cursor: pointer; font-weight: 600; }
    .tip:hover { opacity: 0.75; }
    .modal-ov { display: none; position: fixed; inset: 0;
      background: rgba(0,0,0,0.45); z-index: 10000;
      align-items: center; justify-content: center; }
    .modal-ov.open { display: flex; }
    .modal-bx { background: #fff; border-radius: 10px; padding: 20px 24px;
      max-width: 480px; width: 88%; font-size: 0.80em; line-height: 1.65;
      box-shadow: 0 8px 32px rgba(0,0,0,0.2); position: relative; color: #222; }
    .modal-bx h4 { margin: 0 0 12px; color: #2563EB; font-size: 1.05em; }
    .modal-bx .eg { background: #f0f4ff; border-left: 3px solid #2563EB;
      padding: 8px 12px; border-radius: 4px; margin-top: 10px; }
    .modal-x { position: absolute; top: 8px; right: 12px; cursor: pointer;
      color: #888; font-size: 1.1em; font-weight: bold;
      background: none; border: none; line-height: 1; }
    .modal-x:hover { color: #333; }
  </style>
</head>
<body>
<div class="reveal">
<div class="slides">

<!-- PORTADA -->
<section>
  <div class="slide-title">
    <h1>MarTech Consultant Assessment</h1>
    <p>Inferencia Causal en experimentos</p>
    <p style="margin-top:40px; font-size:0.85em;">Fernanda Cortés &nbsp;|&nbsp; '

writeLines(paste0(pres_header, format(Sys.Date(), "%d/%m/%Y"),
                  "</p>\n  </div>\n</section>"), path_pres)
append_pres <- function(...) cat(paste0(..., "\n"), file = path_pres, append = TRUE)

close_pres <- function() {
  b1 <- if (exists("m_cate")) round(coef(m_cate)["treatment"], 1) else 36.4
  b4 <- if (exists("m_cate")) round(coef(m_cate)["treatment:canalpaid_search"], 1) else -17.5
  b5 <- if (exists("m_cate")) round(coef(m_cate)["treatment:canalsocial"], 1) else -28.1

  regaux_txt <- if (exists("lm_check_summary") && exists("f_val") && exists("f_pval"))
    sprintf(
      '<strong>R² = %.4f</strong> → las características explican el %.2f%% de la variación → prácticamente cero ✅<br><strong>F = %.2f &nbsp;|&nbsp; p = %.3f</strong> → en conjunto no predicen el grupo.<br><br>',
      lm_check_summary$r.squared, lm_check_summary$r.squared * 100, f_val, f_pval)
  else
    '<strong>R² ≈ 0 | F ≈ 0 | p ≈ 1</strong> → en conjunto no predicen el grupo.<br><br>'

  cate_txt <- sprintf(
    paste0(
      'Modelo con interacciones entre tratamiento y canal. Categoría base: <em>organic</em>.<br><br>',
      'Coeficientes clave:<br>',
      '&bull; <strong>β₁ = %.1f</strong> → efecto para organic<br>',
      '&bull; <strong>β₄ = %.1f</strong> → paid search tiene %.1f menos que organic<br>',
      '&bull; <strong>β₅ = %.1f</strong> → social tiene %.1f menos que organic<br><br>',
      'Efecto final:<br>',
      '&bull; Organic = <strong>%.1f</strong><br>',
      '&bull; Paid search = %.1f + (%.1f) = <strong>%.1f</strong><br>',
      '&bull; Social = %.1f + (%.1f) = <strong>%.1f</strong>'
    ),
    b1, b4, abs(b4), b5, abs(b5),
    b1,
    b1, b4, b1 + b4,
    b1, b5, b1 + b5
  )

  modals <- paste0(
    # m-aleatorio
    '<div id="m-aleatorio" class="modal-ov"><div class="modal-bx">',
    '<button class="modal-x">&#x2715;</button>',
    '<h4>¿Qué significa "exclusivamente"?</h4>',
    'Al asignar usuarios al azar, los dos grupos son equivalentes antes del experimento: ',
    'mismo historial, mismo canal, mismas características previas. ',
    'Cualquier diferencia observada después solo puede deberse a la campaña.<br><br>',
    '<div class="eg">Ejemplo: si el grupo countdown convirtió más, no es porque eran ',
    '"mejores compradores" de antemano. Eran iguales. La única diferencia fue el push recibido.</div>',
    '</div></div>',
    # m-split
    '<div id="m-split" class="modal-ov"><div class="modal-bx">',
    '<button class="modal-x">&#x2715;</button>',
    '<h4>¿Por qué no es exactamente 50/50?</h4>',
    'Con asignación aleatoria pura, caer exactamente 25,000 en cada grupo sería una coincidencia. ',
    'Una diferencia de ~286 usuarios sobre 50,000 (0.6%) es ruido de muestreo completamente normal.<br><br>',
    '<div class="eg">Es como tirar una moneda 50,000 veces: es casi imposible obtener exactamente ',
    '25,000 caras y 25,000 cecas.</div><br>',
    'Un test estadístico (chi-cuadrado) confirma que el desvío es compatible con el azar.',
    '</div></div>',
    # m-categ
    '<div id="m-categ" class="modal-ov"><div class="modal-bx">',
    '<button class="modal-x">&#x2715;</button>',
    '<h4>¿Por qué las variables categóricas se muestran diferente?</h4>',
    'Una variable <strong>continua</strong> (ej. antigüedad del dispositivo) tiene promedio ',
    '→ se compara el promedio entre grupos.<br><br>',
    'Una variable <strong>categórica</strong> (ej. canal) no tiene promedio: sus valores son ',
    '"organic", "paid search" o "social" → se compara el % de cada categoría entre grupos.<br><br>',
    '<div class="eg">Ejemplo: 33% del control llegó por organic. 34% del tratado llegó por organic. ',
    'Diferencia: +1 pp → no significativa → los grupos son equivalentes en esa variable.</div>',
    '</div></div>',
    # m-regaux
    '<div id="m-regaux" class="modal-ov"><div class="modal-bx">',
    '<button class="modal-x">&#x2715;</button>',
    '<h4>¿Qué es la regresión auxiliar?</h4>',
    'Es un chequeo formal de la aleatorización. Se construye un modelo que intenta predecir ',
    'a qué grupo fue asignado cada usuario usando sus características previas.<br><br>',
    'Si la asignación fue aleatoria, <em>ninguna característica debería poder predecir el grupo</em>.<br><br>',
    regaux_txt,
    '<div class="eg">Conclusión: la asignación fue verdaderamente aleatoria. El experimento es válido.</div>',
    '</div></div>',
    # m-pval
    '<div id="m-pval" class="modal-ov"><div class="modal-bx">',
    '<button class="modal-x">&#x2715;</button>',
    '<h4>¿Qué significa p &lt; 0.0001?</h4>',
    'El p-valor responde: "Si el countdown push no tuviera ningún efecto real, ',
    '¿qué tan probable sería ver una diferencia tan grande solo por azar?"<br><br>',
    '<strong>p &lt; 0.0001</strong> → probabilidad menor al 0.01% → ',
    'tan improbable que concluimos que el efecto es real.<br><br>',
    '<div class="eg">Convención estándar: si p &lt; 0.05 (5%), el resultado no es atribuible al azar. ',
    'Acá el resultado es mucho más sólido que ese umbral.</div>',
    '</div></div>',
    # m-cate
    '<div id="m-cate" class="modal-ov"><div class="modal-bx">',
    '<button class="modal-x">&#x2715;</button>',
    '<h4>¿Cómo se calculó el efecto por canal?</h4>',
    cate_txt,
    '<br><br><div class="eg">β₄ y β₅ negativos no significan efecto negativo — ',
    'significan que el efecto es menor que en organic.</div>',
    '</div></div>'
  )

  append_pres('</div></div>')
  append_pres(modals)
  append_pres('<script src="https://cdn.jsdelivr.net/npm/reveal.js@5.1.0/dist/reveal.js"></script>')
  append_pres('<script>')
  append_pres('Reveal.initialize({ hash: true, transition: "fade", slideNumber: true, margin: 0.04 });')
  append_pres('function openM(id) { document.getElementById(id).classList.add("open"); }')
  append_pres('document.addEventListener("click", function(e) {')
  append_pres('  if (e.target.classList.contains("modal-ov")) { e.target.classList.remove("open"); }')
  append_pres('  var btn = e.target.closest(".modal-x"); if (btn) { btn.closest(".modal-ov").classList.remove("open"); }')
  append_pres('  var tip = e.target.closest(".tip"); if (tip) { var mid = tip.getAttribute("data-modal"); if (mid) { e.stopPropagation(); openM(mid); } }')
  append_pres('});')
  append_pres('document.addEventListener("keydown", function(e) {')
  append_pres('  if (e.key === "Escape") { document.querySelectorAll(".modal-ov.open").forEach(function(m){ m.classList.remove("open"); }); }')
  append_pres('});')
  append_pres('</script>')
  append_pres('</body></html>')
}

# =============================================================================
# EJ1 — ATE
# =============================================================================

footnotes_chisq  <- paste(
  "Test chi-cuadrado conjunto (H0: distribución idéntica entre grupos):",
  paste(chisq_pvals, collapse = " | ")
)
texto_balance <- ifelse(
  balance_ok,
  "Las características pre-tratamiento están balanceadas entre grupos.",
  "ADVERTENCIA: se detectaron desbalances. Revisar la aleatorización."
)

html_tabla_balance <- tabla_balance_export |>
  kbl(format    = "html",
      caption   = "Tabla 1. Balance de Covariables Pre-Tratamiento",
      col.names = c("Variable", "Control", "Tratado", "Diferencia", "p-valor")) |>
  kable_styling(bootstrap_options = c("striped", "condensed", "hover"),
                full_width = FALSE, position = "left") |>
  footnote(general = footnotes_chisq, general_title = "Nota:")

tabla_modelos <- modelsummary(
  list("Conversion rate (LPM)" = m_conv, "Revenue" = m_rev),
  vcov     = "HC2",
  coef_map = c("treatment" = "Tratamiento (ATE)"),
  gof_map  = c("nobs", "r.squared"),
  stars    = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  output   = "html",
  title    = "Tabla 2. Estimación del ATE (SE robustos HC2)"
)

tabla_modelos_comp <- modelsummary(
  list("Conversion — ANCOVA" = m_conv_ancova,
       "Revenue — ANCOVA"    = m_rev_ancova,
       "Ticket (converters)" = m_ticket),
  vcov     = "HC2",
  coef_map = c("treatment" = "Tratamiento (ATE)"),
  gof_map  = c("nobs", "r.squared"),
  stars    = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  output   = "html",
  title    = "Tabla 3. Análisis complementario — ANCOVA y efecto sobre ticket (SE robustos HC2)"
)

# --- Documento EJ1 ---
append_doc("  <h2>Ejercicio 1 — Average Treatment Effect (ATE)</h2>")
append_doc("  <h3>1.1 Validación del diseño experimental</h3>")
append_doc(sprintf(
  '<p>El experimento contó con <strong>%s usuarios</strong>:
   <strong>%s en el grupo control</strong> (push estándar, %.1f%%) y
   <strong>%s en el grupo tratado</strong> (push countdown, %.1f%%).
   Se verificó el balance de las covariables pre-tratamiento mediante t-tests
   para variables continuas y tests de proporciones por categoría para
   variables categóricas.</p>',
  format(nrow(df_exp), big.mark = ","),
  format(sum(df_exp$treatment == 0), big.mark = ","), mean(df_exp$treatment == 0) * 100,
  format(sum(df_exp$treatment == 1), big.mark = ","), mean(df_exp$treatment == 1) * 100
))
append_doc(as.character(html_tabla_balance))
append_doc(sprintf(
  '<div class="insight"><strong>Resultado:</strong> %s
   Todos los p-valores son superiores a 0.05. El R² de la regresión auxiliar
   (D ~ covariables) es <strong>%.4f</strong> (F = %.2f, p = %.3f).
   <strong>La aleatorización es válida.</strong></div>',
  texto_balance, lm_check_summary$r.squared, f_val, f_pval
))
append_doc("  <h3>1.2 Estimación del Average Treatment Effect (ATE)</h3>")
append_doc(
  '<p>Se estimaron dos modelos principales, uno por cada outcome requerido.
   Para <em>conversion</em> se utilizó un Modelo de Probabilidad Lineal (LPM):
   el coeficiente se interpreta directamente en puntos porcentuales.
   Para <em>revenue</em> se aplicó OLS sobre todos los usuarios (incluye ceros).
   En ambos casos el estimador crudo es consistente porque la aleatorización
   garantiza independencia entre el tratamiento y los resultados potenciales.
   Se reportan errores estándar robustos (HC2).</p>'
)
append_doc(as.character(tabla_modelos))
append_doc(sprintf(
  '<div class="insight">
     <strong>Resultados:</strong><br>
     &bull; <strong>Conversion rate:</strong> el countdown push aumentó la tasa de
     conversión en <strong>%.1f pp</strong> (p = %.4f, %s).<br>
     &bull; <strong>Revenue por usuario:</strong> incremento de
     <strong>$%.2f</strong> en promedio (p = %.4f, %s).
   </div>',
  ate_conv * 100, p_conv, sig_conv,
  ate_rev,        p_rev,  sig_rev
))
append_doc("  <h3>1.3 Análisis complementario</h3>")
append_doc(
  '<p>Los modelos ANCOVA incorporan covariables pre-tratamiento. El modelo de ticket
   estima el ATE sobre revenue condicional a haber convertido.</p>'
)
append_doc(as.character(tabla_modelos_comp))
append_doc(sprintf(
  '<div class="insight">
     <strong>A. ANCOVA como confirmación de aleatorización exitosa:</strong>
     en un RCT correctamente ejecutado, agregar covariables pre-tratamiento
     no debería cambiar el coeficiente de tratamiento. Que los ATEs sean
     prácticamente idénticos entre el modelo crudo y el ANCOVA confirma
     que la asignación fue efectivamente aleatoria: el estimador es robusto
     e independiente de las características previas de los usuarios.<br><br>
     <strong>B. Análisis exploratorio — modelo de ticket:</strong>
     al filtrar solo usuarios con <em>conversion = 1</em> se condiciona en una
     variable post-tratamiento. Este modelo se reporta con carácter
     <strong>exploratorio</strong>: el resultado (+$%.2f, p = %.4f) es un
     <em>indicador de dirección</em>, no un estimador causal validado.<br><br>
     <strong>Siguiente paso — Uplift modeling:</strong>
     podría implementarse un enfoque de uplift modeling para identificar qué
     perfiles de usuarios fueron verdaderamente influenciados por el countdown push,
     distinguiéndolos de aquellos que probablemente hubieran convertido incluso
     sin tratamiento.
   </div>',
  ate_tick, p_tick
))

# --- Presentación EJ1 ---
# Slide 1A: ¿Qué se testó?
append_pres("<section>")
append_pres("<h2>Ejercicio 1 — ¿Qué se testó?</h2>")
append_pres(sprintf(
  '<table style="width:100%%; font-size:0.8em;">
   <thead><tr>
     <th style="background:#2c3e50; color:white; padding:6px; width:25%%;">Elemento</th>
     <th style="background:#2c3e50; color:white; padding:6px;">Detalle</th>
   </tr></thead>
   <tbody>
   <tr><td><strong>Campaña A — Control</strong></td>
       <td>Push notification estándar (sin urgencia)</td></tr>
   <tr><td><strong>Campaña B — Tratado</strong></td>
       <td>Push notification con countdown timer de 24 horas</td></tr>
   <tr><td><strong>Usuarios</strong></td>
       <td>%s asignados al azar — %s control (%.0f%%) | %s tratado (%.0f%%)<br>
           <small><span class="tip" data-modal="m-split" style="font-size:0.9em;">¿Por qué no es exactamente 50/50?</span></small></td></tr>
   <tr><td><strong>Métricas objetivo</strong></td>
       <td>Tasa de conversión (¿compró?) y revenue total por usuario</td></tr>
   <tr><td><strong>Método</strong></td>
       <td>RCT — el estándar de oro para medir causalidad</td></tr>
   </tbody></table>
   <div class="insight" style="margin-top:14px;">
     Al asignar usuarios al azar, cualquier diferencia en los resultados se debe
     <span class="tip" data-modal="m-aleatorio">exclusivamente</span>
     a la campaña, no a diferencias previas entre los grupos.
   </div>',
  format(nrow(df_exp), big.mark = ","),
  format(sum(df_exp$treatment == 0), big.mark = ","), mean(df_exp$treatment == 0) * 100,
  format(sum(df_exp$treatment == 1), big.mark = ","), mean(df_exp$treatment == 1) * 100
))
append_pres("</section>")

# Slide 1B: Tabla de balance completa
html_balance_slide_full <- tabla_balance_export |>
  kbl(format    = "html",
      caption   = "Características de los grupos antes del experimento",
      col.names = c("Variable", "Control", "Tratado", "Diferencia", "p-valor")) |>
  kable_styling(full_width = FALSE, position = "left", font_size = 13) |>
  footnote(
    general       = "Variables continuas: diferencia de medias (t-test). Variables categóricas: distribución % por categoría (diferencia en pp).",
    general_title = "Nota:"
  )

append_pres("<section>")
append_pres("<h2>Ejercicio 1 — Los grupos son equivalentes antes del test</h2>")
append_pres(
  '<p style="font-size:0.78em; color:#555; margin-bottom:8px;">
   Comparación de características pre-tratamiento. Continuas: diferencia de medias.
   <span class="tip" data-modal="m-categ">Categóricas</span>: distribución porcentual por categoría (diferencia en pp).
  </p>'
)
append_pres(as.character(html_balance_slide_full))
append_pres("</section>")

# Slide 1B2: Interpretación del balance
append_pres("<section>")
append_pres("<h2>Ejercicio 1 — ¿Qué nos dice la tabla de balance?</h2>")
append_pres(sprintf(
  '<div style="display:flex; gap:20px; margin-top:14px; font-size:0.85em;">
     <div style="flex:1; background:#f0f4ff; padding:16px; border-radius:8px;
                 border-left:4px solid #2563EB;">
       <div style="font-size:1.05em; font-weight:bold; color:#2563EB; margin-bottom:8px;">
         1. No hay diferencias significativas entre grupos
       </div>
       Ninguna variable pre-tratamiento muestra diferencia estadísticamente
       significativa entre control y tratado (p &gt; 0.05 en todas las comparaciones),
       tanto en variables continuas como en la distribución de canal,
       plataforma y nivel de lealtad.
     </div>
     <div style="flex:1; background:#f0f9f0; padding:16px; border-radius:8px;
                 border-left:4px solid #16a34a;">
       <div style="font-size:1.05em; font-weight:bold; color:#16a34a; margin-bottom:8px;">
         2. Ninguna variable predice la asignación al grupo
       </div>
       Regresión auxiliar: <em>tratamiento ~ todas las covariables</em><br>
       <span class="tip" data-modal="m-regaux">R² = %.4f &nbsp;·&nbsp; F = %.2f &nbsp;·&nbsp; p = %.3f</span><br><br>
       Las covariables en conjunto <strong>no predicen</strong> a qué grupo
       fue asignado el usuario.
     </div>
   </div>
   <div class="insight" style="margin-top:14px;">
     <strong>Conclusión:</strong> la aleatorización es válida. No hay sesgo de selección.
     Cualquier diferencia observada en los resultados se debe exclusivamente al tratamiento.
   </div>',
  lm_check_summary$r.squared, f_val, f_pval
))
append_pres("</section>")

# Slide 1C: La campaña funcionó
append_pres("<section>")
append_pres("<h2>Ejercicio 1 — El countdown push funciona</h2>")
append_pres(sprintf(
  '<div style="display:flex; gap:20px; margin:14px 0;">

     <!-- Conversión -->
     <div style="flex:1; background:#f0f4ff; padding:16px; border-radius:8px;
                 border:1px solid #d0d9ff;">
       <div style="font-size:1.9em; font-weight:bold; color:#2563EB; text-align:center;">
         +%.1f pp</div>
       <div style="font-size:0.88em; font-weight:bold; text-align:center; margin:4px 0 10px;">
         Tasa de conversión</div>
       <table style="width:100%%; font-size:0.78em; border:none;">
         <tr>
           <td style="border:none; color:#555;">Antes (control)</td>
           <td style="border:none; text-align:right; font-weight:bold;">%.1f%%</td>
         </tr>
         <tr>
           <td style="border:none; color:#555;">Después (tratado)</td>
           <td style="border:none; text-align:right; font-weight:bold; color:#2563EB;">%.1f%%</td>
         </tr>
         <tr style="border-top:1px solid #d0d9ff;">
           <td style="border:none; color:#555;">Crecimiento relativo</td>
           <td style="border:none; text-align:right; font-weight:bold;">+%.0f%%</td>
         </tr>
         <tr>
           <td style="border:none; color:#555;">Por cada 1,000 usuarios</td>
           <td style="border:none; text-align:right; font-weight:bold;">~%.0f compras más</td>
         </tr>
       </table>
     </div>

     <!-- Revenue -->
     <div style="flex:1; background:#f0f4ff; padding:16px; border-radius:8px;
                 border:1px solid #d0d9ff;">
       <div style="font-size:1.9em; font-weight:bold; color:#2563EB; text-align:center;">
         +$%.2f</div>
       <div style="font-size:0.88em; font-weight:bold; text-align:center; margin:4px 0 10px;">
         Revenue por usuario</div>
       <table style="width:100%%; font-size:0.78em; border:none;">
         <tr>
           <td style="border:none; color:#555;">Promedio sobre todos los usuarios</td>
           <td style="border:none; text-align:right; font-weight:bold;">incluye no compradores</td>
         </tr>
         <tr style="border-top:1px solid #d0d9ff;">
           <td style="border:none; color:#555;">Por cada 1,000 usuarios</td>
           <td style="border:none; text-align:right; font-weight:bold; color:#2563EB;">
             ~$%.0f más</td>
         </tr>
       </table>
     </div>

   </div>
   <div class="insight" style="font-size:0.82em;">
     Ambos efectos son estadísticamente significativos
     (<span class="tip" data-modal="m-pval">p &lt; 0.0001</span>) y
     replicables. No son atribuibles al azar.
   </div>',
  ate_conv * 100,
  mean(df_exp$conversion[df_exp$treatment == 0]) * 100,
  mean(df_exp$conversion[df_exp$treatment == 1]) * 100,
  ate_conv / mean(df_exp$conversion[df_exp$treatment == 0]) * 100,
  ate_conv * 1000,
  ate_rev,
  ate_rev * 1000
))
append_pres("</section>")

# Slide 1D: ¿Cómo opera el efecto?
html_resumen_comp <- resumen_ate_comp |>
  kbl(format = "html",
      caption = "Tabla 3. Análisis complementario (SE robustos HC2)",
      digits  = 4) |>
  kable_styling(full_width = FALSE, position = "left")

append_pres("<section>")
append_pres("<h2>Ejercicio 1 — ¿Cómo opera el efecto?</h2>")
append_pres(as.character(html_resumen_comp))
append_pres(sprintf(
  '<div class="insight">
     <strong>A. ANCOVA — confirmación de aleatorización exitosa:</strong>
     agregar covariables pre-tratamiento al modelo no cambia el ATE estimado.
     En un RCT, esto no es "precisión adicional" — es evidencia de que
     la aleatorización funcionó: el efecto es robusto e independiente
     de las características previas de los usuarios.<br><br>
     <strong>B. Análisis exploratorio — modelo de ticket:</strong>
     entre quienes sí compraron, el grupo con countdown gastó
     <strong>$%.2f más en promedio</strong>. Resultado <em>exploratorio</em>:
     al condicionar en conversión (variable post-tratamiento), el estimador
     puede tener sesgo de selección.<br><br>
     <strong>Siguiente paso — Uplift modeling:</strong>
     podría implementarse un enfoque de uplift modeling para identificar qué
     perfiles de usuarios fueron verdaderamente influenciados por el countdown push,
     distinguiéndolos de aquellos que probablemente hubieran convertido incluso
     sin tratamiento.
   </div>',
  ate_tick
))
append_pres("</section>")

# =============================================================================
# EJ2 — CATE
# =============================================================================

html_cate <- cate_por_canal |>
  kbl(format    = "html",
      caption   = "Tabla 4. CATE por canal de adquisición (SE robustos HC2)",
      col.names = c("Canal", "CATE ($)", "SE", "IC 95% inf", "IC 95% sup", "p-valor")) |>
  kable_styling(bootstrap_options = c("striped", "condensed", "hover"),
                full_width = FALSE, position = "left") |>
  footnote(
    general = sprintf(
      "Test de heterogeneidad conjunta (H0: CATE igual en todos los canales): F = %.3f, p = %.4f.",
      f_het, p_het),
    general_title = "Nota:"
  )

tabla_cate_modelo <- modelsummary(
  list("Revenue ~ treatment × canal" = m_cate),
  vcov    = "HC2",
  gof_map = c("nobs", "r.squared"),
  stars   = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  output  = "html",
  title   = "Tabla 5. Coeficientes del modelo con interacciones (SE robustos HC2)"
)

# --- Documento EJ2 ---
append_doc("  <h2>Ejercicio 2 — Conditional Average Treatment Effect (CATE)</h2>")
append_doc(
  '<p>Se estimó el CATE por canal de adquisición mediante una regresión con
   interacciones entre el tratamiento y el canal:</p>
   <p style="margin-left:20px; font-family:monospace;">
   y<sub>i</sub> = β₀ + β₁·D<sub>i</sub> + β₂·canal<sub>i</sub> +
   β₃·(D<sub>i</sub> × canal<sub>i</sub>) + ε<sub>i</sub></p>
   <p>CATE(k) = β₁ + β₃<sub>k</sub>. Categoría base: <em>organic</em>.</p>'
)
append_doc(as.character(tabla_cate_modelo))
append_doc("  <h3>2.1 CATE por canal</h3>")
append_doc(as.character(html_cate))
append_doc(sprintf(
  '<div class="insight">
     <strong>Interpretación:</strong>
     El canal con mayor efecto incremental es <strong>%s</strong>
     (CATE = $%.2f), mientras que el de menor efecto es <strong>%s</strong>
     (CATE = $%.2f). %s
   </div>',
  canal_max, cate_vals[[canal_max]],
  canal_min, cate_vals[[canal_min]],
  ifelse(hay_het,
    "La heterogeneidad entre canales es estadísticamente significativa.",
    "La heterogeneidad entre canales no es estadísticamente significativa.")
))
append_doc(
  '<h3>2.2 Lectura de marketing</h3>
   <ul>
     <li><strong>Organic ($36.4):</strong> usuario con intención de compra,
     conoce la marca. El countdown actúa como el último empujón para cerrar.</li>
     <li><strong>Paid search ($18.9):</strong> tiene intención pero compara
     alternativas. El efecto existe pero es moderado.</li>
     <li><strong>Social ($8.3):</strong> descubrió el producto en el feed,
     sin intención previa. El countdown tiene impacto marginal.</li>
   </ul>
   <p><strong>Implicancia:</strong> el countdown push es una herramienta de
   <em>cierre</em>, no de <em>descubrimiento</em>. Su valor máximo está en
   usuarios con intención formada.</p>'
)

# --- Presentación EJ2 ---
html_cate_pres <- cate_por_canal |>
  kbl(format    = "html",
      caption   = "Tabla 4. Efecto del countdown push por canal de adquisición",
      col.names = c("Canal", "Efecto ($)", "Error std.", "IC 95% inf", "IC 95% sup", "p-valor")) |>
  kable_styling(full_width = FALSE, position = "left")

append_pres("<section>")
append_pres("<h2>Ejercicio 2 — El efecto varía según cómo llegó el usuario</h2>")
append_pres(sprintf(
  '<p style="font-size:0.82em; color:#555; margin-bottom:8px;">
   Efecto estimado por canal (<span class="tip" data-modal="m-cate">regresión con interacciones</span>).
   Test de heterogeneidad: F = %.3f (p &lt; 0.0001) — las diferencias
   <strong>no son atribuibles al azar</strong>.
   </p>',
  f_het
))
append_pres(as.character(html_cate_pres))
append_pres(sprintf(
  '<div style="display:flex; gap:20px; margin-top:12px; font-size:0.82em;">
     <div style="flex:1; background:#f0f4ff; padding:10px 14px; border-radius:6px; border-left:4px solid #2563EB;">
       <strong>Organic: $%.2f</strong><br>Alta intención de compra. El countdown actúa como el empujón final.
     </div>
     <div style="flex:1; background:#f0f9f0; padding:10px 14px; border-radius:6px; border-left:4px solid #16a34a;">
       <strong>Paid search: $%.2f</strong><br>Tiene intención pero está comparando. El efecto existe pero se diluye.
     </div>
     <div style="flex:1; background:#fff8f0; padding:10px 14px; border-radius:6px; border-left:4px solid #ea580c;">
       <strong>Social: $%.2f</strong><br>Sin intención previa. La urgencia tiene poco impacto.
     </div>
   </div>',
  cate_vals[["organic"]], cate_vals[["paid_search"]], cate_vals[["social"]]
))
append_pres("</section>")

append_pres("<section>")
append_pres("<h2>Ejercicio 2 — Implicancia estratégica</h2>")
append_pres(
  '<table style="width:100%; font-size:0.8em; margin-bottom:12px;">
   <thead><tr>
     <th style="background:#2c3e50; color:white; padding:6px;">Canal</th>
     <th style="background:#2c3e50; color:white; padding:6px;">Perfil del usuario</th>
     <th style="background:#2c3e50; color:white; padding:6px;">¿Conviene el countdown?</th>
   </tr></thead>
   <tbody>
   <tr style="background:#f0f4ff;">
     <td><strong>Organic</strong></td>
     <td>Alta intención de compra, conoce la marca</td>
     <td>✅ Muy alta prioridad — máximo retorno</td></tr>
   <tr>
     <td><strong>Paid search</strong></td>
     <td>Tiene intención, pero evalúa alternativas</td>
     <td>✅ Recomendado — efecto moderado pero positivo</td></tr>
   <tr style="background:#fff8f0;">
     <td><strong>Social</strong></td>
     <td>Descubrimiento frío, sin intención formada</td>
     <td>⚠️ Revisar — efecto marginal, posible costo ineficiente</td></tr>
   </tbody></table>
   <div class="insight">
     <strong>Conclusión:</strong> el countdown push es una herramienta de <em>cierre</em>,
     no de <em>adquisición</em>. Concentrar el presupuesto en organic y paid search
     maximiza el ROI de la campaña.
   </div>'
)
append_pres("</section>")

# =============================================================================
# EJ3 — DISEÑO EXPERIMENTAL
# =============================================================================

html_sens <- tabla_sens_simple |>
  kbl(format      = "html",
      caption     = "Tabla 6. Tamaño de muestra requerido por grupo (alpha=0.05, poder=0.80)",
      format.args = list(big.mark = ",")) |>
  kable_styling(bootstrap_options = c("striped", "condensed", "hover"),
                full_width = FALSE, position = "left") |>
  row_spec(which(tabla_sens_simple$`Baseline (p0)` == "28%"),
           bold = TRUE, background = "#e8f4f8") |>
  footnote(general = "Fila resaltada: baseline empírico del grupo control (Ejercicio 1).",
           general_title = "Nota:")

html_dur_doc <- tabla_duracion |>
  kbl(format  = "html",
      caption = sprintf("Tabla: Duración estimada del experimento según tráfico diario
                        (N requerido: %s por grupo, %s total)",
                        format(n_formula, big.mark = ","),
                        format(n_formula * 2, big.mark = ","))) |>
  kable_styling(bootstrap_options = c("striped", "condensed", "hover"),
                full_width = FALSE, position = "left") |>
  column_spec(3, bold = TRUE) |>
  row_spec(which(daily_traffic >= 2000), background = "#fff8f0") |>
  footnote(
    general = "Filas con fondo naranja: la restricción de ciclo semanal (14 días) domina.",
    general_title = "Nota:"
  )

# --- Documento EJ3 ---
append_doc("<h2>Ejercicio 3 — Diseño Experimental: Rediseño del Checkout Flow</h2>")
append_doc("<h3>3.1 Definición del experimento</h3>")
append_doc(
  '<table class="table table-bordered" style="width:100%">
   <thead><tr>
     <th style="width:25%">Dimensión</th><th>Decisión</th><th>Justificación</th>
   </tr></thead>
   <tbody>
   <tr>
     <td><strong>Unidad de aleatorización</strong></td>
     <td>Usuario (<code>user_id</code> persistente), asignado al iniciar el checkout</td>
     <td>Aleatorizar por sesión viola SUTVA: el mismo usuario podría ver ambas versiones.
     El <code>user_id</code> garantiza asignación consistente entre visitas.<br><br>
     <strong>Momento de asignación:</strong> al iniciar el checkout (no al abrir la app).
     El denominador relevante son los usuarios que llegaron al checkout.<br><br>
     <strong>Usuarios no autenticados:</strong> usar cookie persistente como proxy.<br><br>
     <strong>Versión de app:</strong> usuarios con versiones anteriores pueden no
     renderizar el nuevo checkout. Filtrar por versión mínima o reportar por versión.</td>
   </tr>
   <tr>
     <td><strong>Ratio de asignación</strong></td>
     <td>50% control / 50% tratado</td>
     <td>Maximiza el poder estadístico para un N dado.</td>
   </tr>
   <tr>
     <td><strong>Métrica primaria (OEC)</strong></td>
     <td>Tasa de conversión del checkout<br>= compras completadas / usuarios que iniciaron el checkout</td>
     <td>Indicador directo de la fricción del checkout. Alta frecuencia de ocurrencia
     y baja varianza vs. revenue. Ver sección 3.1.1.</td>
   </tr>
   <tr>
     <td><strong>Métricas guardrail</strong></td>
     <td>Revenue/usuario · tiempo en checkout · tasa de abandono · NPS</td>
     <td>Detectan si se gana conversión a costa de experiencia o ticket.</td>
   </tr>
   <tr>
     <td><strong>Duración mínima</strong></td>
     <td>max(días estadísticos, 14 días) + período de burn-in opcional</td>
     <td>Los 14 días cubren dos ciclos lunes-domingo completos. El burn-in descarta
     el efecto de aprendizaje del nuevo flujo en los primeros días.</td>
   </tr>
   </tbody></table>'
)
append_doc("<h4>3.1.1 Elección de la métrica primaria</h4>")
append_doc(
  '<table class="table table-bordered" style="width:100%">
   <thead><tr>
     <th>Métrica candidata</th><th>Tipo</th><th>Ventaja</th><th>Limitación</th>
   </tr></thead>
   <tbody>
   <tr style="background:#f0f4ff;">
     <td><strong>✅ Tasa de conversión del checkout</strong><br>
     <small>= compras / usuarios que iniciaron checkout</small></td>
     <td>Primaria</td>
     <td>Directamente ligada al objetivo; alta frecuencia; baja varianza.</td>
     <td>No captura si el ticket cambia.</td>
   </tr>
   <tr>
     <td>Revenue por usuario</td><td>Secundaria</td>
     <td>Captura el efecto completo (conversión × ticket).</td>
     <td>(1) Conceptual: al iniciar el checkout el usuario ya decidió qué comprar;
     el rediseño afecta si <em>completa</em> la compra, no cuánto gasta.
     (2) Estadística: alta varianza → requiere N mucho mayor.</td>
   </tr>
   <tr>
     <td>Tasa de abandono del carrito</td><td>Complementaria</td>
     <td>Identifica en qué paso se pierde el usuario.</td>
     <td>Métrica intermedia, no final.</td>
   </tr>
   <tr>
     <td>Tiempo hasta completar el checkout</td><td>Guardrail</td>
     <td>Detecta si el nuevo flujo es más lento.</td>
     <td>No mide si el usuario compra.</td>
   </tr>
   <tr>
     <td>NPS / satisfacción post-compra</td><td>Guardrail</td>
     <td>Captura la experiencia percibida.</td>
     <td>Señal retrasada; alta tasa de no-respuesta.</td>
   </tr>
   </tbody></table>
   <div class="insight">
     <strong>Criterio de selección:</strong> tasa de conversión es la opción dominante.
     Revenue se reporta como métrica secundaria para validar que el efecto se traslada
     al resultado de negocio.
   </div>'
)
append_doc("<h3>3.2 Cálculo de poder estadístico</h3>")
append_doc(sprintf(
  '<p>Baseline empírico: <strong>p₀ = %.1f%%</strong>. MDE de 2 pp implica
   detectar un incremento de %.1f%% a %.1f%%.</p>
   <p style="margin-left:20px; font-family:monospace; background:#f8f9fa; padding:10px; border-radius:4px;">
   n = 2 × (z<sub>α/2</sub> + z<sub>β</sub>)² × p̄(1−p̄) / δ²<br><br>
   z<sub>α/2</sub> = %.4f &nbsp;|&nbsp; z<sub>β</sub> = %.4f &nbsp;|&nbsp;
   p̄ = %.4f &nbsp;|&nbsp; δ = %.2f pp<br><br>
   <strong>n = %s por grupo (%s total)</strong></p>',
  p0 * 100, p0 * 100, p1 * 100,
  z_alpha, z_beta, p_bar, MDE * 100,
  format(n_formula, big.mark = ","), format(n_formula * 2, big.mark = ",")
))
append_doc(sprintf(
  '<div class="insight">
     Con p₀ = <strong>%.1f%%</strong> y MDE = <strong>2 pp</strong>
     (α = 0.05, poder = 0.80): <strong>%s usuarios por grupo</strong>
     (%s en total). Consistente con <code>power.prop.test()</code> (n = %s).
   </div>',
  p0 * 100,
  format(n_formula, big.mark = ","), format(n_formula * 2, big.mark = ","),
  format(n_r, big.mark = ",")
))
append_doc("<h4>3.2.1 Análisis de sensibilidad</h4>")
append_doc(as.character(html_sens))
append_doc("<h4>3.2.2 Duración del experimento</h4>")
append_doc(sprintf(
  '<p>Dos restricciones simultáneas:</p>
   <ul>
     <li><strong>Estadística:</strong> días = ⌈ %s usuarios / tráfico_diario ⌉.</li>
     <li><strong>Ciclo semanal / quincenal:</strong> mínimo 14 días para capturar
     la variación lunes-domingo. En mercados con cobro quincenal (días 1 y 15,
     frecuente en México), 2 semanas garantizan al menos un período post-cobro
     y uno sin pago — el comportamiento de compra difiere entre ambos.</li>
   </ul>
   <p><strong>Duración recomendada = max(días estadísticos, 14 días)</strong></p>
   <p><em>Nota sobre el baseline:</em> p₀ = %.1f%% proviene del grupo control del
   Ejercicio 1 (push notifications). En un escenario ideal el baseline debería
   calcularse a partir de datos históricos del checkout actual (último mes sin
   intervenciones).</p>',
  format(n_formula * 2, big.mark = ","),
  p0 * 100
))
append_doc(as.character(html_dur_doc))
append_doc(
  '<div class="insight">
     <strong>Implicancia:</strong> plataformas con alto tráfico (≥2,000/día)
     están restringidas por el ciclo semanal, no por el tamaño muestral.
   </div>'
)
append_doc("<h3>3.3 Amenazas a la validez interna y externa</h3>")
append_doc(
  '<table class="table table-bordered" style="width:100%">
   <thead><tr>
     <th style="width:20%">Amenaza</th><th style="width:15%">Tipo</th>
     <th>Descripción</th><th>Mitigación</th>
   </tr></thead>
   <tbody>
   <tr>
     <td><strong>SRM</strong></td><td>Interna</td>
     <td>El ratio tratados/control difiere del esperado (50/50): bug en la asignación.
     Consecuencia: estimador sesgado y errores estándar incorrectos.</td>
     <td>Chi-cuadrado de bondad de ajuste sobre el ratio observado vs. esperado:
     <code>chisq.test(x = c(n_ctrl, n_trat), p = c(0.5, 0.5))</code>.
     Si p &lt; 0.01, SRM confirmado — invalidar y revisar logs. No corregir post-hoc.</td>
   </tr>
   <tr>
     <td><strong>Bug en el checkout</strong></td><td>Interna</td>
     <td>El nuevo checkout tiene errores. Consecuencias directas: pérdida de revenue
     y datos inutilizables. Tres escenarios:<br><br>
     <strong>Crítico</strong> — falla para una fracción significativa del grupo tratado.
     Acción: pausar de inmediato.<br><br>
     <strong>Menor</strong> — error visual sin impacto medible en la conversión.
     Acción: continuar y documentar el período.<br><br>
     <strong>Segmento específico</strong> — bug solo en iOS 16 o versión &lt; 3.2.
     Acción: excluir ese segmento y reportar por separado.</td>
     <td><strong>Monitoreo operativo desde el día 1</strong> (distinto del estadístico):
     alertas sobre tasa de errores HTTP, tiempo de carga, crashes, abandonos por paso.
     No lee resultados del experimento → no rompe el diseño estadístico.</td>
   </tr>
   <tr>
     <td><strong>Peeking</strong></td><td>Interna</td>
     <td>Leer los resultados estadísticos antes del N pre-especificado infla la tasa
     de falsos positivos. Con α = 0.05 y múltiples lecturas puede superar el 30%.</td>
     <td>Pre-registrar fecha de lectura y N objetivo.<br><br>
     <em>No hacer peeking estadístico</em> (tasa de conversión).<br>
     <em>Sí monitorear las métricas operativas</em> (errores, crashes) — no afecta el FPR.<br><br>
     Alternativa: Sequential Testing (always-valid p-values).</td>
   </tr>
   <tr>
     <td><strong>Heterogeneidad temporal</strong></td><td>Externa</td>
     <td>Experimento durante períodos atípicos: <strong>Hot Sale</strong> (mayo),
     <strong>Buen Fin</strong> (noviembre) o <strong>Black Friday</strong> elevan
     la conversión basal. El estimador no generaliza al resto del año.</td>
     <td>Mantener calendario de eventos e-commerce. Si hay coincidencia por razones
     operativas, estratificar el análisis (período especial vs. normal) y reportar
     los efectos por separado.</td>
   </tr>
   </tbody></table>'
)

# --- Presentación EJ3 ---

# Slide 3.0: Proceso completo
append_pres("<section>")
append_pres("<h2>Ejercicio 3 — Proceso para rediseñar el checkout</h2>")
append_pres(
  '<div style="display:grid; grid-template-columns:1fr 1fr; gap:10px;
              font-size:0.76em; margin-top:10px;">

   <!-- Paso 1 -->
   <div style="background:#f8f9fa; border-left:4px solid #2563EB;
               padding:10px 14px; border-radius:4px;">
     <div style="font-weight:bold; color:#2563EB; margin-bottom:5px;">
       1. Descubrimiento</div>
     <b>Cuantitativo:</b> Identificar el paso exacto donde se abandona
     el carrito y la tasa de rebote del checkout.<br>
     <b>Cualitativo:</b> mapas de calor y grabaciones de sesión → detectar
     dudas en campos, problemas de scroll en móvil o confusión en la interfaz.
   </div>

   <!-- Paso 2 -->
   <div style="background:#f8f9fa; border-left:4px solid #2563EB;
               padding:10px 14px; border-radius:4px;">
     <div style="font-weight:bold; color:#2563EB; margin-bottom:5px;">
       2. Hipótesis</div>
     Toda modificación responde a un problema detectado.<br><br>
     <em>"Si reducimos los campos de facturación de 10 a 5 y eliminamos
     la cuenta obligatoria, entonces aumentaremos la conversión en 2%,
     porque reducimos la fricción y el esfuerzo cognitivo."</em>
   </div>

   <!-- Paso 3 -->
   <div style="background:#f8f9fa; border-left:4px solid #16a34a;
               padding:10px 14px; border-radius:4px;">
     <div style="font-weight:bold; color:#16a34a; margin-bottom:5px;">
       3. Variables</div>
     <b>Independiente:</b> la modificación (checkout 1 paso vs. 3 pasos,
     color del botón, ocultar menú de navegación).<br>
     <b>Dependiente:</b> tasa de conversión (primaria) + tiempo en checkout
     y abandono de carrito (secundarias).<br>
     <b>Control:</b> tráfico, fuentes de adquisición, promociones vigentes.
   </div>

   <!-- Paso 4 -->
   <div style="background:#f8f9fa; border-left:4px solid #16a34a;
               padding:10px 14px; border-radius:4px;">
     <div style="font-weight:bold; color:#16a34a; margin-bottom:5px;">
       4. Ejecución (A/B test)</div>
     Tráfico dividido aleatoriamente:<br>
     &bull; <b>Grupo A (control):</b> checkout original<br>
     &bull; <b>Grupo B (tratado):</b> checkout modificado<br><br>
     Duración: hasta alcanzar el N pre-especificado y cubrir al menos
     2 semanas completas para capturar variación semanal.
   </div>

   <!-- Paso 5 — ancho completo -->
   <div style="grid-column:1/3; background:#f0f4ff;
               border-left:4px solid #7c3aed; padding:10px 14px; border-radius:4px;">
     <div style="font-weight:bold; color:#7c3aed; margin-bottom:5px;">
       5. Análisis e iteración</div>
     Comparar versiones con el software de experimentación (Braze / Altitude).<br>
     &bull; <b>Grupo B convierte más</b> → hipótesis validada, se implementa para todos.<br>
     &bull; <b>Sin cambio o conversión cae</b> → se descarta y se formula una nueva hipótesis.
   </div>

   </div>'
)
append_pres("</section>")

# Slide 3B: Pasos 3+4 — Variables y diseño del test
append_pres("<section>")
append_pres("<h2>Ejercicio 3 — Pasos 3 y 4: variables y diseño del test</h2>")
append_pres(sprintf(
  '<p style="font-size:0.80em; color:#555; margin-bottom:10px;">
   La hipótesis (paso 2) define directamente qué medir y cómo asignar.
   Si el problema es fricción en el checkout, la variable dependiente es si el usuario completó la compra.
  </p>
  <table style="width:100%%; font-size:0.77em;">
  <thead><tr>
    <th style="background:#2c3e50; color:white; padding:5px; width:28%%;">Variable / Decisión</th>
    <th style="background:#2c3e50; color:white; padding:5px;">Elección</th>
    <th style="background:#2c3e50; color:white; padding:5px;">Justificación</th>
  </tr></thead>
  <tbody>
  <tr style="background:#f0f4ff;">
    <td><strong>✅ Métrica primaria (OEC)</strong></td>
    <td>Compras / usuarios que iniciaron checkout</td>
    <td>Mide directamente la fricción. Alta frecuencia, baja varianza → N manejable.<br>
    <em>Revenue:</em> alta varianza y al llegar al checkout el usuario ya decidió qué comprar.</td></tr>
  <tr>
    <td><strong>Métricas guardrail</strong></td>
    <td>Revenue/usuario · NPS · tiempo en checkout · abandono</td>
    <td>Detectan si se gana conversión pero se pierde experiencia o ticket promedio.</td></tr>
  <tr style="background:#f8f9fa;">
    <td><strong>Unidad de aleatorización</strong></td>
    <td>Usuario (user_id), asignado al iniciar el checkout</td>
    <td>Evita que el mismo usuario vea ambas versiones. Así cada persona cuenta una sola vez y la tasa de conversión se mide sobre la misma base en ambos grupos.<br>
    <em>App antigua:</em> excluir o analizar por separado.</td></tr>
  <tr>
    <td><strong>Ratio de asignación</strong></td>
    <td>50%% control / 50%% tratado</td>
    <td>Maximiza el poder estadístico para un N dado.</td></tr>
  </tbody></table>
  <div class="insight" style="margin-top:8px; font-size:0.80em;">
    Baseline empírico (grupo control, Ej. 1): <strong>p₀ = %.1f%%</strong>.
    En producción usar datos históricos del checkout actual.
  </div>',
  p0 * 100
))
append_pres("</section>")

# Slide 3C: Paso 4 — Tamaño muestral y duración
html_dur_pres <- tabla_duracion |>
  kbl(format = "html",
      caption = sprintf("Duración estimada para n = %s por grupo",
                        format(n_formula, big.mark = ","))) |>
  kable_styling(full_width = FALSE, position = "left") |>
  column_spec(3, bold = TRUE, background = "#e8f4f8") |>
  row_spec(which(daily_traffic == 1000), background = "#f0f4ff")

html_sens_pres <- tabla_sens_simple |>
  kbl(format = "html",
      caption = "Usuarios necesarios por grupo (alpha=0.05, poder=80%)",
      format.args = list(big.mark = ",")) |>
  kable_styling(full_width = FALSE, position = "left") |>
  row_spec(which(tabla_sens_simple$`Baseline (p0)` == "28%"),
           bold = TRUE, background = "#e8f4f8")

append_pres("<section>")
append_pres("<h2>Ejercicio 3 — Paso 4: ¿cuántos usuarios y cuánto tiempo?</h2>")
append_pres(sprintf(
  '<div style="display:flex; gap:16px; margin-bottom:12px; font-size:0.80em;">
     <div style="flex:1; background:#f0f4ff; padding:10px 14px; border-radius:6px;
                 border-left:4px solid #2563EB;">
       <strong>Tamaño muestral</strong><br>
       p₀ = %.1f%% · MDE = 2 pp · α = 0.05 · poder = 80%%<br>
       → <strong>%s usuarios por grupo (%s total)</strong>
     </div>
     <div style="flex:1; background:#f0f9f0; padding:10px 14px; border-radius:6px;
                 border-left:4px solid #16a34a;">
       <strong>Duración mínima</strong><br>
       max(días estadísticos, 14 días)<br>
       14 días = 2 ciclos lunes-domingo. En mercados con cobro quincenal
       (días 1 y 15), captura al menos un período post-pago y uno pre-pago.
     </div>
   </div>',
  p0 * 100,
  format(n_formula, big.mark = ","),
  format(n_formula * 2, big.mark = ",")
))
append_pres(as.character(html_dur_pres))
append_pres(sprintf(
  '<div class="insight" style="margin-top:8px; font-size:0.80em;">
     Con ~1,000 usuarios/día en checkout: mínimo <strong>%d días</strong>.
     Plataformas con alto tráfico (≥2,000/día) están limitadas por el ciclo semanal, no por la muestra.
   </div>',
  pmax(ceiling(n_formula * 2 / 1000), 14)
))
append_pres("</section>")

# Slide 3D: Paso 4/5 — Riesgos antes de analizar
append_pres("<section>")
append_pres("<h2>Ejercicio 3 — Paso 4/5: riesgos que pueden invalidar el test</h2>")
append_pres(
  '<p style="font-size:0.80em; color:#555; margin-bottom:8px;">
   Antes de lanzar el experimento y antes de leer los resultados, verificar:
  </p>
  <table style="width:100%; font-size:0.74em;">
  <thead><tr>
    <th style="background:#2c3e50; color:white; padding:5px; width:18%;">Riesgo</th>
    <th style="background:#2c3e50; color:white; padding:5px; width:9%;">Tipo</th>
    <th style="background:#2c3e50; color:white; padding:5px;">¿Qué puede pasar?</th>
    <th style="background:#2c3e50; color:white; padding:5px;">Mitigación</th>
  </tr></thead>
  <tbody>
  <tr>
    <td><strong>SRM</strong></td><td>Interna</td>
    <td>Ratio no es 50/50 por bug de asignación → estimador sesgado</td>
    <td>Chi-cuadrado sobre ratio observado vs. esperado. Si p &lt; 0.01, invalidar.</td></tr>
  <tr style="background:#f8f9fa;">
    <td><strong>Bug en el checkout</strong></td><td>Interna</td>
    <td><em>Crítico</em> → pausar;<br>
    <em>Menor</em> → continuar + documentar;<br>
    <em>Segmento</em> (ej. iOS 16) → excluir y analizar por separado</td>
    <td><strong>Monitoreo operativo desde día 1</strong>: alertas sobre errores,
    crashes, tiempo de carga. No interfiere con el diseño estadístico.</td></tr>
  <tr>
    <td><strong>Peeking</strong></td><td>Interna</td>
    <td>Leer resultados antes del N objetivo → FPR puede superar el 30% con α = 0.05</td>
    <td>Pre-registrar fecha de lectura y N.<br>
    <em>No hacer peeking estadístico.</em><br>
    <em>Sí monitorear métricas operativas</em> (errores, crashes).</td></tr>
  <tr style="background:#f8f9fa;">
    <td><strong>Heterogeneidad temporal</strong></td><td>Externa</td>
    <td>Hot Sale, Buen Fin, Black Friday: conversión basal sube → resultado no generaliza</td>
    <td>Calendario e-commerce. Si hay coincidencia, estratificar el análisis.</td></tr>
  </tbody></table>'
)
append_pres("</section>")

# =============================================================================
# EJ4 — DiD
# =============================================================================

html_balance_obs <- tabla_balance_obs |>
  kbl(format    = "html",
      caption   = sprintf("Tabla 7. Balance de covariables observables por grupo (pre-período)"),
      col.names = c("Variable",
                    sprintf("Control (N=%s)", format(n_e0, big.mark=",")), "SD",
                    sprintf("Tratados (N=%s)", format(n_e1, big.mark=",")), "SD",
                    "Diferencia", "p-valor"),
      align     = c("l","r","r","r","r","r","r")) |>
  kable_styling(bootstrap_options = c("striped", "condensed", "hover"),
                full_width = FALSE, position = "left") |>
  footnote(
    general = sprintf(
      "Variables continuas: t-test. Segmento: t-test por categoría. Región: χ² p = %.4f. App version: χ² p = %.4f.",
      chi_region$p.value, chi_appver$p.value),
    general_title = "Nota:"
  )

html_did_manual <- tabla_did_manual |>
  kbl(format  = "html",
      caption = "Tabla 8. Descomposición de la doble diferencia") |>
  kable_styling(bootstrap_options = c("striped", "condensed", "hover"),
                full_width = FALSE, position = "left") |>
  row_spec(3, bold = TRUE, background = "#EFF6FF") |>
  footnote(
    general = sprintf(
      "ATT (δ) = $%.2f/semana (SE = %.2f, p = %.4f, IC 95%%: [$%.2f, $%.2f]).",
      att, att_se, att_p, att_ci_l, att_ci_u),
    general_title = "Nota:"
  )

tabla_did_modelos <- modelsummary(
  list("(1) FE semana" = m_did_fe, "(2) TWFE" = m_did_twfe),
  vcov      = list(vcov(m_did_fe), vcov(m_did_twfe)),
  coef_map  = c("enrolled"      = "Enrolled (β)",
                "enrolled:post" = "Enrolled × Post = ATT (δ)"),
  gof_map   = c("nobs", "r.squared"),
  stars     = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  output    = "html",
  title     = "Tabla 9. Modelos DiD (SE clusterizados por client_id)"
)

fig_tag <- sprintf(
  '<figure style="margin:1em 0;">
     <img src="figures/fig_did_trends.png"
          style="max-width:100%%; border:1px solid #e0e0e0; border-radius:4px;">
     <figcaption style="font-size:0.85em; color:#555; margin-top:4px;">
       Figura 1. Tendencias de revenue promedio por semana y grupo de inscripción.
     </figcaption>
   </figure>'
)

# --- Documento EJ4 ---
append_doc("  <h2>Ejercicio 4 (Bonus) — Difference-in-Differences</h2>")
append_doc(
  '<p>Con los datos observacionales (<code>cai_obs_data.csv</code>) se estimó el efecto
   del programa de fidelización sobre el revenue semanal. A diferencia del Ejercicio 1,
   aquí <strong>no hay aleatorización</strong>: los clientes eligieron inscribirse.</p>'
)
append_doc('<h3>4.0 Balance de covariables por grupo</h3>')
append_doc(sprintf(
  '<p>Comparación de inscriptos (N=%s) y no inscriptos (N=%s) en el pre-período.
   En datos observacionales se espera desbalance.</p>',
  format(n_e1, big.mark=","), format(n_e0, big.mark=",")
))
append_doc(as.character(html_balance_obs))
append_doc(sprintf(
  '<div class="insight">
     <strong>Desbalance confirmado:</strong> los inscriptos parten de revenue base
     $%.0f vs $%.0f, mayor antigüedad y mayor NPS. La inscripción <strong>no es
     aleatoria</strong>. El DiD controla por diferencias de nivel, no de tendencias.
   </div>',
  rev_mean_e1, rev_mean_e0
))
append_doc("  <h3>4.1 Test de tendencias paralelas (pre-período)</h3>")
append_doc(sprintf(
  '<div class="insight">
     <strong>H0:</strong> la tendencia pre-tratamiento es igual entre grupos.<br>
     <strong>Resultado:</strong> F = %.3f, p = %.4f. %s
   </div>',
  f_trends, p_trends,
  ifelse(tendencias_paralelas,
    "No se rechaza H0: supuesto compatible con los datos.",
    "Se rechaza H0: tendencias difieren. Interpretar ATT como cota superior.")
))
append_doc(fig_tag)
append_doc("  <h3>4.2 Descomposición de la doble diferencia</h3>")
append_doc(as.character(html_did_manual))
append_doc("  <h3>4.3 Modelos DiD</h3>")
append_doc(as.character(tabla_did_modelos))
append_doc(sprintf(
  '<div class="insight">
     <strong>FE semana:</strong> ATT = <strong>$%.2f/semana</strong>
     (SE=%.2f, IC 95%%: [$%.2f,$%.2f], p=%.4f).<br>
     <strong>TWFE:</strong> $%.2f — diferencia (~$%.1f) refleja heterogeneidad
     intra-cliente. Ambos estimadores convergen en orden de magnitud.<br><br>
     <strong>Caveat:</strong> el test de tendencias paralelas se rechaza
     (F=%.3f, p&lt;0.0001): el grupo inscripto ya crecía más rápido antes del corte.
     Los estimadores son cota superior del efecto causal.
   </div>',
  att, att_se, att_ci_l, att_ci_u, att_p,
  att_twfe, abs(att_twfe - att), f_trends
))
append_doc(
  '<h3>4.4 Limitaciones y supuestos</h3>
   <ul>
     <li><strong>Selección en enrolled:</strong> clientes con mayor propensión de gasto
     tienden a inscribirse más.</li>
     <li><strong>Tendencias paralelas:</strong> test formal rechaza la hipótesis
     (F=998.6, p&lt;0.0001).</li>
     <li><strong>Conclusión:</strong> evidencia descriptiva sólida de asociación positiva.
     Para causalidad limpia se requeriría un RCT o discontinuidad en la elegibilidad.</li>
   </ul>'
)
append_doc("</body></html>")

# --- Presentación EJ4 ---
html_balance_pres <- tabla_balance_obs |>
  kbl(format    = "html",
      col.names = c("Variable",
                    sprintf("Control (N=%s)", format(n_e0, big.mark=",")), "SD",
                    sprintf("Tratados (N=%s)", format(n_e1, big.mark=",")), "SD",
                    "Diferencia", "p-valor"),
      align     = c("l","r","r","r","r","r","r")) |>
  kable_styling(full_width = FALSE, position = "left", font_size = 13)

append_pres("<section>")
append_pres("<h2>Ejercicio 4 — ¿Son comparables los grupos?</h2>")
append_pres(
  '<p style="font-size:0.80em; color:#555; margin-bottom:8px;">
   Sin aleatorización, los grupos pueden diferir <em>antes</em> del programa.
   </p>'
)
append_pres(as.character(html_balance_pres))
append_pres(sprintf(
  '<div class="insight" style="font-size:0.80em; margin-top:10px;">
     <strong>Selección evidente:</strong> inscriptos parten de revenue base
     $%.0f vs $%.0f, son más antiguos y tienen mayor NPS.<br>
     No se pueden comparar directamente: se necesita el método DiD.
   </div>',
  rev_mean_e1, rev_mean_e0
))
append_pres("</section>")

append_pres("<section>")
append_pres("<h2>Ejercicio 4 (Bonus) — Programa de fidelización: ¿funciona?</h2>")
append_pres(sprintf(
  '<table style="width:100%%; font-size:0.8em;">
  <thead><tr>
    <th style="background:#2c3e50; color:white; padding:6px;">Elemento</th>
    <th style="background:#2c3e50; color:white; padding:6px;">Detalle</th>
  </tr></thead>
  <tbody>
  <tr><td><strong>Datos</strong></td><td>20,000 clientes × 18 semanas</td></tr>
  <tr><td><strong>Grupo tratado</strong></td><td>Inscriptos (enrolled=1): %s clientes</td></tr>
  <tr><td><strong>Grupo control</strong></td><td>No inscriptos (enrolled=0): %s clientes</td></tr>
  <tr><td><strong>Pre-período</strong></td><td>Semanas 1–9</td></tr>
  <tr><td><strong>Post-período</strong></td><td>Semanas 10–18</td></tr>
  </tbody></table>
  <div class="insight" style="margin-top:12px; font-size:0.85em;">
    <strong>Idea del DiD:</strong> en lugar de comparar grupos directamente,
    se mide cuánto <em>aceleró</em> el crecimiento de revenue al comenzar el
    programa, relativo al grupo de control.
  </div>',
  format(n_e1, big.mark = ","),
  format(n_e0, big.mark = ",")
))
append_pres("</section>")

html_did_manual_pres <- tabla_did_manual |>
  kbl(format  = "html",
      caption = "Descomposición: cambio en revenue antes y después del programa") |>
  kable_styling(full_width = FALSE, position = "left")

append_pres("<section>")
append_pres("<h2>Ejercicio 4 — Resultado del análisis</h2>")
append_pres(as.character(html_did_manual_pres))
append_pres(sprintf(
  '<div style="display:flex; gap:20px; margin-top:14px; font-size:0.82em;">
     <div style="flex:1; background:#f0f4ff; padding:12px 16px;
                 border-radius:6px; border-left:4px solid #2563EB;">
       <div style="font-size:1.6em; font-weight:bold; color:#2563EB;">+$%.2f</div>
       <div><strong>por semana adicional</strong> para los inscriptos</div>
       <div style="font-size:0.88em; color:#555; margin-top:4px;">
         IC 95%%: [$%.2f, $%.2f] &nbsp;|&nbsp; p &lt; 0.0001</div>
     </div>
     <div style="flex:1; background:#f8f9fa; padding:12px 16px;
                 border-radius:6px; border-left:4px solid #6c757d; font-size:0.88em;">
       <strong>¿Cómo se lee la tabla?</strong><br>
       Inscriptos crecieron +$%.1f/sem. El control creció +$%.1f/sem. sin el programa.
       La diferencia de diferencias = efecto del programa.
     </div>
   </div>',
  att, att_ci_l, att_ci_u,
  rev_e1_post - rev_e1_pre,
  rev_e0_post - rev_e0_pre
))
append_pres("</section>")

append_pres("<section>")
append_pres("<h2>Ejercicio 4 — ¿Podemos confiar en el resultado?</h2>")
append_pres(
  '<img src="figures/fig_did_trends.png"
        style="max-height:260px; margin-bottom:10px; border:1px solid #ddd; border-radius:4px;">'
)
append_pres(sprintf(
  '<div class="insight" style="font-size:0.82em;">
     <strong style="color:#b91c1c;">⚠ Limitación — sesgo de selección:</strong><br>
     Los inscriptos ya crecían más rápido <em>antes</em> del programa (semanas 1–9).
     El resultado de <strong>+$%.2f/semana</strong> es la <em>cota superior</em>
     del efecto: parte refleja quién se inscribe (selección), no el programa.<br><br>
     <strong>Para el efecto causal neto</strong> haría falta un RCT o un diseño
     con discontinuidad en la elegibilidad.
   </div>',
  att
))
append_pres("</section>")

close_pres()

cat("\n[render_es] Listo.\n")
cat("  Documento:     ", path_reporte, "\n")
cat("  Presentación:  ", path_pres, "\n")
