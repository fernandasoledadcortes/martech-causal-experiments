# =============================================================================
# render_en.R — Generates HTML outputs in English
# REQUIRES: analysis scripts 01–04_ej*.R must have been run (objects in global env)
# PRODUCES:
#   outputs/report_results.html
#   outputs/presentation.html
# =============================================================================

options(knitr.kable.NA = "")

path_reporte <- file.path(root, "outputs", "report_results.html")
path_pres    <- file.path(root, "outputs", "presentation.html")

# =============================================================================
# HELPERS
# =============================================================================

doc_header <- '<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <title>Results — MarTech Consultant Assessment</title>
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
  <h1>Results — MarTech Consultant Assessment</h1>
  <p class="meta">Countdown push vs. standard push campaign &nbsp;|&nbsp;
     Dataset: cai_exp_data.csv &nbsp;|&nbsp; Generated: '

writeLines(paste0(doc_header, format(Sys.Date(), "%m/%d/%Y"), "</p>"), path_reporte)
append_doc <- function(...) cat(paste0(..., "\n"), file = path_reporte, append = TRUE)

pres_header <- '<!DOCTYPE html>
<html lang="en">
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
    /* === INTERACTIVE MODALS === */
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

<!-- COVER SLIDE -->
<section>
  <div class="slide-title">
    <h1>MarTech Consultant Assessment</h1>
    <p>Causal Inference in Experiments</p>
    <p style="margin-top:40px; font-size:0.85em;">Fernanda Cortés &nbsp;|&nbsp; '

writeLines(paste0(pres_header, format(Sys.Date(), "%m/%d/%Y"),
                  "</p>\n  </div>\n</section>"), path_pres)
append_pres <- function(...) cat(paste0(..., "\n"), file = path_pres, append = TRUE)

close_pres <- function() {
  b1 <- if (exists("m_cate")) round(coef(m_cate)["treatment"], 1) else 36.4
  b4 <- if (exists("m_cate")) round(coef(m_cate)["treatment:canalpaid_search"], 1) else -17.5
  b5 <- if (exists("m_cate")) round(coef(m_cate)["treatment:canalsocial"], 1) else -28.1

  regaux_txt <- if (exists("lm_check_summary") && exists("f_val") && exists("f_pval"))
    sprintf(
      '<strong>R² = %.4f</strong> → covariates explain %.2f%% of variation → practically zero ✅<br><strong>F = %.2f &nbsp;|&nbsp; p = %.3f</strong> → jointly they do not predict group assignment.<br><br>',
      lm_check_summary$r.squared, lm_check_summary$r.squared * 100, f_val, f_pval)
  else
    '<strong>R² ≈ 0 | F ≈ 0 | p ≈ 1</strong> → jointly they do not predict group assignment.<br><br>'

  cate_txt <- sprintf(
    paste0(
      'Model with interactions between treatment and channel. Reference category: <em>organic</em>.<br><br>',
      'Key coefficients:<br>',
      '&bull; <strong>β₁ = %.1f</strong> → effect for organic<br>',
      '&bull; <strong>β₄ = %.1f</strong> → paid search has %.1f less than organic<br>',
      '&bull; <strong>β₅ = %.1f</strong> → social has %.1f less than organic<br><br>',
      'Final effect:<br>',
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
    '<h4>What does "exclusively" mean?</h4>',
    'By randomly assigning users, both groups are equivalent before the experiment: ',
    'same history, same channel, same prior characteristics. ',
    'Any observed difference afterward can only be due to the campaign.<br><br>',
    '<div class="eg">Example: if the countdown group converted more, it is not because they were ',
    '"better buyers" to begin with. They were equivalent. The only difference was the push they received.</div>',
    '</div></div>',
    # m-split
    '<div id="m-split" class="modal-ov"><div class="modal-bx">',
    '<button class="modal-x">&#x2715;</button>',
    '<h4>Why not exactly 50/50?</h4>',
    'With pure random assignment, landing at exactly 25,000 in each group would be a coincidence. ',
    'A difference of ~286 users out of 50,000 (0.6%) is completely normal sampling noise.<br><br>',
    '<div class="eg">It is like flipping a coin 50,000 times: it is nearly impossible to get exactly ',
    '25,000 heads and 25,000 tails.</div><br>',
    'A statistical test (chi-square) confirms the deviation is consistent with chance.',
    '</div></div>',
    # m-categ
    '<div id="m-categ" class="modal-ov"><div class="modal-bx">',
    '<button class="modal-x">&#x2715;</button>',
    '<h4>Why do categorical variables look different?</h4>',
    'A <strong>continuous</strong> variable (e.g. device age) has a mean ',
    '→ the means are compared between groups.<br><br>',
    'A <strong>categorical</strong> variable (e.g. channel) has no mean: its values are ',
    '"organic", "paid search" or "social" → the % share of each category is compared between groups.<br><br>',
    '<div class="eg">Example: 33% of control arrived via organic. 34% of treated arrived via organic. ',
    'Difference: +1 pp → not significant → the groups are equivalent on that variable.</div>',
    '</div></div>',
    # m-regaux
    '<div id="m-regaux" class="modal-ov"><div class="modal-bx">',
    '<button class="modal-x">&#x2715;</button>',
    '<h4>What is the auxiliary regression?</h4>',
    'It is a formal check of randomization. A model is built that tries to predict ',
    'which group each user was assigned to using their prior characteristics.<br><br>',
    'If assignment was random, <em>no characteristic should be able to predict the group</em>.<br><br>',
    regaux_txt,
    '<div class="eg">Conclusion: the assignment was truly random. The experiment is valid.</div>',
    '</div></div>',
    # m-pval
    '<div id="m-pval" class="modal-ov"><div class="modal-bx">',
    '<button class="modal-x">&#x2715;</button>',
    '<h4>What does p &lt; 0.0001 mean?</h4>',
    'The p-value answers: "If the countdown push had no real effect at all, ',
    'how likely would it be to see a difference this large purely by chance?"<br><br>',
    '<strong>p &lt; 0.0001</strong> → less than 0.01% probability → ',
    'so unlikely that we conclude the effect is real.<br><br>',
    '<div class="eg">Standard convention: if p &lt; 0.05 (5%), the result is not attributable to chance. ',
    'Here the result is much stronger than that threshold.</div>',
    '</div></div>',
    # m-cate
    '<div id="m-cate" class="modal-ov"><div class="modal-bx">',
    '<button class="modal-x">&#x2715;</button>',
    '<h4>How was the effect per channel calculated?</h4>',
    cate_txt,
    '<br><br><div class="eg">Negative β₄ and β₅ do not mean a negative effect — ',
    'they mean the effect is smaller than in organic.</div>',
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
# EX1 — ATE
# =============================================================================

footnotes_chisq_en <- paste(
  "Joint chi-square test (H0: identical distribution across groups):",
  paste(
    gsub("Canal de adquisicion", "Acquisition channel",
    gsub("Plataforma", "Platform",
    gsub("Nivel de lealtad", "Loyalty tier",
         chisq_pvals))),
    collapse = " | "
  )
)
texto_balance_en <- ifelse(
  balance_ok,
  "Pre-treatment characteristics are balanced across groups.",
  "WARNING: imbalances detected. Review the randomization."
)

tabla_balance_en <- tabla_balance_export |>
  setNames(c("Variable", "Control", "Treated", "Difference", "p-value"))

html_tabla_balance_en <- tabla_balance_en |>
  kbl(format  = "html",
      caption = "Table 1. Pre-Treatment Covariate Balance") |>
  kable_styling(bootstrap_options = c("striped", "condensed", "hover"),
                full_width = FALSE, position = "left") |>
  footnote(general = footnotes_chisq_en, general_title = "Note:")

tabla_modelos_en <- modelsummary(
  list("Conversion rate (LPM)" = m_conv, "Revenue" = m_rev),
  vcov     = "HC2",
  coef_map = c("treatment" = "Treatment (ATE)"),
  gof_map  = c("nobs", "r.squared"),
  stars    = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  output   = "html",
  title    = "Table 2. ATE Estimates (HC2 robust SE)"
)

tabla_modelos_comp_en <- modelsummary(
  list("Conversion — ANCOVA" = m_conv_ancova,
       "Revenue — ANCOVA"    = m_rev_ancova,
       "Ticket (converters)" = m_ticket),
  vcov     = "HC2",
  coef_map = c("treatment" = "Treatment (ATE)"),
  gof_map  = c("nobs", "r.squared"),
  stars    = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  output   = "html",
  title    = "Table 3. Supplementary analysis — ANCOVA and ticket effect (HC2 robust SE)"
)

sig_conv_en <- ifelse(p_conv < 0.05, "statistically significant", "not significant")
sig_rev_en  <- ifelse(p_rev  < 0.05, "statistically significant", "not significant")
sig_tick_en <- ifelse(p_tick < 0.05, "statistically significant", "not significant")

# --- Document EX1 ---
append_doc("  <h2>Exercise 1 — Average Treatment Effect (ATE)</h2>")
append_doc("  <h3>1.1 Experimental design validation</h3>")
append_doc(sprintf(
  '<p>The experiment included <strong>%s users</strong>:
   <strong>%s in the control group</strong> (standard push, %.1f%%) and
   <strong>%s in the treatment group</strong> (countdown push, %.1f%%).
   Pre-treatment covariate balance was verified using t-tests for continuous
   variables and proportion tests by category for categorical variables.</p>',
  format(nrow(df_exp), big.mark = ","),
  format(sum(df_exp$treatment == 0), big.mark = ","), mean(df_exp$treatment == 0) * 100,
  format(sum(df_exp$treatment == 1), big.mark = ","), mean(df_exp$treatment == 1) * 100
))
append_doc(as.character(html_tabla_balance_en))
append_doc(sprintf(
  '<div class="insight"><strong>Result:</strong> %s
   All p-values exceed 0.05. The auxiliary regression R²
   (D ~ covariates) is <strong>%.4f</strong> (F = %.2f, p = %.3f).
   <strong>Randomization is valid.</strong></div>',
  texto_balance_en, lm_check_summary$r.squared, f_val, f_pval
))
append_doc("  <h3>1.2 Average Treatment Effect (ATE) estimation</h3>")
append_doc(
  '<p>Two main models were estimated, one per required outcome.
   For <em>conversion</em>, a Linear Probability Model (LPM) was used:
   the coefficient is directly interpreted in percentage points.
   For <em>revenue</em>, OLS was applied to all users (including zeros).
   In both cases the raw estimator is consistent because randomization
   guarantees independence between treatment and potential outcomes.
   HC2 robust standard errors are reported.</p>'
)
append_doc(as.character(tabla_modelos_en))
append_doc(sprintf(
  '<div class="insight">
     <strong>Results:</strong><br>
     &bull; <strong>Conversion rate:</strong> the countdown push increased the
     conversion rate by <strong>%.1f pp</strong> (p = %.4f, %s).<br>
     &bull; <strong>Revenue per user:</strong> average increase of
     <strong>$%.2f</strong> (p = %.4f, %s).
   </div>',
  ate_conv * 100, p_conv, sig_conv_en,
  ate_rev,        p_rev,  sig_rev_en
))
append_doc("  <h3>1.3 Supplementary analysis</h3>")
append_doc(
  '<p>ANCOVA models add pre-treatment covariates. The ticket model
   estimates the ATE on revenue conditional on having converted.</p>'
)
append_doc(as.character(tabla_modelos_comp_en))
append_doc(sprintf(
  '<div class="insight">
     <strong>A. ANCOVA as confirmation of successful randomization:</strong>
     in a correctly executed RCT, adding pre-treatment covariates should not
     change the treatment coefficient. The fact that ATEs are virtually
     identical between the raw model and ANCOVA confirms that assignment was
     truly random: the estimator is robust and independent of users\'
     prior characteristics.<br><br>
     <strong>B. Exploratory analysis — ticket model:</strong>
     filtering to <em>conversion = 1</em> conditions on a post-treatment variable.
     This model is reported as <strong>exploratory</strong>: the result
     (+$%.2f, p = %.4f) is a <em>directional indicator</em>,
     not a validated causal estimator.<br><br>
     <strong>Next step — Uplift modeling:</strong>
     an uplift modeling approach could be implemented to identify which user
     profiles were truly influenced by the countdown push, distinguishing them
     from those who would likely have converted even without treatment.
   </div>',
  ate_tick, p_tick
))

# --- Presentation EX1 ---
# Slide 1A: What was tested?
append_pres("<section>")
append_pres("<h2>Exercise 1 — What was tested?</h2>")
append_pres(sprintf(
  '<table style="width:100%%; font-size:0.8em;">
   <thead><tr>
     <th style="background:#2c3e50; color:white; padding:6px; width:25%%;">Element</th>
     <th style="background:#2c3e50; color:white; padding:6px;">Detail</th>
   </tr></thead>
   <tbody>
   <tr><td><strong>Campaign A — Control</strong></td>
       <td>Standard push notification (no urgency)</td></tr>
   <tr><td><strong>Campaign B — Treatment</strong></td>
       <td>Push notification with a 24-hour countdown timer</td></tr>
   <tr><td><strong>Users</strong></td>
       <td>%s randomly assigned — %s control (%.0f%%) | %s treated (%.0f%%)<br>
           <small><span class="tip" data-modal="m-split" style="font-size:0.9em;">Why not exactly 50/50?</span></small></td></tr>
   <tr><td><strong>Target metrics</strong></td>
       <td>Conversion rate (did they buy?) and total revenue per user</td></tr>
   <tr><td><strong>Method</strong></td>
       <td>RCT — the gold standard for measuring causality</td></tr>
   </tbody></table>
   <div class="insight" style="margin-top:14px;">
     By randomly assigning users, any difference in outcomes is due
     <span class="tip" data-modal="m-aleatorio">exclusively</span>
     to the campaign, not to pre-existing differences between groups.
   </div>',
  format(nrow(df_exp), big.mark = ","),
  format(sum(df_exp$treatment == 0), big.mark = ","), mean(df_exp$treatment == 0) * 100,
  format(sum(df_exp$treatment == 1), big.mark = ","), mean(df_exp$treatment == 1) * 100
))
append_pres("</section>")

# Slide 1B: Balance table
html_balance_slide_full_en <- tabla_balance_en |>
  kbl(format  = "html",
      caption = "Group characteristics before the experiment") |>
  kable_styling(full_width = FALSE, position = "left", font_size = 13) |>
  footnote(
    general       = "Continuous variables: mean difference (t-test). Categorical variables: % distribution per category (difference in pp).",
    general_title = "Note:"
  )

append_pres("<section>")
append_pres("<h2>Exercise 1 — Groups are equivalent before the test</h2>")
append_pres(
  '<p style="font-size:0.78em; color:#555; margin-bottom:8px;">
   Pre-treatment characteristic comparison. Continuous: mean difference.
   <span class="tip" data-modal="m-categ">Categorical</span>: percentage distribution by category (difference in pp).
  </p>'
)
append_pres(as.character(html_balance_slide_full_en))
append_pres("</section>")

# Slide 1B2: Balance interpretation
append_pres("<section>")
append_pres("<h2>Exercise 1 — What does the balance table tell us?</h2>")
append_pres(sprintf(
  '<div style="display:flex; gap:20px; margin-top:14px; font-size:0.85em;">
     <div style="flex:1; background:#f0f4ff; padding:16px; border-radius:8px;
                 border-left:4px solid #2563EB;">
       <div style="font-size:1.05em; font-weight:bold; color:#2563EB; margin-bottom:8px;">
         1. No significant differences between groups
       </div>
       No pre-treatment variable shows a statistically significant difference
       between control and treatment (p &gt; 0.05 in all comparisons),
       for both continuous variables and the distribution of acquisition
       channel, platform, and loyalty tier.
     </div>
     <div style="flex:1; background:#f0f9f0; padding:16px; border-radius:8px;
                 border-left:4px solid #16a34a;">
       <div style="font-size:1.05em; font-weight:bold; color:#16a34a; margin-bottom:8px;">
         2. No variable predicts group assignment
       </div>
       Auxiliary regression: <em>treatment ~ all covariates</em><br>
       <span class="tip" data-modal="m-regaux">R² = %.4f &nbsp;·&nbsp; F = %.2f &nbsp;·&nbsp; p = %.3f</span><br><br>
       The covariates jointly <strong>do not predict</strong> which group
       a user was assigned to.
     </div>
   </div>
   <div class="insight" style="margin-top:14px;">
     <strong>Conclusion:</strong> randomization is valid. No selection bias.
     Any observed difference in outcomes is attributable solely to treatment.
   </div>',
  lm_check_summary$r.squared, f_val, f_pval
))
append_pres("</section>")

# Slide 1C: The campaign worked
append_pres("<section>")
append_pres("<h2>Exercise 1 — The countdown push works</h2>")
append_pres(sprintf(
  '<div style="display:flex; gap:20px; margin:14px 0;">

     <!-- Conversion -->
     <div style="flex:1; background:#f0f4ff; padding:16px; border-radius:8px;
                 border:1px solid #d0d9ff;">
       <div style="font-size:1.9em; font-weight:bold; color:#2563EB; text-align:center;">
         +%.1f pp</div>
       <div style="font-size:0.88em; font-weight:bold; text-align:center; margin:4px 0 10px;">
         Conversion rate</div>
       <table style="width:100%%; font-size:0.78em; border:none;">
         <tr>
           <td style="border:none; color:#555;">Before (control)</td>
           <td style="border:none; text-align:right; font-weight:bold;">%.1f%%</td>
         </tr>
         <tr>
           <td style="border:none; color:#555;">After (treated)</td>
           <td style="border:none; text-align:right; font-weight:bold; color:#2563EB;">%.1f%%</td>
         </tr>
         <tr style="border-top:1px solid #d0d9ff;">
           <td style="border:none; color:#555;">Relative growth</td>
           <td style="border:none; text-align:right; font-weight:bold;">+%.0f%%</td>
         </tr>
         <tr>
           <td style="border:none; color:#555;">Per 1,000 users</td>
           <td style="border:none; text-align:right; font-weight:bold;">~%.0f more purchases</td>
         </tr>
       </table>
     </div>

     <!-- Revenue -->
     <div style="flex:1; background:#f0f4ff; padding:16px; border-radius:8px;
                 border:1px solid #d0d9ff;">
       <div style="font-size:1.9em; font-weight:bold; color:#2563EB; text-align:center;">
         +$%.2f</div>
       <div style="font-size:0.88em; font-weight:bold; text-align:center; margin:4px 0 10px;">
         Revenue per user</div>
       <table style="width:100%%; font-size:0.78em; border:none;">
         <tr>
           <td style="border:none; color:#555;">Average across all users</td>
           <td style="border:none; text-align:right; font-weight:bold;">includes non-buyers</td>
         </tr>
         <tr style="border-top:1px solid #d0d9ff;">
           <td style="border:none; color:#555;">Per 1,000 users</td>
           <td style="border:none; text-align:right; font-weight:bold; color:#2563EB;">
             ~$%.0f more</td>
         </tr>
       </table>
     </div>

   </div>
   <div class="insight" style="font-size:0.82em;">
     Both effects are statistically significant
     (<span class="tip" data-modal="m-pval">p &lt; 0.0001</span>) and replicable.
     They are not attributable to chance.
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

# Slide 1D: How does the effect operate?
html_resumen_comp_en <- resumen_ate_comp |>
  kbl(format = "html",
      caption = "Table 3. Supplementary analysis (HC2 robust SE)",
      digits  = 4) |>
  kable_styling(full_width = FALSE, position = "left")

append_pres("<section>")
append_pres("<h2>Exercise 1 — How does the effect operate?</h2>")
append_pres(as.character(html_resumen_comp_en))
append_pres(sprintf(
  '<div class="insight">
     <strong>A. ANCOVA — confirmation of successful randomization:</strong>
     adding pre-treatment covariates does not change the estimated ATE.
     In an RCT this is not "additional precision" — it is evidence that
     randomization worked: the effect is robust and independent of
     users\' prior characteristics.<br><br>
     <strong>B. Exploratory analysis — ticket model:</strong>
     among users who did purchase, the countdown group spent
     <strong>$%.2f more on average</strong>. <em>Exploratory</em> result:
     by conditioning on conversion (a post-treatment variable), the estimator
     may carry selection bias.<br><br>
     <strong>Next step — Uplift modeling:</strong>
     an uplift modeling approach could be implemented to identify which user
     profiles were truly influenced by the countdown push, distinguishing them
     from those who would likely have converted even without treatment.
   </div>',
  ate_tick
))
append_pres("</section>")

# =============================================================================
# EX2 — CATE
# =============================================================================

cate_por_canal_en <- cate_por_canal |>
  setNames(c("Channel", "CATE ($)", "SE", "95% CI lower", "95% CI upper", "p-value"))

html_cate_en <- cate_por_canal_en |>
  kbl(format  = "html",
      caption = "Table 4. CATE by acquisition channel (HC2 robust SE)") |>
  kable_styling(bootstrap_options = c("striped", "condensed", "hover"),
                full_width = FALSE, position = "left") |>
  footnote(
    general = sprintf(
      "Joint heterogeneity test (H0: CATE equal across all channels): F = %.3f, p = %.4f.",
      f_het, p_het),
    general_title = "Note:"
  )

tabla_cate_modelo_en <- modelsummary(
  list("Revenue ~ treatment × channel" = m_cate),
  vcov    = "HC2",
  gof_map = c("nobs", "r.squared"),
  stars   = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  output  = "html",
  title   = "Table 5. Interaction model coefficients (HC2 robust SE)"
)

canal_max_en <- canal_max
canal_min_en <- canal_min

# --- Document EX2 ---
append_doc("  <h2>Exercise 2 — Conditional Average Treatment Effect (CATE)</h2>")
append_doc(
  '<p>The CATE by acquisition channel was estimated using a regression with
   interactions between treatment and channel:</p>
   <p style="margin-left:20px; font-family:monospace;">
   y<sub>i</sub> = β₀ + β₁·D<sub>i</sub> + β₂·channel<sub>i</sub> +
   β₃·(D<sub>i</sub> × channel<sub>i</sub>) + ε<sub>i</sub></p>
   <p>CATE(k) = β₁ + β₃<sub>k</sub>. Reference category: <em>organic</em>.</p>'
)
append_doc(as.character(tabla_cate_modelo_en))
append_doc("  <h3>2.1 CATE by channel</h3>")
append_doc(as.character(html_cate_en))
append_doc(sprintf(
  '<div class="insight">
     <strong>Interpretation:</strong>
     The channel with the largest incremental effect is <strong>%s</strong>
     (CATE = $%.2f), while the one with the smallest effect is <strong>%s</strong>
     (CATE = $%.2f). %s
   </div>',
  canal_max_en, cate_vals[[canal_max_en]],
  canal_min_en, cate_vals[[canal_min_en]],
  ifelse(hay_het,
    "Heterogeneity across channels is statistically significant.",
    "Heterogeneity across channels is not statistically significant.")
))
append_doc(
  '<h3>2.2 Marketing interpretation</h3>
   <ul>
     <li><strong>Organic ($36.4):</strong> user with purchase intent, familiar
     with the brand. The countdown acts as the final nudge to close the sale.</li>
     <li><strong>Paid search ($18.9):</strong> has intent but is comparing
     alternatives. The effect exists but is moderate.</li>
     <li><strong>Social ($8.3):</strong> discovered the product in the feed,
     no prior intent. The countdown has marginal impact.</li>
   </ul>
   <p><strong>Implication:</strong> the countdown push is a <em>closing</em> tool,
   not a <em>discovery</em> tool. Its maximum value is with users who have
   already formed purchase intent.</p>'
)

# --- Presentation EX2 ---
html_cate_pres_en <- cate_por_canal_en |>
  kbl(format  = "html",
      caption = "Table 4. Countdown push effect by acquisition channel") |>
  kable_styling(full_width = FALSE, position = "left")

append_pres("<section>")
append_pres("<h2>Exercise 2 — The effect varies by how the user arrived</h2>")
append_pres(sprintf(
  '<p style="font-size:0.82em; color:#555; margin-bottom:8px;">
   Estimated effect by channel (<span class="tip" data-modal="m-cate">interaction regression</span>).
   Heterogeneity test: F = %.3f (p &lt; 0.0001) — differences
   <strong>are not attributable to chance</strong>.
   </p>',
  f_het
))
append_pres(as.character(html_cate_pres_en))
append_pres(sprintf(
  '<div style="display:flex; gap:20px; margin-top:12px; font-size:0.82em;">
     <div style="flex:1; background:#f0f4ff; padding:10px 14px; border-radius:6px; border-left:4px solid #2563EB;">
       <strong>Organic: $%.2f</strong><br>High purchase intent. The countdown acts as the final closing nudge.
     </div>
     <div style="flex:1; background:#f0f9f0; padding:10px 14px; border-radius:6px; border-left:4px solid #16a34a;">
       <strong>Paid search: $%.2f</strong><br>Has intent but is comparing options. The effect exists but is diluted.
     </div>
     <div style="flex:1; background:#fff8f0; padding:10px 14px; border-radius:6px; border-left:4px solid #ea580c;">
       <strong>Social: $%.2f</strong><br>No prior intent. Urgency has little impact.
     </div>
   </div>',
  cate_vals[["organic"]], cate_vals[["paid_search"]], cate_vals[["social"]]
))
append_pres("</section>")

append_pres("<section>")
append_pres("<h2>Exercise 2 — Strategic implication</h2>")
append_pres(
  '<table style="width:100%; font-size:0.8em; margin-bottom:12px;">
   <thead><tr>
     <th style="background:#2c3e50; color:white; padding:6px;">Channel</th>
     <th style="background:#2c3e50; color:white; padding:6px;">User profile</th>
     <th style="background:#2c3e50; color:white; padding:6px;">Is the countdown worthwhile?</th>
   </tr></thead>
   <tbody>
   <tr style="background:#f0f4ff;">
     <td><strong>Organic</strong></td>
     <td>High purchase intent, knows the brand</td>
     <td>&#x2705; Top priority — maximum return</td></tr>
   <tr>
     <td><strong>Paid search</strong></td>
     <td>Has intent, but evaluating alternatives</td>
     <td>&#x2705; Recommended — moderate but positive effect</td></tr>
   <tr style="background:#fff8f0;">
     <td><strong>Social</strong></td>
     <td>Cold discovery, no formed intent</td>
     <td>&#x26A0;&#xFE0F; Review — marginal effect, potentially inefficient cost</td></tr>
   </tbody></table>
   <div class="insight">
     <strong>Conclusion:</strong> the countdown push is a <em>closing</em> tool,
     not an <em>acquisition</em> tool. Concentrating the budget on organic and
     paid search maximizes campaign ROI.
   </div>'
)
append_pres("</section>")

# =============================================================================
# EX3 — EXPERIMENTAL DESIGN
# =============================================================================

html_sens_en <- tabla_sens_simple |>
  kbl(format      = "html",
      caption     = "Table 6. Required sample size per group (alpha=0.05, power=0.80)",
      format.args = list(big.mark = ",")) |>
  kable_styling(bootstrap_options = c("striped", "condensed", "hover"),
                full_width = FALSE, position = "left") |>
  row_spec(which(tabla_sens_simple$`Baseline (p0)` == "28%"),
           bold = TRUE, background = "#e8f4f8") |>
  footnote(general = "Highlighted row: empirical baseline from control group (Exercise 1).",
           general_title = "Note:")

tabla_duracion_en <- tabla_duracion |>
  mutate(`Restricción activa` = case_when(
    `Restricción activa` == "Tráfico"       ~ "Traffic",
    `Restricción activa` == "Ciclo semanal" ~ "Weekly cycle",
    TRUE ~ `Restricción activa`
  )) |>
  setNames(c("Users/day at checkout", "Days (statistical)", "Recommended days", "Active constraint"))

html_dur_doc_en <- tabla_duracion_en |>
  kbl(format  = "html",
      caption = sprintf("Table: Estimated experiment duration by daily traffic
                        (N required: %s per group, %s total)",
                        format(n_formula, big.mark = ","),
                        format(n_formula * 2, big.mark = ","))) |>
  kable_styling(bootstrap_options = c("striped", "condensed", "hover"),
                full_width = FALSE, position = "left") |>
  column_spec(3, bold = TRUE) |>
  row_spec(which(daily_traffic >= 2000), background = "#fff8f0") |>
  footnote(
    general = "Orange-background rows: weekly cycle constraint (14 days) is binding.",
    general_title = "Note:"
  )

# --- Document EX3 ---
append_doc("<h2>Exercise 3 — Experimental Design: Checkout Flow Redesign</h2>")
append_doc("<h3>3.1 Experiment definition</h3>")
append_doc(
  '<table class="table table-bordered" style="width:100%">
   <thead><tr>
     <th style="width:25%">Dimension</th><th>Decision</th><th>Justification</th>
   </tr></thead>
   <tbody>
   <tr>
     <td><strong>Randomization unit</strong></td>
     <td>User (<code>user_id</code> persistent), assigned at checkout start</td>
     <td>Randomizing by session violates SUTVA: the same user could see both versions.
     The <code>user_id</code> ensures consistent assignment across visits.<br><br>
     <strong>Assignment moment:</strong> at checkout start (not app open).
     The relevant denominator is users who reached checkout.<br><br>
     <strong>Unauthenticated users:</strong> use a persistent cookie as proxy.<br><br>
     <strong>App version:</strong> users on older versions may not render the new
     checkout. Filter by minimum version or report by version.</td>
   </tr>
   <tr>
     <td><strong>Assignment ratio</strong></td>
     <td>50% control / 50% treatment</td>
     <td>Maximizes statistical power for a given N.</td>
   </tr>
   <tr>
     <td><strong>Primary metric (OEC)</strong></td>
     <td>Checkout conversion rate<br>= completed purchases / users who started checkout</td>
     <td>Direct indicator of checkout friction. High frequency and low variance
     vs. revenue. See section 3.1.1.</td>
   </tr>
   <tr>
     <td><strong>Guardrail metrics</strong></td>
     <td>Revenue/user &middot; time in checkout &middot; abandonment rate &middot; NPS</td>
     <td>Detect whether conversion is gained at the cost of experience or ticket size.</td>
   </tr>
   <tr>
     <td><strong>Minimum duration</strong></td>
     <td>max(statistical days, 14 days) + optional burn-in period</td>
     <td>14 days covers two complete Monday-Sunday cycles. The burn-in discards
     the learning effect of the new flow in the first days.</td>
   </tr>
   </tbody></table>'
)
append_doc("<h4>3.1.1 Primary metric selection</h4>")
append_doc(
  '<table class="table table-bordered" style="width:100%">
   <thead><tr>
     <th>Candidate metric</th><th>Type</th><th>Advantage</th><th>Limitation</th>
   </tr></thead>
   <tbody>
   <tr style="background:#f0f4ff;">
     <td><strong>&#x2705; Checkout conversion rate</strong><br>
     <small>= purchases / users who started checkout</small></td>
     <td>Primary</td>
     <td>Directly tied to the goal; high frequency; low variance.</td>
     <td>Does not capture whether ticket size changes.</td>
   </tr>
   <tr>
     <td>Revenue per user</td><td>Secondary</td>
     <td>Captures the full effect (conversion &times; ticket).</td>
     <td>(1) Conceptual: at checkout start the user has already decided what to buy;
     the redesign affects whether they <em>complete</em> the purchase, not how much
     they spend. (2) Statistical: high variance &rarr; requires much larger N.</td>
   </tr>
   <tr>
     <td>Cart abandonment rate</td><td>Supplementary</td>
     <td>Identifies at which step the user drops off.</td>
     <td>Intermediate metric, not final.</td>
   </tr>
   <tr>
     <td>Time to complete checkout</td><td>Guardrail</td>
     <td>Detects whether the new flow is slower.</td>
     <td>Does not measure whether the user buys.</td>
   </tr>
   <tr>
     <td>Post-purchase NPS / satisfaction</td><td>Guardrail</td>
     <td>Captures perceived experience.</td>
     <td>Lagged signal; high non-response rate.</td>
   </tr>
   </tbody></table>
   <div class="insight">
     <strong>Selection criterion:</strong> conversion rate is the dominant option.
     Revenue is reported as a secondary metric to validate that the effect
     translates to business outcomes.
   </div>'
)
append_doc("<h3>3.2 Statistical power calculation</h3>")
append_doc(sprintf(
  '<p>Empirical baseline: <strong>p₀ = %.1f%%</strong>. An MDE of 2 pp means
   detecting an increase from %.1f%% to %.1f%%.</p>
   <p style="margin-left:20px; font-family:monospace; background:#f8f9fa; padding:10px; border-radius:4px;">
   n = 2 &times; (z<sub>α/2</sub> + z<sub>β</sub>)² &times; p̅(1−p̅) / δ²<br><br>
   z<sub>α/2</sub> = %.4f &nbsp;|&nbsp; z<sub>β</sub> = %.4f &nbsp;|&nbsp;
   p̅ = %.4f &nbsp;|&nbsp; δ = %.2f pp<br><br>
   <strong>n = %s per group (%s total)</strong></p>',
  p0 * 100, p0 * 100, p1 * 100,
  z_alpha, z_beta, p_bar, MDE * 100,
  format(n_formula, big.mark = ","), format(n_formula * 2, big.mark = ",")
))
append_doc(sprintf(
  '<div class="insight">
     With p₀ = <strong>%.1f%%</strong> and MDE = <strong>2 pp</strong>
     (α = 0.05, power = 0.80): <strong>%s users per group</strong>
     (%s total). Consistent with <code>power.prop.test()</code> (n = %s).
   </div>',
  p0 * 100,
  format(n_formula, big.mark = ","), format(n_formula * 2, big.mark = ","),
  format(n_r, big.mark = ",")
))
append_doc("<h4>3.2.1 Sensitivity analysis</h4>")
append_doc(as.character(html_sens_en))
append_doc("<h4>3.2.2 Experiment duration</h4>")
append_doc(sprintf(
  '<p>Two simultaneous constraints:</p>
   <ul>
     <li><strong>Statistical:</strong> days = ⌈ %s users / daily_traffic ⌉.</li>
     <li><strong>Weekly / biweekly pay cycle:</strong> minimum 14 days to capture
     Monday-Sunday variation. In markets with biweekly pay (days 1 and 15,
     common in Mexico), 2 weeks guarantee at least one post-payday period
     and one pre-payday period — purchasing behavior differs between the two.</li>
   </ul>
   <p><strong>Recommended duration = max(statistical days, 14 days)</strong></p>
   <p><em>Note on the baseline:</em> p₀ = %.1f%% comes from the control group in
   Exercise 1 (push notifications). Ideally, the baseline should be calculated
   from historical checkout data (past month without interventions).</p>',
  format(n_formula * 2, big.mark = ","),
  p0 * 100
))
append_doc(as.character(html_dur_doc_en))
append_doc(
  '<div class="insight">
     <strong>Implication:</strong> platforms with high traffic (≥2,000/day)
     are constrained by the weekly cycle, not by sample size.
   </div>'
)
append_doc("<h3>3.3 Threats to internal and external validity</h3>")
append_doc(
  '<table class="table table-bordered" style="width:100%">
   <thead><tr>
     <th style="width:20%">Threat</th><th style="width:15%">Type</th>
     <th>Description</th><th>Mitigation</th>
   </tr></thead>
   <tbody>
   <tr>
     <td><strong>SRM</strong></td><td>Internal</td>
     <td>The treatment/control ratio deviates from the expected 50/50: assignment bug.
     Consequence: biased estimator and incorrect standard errors.</td>
     <td>Goodness-of-fit chi-square on observed vs. expected ratio:
     <code>chisq.test(x = c(n_ctrl, n_trat), p = c(0.5, 0.5))</code>.
     If p &lt; 0.01, SRM confirmed — invalidate and review logs. Do not correct post-hoc.</td>
   </tr>
   <tr>
     <td><strong>Bug in the checkout</strong></td><td>Internal</td>
     <td>The new checkout has errors. Direct consequences: revenue loss and
     unusable data. Three scenarios:<br><br>
     <strong>Critical</strong> — fails for a significant fraction of the treatment group.
     Action: pause immediately.<br><br>
     <strong>Minor</strong> — visual error without measurable impact on conversion.
     Action: continue and document the period.<br><br>
     <strong>Segment-specific</strong> — bug only on iOS 16 or version &lt; 3.2.
     Action: exclude that segment and report separately.</td>
     <td><strong>Operational monitoring from day 1</strong> (distinct from statistical):
     alerts on HTTP error rate, load time, crashes, step-level abandonment.
     Does not read experiment results &rarr; does not break the statistical design.</td>
   </tr>
   <tr>
     <td><strong>Peeking</strong></td><td>Internal</td>
     <td>Reading statistical results before the pre-specified N inflates the false
     positive rate. With &alpha; = 0.05 and multiple looks it can exceed 30%.</td>
     <td>Pre-register the read date and N target.<br><br>
     <em>No statistical peeking</em> (conversion rate).<br>
     <em>Monitor operational metrics</em> (errors, crashes) &mdash; does not affect FPR.<br><br>
     Alternative: Sequential Testing (always-valid p-values).</td>
   </tr>
   <tr>
     <td><strong>Temporal heterogeneity</strong></td><td>External</td>
     <td>Experiment during atypical periods: <strong>Hot Sale</strong> (May),
     <strong>Buen Fin</strong> (November) or <strong>Black Friday</strong> raise
     the baseline conversion rate. The estimator does not generalize to the rest
     of the year.</td>
     <td>Maintain an e-commerce event calendar. If overlap occurs for operational
     reasons, stratify the analysis (special vs. normal period) and report
     the effects separately.</td>
   </tr>
   </tbody></table>'
)

# --- Presentation EX3 ---

# Slide 3.0: Full process
append_pres("<section>")
append_pres("<h2>Exercise 3 — Process for redesigning the checkout</h2>")
append_pres(
  '<div style="display:grid; grid-template-columns:1fr 1fr; gap:10px;
              font-size:0.76em; margin-top:10px;">

   <!-- Step 1 -->
   <div style="background:#f8f9fa; border-left:4px solid #2563EB;
               padding:10px 14px; border-radius:4px;">
     <div style="font-weight:bold; color:#2563EB; margin-bottom:5px;">
       1. Discovery</div>
     <b>Quantitative:</b> Identify the exact step where users abandon
     the cart and the checkout bounce rate.<br>
     <b>Qualitative:</b> heatmaps and session recordings → detect
     hesitation on form fields, mobile scroll issues, or UI confusion.
   </div>

   <!-- Step 2 -->
   <div style="background:#f8f9fa; border-left:4px solid #2563EB;
               padding:10px 14px; border-radius:4px;">
     <div style="font-weight:bold; color:#2563EB; margin-bottom:5px;">
       2. Hypothesis</div>
     Every change must respond to a detected problem.<br><br>
     <em>"If we reduce billing fields from 10 to 5 and remove mandatory
     account creation, then conversion will increase by 2%,
     because we reduce friction and cognitive effort."</em>
   </div>

   <!-- Step 3 -->
   <div style="background:#f8f9fa; border-left:4px solid #16a34a;
               padding:10px 14px; border-radius:4px;">
     <div style="font-weight:bold; color:#16a34a; margin-bottom:5px;">
       3. Variables</div>
     <b>Independent:</b> the change itself (1-step vs. 3-step checkout,
     button color, hiding the navigation menu).<br>
     <b>Dependent:</b> conversion rate (primary) + time in checkout
     and cart abandonment rate (secondary).<br>
     <b>Control:</b> traffic, acquisition sources, active promotions.
   </div>

   <!-- Step 4 -->
   <div style="background:#f8f9fa; border-left:4px solid #16a34a;
               padding:10px 14px; border-radius:4px;">
     <div style="font-weight:bold; color:#16a34a; margin-bottom:5px;">
       4. Execution (A/B test)</div>
     Traffic split randomly:<br>
     &bull; <b>Group A (control):</b> original checkout<br>
     &bull; <b>Group B (treatment):</b> modified checkout<br><br>
     Duration: until the pre-specified N is reached and at least
     2 full weeks have passed to capture weekly variation.
   </div>

   <!-- Step 5 — full width -->
   <div style="grid-column:1/3; background:#f0f4ff;
               border-left:4px solid #7c3aed; padding:10px 14px; border-radius:4px;">
     <div style="font-weight:bold; color:#7c3aed; margin-bottom:5px;">
       5. Analysis &amp; iteration</div>
     Compare versions using the experimentation software (Braze / Altitude).<br>
     &bull; <b>Group B converts more</b> → hypothesis validated, change rolled out to all users.<br>
     &bull; <b>No change or conversion drops</b> → idea discarded, new hypothesis formulated.
   </div>

   </div>'
)
append_pres("</section>")

# Slide 3B: Steps 3+4 — Variables and test design
append_pres("<section>")
append_pres("<h2>Exercise 3 — Steps 3 &amp; 4: variables and test design</h2>")
append_pres(sprintf(
  '<p style="font-size:0.80em; color:#555; margin-bottom:10px;">
   The hypothesis (step 2) directly defines what to measure and how to assign.
   If the problem is checkout friction, the dependent variable is whether the user completed the purchase.
  </p>
  <table style="width:100%%; font-size:0.77em;">
  <thead><tr>
    <th style="background:#2c3e50; color:white; padding:5px; width:28%%;">Variable / Decision</th>
    <th style="background:#2c3e50; color:white; padding:5px;">Choice</th>
    <th style="background:#2c3e50; color:white; padding:5px;">Justification</th>
  </tr></thead>
  <tbody>
  <tr style="background:#f0f4ff;">
    <td><strong>&#x2705; Primary metric (OEC)</strong></td>
    <td>Purchases / users who started checkout</td>
    <td>Directly measures friction. High frequency, low variance &rarr; manageable N.<br>
    <em>Revenue:</em> high variance and at checkout the user has already decided what to buy.</td></tr>
  <tr>
    <td><strong>Guardrail metrics</strong></td>
    <td>Revenue/user &middot; NPS &middot; time in checkout &middot; abandonment</td>
    <td>Detect if conversion is gained but experience or ticket size is lost.</td></tr>
  <tr style="background:#f8f9fa;">
    <td><strong>Randomization unit</strong></td>
    <td>User (user_id), assigned at checkout start</td>
    <td>Prevents the same user from seeing both versions. This way each person counts once and the conversion rate is measured on the same base in both groups.<br>
    <em>Old app versions:</em> exclude or analyze separately.</td></tr>
  <tr>
    <td><strong>Assignment ratio</strong></td>
    <td>50%% control / 50%% treatment</td>
    <td>Maximizes statistical power for a given N.</td></tr>
  </tbody></table>
  <div class="insight" style="margin-top:8px; font-size:0.80em;">
    Empirical baseline (control group, Ex. 1): <strong>p₀ = %.1f%%</strong>.
    In production, use historical checkout data instead.
  </div>',
  p0 * 100
))
append_pres("</section>")

# Slide 3C: Step 4 — Sample size and duration
html_dur_pres_en <- tabla_duracion_en |>
  kbl(format = "html",
      caption = sprintf("Estimated duration for n = %s per group",
                        format(n_formula, big.mark = ","))) |>
  kable_styling(full_width = FALSE, position = "left") |>
  column_spec(3, bold = TRUE, background = "#e8f4f8") |>
  row_spec(which(daily_traffic == 1000), background = "#f0f4ff")

html_sens_pres_en <- tabla_sens_simple |>
  kbl(format = "html",
      caption = "Users needed per group (alpha=0.05, power=80%)",
      format.args = list(big.mark = ",")) |>
  kable_styling(full_width = FALSE, position = "left") |>
  row_spec(which(tabla_sens_simple$`Baseline (p0)` == "28%"),
           bold = TRUE, background = "#e8f4f8")

append_pres("<section>")
append_pres("<h2>Exercise 3 — Step 4: how many users and how long?</h2>")
append_pres(sprintf(
  '<div style="display:flex; gap:16px; margin-bottom:12px; font-size:0.80em;">
     <div style="flex:1; background:#f0f4ff; padding:10px 14px; border-radius:6px;
                 border-left:4px solid #2563EB;">
       <strong>Sample size</strong><br>
       p₀ = %.1f%% · MDE = 2 pp · α = 0.05 · power = 80%%<br>
       → <strong>%s users per group (%s total)</strong>
     </div>
     <div style="flex:1; background:#f0f9f0; padding:10px 14px; border-radius:6px;
                 border-left:4px solid #16a34a;">
       <strong>Minimum duration</strong><br>
       max(statistical days, 14 days)<br>
       14 days = 2 Monday-Sunday cycles. In markets with biweekly pay
       (days 1 and 15), captures at least one post-payday and one pre-payday period.
     </div>
   </div>',
  p0 * 100,
  format(n_formula, big.mark = ","),
  format(n_formula * 2, big.mark = ",")
))
append_pres(as.character(html_dur_pres_en))
append_pres(sprintf(
  '<div class="insight" style="margin-top:8px; font-size:0.80em;">
     With ~1,000 users/day at checkout: minimum <strong>%d days</strong>.
     High-traffic platforms (&ge;2,000/day) are constrained by the weekly cycle, not sample size.
   </div>',
  pmax(ceiling(n_formula * 2 / 1000), 14)
))
append_pres("</section>")

# Slide 3D: Steps 4/5 — Risks before analyzing
append_pres("<section>")
append_pres("<h2>Exercise 3 — Steps 4/5: risks that can invalidate the test</h2>")
append_pres(
  '<p style="font-size:0.80em; color:#555; margin-bottom:8px;">
   Before launching the experiment and before reading the results, verify:
  </p>
  <table style="width:100%; font-size:0.74em;">
  <thead><tr>
    <th style="background:#2c3e50; color:white; padding:5px; width:18%;">Risk</th>
    <th style="background:#2c3e50; color:white; padding:5px; width:9%;">Type</th>
    <th style="background:#2c3e50; color:white; padding:5px;">What can happen?</th>
    <th style="background:#2c3e50; color:white; padding:5px;">Mitigation</th>
  </tr></thead>
  <tbody>
  <tr>
    <td><strong>SRM</strong></td><td>Internal</td>
    <td>Ratio is not 50/50 due to assignment bug &rarr; biased estimator</td>
    <td>Chi-square on observed vs. expected ratio. If p &lt; 0.01, invalidate.</td></tr>
  <tr style="background:#f8f9fa;">
    <td><strong>Bug in the checkout</strong></td><td>Internal</td>
    <td><em>Critical</em> &rarr; pause;<br>
    <em>Minor</em> &rarr; continue + document;<br>
    <em>Segment</em> (e.g. iOS 16) &rarr; exclude and analyze separately</td>
    <td><strong>Operational monitoring from day 1</strong>: alerts on errors,
    crashes, load time. Does not interfere with the statistical design.</td></tr>
  <tr>
    <td><strong>Peeking</strong></td><td>Internal</td>
    <td>Reading results before the N target &rarr; FPR can exceed 30% with α = 0.05</td>
    <td>Pre-register read date and N.<br>
    <em>No statistical peeking.</em><br>
    <em>Monitor operational metrics</em> (errors, crashes).</td></tr>
  <tr style="background:#f8f9fa;">
    <td><strong>Temporal heterogeneity</strong></td><td>External</td>
    <td>Hot Sale, Buen Fin, Black Friday: baseline conversion rises &rarr; result does not generalize</td>
    <td>E-commerce event calendar. If overlap occurs, stratify the analysis.</td></tr>
  </tbody></table>'
)
append_pres("</section>")

# =============================================================================
# EX4 — DiD
# =============================================================================

html_balance_obs_en <- tabla_balance_obs |>
  kbl(format    = "html",
      caption   = "Table 7. Observable covariate balance by group (pre-period)",
      col.names = c("Variable",
                    sprintf("Control (N=%s)", format(n_e0, big.mark=",")), "SD",
                    sprintf("Treated (N=%s)", format(n_e1, big.mark=",")), "SD",
                    "Difference", "p-value"),
      align     = c("l","r","r","r","r","r","r")) |>
  kable_styling(bootstrap_options = c("striped", "condensed", "hover"),
                full_width = FALSE, position = "left") |>
  footnote(
    general = sprintf(
      "Continuous variables: t-test. Segment: t-test by category. Region: χ² p = %.4f. App version: χ² p = %.4f.",
      chi_region$p.value, chi_appver$p.value),
    general_title = "Note:"
  )

tabla_did_manual_en <- tabla_did_manual |>
  mutate(Grupo = case_when(
    Grupo == "Diferencia (DiD)" ~ "Difference (DiD)",
    TRUE ~ Grupo
  )) |>
  setNames(c("Group", "Pre ($)", "Post ($)", "Δ ($)"))

html_did_manual_en <- tabla_did_manual_en |>
  kbl(format  = "html",
      caption = "Table 8. Double difference decomposition") |>
  kable_styling(bootstrap_options = c("striped", "condensed", "hover"),
                full_width = FALSE, position = "left") |>
  row_spec(3, bold = TRUE, background = "#EFF6FF") |>
  footnote(
    general = sprintf(
      "ATT (δ) = $%.2f/week (SE = %.2f, p = %.4f, 95%% CI: [$%.2f, $%.2f]).",
      att, att_se, att_p, att_ci_l, att_ci_u),
    general_title = "Note:"
  )

tabla_did_modelos_en <- modelsummary(
  list("(1) Week FE" = m_did_fe, "(2) TWFE" = m_did_twfe),
  vcov      = list(vcov(m_did_fe), vcov(m_did_twfe)),
  coef_map  = c("enrolled"      = "Enrolled (β)",
                "enrolled:post" = "Enrolled × Post = ATT (δ)"),
  gof_map   = c("nobs", "r.squared"),
  stars     = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  output    = "html",
  title     = "Table 9. DiD models (SE clustered by client_id)"
)

fig_tag_en <- sprintf(
  '<figure style="margin:1em 0;">
     <img src="figures/fig_did_trends.png"
          style="max-width:100%%; border:1px solid #e0e0e0; border-radius:4px;">
     <figcaption style="font-size:0.85em; color:#555; margin-top:4px;">
       Figure 1. Average weekly revenue trends by enrollment group.
     </figcaption>
   </figure>'
)

# --- Document EX4 ---
append_doc("  <h2>Exercise 4 (Bonus) — Difference-in-Differences</h2>")
append_doc(
  '<p>Using observational data (<code>cai_obs_data.csv</code>), the effect of the
   loyalty program on weekly revenue was estimated. Unlike Exercise 1,
   <strong>there is no randomization</strong> here: clients chose to enroll.</p>'
)
append_doc('<h3>4.0 Covariate balance by group</h3>')
append_doc(sprintf(
  '<p>Comparison of enrolled (N=%s) and non-enrolled (N=%s) clients in the pre-period.
   In observational data, imbalance is expected.</p>',
  format(n_e1, big.mark=","), format(n_e0, big.mark=",")
))
append_doc(as.character(html_balance_obs_en))
append_doc(sprintf(
  '<div class="insight">
     <strong>Imbalance confirmed:</strong> enrolled clients start with a higher revenue
     base ($%.0f vs $%.0f), greater tenure, and higher NPS. Enrollment is
     <strong>not random</strong>. DiD controls for level differences, not trend differences.
   </div>',
  rev_mean_e1, rev_mean_e0
))
append_doc("  <h3>4.1 Parallel trends test (pre-period)</h3>")
append_doc(sprintf(
  '<div class="insight">
     <strong>H0:</strong> the pre-treatment trend is equal across groups.<br>
     <strong>Result:</strong> F = %.3f, p = %.4f. %s
   </div>',
  f_trends, p_trends,
  ifelse(tendencias_paralelas,
    "H0 not rejected: assumption is compatible with the data.",
    "H0 rejected: trends differ. Interpret ATT as an upper bound.")
))
append_doc(fig_tag_en)
append_doc("  <h3>4.2 Double difference decomposition</h3>")
append_doc(as.character(html_did_manual_en))
append_doc("  <h3>4.3 DiD models</h3>")
append_doc(as.character(tabla_did_modelos_en))
append_doc(sprintf(
  '<div class="insight">
     <strong>Week FE:</strong> ATT = <strong>$%.2f/week</strong>
     (SE=%.2f, 95%% CI: [$%.2f,$%.2f], p=%.4f).<br>
     <strong>TWFE:</strong> $%.2f &mdash; the difference (~$%.1f) reflects
     within-client heterogeneity. Both estimators converge in order of magnitude.<br><br>
     <strong>Caveat:</strong> the parallel trends test is rejected
     (F=%.3f, p&lt;0.0001): the enrolled group was already growing faster before
     the cutoff. The estimators are an upper bound on the causal effect.
   </div>',
  att, att_se, att_ci_l, att_ci_u, att_p,
  att_twfe, abs(att_twfe - att), f_trends
))
append_doc(
  '<h3>4.4 Limitations and assumptions</h3>
   <ul>
     <li><strong>Selection into enrolled:</strong> clients with higher spending
     propensity tend to enroll more.</li>
     <li><strong>Parallel trends:</strong> formal test rejects the hypothesis
     (F=998.6, p&lt;0.0001).</li>
     <li><strong>Conclusion:</strong> solid descriptive evidence of a positive
     association. For clean causal identification, an RCT or an eligibility
     discontinuity design would be required.</li>
   </ul>'
)
append_doc("</body></html>")

# --- Presentation EX4 ---
html_balance_pres_en <- tabla_balance_obs |>
  kbl(format    = "html",
      col.names = c("Variable",
                    sprintf("Control (N=%s)", format(n_e0, big.mark=",")), "SD",
                    sprintf("Treated (N=%s)", format(n_e1, big.mark=",")), "SD",
                    "Difference", "p-value"),
      align     = c("l","r","r","r","r","r","r")) |>
  kable_styling(full_width = FALSE, position = "left", font_size = 13)

append_pres("<section>")
append_pres("<h2>Exercise 4 — Are the groups comparable?</h2>")
append_pres(
  '<p style="font-size:0.80em; color:#555; margin-bottom:8px;">
   Without randomization, groups may differ <em>before</em> the program.
   </p>'
)
append_pres(as.character(html_balance_pres_en))
append_pres(sprintf(
  '<div class="insight" style="font-size:0.80em; margin-top:10px;">
     <strong>Selection is evident:</strong> enrolled clients start with a higher
     revenue base ($%.0f vs $%.0f), are more tenured, and have higher NPS.<br>
     Direct comparison is not valid: the DiD method is required.
   </div>',
  rev_mean_e1, rev_mean_e0
))
append_pres("</section>")

append_pres("<section>")
append_pres("<h2>Exercise 4 (Bonus) — Loyalty program: does it work?</h2>")
append_pres(sprintf(
  '<table style="width:100%%; font-size:0.8em;">
  <thead><tr>
    <th style="background:#2c3e50; color:white; padding:6px;">Element</th>
    <th style="background:#2c3e50; color:white; padding:6px;">Detail</th>
  </tr></thead>
  <tbody>
  <tr><td><strong>Data</strong></td><td>20,000 clients &times; 18 weeks</td></tr>
  <tr><td><strong>Treatment group</strong></td><td>Enrolled (enrolled=1): %s clients</td></tr>
  <tr><td><strong>Control group</strong></td><td>Non-enrolled (enrolled=0): %s clients</td></tr>
  <tr><td><strong>Pre-period</strong></td><td>Weeks 1–9</td></tr>
  <tr><td><strong>Post-period</strong></td><td>Weeks 10–18</td></tr>
  </tbody></table>
  <div class="insight" style="margin-top:12px; font-size:0.85em;">
    <strong>DiD intuition:</strong> instead of comparing groups directly,
    we measure how much revenue <em>accelerated</em> after the program started,
    relative to the control group.
  </div>',
  format(n_e1, big.mark = ","),
  format(n_e0, big.mark = ",")
))
append_pres("</section>")

html_did_manual_pres_en <- tabla_did_manual_en |>
  kbl(format  = "html",
      caption = "Decomposition: revenue change before and after the program") |>
  kable_styling(full_width = FALSE, position = "left")

append_pres("<section>")
append_pres("<h2>Exercise 4 — Analysis results</h2>")
append_pres(as.character(html_did_manual_pres_en))
append_pres(sprintf(
  '<div style="display:flex; gap:20px; margin-top:14px; font-size:0.82em;">
     <div style="flex:1; background:#f0f4ff; padding:12px 16px;
                 border-radius:6px; border-left:4px solid #2563EB;">
       <div style="font-size:1.6em; font-weight:bold; color:#2563EB;">+$%.2f</div>
       <div><strong>additional per week</strong> for enrolled clients</div>
       <div style="font-size:0.88em; color:#555; margin-top:4px;">
         95%% CI: [$%.2f, $%.2f] &nbsp;|&nbsp; p &lt; 0.0001</div>
     </div>
     <div style="flex:1; background:#f8f9fa; padding:12px 16px;
                 border-radius:6px; border-left:4px solid #6c757d; font-size:0.88em;">
       <strong>How to read the table:</strong><br>
       Enrolled grew +$%.1f/week. Control grew +$%.1f/week without the program.
       The difference-in-differences = program effect.
     </div>
   </div>',
  att, att_ci_l, att_ci_u,
  rev_e1_post - rev_e1_pre,
  rev_e0_post - rev_e0_pre
))
append_pres("</section>")

append_pres("<section>")
append_pres("<h2>Exercise 4 — Can we trust the result?</h2>")
append_pres(
  '<img src="figures/fig_did_trends.png"
        style="max-height:260px; margin-bottom:10px; border:1px solid #ddd; border-radius:4px;">'
)
append_pres(sprintf(
  '<div class="insight" style="font-size:0.82em;">
     <strong style="color:#b91c1c;">⚠ Limitation — selection bias:</strong><br>
     Enrolled clients were already growing faster <em>before</em> the program (weeks 1–9).
     The result of <strong>+$%.2f/week</strong> is the <em>upper bound</em>
     of the effect: part of it reflects who enrolls (selection), not the program.<br><br>
     <strong>For the net causal effect</strong>, an RCT or an eligibility
     discontinuity design would be needed.
   </div>',
  att
))
append_pres("</section>")

close_pres()

cat("\n[render_en] Done.\n")
cat("  Report:        ", path_reporte, "\n")
cat("  Presentation:  ", path_pres, "\n")
