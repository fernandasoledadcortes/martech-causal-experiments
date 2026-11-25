# Causal Inference & A/B Testing for MarTech

A take-home causal-inference assessment for a MarTech consultant role, solved end
to end in **R**: estimate the causal effect of a marketing experiment, find *who*
it works for, **design the next experiment**, and — as a bonus — handle a messier
observational question honestly. Results are communicated to a non-technical
audience in bilingual executive reports.

> The analysis, with full commentary, lives in [`code/`](code); the rendered
> executive reports and slides are generated into `outputs/` (Spanish + English).
> This README is the summary.

---

## Context

An e-commerce platform's marketing team ran a randomized experiment: send a push
notification with a **countdown-timer promotion** (treatment) vs a standard push
(control), and measure the lift in conversion and revenue. The business wants to
know the effect, who responds most, how to design a clean follow-up test, and what
can (and can't) be concluded from observational data.

## What's in it

### Ex. 1 — Average Treatment Effect (RCT)
Validate randomization (covariate **balance tests** + a joint F-test / SRM logic),
then estimate the ATE by OLS with **HC2 robust standard errors**.
- **Conversion: +11.4 pp**, **revenue: +$24.25 per user** (both p < 0.0001),
  confirmed with an ANCOVA specification (covariate-adjusted).

### Ex. 2 — Conditional ATE (heterogeneity by channel)
Interaction model `revenue ~ treatment × channel`, CATEs via `avg_slopes()` (HC2).
- **Organic +$36.4 · Paid search +$18.9 · Social +$8.3**; joint heterogeneity
  test **F = 197, p < 0.0001**.
- Read: the countdown is a **closing** tool, not a **discovery** tool — it works on
  users who already have intent.

### Ex. 3 — Experiment design (a new checkout-flow test)
- **Randomization unit:** the user (randomizing by session would violate SUTVA).
- **Power/sample size:** to detect a **+2 pp MDE** at α = 0.05, power = 0.80 from a
  28.1% baseline → **≈ 8,100 users per arm** (closed-form formula *and*
  `power.prop.test`, cross-checked), with a **sensitivity table** over baseline × MDE.
- **Duration:** a traffic-vs-days table with a **14-day minimum** to cover weekly
  seasonality.
- **5 threats to validity** (internal/external) each with a concrete mitigation.

### Ex. 4 — Difference-in-Differences (bonus, observational)
Effect of a loyalty program on weekly revenue from a 20,000-customer × 18-week panel,
clustered SEs.
- ATT ≈ **$81.72/week** — **but the parallel-trends test is rejected (F = 998.6)**,
  so this is reported as an **upper bound under selection bias, not a clean causal
  effect.** Being honest about identification is the point.

## From prediction to decisions

The write-up closes on the consulting move: **CATEs motivate uplift modelling**
(target the *persuadables*, not the sure-things or lost-causes), and the DiD caveat
motivates a **properly randomized** follow-up rather than trusting the observational
estimate.

## How to run

From the **repo root**:

```r
source("code/run_all.R")   # root defaults to getwd(); runs Ex.1–4 and renders all reports
```

Missing packages install automatically. Outputs (into `outputs/`): executive report
+ reveal.js slides, each in **Spanish and English**, plus the DiD trends figure.
Run an individual exercise with `code/01_ej1_ate.R` … `04_ej4_obs.R` (each sources
`00_setup.R`).

## Tools

R · tidyverse · **fixest** (fixed effects, clustered SE) · **sandwich/lmtest** (HC2
robust SE) · **marginaleffects** (CATEs, AME) · **pwr / power.prop.test** (sample
size) · **car** (VIF) · reproducible staged scripts · bilingual HTML reporting.

## Data

Two **synthetic** datasets generated for the exercise (no real personal data):
`cai_exp_data.csv` (RCT, 50,000 users after cleaning) and `cai_obs_data.csv`
(observational panel, 20,000 customers × 18 weeks). `00_setup.R` documents the
data-quality fixes applied (exact duplicates, dirty categories, sentinel values).

## A note on how this was built

A take-home case (practice, not paid work). I used AI (Claude) as a coding
assistant — scaffolding, writing R, proposing checks — while **every modelling and
identification decision, and its justification, is mine** (see the running
commentary in the code).
