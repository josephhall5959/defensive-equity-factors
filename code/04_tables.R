# ============================================================================
# 04_tables.R  —  Emit LaTeX tables (booktabs) and an inline-numbers macro file
# ============================================================================
# Writes output/tables/tab1..tab4.tex (each a standalone tabular for \input)
# and output/tables/numbers.tex (a set of \newcommand macros so the prose can
# cite live numbers without hand-transcription).
# ============================================================================

source("code/00_helpers.R")
r <- readRDS(file.path(PATHS$processed, "results.rds"))
dir.create(PATHS$tables, recursive = TRUE, showWarnings = FALSE)

w <- function(x, file) writeLines(x, file.path(PATHS$tables, file))
b <- function(...) paste0(...)                       # concat shorthand
tstr <- function(t) paste0("(", fmt(t, 2), ")")      # t-stat in parentheses

# ---------------------------------------------------------------------------
# Table 1 — Unconditional factor performance
# ---------------------------------------------------------------------------
t1 <- r$t1
rows <- apply(t1, 1, function(z) {
  b(z["factor"], " & ", fmt(as.numeric(z["mean_mo"]),2), " & ",
    fmt(as.numeric(z["mean_ann"]),2), " & ", fmt(as.numeric(z["sd_ann"]),2), " & ",
    fmt(as.numeric(z["sharpe"]),2), " & ", fmt(as.numeric(z["t_mean"]),2), " & ",
    fmt(as.numeric(z["skew"]),2), " & ", fmt(as.numeric(z["kurt"]),2), " & ",
    fmt(as.numeric(z["mdd"]),1), " & ", fmt(as.numeric(z["pct_pos"]),1), " \\\\")
})
w(c(
  "\\begin{tabular}{lccccccccc}",
  "\\toprule",
  " & Mean & Mean & Vol. & & & & & Max & \\% \\\\",
  "Factor & (mo.) & (ann.) & (ann.) & Sharpe & $t$(mean) & Skew & Kurt. & DD & Pos. \\\\",
  "\\midrule",
  rows,
  "\\bottomrule",
  "\\end{tabular}"), "tab1.tex")

# ---------------------------------------------------------------------------
# Table 2 — Performance conditional on the NBER business-cycle state
# ---------------------------------------------------------------------------
t2 <- r$t2
rows <- apply(t2, 1, function(z) {
  diff <- as.numeric(z["diff_ann"]); tt <- as.numeric(z["t_diff"]); pp <- as.numeric(z["p_diff"])
  b(z["factor"], " & ",
    fmt(as.numeric(z["exp_ann"]),2), " & ", fmt(as.numeric(z["rec_ann"]),2), " & ",
    fmt(diff,2), star(pp), " & ", tstr(tt), " & ",
    fmt(as.numeric(z["sharpe_e"]),2), " & ", fmt(as.numeric(z["sharpe_r"]),2), " & ",
    fmt(as.numeric(z["mdd_r"]),1), " & ", fmt(as.numeric(z["worst_r"]),1), " \\\\")
})
w(c(
  "\\begin{tabular}{lcccccccc}",
  "\\toprule",
  " & \\multicolumn{2}{c}{Mean return (ann. \\%)} & & & \\multicolumn{2}{c}{Sharpe} & Rec. & Worst \\\\",
  "\\cmidrule(lr){2-3}\\cmidrule(lr){6-7}",
  "Factor & Expansion & Recession & Diff. & $t$(Diff.) & Exp. & Rec. & MaxDD & month \\\\",
  "\\midrule",
  rows,
  "\\bottomrule",
  "\\end{tabular}"), "tab2.tex")

# ---------------------------------------------------------------------------
# Table 3 — Real-time bear-market signal (implementable, no look-ahead)
# ---------------------------------------------------------------------------
t4 <- r$t4
rows <- apply(t4, 1, function(z) {
  pp <- as.numeric(z["p_diff"])
  b(z["factor"], " & ",
    fmt(as.numeric(z["bull_ann"]),2), " & ", fmt(as.numeric(z["bear_ann"]),2), " & ",
    fmt(as.numeric(z["diff_ann"]),2), star(pp), " & ", tstr(as.numeric(z["t_diff"])), " \\\\")
})
w(c(
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  " & \\multicolumn{2}{c}{Mean return (ann. \\%)} & & \\\\",
  "\\cmidrule(lr){2-3}",
  "Factor & Bull & Bear & Diff. & $t$(Diff.) \\\\",
  "\\midrule",
  rows,
  "\\bottomrule",
  "\\end{tabular}"), "tab3.tex")

# ---------------------------------------------------------------------------
# Table 4 — Subsample stability of the recession differential
# ---------------------------------------------------------------------------
ss <- r$subsample
rows <- apply(ss, 1, function(z) {
  b(z["factor"], " & ",
    fmt(as.numeric(z["diff_pre"]),2), " & ", tstr(as.numeric(z["t_pre"])), " & ",
    fmt(as.numeric(z["diff_post"]),2), " & ", tstr(as.numeric(z["t_post"])), " \\\\")
})
w(c(
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  " & \\multicolumn{2}{c}{1963--1989} & \\multicolumn{2}{c}{1990--2026} \\\\",
  "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}",
  "Factor & Diff. & $t$ & Diff. & $t$ \\\\",
  "\\midrule",
  rows,
  "\\bottomrule",
  "\\end{tabular}"), "tab4.tex")

# ---------------------------------------------------------------------------
# Inline-number macros (numbers.tex)
# ---------------------------------------------------------------------------
m  <- r$meta
g  <- function(f, col) r$t2[r$t2$factor == f, col]
g1 <- function(f, col) r$t1[r$t1$factor == f, col]
g4 <- function(f, col) r$t4[r$t4$factor == f, col]
mc <- r$mom_crash
worst_row <- mc$worst10[1, ]

macro <- function(name, val) b("\\newcommand{\\", name, "}{", val, "}")
lines <- c(
  macro("nMonths",   m$n_months),
  macro("sampStart", m$start),
  macro("sampEnd",   m$end),
  macro("nRec",      m$n_rec),
  macro("pctRec",    fmt(m$pct_rec,1)),
  macro("nRecessions", m$n_recessions),
  macro("bearFrac",  fmt(m$bear_frac,1)),
  macro("bearOverlap", fmt(m$bear_nber_overlap,0)),
  # NBER-conditional differentials
  macro("mktRecDiff", fmt(g("MKT","diff_ann"),1)),
  macro("mktRecT",    fmt(g("MKT","t_diff"),2)),
  macro("cmaRecDiff", fmt(g("CMA","diff_ann"),1)),
  macro("cmaRecT",    fmt(g("CMA","t_diff"),2)),
  macro("cmaRecMean", fmt(g("CMA","rec_ann"),1)),
  macro("rmwRecDiff", fmt(g("RMW","diff_ann"),1)),
  macro("rmwRecMean", fmt(g("RMW","rec_ann"),1)),
  macro("hmlRecDiff", fmt(g("HML","diff_ann"),1)),
  macro("smbRecDiff", fmt(g("SMB","diff_ann"),1)),
  macro("momRecDiff", fmt(g("MOM","diff_ann"),1)),
  macro("momRecT",    fmt(g("MOM","t_diff"),2)),
  # Sharpe ratios
  macro("cmaSharpeE", fmt(g("CMA","sharpe_e"),2)),
  macro("cmaSharpeR", fmt(g("CMA","sharpe_r"),2)),
  macro("momSharpeE", fmt(g("MOM","sharpe_e"),2)),
  macro("momSharpeR", fmt(g("MOM","sharpe_r"),2)),
  # real-time bear signal
  macro("momBearDiff", fmt(g4("MOM","diff_ann"),1)),
  macro("momBearT",    fmt(g4("MOM","t_diff"),2)),
  macro("rmwBearDiff", fmt(g4("RMW","diff_ann"),1)),
  macro("cmaBearDiff", fmt(g4("CMA","diff_ann"),1)),
  # momentum crash
  macro("momWorst",     fmt(worst_row$MOM,1)),
  macro("momWorstDate", format(worst_row$date, "%B %Y")),
  macro("momRecovAnn",  fmt(mc$mom_recovery_ann,1)),
  macro("momOtherAnn",  fmt(mc$mom_other_ann,1)),
  # unconditional Sharpe extremes
  macro("momSharpe", fmt(g1("MOM","sharpe"),2)),
  macro("smbSharpe", fmt(g1("SMB","sharpe"),2))
)
w(lines, "numbers.tex")

message("Tables written: tab1-tab4.tex + numbers.tex in ", PATHS$tables, "/")
