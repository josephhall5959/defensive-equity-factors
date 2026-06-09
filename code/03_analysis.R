# ============================================================================
# 03_analysis.R  —  All estimation; writes data/processed/results.rds
# ============================================================================
# Produces every number used in the paper:
#   T1  unconditional factor performance
#   T2  performance conditional on the NBER business-cycle state
#   T3  recession-dummy regressions (Newey-West HAC)
#   T4  real-time, implementable bear-market signal regressions
#   event studies around NBER peaks and troughs (for figures)
#   momentum-crash diagnostics
# ============================================================================

source("code/00_helpers.R")
panel <- readRDS(file.path(PATHS$processed, "factors.rds"))

FACTORS <- c("MKT.RF", "SMB", "HML", "RMW", "CMA", "MOM")
FLABEL  <- c(MKT.RF = "MKT", SMB = "SMB", HML = "HML",
             RMW = "RMW", CMA = "CMA", MOM = "MOM")

skewness <- function(x) { x <- x[!is.na(x)]; m <- mean(x)
  mean((x - m)^3) / (mean((x - m)^2))^1.5 }
ex_kurtosis <- function(x) { x <- x[!is.na(x)]; m <- mean(x)
  mean((x - m)^4) / (mean((x - m)^2))^2 - 3 }

# ---------------------------------------------------------------------------
# T1: Unconditional performance
# ---------------------------------------------------------------------------
t1 <- lapply(FACTORS, function(f) {
  x  <- panel[[f]]
  m0 <- lm(x ~ 1)
  ht <- hac_test(m0)
  data.frame(
    factor   = FLABEL[f],
    mean_mo  = mean(x),
    mean_ann = ann_mean(x),
    sd_ann   = ann_sd(x),
    sharpe   = ann_sharpe(x),
    t_mean   = ht$t[1],
    skew     = skewness(x),
    kurt     = ex_kurtosis(x),
    mdd      = max_drawdown(x),
    pct_pos  = 100 * mean(x > 0),
    row.names = NULL)
})
t1 <- do.call(rbind, t1)

# ---------------------------------------------------------------------------
# T2 & T3: Conditional performance and recession-dummy regressions
# ---------------------------------------------------------------------------
rec <- panel$recession
t2 <- lapply(FACTORS, function(f) {
  x  <- panel[[f]]
  xe <- x[rec == 0]; xr <- x[rec == 1]
  reg <- lm(x ~ rec)                       # intercept = expansion mean; slope = rec - exp
  ht  <- hac_test(reg)
  data.frame(
    factor    = FLABEL[f],
    exp_ann   = ann_mean(xe),
    rec_ann   = ann_mean(xr),
    diff_ann  = ann_mean(xr) - ann_mean(xe),
    t_diff    = ht$t[ht$term == "rec"],
    p_diff    = ht$p[ht$term == "rec"],
    sharpe_e  = ann_sharpe(xe),
    sharpe_r  = ann_sharpe(xr),
    mdd_r     = max_drawdown(xr),
    worst_r   = min(xr),
    row.names = NULL)
})
t2 <- do.call(rbind, t2)

# Full regression detail (intercept + slope with HAC t-stats) for T3.
t3 <- lapply(FACTORS, function(f) {
  ht <- hac_test(lm(panel[[f]] ~ rec))
  data.frame(factor = FLABEL[f],
             alpha = ht$est[ht$term == "(Intercept)"],
             t_alpha = ht$t[ht$term == "(Intercept)"],
             beta = ht$est[ht$term == "rec"],
             t_beta = ht$t[ht$term == "rec"],
             p_beta = ht$p[ht$term == "rec"], row.names = NULL)
})
t3 <- do.call(rbind, t3)

# ---------------------------------------------------------------------------
# T4: Real-time, implementable bear-market signal
# ---------------------------------------------------------------------------
# NBER dates are announced with long lags and are not tradeable. We define an
# ex-ante signal known at the start of month t: BEAR = 1 if the trailing
# 12-month total market return (through t-1) is negative. The market total
# return is MKT.RF + RF. This is fully implementable in real time.
mkt_tot <- panel$MKT.RF + panel$RF
roll12  <- rep(NA_real_, nrow(panel))
for (i in 13:nrow(panel)) {
  roll12[i] <- prod(1 + mkt_tot[(i - 12):(i - 1)] / 100) - 1   # through t-1
}
panel$bear <- ifelse(roll12 < 0, 1L, 0L)
bear <- panel$bear

t4 <- lapply(FACTORS, function(f) {
  d  <- data.frame(x = panel[[f]], bear = bear)
  d  <- d[!is.na(d$bear), ]
  ht <- hac_test(lm(x ~ bear, data = d))
  data.frame(
    factor   = FLABEL[f],
    bull_ann = ann_mean(d$x[d$bear == 0]),
    bear_ann = ann_mean(d$x[d$bear == 1]),
    diff_ann = ann_mean(d$x[d$bear == 1]) - ann_mean(d$x[d$bear == 0]),
    t_diff   = ht$t[ht$term == "bear"],
    p_diff   = ht$p[ht$term == "bear"],
    row.names = NULL)
})
t4 <- do.call(rbind, t4)
bear_frac <- mean(bear, na.rm = TRUE)
# overlap of bear signal with NBER recessions
bd <- panel[!is.na(panel$bear), ]
bear_nber_overlap <- mean(bd$bear[bd$recession == 1] == 1)   # share of recession months flagged

# ---------------------------------------------------------------------------
# Event studies around NBER peaks and troughs (averaged cumulative paths)
# ---------------------------------------------------------------------------
H <- 12
event_matrix <- function(event_dates) {
  out <- array(NA_real_, dim = c(2 * H + 1, length(FACTORS), length(event_dates)),
               dimnames = list(as.character(-H:H), FACTORS, NULL))
  for (e in seq_along(event_dates)) {
    i0 <- which(panel$date == event_dates[e])
    if (length(i0) == 0) next
    idx <- (i0 - H):(i0 + H)
    if (idx[1] < 1 || idx[length(idx)] > nrow(panel)) next
    for (f in FACTORS) {
      r <- panel[[f]][idx]
      out[, f, e] <- (cumprod(1 + r / 100) - 1) * 100      # cumulative %, start at h=-12
    }
  }
  apply(out, c(1, 2), mean, na.rm = TRUE)                  # average across episodes
}
peaks_in   <- NBER_CYCLES$peak[NBER_CYCLES$peak   >= min(panel$date) &
                               NBER_CYCLES$peak   <= max(panel$date)]
troughs_in <- NBER_CYCLES$trough[NBER_CYCLES$trough >= min(panel$date) &
                                 NBER_CYCLES$trough <= max(panel$date)]
event_peak   <- event_matrix(peaks_in)
event_trough <- event_matrix(troughs_in)

# ---------------------------------------------------------------------------
# Momentum-crash diagnostics
# ---------------------------------------------------------------------------
# Months within 12 months following an NBER trough ("recovery" window).
months_after_trough <- function(k = 12) {
  flag <- integer(nrow(panel))
  for (tr in troughs_in) {
    i0 <- which(panel$date == tr)
    if (length(i0)) flag[i0:min(i0 + k, nrow(panel))] <- 1L
  }
  flag
}
panel$post_trough <- months_after_trough(12)
ord <- order(panel$MOM)
worst10 <- panel[ord[1:10], c("date", "MOM", "recession", "post_trough")]
mom_crash <- list(
  worst10            = worst10,
  worst10_in_recovery = mean(worst10$post_trough == 1),
  mom_recovery_ann   = ann_mean(panel$MOM[panel$post_trough == 1]),
  mom_other_ann      = ann_mean(panel$MOM[panel$post_trough == 0]),
  mom_uncond_ann     = ann_mean(panel$MOM)
)

# ---------------------------------------------------------------------------
# Subsample stability of the recession dummy (pre/post 1990)
# ---------------------------------------------------------------------------
sub <- lapply(c("1963/1989" = 1989, "1990/2026" = 1990), function(yr) NULL)
split_yr <- 1990
sub_pre  <- panel[panel$year <  split_yr, ]
sub_post <- panel[panel$year >= split_yr, ]
subsample <- lapply(FACTORS, function(f) {
  hp <- hac_test(lm(sub_pre[[f]]  ~ sub_pre$recession))
  hq <- hac_test(lm(sub_post[[f]] ~ sub_post$recession))
  data.frame(factor = FLABEL[f],
             diff_pre  = ann_mean(sub_pre[[f]][sub_pre$recession==1])  - ann_mean(sub_pre[[f]][sub_pre$recession==0]),
             t_pre     = hp$t[2],
             diff_post = ann_mean(sub_post[[f]][sub_post$recession==1]) - ann_mean(sub_post[[f]][sub_post$recession==0]),
             t_post    = hq$t[2], row.names = NULL)
})
subsample <- do.call(rbind, subsample)

# ---------------------------------------------------------------------------
# Save everything (plus the panel with derived columns, for figures)
# ---------------------------------------------------------------------------
results <- list(
  meta = list(
    n_months   = nrow(panel),
    start      = format(min(panel$date), "%B %Y"),
    end        = format(max(panel$date), "%B %Y"),
    n_rec      = sum(panel$recession),
    pct_rec    = 100 * mean(panel$recession),
    n_recessions = length(peaks_in),
    bear_frac  = 100 * bear_frac,
    bear_nber_overlap = 100 * bear_nber_overlap,
    nw_note    = "Newey-West HAC, automatic lag = floor(4*(n/100)^(2/9))"
  ),
  t1 = t1, t2 = t2, t3 = t3, t4 = t4,
  event_peak = event_peak, event_trough = event_trough,
  mom_crash = mom_crash, subsample = subsample,
  factors = FACTORS, flabel = FLABEL
)
saveRDS(results, file.path(PATHS$processed, "results.rds"))
saveRDS(panel,   file.path(PATHS$processed, "panel_derived.rds"))

message("Analysis complete. Key conditional results (annualized % difference, recession - expansion):")
print(within(t2[, c("factor","exp_ann","rec_ann","diff_ann","t_diff")],
             { exp_ann<-round(exp_ann,1); rec_ann<-round(rec_ann,1)
               diff_ann<-round(diff_ann,1); t_diff<-round(t_diff,2) }), row.names = FALSE)
