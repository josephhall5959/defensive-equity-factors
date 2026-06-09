# ============================================================================
# 00_helpers.R  —  Shared constants and utility functions
# Project: Do Defensive Equity Factors Deliver?
# ============================================================================
# This file defines:
#   (a) the NBER U.S. business-cycle reference dates (public domain),
#   (b) a function to build a monthly recession indicator,
#   (c) Newey-West HAC inference (with a self-contained fallback), and
#   (d) small formatting helpers used by the table/figure scripts.
# It is sourced by every numbered script; it performs no I/O on its own.
# ============================================================================

# ---- Paths -----------------------------------------------------------------
PATHS <- list(
  raw       = "data/raw",
  processed = "data/processed",
  tables    = "output/tables",
  figures   = "output/figures"
)

# ---- NBER business-cycle reference dates -----------------------------------
# Source: NBER, "US Business Cycle Expansions and Contractions"
# (https://www.nber.org/research/data/us-business-cycle-expansions-and-contractions).
# Each row is one contraction: a peak month (last month of expansion) and the
# subsequent trough month (last month of contraction). Dates are first-of-month.
NBER_CYCLES <- data.frame(
  peak   = as.Date(c("1926-10-01","1929-08-01","1937-05-01","1945-02-01",
                     "1948-11-01","1953-07-01","1957-08-01","1960-04-01",
                     "1969-12-01","1973-11-01","1980-01-01","1981-07-01",
                     "1990-07-01","2001-03-01","2007-12-01","2020-02-01")),
  trough = as.Date(c("1927-11-01","1933-03-01","1938-06-01","1945-10-01",
                     "1949-10-01","1954-05-01","1958-04-01","1961-02-01",
                     "1970-11-01","1975-03-01","1980-07-01","1982-11-01",
                     "1991-03-01","2001-11-01","2009-06-01","2020-04-01")),
  stringsAsFactors = FALSE
)

# Build a 0/1 monthly recession indicator following the FRED/NBER USREC
# convention: a month is "recession" if it lies strictly after a peak and on
# or before the following trough, i.e. the interval (peak, trough]. Under this
# convention the 2020 recession spans Mar-Apr 2020 (two months), matching the
# official record.
make_recession_indicator <- function(dates) {
  rec <- integer(length(dates))
  for (i in seq_len(nrow(NBER_CYCLES))) {
    in_rec <- dates > NBER_CYCLES$peak[i] & dates <= NBER_CYCLES$trough[i]
    rec[in_rec] <- 1L
  }
  rec
}

# ---- Newey-West HAC standard errors ----------------------------------------
# Returns a list with coefficient estimates, HAC standard errors, t-stats and
# two-sided p-values for an OLS fit. Uses the 'sandwich'/'lmtest' packages when
# available; otherwise falls back to a self-contained Newey-West estimator so
# the pipeline never depends on package availability.
nw_lag <- function(n) floor(4 * (n / 100)^(2 / 9))   # Newey-West (1994) rule

hac_test <- function(model, lag = NULL) {
  X <- model.matrix(model)
  u <- residuals(model)
  n <- length(u)
  if (is.null(lag)) lag <- nw_lag(n)
  b <- coef(model)

  have_pkgs <- requireNamespace("sandwich", quietly = TRUE) &&
               requireNamespace("lmtest",   quietly = TRUE)
  if (have_pkgs) {
    V  <- sandwich::NeweyWest(model, lag = lag, prewhite = FALSE, adjust = TRUE)
    ct <- lmtest::coeftest(model, vcov. = V)
    return(data.frame(term = rownames(ct), est = ct[, 1], se = ct[, 2],
                      t = ct[, 3], p = ct[, 4], row.names = NULL))
  }

  # ---- Fallback: manual Newey-West HAC covariance --------------------------
  XtX_inv <- solve(crossprod(X))
  S <- crossprod(X * u)                                   # lag 0
  for (l in seq_len(lag)) {
    w  <- 1 - l / (lag + 1)                               # Bartlett kernel
    Xu <- X * u
    G  <- crossprod(Xu[(l + 1):n, , drop = FALSE], Xu[1:(n - l), , drop = FALSE])
    S  <- S + w * (G + t(G))
  }
  V  <- n / (n - ncol(X)) * XtX_inv %*% S %*% XtX_inv     # small-sample adj.
  se <- sqrt(diag(V))
  tstat <- b / se
  p  <- 2 * pt(-abs(tstat), df = n - ncol(X))
  data.frame(term = names(b), est = b, se = se, t = tstat, p = p,
             row.names = NULL)
}

# ---- Annualization & performance helpers (monthly % inputs) ----------------
ann_mean   <- function(x) mean(x, na.rm = TRUE) * 12              # %/yr
ann_sd     <- function(x) sd(x,   na.rm = TRUE) * sqrt(12)        # %/yr
ann_sharpe <- function(x) ann_mean(x) / ann_sd(x)                # excess inputs

# Maximum drawdown of a return series given in percent per month.
max_drawdown <- function(r_pct) {
  wealth <- cumprod(1 + r_pct / 100)
  peak   <- cummax(wealth)
  min(wealth / peak - 1) * 100                                    # most negative %
}

# ---- Formatting helpers ----------------------------------------------------
star <- function(p) ifelse(p < 0.01, "***", ifelse(p < 0.05, "**",
                    ifelse(p < 0.10, "*", "")))

fmt <- function(x, d = 2) formatC(x, format = "f", digits = d)
