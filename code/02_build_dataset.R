# ============================================================================
# 02_build_dataset.R  —  Parse, merge, and assemble the analysis panel
# ============================================================================
# Reads the raw Ken French CSVs, parses the monthly blocks, merges the
# 5 factors with the momentum factor, attaches the NBER recession indicator,
# and writes a tidy monthly panel to data/processed/{factors.rds, factors.csv}.
# ============================================================================

source("code/00_helpers.R")
dir.create(PATHS$processed, recursive = TRUE, showWarnings = FALSE)

# ---- Generic parser for a Ken French monthly CSV ---------------------------
# French files have several metadata lines, then a header row whose first field
# is blank (",Mkt-RF,SMB,..."), then monthly rows keyed by YYYYMM, then an
# annual section keyed by 4-digit years. We keep only the 6-digit monthly rows.
read_french_monthly <- function(path) {
  lines   <- readLines(path, warn = FALSE)
  hdr_idx <- grep("^\\s*,", lines)[1]                 # first line starting with a comma
  hdr     <- trimws(strsplit(trimws(lines[hdr_idx]), ",")[[1]])
  varnames <- hdr[-1]                                  # drop empty first field

  is_month <- grepl("^\\s*[0-9]{6}\\s*,", lines)       # YYYYMM rows only (annual = 4 digits)
  idx      <- which(is_month & seq_along(lines) > hdr_idx)
  mat      <- do.call(rbind, strsplit(trimws(lines[idx]), ","))

  df <- data.frame(date = as.Date(paste0(substr(mat[, 1], 1, 4), "-",
                                          substr(mat[, 1], 5, 6), "-01")),
                   stringsAsFactors = FALSE)
  for (j in seq_along(varnames)) df[[varnames[j]]] <- as.numeric(trimws(mat[, j + 1]))
  df
}

# ---- Locate raw files (names are stable in the French library) -------------
ff5_csv <- file.path(PATHS$raw, "F-F_Research_Data_5_Factors_2x3.csv")
mom_csv <- list.files(PATHS$raw, pattern = "Momentum.*\\.csv$", full.names = TRUE)[1]
stopifnot(file.exists(ff5_csv), !is.na(mom_csv))

ff5 <- read_french_monthly(ff5_csv)
mom <- read_french_monthly(mom_csv)
names(mom)[names(mom) %in% c("Mom", "WML", "UMD")] <- "MOM"   # normalize label

# French CSVs sometimes use -99.99 / -999 as missing sentinels.
sentinel_to_na <- function(df) {
  num <- vapply(df, is.numeric, logical(1))
  df[num] <- lapply(df[num], function(x) { x[x <= -99.99] <- NA; x })
  df
}
ff5 <- sentinel_to_na(ff5); mom <- sentinel_to_na(mom)

# ---- Merge and assemble ----------------------------------------------------
panel <- merge(ff5, mom[, c("date", "MOM")], by = "date", all = FALSE)  # inner join
panel <- panel[order(panel$date), ]
names(panel)[names(panel) == "Mkt-RF"] <- "MKT.RF"

# Drop any trailing months with missing factor data (keeps a balanced panel).
factor_cols <- c("MKT.RF", "SMB", "HML", "RMW", "CMA", "MOM")
panel <- panel[stats::complete.cases(panel[, factor_cols]), ]

# Attach NBER recession indicator and a few convenience columns.
panel$recession <- make_recession_indicator(panel$date)
panel$state     <- ifelse(panel$recession == 1, "Recession", "Expansion")
panel$year      <- as.integer(format(panel$date, "%Y"))

# ---- Save ------------------------------------------------------------------
saveRDS(panel, file.path(PATHS$processed, "factors.rds"))
write.csv(panel, file.path(PATHS$processed, "factors.csv"), row.names = FALSE)

# ---- Console summary -------------------------------------------------------
rng <- range(panel$date)
message(sprintf("Built panel: %d months, %s to %s (%d recession months, %.1f%%).",
                nrow(panel), format(rng[1], "%Y-%m"), format(rng[2], "%Y-%m"),
                sum(panel$recession), 100 * mean(panel$recession)))
message("Factors: ", paste(factor_cols, collapse = ", "))
