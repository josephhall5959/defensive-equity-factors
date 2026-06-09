# ============================================================================
# 01_download_data.R  —  Download raw data from the Ken French Data Library
# ============================================================================
# Downloads the monthly Fama-French 5-factor file and the momentum factor file
# (CSV zips) into data/raw/. Idempotent: existing files are reused unless the
# FORCE_DOWNLOAD environment variable is set. The only external dependency is
# the Ken French Data Library, which is publicly accessible without credentials.
# ============================================================================

source("code/00_helpers.R")
dir.create(PATHS$raw, recursive = TRUE, showWarnings = FALSE)

KF_BASE <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/"
DOWNLOADS <- list(
  ff5 = list(url = paste0(KF_BASE, "F-F_Research_Data_5_Factors_2x3_CSV.zip"),
             zip = file.path(PATHS$raw, "F-F_5_Factors.zip")),
  mom = list(url = paste0(KF_BASE, "F-F_Momentum_Factor_CSV.zip"),
             zip = file.path(PATHS$raw, "F-F_Momentum.zip"))
)

force_dl <- nzchar(Sys.getenv("FORCE_DOWNLOAD"))

fetch <- function(item) {
  if (file.exists(item$zip) && !force_dl) {
    message("  [cached] ", basename(item$zip))
  } else {
    message("  [get]    ", item$url)
    # A browser-like user agent avoids occasional 403s from the host.
    old <- options(HTTPUserAgent = "Mozilla/5.0 (X11; Linux x86_64) R-download")
    on.exit(options(old), add = TRUE)
    utils::download.file(item$url, item$zip, mode = "wb", quiet = TRUE)
  }
  files <- utils::unzip(item$zip, exdir = PATHS$raw)
  message("    -> ", paste(basename(files), collapse = ", "))
  invisible(files)
}

message("Downloading Ken French Data Library files ...")
invisible(lapply(DOWNLOADS, fetch))
message("Done. Raw files in ", PATHS$raw, "/")
