# ============================================================================
# run_all.R  —  Reproduce the entire data pipeline end to end.
# Usage:  Rscript run_all.R        (from the repository root)
# Set FORCE_DOWNLOAD=1 to re-fetch the raw Ken French files.
# ============================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))
if (dir.exists("~/R/library")) .libPaths(c("~/R/library", .libPaths()))

stopifnot(file.exists("code/00_helpers.R"))   # must run from repo root

scripts <- c(
  "code/01_download_data.R",
  "code/02_build_dataset.R",
  "code/03_analysis.R",
  "code/04_tables.R",
  "code/05_figures.R"
)
for (s in scripts) {
  message("\n========== ", s, " ==========")
  source(s, echo = FALSE)
}
message("\nPipeline complete. Tables in output/tables/, figures in output/figures/.")
message("Compile the paper with:  make paper   (or see README.md)")
