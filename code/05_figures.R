# ============================================================================
# 05_figures.R  —  Emit publication figures as PDF (base graphics, no deps)
# ============================================================================
#   fig_cumulative.pdf  cumulative factor wealth with NBER recession shading
#   fig_states.pdf      annualized premium by business-cycle state
#   fig_event.pdf       event-time cumulative returns around peaks and troughs
#   fig_bear.pdf        annualized premium under a real-time bear signal
# ============================================================================

source("code/00_helpers.R")
panel <- readRDS(file.path(PATHS$processed, "panel_derived.rds"))
r     <- readRDS(file.path(PATHS$processed, "results.rds"))
dir.create(PATHS$figures, recursive = TRUE, showWarnings = FALSE)

FACTORS <- r$factors
FLABEL  <- r$flabel
PAL <- c(MKT.RF="#1b1b1b", SMB="#7570b3", HML="#1f78b4",
         RMW="#33a02c", CMA="#e6550d", MOM="#e7298a")
EXP_COL <- "#9ecae1"; REC_COL <- "#fb6a4a"
recch <- function() {                      # NBER intervals within the sample
  d <- NBER_CYCLES
  d[d$trough >= min(panel$date) & d$peak <= max(panel$date), ]
}
shade_recessions <- function(ybot, ytop) {
  rc <- recch()
  for (i in seq_len(nrow(rc)))
    rect(rc$peak[i], ybot, rc$trough[i], ytop,
         col = "#e9e9e9", border = NA)
}

# ---------------------------------------------------------------------------
# Figure 1 — Cumulative wealth (log scale) with recession shading
# ---------------------------------------------------------------------------
pdf(file.path(PATHS$figures, "fig_cumulative.pdf"), width = 9, height = 5.6,
    pointsize = 11)
par(mfrow = c(2, 3), mar = c(2.6, 3.0, 2.2, 0.8), mgp = c(1.8, 0.6, 0),
    cex.main = 1.1, las = 1)
for (f in FACTORS) {
  w <- cumprod(1 + panel[[f]] / 100)
  plot(panel$date, w, type = "n", log = "y", xlab = "", ylab = "Cumulative $1",
       main = FLABEL[f], yaxt = "n")
  axis(2, at = axTicks(2), labels = formatC(axTicks(2), format = "g"))
  shade_recessions(1e-6, max(w) * 10)
  lines(panel$date, w, col = PAL[f], lwd = 1.6)
  abline(h = 1, col = "grey60", lty = 3)
  box()
}
dev.off()

# ---------------------------------------------------------------------------
# Figure 2 — Annualized premium by NBER business-cycle state
# ---------------------------------------------------------------------------
M <- rbind(Expansion = r$t2$exp_ann, Recession = r$t2$rec_ann)
colnames(M) <- r$t2$factor
pdf(file.path(PATHS$figures, "fig_states.pdf"), width = 7.2, height = 4.4,
    pointsize = 11)
par(mar = c(3.0, 3.6, 1.2, 0.8), mgp = c(2.2, 0.6, 0), las = 1)
bp <- barplot(M, beside = TRUE, col = c(EXP_COL, REC_COL), border = "grey30",
              ylab = "Annualized premium (%)", ylim = range(0, M) + c(-2, 2),
              legend.text = TRUE,
              args.legend = list(x = "topright", bty = "n", cex = 0.95))
abline(h = 0, col = "grey30")
dev.off()

# ---------------------------------------------------------------------------
# Figure 3 — Event-time cumulative returns around NBER peaks and troughs
# ---------------------------------------------------------------------------
sel <- c("MKT.RF", "CMA", "RMW", "MOM")
H   <- (nrow(r$event_trough) - 1) / 2
hh  <- -H:H
draw_event <- function(mat, ttl, marker) {
  matplot(hh, mat[, sel], type = "l", lty = 1, lwd = 2,
          col = PAL[sel], xlab = "Months relative to event", ylab = "Cumulative return (%)",
          main = ttl)
  abline(v = 0, col = "grey50", lty = 2); abline(h = 0, col = "grey70", lty = 3)
  text(0, par("usr")[4], marker, pos = 1, cex = 0.85, col = "grey40")
}
pdf(file.path(PATHS$figures, "fig_event.pdf"), width = 9, height = 4.2,
    pointsize = 11)
par(mfrow = c(1, 2), mar = c(3.2, 3.4, 2.0, 0.8), mgp = c(2.0, 0.6, 0), las = 1)
draw_event(r$event_peak,   "Around NBER peaks",   "peak")
draw_event(r$event_trough, "Around NBER troughs", "trough")
legend("topleft", legend = FLABEL[sel], col = PAL[sel], lwd = 2, bty = "n", cex = 0.85)
dev.off()

# ---------------------------------------------------------------------------
# Figure 4 — Annualized premium under a real-time bear-market signal
# ---------------------------------------------------------------------------
B <- rbind(Bull = r$t4$bull_ann, Bear = r$t4$bear_ann)
colnames(B) <- r$t4$factor
pdf(file.path(PATHS$figures, "fig_bear.pdf"), width = 7.2, height = 4.4,
    pointsize = 11)
par(mar = c(3.0, 3.6, 1.2, 0.8), mgp = c(2.2, 0.6, 0), las = 1)
barplot(B, beside = TRUE, col = c("#a1d99b", "#c994c7"), border = "grey30",
        ylab = "Annualized premium (%)", ylim = range(0, B) + c(-2, 2),
        legend.text = TRUE,
        args.legend = list(x = "topright", bty = "n", cex = 0.95))
abline(h = 0, col = "grey30")
dev.off()

message("Figures written: fig_cumulative, fig_states, fig_event, fig_bear (.pdf) in ",
        PATHS$figures, "/")
