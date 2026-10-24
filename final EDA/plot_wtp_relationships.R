# All calculations and figures use R. Source this file to load the plotting function.
# As a standalone script: Rscript plot_wtp_relationships.R [input.csv] [output_folder]
plot_wtp_relationships <- function(d, panels = 1:6, footer = TRUE) {
vars <- c("budget_cad", "embarrassed_easily", "self_conscious",
          "judgment_worry", "general_embarrassment", "communication_discomfort")
labels <- c("Monthly discretionary budget ($)", "Easily embarrassed",
            "Self-conscious after mistakes", "Worry about others' judgment (1-10)",
            "General embarrassment (1-10)", "Communication discomfort (1-10)")
stopifnot(all(c(vars, "max_wtp_cad") %in% names(d)))
stopifnot(nrow(d) > 1, !anyNA(d[c(vars, "max_wtp_cad")]))
stopifnot(all(vapply(d[c(vars, "max_wtp_cad")], is.numeric, logical(1))))
stopifnot(all(d$max_wtp_cad >= 0), all(d$budget_cad >= 0))
for (v in vars[2:3]) stopifnot(all(d[[v]] %in% 0:1))
for (v in vars[4:6]) stopifnot(all(d[[v]] %in% 1:10))

# asinh keeps zero values and every high response visible, without adding a constant.
# The axis labels show original dollar values. No model is fitted on this display scale.
y <- asinh(d$max_wtp_cad)
ticks <- sort(unique(c(0, 1, 2, 5, 10, 20, 50, 100, 150, max(d$max_wtp_cad))))
ticks <- ticks[ticks <= max(d$max_wtp_cad)]
draw_panel <- function(k) {
  set.seed(304 + k)
  x <- d[[vars[k]]]
  binary <- k %in% 2:3
  xlim <- if (binary) c(.5, 2.5) else if (k >= 4) c(.65, 10.35) else range(x)
  plot(NA, xlim = xlim, ylim = c(-.10, max(y) + .12),
       xlab = labels[k], ylab = "Maximum WTP (asinh spacing)",
       main = paste0(LETTERS[k], ". ", strsplit(labels[k], " (", fixed = TRUE)[[1]][1]),
       axes = FALSE, bty = "l")
  abline(h = asinh(ticks), col = "#E7EBEE", lwd = .7)
  axis(2, at = asinh(ticks), labels = paste0("$", format(ticks, trim = TRUE)), las = 1)
  if (binary) {
    axis(1, at = 1:2, labels = c("No", "Yes"))
    # Compute hinges, whiskers and outliers in original dollars first.
    boxes <- boxplot(d$max_wtp_cad ~ factor(x, levels = 0:1), plot = FALSE)
    boxes$stats <- asinh(boxes$stats)
    boxes$out <- asinh(boxes$out)
    bxp(boxes, add = TRUE, axes = FALSE, outline = FALSE,
        col = c("#DEE8EF", "#DDEEEA"), border = "#294C60", boxwex = .45)
    points(x + 1 + runif(length(x), -.17, .17), y,
           pch = 16, cex = .65, col = adjustcolor("#294C60", alpha.f = .55))
  } else {
    if (k >= 4) axis(1, at = 1:10) else axis(1, at = pretty(x), labels = paste0("$", format(pretty(x), trim = TRUE)))
    shown_x <- if (k >= 4) x + runif(length(x), -.15, .15) else x
    points(shown_x, y, pch = 16, cex = .7,
           col = adjustcolor("#294C60", alpha.f = .55))
  }
  box(bty = "l", col = "#596571")
}


  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(mfrow = if (length(panels) == 6) c(2, 3) else c(1, 1),
      mar = c(4.7, 4.7, 3, 1), oma = if (footer) c(3.4, 0, 0, 0) else c(0, 0, 0, 0),
      mgp = c(3, .8, 0), cex = .85,
      col.lab = "#294C60", col.main = "#294C60")
  for (k in panels) draw_panel(k)
  if (footer) {
    note <- sprintf("All %d respondents retained: %d zero responses; maximum WTP $%s.",
                    nrow(d), sum(d$max_wtp_cad == 0),
                    format(max(d$max_wtp_cad), trim = TRUE))
    mtext(note, side = 1, outer = TRUE, line = 1, cex = .8)
    mtext("Asinh spacing; ticks show original dollars. Horizontal jitter is display-only.",
          side = 1, outer = TRUE, line = 2.2, cex = .8)
  }
  invisible(NULL)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  input <- if (length(args)) args[1] else "The_Significant_Five_dataset.csv"
  out <- if (length(args) > 1) args[2] else "output/wtp_relationships"
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  d <- read.csv(input, colClasses = c(studentID = "character"))
  png(file.path(out, "wtp_six_relationships.png"),
      width = 3000, height = 1850, res = 240)
  plot_wtp_relationships(d)
  dev.off()
  variables <- c("budget_cad", "embarrassed_easily", "self_conscious",
                 "judgment_worry", "general_embarrassment", "communication_discomfort")
  for (k in seq_along(variables)) {
    png(file.path(out, paste0(variables[k], "_vs_wtp.png")),
        width = 1600, height = 1200, res = 200)
    plot_wtp_relationships(d, panels = k, footer = FALSE)
    dev.off()
  }
  cat("Created one combined plot and six individual plots in:", normalizePath(out), "\n")
}
