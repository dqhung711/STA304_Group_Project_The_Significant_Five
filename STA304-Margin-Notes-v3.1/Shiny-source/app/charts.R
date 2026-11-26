# Base graphics for the local STA304 research demo.
# Source this file from the Shiny app; no data is read or written here.
# The fixed defaults cover the audited complete sample. An optional attribute
# face_axes = c(budget = ..., wtp = ...) can supply full-sample maxima; preserve
# it when subsetting so groups retain the same axes if the data changes.

face_limits <- function(d) {
  reference <- attr(d, "face_axes", exact = TRUE)
  budget <- 4000
  wtp <- 150
  if (!is.null(reference)) {
    if (is.finite(reference["budget"]) && reference["budget"] > 0)
      budget <- max(budget, unname(reference["budget"]))
    if (is.finite(reference["wtp"]) && reference["wtp"] > 0)
      wtp <- max(wtp, unname(reference["wtp"]))
  }
  budget_data <- d$budget_cad[is.finite(d$budget_cad) & d$budget_cad >= 0]
  wtp_data <- d$max_wtp_cad[is.finite(d$max_wtp_cad) & d$max_wtp_cad >= 0]
  # Expand rather than silently clipping any values outside the audited range.
  if (length(budget_data)) budget <- max(budget, max(budget_data))
  if (length(wtp_data)) wtp <- max(wtp, max(wtp_data))
  c(budget = budget, wtp = wtp)
}

face_scatter <- function(d, limit_y, palette) {
  stopifnot(is.data.frame(d), all(c("budget_cad", "max_wtp_cad") %in% names(d)),
            is.numeric(d$budget_cad), is.numeric(d$max_wtp_cad),
            all(c("ink", "muted", "line", "primary", "secondary", "paper") %in% names(palette)))
  limits <- face_limits(d)
  if (length(limit_y) != 1L || is.na(limit_y) || limit_y <= 0)
    stop("limit_y must be a positive number or Inf")
  ceiling_y <- if (is.infinite(limit_y)) limits[["wtp"]] else as.numeric(limit_y)
  x <- d$budget_cad
  y <- d$max_wtp_cad
  valid <- is.finite(x) & is.finite(y) & x >= 0 & y >= 0
  visible <- valid & y <= ceiling_y
  omitted <- sum(valid & !visible)
  invalid <- sum(!valid)
  dots <- if (any(visible)) {
    stats::aggregate(rep.int(1L, sum(visible)),
                     list(budget = x[visible], wtp = y[visible]), sum)
  } else data.frame(budget = numeric(), wtp = numeric(), x = integer())
  names(dots)[3] <- "count"

  old <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old), add = TRUE)
  compact <- grDevices::dev.size("in")[1] < 4.3
  graphics::par(bg = palette[["paper"]], fg = palette[["ink"]],
                col.axis = palette[["muted"]], col.lab = palette[["ink"]],
                family = "sans", mar = if (compact) c(4.0, 3.8, 3.8, 0.8) else c(4.8, 5.4, 4.1, 1.2),
                cex.axis = if (compact) 0.8 else 1,
                xaxs = "i", yaxs = "i", tcl = -0.22, las = 1)
  # Margins below zero and above endpoints keep points centered at the edges
  # visible, while tick labels show only meaningful nonnegative values.
  graphics::plot.new()
  graphics::plot.window(xlim = c(-0.22 * limits[["budget"]], 1.22 * limits[["budget"]]),
                        ylim = c(-0.15 * ceiling_y, 1.15 * ceiling_y), xaxs = "i", yaxs = "i")
  # Fewer ticks on phones leave enough room for both ends of the budget scale.
  xt <- pretty(c(0, limits[["budget"]]), n = if (graphics::par("pin")[1] < 3) 2 else 4)
  xt <- xt[xt >= 0 & xt <= limits[["budget"]]]
  yt <- pretty(c(0, ceiling_y), n = 4)
  yt <- yt[yt >= 0 & yt <= ceiling_y]
  if (!0 %in% xt) xt <- c(0, xt)
  if (!0 %in% yt) yt <- c(0, yt)
  graphics::abline(h = yt, col = palette[["line"]], lwd = 0.8)
  graphics::axis(1, at = xt, labels = FALSE,
                 col = palette[["line"]], col.ticks = palette[["line"]])
  graphics::mtext(format(xt, big.mark = ",", trim = TRUE), side = 1, at = xt,
                  line = 0.8, cex = if (compact) 0.73 else 0.95, col = palette[["muted"]])
  graphics::axis(2, at = yt, labels = format(yt, big.mark = ",", trim = TRUE),
                 col = palette[["line"]], col.ticks = palette[["line"]])
  graphics::title(xlab = if (compact) "Monthly budget (CAD)" else "Monthly discretionary budget (CAD)",
                  ylab = "Maximum offer (CAD)", line = if (compact) 2.3 else 3.0,
                  cex.lab = if (compact) 0.85 else 1)
  if (nrow(dots)) {
    # With a fixed coefficient, marker AREA is proportional to identical pairs.
    graphics::points(dots$budget, dots$wtp, pch = 21,
                     cex = 1.05 * sqrt(dots$count), lwd = 0.9,
                     col = palette[["primary"]],
                     bg = grDevices::adjustcolor(palette[["primary"]], alpha.f = 0.40))
  } else {
    graphics::text(limits[["budget"]] / 2, ceiling_y / 2,
                   "No valid observations in this view", col = palette[["muted"]], cex = 0.94)
  }
  # The legend occupies its own top margin, never the data area. Its symbols
  # use exactly the same pch, cex formula, outline and fill as the data points.
  usr <- graphics::par("usr")
  plot_left_in <- graphics::grconvertX(usr[1], from = "user", to = "inches")
  legend_step <- max(0.1, min(0.9, (graphics::par("pin")[1] - 0.75) / 2))
  legend_x <- graphics::grconvertX(plot_left_in + 0.24 + c(0, 1, 2) * legend_step,
                                  from = "inches", to = "user")
  top_in <- graphics::grconvertY(usr[4], from = "user", to = "inches")
  legend_y <- graphics::grconvertY(top_in + 0.34, from = "inches", to = "user")
  label_offset <- graphics::grconvertX(0.23, from = "inches", to = "user") -
                  graphics::grconvertX(0, from = "inches", to = "user")
  graphics::mtext("Responses per point", side = 3, line = 3.0,
                  adj = 0, col = palette[["muted"]], cex = 0.8)
  graphics::points(legend_x, rep(legend_y, 3), pch = 21,
                   cex = 1.05 * sqrt(c(1, 5, 15)), lwd = 0.9,
                   col = palette[["primary"]],
                   bg = grDevices::adjustcolor(palette[["primary"]], alpha.f = 0.40),
                   xpd = NA)
  graphics::text(legend_x + label_offset, rep(legend_y, 3), labels = c("1", "5", "15"),
                 adj = c(0, 0.5), col = palette[["muted"]], cex = 0.8, xpd = NA)
  invisible(list(visible = sum(visible), omitted_above_limit = omitted,
                 missing_or_invalid = invalid, x_axis_max = limits[["budget"]],
                 y_axis_max = ceiling_y, distinct_pairs = nrow(dots)))
}

face_ratings <- function(d, item, palette) {
  stopifnot(is.data.frame(d), length(item) == 1L, is.character(item), item %in% names(d),
            is.numeric(d[[item]]),
            all(c("ink", "muted", "line", "primary", "secondary", "paper") %in% names(palette)))
  x <- d[[item]]
  valid <- is.finite(x) & x >= 1 & x <= 10 & x == floor(x)
  counts <- tabulate(as.integer(x[valid]), nbins = 10L)
  n <- sum(valid)
  old <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old), add = TRUE)
  graphics::par(bg = palette[["paper"]], fg = palette[["ink"]],
                col.axis = palette[["muted"]], col.lab = palette[["ink"]],
                family = "sans", mar = c(4.5, 4.4, 2.1, 1.2),
                xaxs = "i", yaxs = "i", tcl = -0.22, las = 1)
  y_top <- max(2, max(counts)) * 1.22
  graphics::plot.new()
  graphics::plot.window(xlim = c(0.35, 10.65), ylim = c(0, y_top), xaxs = "i", yaxs = "i")
  y_ticks <- pretty(c(0, max(1, counts)), n = 4)
  y_ticks <- y_ticks[y_ticks >= 0 & y_ticks <= y_top & y_ticks == floor(y_ticks)]
  if (!length(y_ticks)) y_ticks <- 0:1
  graphics::abline(h = y_ticks, col = palette[["line"]], lwd = 0.8)
  graphics::rect(seq_len(10) - 0.32, 0, seq_len(10) + 0.32, counts,
                 col = palette[["primary"]], border = NA)
  # axis() can silently suppress the last label at narrow widths. Draw all
  # ten labels explicitly instead; leave both endpoints inside the plot range.
  graphics::axis(1, at = 1:10, labels = FALSE,
                 col = palette[["line"]], col.ticks = palette[["line"]])
  graphics::mtext(as.character(1:10), side = 1, at = 1:10, line = 0.65,
                  col = palette[["muted"]], cex = 0.8)
  graphics::axis(2, at = y_ticks, col = palette[["line"]], col.ticks = palette[["line"]])
  graphics::title(xlab = "Response (1-10)", ylab = "Number of respondents", line = 3.0)
  graphics::mtext(sprintf("Valid responses: %s", format(n, big.mark = ",")),
                  side = 3, line = 0.35, adj = 0, col = palette[["muted"]], cex = 0.88)
  if (!n) graphics::text(5.5, y_top / 2, "No valid responses in this view",
                         col = palette[["muted"]], cex = 0.94)
  invisible(list(valid = n, missing_or_invalid = sum(!valid), counts = counts))
}
