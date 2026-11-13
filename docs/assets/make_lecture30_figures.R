#Checks the numbers on the Week 12 Lecture 30 deck (11/13/2026): the chi-square goodness-of-fit test, the
#test of independence and the test for homogeneity (OpenStax Chapter 11). The categorical-data slides carried
#over from Lecture 29 are checked in make_lecture29_figures.R. It draws:
#
#  l30_pew.png           chi-square(3) curve: X^2 = 147.77 is far beyond the right edge
#  l30_absences.png      chi-square(4) curve, upper tail beyond X^2 = 3 shaded (employee absences)
#  l30_volunteers.png    chi-square(4) curve, upper tail beyond X^2 = 12.99 shaded (volunteers and hours)
#  l30_living.png        chi-square(3) curve, upper tail beyond X^2 = 10.13 shaded (living arrangements)
#  l30_figure_sizes.csv  the size, in points, each figure is drawn at
#
#The same numbers are asserted on the Week 12 notes page (lecturemenu12.Rmd, chunk lecture30-setup).
#
#P-VALUES. The course brackets a chi-square P-value with the site's chi-square table (misc/chisq_table.Rmd,
#upper-tail columns 0.25, 0.2, 0.15, 0.125, 0.1, 0.075, 0.05, 0.025, 0.005, 0.0025, 0.0005); the exact
#value from software is quoted beside it.
#
#TYPE SIZE. Same rule as the other make_lectureN_figures.R scripts: nothing on a slide is under 16pt, so
#each figure is drawn at exactly its on-slide size (points / 72 inches) and every text size goes through
#tsz() or el(), which refuse anything smaller.

library(ggplot2)

this_file <- tryCatch(normalizePath(sys.frame(1)$ofile, winslash = "/"),
                      error = function(e) NA_character_)
if (is.na(this_file)) {
  args <- commandArgs(trailingOnly = FALSE)
  m <- grep("^--file=", args, value = TRUE)
  this_file <- if (length(m)) normalizePath(sub("^--file=", "", m[[1]]),
                                            winslash = "/") else NA_character_
}
if (is.na(this_file)) stop("Could not determine the script location; run it with Rscript.")
assets_dir <- dirname(this_file)

#---- helpers for the hand method
tails <- c(0.25, 0.2, 0.15, 0.125, 0.1, 0.075, 0.05, 0.025, 0.005, 0.0025, 0.0005)
crit <- function(df) round(qchisq(1 - tails, df), 3)           # one row of the chi-square table
#the two table columns that bracket x: c(larger tail, smaller tail); NA = beyond the table
bracket <- function(x, df) {
  cr <- crit(df)
  c(if (any(cr <= x)) tails[max(which(cr <= x))] else NA, if (any(cr > x)) tails[min(which(cr > x))] else NA)
}
pv <- function(x, df) pchisq(x, df, lower.tail = FALSE)
gof <- function(O, p) { E <- sum(O) * p; list(E = E, terms = (O - E)^2 / E, X2 = sum((O - E)^2 / E), df = length(O) - 1) }
indep <- function(M) {
  E <- outer(rowSums(M), colSums(M)) / sum(M)
  list(E = E, terms = (M - E)^2 / E, X2 = sum((M - E)^2 / E), df = (nrow(M) - 1) * (ncol(M) - 1))
}

#---- goodness of fit
#Pew (Lecture 29): H0 all four answers equally common, X^2 = 147.77, df = 3 -> beyond the 0.0005 column
stopifnot(crit(3)[11] == 17.730, 147.77 > 17.730, pv(147.77, 3) < 1e-30)

#OpenStax Example 11.2: the weekday with the most employee absences, 60 managers, H0 uniform (p = 1/5 each)
ab <- gof(c(Mon = 15, Tue = 12, Wed = 9, Thu = 9, Fri = 15), rep(0.2, 5))
stopifnot(all(ab$E == 12), identical(unname(ab$terms), c(0.75, 0, 0.75, 0.75, 0.75)), ab$X2 == 3, ab$df == 4,
          crit(4)[1] == 5.385, is.na(bracket(3, 4)[1]), round(pv(3, 4), 4) == 0.5578)

#OpenStax Example 11.3: streaming services, 600 far-western families vs the national percents, alpha = 0.01
st <- gof(c(66, 119, 340, 60, 15), c(0.10, 0.16, 0.55, 0.11, 0.08))
stopifnot(identical(st$E, c(60, 96, 330, 66, 48)), identical(round(st$terms, 2), c(0.60, 5.51, 0.30, 0.55, 22.69)),
          round(st$X2, 2) == 29.65, st$df == 4, crit(4)[11] == 19.997, round(pv(st$X2, 4), 6) == 0.000006)

#OpenStax Example 11.1: absenteeism; 12+ has E = 2 < 5, so it is combined with 9-11 (E = 8, O = 5). The book
#stops at df = 3; the test itself is ours.
at <- gof(c(35, 40, 20, 5), c(50, 30, 12, 8) / 100)
stopifnot(min(c(50, 30, 12, 6, 2)) == 2, identical(at$E, c(50, 30, 12, 8)), at$df == 3,
          identical(round(at$terms, 3), c(4.500, 3.333, 5.333, 1.125)), round(at$X2, 2) == 14.29,
          identical(bracket(at$X2, 3), c(0.005, 0.0025)), round(pv(at$X2, 3), 4) == 0.0025)

#---- independence
#OpenStax Example 11.5: E = (row total)(column total)/n for speeding x cell phone
stopifnot(round(70 * 305 / 755, 1) == 28.3)

#OpenStax Example 11.6: volunteers and hours (introduced in Lecture 29)
vol <- indep(matrix(c(111, 96, 48, 96, 133, 61, 91, 150, 53), 3, byrow = TRUE))
stopifnot(identical(round(vol$E, 2), matrix(c(90.57, 115.19, 49.24, 103, 131, 56, 104.42, 132.81, 56.77), 3, byrow = TRUE)),
          identical(round(vol$terms, 2), matrix(c(4.61, 3.20, 0.03, 0.48, 0.03, 0.45, 1.73, 2.23, 0.25), 3, byrow = TRUE)),
          round(vol$X2, 2) == 12.99, vol$df == 4, identical(bracket(vol$X2, 4), c(0.025, 0.005)),
          round(pv(vol$X2, 4), 4) == 0.0113, min(vol$E) > 5)

#OpenStax Example 11.7: anxiety x need to succeed, n = 400 (the book gives two expected counts; the test is ours)
anx <- indep(matrix(c(35, 42, 53, 15, 10, 18, 48, 63, 33, 31, 4, 5, 11, 15, 17), 3, byrow = TRUE))
stopifnot(round(anx$E[1, 1], 2) == 22.09, round(anx$E[3, 4], 2) == 8.19, round(min(anx$E), 2) == 7.41,
          round(anx$X2, 2) == 48.42, anx$df == 8, crit(8)[11] == 27.868, pv(anx$X2, 8) < 0.0005)

#---- homogeneity
#OpenStax Example 11.8: living arrangements, 250 men and 300 women
liv <- indep(matrix(c(72, 84, 49, 45, 91, 86, 88, 35), 2, byrow = TRUE))
stopifnot(identical(round(liv$E, 2), matrix(c(74.09, 77.27, 62.27, 36.36, 88.91, 92.73, 74.73, 43.64), 2, byrow = TRUE)),
          round(liv$X2, 2) == 10.13, liv$df == 3, identical(bracket(liv$X2, 3), c(0.025, 0.005)),
          round(pv(liv$X2, 3), 4) == 0.0175)

#OpenStax Example 11.9: voter preference before (430) and after (636) an earthquake
qk <- indep(matrix(c(167, 128, 135, 214, 197, 225), 2, byrow = TRUE))
stopifnot(round(qk$X2, 2) == 3.26, qk$df == 2, identical(bracket(qk$X2, 2), c(0.2, 0.15)),
          round(pv(qk$X2, 2), 4) == 0.1959, round(min(qk$E), 2) == 131.1)

#---- type size helpers: points in, refuse anything under 16pt
MIN_PT <- 16
tsz <- function(pt) { stopifnot(pt >= MIN_PT); pt / .pt }     # geom text size (mm)
el <- function(pt, ...) { stopifnot(pt >= MIN_PT); element_text(size = pt, ...) }

NAVY <- "#1F3864"; SHADE <- "#8FAADC"; RED <- "#C00000"
bare <- theme_classic() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank(),
        axis.text.x = el(16, colour = 'black'), axis.title.x = el(16), plot.margin = margin(8, 14, 4, 8))
#parse = TRUE: s is a plotmath expression (subscripts typeset, as in the slides' equations)
lab <- function(x, y, s, col, hj, face = 'plain', pt = 16, parse = FALSE)
  annotate('label', x = x, y = y, label = s, colour = col, hjust = hj, size = tsz(pt), fontface = face, parse = parse,
           fill = 'white', border.colour = NA, label.padding = unit(0.1, 'lines'))
arr <- function(x, xend, y, yend, col)
  annotate('segment', x = x, xend = xend, y = y, yend = yend, colour = col,
           arrow = arrow(length = unit(0.2, 'cm')))

sizes <- data.frame(file = character(), width_pt = numeric(), height_pt = numeric())
save_fig <- function(g, file, wpt, hpt) {
  ggsave(file.path(assets_dir, file), g, width = wpt / 72, height = hpt / 72, dpi = 200, bg = 'white')
  sizes[nrow(sizes) + 1, ] <<- list(file, wpt, hpt)
}

#a chi-square(df) curve with the upper tail beyond x_obs shaded. head = the "if H0 is true" label (plotmath),
#tail = the label for the shaded area, note = the table bracket, xlab = the axis title
tail_fig <- function(df, x_obs, xmax, head, tail, note, xlab, breaks, tail_at, note_at) {
  gx <- seq(0.02, xmax, length.out = 900)
  d <- data.frame(x = gx, y = dchisq(gx, df)); top <- max(d$y)
  ggplot(d, aes(x, y)) +
    geom_area(fill = SHADE, alpha = 0.5) +
    geom_area(data = subset(d, x >= x_obs), fill = RED, alpha = 0.85) +
    geom_line(colour = NAVY, linewidth = 1.1) +
    geom_vline(xintercept = x_obs, colour = NAVY, linetype = 'dashed') +
    lab(xmax * 0.98, top * 1.12, head, NAVY, 1, 'bold', parse = TRUE) +
    lab(tail_at[1], tail_at[2] * top, tail, RED, 0, 'bold') +
    arr(tail_at[1] + 0.3, x_obs + (xmax - x_obs) * 0.12, tail_at[2] * top * 0.9, top * 0.04, RED) +
    lab(note_at[1], note_at[2] * top, note, 'black', 1, 'bold') +
    scale_x_continuous(breaks = breaks, labels = function(b) sub('\\.?0+$', '', sprintf('%.3f', b))) +
    coord_cartesian(xlim = c(0, xmax), ylim = c(0, top * 1.2), expand = FALSE) +
    labs(x = xlab, y = NULL) + bare
}

#---- Pew: chi-square(3); 147.77 is off the chart (the table's last column, 17.73, is already far out)
gx <- seq(0.02, 22, length.out = 900)
d3 <- data.frame(x = gx, y = dchisq(gx, 3)); top3 <- max(d3$y)
gp <- ggplot(d3, aes(x, y)) +
  geom_area(fill = SHADE, alpha = 0.5) +
  geom_line(colour = NAVY, linewidth = 1.1) +
  geom_vline(xintercept = 17.73, colour = RED, linetype = 'dashed') +
  lab(21.6, top3 * 1.12, 'bold("if"~H[0]~"is true:"~~X^2~"~"~chi^2*(3))', NAVY, 1, 'bold', parse = TRUE) +
  lab(17.4, top3 * 0.62, 'table: 17.73 cuts off 0.0005', RED, 1, 'bold') +
  lab(21.6, top3 * 0.32, 'ours: 147.77, far off this edge', 'black', 1, 'bold') +
  arr(18.6, 21.8, top3 * 0.2, top3 * 0.2, 'black') +
  scale_x_continuous(breaks = c(0, 3, 5, 10, 15, 17.73, 20), labels = c('0', '3', '5', '10', '15', '17.73', '20')) +
  coord_cartesian(xlim = c(0, 22), ylim = c(0, top3 * 1.2), expand = FALSE) +
  labs(x = expression(X^2~'value (mean of'~chi^2*(3)~'is 3)'), y = NULL) + bare
save_fig(gp, 'l30_pew.png', 840, 270)

#---- employee absences: chi-square(4), X^2 = 3, P-value = 0.5578 (software), compared with alpha = 0.05 only
ga <- tail_fig(4, 3, 16, 'bold("if"~H[0]~"is true:"~~X^2~"~"~chi^2*(4))', 'P(χ² ≥ 3)',
               'P-value = 0.5578 > α = 0.05', expression(X^2~'(ours: 3)'), c(0, 3, 5, 10, 15),
               c(7, 0.55), c(15.7, 0.3))
save_fig(ga, 'l30_absences.png', 840, 270)

#---- volunteers: chi-square(4), X^2 = 12.99, 0.005 < P < 0.025
gv <- tail_fig(4, 12.99, 18, 'bold("if"~H[0]~"is true:"~~X^2~"~"~chi^2*(4))', 'P(χ² ≥ 12.99)',
               'table: 11.143 < 12.99 < 14.860', expression(X^2~'(ours: 12.99)'), c(0, 4, 8, 11.143, 12.99, 14.86),
               c(6.4, 0.45), c(12.7, 0.8))
save_fig(gv, 'l30_volunteers.png', 840, 270)

#---- living arrangements: chi-square(3), X^2 = 10.13, 0.005 < P < 0.025
gl <- tail_fig(3, 10.13, 15, 'bold("if"~H[0]~"is true:"~~X^2~"~"~chi^2*(3))', 'P(χ² ≥ 10.13)',
               'table: 9.348 < 10.13 < 12.838', expression(X^2~'(ours: 10.13)'), c(0, 3, 6, 9.348, 12.838),
               c(4.6, 0.45), c(9.85, 0.8))
save_fig(gl, 'l30_living.png', 840, 270)

write.csv(sizes, file.path(assets_dir, 'l30_figure_sizes.csv'), row.names = FALSE)
print(sizes)
