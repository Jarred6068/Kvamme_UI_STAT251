#Checks the numbers on the Week 11 Lecture 27 deck (11/6/2026): a power calculation in steps, and
#the two-sample z-test for two proportions. It draws:
#
#  l27_power_rebar.png   the rebar test (adapted from Penn State STAT 415, Example 10.4): H0 mu = 170
#                        against one possible truth mu = 175, n = 25, alpha = 0.05, upper-tailed
#  l27_hives.png         the hives medications (OpenStax Example 10.8): z_obs = 1.47 on N(0, 1), both tails
#  l27_figure_sizes.csv  the size, in points, each figure is drawn at
#
#P-values are hand-method values (z-table, four decimals). The same numbers are asserted on the
#Week 11 notes page (lecturemenu11.Rmd).
#
#TYPE SIZE. Same rule as the other make_lectureN_figures.R scripts: nothing on a slide is under
#16pt, so each figure is drawn at exactly its on-slide size (points / 72 inches) and every text
#size goes through tsz() or el(), which refuse anything smaller.

library(ggplot2)

fmt <- function(x, d = 2) formatC(x, format = 'f', digits = d)
zt <- function(z) round(pnorm(z), 4)     # a z-table lookup, four decimals
near <- function(a, b, tol = 5e-4) abs(a - b) < tol

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

#---- numbers (the same checks as lecturemenu11.Rmd)
#the rebar: sigma = 10, H0: mu = 170 vs HA: mu > 170, alpha = 0.05 (Z_0.95 = 1.645), truth mu = 175.
#z* = -0.855 exactly, which the hand method rounds to -0.86 (table power 0.8051; exact 0.8038)
stopifnot(round(qnorm(0.95), 3) == 1.645,
          10 / sqrt(25) == 2, round(170 + 1.645 * 2, 2) == 173.29, near((173.29 - 175) / 2, -0.855, 1e-9),
          zt(-0.86) == 0.1949, round(1 - zt(-0.86), 4) == 0.8051, round(1 - pnorm(qnorm(0.95) - 2.5), 4) == 0.8038,
          10 / sqrt(64) == 1.25, round(170 + 1.645 * 1.25, 2) == 172.06, round((172.06 - 175) / 1.25, 2) == -2.35,
          zt(-2.35) == 0.0094, round(1 - zt(-2.35), 4) == 0.9906)
#two proportions: pooled p-hat, pooled SE and z_obs
tp <- function(x1, n1, x2, n2) {
  pp <- (x1 + x2) / (n1 + n2); se <- sqrt(pp * (1 - pp) * (1 / n1 + 1 / n2))
  list(p1 = x1 / n1, p2 = x2 / n2, pp = pp, se = se, z = round((x1 / n1 - x2 / n2) / se, 2),
       mincount = min(c(n1, n2) * pp, c(n1, n2) * (1 - pp)))
}
hv <- tp(20, 200, 12, 200)     # hives: A vs B, two-sided, alpha = 0.01
sb <- tp(156, 2169, 183, 2231) # seat belts: women vs men, lower-tailed, alpha = 0.01
ev <- tp(135, 1343, 12, 232)   # electric vehicles: younger vs older, upper-tailed, alpha = 0.05
stopifnot(hv$pp == 0.08, hv$mincount >= 15, round(hv$se, 4) == 0.0271, hv$z == 1.47,
          zt(1.47) == 0.9292, round(2 * (1 - zt(1.47)), 4) == 0.1416,
          round(sb$pp, 4) == 0.0770, sb$mincount >= 15, round(sb$se, 4) == 0.0080, sb$z == -1.26,
          zt(-1.26) == 0.1038,
          round(ev$pp, 4) == 0.0933, ev$mincount >= 15, round(ev$se, 4) == 0.0207, ev$z == 2.36,
          zt(2.36) == 0.9909, round(1 - zt(2.36), 4) == 0.0091)

#---- type size helpers: points in, refuse anything under 16pt
MIN_PT <- 16
tsz <- function(pt) { stopifnot(pt >= MIN_PT); pt / .pt }     # geom text size (mm)
el <- function(pt, ...) { stopifnot(pt >= MIN_PT); element_text(size = pt, ...) }

NAVY <- "#1F3864"; SHADE <- "#8FAADC"; RED <- "#C00000"; GREEN <- "#548235"
bare <- theme_classic() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank(),
        axis.text.x = el(16, colour = 'black'), axis.title.x = el(16))
lab <- function(x, y, s, col, hj, face = 'plain', pt = 16)
  annotate('label', x = x, y = y, label = s, colour = col, hjust = hj, size = tsz(pt), fontface = face,
           fill = 'white', border.colour = NA, label.padding = unit(0.1, 'lines'))

sizes <- data.frame(file = character(), width_pt = numeric(), height_pt = numeric())
save_fig <- function(g, file, wpt, hpt) {
  ggsave(file.path(assets_dir, file), g, width = wpt / 72, height = hpt / 72, dpi = 200, bg = 'white')
  sizes[nrow(sizes) + 1, ] <<- list(file, wpt, hpt)
}

#---- the rebar test's power: x-bar ~ N(170, 2) if H0 is true; N(175, 2) for one possible truth.
#Reject H0 right of the cutoff 173.29 (never labelled "HA true": HA is not one model).
cut_r <- 173.29
rx <- seq(163, 182, length.out = 900)
r0 <- data.frame(x = rx, y = dnorm(rx, 170, 2)); r1 <- data.frame(x = rx, y = dnorm(rx, 175, 2))
top <- dnorm(0, 0, 2)
gr <- ggplot() +
  geom_area(data = subset(r1, x >= cut_r), aes(x, y), fill = GREEN, alpha = 0.55) +
  geom_area(data = subset(r1, x <= cut_r), aes(x, y), fill = 'grey55', alpha = 0.75) +
  geom_area(data = subset(r0, x >= cut_r), aes(x, y), fill = RED, alpha = 0.85) +
  geom_line(data = r0, aes(x, y), colour = NAVY, linewidth = 1.1) +
  geom_line(data = r1, aes(x, y), colour = GREEN, linewidth = 1.1) +
  geom_vline(xintercept = cut_r, colour = 'black', linewidth = 1) +
  lab(169.6, top * 1.1, 'H0 true:  μ = 170', NAVY, 1, 'bold') +
  lab(175.4, top * 1.1, 'one possible truth:  μ = 175', GREEN, 0, 'bold') +
  lab(cut_r, top * 1.3, 'cutoff  x̄* = 173.29', 'black', 0.5, 'bold') +
  lab(181.9, top * 0.62, 'power = 0.81', GREEN, 1, 'bold') +
  annotate('segment', x = 179.4, xend = 176.6, y = top * 0.55, yend = top * 0.42, colour = GREEN,
           arrow = arrow(length = unit(0.2, 'cm'))) +
  lab(163.1, top * 0.62, 'β = 0.19', 'grey25', 0, 'bold') +
  annotate('segment', x = 165.4, xend = 172.6, y = top * 0.55, yend = top * 0.12, colour = 'grey25',
           arrow = arrow(length = unit(0.2, 'cm'))) +
  lab(181.9, top * 0.16, 'α = 0.05', RED, 1, 'bold') +
  annotate('segment', x = 179.2, xend = 174.5, y = top * 0.13, yend = top * 0.03, colour = RED,
           arrow = arrow(length = unit(0.2, 'cm'))) +
  scale_x_continuous(breaks = seq(164, 182, by = 2)) +
  coord_cartesian(xlim = c(163, 182), ylim = c(0, top * 1.38)) +
  labs(x = 'mean hardness of 25 bars, x̄  (reject H0 right of the line)', y = NULL) + bare
save_fig(gr, 'l27_power_rebar.png', 840, 300)

#---- the hives P-value: z_obs = 1.47 on N(0, 1), two-sided, each tail 1 - 0.9292 = 0.0708
gx <- seq(-3.9, 3.9, length.out = 800)
dd <- data.frame(x = gx, y = dnorm(gx))
tp0 <- dnorm(0)
gh <- ggplot(dd, aes(x, y)) +
  geom_area(fill = SHADE, alpha = 0.5) +
  geom_area(data = subset(dd, x >= 1.47), fill = RED, alpha = 0.85) +
  geom_area(data = subset(dd, x <= -1.47), fill = RED, alpha = 0.85) +
  geom_line(colour = NAVY, linewidth = 1.1) +
  geom_vline(xintercept = c(-1.47, 1.47), colour = NAVY, linetype = 'dashed') +
  lab(0, tp0 * 1.18, 'if H0 is true:  z_obs ~ N(0, 1)', NAVY, 0.5, 'bold') +
  lab(3.85, tp0 * 0.45, 'P(Z ≥ 1.47) = 0.0708', RED, 1, 'bold') +
  annotate('segment', x = 2.6, xend = 1.85, y = tp0 * 0.38, yend = tp0 * 0.1, colour = RED,
           arrow = arrow(length = unit(0.2, 'cm'))) +
  lab(-3.85, tp0 * 0.45, 'P(Z ≤ −1.47) = 0.0708', RED, 0, 'bold') +
  annotate('segment', x = -2.6, xend = -1.85, y = tp0 * 0.38, yend = tp0 * 0.1, colour = RED,
           arrow = arrow(length = unit(0.2, 'cm'))) +
  lab(0, tp0 * 0.3, 'P-value = 2(0.0708) = 0.1416', 'black', 0.5, 'bold') +
  scale_x_continuous(breaks = c(-3, -1.47, 0, 1.47, 3), labels = c('-3', '-1.47', '0', '1.47', '3')) +
  coord_cartesian(xlim = c(-3.9, 3.9), ylim = c(0, tp0 * 1.25)) +
  labs(x = 'z_obs for medication A − medication B (ours: 1.47)', y = NULL) + bare
save_fig(gh, 'l27_hives.png', 840, 280)

write.csv(sizes, file.path(assets_dir, 'l27_figure_sizes.csv'), row.names = FALSE)
print(sizes)
