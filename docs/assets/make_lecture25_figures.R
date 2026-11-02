#Checks the numbers on the Week 11 Lecture 25 deck (11/2/2026): the three distributions behind a
#test, composite hypotheses, types of errors and power. It draws:
#
#  l25_dist_mean.png     distribution of X, the sampling distribution of the mean, z_obs ~ N(0, 1)
#  l25_dist_prop.png     Y ~ Bernoulli(p), the sampling distribution of the sample proportion, z_obs
#  l25_boundary.png      glass (OpenStax 9.20): the P-value at three means inside H0: mu <= 1
#  l25_power.png         goggles (OpenStax 9.14) at an assumed true mean of 16.0 s: alpha, beta, power
#  l25_figure_sizes.csv  the size, in points, each figure is drawn at
#
#P-values from the normal are the hand-method values from the course z-table (four decimals). The
#same numbers are asserted on the Week 11 notes page (lecturemenu11.Rmd).
#
#TYPE SIZE. Same rule as the other make_lectureN_figures.R scripts: nothing on a slide is under
#16pt, so each figure is drawn at exactly its on-slide size (points / 72 inches) and every text
#size goes through tsz() or el(), which refuse anything smaller.

library(ggplot2)
library(ggpubr)
library(latex2exp)

near <- function(a, b, tol = 5e-4) abs(a - b) < tol
zt <- function(z) round(pnorm(z), 4)            # a z-table lookup
fmt <- function(x, d = 2) formatC(x, format = 'f', digits = d)

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

#---- numbers (the same checks as lecturemenu11.Rmd and lecturemenu10.Rmd)
#first statistics test (OpenStax 9.16) and boiling point (Try It 9.20), sigma unknown
sc <- c(65, 65, 70, 67, 66, 63, 63, 68, 72, 71)
stopifnot(mean(sc) == 67, round(sd(sc), 3) == 3.197, round(3.197 / sqrt(10), 3) == 1.011,
          round(2 / 1.011, 2) == 1.98, round(qt(0.95, 9), 3) == 1.833, round(qt(0.975, 9), 3) == 2.262)
bo <- c(205, 206, 206, 202, 199, 194, 197, 198, 198, 201, 201, 202, 207, 211, 205)
stopifnot(round(mean(bo), 2) == 202.13, round(sd(bo), 3) == 4.533, round(4.533 / sqrt(15), 3) == 1.170,
          round(2.13 / 1.170, 2) == 1.82, round(qt(0.95, 14), 3) == 1.761, round(qt(0.975, 14), 3) == 2.145,
          round(1 - pt(1.82, 14), 3) == 0.045)
#bench press (OpenStax 9.15), sigma known
bp <- rep(c(205, 215, 225, 241, 252, 265, 275, 313, 316, 338, 341, 345, 368, 385),
          c(3, 3, 1, 2, 2, 2, 2, 2, 5, 2, 1, 2, 2, 1))
stopifnot(sum(bp) == 8585, round(mean(bp), 2) == 286.17, round(55 / sqrt(30), 2) == 10.04,
          round((286.17 - 275) / 10.04, 2) == 1.11, round(1 - zt(1.11), 4) == 0.1335)
#glass (OpenStax 9.20): t at the boundary and at two other values inside H0: mu <= 1
gl <- c(1.11, 1.07, 1.11, 1.07, 1.12, 1.08, .98, .98, 1.02, .95, .95)
xb_gl <- 1.04; se_gl <- 0.0199
stopifnot(round(mean(gl), 2) == xb_gl, round(sd(gl), 4) == 0.0659, round(0.0659 / sqrt(11), 4) == se_gl,
          round(0.04 / se_gl, 2) == 2.01, round(qt(0.95, 10), 3) == 1.812, round(qt(0.975, 10), 3) == 2.228)
mus <- c(0.98, 0.99, 1.00)
t_mu <- round((xb_gl - mus) / se_gl, 2); p_mu <- round(1 - pt(t_mu, 10), 4)
stopifnot(t_mu == c(3.02, 2.51, 2.01), p_mu == c(0.0064, 0.0155, 0.0361))
#bread (OpenStax 9.9) and Try It 9.9
stopifnot(round(0.5 / sqrt(10), 4) == 0.1581, round(2 / 0.1581, 2) == 12.65,
          round(0.5 / (1 / 6), 2) == 3, round(1 - zt(3), 4) == 0.0013)
#cell phone owners (Try It 9.18): H0: p >= 0.92
stopifnot(174 / 200 == 0.87, round(sqrt(0.92 * 0.08 / 200), 4) == 0.0192,
          round(-0.05 / 0.0192, 2) == -2.60, zt(-2.60) == 0.0047, 200 * 0.08 >= 15)
#power: goggles, lower-tailed z at alpha = 0.05, true mean 16.0
se_g <- 0.2066; cut_g <- 16.09
stopifnot(round(0.8 / sqrt(15), 4) == se_g, round(16.43 - 1.645 * se_g, 2) == cut_g,
          round((cut_g - 16) / se_g, 2) == 0.44, zt(0.44) == 0.67, round(qnorm(0.05), 3) == -1.645)
pw <- function(n = 15, a = 0.05, mu = 16) {
  se <- 0.8 / sqrt(n); round(pnorm((16.43 - qnorm(1 - a) * se - mu) / se), 2) }
stopifnot(pw() == 0.67, pw(n = 30) == 0.90, pw(n = 60) == 0.99, pw(a = 0.01) == 0.40,
          pw(a = 0.10) == 0.79, pw(mu = 16.2) == 0.30, pw(mu = 15.8) == 0.92)

#---- type size helpers: points in, refuse anything under 16pt
MIN_PT <- 16
tsz <- function(pt) { stopifnot(pt >= MIN_PT); pt / .pt }     # geom text size (mm)
el <- function(pt, ...) { stopifnot(pt >= MIN_PT); element_text(size = pt, ...) }

NAVY <- "#1F3864"; SHADE <- "#8FAADC"; RED <- "#C00000"; GREEN <- "#548235"
bare <- theme_classic() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank(),
        axis.text.x = el(16, colour = 'black'), axis.title.x = el(16),
        plot.title = el(18, face = 'bold', colour = NAVY), plot.subtitle = el(16, colour = NAVY),
        strip.text = el(16, face = 'bold', colour = NAVY), strip.background = element_blank(),
        legend.text = el(16), legend.title = el(16))
lab <- function(x, y, s, col, hj, face = 'plain', pt = 16)
  annotate('label', x = x, y = y, label = s, colour = col, hjust = hj, size = tsz(pt), fontface = face,
           fill = 'white', border.colour = NA, label.padding = unit(0.1, 'lines'))

sizes <- data.frame(file = character(), width_pt = numeric(), height_pt = numeric())
save_fig <- function(g, file, wpt, hpt) {
  ggsave(file.path(assets_dir, file), g, width = wpt / 72, height = hpt / 72, dpi = 200, bg = 'white')
  sizes[nrow(sizes) + 1, ] <<- list(file, wpt, hpt)
}

gen_panel <- function(d, ttl, sub, xl, brk, lbl, m) {
  ggplot(d, aes(x, y)) + geom_area(fill = SHADE, alpha = 0.6) + geom_line(colour = NAVY, linewidth = 1) +
    geom_vline(xintercept = m, colour = NAVY, linetype = 'dashed') +
    scale_x_continuous(breaks = brk, labels = lbl) +
    labs(title = ttl, subtitle = sub, x = xl, y = NULL) + bare
}
zx <- seq(-3.5, 3.5, length.out = 400)
zpan <- gen_panel(data.frame(x = zx, y = dnorm(zx)), 'Distribution of z_obs',
                  TeX('$z_{obs} \\sim N(0, 1)$, if $H_0$ true'), 'z_obs', c(-3, 0, 3), c(-3, 0, 3), 0)

#---- the three distributions: a mean
gx <- seq(0, 16, length.out = 600)          # X: a gamma with mean 5, a little right-skewed
a1 <- gen_panel(data.frame(x = gx, y = dgamma(gx, shape = 5, rate = 1)), 'Distribution of X',
                TeX('mean $\\mu$, SD $\\sigma$, any shape'), 'X, one individual', 5, TeX('$\\mu$'), 5)
a2 <- gen_panel(data.frame(x = gx, y = dnorm(gx, 5, sqrt(5 / 30))), 'Sampling dist. of the mean',
                TeX('$\\bar{x} \\sim N(\\mu, \\sigma / \\sqrt{n})$'), 'x-bar, a sample of n', 5, TeX('$\\mu$'), 5)
save_fig(ggarrange(a1, a2, zpan, ncol = 3), 'l25_dist_mean.png', 840, 300)

#---- the three distributions: a proportion
b1 <- ggplot(data.frame(y = factor(c('0', '1')), p = c(0.7, 0.3)), aes(y, p)) +
  geom_col(fill = SHADE, colour = NAVY, width = 0.5) +
  annotate('text', x = 1:2, y = c(0.7, 0.3) + 0.1, colour = NAVY, size = tsz(16),
           label = c('1 - p', 'p')) +
  scale_y_continuous(limits = c(0, 0.85)) +
  labs(title = 'Distribution of Y', subtitle = TeX('$Y \\sim Bernoulli(p)$'),
       x = 'Y, one individual', y = NULL) + bare
px <- seq(0.1, 0.5, length.out = 400)
b2 <- gen_panel(data.frame(x = px, y = dnorm(px, 0.3, sqrt(0.3 * 0.7 / 80))), 'Sampling dist. of p-hat',
                TeX('$\\hat{p} \\sim N(p, \\sqrt{p(1-p)/n})$'), 'p-hat, a sample of n', 0.3, TeX('$p$'), 0.3)
save_fig(ggarrange(b1, b2, zpan, ncol = 3), 'l25_dist_prop.png', 840, 300)

#---- why test at the boundary: the glass P-value at three means inside H0: mu <= 1
bx <- seq(0.92, 1.08, length.out = 600)
gd <- do.call(rbind, lapply(seq_along(mus), function(i)
  data.frame(x = bx, y = dt((bx - mus[i]) / se_gl, 10) / se_gl,
             panel = paste0('μ = ', fmt(mus[i], 2), ':  P = ', fmt(p_mu[i], 4)))))
gd$panel <- factor(gd$panel, levels = unique(gd$panel))
gb <- ggplot(gd, aes(x, y)) +
  geom_area(data = subset(gd, x >= xb_gl), fill = SHADE, alpha = 0.95) +
  geom_line(colour = NAVY, linewidth = 1) +
  geom_vline(xintercept = xb_gl, colour = RED, linewidth = 1.1) +
  geom_vline(data = data.frame(m = mus, panel = levels(gd$panel)), aes(xintercept = m),
             colour = NAVY, linetype = 'dashed') +
  facet_wrap(~ panel, nrow = 1) +
  scale_x_continuous(breaks = c(0.96, 1.00, 1.04)) +
  labs(x = 'x-bar for 11 pieces of glass (red line: ours, 1.04)', y = NULL) + bare
save_fig(gb, 'l25_boundary.png', 840, 260)

#---- power: the goggles if the true mean is 16.0 s
gx <- seq(15.3, 17.1, length.out = 800)
d0 <- data.frame(x = gx, y = dnorm(gx, 16.43, se_g)); d1 <- data.frame(x = gx, y = dnorm(gx, 16, se_g))
top <- max(d0$y)
gp <- ggplot() +
  geom_area(data = subset(d1, x <= cut_g), aes(x, y), fill = GREEN, alpha = 0.55) +
  geom_area(data = subset(d1, x >= cut_g), aes(x, y), fill = 'grey70', alpha = 0.7) +
  geom_area(data = subset(d0, x <= cut_g), aes(x, y), fill = RED, alpha = 0.75) +
  geom_line(data = d1, aes(x, y), colour = GREEN, linewidth = 1.1) +
  geom_line(data = d0, aes(x, y), colour = NAVY, linewidth = 1.1) +
  geom_vline(xintercept = cut_g, colour = 'black', linewidth = 1) +
  lab(15.95, top * 1.08, 'truth: μ = 16.0', GREEN, 1, 'bold') +
  lab(16.48, top * 1.08, 'H0: μ = 16.43', NAVY, 0, 'bold') +
  lab(cut_g, top * 1.24, paste0('cutoff ', fmt(cut_g, 2), ' s'), 'black', 0.5, 'bold') +
  lab(15.33, top * 0.8, 'reject H0\nto the left', 'black', 0) +
  lab(15.76, top * 0.42, paste0('power\n= ', fmt(zt(0.44), 2)), GREEN, 0.5, 'bold') +
  lab(16.26, top * 0.13, paste0('β = ', fmt(1 - zt(0.44), 2)), 'grey30', 0, 'bold') +
  annotate('segment', x = 15.66, xend = 15.99, y = top * 0.15, yend = top * 0.03, colour = RED,
           arrow = arrow(length = unit(0.2, 'cm'))) +
  lab(15.66, top * 0.17, 'α = 0.05', RED, 1, 'bold') +
  scale_x_continuous(breaks = seq(15.4, 17, by = 0.4)) +
  coord_cartesian(ylim = c(0, top * 1.3)) +
  labs(x = "Jeffrey's mean time for 15 swims (seconds)", y = NULL) + bare
save_fig(gp, 'l25_power.png', 840, 300)

write.csv(sizes, file.path(assets_dir, 'l25_figure_sizes.csv'), row.names = FALSE)
print(sizes)
