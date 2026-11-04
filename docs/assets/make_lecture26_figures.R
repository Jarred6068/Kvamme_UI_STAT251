#Checks the numbers on the Week 11 Lecture 26 deck (11/4/2026): confidence intervals and two-sided
#tests, and the sign test. It draws:
#
#  l26_ci_null.png       generic: if H0 is true, 100(1 - alpha)% of estimates land in theta0 -/+ ME;
#                        an estimate outside that interval is evidence against H0
#  l26_errors.png      a generic upper-tailed z-test: true negative, Type I, Type II and power as areas
#  l26_power_n.png     goggles (OpenStax 9.14) on the z_obs scale at n = 15, 30, 60 if the true mean is 16.0
#  l26_power_curve.png power against n for true means 15.8, 16.0 and 16.2 s
#  l26_sign.png          the couples' ratings (Jarred's 2024 notes): S ~ Binomial(14, 0.5), both tails
#  l26_figure_sizes.csv  the size, in points, each figure is drawn at
#
#P-values are hand-method values (z-table, exact binomial sums). The same numbers are asserted on
#the Week 11 notes page (lecturemenu11.Rmd).
#
#TYPE SIZE. Same rule as the other make_lectureN_figures.R scripts: nothing on a slide is under
#16pt, so each figure is drawn at exactly its on-slide size (points / 72 inches) and every text
#size goes through tsz() or el(), which refuse anything smaller.

library(ggplot2)

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

#---- numbers (the same checks as lecturemenu11.Rmd)
stopifnot(round(qnorm(0.975), 2) == 1.96)
#the couples (Jarred's 2024 notes): 11 of the 14 non-zero differences are +
hus <- c(5, 4, 6, 6, 3, 2, 5, 3, 1, 4, 5, 4, 4, 7, 5, 5, 5)
wif <- c(3, 3, 4, 5, 3, 3, 2, 3, 2, 3, 2, 2, 5, 2, 5, 3, 1)
dif <- hus - wif
stopifnot(sum(dif > 0) == 11, sum(dif < 0) == 3, sum(dif == 0) == 3,
          choose(14, 11:14) == c(364, 91, 14, 1), 2^14 == 16384,
          round(470 / 16384, 4) == 0.0287, round(2 * 470 / 16384, 4) == 0.0574)
#the stretching program (hypothetical): 8 of 9 non-zero differences are +, upper-tailed
kn <- c(3, 1, 0, 2, 4, -1, 2, 1, 3, 2)
stopifnot(sum(kn > 0) == 8, sum(kn < 0) == 1, sum(kn == 0) == 1,
          choose(9, 8:9) == c(9, 1), round(10 / 512, 4) == 0.0195,
          round(1 - pbinom(7, 9, 0.5), 4) == 0.0195)
#the couples: mean and SE of S, and why n = 14 is too small for the z approximation
stopifnot(14 / 2 == 7, round(sqrt(14) / 2, 2) == 1.87, 14 / 2 < 15)
#the coffee blend (hypothetical): 60 customers, 6 ties, 38 of the 54 others prefer the new blend
stopifnot(60 - 6 == 54, 54 / 2 == 27, 54 / 2 >= 15, round(sqrt(54) / 2, 3) == 3.674,
          round((38 - 27) / 3.674, 2) == 2.99, round(1 - round(pnorm(2.99), 4), 4) == 0.0014)

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

#---- confidence intervals and two-sided tests (generic): if H0 is true, the estimate's sampling
#distribution is centred at theta0, and 100(1 - alpha)% of estimates land within theta0 -/+ ME,
#ME = Z_{1-alpha/2} x SE. An estimate outside that interval is evidence against H0.
gx <- seq(-3.9, 3.9, length.out = 800)
dd <- data.frame(x = gx, y = dnorm(gx))
top <- dnorm(0)
gc <- ggplot(dd, aes(x, y)) +
  geom_area(data = subset(dd, abs(x) <= 1.96), fill = SHADE, alpha = 0.7) +
  geom_area(data = subset(dd, x <= -1.96), fill = RED, alpha = 0.85) +
  geom_area(data = subset(dd, x >= 1.96), fill = RED, alpha = 0.85) +
  geom_line(colour = NAVY, linewidth = 1.1) +
  geom_vline(xintercept = c(-1.96, 1.96), colour = NAVY, linetype = 'dashed') +
  lab(0, top * 1.4, 'if H0 is true:  100(1 − α)% of estimates land in  θ0 ± ME', NAVY, 0.5, 'bold') +
  annotate('segment', x = 0, xend = 1.94, y = top * 1.07, yend = top * 1.07, colour = 'black',
           arrow = arrow(ends = 'both', length = unit(0.18, 'cm'))) +
  annotate('label', x = 0.98, y = top * 1.19, label = 'ME == Z[1-alpha/2] %*% SE', parse = TRUE,
           size = tsz(16), fill = 'white', border.colour = NA, label.padding = unit(0.1, 'lines')) +
  lab(-2.35, top * 0.3, 'α/2', RED, 0.5, 'bold') + lab(2.3, top * 0.3, 'α/2', RED, 0.5, 'bold') +
  annotate('point', x = c(-0.8, 2.75), y = 0, colour = c(NAVY, RED), size = 4.5) +
  lab(-0.8, top * 0.45, 'estimate inside:\ndo not reject H0', NAVY, 0.5, 'bold') +
  annotate('segment', x = -0.8, xend = -0.8, y = top * 0.3, yend = top * 0.05, colour = NAVY,
           arrow = arrow(length = unit(0.18, 'cm'))) +
  lab(3.0, top * 0.66, 'estimate outside\n(more than ME from θ0):\nreject H0 at level α', RED, 0.5, 'bold') +
  annotate('segment', x = 2.75, xend = 2.75, y = top * 0.43, yend = top * 0.05, colour = RED,
           arrow = arrow(length = unit(0.18, 'cm'))) +
  scale_x_continuous(breaks = c(-1.96, 0, 1.96), labels = c('θ0 − ME', 'θ0', 'θ0 + ME')) +
  coord_cartesian(xlim = c(-3.9, 3.9), ylim = c(0, top * 1.48)) +
  labs(x = 'the estimate (x̄ or p̂), if H0 is true', y = NULL) + bare
save_fig(gc, 'l26_ci_null.png', 840, 280)

#---- the sign test: S ~ Binomial(14, 0.5), observed S = 11, two-sided
bd <- data.frame(s = 0:14, p = dbinom(0:14, 14, 0.5))
bd$tail <- bd$s >= 11 | bd$s <= 3
gs <- ggplot(bd, aes(s, p, fill = tail)) +
  geom_col(colour = NAVY, width = 0.8) +
  scale_fill_manual(values = c(`FALSE` = SHADE, `TRUE` = RED), guide = 'none') +
  lab(12.5, 0.075, 'P(S ≥ 11) = 0.0287', RED, 0.5, 'bold') +
  lab(1.5, 0.075, 'P(S ≤ 3) = 0.0287', RED, 0.5, 'bold') +
  lab(7, 0.235, 'if H0 is true:  S ~ Binomial(14, 0.5)', NAVY, 0.5, 'bold') +
  annotate('segment', x = 11, xend = 11, y = 0.05, yend = 0.028, colour = RED,
           arrow = arrow(length = unit(0.2, 'cm'))) +
  scale_x_continuous(breaks = 0:14) +
  coord_cartesian(ylim = c(0, 0.25)) +
  labs(x = 'S = number of + signs among n = 14 couples (ours: 11)', y = NULL) + bare
save_fig(gs, 'l26_sign.png', 840, 280)

#---- the four outcomes on the distributions (generic upper-tailed z-test, alpha = 0.05)
#H0 true: z ~ N(0, 1). One possible truth with H0 false: centred at 2.5 (never labelled "HA true":
#HA is not one model). Left of the cutoff Z_0.95 = do not reject.
cz <- round(qnorm(0.95), 3); stopifnot(cz == 1.645)
ex <- seq(-3.6, 6.1, length.out = 900)
e0 <- data.frame(x = ex, y = dnorm(ex)); e1 <- data.frame(x = ex, y = dnorm(ex, 2.5))
ge <- ggplot() +
  geom_area(data = subset(e0, x <= cz), aes(x, y), fill = SHADE, alpha = 0.7) +
  geom_area(data = subset(e1, x >= cz), aes(x, y), fill = GREEN, alpha = 0.55) +
  geom_area(data = subset(e1, x <= cz), aes(x, y), fill = 'grey55', alpha = 0.75) +
  geom_area(data = subset(e0, x >= cz), aes(x, y), fill = RED, alpha = 0.85) +
  geom_line(data = e0, aes(x, y), colour = NAVY, linewidth = 1.1) +
  geom_line(data = e1, aes(x, y), colour = GREEN, linewidth = 1.1) +
  geom_vline(xintercept = cz, colour = 'black', linewidth = 1) +
  lab(0, 0.445, 'H0 true', NAVY, 0.5, 'bold') + lab(2.5, 0.445, 'H0 false:  one possible truth', GREEN, 0.18, 'bold') +
  lab(cz - 0.1, 0.52, '← do not reject H0', 'black', 1) + lab(cz + 0.1, 0.52, 'reject H0 →', 'black', 0) +
  lab(-3.5, 0.27, 'true negative:  1 − α', NAVY, 0, 'bold') +
  annotate('segment', x = -1.5, xend = -0.6, y = 0.25, yend = 0.15, colour = NAVY, arrow = arrow(length = unit(0.2, 'cm'))) +
  lab(-3.5, 0.08, 'Type II error:  β', 'grey25', 0, 'bold') +
  annotate('segment', x = -0.9, xend = 1.1, y = 0.08, yend = 0.06, colour = 'grey25', arrow = arrow(length = unit(0.2, 'cm'))) +
  lab(6.1, 0.27, 'power:  1 − β', GREEN, 1, 'bold') +
  annotate('segment', x = 4.6, xend = 3.3, y = 0.25, yend = 0.17, colour = GREEN, arrow = arrow(length = unit(0.2, 'cm'))) +
  lab(6.1, 0.08, 'Type I error:  α', RED, 1, 'bold') +
  annotate('segment', x = 4.2, xend = 2.05, y = 0.075, yend = 0.025, colour = RED, arrow = arrow(length = unit(0.2, 'cm'))) +
  scale_x_continuous(breaks = c(0, cz), labels = c('0', 'Z0.95 = 1.645')) +
  coord_cartesian(xlim = c(-3.6, 6.1), ylim = c(0, 0.55)) +
  labs(x = 'the test statistic, z_obs', y = NULL) + bare
save_fig(ge, 'l26_errors.png', 840, 300)

#---- power and sample size: the goggles (OpenStax 9.14), lower-tailed z at alpha = 0.05, true mean 16.0
#On the z_obs scale the null curve N(0, 1) and the cutoff Z_0.05 = -1.645 never move; if the true mean
#is 16.0, z_obs is centred at (16.0 - 16.43) / (0.8 / sqrt(n)), which moves left as n grows.
pwn <- function(n, mu = 16) pnorm((16.43 - qnorm(0.95) * 0.8 / sqrt(n) - mu) / (0.8 / sqrt(n)))
ns <- c(15, 30, 60); sh <- round((16 - 16.43) / (0.8 / sqrt(ns)), 2); pws <- round(pwn(ns), 2)
nn <- 2:120; n80 <- min(nn[pwn(nn) >= 0.8])
stopifnot(sh == c(-2.08, -2.94, -4.16), pws == c(0.67, 0.90, 0.99), n80 == 22,
          round(pwn(21:22), 2) == c(0.79, 0.81), min(nn[pwn(nn, 16.2) >= 0.8]) == 75,
          min(nn[pwn(nn, 15.8) >= 0.8]) == 10)
zx <- seq(-7.5, 3.2, length.out = 700)
pn <- do.call(rbind, lapply(seq_along(ns), function(i) {
  lv <- paste0('n = ', ns[i], ':   power = ', fmt(pws[i], 2))
  rbind(data.frame(x = zx, y = dnorm(zx), curve = 'null', panel = lv),
        data.frame(x = zx, y = dnorm(zx, sh[i]), curve = 'truth', panel = lv)) }))
pn$panel <- factor(pn$panel, levels = unique(pn$panel))
gn <- ggplot(pn, aes(x, y)) +
  geom_area(data = subset(pn, curve == 'truth' & x <= -1.645), fill = GREEN, alpha = 0.55) +
  geom_line(data = subset(pn, curve == 'null'), colour = NAVY, linewidth = 1) +
  geom_line(data = subset(pn, curve == 'truth'), colour = GREEN, linewidth = 1) +
  geom_vline(xintercept = -1.645, colour = 'black', linewidth = 0.9) +
  geom_text(data = data.frame(panel = factor(levels(pn$panel)[1], levels = levels(pn$panel)),
                              x = c(0.6, -4.3), y = 0.47, s = c('H0 true', 'μ = 16.0')),
            aes(x, y, label = s), colour = c(NAVY, GREEN), size = tsz(16), fontface = 'bold', inherit.aes = FALSE) +
  facet_wrap(~ panel, nrow = 1) +
  scale_x_continuous(breaks = c(-6, -1.645, 2), labels = c('-6', '-1.645', '2')) +
  coord_cartesian(ylim = c(0, 0.5)) +
  labs(x = 'z_obs   (reject H0 left of the line)', y = NULL) + bare +
  theme(strip.text = el(16, face = 'bold', colour = NAVY), strip.background = element_blank())
save_fig(gn, 'l26_power_n.png', 840, 280)

#power against n for three true means; points at n = 15, 30, 60 on the 16.0 curve
pc <- do.call(rbind, lapply(c(16.2, 16.0, 15.8), function(m)
  data.frame(n = nn, p = pwn(nn, m), mu = paste0('true mean ', fmt(m, 1), ' s'))))
pc$mu <- factor(pc$mu, levels = unique(pc$mu))
gpc <- ggplot(pc, aes(n, p, colour = mu)) +
  geom_hline(yintercept = 0.8, colour = 'grey45', linetype = 'dashed') +
  geom_line(linewidth = 1.2) +
  geom_point(data = data.frame(n = ns, p = pwn(ns), mu = factor('true mean 16.0 s', levels = levels(pc$mu))),
             size = 3.5) +
  annotate('text', x = c(120, 40, 5), y = c(0.62, 0.9, 1.05), label = levels(pc$mu), hjust = c(1, 0, 0),
           colour = c(RED, GREEN, NAVY), size = tsz(16), fontface = 'bold') +
  annotate('text', x = 121, y = 0.765, label = '80% power', colour = 'grey35', size = tsz(16), hjust = 1) +
  scale_colour_manual(values = c(RED, GREEN, NAVY), guide = 'none') +
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1.08)) +
  scale_x_continuous(breaks = c(0, 15, 30, 60, 90, 120)) +
  labs(x = 'number of swims, n', y = 'power') +
  theme_classic() + theme(axis.text = el(16, colour = 'black'), axis.title = el(16))
save_fig(gpc, 'l26_power_curve.png', 840, 300)

write.csv(sizes, file.path(assets_dir, 'l26_figure_sizes.csv'), row.names = FALSE)
print(sizes)
