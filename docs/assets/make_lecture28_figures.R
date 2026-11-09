#Checks the numbers on the Week 12 Lecture 28 deck (11/9/2026): the two independent samples t-test
#and the matched-pairs t-test. It draws:
#
#  l28_sports.png        girls vs boys, hours of sports a day (OpenStax Example 10.1): t_obs = -3.14 on
#                        t(8), both tails
#  l28_hypnotism.png     pain before and after hypnotism (OpenStax Example 10.11): t_obs = -3.04 on
#                        t(7), lower tail
#  l28_figure_sizes.csv  the size, in points, each figure is drawn at
#
#Course conventions: the unpooled SE sqrt(s1^2/n1 + s2^2/n2) with df = min(n1 - 1, n2 - 1); P-values
#are bracketed from the course t-table (misc/t_table.html, upper-tail columns 0.25, 0.2, 0.15, 0.125,
#0.1, 0.075, 0.05, 0.025, 0.005, 0.0025, 0.0005), with the software value alongside. The same
#numbers are asserted on the Week 12 notes page (lecturemenu12.Rmd).
#
#TYPE SIZE. Same rule as the other make_lectureN_figures.R scripts: nothing on a slide is under
#16pt, so each figure is drawn at exactly its on-slide size (points / 72 inches) and every text
#size goes through tsz() or el(), which refuse anything smaller.

library(ggplot2)

fmt <- function(x, d = 2) formatC(x, format = 'f', digits = d)
near <- function(a, b, tol = 5e-4) abs(a - b) < tol
tq <- function(tail, df) round(qt(1 - tail, df), 3)     # a t-table entry: upper-tail area, df

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

#---- numbers (the same checks as lecturemenu12.Rmd)
#two independent means, unpooled, hand method: SE to 4 decimals, t_obs to 2
t2 <- function(xb1, s1, n1, xb2, s2, n2) {
  se <- round(sqrt(s1^2 / n1 + s2^2 / n2), 4)
  list(se = se, t = round((xb1 - xb2) / se, 2), df = min(n1 - 1, n2 - 1))
}
#sports (Example 10.1): girls 9, 2, 0.866; boys 16, 3.2, 1.00; two-sided, alpha = 0.05
sp <- t2(2, 0.866, 9, 3.2, 1, 16)
stopifnot(sp$se == 0.3819, sp$t == -3.14, sp$df == 8,
          tq(0.025, 8) == 2.306, tq(0.005, 8) == 3.355,           # 0.005 < upper tail < 0.025
          tq(0.025, 8) < 3.14, 3.14 < tq(0.005, 8),               # so 0.01 < P-value < 0.05
          round(2 * pt(-3.14, 8), 4) == 0.0138)
#the book's Welch df and P-value, for the notes
w_df <- (0.866^2 / 9 + 1 / 16)^2 / ((0.866^2 / 9)^2 / 8 + (1 / 16)^2 / 15)
stopifnot(round(w_df, 2) == 18.85, round(2 * pt(-(1.2 / sqrt(0.866^2 / 9 + 1 / 16)), w_df), 4) == 0.0054)
#retention (Try It 10.2): A 15, 5, 1.2; B 20, 4.5, 0.8; upper-tailed, alpha = 0.05
rt <- t2(5, 1.2, 15, 4.5, 0.8, 20)
stopifnot(rt$se == 0.3578, rt$t == 1.40, rt$df == 14,
          tq(0.1, 14) == 1.345, tq(0.075, 14) == 1.523,
          tq(0.1, 14) < 1.40, 1.40 < tq(0.075, 14),               # 0.075 < P-value < 0.10
          round(1 - pt(1.40, 14), 3) == 0.092)

#matched pairs: d = after - before for hypnotism (Example 10.11), lower-tailed, alpha = 0.05
hb <- c(6.6, 6.5, 9.0, 10.3, 11.3, 8.1, 6.3, 11.6)
ha <- c(6.8, 2.4, 7.4, 8.5, 8.1, 6.1, 3.4, 2.0)
hd <- round(ha - hb, 1)
stopifnot(identical(hd, c(0.2, -4.1, -1.6, -1.8, -3.2, -2.0, -2.9, -9.6)),
          near(sum(hd), -25, 1e-9), near(mean(hd), -3.125, 1e-9), round(sd(hd), 2) == 2.91,   # book rounds d-bar to -3.13
          round(2.91 / sqrt(8), 4) == 1.0288, round(-3.125 / 1.0288, 2) == -3.04,
          tq(0.025, 7) == 2.365, tq(0.005, 7) == 3.499,
          tq(0.025, 7) < 3.04, 3.04 < tq(0.005, 7),               # 0.005 < P-value < 0.025
          round(pt(-3.04, 7), 4) == 0.0094)
#the same data analysed as if the samples were independent (wrong): a smaller |t|
h_ind <- t2(mean(ha), sd(ha), 8, mean(hb), sd(hb), 8)
stopifnot(h_ind$t == -2.60, round(sqrt(2.18^2 / 8 + 2.61^2 / 8), 4) == 1.2023, round(-3.125 / 1.2023, 2) == -2.60, round(sd(hb), 2) == 2.18, round(sd(ha), 2) == 2.61)
#shot-put (Example 10.13): d = dominant - weaker, two-sided, alpha = 0.05
sd_ <- c(30, 26, 34, 17, 19, 26, 20) - c(28, 14, 27, 18, 17, 26, 16)
stopifnot(identical(sd_, c(2, 12, 7, -1, 2, 0, 4)),
          round(mean(sd_), 2) == 3.71, round(sd(sd_), 2) == 4.50,
          round(4.50 / sqrt(7), 4) == 1.7008, round(3.71 / 1.7008, 2) == 2.18,
          tq(0.05, 6) == 1.943, tq(0.025, 6) == 2.447,
          tq(0.05, 6) < 2.18, 2.18 < tq(0.025, 6),                # 0.05 < P-value < 0.10
          round(2 * pt(-2.18, 6), 3) == 0.072)

#pooling the sports data (the "pooling for girls and boys" slide): s_p, pooled SE, t_obs on 23 df
s_p <- round(sqrt((8 * 0.866^2 + 15 * 1^2) / 23), 4)
stopifnot(s_p == 0.9555, round(s_p * sqrt(1/9 + 1/16), 4) == 0.3981, round(-1.2 / 0.3981, 2) == -3.01,
          tq(0.005, 23) == 2.807, tq(0.0025, 23) == 3.104,                # 0.005 < P-value < 0.01
          round(2 * pt(-3.01, 23), 4) == 0.0062)
#the cost of pooling when it is wrong: n1 = 10, sigma1 = 3 vs n2 = 40, sigma2 = 1, H0 true, alpha = 0.05
set.seed(251)
rej <- replicate(20000, { x <- rnorm(10, 0, 3); y <- rnorm(40, 0, 1)
  c(t.test(x, y, var.equal = TRUE)$p.value < 0.05, t.test(x, y)$p.value < 0.05) })
stopifnot(round(mean(rej[1, ]), 2) == 0.26, abs(mean(rej[2, ]) - 0.05) < 0.005)   # pooled 26%, Welch 5%

#---- type size helpers: points in, refuse anything under 16pt
MIN_PT <- 16
tsz <- function(pt) { stopifnot(pt >= MIN_PT); pt / .pt }     # geom text size (mm)
el <- function(pt, ...) { stopifnot(pt >= MIN_PT); element_text(size = pt, ...) }

NAVY <- "#1F3864"; SHADE <- "#8FAADC"; RED <- "#C00000"
bare <- theme_classic() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank(),
        axis.text.x = el(16, colour = 'black'), axis.title.x = el(16))
#parse = TRUE: s is a plotmath expression (subscripts typeset, as in the slides' equations)
lab <- function(x, y, s, col, hj, face = 'plain', pt = 16, parse = FALSE)
  annotate('label', x = x, y = y, label = s, colour = col, hjust = hj, size = tsz(pt), fontface = face, parse = parse,
           fill = 'white', border.colour = NA, label.padding = unit(0.1, 'lines'))

sizes <- data.frame(file = character(), width_pt = numeric(), height_pt = numeric())
save_fig <- function(g, file, wpt, hpt) {
  ggsave(file.path(assets_dir, file), g, width = wpt / 72, height = hpt / 72, dpi = 200, bg = 'white')
  sizes[nrow(sizes) + 1, ] <<- list(file, wpt, hpt)
}
arr <- function(x, xend, y, yend, col)
  annotate('segment', x = x, xend = xend, y = y, yend = yend, colour = col,
           arrow = arrow(length = unit(0.2, 'cm')))

#---- the sports P-value: t_obs = -3.14 on t(8), two-sided
gx <- seq(-4.6, 4.6, length.out = 900)
d8 <- data.frame(x = gx, y = dt(gx, 8)); top8 <- dt(0, 8)
gs <- ggplot(d8, aes(x, y)) +
  geom_area(fill = SHADE, alpha = 0.5) +
  geom_area(data = subset(d8, x <= -3.14), fill = RED, alpha = 0.85) +
  geom_area(data = subset(d8, x >= 3.14), fill = RED, alpha = 0.85) +
  geom_line(colour = NAVY, linewidth = 1.1) +
  geom_vline(xintercept = c(-3.14, 3.14), colour = NAVY, linetype = 'dashed') +
  lab(0, top8 * 1.18, 'bold("if"~H[0]~"is true:"~~t[obs]~"~ t(8)")', NAVY, 0.5, 'bold', parse = TRUE) +
  lab(-4.55, top8 * 0.5, 'tail area  P(T ≤ −3.14)', RED, 0, 'bold') + arr(-3.9, -3.45, top8 * 0.42, top8 * 0.06, RED) +
  lab(4.55, top8 * 0.5, 'tail area  P(T ≥ 3.14)', RED, 1, 'bold') + arr(3.9, 3.45, top8 * 0.42, top8 * 0.06, RED) +
  lab(0, top8 * 0.3, 't-table:  0.01 < P-value < 0.05', 'black', 0.5, 'bold') +
  scale_x_continuous(breaks = c(-3.14, -2, 0, 2, 3.14), labels = c('-3.14', '-2', '0', '2', '3.14')) +
  coord_cartesian(xlim = c(-4.6, 4.6), ylim = c(0, top8 * 1.25)) +
  labs(x = expression(t[obs]~'for girls − boys (ours: −3.14)'), y = NULL) + bare
save_fig(gs, 'l28_sports.png', 840, 280)

#---- the hypnotism P-value: t_obs = -3.04 on t(7), lower-tailed
d7 <- data.frame(x = gx, y = dt(gx, 7)); top7 <- dt(0, 7)
gh <- ggplot(d7, aes(x, y)) +
  geom_area(fill = SHADE, alpha = 0.5) +
  geom_area(data = subset(d7, x <= -3.04), fill = RED, alpha = 0.85) +
  geom_line(colour = NAVY, linewidth = 1.1) +
  geom_vline(xintercept = -3.04, colour = NAVY, linetype = 'dashed') +
  lab(0, top7 * 1.18, 'bold("if"~H[0]~"is true:"~~t[obs]~"~ t(7)")', NAVY, 0.5, 'bold', parse = TRUE) +
  lab(-4.55, top7 * 0.5, 'P(T ≤ −3.04)', RED, 0, 'bold') + arr(-3.9, -3.4, top7 * 0.42, top7 * 0.06, RED) +
  lab(4.55, top7 * 0.62, 't-table:  0.005 < P-value < 0.025', 'black', 1, 'bold') +
  scale_x_continuous(breaks = c(-3.04, -2, 0, 2, 4), labels = c('-3.04', '-2', '0', '2', '4')) +
  coord_cartesian(xlim = c(-4.6, 4.6), ylim = c(0, top7 * 1.25)) +
  labs(x = expression(t[obs]~'for the mean difference, after − before (ours: −3.04)'), y = NULL) + bare
save_fig(gh, 'l28_hypnotism.png', 840, 280)

#---- the sampling distribution of x-bar1 - x-bar2 (a concept picture; illustrative values, no axis numbers).
#Left: x-bar1 ~ N(mu1, SE1 = 1) and x-bar2 ~ N(mu2, SE2 = 0.75), with mu1 = 10, mu2 = 7.
#Right: x-bar1 - x-bar2 ~ N(mu1 - mu2 = 3, sqrt(1^2 + 0.75^2) = 1.25): centred at the true difference and
#wider than either mean's distribution, because the variances add. The dashed line marks 0, the value H0 claims.
m1 <- 10; m2 <- 7; s1 <- 1; s2 <- 0.75; sd_ <- sqrt(s1^2 + s2^2)
stopifnot(sd_ == 1.25)
lx <- seq(3.5, 13.5, length.out = 800)
left <- rbind(data.frame(x = lx, y = dnorm(lx, m1, s1), g = '1'), data.frame(x = lx, y = dnorm(lx, m2, s2), g = '2'))
pk <- dnorm(m2, m2, s2)
GOLD <- "#B8860B"
gl <- ggplot(left, aes(x, y, group = g, colour = g, fill = g)) +
  geom_area(alpha = 0.25, position = 'identity') + geom_line(linewidth = 1.1) +
  scale_colour_manual(values = c('1' = NAVY, '2' = GOLD), guide = 'none') +
  scale_fill_manual(values = c('1' = NAVY, '2' = GOLD), guide = 'none') +
  geom_vline(xintercept = c(m1, m2), colour = c(NAVY, GOLD), linetype = 'dotted') +
  lab(m2, pk * 1.12, 'bar(x)[2]', GOLD, 0.5, 'bold', parse = TRUE) +
  lab(m1, dnorm(m1, m1, s1) * 1.2, 'bar(x)[1]', NAVY, 0.5, 'bold', parse = TRUE) +
  scale_x_continuous(breaks = c(m2, m1), labels = c(expression(mu[2]), expression(mu[1]))) +
  coord_cartesian(xlim = c(3.5, 13.5), ylim = c(0, pk * 1.25)) +
  labs(x = 'two sample means, each with its own SE', y = NULL) + bare
rx <- seq(-2.5, 8.5, length.out = 800)
right <- data.frame(x = rx, y = dnorm(rx, m1 - m2, sd_)); pr <- dnorm(0, 0, sd_)
gr2 <- ggplot(right, aes(x, y)) +
  geom_area(fill = RED, alpha = 0.18) + geom_line(colour = RED, linewidth = 1.1) +
  geom_vline(xintercept = m1 - m2, colour = RED, linetype = 'dotted') +
  geom_vline(xintercept = 0, colour = 'grey30', linetype = 'dashed') +
  annotate('segment', x = m1 - m2, xend = m1 - m2 + sd_, y = dnorm(sd_, 0, sd_), yend = dnorm(sd_, 0, sd_),
           colour = 'black', arrow = arrow(length = unit(0.15, 'cm'), ends = 'both')) +
  lab(m1 - m2 + sd_ + 0.2, dnorm(sd_, 0, sd_), 'SE', 'black', 0, 'bold') +
  lab(m1 - m2, pr * 1.18, 'bar(x)[1] - bar(x)[2]', RED, 0.5, 'bold', parse = TRUE) +
  lab(0.15, pk * 1.02, 'H[0]:~mu[1] - mu[2] == 0', 'grey30', 0, 'plain', parse = TRUE) +
  scale_x_continuous(breaks = c(0, m1 - m2), labels = c('0', expression(mu[1] - mu[2]))) +
  coord_cartesian(xlim = c(-2.5, 8.5), ylim = c(0, pk * 1.25)) +   # same height scale as the left panel, so 'wider' shows
  labs(x = 'the difference: wider, because variances add', y = NULL) + bare
gd <- gridExtra::arrangeGrob(gl, gr2, ncol = 2)
ggsave(file.path(assets_dir, 'l28_diff_dist.png'), gd, width = 840 / 72, height = 300 / 72, dpi = 200, bg = 'white')
sizes[nrow(sizes) + 1, ] <- list('l28_diff_dist.png', 840, 300)

write.csv(sizes, file.path(assets_dir, 'l28_figure_sizes.csv'), row.names = FALSE)
print(sizes)
