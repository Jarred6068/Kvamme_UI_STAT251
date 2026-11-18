#Checks the numbers on the Week 13 Lecture 32 deck (11/18/2026): Cramer's V on the two-way tables from Lecture 31,
#covariance and correlation on the 2010 National League team statistics, and the Pearson correlation test (the
#OpenStax third-exam / final-exam example and the NL teams). It draws:
#
#  l32_axes.png          the same ERA-vs-wins scatterplot on tight and on wide axes ("which is stronger?")
#  l32_shapes.png        four simulated scatterplots: strong positive, weak negative, curved, no association
#  l32_nl.png            2010 NL teams: runs scored vs wins, and team ERA vs wins
#  l32_guess.png         six simulated scatterplots, A-F, for "guess the correlation" (r revealed on the slide)
#  l32_cautions.png      a curved pattern with r near 0, and one outlier that creates a correlation
#  l32_exam_t.png        t(9) curve with both tails beyond t = +/-2.66 shaded (exam-score example)
#  l32_figure_sizes.csv  the size, in points, each figure is drawn at
#
#The same numbers are asserted on the Week 13 notes page (lecturemenu13.Rmd, chunk association-setup).
#
#DATA
#  docs/Data/NL_team_statistics.csv: the 16 National League teams, 2010 regular season (162 games): runs scored,
#    batting average, runs per game (RPG = Runs / 162), wins, team ERA. The season is fixed by the standings in
#    the file (Philadelphia 97 wins, San Francisco 92, Cincinnati 91, Pittsburgh 57 = the 2010 NL).
#  OpenStax Introductory Statistics 2e, Section 12.3 (third exam / final exam, 11 students) and Section 12.4
#    (r = 0.6631, n = 11, P-value 0.026, critical r 0.602 for df = 9).
#
#TYPE SIZE. Same rule as the other make_lectureN_figures.R scripts: nothing on a slide is under 16pt.

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

tstat <- function(r, n) r * sqrt(n - 2) / sqrt(1 - r^2)
V <- function(X2, n, k) sqrt(X2 / (n * (k - 1)))
tcrit <- function(df) round(qt(1 - c(0.10, 0.05, 0.01, 0.005, 0.001) / 2, df), 3)   # two-tail columns of the t table

#---- Cramer's V on the Lecture 31 tables (X^2 values checked in make_lecture30/31_figures.R)
stopifnot(round(V(10.13, 550, 2), 3) == 0.136,              # living arrangements, 2 x 4, k = 2
          round(V(10.13 * 10, 5500, 2), 3) == 0.136,        # the same proportions with 10 times the students
          round(10.13 * 10, 1) == 101.3,
          round(V(0.703, 755, 2), 3) == 0.031, round(V(0.70, 755, 2), 2) == 0.03, pchisq(101.3, 3, lower.tail = FALSE) < 0.0005,              # speeding and cell phones, 2 x 2
          round(V(48.42, 400, 3), 3) == 0.246,              # anxiety and need to succeed, 3 x 5, k = 3
          round(V(256.3, 400, 2), 2) == 0.80)               # saliva vs swab treated as independence

#---- 2010 National League
nl <- read.csv(file.path(dirname(assets_dir), 'Data', 'NL_team_statistics.csv'))
stopifnot(nrow(nl) == 16, nl$W[nl$Team == 'Philadelphia'] == 97, nl$W[nl$Team == 'Pittsburgh'] == 57,
          max(abs(nl$RPG * 162 - nl$Runs)) < 1e-6)
rR <- cor(nl$Runs, nl$W); rE <- cor(nl$ERA, nl$W)
stopifnot(round(rR, 3) == 0.624, round(rE, 3) == -0.850,
          round(cov(nl$Runs, nl$W), 1) == 392.7, round(cov(nl$RPG, nl$W), 2) == 2.42,
          all.equal(cov(nl$Runs, nl$W) / cov(nl$RPG, nl$W), 162), all.equal(cor(nl$RPG, nl$W), rR),
          round(mean(nl$Runs), 1) == 700.7, round(mean(nl$W), 1) == 80.5,
          round(sd(nl$Runs), 1) == 58.4, round(sd(nl$W), 1) == 10.8,
          #runs vs wins: two-sided test, df = 14; the hand value from r = 0.624 must clear the same table column
          round(tstat(rR, 16), 2) == 2.99, round(tstat(0.624, 16), 2) == 2.99,
          identical(tcrit(14), c(1.761, 2.145, 2.977, 3.326, 4.140)),
          tstat(0.624, 16) > 2.977, tstat(0.624, 16) < 3.326, round(2 * pt(-tstat(rR, 16), 14), 4) == 0.0098,
          #ERA vs wins: lower-tailed, df = 14; |t| beyond the last column (4.140, upper tail 0.0005)
          round(tstat(rE, 16), 2) == -6.04, round(tstat(-0.850, 16), 2) == -6.04,
          pt(tstat(rE, 16), 14) < 0.0005)

#a 4-team hand computation of r (Philadelphia, Arizona, San Francisco, Pittsburgh)
h <- nl[match(c('Philadelphia', 'Arizona', 'San Francisco', 'Pittsburgh'), nl$Team), ]
zx <- (h$Runs - mean(h$Runs)) / sd(h$Runs); zy <- (h$W - mean(h$W)) / sd(h$W)
stopifnot(identical(h$Runs, c(772L, 713L, 697L, 587L)), identical(h$W, c(97L, 65L, 92L, 57L)),
          mean(h$Runs) == 692.25, mean(h$W) == 77.75, round(sd(h$Runs), 2) == 77.22, round(sd(h$W), 2) == 19.72,
          round(cor(h$Runs, h$W), 2) == 0.77)

#---- OpenStax third exam / final exam (Section 12.3, Table 12.x; Section 12.4)
x3 <- c(65, 67, 71, 71, 66, 75, 67, 70, 71, 69, 69)
yF <- c(175, 133, 185, 163, 126, 198, 153, 163, 159, 151, 159)
rX <- cor(x3, yF)
stopifnot(round(rX, 4) == 0.6631, round(tstat(rX, 11), 2) == 2.66, round(tstat(0.663, 11), 2) == 2.66,
          identical(tcrit(9), c(1.833, 2.262, 3.250, 3.690, 4.781)),
          tstat(0.663, 11) > 2.262, tstat(0.663, 11) < 3.250, round(2 * pt(-tstat(rX, 11), 9), 3) == 0.026,
          #the book's 95% critical value of r for df = 9 gives the same decision
          round(qt(0.975, 9) / sqrt(9 + qt(0.975, 9)^2), 3) == 0.602, rX > 0.602,
          round(rX^2, 4) == 0.4397)

#---- type size helpers: points in, refuse anything under 16pt
MIN_PT <- 16
tsz <- function(pt) { stopifnot(pt >= MIN_PT); pt / .pt }
el <- function(pt, ...) { stopifnot(pt >= MIN_PT); element_text(size = pt, ...) }
NAVY <- "#1F3864"; SHADE <- "#8FAADC"; RED <- "#C00000"
pts <- function() geom_point(size = 3, shape = 21, colour = NAVY, fill = SHADE, stroke = 0.9)
sc <- theme_bw() + theme(axis.text = el(16, colour = 'black'), axis.title = el(16), plot.title = el(18, face = 'bold'),
                         panel.grid.minor = element_blank(), plot.margin = margin(6, 12, 4, 6))
blank <- theme_bw() + theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
                            panel.grid = element_blank(), plot.title = el(20, face = 'bold', hjust = 0.5),
                            plot.margin = margin(4, 6, 4, 6))
sizes <- data.frame(file = character(), width_pt = numeric(), height_pt = numeric())
save_fig <- function(g, file, wpt, hpt) {
  ggsave(file.path(assets_dir, file), g, width = wpt / 72, height = hpt / 72, dpi = 200, bg = 'white')
  sizes[nrow(sizes) + 1, ] <<- list(file, wpt, hpt)
}
side <- function(...) gridExtra::arrangeGrob(..., nrow = 1)

#---- the same ERA-vs-wins data on two sets of axes
a1 <- ggplot(nl, aes(ERA, W)) + pts() + labs(x = 'Team ERA', y = 'Wins', title = 'Plot 1') + sc
a2 <- ggplot(nl, aes(ERA, W)) + pts() + labs(x = 'Team ERA', y = 'Wins', title = 'Plot 2') +
  scale_x_continuous(limits = c(0, 10)) + scale_y_continuous(limits = c(0, 162)) + sc
save_fig(side(a1, a2), 'l32_axes.png', 840, 290)

#---- four shapes (simulated)
set.seed(32)
n0 <- 40; u <- runif(n0, 0, 10)
shp <- list(A = data.frame(x = u, y = 2 + 0.9 * u + rnorm(n0, 0, 0.8)),
            B = data.frame(x = u, y = 8 - 0.35 * u + rnorm(n0, 0, 1.0)),
            C = data.frame(x = u, y = 0.25 * (u - 5)^2 + rnorm(n0, 0, 0.5)),
            D = data.frame(x = u, y = rnorm(n0, 5, 1.5)))
r_shp <- sapply(shp, function(d) cor(d$x, d$y))
stopifnot(identical(unname(round(r_shp, 2)), c(0.95, -0.51, 0.49, -0.06)))
sp <- lapply(names(shp), function(k) ggplot(shp[[k]], aes(x, y)) + pts() + ggtitle(k) + blank)
save_fig(do.call(side, sp), 'l32_shapes.png', 840, 230)

#---- 2010 NL: runs vs wins, ERA vs wins
n1 <- ggplot(nl, aes(Runs, W)) + pts() + labs(x = 'Runs scored', y = 'Wins') + sc
n2 <- ggplot(nl, aes(ERA, W)) + pts() + labs(x = 'Team ERA (runs allowed per 9 innings)', y = 'Wins') + sc
save_fig(side(n1, n2), 'l32_nl.png', 840, 290)

#---- guess the correlation (simulated bivariate normal, n = 50)
gen <- function(rho, n = 50) { z1 <- rnorm(n); z2 <- rnorm(n); data.frame(x = z1, y = rho * z1 + sqrt(1 - rho^2) * z2) }
set.seed(251)
gs <- list(A = gen(0.95), B = gen(-0.5), C = gen(0), D = gen(0.6), E = gen(-0.9), F = gen(0.25))
r_gs <- sapply(gs, function(d) cor(d$x, d$y))
stopifnot(identical(unname(round(r_gs, 2)), c(0.93, -0.55, -0.14, 0.71, -0.94, 0.40)))
gp <- lapply(names(gs), function(k) ggplot(gs[[k]], aes(x, y)) + pts() + ggtitle(k) + blank)
save_fig(do.call(side, gp), 'l32_guess.png', 840, 170)

#---- cautions: a curve with r near 0; one outlier creating a correlation
set.seed(7)
cu <- data.frame(x = seq(0, 10, length.out = 30)); cu$y <- 20 - 0.8 * (cu$x - 5)^2 + rnorm(30, 0, 1.2)
ou <- data.frame(x = runif(15, 0, 4), y = runif(15, 0, 4)); ou <- rbind(ou, data.frame(x = 12, y = 12))
r_cu <- cor(cu$x, cu$y); r_ou <- cor(ou$x, ou$y); r_ou15 <- cor(ou$x[1:15], ou$y[1:15])
stopifnot(round(r_cu, 2) == 0, round(r_ou, 2) == 0.90, round(r_ou15, 2) == 0.22)
c1 <- ggplot(cu, aes(x, y)) + pts() + ggtitle('A strong curve') + blank
c2 <- ggplot(ou, aes(x, y)) + pts() + ggtitle('One unusual point') + blank
save_fig(side(c1, c2), 'l32_cautions.png', 840, 250)

#---- exam-score P-value: t(9), both tails beyond +/-2.66
tx <- seq(-4.5, 4.5, length.out = 900); dt9 <- data.frame(x = tx, y = dt(tx, 9)); top <- 0.45
lab <- function(x, y, s, col, hj, pt = 16, parse = FALSE)
  annotate('label', x = x, y = y, label = s, colour = col, hjust = hj, size = tsz(pt), fontface = 'bold', parse = parse,
           fill = 'white', border.colour = NA, label.padding = unit(0.1, 'lines'))
ge <- ggplot(dt9, aes(x, y)) +
  geom_area(fill = SHADE, alpha = 0.5) +
  geom_area(data = subset(dt9, x >= 2.66), fill = RED, alpha = 0.85) +
  geom_area(data = subset(dt9, x <= -2.66), fill = RED, alpha = 0.85) +
  geom_line(colour = NAVY, linewidth = 1.1) +
  lab(-4.4, top * 0.93, 'bold("if"~H[0]:rho==0~"is true:"~~t~"~"~t*(9))', NAVY, 0, parse = TRUE) +
  lab(4.4, top * 0.93, 'P-value = sum of both red tails', RED, 1) +
  lab(4.4, top * 0.75, 'between 0.01 and 0.05 (software: 0.026)', 'black', 1) +
  scale_x_continuous(breaks = c(-2.66, 0, 2.66), labels = c('−2.66', '0', '2.66')) +
  coord_cartesian(xlim = c(-4.5, 4.5), ylim = c(0, top), expand = FALSE) +
  labs(x = expression(t~'value  (table, df = 9: 2.262 for 0.05, 3.250 for 0.01)'), y = NULL) +
  theme_classic() + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank(),
                          axis.text.x = el(16, colour = 'black'), axis.title.x = el(16), plot.margin = margin(8, 14, 4, 8))
save_fig(ge, 'l32_exam_t.png', 840, 270)

write.csv(sizes, file.path(assets_dir, 'l32_figure_sizes.csv'), row.names = FALSE)
print(sizes)
