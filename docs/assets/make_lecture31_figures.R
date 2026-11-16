#Checks the numbers on the Week 13 Lecture 31 deck (11/16/2026): the living-arrangements tables (homogeneity,
#OpenStax Example 11.8) and McNemar's test. The goodness-of-fit and independence slides carried over from
#Lecture 30 are checked in make_lecture30_figures.R. It draws:
#
#  l31_saliva.png        chi-square(1) curve: X^2 = 20.63 for saliva vs swab is far beyond the right edge
#  l31_figure_sizes.csv  the size, in points, each figure is drawn at
#
#The same numbers are asserted on the Week 13 notes page (lecturemenu13.Rmd, chunk two-way-setup).
#
#McNEMAR DATA (both verified against the published tables, Nov 16 2026):
#  de Oliveira et al. (2021), J. Virol. Methods 296:114241, Table 1, protocol T: 400 patients, saliva vs
#    oro/nasopharyngeal swab; both + 119, saliva only 5, swab only 33, both - 243.
#  Bouska et al. (2022), Viruses 14(12):2829, Table 2: 494 patients, gargle lavage vs nasopharyngeal swab;
#    both + 255, gargle only 20, swab only 20, both - 199.
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

pv <- function(x, df) pchisq(x, df, lower.tail = FALSE)

#---- living arrangements (homogeneity): expected counts and contributions shown on the slides
O <- matrix(c(72, 84, 49, 45, 91, 86, 88, 35), 2, byrow = TRUE)
E <- outer(rowSums(O), colSums(O)) / sum(O)
stopifnot(identical(rowSums(O), c(250, 300)), identical(colSums(O), c(163, 170, 137, 80)), sum(O) == 550,
          identical(round(E, 2), matrix(c(74.09, 77.27, 62.27, 36.36, 88.91, 92.73, 74.73, 43.64), 2, byrow = TRUE)),
          identical(round((O - E)^2 / E, 3), matrix(c(0.059, 0.586, 2.829, 2.051, 0.049, 0.488, 2.357, 1.709), 2, byrow = TRUE)),
          round(sum(round((O - E)^2 / E, 3)), 2) == 10.13, round(sum((O - E)^2 / E), 2) == 10.13)

#---- McNemar: saliva vs swab (b = saliva positive only, c = swab positive only)
a <- 119; b <- 5; c <- 33; d <- 243; n <- a + b + c + d
X2 <- (b - c)^2 / (b + c); z <- (b - c) / sqrt(b + c)
stopifnot(n == 400, (a + b) / n == 0.31, (a + c) / n == 0.38, b + c == 38, (b - c)^2 == 784,
          round(X2, 2) == 20.63, round(z, 2) == -4.54, all.equal(z^2, X2),
          round(qchisq(1 - 0.0005, 1), 3) == 12.116, pv(X2, 1) < 0.0005, signif(pv(X2, 1), 2) == 5.6e-06,
          round(unname(mcnemar.test(matrix(c(a, c, b, d), 2), correct = FALSE)$statistic), 2) == 20.63,
          round((abs(b - c) - 1)^2 / (b + c), 2) == 19.18,
          #the sign-test view: b ~ Binomial(b + c, 1/2) under H0
          round((b - (b + c) / 2) / (sqrt(b + c) / 2), 2) == -4.54)

#---- McNemar: gargle vs swab
g <- c(a = 255, b = 20, c = 20, d = 199)
stopifnot(sum(g) == 494, (g[['a']] + g[['b']]) / 494 == (g[['a']] + g[['c']]) / 494,
          round((g[['a']] + g[['b']]) / 494, 3) == 0.557, (g[['b']] - g[['c']])^2 / (g[['b']] + g[['c']]) == 0,
          pv(0, 1) == 1, g[['a']] + g[['d']] == 454)

#---- type size helpers: points in, refuse anything under 16pt
MIN_PT <- 16
tsz <- function(pt) { stopifnot(pt >= MIN_PT); pt / .pt }
el <- function(pt, ...) { stopifnot(pt >= MIN_PT); element_text(size = pt, ...) }
NAVY <- "#1F3864"; SHADE <- "#8FAADC"; RED <- "#C00000"
bare <- theme_classic() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank(),
        axis.text.x = el(16, colour = 'black'), axis.title.x = el(16), plot.margin = margin(8, 14, 4, 8))
lab <- function(x, y, s, col, hj, face = 'plain', pt = 16, parse = FALSE)
  annotate('label', x = x, y = y, label = s, colour = col, hjust = hj, size = tsz(pt), fontface = face, parse = parse,
           fill = 'white', border.colour = NA, label.padding = unit(0.1, 'lines'))
sizes <- data.frame(file = character(), width_pt = numeric(), height_pt = numeric())
save_fig <- function(g, file, wpt, hpt) {
  ggsave(file.path(assets_dir, file), g, width = wpt / 72, height = hpt / 72, dpi = 200, bg = 'white')
  sizes[nrow(sizes) + 1, ] <<- list(file, wpt, hpt)
}

#---- saliva vs swab: chi-square(1); 20.63 is off the chart (the table's last column, 12.116, is already far out)
gx <- seq(0.05, 16, length.out = 900)
d1 <- data.frame(x = gx, y = dchisq(gx, 1)); top <- 1.2
gs <- ggplot(d1, aes(x, y)) +
  geom_area(fill = SHADE, alpha = 0.5) +
  geom_line(colour = NAVY, linewidth = 1.1) +
  geom_vline(xintercept = 12.116, colour = RED, linetype = 'dashed') +
  lab(15.7, top * 0.95, 'bold("if"~H[0]~"is true:"~~X^2~"~"~chi^2*(1))', NAVY, 1, 'bold', parse = TRUE) +
  lab(11.8, top * 0.55, 'table: 12.116 cuts off 0.0005', RED, 1, 'bold') +
  lab(15.7, top * 0.28, 'ours: 20.63, off this edge', 'black', 1, 'bold') +
  annotate('segment', x = 12.6, xend = 15.9, y = top * 0.16, yend = top * 0.16, colour = 'black',
           arrow = arrow(length = unit(0.2, 'cm'))) +
  scale_x_continuous(breaks = c(0, 1, 4, 8, 12.116), labels = c('0', '1', '4', '8', '12.116')) +
  coord_cartesian(xlim = c(0, 16), ylim = c(0, top), expand = FALSE) +
  labs(x = expression(X^2~'value (mean of'~chi^2*(1)~'is 1)'), y = NULL) + bare
save_fig(gs, 'l31_saliva.png', 840, 270)

write.csv(sizes, file.path(assets_dir, 'l31_figure_sizes.csv'), row.names = FALSE)
print(sizes)
