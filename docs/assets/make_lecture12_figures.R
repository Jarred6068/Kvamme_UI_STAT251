#Builds the figures for the Week 6 Lecture 12 deck (9/28/2026): the empirical rule,
#standardizing, the z-table, outlier rules and linear transformations.
#
#  l12_sigma_quiz.png       two bell curves at different axis scales - read sigma off the axis
#  l12_empirical_rule.png   68-95-99.7 with the 34 / 13.5 / 2.35 / 0.15 pieces
#  l12_exam_rule.png        exam scores N(74, 8), three shaded regions, NO percentages
#  l12_standardize_axes.png the female heights on an inches axis and on a z axis
#  l12_outlier_tails.png    the +/-2s rule, two sided and one sided
#  l12_iqr_vs_2s.png        where 1.5xIQR and +/-2s disagree on the same data
#  l12_ztable.png           a crop of the standard normal table, z = 1.36 highlighted
#  l12_skew_z.png           a skewed variable and its z-scores - identical shape
#  l12_bell_examples.png    three real bell-shaped distributions (opening question)
#  l12_bell_examples_fit.png  the same three with their fitted normal curves
#  l12_dice_means.png       averages of 1, 2, 5, 10 dice becoming a bell (why the normal matters)
#  l12_temps_density.png    Central Park Januaries: histogram on a density scale + curve
#  l12_temps_area.png       the same curve with 28-36 F shaded: area = proportion
#
#  l12_figure_sizes.csv     the size, in points, each figure is drawn at
#
#TYPE SIZE. Same rule as make_lecture8/11_figures.R: nothing on a slide may be under
#16pt, text inside a picture included, so every figure is drawn at EXACTLY the size it
#occupies on the slide (width_in = points / 72) and the build script places it at that
#size, read from l12_figure_sizes.csv. Every text size goes through tsz() or el(),
#which refuse anything under 16pt.
#
#ONE RUNNING DATASET. The deck uses the 262 female heights from docs/Data/heights.csv -
#the same data as the 2024 Lecture 7 deck, xbar = 65.4 in, s = 3.38 in - for
#standardizing, the z-table lookup, the outlier comparison and the unit conversion. The
#empirical-rule example deliberately uses the exam from the carried slide ("Which one is
#further from typical?", mean 74, s 8) instead of a second height distribution, because
#two height examples with different standard deviations read as a contradiction.
#
#Every number the slides print is asserted below.

suppressPackageStartupMessages({
  library(ggplot2)
  library(ggpubr)
})

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
data_dir   <- file.path(dirname(assets_dir), "Data")

NAVY <- "#1F3864"
BLUE <- "#4472C4"
PALE <- "#D9E5F7"
RED  <- "#C00000"
GOLD <- "#BF8F00"
GREY <- "#595959"

MIN_PT <- 16
tsz <- function(pt) { stopifnot(pt >= MIN_PT); pt / ggplot2::.pt }
el  <- function(pt, ...) { stopifnot(pt >= MIN_PT); element_text(size = pt, ...) }

sizes <- data.frame(file = character(), w_pt = numeric(), h_pt = numeric(),
                    stringsAsFactors = FALSE)
save_fig <- function(p, file, w_pt, h_pt) {
  ggsave(file.path(assets_dir, file), p, width = w_pt / 72, height = h_pt / 72,
         dpi = 300, bg = "white")
  sizes[nrow(sizes) + 1, ] <<- list(file, w_pt, h_pt)
}

#a normal curve over [lo, hi], optionally with the region [a, b] filled
bell <- function(mu, sd, lo, hi, a = NA, b = NA, fill = PALE) {
  xs <- seq(lo, hi, length.out = 600)
  d  <- data.frame(x = xs, y = dnorm(xs, mu, sd))
  layers <- list()
  if (!is.na(a)) {
    dd <- d[d$x >= a & d$x <= b, ]
    dd <- rbind(data.frame(x = a, y = 0), dd, data.frame(x = b, y = 0))
    layers <- c(layers, list(geom_polygon(data = dd, aes(x, y), fill = fill)))
  }
  c(layers, list(geom_line(data = d, aes(x, y), linewidth = 1.1, colour = NAVY)))
}

bare <- function(...) {
  theme_classic() +
    theme(axis.title.x = el(16), axis.text.x = el(16),
          axis.title.y = element_blank(), axis.text.y = element_blank(),
          axis.ticks.y = element_blank(), axis.line.y = element_blank(),
          plot.title = el(16, hjust = 0.5),
          plot.margin = margin(...))
}

#----------------------------------------------------------------------------
#1. read sigma off the axis. Two panels with the SAME drawn shape and different
#axis scales, so the answer cannot be guessed from the picture.

quiz_panel <- function(sd, lo, hi, brk, title) {
  ggplot() + bell(0, sd, lo, hi, lo, hi) +
    annotate("segment", x = 0, xend = 0, y = 0, yend = dnorm(0, 0, sd),
             colour = RED, linewidth = 1.0) +
    scale_x_continuous(breaks = brk) +
    coord_cartesian(xlim = c(lo, hi), ylim = c(0, dnorm(0, 0, sd) * 1.12),
                    expand = FALSE) +
    labs(x = NULL, title = title) +
    bare(6, 10, 2, 10)
}
p_quiz <- ggarrange(
  quiz_panel(5,   -18,  18,  seq(-15, 15, by = 5),    "A"),
  quiz_panel(0.5, -1.8, 1.8, seq(-1.5, 1.5, by = 0.5), "B"),
  ncol = 2)
save_fig(p_quiz, "l12_sigma_quiz.png", 840, 215)

#----------------------------------------------------------------------------
#2. the empirical rule. Percentages for the six pieces sit inside the curve where
#there is room and just above it in the tails, where there is not.

lab_at <- function(x, y, txt, colour = NAVY, sz = 16) {
  annotate("text", x = x, y = y, label = txt, size = tsz(sz), colour = colour)
}
span <- function(a, b, y, txt) {
  list(annotate("segment", x = a, xend = b, y = y, yend = y, colour = GREY,
                linewidth = 0.7, arrow = arrow(length = unit(6, "pt"),
                                               ends = "both", type = "closed")),
       annotate("text", x = 0, y = y + 0.012, label = txt, vjust = 0,
                size = tsz(16), colour = GREY))
}
p_emp <- ggplot() +
  bell(0, 1, -4.2, 4.2, -3, 3) +
  lapply(c(-3, -2, -1, 1, 2, 3), function(k)
    annotate("segment", x = k, xend = k, y = 0, yend = dnorm(k),
             colour = NAVY, linetype = "dashed", linewidth = 0.6)) +
  annotate("segment", x = 0, xend = 0, y = 0, yend = dnorm(0),
           colour = RED, linewidth = 0.9) +
  lab_at(-0.5, 0.13, "34%") + lab_at(0.5, 0.13, "34%") +
  lab_at(-1.5, 0.045, "13.5%") + lab_at(1.5, 0.045, "13.5%") +
  #the 2.35% slivers have no room for a label inside them, so the label sits above
  #the curve with a leader that stops clear of the text
  lab_at(-2.5, 0.085, "2.35%") + lab_at(2.5, 0.085, "2.35%") +
  annotate("segment", x = -2.5, xend = -2.5, y = 0.070, yend = 0.022,
           colour = NAVY, linewidth = 0.5) +
  annotate("segment", x = 2.5, xend = 2.5, y = 0.070, yend = 0.022,
           colour = NAVY, linewidth = 0.5) +
  span(-1, 1, 0.45, "about 68%") +
  span(-2, 2, 0.52, "about 95%") +
  span(-3, 3, 0.59, "about 99.7%") +
  scale_x_continuous(
    breaks = -3:3,
    labels = parse(text = c("bar(x)-3*s", "bar(x)-2*s", "bar(x)-s", "bar(x)",
                            "bar(x)+s", "bar(x)+2*s", "bar(x)+3*s"))) +
  coord_cartesian(xlim = c(-4.2, 4.2), ylim = c(0, 0.65), expand = FALSE) +
  labs(x = NULL) +
  bare(6, 12, 2, 12)
save_fig(p_emp, "l12_empirical_rule.png", 840, 250)

#----------------------------------------------------------------------------
#3. the exam example, N(74, 8). Regions are shaded but NOT labelled - the
#percentages are the answers and are revealed in the slide text.

EX_MU <- 74; EX_SD <- 8
stopifnot(EX_MU - EX_SD == 66, EX_MU + EX_SD == 82,
          EX_MU - 2 * EX_SD == 58, EX_MU + 2 * EX_SD == 90)

exam_panel <- function(a, b, title) {
  ggplot() + bell(EX_MU, EX_SD, 46, 102, a, b) +
    annotate("segment", x = EX_MU, xend = EX_MU, y = 0, yend = dnorm(EX_MU, EX_MU, EX_SD),
             colour = RED, linewidth = 0.9) +
    scale_x_continuous(breaks = seq(50, 100, by = 10)) +
    coord_cartesian(xlim = c(46, 102), ylim = c(0, dnorm(EX_MU, EX_MU, EX_SD) * 1.15),
                    expand = FALSE) +
    labs(x = NULL, title = title) +
    bare(6, 8, 2, 8)
}
p_exam <- ggarrange(
  exam_panel(66, 82,  "(a)  66 to 82 points"),
  exam_panel(82, 102, "(b)  82 points and above"),
  exam_panel(58, 90,  "(c)  58 to 90 points"),
  ncol = 3)
save_fig(p_exam, "l12_exam_rule.png", 840, 205)

#----------------------------------------------------------------------------
#4. standardizing: the same distribution on an inches axis and on a z axis.

h <- read.csv(file.path(data_dir, "heights.csv"))
fh <- h$HEIGHT[h$GENDER == "Female"]
HM <- round(mean(fh), 1)     # 65.4
HS <- round(sd(fh), 2)       # 3.38
stopifnot(length(fh) == 262, HM == 65.4, HS == 3.38,
          round((70 - HM) / HS, 2) == 1.36,
          round(pnorm(1.36), 4) == 0.9131,
          round(HM * 2.54, 1) == 166.1, round(HS * 2.54, 2) == 8.59,
          round((70 * 2.54 - HM * 2.54) / (HS * 2.54), 2) == 1.36)

std_panel <- function(mu, sd, lo, hi, brk, mark, xlab, marklab) {
  ggplot() + bell(mu, sd, lo, hi, lo, hi) +
    annotate("segment", x = mu, xend = mu, y = 0, yend = dnorm(mu, mu, sd),
             colour = RED, linewidth = 0.9) +
    #the marker runs past the curve so its label never sits on the curve itself
    annotate("segment", x = mark, xend = mark, y = 0, yend = dnorm(mu, mu, sd) * 1.04,
             colour = GOLD, linewidth = 1.1) +
    annotate("text", x = mark, y = dnorm(mu, mu, sd) * 1.07,
             label = marklab, vjust = 0, hjust = 0.4, size = tsz(16), colour = GOLD) +
    scale_x_continuous(breaks = brk) +
    coord_cartesian(xlim = c(lo, hi), ylim = c(0, dnorm(mu, mu, sd) * 1.22),
                    expand = FALSE) +
    labs(x = xlab) +
    bare(6, 12, 2, 12)
}
p_std <- ggarrange(
  std_panel(HM, HS, 54, 77, seq(56, 76, by = 4), 70,
            "height in inches", "70 in"),
  std_panel(0, 1, -3.4, 3.4, -3:3, 1.36,
            "the same heights as z-scores", "z = 1.36"),
  ncol = 2)
save_fig(p_std, "l12_standardize_axes.png", 840, 215)

#----------------------------------------------------------------------------
#5. the +/-2s rule: two sided, then each one sided version.

tail_panel <- function(lo_cut, hi_cut, title) {
  g <- ggplot() + bell(0, 1, -3.6, 3.6, -3.6, 3.6, fill = "#EFEFEF")
  if (!is.na(lo_cut)) {
    g <- g + bell(0, 1, -3.6, lo_cut, -3.6, lo_cut, fill = "#F4B7B7")[1] +
      annotate("segment", x = lo_cut, xend = lo_cut, y = 0, yend = dnorm(lo_cut),
               colour = RED, linetype = "dashed", linewidth = 0.8)
  }
  if (!is.na(hi_cut)) {
    g <- g + bell(0, 1, hi_cut, 3.6, hi_cut, 3.6, fill = "#F4B7B7")[1] +
      annotate("segment", x = hi_cut, xend = hi_cut, y = 0, yend = dnorm(hi_cut),
               colour = RED, linetype = "dashed", linewidth = 0.8)
  }
  g + geom_line(data = data.frame(x = seq(-3.6, 3.6, length.out = 600)),
                aes(x = x, y = dnorm(x)), linewidth = 1.1, colour = NAVY) +
    scale_x_continuous(breaks = c(-2, 0, 2),
                       labels = parse(text = c("bar(x)-2*s", "bar(x)", "bar(x)+2*s"))) +
    coord_cartesian(xlim = c(-3.6, 3.6), ylim = c(0, 0.52), expand = FALSE) +
    labs(x = NULL, title = title) +
    bare(6, 10, 2, 10)
}
p_tails <- ggarrange(
  tail_panel(-2, 2,  "both tails: 2.5% + 2.5% = 5%") +
    annotate("text", x = 0, y = 0.18, label = "95%", size = tsz(16), colour = NAVY),
  tail_panel(NA, 2,  "upper tail only: 2.5%") +
    annotate("text", x = -0.4, y = 0.18, label = "97.5%", size = tsz(16), colour = NAVY),
  tail_panel(-2, NA, "lower tail only: 2.5%") +
    annotate("text", x = 0.4, y = 0.18, label = "97.5%", size = tsz(16), colour = NAVY),
  ncol = 3)
save_fig(p_tails, "l12_outlier_tails.png", 840, 205)

#----------------------------------------------------------------------------
#6. where the two outlier rules disagree, on the running height data.
#The single 92-inch value inflates s, which widens the +/-2s fence past the seven
#72-inch students that the 1.5 x IQR rule flags.

q  <- quantile(fh, c(0.25, 0.75), type = 7)
iq <- as.numeric(q[2] - q[1])
f_lo <- as.numeric(q[1] - 1.5 * iq); f_hi <- as.numeric(q[2] + 1.5 * iq)
s_lo <- mean(fh) - 2 * sd(fh);       s_hi <- mean(fh) + 2 * sd(fh)
by_iqr <- sort(unique(fh[fh < f_lo | fh > f_hi]))
by_2s  <- sort(unique(fh[fh < s_lo | fh > s_hi]))
stopifnot(iq == 3, f_lo == 59.5, f_hi == 71.5,
          round(s_lo, 2) == 58.63, round(s_hi, 2) == 72.14,
          identical(by_iqr, c(56, 57, 58, 72, 76, 77, 92)),
          identical(by_2s,  c(56, 57, 58, 76, 77, 92)),
          sum(fh == 72) == 7)

hd <- as.data.frame(table(fh), stringsAsFactors = FALSE)
names(hd) <- c("x", "n"); hd$x <- as.numeric(hd$x)
hd$only_iqr <- hd$x %in% setdiff(by_iqr, by_2s)
p_rules <- ggplot(hd, aes(x = x, y = n)) +
  geom_col(aes(fill = only_iqr), width = 0.75, colour = "grey35", linewidth = 0.3,
           show.legend = FALSE) +
  scale_fill_manual(values = c("TRUE" = GOLD, "FALSE" = PALE)) +
  #both fences and the disputed bar are labelled out in the empty right half, with
  #leader lines back to what they point at, so no label sits on top of the bars
  annotate("segment", x = f_hi, xend = f_hi, y = 0, yend = 38, colour = BLUE,
           linetype = "dashed", linewidth = 0.9) +
  annotate("segment", x = f_hi, xend = 74.2, y = 38, yend = 38, colour = BLUE,
           linewidth = 0.5) +
  annotate("text", x = 74.6, y = 38, label = "1.5 x IQR fence: 71.5",
           hjust = 0, vjust = 0.4, size = tsz(16), colour = BLUE) +
  annotate("segment", x = s_hi, xend = s_hi, y = 0, yend = 26, colour = RED,
           linetype = "dashed", linewidth = 0.9) +
  annotate("segment", x = s_hi, xend = 74.2, y = 26, yend = 26, colour = RED,
           linewidth = 0.5) +
  annotate("text", x = 74.6, y = 26, label = "mean + 2s: 72.1",
           hjust = 0, vjust = 0.4, size = tsz(16), colour = RED) +
  annotate("segment", x = 72, xend = 74.2, y = 13, yend = 13, colour = GOLD,
           linewidth = 0.5) +
  annotate("segment", x = 72, xend = 72, y = 13, yend = 7.6, colour = GOLD,
           linewidth = 0.5) +
  annotate("text", x = 74.6, y = 13, label = "seven students at 72 in",
           hjust = 0, vjust = 0.4, size = tsz(16), colour = GOLD) +
  scale_x_continuous(breaks = seq(56, 92, by = 4)) +
  coord_cartesian(xlim = c(54, 94), ylim = c(0, 48), expand = FALSE) +
  labs(x = "height in inches, 262 female students") +
  bare(6, 12, 2, 12)
save_fig(p_rules, "l12_iqr_vs_2s.png", 840, 235)

#----------------------------------------------------------------------------
#7. the standard normal table, cropped to the rows around z = 1.36.

zr <- seq(1.0, 1.6, by = 0.1)
zc <- seq(0, 0.09, by = 0.01)
tb <- expand.grid(ci = seq_along(zc), ri = seq_along(zr))
tb$p <- sprintf("%.4f", pnorm(zr[tb$ri] + zc[tb$ci]))
tb$hit <- tb$ri == which(zr == 1.3) & tb$ci == which(abs(zc - 0.06) < 1e-9)
tb$band <- tb$ri == which(zr == 1.3) | tb$ci == which(abs(zc - 0.06) < 1e-9)
stopifnot(tb$p[tb$hit] == "0.9131")

p_tab <- ggplot(tb, aes(x = ci, y = -ri)) +
  geom_tile(aes(fill = ifelse(hit, "hit", ifelse(band, "band", "plain"))),
            colour = "grey70", linewidth = 0.4, show.legend = FALSE) +
  geom_text(aes(label = p, fontface = ifelse(hit, "bold", "plain")),
            size = tsz(16), colour = NAVY) +
  scale_fill_manual(values = c(hit = "#FFE08A", band = "#F2F6FC", plain = "white")) +
  annotate("text", x = seq_along(zc), y = -0.3, label = sprintf(".%02d", as.integer(round(zc * 100))),
           size = tsz(16), colour = GREY, fontface = "bold") +
  annotate("text", x = 0.25, y = -seq_along(zr), label = sprintf("%.1f", zr),
           size = tsz(16), colour = GREY, fontface = "bold", hjust = 1) +
  annotate("text", x = 0.25, y = -0.3, label = "z", size = tsz(16),
           colour = GREY, fontface = "bold", hjust = 1) +
  coord_cartesian(xlim = c(-0.5, length(zc) + 0.6),
                  ylim = c(-length(zr) - 0.7, 0.3), expand = FALSE) +
  theme_void() +
  theme(plot.margin = margin(2, 2, 2, 2))
save_fig(p_tab, "l12_ztable.png", 760, 230)

#----------------------------------------------------------------------------
#8. standardizing does not make a skewed variable normal.

set.seed(123)
xs <- rchisq(1000, 5)
zs <- (xs - mean(xs)) / sd(xs)
sk <- function(v) mean((v - mean(v))^3) / sd(v)^3
stopifnot(abs(sk(xs) - sk(zs)) < 1e-8)

skew_panel <- function(v, xlab, brk) {
  ggplot(data.frame(v = v), aes(v)) +
    geom_histogram(bins = 32, fill = PALE, colour = "grey35", linewidth = 0.3) +
    scale_x_continuous(breaks = brk) +
    labs(x = xlab) +
    bare(6, 12, 2, 12)
}
p_skew <- ggarrange(
  skew_panel(xs, "X, in its original units", seq(0, 20, by = 5)),
  skew_panel(zs, "the same values as z-scores", -2:4),
  ncol = 2)
save_fig(p_skew, "l12_skew_z.png", 760, 215)

#----------------------------------------------------------------------------
#9. the opening: three real distributions that share one shape. Drawn twice -
#bare for the question slide, then with the fitted normal curve over each for the
#definition slide. Every panel is on a density scale so the curves can overlay.
#  - the 262 female heights (the running data set)
#  - the mean January temperature in Central Park, 150 winters 1869-2018
#  - the average of 10 rolls of a fair die, as EXACT probabilities (convolution),
#    so there is no simulation noise in it

cp  <- read.csv(file.path(data_dir, "central_park_temps.csv"))
jan <- cp$JAN
CM <- round(mean(jan), 1); CS <- round(sd(jan), 1)
stopifnot(length(jan) == 150, min(cp$YEAR) == 1869, max(cp$YEAR) == 2018,
          CM == 32.0, CS == 4.6)

#exact distribution of the mean of k fair dice
dice_mean <- function(k) {
  p <- rep(1 / 6, 6)
  if (k > 1) for (i in 2:k) p <- stats::convolve(p, rev(rep(1 / 6, 6)), type = "open")
  data.frame(x = (k:(6 * k)) / k, p = p)
}
d10 <- dice_mean(10)
D_SD <- sqrt(35 / 12) / sqrt(10)
stopifnot(abs(sum(d10$p) - 1) < 1e-12, abs(sum(d10$x * d10$p) - 3.5) < 1e-12,
          abs(sqrt(sum((d10$x - 3.5)^2 * d10$p)) - D_SD) < 1e-12)

ex_panel <- function(title, xlab, fit = NULL, ...) {
  p <- ggplot() + list(...)
  if (!is.null(fit)) p <- p + fit
  p + labs(x = xlab, title = title) + bare(4, 8, 2, 8)
}
hist_layer <- function(v, bw) geom_histogram(aes(x = v, y = after_stat(density)),
                                             data = data.frame(v = v), binwidth = bw,
                                             fill = PALE, colour = "grey35", linewidth = 0.3)
fit_layer <- function(mu, sd, lo, hi) {
  xs <- seq(lo, hi, length.out = 400)
  list(geom_line(data = data.frame(x = xs, y = dnorm(xs, mu, sd)), aes(x, y),
                 colour = RED, linewidth = 1.1))
}
#the heights panel drops the one 92-inch value from the VIEW only (xlim), so the
#bell is not squashed into the left third of the panel
panels <- function(with_fit) {
  f1 <- if (with_fit) fit_layer(HM, HS, 55, 76) else NULL
  f2 <- if (with_fit) fit_layer(CM, CS, 18, 46) else NULL
  f3 <- if (with_fit) fit_layer(3.5, D_SD, 1.6, 5.4) else NULL
  t1 <- if (with_fit) "N(65.4, 3.38)" else "262 female students"
  t2 <- if (with_fit) "N(32.0, 4.6)" else "150 Januaries, Central Park"
  t3 <- if (with_fit) "N(3.5, 0.54)" else "average of 10 dice"
  ggarrange(
    ex_panel(t1, "height (inches)", f1, hist_layer(fh, 1),
             coord_cartesian(xlim = c(55, 77), expand = FALSE)),
    ex_panel(t2, "mean temp. (F)", f2, hist_layer(jan, 2),
             coord_cartesian(xlim = c(18, 46), expand = FALSE)),
    ex_panel(t3, "average roll", f3,
             geom_col(data = d10, aes(x = x, y = p * 10), width = 0.1,
                      fill = PALE, colour = "grey35", linewidth = 0.3),
             scale_x_continuous(breaks = 2:5),
             coord_cartesian(xlim = c(1.6, 5.4), expand = FALSE)),
    ncol = 3)
}
save_fig(panels(FALSE), "l12_bell_examples.png", 840, 230)
save_fig(panels(TRUE),  "l12_bell_examples_fit.png", 840, 230)

#----------------------------------------------------------------------------
#10. why the normal matters: averages of 1, 2, 5 and 10 dice. One die is flat;
#the average of a few is already a bell. Exact probabilities, no simulation.

dice_panel <- function(k) {
  d <- dice_mean(k)
  ggplot(d, aes(x = x, y = p * k)) +
    geom_col(width = if (k == 1) 0.8 else 0.8 / k, fill = PALE, colour = "grey35",
             linewidth = 0.3) +
    scale_x_continuous(breaks = c(1, 3.5, 6), labels = c("1", "3.5", "6")) +
    coord_cartesian(xlim = c(0.5, 6.5), expand = FALSE) +
    labs(x = NULL, title = if (k == 1) "1 die" else paste(k, "dice")) +
    bare(4, 6, 2, 6)
}
save_fig(ggarrange(dice_panel(1), dice_panel(2), dice_panel(5), dice_panel(10), ncol = 4),
         "l12_dice_means.png", 840, 200)

#----------------------------------------------------------------------------
#11. density curves, on the Central Park Januaries: the histogram on a density
#scale with the fitted normal curve over it. 12. area is proportion: the same
#curve with 28-36 F shaded, which the slide compares with the actual count.

A_LO <- 28; A_HI <- 36
AREA <- round(pnorm(A_HI, CM, CS) - pnorm(A_LO, CM, CS), 2)
N_IN <- sum(jan >= A_LO & jan < A_HI)
stopifnot(AREA == 0.62, N_IN == 92, round(N_IN / 150, 2) == 0.61)

p_dens <- ggplot() +
  hist_layer(jan, 2) +
  fit_layer(CM, CS, 18, 46) +
  annotate("text", x = 37.5, y = 0.085, label = "the density curve", hjust = 0,
           size = tsz(16), colour = RED) +
  annotate("text", x = 18.8, y = 0.085, label = "150 Januaries", hjust = 0,
           size = tsz(16), colour = GREY) +
  scale_y_continuous(breaks = c(0, 0.04, 0.08)) +
  coord_cartesian(xlim = c(18, 46), ylim = c(0, 0.105), expand = FALSE) +
  labs(x = "mean January temperature (F)", y = "density") +
  theme_classic() +
  theme(axis.title = el(16), axis.text = el(16), plot.margin = margin(6, 10, 2, 6))
save_fig(p_dens, "l12_temps_density.png", 620, 235)

p_area <- ggplot() +
  bell(CM, CS, 18, 46, A_LO, A_HI) +
  annotate("text", x = CM, y = 0.035, label = "area = ?", size = tsz(18),
           colour = NAVY, fontface = "bold") +
  scale_x_continuous(breaks = c(20, A_LO, CM, A_HI, 44)) +
  coord_cartesian(xlim = c(18, 46), ylim = c(0, 0.098), expand = FALSE) +
  labs(x = "mean January temperature (F)") +
  bare(6, 12, 2, 12)
save_fig(p_area, "l12_temps_area.png", 840, 200)

#----------------------------------------------------------------------------
#13-15. sampling distributions and the margin of error (the 2024 Lecture 8
#sequence, rebuilt on real data). The 262 women play the population. The random
#draws repeat the sampling-setup chunk of lecturemenu6.Rmd EXACTLY (same seed,
#same order), so the slides and the notes quote the same sample means.

mh <- h$HEIGHT[h$GENDER == "Male"]
MU <- mean(fh); SIG <- sd(fh); N20 <- 20; SE <- SIG / sqrt(N20)
set.seed(2)
s1 <- sample(fh, N20, replace = TRUE); s2 <- sample(fh, N20, replace = TRUE)
xbar <- replicate(10000, mean(sample(fh, N20, replace = TRUE)))
set.seed(117)
ms <- sample(mh, N20)
stopifnot(round(mean(s1), 2) == 66.33, round(mean(s2), 2) == 64.85,
          round(SE, 2) == 0.76, round(2 * SE, 1) == 1.5,
          round(mean(ms), 2) == 70.33, round((mean(ms) - MU) / SE, 1) == 6.5,
          abs(mean(abs(xbar - MU) <= 2 * SE) - 0.95) < 0.01)

pop_hist <- function() {
  ggplot(data.frame(v = fh), aes(v)) +
    geom_histogram(binwidth = 1, fill = PALE, colour = "grey35", linewidth = 0.3) +
    geom_vline(xintercept = MU, colour = NAVY, linewidth = 1.1)
}
p_two <- pop_hist() +
  geom_vline(xintercept = mean(s1), colour = RED, linewidth = 1.1, linetype = "dashed") +
  geom_vline(xintercept = mean(s2), colour = GOLD, linewidth = 1.1, linetype = "dashed") +
  annotate("text", x = 72, y = 44, label = "population:~~mu == 65.4", parse = TRUE, hjust = 0,
           size = tsz(18), colour = NAVY) +
  annotate("text", x = 72, y = 33, label = "sample~1:~~bar(x) == 66.33", parse = TRUE, hjust = 0,
           size = tsz(16), colour = RED) +
  annotate("text", x = 72, y = 22, label = "sample~2:~~bar(x) == 64.85", parse = TRUE, hjust = 0,
           size = tsz(16), colour = GOLD) +
  coord_cartesian(xlim = c(55, 93), ylim = c(0, 50), expand = FALSE) +
  labs(x = "height (inches), all 262 women") +
  bare(6, 12, 2, 12)
save_fig(p_two, "l12_two_samples.png", 840, 215)

samp_panel <- function(v, bw, title) {
  ggplot(data.frame(v = v), aes(v)) +
    geom_histogram(binwidth = bw, fill = PALE, colour = "grey35", linewidth = 0.2) +
    geom_vline(xintercept = MU, colour = NAVY, linewidth = 1.1) +
    coord_cartesian(xlim = c(55, 93), expand = FALSE) +
    labs(x = NULL, title = title) +
    bare(4, 10, 2, 10)
}
p_samp <- ggarrange(samp_panel(fh, 1, "262 individual heights"),
                    samp_panel(xbar, 0.25, "10,000 sample means (n = 20)"), ncol = 2)
save_fig(p_samp, "l12_sampling_dist.png", 840, 205)

lo <- MU - 2 * SE; hi <- MU + 2 * SE
p_moe <- ggplot(data.frame(v = xbar), aes(v)) +
  geom_histogram(aes(fill = abs(v - MU) <= 2 * SE), binwidth = 0.1, colour = "grey35",
                 linewidth = 0.2, show.legend = FALSE) +
  scale_fill_manual(values = c(`TRUE` = PALE, `FALSE` = "#F4B6B6")) +
  geom_vline(xintercept = MU, colour = NAVY, linewidth = 1.1) +
  geom_vline(xintercept = c(lo, hi), colour = RED, linewidth = 1, linetype = "dashed") +
  annotate("segment", x = MU, xend = hi, y = 560, yend = 560, colour = RED, linewidth = 0.9,
           arrow = arrow(length = unit(6, "pt"), ends = "both", type = "closed")) +
  annotate("text", x = hi + 0.12, y = 560, label = "2 SE", hjust = 0, size = tsz(18),
           colour = RED, fontface = "bold") +
  annotate("text", x = MU - 2.9, y = 380, label = "about 95% of\nsample means", hjust = 0.5,
           size = tsz(16), colour = NAVY, lineheight = 0.9) +
  scale_x_continuous(breaks = c(62, 63, 64, 65.4, 67, 68, 69),
                     labels = c("62", "63", "64", "65.4", "67", "68", "69")) +
  coord_cartesian(xlim = c(61.8, 69.2), ylim = c(0, 620), expand = FALSE) +
  labs(x = "sample mean height (inches), n = 20") +
  bare(6, 12, 2, 12)
save_fig(p_moe, "l12_moe.png", 840, 215)

#----------------------------------------------------------------------------

write.csv(sizes, file.path(assets_dir, "l12_figure_sizes.csv"), row.names = FALSE)
cat("wrote", nrow(sizes), "figures to", assets_dir, "\n")
print(sizes)
