#Builds the figures for the three opening review problems of the Week 6 Lecture 13
#deck (9/30/2026). Every other Lecture 13 figure is carried over from
#make_lecture12_figures.R.
#
#  l13_review_z.png     X with mean 50, s 8   - how many SDs from the mean is x = 62?
#  l13_review_x.png     X with mean 120, s 15 - what x is 3 SDs above the mean?
#  l13_review_p16.png   X with mean 70, s 10  - what x is about the 16th percentile?
#
#  l13_figure_sizes.csv the size, in points, each figure is drawn at
#
#TYPE SIZE. Same rule as the other make_lectureN_figures.R scripts: nothing on a slide
#is under 16pt, so each figure is drawn at exactly its on-slide size (points / 72
#inches) and every text size goes through tsz() or el(), which refuse anything smaller.
#
#Each histogram is a simulated sample of 500 values RESCALED so its mean and standard
#deviation are exactly the values the slide prints; the answers are asserted below.

suppressPackageStartupMessages(library(ggplot2))

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

PALE <- "#D9E5F7"
MIN_PT <- 16
el <- function(pt, ...) { stopifnot(pt >= MIN_PT); element_text(size = pt, ...) }

sizes <- data.frame(file = character(), w_pt = numeric(), h_pt = numeric(),
                    stringsAsFactors = FALSE)
save_fig <- function(p, file, w_pt, h_pt) {
  ggsave(file.path(assets_dir, file), p, width = w_pt / 72, height = h_pt / 72,
         dpi = 300, bg = "white")
  sizes[nrow(sizes) + 1, ] <<- list(file, w_pt, h_pt)
}

#a sample of n values with EXACTLY mean mu and standard deviation s
exact_sample <- function(n, mu, s, seed) {
  set.seed(seed)
  z <- rnorm(n)
  mu + s * (z - mean(z)) / sd(z)
}

review_hist <- function(mu, s, seed, file, brk) {
  v <- exact_sample(500, mu, s, seed)
  stopifnot(abs(mean(v) - mu) < 1e-9, abs(sd(v) - s) < 1e-9)
  p <- ggplot(data.frame(v = v), aes(v)) +
    geom_histogram(binwidth = s / 2, boundary = mu, fill = PALE, colour = "grey35",
                   linewidth = 0.3) +
    scale_x_continuous(breaks = brk) +
    coord_cartesian(xlim = c(mu - 4 * s, mu + 4 * s), expand = FALSE) +
    labs(x = "x", y = "Frequency") +
    theme_classic() +
    theme(axis.title = el(16), axis.text = el(16), plot.margin = margin(6, 14, 2, 6))
  save_fig(p, file, 840, 185)
}

#1. x to z
stopifnot((62 - 50) / 8 == 1.5)
review_hist(50, 8, 1301, "l13_review_z.png", seq(20, 80, by = 10))
#2. z to x
stopifnot(120 + 3 * 15 == 165)
review_hist(120, 15, 1302, "l13_review_x.png", seq(60, 180, by = 20))
#3. 16th percentile: 50% below the mean, 34% between mean - s and the mean
stopifnot(50 - 34 == 16, 70 - 10 == 60, round(pnorm(-1), 2) == 0.16)
review_hist(70, 10, 1303, "l13_review_p16.png", seq(30, 110, by = 10))

#4. the empirical-rule example (Lecture 13 slide 7). Same three regions as
#l12_exam_rule.png, N(74, 8), but for a generic variable x instead of exam scores,
#so the panel titles carry no units. Regions shaded, percentages NOT labelled -
#they are the answers.
MU4 <- 74; SD4 <- 8
stopifnot(MU4 - SD4 == 66, MU4 + SD4 == 82, MU4 - 2 * SD4 == 58, MU4 + 2 * SD4 == 90)
NAVY <- "#1F3864"; RED <- "#C00000"
region_panel <- function(a, b, title) {
  xs <- seq(46, 102, length.out = 600)
  d  <- data.frame(x = xs, y = dnorm(xs, MU4, SD4))
  dd <- rbind(data.frame(x = a, y = 0), d[d$x >= a & d$x <= b, ], data.frame(x = b, y = 0))
  ggplot() +
    geom_polygon(data = dd, aes(x, y), fill = PALE) +
    geom_line(data = d, aes(x, y), linewidth = 1.1, colour = NAVY) +
    annotate("segment", x = MU4, xend = MU4, y = 0, yend = dnorm(MU4, MU4, SD4),
             colour = RED, linewidth = 0.9) +
    scale_x_continuous(breaks = seq(50, 100, by = 10)) +
    coord_cartesian(xlim = c(46, 102), ylim = c(0, dnorm(MU4, MU4, SD4) * 1.15), expand = FALSE) +
    labs(x = "x", title = title) +
    theme_classic() +
    theme(axis.title.x = el(16), axis.text.x = el(16), plot.title = el(16, hjust = 0.5),
          axis.title.y = element_blank(), axis.text.y = element_blank(),
          axis.ticks.y = element_blank(), axis.line.y = element_blank(),
          plot.margin = margin(6, 8, 2, 8))
}
p_regions <- ggpubr::ggarrange(region_panel(66, 82,  "(a)  66 to 82"),
                               region_panel(82, 102, "(b)  82 and above"),
                               region_panel(58, 90,  "(c)  58 to 90"), ncol = 3)
save_fig(p_regions, "l13_empirical_regions.png", 840, 205)

#----------------------------------------------------------------------------
#5-9. ALL 379 STUDENTS (Sept 30 2026). Lecture 13 switched its running heights data
#from the 262 women to every student in Data/heights.csv, so the outlier comparison,
#the z-table lookup and the margin-of-error block are redrawn here. The l12_ versions
#are left alone: Lecture 12 and the Week 6 notes still use the women.
BLUE <- "#4472C4"; GOLD <- "#BF8F00"; GREY <- "#595959"
tsz <- function(pt) { stopifnot(pt >= MIN_PT); pt / ggplot2::.pt }
data_dir <- file.path(dirname(assets_dir), "Data")
hh <- read.csv(file.path(data_dir, "heights.csv"))
x  <- hh$HEIGHT; mh <- hh$HEIGHT[hh$GENDER == "Male"]
AM <- round(mean(x), 1); AS <- round(sd(x), 2)
stopifnot(length(x) == 379, AM == 67.1, AS == 4.12)

bare_x <- function() theme_classic() +
  theme(axis.title.x = el(16), axis.text.x = el(16), axis.title.y = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.line.y = element_blank(), plot.title = el(16, hjust = 0.5),
        plot.margin = margin(6, 12, 2, 12))

#5. the two outlier rules on all 379 students: now the 2s rule flags MORE
q  <- quantile(x, c(0.25, 0.75), type = 7); iq <- as.numeric(q[2] - q[1])
f_lo <- as.numeric(q[1] - 1.5 * iq); f_hi <- as.numeric(q[2] + 1.5 * iq)
s_lo <- mean(x) - 2 * sd(x);         s_hi <- mean(x) + 2 * sd(x)
n_iqr <- sum(x < f_lo | x > f_hi);   n_2s <- sum(x < s_lo | x > s_hi)
stopifnot(iq == 6, f_lo == 55, f_hi == 79, round(s_lo, 1) == 58.9, round(s_hi, 1) == 75.3,
          n_iqr == 1, n_2s == 12, round(2 * pnorm(-2), 2) == 0.05,
          round(2 * pnorm(-(0.6745 + 1.5 * 2 * 0.6745)), 3) == 0.007)
hd <- as.data.frame(table(x), stringsAsFactors = FALSE); names(hd) <- c("x", "n")
hd$x <- as.numeric(hd$x)
hd$flag <- ifelse(hd$x < f_lo | hd$x > f_hi, "both", ifelse(hd$x < s_lo | hd$x > s_hi, "2s", "none"))
p_rules <- ggplot(hd, aes(x = x, y = n)) +
  geom_col(aes(fill = flag), width = 0.75, colour = "grey35", linewidth = 0.3, show.legend = FALSE) +
  scale_fill_manual(values = c(both = "#F4B6B6", `2s` = GOLD, none = PALE)) +
  annotate("segment", x = c(s_lo, s_hi), xend = c(s_lo, s_hi), y = 0, yend = 36,
           colour = RED, linetype = "dashed", linewidth = 0.9) +
  annotate("segment", x = c(f_lo, f_hi), xend = c(f_lo, f_hi), y = 0, yend = 36,
           colour = BLUE, linetype = "dashed", linewidth = 0.9) +
  annotate("text", x = 80, y = 34, label = "1.5 x IQR fences: 55 and 79", hjust = 0,
           size = tsz(16), colour = BLUE) +
  annotate("text", x = 80, y = 26, label = "mean ± 2s: 58.9 and 75.3", hjust = 0,
           size = tsz(16), colour = RED) +
  annotate("text", x = 80, y = 18, label = "gold: flagged only by ± 2s", hjust = 0,
           size = tsz(16), colour = GOLD) +
  scale_x_continuous(breaks = seq(56, 92, by = 4)) +
  coord_cartesian(xlim = c(53, 94), ylim = c(0, 40), expand = FALSE) +
  labs(x = "height in inches, all 379 students") +
  bare_x()
save_fig(p_rules, "l13_iqr_vs_2s.png", 840, 235)

#6. the z-table, same crop as Lecture 12, highlighting the 72-inch student
Z72 <- round((72 - AM) / AS, 2)
stopifnot(Z72 == 1.19, sprintf("%.4f", pnorm(1.19)) == "0.8830",
          round(2.54 * AM, 1) == 170.4, round(2.54 * AS, 2) == 10.46,
          round((2.54 * 72 - 2.54 * AM) / (2.54 * AS), 2) == 1.19,
          round(AM + 1.28 * AS, 1) == 72.4)
zr <- seq(1.0, 1.6, by = 0.1); zc <- seq(0, 0.09, by = 0.01)
tb <- expand.grid(ci = seq_along(zc), ri = seq_along(zr))
tb$p <- sprintf("%.4f", pnorm(zr[tb$ri] + zc[tb$ci]))
tb$hit  <- tb$ri == which(abs(zr - 1.1) < 1e-9) & tb$ci == which(abs(zc - 0.09) < 1e-9)
tb$band <- tb$ri == which(abs(zr - 1.1) < 1e-9) | tb$ci == which(abs(zc - 0.09) < 1e-9)
stopifnot(tb$p[tb$hit] == "0.8830")
p_tab <- ggplot(tb, aes(x = ci, y = -ri)) +
  geom_tile(aes(fill = ifelse(hit, "hit", ifelse(band, "band", "plain"))),
            colour = "grey70", linewidth = 0.4, show.legend = FALSE) +
  geom_text(aes(label = p, fontface = ifelse(hit, "bold", "plain")), size = tsz(16), colour = NAVY) +
  scale_fill_manual(values = c(hit = "#FFE08A", band = "#F2F6FC", plain = "white")) +
  annotate("text", x = seq_along(zc), y = -0.3, label = sprintf(".%02d", as.integer(round(zc * 100))),
           size = tsz(16), colour = GREY, fontface = "bold") +
  annotate("text", x = 0.25, y = -seq_along(zr), label = sprintf("%.1f", zr),
           size = tsz(16), colour = GREY, fontface = "bold", hjust = 1) +
  annotate("text", x = 0.25, y = -0.3, label = "z", size = tsz(16), colour = GREY,
           fontface = "bold", hjust = 1) +
  coord_cartesian(xlim = c(-0.5, length(zc) + 0.6), ylim = c(-length(zr) - 0.7, 0.3), expand = FALSE) +
  theme_void() + theme(plot.margin = margin(2, 2, 2, 2))
save_fig(p_tab, "l13_ztable.png", 760, 230)

#7-9. the margin-of-error block with all 379 students as the population
MU <- mean(x); SIG <- sd(x); SE <- SIG / sqrt(20)
set.seed(9)
s1 <- sample(x, 20, replace = TRUE); s2 <- sample(x, 20, replace = TRUE)
xbar <- replicate(10000, mean(sample(x, 20, replace = TRUE)))
set.seed(117)
ms <- sample(mh, 20)
stopifnot(round(mean(s1), 2) == 66.62, round(mean(s2), 2) == 67.83,
          round(SE, 2) == 0.92, round(2 * SE, 1) == 1.8,
          round(mean(ms), 2) == 70.33, round((70.33 - 67.1) / 0.92, 1) == 3.5,
          abs(mean(abs(xbar - MU) <= 2 * SE) - 0.95) < 0.01)

pop <- function() ggplot(data.frame(v = x), aes(v)) +
  geom_histogram(binwidth = 1, fill = PALE, colour = "grey35", linewidth = 0.3) +
  geom_vline(xintercept = MU, colour = NAVY, linewidth = 1.1)
p_two <- pop() +
  geom_vline(xintercept = mean(s1), colour = RED, linewidth = 1.1, linetype = "dashed") +
  geom_vline(xintercept = mean(s2), colour = GOLD, linewidth = 1.1, linetype = "dashed") +
  annotate("text", x = 76, y = 40, label = "population:~~mu == 67.1", parse = TRUE,
           hjust = 0, size = tsz(18), colour = NAVY) +
  annotate("text", x = 76, y = 30, label = "sample~1:~~bar(x) == 66.62", parse = TRUE,
           hjust = 0, size = tsz(16), colour = RED) +
  annotate("text", x = 76, y = 21, label = "sample~2:~~bar(x) == 67.83", parse = TRUE,
           hjust = 0, size = tsz(16), colour = GOLD) +
  coord_cartesian(xlim = c(55, 93), ylim = c(0, 46), expand = FALSE) +
  labs(x = "height (inches), all 379 students") + bare_x()
save_fig(p_two, "l13_two_samples.png", 840, 215)

samp_panel <- function(v, bw, title) ggplot(data.frame(v = v), aes(v)) +
  geom_histogram(binwidth = bw, fill = PALE, colour = "grey35", linewidth = 0.2) +
  geom_vline(xintercept = MU, colour = NAVY, linewidth = 1.1) +
  coord_cartesian(xlim = c(55, 93), expand = FALSE) +
  labs(x = NULL, title = title) + bare_x()
p_samp <- ggpubr::ggarrange(samp_panel(x, 1, "379 individual heights"),
                            samp_panel(xbar, 0.25, "10,000 sample means (n = 20)"), ncol = 2)
save_fig(p_samp, "l13_sampling_dist.png", 840, 205)

lo <- MU - 2 * SE; hi <- MU + 2 * SE
p_moe <- ggplot(data.frame(v = xbar), aes(v)) +
  geom_histogram(aes(fill = abs(v - MU) <= 2 * SE), binwidth = 0.12, colour = "grey35",
                 linewidth = 0.2, show.legend = FALSE) +
  scale_fill_manual(values = c(`TRUE` = PALE, `FALSE` = "#F4B6B6")) +
  geom_vline(xintercept = MU, colour = NAVY, linewidth = 1.1) +
  geom_vline(xintercept = c(lo, hi), colour = RED, linewidth = 1, linetype = "dashed") +
  annotate("segment", x = MU, xend = hi, y = 560, yend = 560, colour = RED, linewidth = 0.9,
           arrow = arrow(length = unit(6, "pt"), ends = "both", type = "closed")) +
  annotate("text", x = hi + 0.15, y = 560, label = "2 SE", hjust = 0, size = tsz(18),
           colour = RED, fontface = "bold") +
  annotate("text", x = MU - 3.4, y = 380, label = "about 95% of\nsample means", hjust = 0.5,
           size = tsz(16), colour = NAVY, lineheight = 0.9) +
  scale_x_continuous(breaks = c(63, 64, 65, 67.1, 69, 70, 71),
                     labels = c("63", "64", "65", "67.1", "69", "70", "71")) +
  coord_cartesian(xlim = c(62.6, 71.6), ylim = c(0, 620), expand = FALSE) +
  labs(x = "sample mean height (inches), n = 20") + bare_x()
save_fig(p_moe, "l13_moe.png", 840, 215)

#----------------------------------------------------------------------------
#10. the law of large numbers, for the probability preview at the end of the deck:
#the running proportion of heads over 5,000 simulated fair-coin tosses (same seed
#as the Week 6 notes' figure), on a log axis so the wild early tosses are visible.
set.seed(432)
flips <- rbinom(5000, 1, 0.5)
lln <- data.frame(n = 1:5000, prop = cumsum(flips) / (1:5000))
stopifnot(abs(lln$prop[5000] - 0.5) < 0.02, round(12012 / 24000, 4) == 0.5005)
p_lln <- ggplot(lln, aes(n, prop)) +
  geom_hline(yintercept = 0.5, colour = RED, linewidth = 1) +
  geom_line(colour = NAVY, linewidth = 0.8) +
  annotate("text", x = 1.2, y = 0.9, label = "a few tosses: anything can happen", hjust = 0,
           size = tsz(16), colour = GREY) +
  annotate("text", x = 5000, y = 0.62, label = "thousands of tosses: close to 0.5", hjust = 1,
           size = tsz(16), colour = RED) +
  scale_x_log10(breaks = c(1, 10, 100, 1000, 5000), labels = c("1", "10", "100", "1,000", "5,000")) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  coord_cartesian(ylim = c(0, 1), expand = FALSE) +
  labs(x = "number of tosses (log scale)", y = "proportion of heads") +
  theme_classic() +
  theme(axis.title = el(16), axis.text = el(16), plot.margin = margin(8, 30, 2, 6))
save_fig(p_lln, "l13_lln.png", 840, 200)

write.csv(sizes, file.path(assets_dir, "l13_figure_sizes.csv"), row.names = FALSE)
cat("wrote", nrow(sizes), "figures to", assets_dir, "\n")
