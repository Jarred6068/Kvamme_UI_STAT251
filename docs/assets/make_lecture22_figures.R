#Checks the numbers on the Week 10 Lecture 22 deck (10/26/2026): an introduction to significance
#tests and P-values. It draws:
#
#  l22_tails.png         one poll (z_obs = -1.90), its P-value under each of the three alternatives
#  l22_figure_sizes.csv  the size, in points, each figure is drawn at
#
#P-values from the normal are the hand-method values from the course z-table (four decimals).
#
#TYPE SIZE. Same rule as the other make_lectureN_figures.R scripts: nothing on a slide is
#under 16pt, so each figure is drawn at exactly its on-slide size (points / 72 inches) and
#every text size goes through tsz() or el(), which refuse anything smaller.
#
#SOURCES
#  Taste test (hypothetical): a roommate names 8 of 8 cups of Coke or Pepsi right, a friend 4 of 6.
#  Coin flips: Bartos et al. (2025), "Fair coins tend to land on the same side they started:
#    evidence from 350,757 flips", J Am Stat Assoc 120(552):2118-2127 (arXiv 2310.04153v4,
#    Methods and Analysis): 48 people; 75,036 + 203,440 + 72,281 = 350,757 flips; 178,079
#    landed on the side they started; the Diaconis-Holmes-Montgomery model predicted about 51%.

near <- function(a, b, tol = 5e-4) abs(a - b) < tol
zt <- function(z) round(pnorm(z), 4)            # a z-table lookup

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

#minimum sample size (carried from Lecture 21): always round up
nprop <- function(z, ps, m) z^2 * ps * (1 - ps) / m^2
stopifnot(near(nprop(1.96, 0.5, 0.03), 1067.1, 0.05), ceiling(nprop(1.96, 0.5, 0.03)) == 1068,
          near((1.96 * 7.9 / 2)^2, 59.9, 0.05), ceiling((1.96 * 7.9 / 2)^2) == 60,
          #practice: skipping breakfast, m = 0.04 at 90%, with last year's 30% and with 0.5
          near(nprop(1.645, 0.3, 0.04), 355.2, 0.05), ceiling(nprop(1.645, 0.3, 0.04)) == 356,
          near(nprop(1.645, 0.5, 0.04), 422.8, 0.05), ceiling(nprop(1.645, 0.5, 0.04)) == 423)

#taste test: under H0 each cup is named right with probability 0.5
stopifnot(near(0.5^8, 0.0039, 1e-4), 1 / 0.5^8 == 256,
          sum(choose(6, 4:6)) == 22, near(1 - pbinom(3, 6, 0.5), 22 / 64, 1e-12),
          near(22 / 64, 0.34, 0.005))

#the poll (hypothetical): other polls say 52% for Candidate X; ours finds 490 of 1,000 likely voters.
#H0: p = 0.52; the SE uses p0. Then the same poll against p0 = 0.5.
se_p <- round(sqrt(0.52 * 0.48 / 1000), 4); z_p <- round((0.490 - 0.52) / se_p, 2)
se_h <- round(sqrt(0.25 / 1000), 4); z_h <- round((0.490 - 0.5) / se_h, 2)
stopifnot(1000 * 0.52 >= 15, 1000 * 0.48 >= 15, se_p == 0.0158, z_p == -1.90,
          zt(-1.90) == 0.0287, round(1 - zt(-1.90), 4) == 0.9713, round(2 * zt(-1.90), 4) == 0.0574,
          round(zt(-1.90), 3) == 0.029, round(1 - zt(-1.90), 3) == 0.971, round(2 * zt(-1.90), 3) == 0.057,
          se_h == 0.0158, z_h == -0.63, zt(-0.63) == 0.2643)

#coin flips: H0 p = 0.5, HA p > 0.5; the SE uses H0's p
n_f <- 75036 + 203440 + 72281; x_f <- 178079
ph_f <- round(x_f / n_f, 4); se_f <- round(sqrt(0.5 * 0.5 / n_f), 6)
z_f <- round((ph_f - 0.5) / se_f, 1)
stopifnot(n_f == 350757, ph_f == 0.5077, se_f == 0.000844, z_f == 9.1,
          1 - pnorm(z_f) < 1e-4,                      # off the end of the z-table
          round(1 / (ph_f - 0.5)) == 130)             # about 1 extra same-side landing per 130

#----------------------------------------------------------------------------
suppressPackageStartupMessages(library(ggplot2))

NAVY <- "#1F3864"; SHADE <- "#8FAADC"; RED <- "#C00000"
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
bare_x <- function() theme_classic() +
  theme(axis.title.x = element_blank(), axis.text.x = el(16), axis.title.y = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        strip.text = el(18, face = "bold", colour = NAVY), strip.background = element_blank(),
        panel.spacing = unit(28, "pt"), plot.margin = margin(4, 12, 2, 12))
MINUS <- "−"
neg <- function(s) sub("-", MINUS, s, fixed = TRUE)

#l22_tails.png: the observed z marked, the P-value shaded for each alternative
grid <- seq(-4, 4, length.out = 401)
panels <- c("H[A]:~p > 0.52", "H[A]:~p < 0.52", "H[A]:~p != 0.52")
pan <- data.frame(panel = factor(panels, levels = panels), z = c(-1.90, -1.90, -1.90))
line <- do.call(rbind, lapply(panels, function(p) data.frame(panel = p, x = grid, y = dnorm(grid))))
line$panel <- factor(line$panel, levels = panels)
tail_poly <- function(panel, lo, hi) {
  g <- c(lo, grid[grid >= lo & grid <= hi], hi)
  data.frame(panel = panel, part = paste(panel, lo), x = g, y = c(0, dnorm(g[-c(1, length(g))]), 0))
}
fill <- rbind(tail_poly(panels[1], -1.90, 4), tail_poly(panels[2], -4, -1.90),
              tail_poly(panels[3], 1.90, 4), tail_poly(panels[3], -4, -1.90))
fill$panel <- factor(fill$panel, levels = panels)
marks <- rbind(pan, data.frame(panel = factor(panels[3], levels = panels), z = 1.90))
p_tails <- ggplot() +
  geom_polygon(data = fill, aes(x, y, group = part), fill = SHADE, alpha = 0.9) +
  geom_line(data = line, aes(x, y), colour = NAVY, linewidth = 0.9) +
  geom_segment(data = marks, aes(x = z, xend = z, y = 0, yend = dnorm(z) + 0.06),
               colour = RED, linewidth = 1) +
  geom_text(data = marks, aes(x = z + sign(z) * 0.12, y = dnorm(z) + 0.05,
                              hjust = ifelse(z > 0, 0, 1),
                              label = neg(formatC(z, format = "f", digits = 2))),
            size = tsz(16), colour = RED, fontface = "bold") +
  facet_wrap(~panel, nrow = 1, labeller = label_parsed) +
  scale_x_continuous(breaks = c(-3, 0, 3), labels = neg(c(-3, 0, 3))) +
  coord_cartesian(xlim = c(-4, 4), ylim = c(0, 0.45), expand = FALSE) +
  bare_x()
save_fig(p_tails, "l22_tails.png", 840, 200)

write.csv(sizes, file.path(assets_dir, "l22_figure_sizes.csv"), row.names = FALSE)
cat("Lecture 22 numbers check out; wrote", nrow(sizes), "figures to", assets_dir, "\n")
