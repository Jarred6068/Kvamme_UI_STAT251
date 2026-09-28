#Builds the figures for the normal-distribution slides tacked onto the end of the
#Lecture 11 (9/25/2026) deck, and reused by Lecture 12 (9/28/2026) and the Week 5
#notes section "The normal distribution and z-scores".
#
#  l11_cereal_sodium.png   the twenty cereals' sodium on a dot plot, with the mean
#                          and Raisin Bran's deviation marked in standard deviations
#  l11_density_curve.png   a histogram of a continuous variable with its density curve
#  l11_normal_params.png   what mu and sigma each do to a normal curve
#
#  l11_figure_sizes.csv    the size, in points, each figure is drawn at
#
#TYPE SIZE. Same rule as make_lecture8_figures.R: nothing on a slide may be under
#16pt, text inside a picture included, so every figure is drawn at EXACTLY the size
#it occupies on the slide (width_in = points / 72) and the build script places it at
#that size, read from l11_figure_sizes.csv. Every text size goes through tsz() or el(),
#which refuse anything under 16pt.
#
#The dot plot runs the full 840pt width of the content area; the two bell-curve figures
#are drawn nearer to square and centred, because a normal curve stretched to 840 x 205
#stops reading as bell shaped at all.
#
#The numbers the slides print are asserted below: mean sodium 167 mg, s about 77 mg,
#Raisin Bran 340 mg, so a deviation of 173 mg and a z-score of about 2.25.

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

W <- 840
H <- 205

#----------------------------------------------------------------------------
#1. the cereals, with one deviation measured in standard deviations.
#
#A dot plot rather than the histogram-plus-dots of the notes: at 205pt tall the
#bars leave no room for the arrow, and the arrow is the whole point of the slide.
#Ties are stacked by hand so the stacking is exact rather than binned by eye.

cereal <- read.csv(file.path(data_dir, "cereal.csv"))
xbar <- mean(cereal$Sodium)
s    <- sd(cereal$Sodium)
rb   <- cereal$Sodium[cereal$Cereal == "Raisin Bran"]
stopifnot(nrow(cereal) == 20, xbar == 167, round(s) == 77, rb == 340,
          rb - xbar == 173, round((rb - xbar) / 77, 2) == 2.25)

cereal <- cereal[order(cereal$Sodium), ]
cereal$stack <- ave(cereal$Sodium, cereal$Sodium, FUN = seq_along)
cereal$hi    <- cereal$Cereal == "Raisin Bran"

p_cereal <- ggplot(cereal, aes(x = Sodium, y = stack)) +
  annotate("segment", x = xbar, xend = xbar, y = 0.6, yend = 4.75,
           colour = NAVY, linetype = "dashed", linewidth = 0.8) +
  annotate("text", x = xbar - 10, y = 4.75, label = "bar(x) == 167", parse = TRUE,
           hjust = 1, vjust = 1, size = tsz(16), colour = NAVY) +
  annotate("segment", x = xbar, xend = rb, y = 4.0, yend = 4.0, colour = RED,
           linewidth = 0.8, arrow = arrow(length = unit(7, "pt"), ends = "both",
                                          type = "closed")) +
  annotate("text", x = (xbar + rb) / 2, y = 4.2,
           label = "173~mg == 2.25~standard~deviations", parse = TRUE,
           vjust = 0, size = tsz(16), colour = RED) +
  geom_point(aes(fill = hi), shape = 21, size = 5.6, colour = "grey30",
             stroke = 0.6, show.legend = FALSE) +
  annotate("text", x = rb, y = 0.5, label = "Raisin Bran", vjust = 1,
           size = tsz(16), colour = RED) +
  scale_fill_manual(values = c("TRUE" = RED, "FALSE" = PALE)) +
  scale_x_continuous(breaks = seq(0, 350, by = 50), limits = c(-35, 380)) +
  scale_y_continuous(limits = c(-0.6, 5.3)) +
  labs(x = "Sodium (mg) in twenty US breakfast cereals", y = NULL) +
  theme_classic() +
  theme(axis.title.x = el(16), axis.text.x = el(16),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        plot.margin = margin(4, 8, 2, 8))
save_fig(p_cereal, "l11_cereal_sodium.png", W, H)

#----------------------------------------------------------------------------
#2. histogram vs density curve - the same picture the Week 5 notes use, drawn at
#slide size. Seeded so the figure is reproducible.

set.seed(251)
dfx <- data.frame(x = rnorm(200))
k   <- ceiling(sqrt(200))

p_density <- ggplot(dfx, aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)), bins = k, fill = PALE,
                 colour = "grey30", linewidth = 0.4) +
  stat_function(fun = dnorm, linewidth = 1.2, colour = NAVY, n = 400) +
  annotate("text", x = 2.1, y = 0.37, label = "the density curve", hjust = 0,
           size = tsz(16), colour = NAVY) +
  annotate("curve", x = 2.05, xend = 1.05, y = 0.355, yend = 0.24, curvature = 0.25,
           colour = NAVY, linewidth = 0.6, arrow = arrow(length = unit(6, "pt"),
                                                         type = "closed")) +
  annotate("text", x = -3.9, y = 0.37, label = "what we observed", hjust = 0,
           size = tsz(16), colour = GREY) +
  annotate("curve", x = -2.6, xend = -1.75, y = 0.345, yend = 0.10, curvature = -0.25,
           colour = GREY, linewidth = 0.6, arrow = arrow(length = unit(6, "pt"),
                                                         type = "closed")) +
  coord_cartesian(xlim = c(-4, 4), ylim = c(0, 0.44), expand = FALSE) +
  labs(x = "X", y = "Density") +
  theme_classic() +
  theme(axis.title = el(16), axis.text = el(16),
        plot.margin = margin(6, 10, 2, 6))
save_fig(p_density, "l11_density_curve.png", 620, 225)

#----------------------------------------------------------------------------
#3. the two parameters. Left: one curve, with mu marking the center and a sigma
#step out to the inflection point. Right: three curves that differ only in sigma,
#labelled where they peak so the panel needs no legend.

curve_df <- function(mu, sd, lo, hi) {
  x <- seq(lo, hi, length.out = 400)
  data.frame(x = x, y = dnorm(x, mu, sd), lab = paste0(mu, "_", sd))
}

one <- curve_df(0, 1, -4, 4)
p_mu <- ggplot(one, aes(x, y)) +
  geom_area(fill = PALE) +
  geom_line(linewidth = 1.2, colour = NAVY) +
  annotate("segment", x = 0, xend = 0, y = 0, yend = dnorm(0), colour = NAVY,
           linetype = "dashed", linewidth = 0.8) +
  annotate("text", x = 0, y = dnorm(0) + 0.015, label = "mu", parse = TRUE,
           size = tsz(18), colour = NAVY, vjust = 0) +
  annotate("segment", x = 0, xend = 1, y = dnorm(1), yend = dnorm(1), colour = RED,
           linewidth = 0.9, arrow = arrow(length = unit(6, "pt"), ends = "both",
                                          type = "closed")) +
  annotate("text", x = 1.12, y = dnorm(1), label = "sigma", parse = TRUE, hjust = 0,
           size = tsz(18), colour = RED) +
  coord_cartesian(xlim = c(-4, 4), ylim = c(0, 0.48), expand = FALSE) +
  labs(x = "the mean locates the center",
       y = NULL) +
  theme_classic() +
  theme(axis.title.x = el(16), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.line.y = element_blank(),
        plot.margin = margin(6, 12, 2, 6))

three <- do.call(rbind, Map(curve_df, c(0, 0, 0), c(0.6, 1, 2),
                            MoreArgs = list(lo = -6, hi = 6)))
labs3 <- data.frame(x = c(0.22, 0.75, 1.8),
                    y = dnorm(0, 0, c(0.6, 1, 2)) + 0.02,
                    txt = c("sigma == 0.6", "sigma == 1", "sigma == 2"))
p_sigma <- ggplot(three, aes(x, y, group = lab, colour = lab)) +
  geom_line(linewidth = 1.2) +
  geom_text(data = labs3, aes(x = x, y = y, label = txt), parse = TRUE,
            inherit.aes = FALSE, hjust = 0, size = tsz(16),
            colour = c(NAVY, BLUE, RED)) +
  scale_colour_manual(values = c("0_0.6" = NAVY, "0_1" = BLUE, "0_2" = RED),
                      guide = "none") +
  coord_cartesian(xlim = c(-6, 6), ylim = c(0, 0.74), expand = FALSE) +
  labs(x = "the standard deviation sets the spread", y = NULL) +
  theme_classic() +
  theme(axis.title.x = el(16), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.line.y = element_blank(),
        plot.margin = margin(6, 10, 2, 12))

p_params <- ggarrange(p_mu, p_sigma, ncol = 2, widths = c(1, 1))
save_fig(p_params, "l11_normal_params.png", 840, 225)

#----------------------------------------------------------------------------

write.csv(sizes, file.path(assets_dir, "l11_figure_sizes.csv"), row.names = FALSE)
cat("wrote", nrow(sizes), "figures to", assets_dir, "\n")
print(sizes)
