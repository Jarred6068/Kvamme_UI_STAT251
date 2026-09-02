#Builds the "shape and center" figure for the Week 2 Lecture 4 deck (slide 4,
#"Shape of a distribution").
#
#  shape_center_mean_median_mode.png   four densities - symmetric, skewed right,
#                                      skewed left, bimodal - each with the mode,
#                                      median and mean drawn as vertical lines
#                                      under one shared legend.
#
#The whole point of the picture is that the THREE MEASURES OF CENTER MOVE APART AS
#THE SHAPE CHANGES, so the panels are chosen to make that unmissable:
#
#  symmetric     all three land on top of each other
#  skewed right  mode < median < mean - the tail drags the mean out
#  skewed left   mean < median < mode - mirror image
#  bimodal       the mode sits on the tall peak while the mean and median fall in
#                the empty gap between the humps, where almost no observation
#                actually lies. This is the panel that earns the slide: it shows
#                that "the center" can be a value the data never takes.
#
#Note this figure deliberately draws the median and the mode, unlike
#make_distribution_feature_figures.R which draws the mean only. That script feeds
#Week 2 Lecture 3, where the median has not been defined yet. Here the slide
#defines all three in words first, so all three can be drawn.
#
#Lines are distinguished by colour AND linetype, so the panels still read when
#printed in greyscale or seen by a colour-blind student.
#
#The output is sized 11.5 x 4.6in to fit the picture frame on slide 4; change the
#size here and the frame in the deck build script has to change with it.

set.seed(2026)

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

FILL <- "#DCE6F5"   #pale tint of the deck's accent blue
EDGE <- "#2F4E7D"
MEAN <- "#C00000"   #the deck's "look here" red
MED  <- "#1F7A4D"
MODE <- "#ED7D31"

#Order the legend the way the slide reads them: peak first, then middle, then average.
LEVELS <- c("Mode", "Median", "Mean")
COLS   <- c(Mode = MODE, Median = MED, Mean = MEAN)
LTYS   <- c(Mode = "dotted", Median = "dashed", Mean = "solid")

#The mode of a continuous variable is read off the density curve - the value where
#the curve is highest - which is exactly what a student would point at on the plot.
#
#`n` is how many peaks to return. A bimodal distribution has TWO modes and both
#get marked: calling only the taller one "the mode" would quietly contradict the
#panel's own title. Returns the n tallest local maxima, ordered along the x-axis.
peaks_of <- function(x, n = 1) {
  d <- density(x, n = 2048)
  y <- d$y
  m <- length(y)
  idx <- which(y[2:(m - 1)] > y[1:(m - 2)] & y[2:(m - 1)] >= y[3:m]) + 1
  idx <- idx[order(y[idx], decreasing = TRUE)][seq_len(min(n, length(idx)))]
  sort(d$x[idx])
}

panel <- function(x, title, subtitle, xlim, nmodes = 1) {
  md <- peaks_of(x, nmodes)
  stats <- data.frame(
    stat  = factor(c(rep("Mode", length(md)), "Median", "Mean"), levels = LEVELS),
    value = c(md, median(x), mean(x)))
  one <- function(s) stats[stats$stat == s, ]

  #Drawn widest-first so that when the three coincide - the whole point of the
  #symmetric panel - the thinner lines sit ON TOP of the thicker ones and all
  #three stay visible, instead of the last one drawn hiding the other two.
  ggplot(data.frame(x = x), aes(x = x)) +
    geom_density(fill = FILL, colour = EDGE, linewidth = 0.7, adjust = 1.1) +
    geom_vline(data = one("Mean"), aes(xintercept = value, colour = stat,
               linetype = stat), linewidth = 1.9, key_glyph = "path") +
    geom_vline(data = one("Median"), aes(xintercept = value, colour = stat,
               linetype = stat), linewidth = 1.3, key_glyph = "path") +
    geom_vline(data = one("Mode"), aes(xintercept = value, colour = stat,
               linetype = stat), linewidth = 0.9, key_glyph = "path") +
    scale_colour_manual(values = COLS, breaks = LEVELS, name = NULL,
                        drop = FALSE) +
    scale_linetype_manual(values = LTYS, breaks = LEVELS, name = NULL,
                          drop = FALSE) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
    coord_cartesian(xlim = xlim) +
    labs(title = title, subtitle = subtitle, x = NULL, y = NULL) +
    theme_minimal(base_size = 14) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.text.y = element_blank(),
          plot.title = element_text(size = 15, face = "bold"),
          plot.subtitle = element_text(size = 12.5, colour = "grey25"),
          legend.text = element_text(size = 13))
}

n <- 40000L   #large enough that the mode read off the density is stable, not noise

#Each panel is cropped to the range where the shape actually lives. Left to
#itself the gamma tail runs so far right that the three lines end up stacked on
#top of each other in the leftmost tenth of the axis.

#symmetric - all three coincide
sym <- rnorm(n, 50, 8)

#skewed right - a long tail to the right drags the mean past the median. The hard
#floor at zero is the honest picture: this is the shape of incomes and prices,
#which cannot go negative but can run away upward.
sr <- rgamma(n, shape = 2, scale = 6)

#skewed left - the mirror image, with the hard ceiling at 100 that exam scores have
sl <- 100 - rgamma(n, shape = 2, scale = 6)

#bimodal - equal weights, but the left hump is narrower and therefore TALLER, so
#the mode is unambiguously the left peak while the mean and median sit in the gap
k  <- sample(c(TRUE, FALSE), n, replace = TRUE)
bi <- ifelse(k, rnorm(n, 35, 4), rnorm(n, 70, 7))

fig <- ggarrange(
  panel(sym, "Symmetric",
        "mean = median = mode: all three land in the same place",
        xlim = c(20, 80)),
  panel(sr, "Skewed right",
        "mode < median < mean: the tail pulls the mean right",
        xlim = c(0, 45)),
  panel(sl, "Skewed left",
        "mean < median < mode: the tail pulls the mean left",
        xlim = c(55, 100)),
  panel(bi, "Bimodal",
        "two modes, and the mean and median fall in the gap between them",
        xlim = c(18, 92), nmodes = 2),
  nrow = 2, ncol = 2, common.legend = TRUE, legend = "bottom")

out <- file.path(assets_dir, "shape_center_mean_median_mode.png")
ggsave(out, fig, width = 11.5, height = 4.6, dpi = 200, bg = "white")

message("Wrote ", out, " (",
        format(file.size(out) %/% 1024L, big.mark = ","), " KB)")

#Print the three numbers per panel so the slide's claims can be checked against
#what was actually drawn.
for (nm in c("Symmetric", "Skewed right", "Skewed left", "Bimodal")) {
  x <- switch(nm, "Symmetric" = sym, "Skewed right" = sr, "Skewed left" = sl, bi)
  k <- if (nm == "Bimodal") 2 else 1
  message(sprintf("  %-13s mode(s) = %-13s median = %5.1f   mean = %5.1f",
                  nm, paste(sprintf("%.1f", peaks_of(x, k)), collapse = ", "),
                  median(x), mean(x)))
}
