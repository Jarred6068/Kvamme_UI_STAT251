#Builds the four figures for the "features of a distribution" slides in the Week 2
#Lecture 3 deck. Each feature gets one slide and one picture, because "shape",
#"center" and "variability" are words students can repeat back without being able
#to point at them on a plot.
#
#  feature_modal_category.png  bar chart, modal bar picked out  (categorical)
#  feature_shape.png           four histograms described in plain English. No
#                              panel is labelled "skewed" or "bimodal": those
#                              terms are covered formally later in the course, and
#                              the point of this slide is to get students LOOKING
#                              at a distribution before they have vocabulary for
#                              what they see.
#  feature_center.png          two histograms with the mean marked - under the
#                              peak when symmetric, dragged off it when skewed.
#                              ONLY the mean: the median is not defined until the
#                              Week 3 notes, so it cannot be drawn on a Week 2
#                              slide and expected to mean anything.
#  feature_variability.png     two histograms, same center, different spread,
#                              drawn on identical axes so the comparison is fair
#
#The bimodal panel is the real Old Faithful waiting times rather than something
#simulated, so the shape the class meets here is the one they will bin two slides
#later. The teen survey supplies the categorical example for the same reason -
#it is the dataset already running through the lecture.
#
#Colours are the deck's: accent blue for the bars, C00000 for anything the eye is
#supposed to land on. Output aspect ratios are chosen to match the picture frames
#in the slides; change a size here and the slide frame has to change with it.

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

BLUE <- "#4472C4"
MARK <- "#C00000"
GREY <- "#BFC9DB"

base_theme <- theme_minimal(base_size = 15) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_text(size = 13),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 13, colour = MARK))

histo <- function(x, title, subtitle = NULL, bins = 18, xlim = NULL) {
  ggplot(data.frame(x = x), aes(x = x)) +
    geom_histogram(bins = bins, fill = BLUE, colour = "white", linewidth = 0.4) +
    labs(title = title, subtitle = subtitle, x = NULL, y = "Frequency") +
    coord_cartesian(xlim = xlim) +
    base_theme
}

#----------------------------------------------------------------------------
#1. modal category - the teen survey, modal bar picked out

teens <- data.frame(
  Response = factor(c("Never", "Rarely", "Sometimes", "Often"),
                    levels = c("Never", "Rarely", "Sometimes", "Often")),
  Frequency = c(289L, 216L, 178L, 60L))
teens$is_mode <- teens$Frequency == max(teens$Frequency)

p_mode <- ggplot(teens, aes(x = Response, y = Frequency, fill = is_mode)) +
  geom_col(colour = "white", linewidth = 0.5, width = 0.75) +
  geom_text(aes(label = Frequency), vjust = -0.4, size = 5) +
  scale_fill_manual(values = c("FALSE" = GREY, "TRUE" = MARK), guide = "none") +
  labs(title = "Are you losing focus in class by checking your phone?",
       subtitle = "The modal category is the tallest bar - the response that came back most often",
       x = NULL, y = "Frequency") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  base_theme
ggsave(file.path(assets_dir, "feature_modal_category.png"), p_mode,
       width = 10, height = 4.2, dpi = 200)

#----------------------------------------------------------------------------
#2. shape - four panels

n <- 1200L   #big enough that the simulated shapes read as their label rather than as noise
shape_panels <- ggarrange(
  histo(rnorm(n, 50, 8), "Values pile up in the middle",
        "and thin out evenly on both sides"),
  histo(30 + rgamma(n, shape = 2, scale = 6), "Values pile up on the left",
        "trailing off a long way to the right"),
  histo(80 - rgamma(n, shape = 2, scale = 6), "Values pile up on the right",
        "trailing off a long way to the left"),
  histo(faithful$waiting, "Values pile up in two separate places",
        "with a gap between them (Old Faithful)"),
  nrow = 2, ncol = 2)
ggsave(file.path(assets_dir, "feature_shape.png"), shape_panels,
       width = 11.5, height = 4.9, dpi = 200)

#----------------------------------------------------------------------------
#3. center - mean and median, together and apart

#The peak is marked by eye rather than computed: it is the midpoint of the tallest
#histogram bar, which is what a student reading the picture would point at.
with_center <- function(x, title, subtitle, bins = 18) {
  brk  <- pretty(range(x), n = bins)
  cts  <- hist(x, breaks = brk, plot = FALSE)
  peak <- cts$mids[which.max(cts$counts)]
  histo(x, title, subtitle, xlim = c(20, 90)) +
    annotate("segment", x = peak, xend = peak, y = 0, yend = Inf,
             colour = GREY, linewidth = 1.1, linetype = "22") +
    geom_vline(xintercept = mean(x), colour = MARK, linewidth = 1.2) +
    annotate("text", x = mean(x), y = Inf, label = "mean", colour = MARK,
             hjust = -0.12, vjust = 1.6, size = 4.6, fontface = "bold") +
    annotate("text", x = peak, y = Inf, label = "peak", colour = "grey35",
             hjust = 1.1, vjust = 1.6, size = 4.4)
}

#Panel titles avoid "symmetric" and "skewed" for the same reason the shape figure
#does: neither word is defined for the class yet.
center_panels <- ggarrange(
  with_center(rnorm(n, 50, 8), "Values pile up evenly",
              "the mean sits under the peak"),
  with_center(30 + rgamma(n, shape = 2, scale = 6), "Values trail off to the right",
              "the long tail drags the mean off the peak"),
  nrow = 1, ncol = 2)
ggsave(file.path(assets_dir, "feature_center.png"), center_panels,
       width = 11.5, height = 3.9, dpi = 200)

#----------------------------------------------------------------------------
#4. variability - same center, different spread, identical axes

LIM <- c(10, 90)
var_panels <- ggarrange(
  histo(rnorm(n, 50, 4), "Small variability",
        "values pile up close to the center", xlim = LIM),
  histo(rnorm(n, 50, 14), "Large variability",
        "the same center, spread far wider", xlim = LIM),
  nrow = 1, ncol = 2)
ggsave(file.path(assets_dir, "feature_variability.png"), var_panels,
       width = 11.5, height = 3.9, dpi = 200)

message("Wrote four feature figures to ", assets_dir)
for (f in c("feature_modal_category.png", "feature_shape.png",
            "feature_center.png", "feature_variability.png"))
  message("  ", f, " (", format(file.size(file.path(assets_dir, f)) %/% 1024L,
                                big.mark = ","), " KB)")
