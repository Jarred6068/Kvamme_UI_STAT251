#Builds the figures for the Week 3 Lecture 6 deck (Wednesday 9/10/2026), the
#lecture that finishes descriptive statistics: IQR, variance, standard
#deviation, the five number summary and the boxplot.
#
#  l6_boxplot_anatomy.png    a labelled boxplot with the 1.5 x IQR fences drawn
#  l6_boxplot_exam.png       worked answer - boxplot of the 15 exam scores
#  l6_boxplot_outlier.png    worked answer - the 12-value set, 10.4 flagged
#  l6_boxplot_shapes.png     four boxplot-over-density panels: symmetric, skew
#                            left, skew right, bimodal
#
#QUARTILES ARE COMPUTED BY THE HAND METHOD the deck teaches - split the ordered
#data at the median, then take the median of each half, dropping the median
#itself when n is odd. R's default quantile(type = 7) disagrees on most of these
#samples, so everything drawn here goes through hand_quartiles() below and every
#figure is checked against the numbers the slides print. Do NOT "fix" this by
#switching to quantile() or to boxplot()'s own hinges, which use yet another
#convention (Tukey hinges) and put Q1 of the exam scores at 65.5 rather than 65.
#
#THE DATASETS all appear on the slides and in the Week 3 notes, so the same
#numbers have to come out in all three places:
#
#  exam scores  61 61 65 65 66 68 69 73 74 75 76 78 79 90 94      (n = 15)
#               Q1 65, Q2 73, Q3 78, IQR 13, fences 45.5 / 97.5, no outliers
#
#  the 12-value set  -5.7 -2.6 -1.5 -1.3 -0.4 0.2 1.5 2.2 2.3 2.6 2.9 10.4
#               Q1 -1.4, Q2 0.85, Q3 2.45, IQR 3.85,
#               fences -7.175 / 8.225, so 10.4 is an outlier and the upper
#               whisker stops at 2.9 - NOT at the maximum
#
#The second dataset is the one that earns its keep: it is the only example in
#the lecture where a whisker does not reach the extreme value, which is the
#whole point of the 1.5 x IQR rule.

suppressPackageStartupMessages({
  library(ggplot2)
  library(ggpubr)
})

set.seed(2026)

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

NAVY <- "#1F3864"
BLUE <- "#4472C4"
PALE <- "#D9E5F7"
RED  <- "#C00000"
GREY <- "#666666"

#----------------------------------------------------------------------------
#The hand method, and the fences that follow from it.

hand_quartiles <- function(x) {
  xs <- sort(x); n <- length(xs)
  half <- n %/% 2                       # excludes the middle value when n is odd
  c(Q1 = median(xs[1:half]),
    Q2 = median(xs),
    Q3 = median(xs[(n - half + 1):n]))
}

five_num <- function(x) {
  q <- hand_quartiles(x)
  iqr <- unname(q["Q3"] - q["Q1"])
  lo  <- unname(q["Q1"]) - 1.5 * iqr
  hi  <- unname(q["Q3"]) + 1.5 * iqr
  inl <- x[x >= lo & x <= hi]
  list(min = min(x), max = max(x), Q1 = unname(q["Q1"]), Q2 = unname(q["Q2"]),
       Q3 = unname(q["Q3"]), IQR = iqr, lo_fence = lo, hi_fence = hi,
       lo_whisker = min(inl), hi_whisker = max(inl),
       outliers = sort(x[x < lo | x > hi]))
}

#Draws a boxplot from a five_num() list rather than from the data, so the box,
#the whiskers and the flagged points are exactly the ones the slides claim.
#stat = "identity" is the point: ggplot's own geom_boxplot would recompute the
#hinges with a different convention and quietly disagree with the board.
draw_box <- function(s, xlab, xlim = NULL, breaks = waiver(), width = 0.42) {
  p <- ggplot() +
    geom_boxplot(aes(y = 0, xlower = s$Q1, xmiddle = s$Q2, xupper = s$Q3,
                     xmin = s$lo_whisker, xmax = s$hi_whisker),
                 stat = "identity", orientation = "y", width = width,
                 fill = PALE, colour = NAVY, linewidth = 0.8)
  if (length(s$outliers)) {
    p <- p + geom_point(aes(x = s$outliers, y = 0), shape = 21, size = 4,
                        fill = RED, colour = RED, stroke = 0.8)
  }
  p + scale_x_continuous(limits = xlim, breaks = breaks) +
    scale_y_continuous(NULL, breaks = NULL, limits = c(-0.55, 0.75)) +
    labs(x = xlab) +
    theme_minimal(base_size = 15) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.title.x = element_text(size = 14, colour = NAVY),
          axis.text.x = element_text(size = 13))
}

exam <- c(61, 61, 65, 65, 66, 68, 69, 73, 74, 75, 76, 78, 79, 90, 94)
xset <- c(-5.7, -2.6, -1.5, -1.3, -0.4, 0.2, 1.5, 2.2, 2.3, 2.6, 2.9, 10.4)

s_exam <- five_num(exam)
s_xset <- five_num(xset)

#The slides print these numbers. If a dataset or the method ever changes, this
#stops the figure rather than letting it drift out of step with the board.
stopifnot(
  s_exam$Q1 == 65, s_exam$Q2 == 73, s_exam$Q3 == 78, s_exam$IQR == 13,
  s_exam$lo_fence == 45.5, s_exam$hi_fence == 97.5,
  length(s_exam$outliers) == 0,
  s_exam$lo_whisker == 61, s_exam$hi_whisker == 94)
stopifnot(
  abs(s_xset$Q1 - (-1.4)) < 1e-9, abs(s_xset$Q2 - 0.85) < 1e-9,
  abs(s_xset$Q3 - 2.45) < 1e-9, abs(s_xset$IQR - 3.85) < 1e-9,
  abs(s_xset$hi_fence - 8.225) < 1e-9, abs(s_xset$lo_fence - (-7.175)) < 1e-9,
  identical(s_xset$outliers, 10.4),
  s_xset$lo_whisker == -5.7, s_xset$hi_whisker == 2.9)

#----------------------------------------------------------------------------
#1. boxplot anatomy, with the fences
#
#   Drawn on the exam scores so the anatomy slide and the practice that follows
#   it are the same picture. The fences are shown as dashed rules with their
#   arithmetic spelled out, because the rule is what the study guide tests and
#   nothing else in the deck states it.

lab_y  <- 0.36
tick_y <- 0.26

p_anat <- draw_box(s_exam, "Exam score", xlim = c(42, 101),
                   breaks = seq(45, 100, 5)) +
  geom_vline(xintercept = c(s_exam$lo_fence, s_exam$hi_fence),
             linetype = "dashed", colour = RED, linewidth = 0.7) +
  annotate("text", x = s_exam$lo_fence, y = 0.62, colour = RED, size = 3.9,
           hjust = 0, vjust = 0.5, label = "  Q1 - 1.5(IQR) = 45.5") +
  annotate("text", x = s_exam$hi_fence, y = 0.62, colour = RED, size = 3.9,
           hjust = 1, vjust = 0.5, label = "Q3 + 1.5(IQR) = 97.5  ") +
  annotate("segment", x = c(s_exam$lo_whisker, s_exam$Q1, s_exam$Q2,
                            s_exam$Q3, s_exam$hi_whisker),
           xend = c(s_exam$lo_whisker, s_exam$Q1, s_exam$Q2,
                    s_exam$Q3, s_exam$hi_whisker),
           y = tick_y, yend = tick_y + 0.06, colour = NAVY, linewidth = 0.5) +
  annotate("text",
           x = c(s_exam$lo_whisker, s_exam$Q1, s_exam$Q2, s_exam$Q3, s_exam$hi_whisker),
           y = lab_y, colour = NAVY, size = 3.9, vjust = 0,
           label = c("Min\n61", "Q1\n65", "Median\n73", "Q3\n78", "Max\n94")) +
  annotate("segment", x = s_exam$Q1, xend = s_exam$Q3, y = -0.36, yend = -0.36,
           colour = BLUE, linewidth = 0.7,
           arrow = arrow(ends = "both", length = unit(0.16, "cm"))) +
  annotate("text", x = s_exam$Q2, y = -0.48, colour = BLUE, size = 4,
           label = "IQR = 78 - 65 = 13")
ggsave(file.path(assets_dir, "l6_boxplot_anatomy.png"), p_anat,
       width = 9, height = 4, dpi = 200, bg = "white")

#----------------------------------------------------------------------------
#2. worked answer - the exam scores
#
#   Both fences fall outside the data (45.5 below the minimum, 97.5 above the
#   maximum), so both whiskers reach the extremes and nothing is flagged. That
#   is the ordinary case, and it is worth seeing before the case that is not.

p_exam <- draw_box(s_exam, "Exam score", xlim = c(58, 97), breaks = seq(60, 95, 5)) +
  annotate("segment", x = c(61, 65, 73, 78, 94), xend = c(61, 65, 73, 78, 94),
           y = tick_y, yend = tick_y + 0.06, colour = NAVY, linewidth = 0.5) +
  annotate("text", x = c(61, 65, 73, 78, 94), y = lab_y, colour = NAVY,
           size = 4.1, vjust = 0,
           label = c("Min\n61", "Q1\n65", "Med\n73", "Q3\n78", "Max\n94")) +
  annotate("text", x = 77.5, y = -0.42, colour = GREY, size = 3.9, hjust = 0.5,
           label = "fences 45.5 and 97.5 - no observation falls outside them")
ggsave(file.path(assets_dir, "l6_boxplot_exam.png"), p_exam,
       width = 9, height = 3.6, dpi = 200, bg = "white")

#----------------------------------------------------------------------------
#3. worked answer - the 12-value set
#
#   10.4 sits past the upper fence, so it is plotted as its own point and the
#   upper whisker stops at 2.9, the largest observation that is NOT an outlier.
#   This is the figure that shows a whisker is not simply "the maximum".

p_out <- draw_box(s_xset, "X", xlim = c(-8.6, 11.6), breaks = seq(-8, 11, 2)) +
  geom_vline(xintercept = s_xset$hi_fence, linetype = "dashed",
             colour = RED, linewidth = 0.7) +
  annotate("text", x = s_xset$hi_fence, y = 0.62, colour = RED, size = 3.9,
           hjust = 1, vjust = 0.5, label = "Q3 + 1.5(IQR) = 8.225  ") +
  annotate("segment", x = c(-5.7, -1.4, 0.85, 2.45),
           xend = c(-5.7, -1.4, 0.85, 2.45),
           y = tick_y, yend = tick_y + 0.06, colour = NAVY, linewidth = 0.5) +
  annotate("text", x = c(-5.7, -1.4, 0.85, 2.45), y = lab_y, colour = NAVY,
           size = 3.9, vjust = 0,
           label = c("Min\n-5.7", "Q1\n-1.4", "Med\n0.85", "Q3\n2.45")) +
  # The whisker label goes UNDERNEATH: at 2.9 it is only 0.45 from Q3, and
  # above the box the two labels overprint each other.
  annotate("segment", x = 2.9, xend = 2.9, y = -0.30, yend = -0.14,
           colour = BLUE, linewidth = 0.5) +
  annotate("text", x = 2.9, y = -0.36, colour = BLUE, size = 3.9, vjust = 1,
           hjust = 0.35, label = "whisker stops at 2.9, the largest value inside the fence") +
  annotate("text", x = 10.4, y = 0.28, colour = RED, size = 3.9, vjust = 0,
           label = "outlier\n10.4")
ggsave(file.path(assets_dir, "l6_boxplot_outlier.png"), p_out,
       width = 9, height = 3.8, dpi = 200, bg = "white")

#----------------------------------------------------------------------------
#4. what shape looks like in a boxplot
#
#   Same four panels as the figure in the Week 3 notes, redrawn in the deck's
#   palette. The density sits behind each boxplot so the class can see which
#   feature of the shape the boxplot keeps (skew, through the whisker lengths)
#   and which it throws away - the bimodal panel is the one that matters, since
#   its boxplot is indistinguishable from a symmetric one.

shape_panel <- function(v, title, box_y, box_w) {
  ggplot() +
    geom_density(aes(x = v), fill = PALE, colour = NAVY, linewidth = 0.7,
                 alpha = 0.85) +
    # outlier.shape = NA on purpose. These panels are drawn from 100,000 draws
    # so the density curve is smooth, and at that n the 1.5 x IQR rule flags
    # thousands of points - a solid red carpet running off both ends that reads
    # as "real data is mostly outliers". The teaching point here is the SHAPE:
    # where the median sits in the box and how the whisker lengths compare.
    # Outliers get their own figure, l6_boxplot_outlier.png, at a sane n.
    geom_boxplot(aes(x = v, y = box_y), width = box_w, fill = "white",
                 colour = NAVY, linewidth = 0.6, outlier.shape = NA) +
    labs(x = title) +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.title.x = element_text(size = 13, colour = NAVY),
          axis.title.y = element_blank(),
          axis.text.y = element_blank())
}

n <- 100000
sym  <- rnorm(n)
sk_l <- rbeta(n, 10, 2)
sk_r <- rbeta(n, 2, 10)
bim  <- c(rnorm(n / 2), rnorm(n / 2, 4))

p_shapes <- ggarrange(
  shape_panel(sym,  "Symmetric",  box_y = -0.10, box_w = 0.10),
  shape_panel(sk_l, "Skew left",  box_y = -0.35, box_w = 0.35),
  shape_panel(sk_r, "Skew right", box_y = -0.35, box_w = 0.35),
  shape_panel(bim,  "Bimodal",    box_y = -0.05, box_w = 0.05),
  nrow = 2, ncol = 2)
ggsave(file.path(assets_dir, "l6_boxplot_shapes.png"), p_shapes,
       width = 9, height = 5.4, dpi = 200, bg = "white")

#----------------------------------------------------------------------------
message("Wrote 4 figures to ", assets_dir)
message("\nChecks (these are the numbers on the slides and in the Week 3 notes):")
message(sprintf("  exam scores : Q1 %g  Q2 %g  Q3 %g  IQR %g",
                s_exam$Q1, s_exam$Q2, s_exam$Q3, s_exam$IQR))
message(sprintf("                fences %g / %g, whiskers %g / %g, %d outlier(s)",
                s_exam$lo_fence, s_exam$hi_fence, s_exam$lo_whisker,
                s_exam$hi_whisker, length(s_exam$outliers)))
message(sprintf("  12-value X  : Q1 %g  Q2 %g  Q3 %g  IQR %g",
                s_xset$Q1, s_xset$Q2, s_xset$Q3, s_xset$IQR))
message(sprintf("                fences %g / %g, whiskers %g / %g, outlier(s) %s",
                s_xset$lo_fence, s_xset$hi_fence, s_xset$lo_whisker,
                s_xset$hi_whisker, paste(s_xset$outliers, collapse = ", ")))
for (f in c("l6_boxplot_anatomy.png", "l6_boxplot_exam.png",
            "l6_boxplot_outlier.png", "l6_boxplot_shapes.png")) {
  d <- dim(png::readPNG(file.path(assets_dir, f)))
  message(sprintf("  %-26s %d x %d px   aspect %.4f  (slide frame must match)",
                  f, d[2], d[1], d[2] / d[1]))
}
