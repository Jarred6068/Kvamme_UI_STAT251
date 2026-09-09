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
#5 and 6. the quartile convention slides
#
#  l6_quartile_halves.png    the worked example - median boxed, halves braced
#  l6_quartile_methods.png   the same data under three quartile definitions
#
#WHY THIS EXISTS. Students check their homework in R, get different quartiles
#from the ones they computed by hand, and conclude that one of the two is
#broken. Neither is: a sample quartile has several accepted definitions and the
#software has to pick one. Naming ours up front is cheaper than fielding the
#confusion later.
#
#THE ATTRIBUTION MATTERS, and it is easy to get backwards. The method this
#course teaches - find the median, then take the median of each half, LEAVING
#THE MEDIAN OUT of both halves when n is odd - is the Moore & McCabe convention
#used by most introductory texts. It is NOT Tukey's.
#
#Tukey's hinges INCLUDE the median in both halves when n is odd, so on the five
#values below Tukey's lower hinge is median(2, 4, 5) = 4, not 3. R's fivenum()
#computes Tukey's hinges, and boxplot() draws its box at them - so telling the
#class "we use Tukey's method" and then having them run boxplot() would show a
#box at 4 and 7 against a hand answer of 3 and 9.5, which is precisely the
#confusion the slide is meant to prevent.
#
#For EVEN n the two conventions coincide (there is no middle value to argue
#over), which is why every other example in this lecture agrees with boxplot().
#The disagreement only shows up when n is odd.

qx <- c(2, 4, 5, 7, 12)
stopifnot(length(qx) %% 2 == 1)

q_halves <- hand_quartiles(qx)                       # this class
q_r7      <- unname(quantile(qx, c(.25, .50, .75)))  # R's quantile() default
q_hinges  <- fivenum(qx)[2:4]                        # Tukey hinges = boxplot()

stopifnot(identical(unname(q_halves), c(3, 5, 9.5)),
          identical(q_r7, c(4, 5, 7)),
          identical(q_hinges, c(4, 5, 7)))
# boxplot() really does draw its box at the hinges, not at quantile()'s answer.
stopifnot(identical(boxplot(qx, plot = FALSE)$stats[c(2, 3, 4), 1], q_hinges))

find_chrome <- function() {
  chrome <- Sys.getenv("CHROME_BIN")
  if (nzchar(chrome) && file.exists(chrome)) return(chrome)
  candidates <- c(
    "C:/Program Files/Google/Chrome/Application/chrome.exe",
    "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe",
    "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe",
    Sys.which(c("google-chrome", "chromium", "chromium-browser")))
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (!length(candidates)) stop("Chrome not found; set CHROME_BIN.")
  candidates[[1]]
}

shoot <- function(html, out, w, h, profile) {
  tmp <- tempfile(fileext = ".html")
  writeLines(html, tmp, useBytes = TRUE)
  status <- system2(find_chrome(), c(
    "--headless=new", "--disable-gpu", "--hide-scrollbars",
    paste0("--user-data-dir=", shQuote(file.path(tempdir(), profile))),
    "--force-device-scale-factor=2", "--virtual-time-budget=4000",
    paste0("--window-size=", w, ",", h),
    paste0("--screenshot=", shQuote(normalizePath(out, winslash = "/", mustWork = FALSE))),
    shQuote(paste0("file:///", normalizePath(tmp, winslash = "/")))
  ), stdout = FALSE, stderr = FALSE)
  unlink(tmp)
  if (status != 0 || !file.exists(out)) stop("Chrome failed to render ", out, " (status ", status, ")")
}

#The braces are drawn with CSS borders rather than the U+FE38 bracket glyph,
#which is missing from most Windows faces and renders as a tofu box.
halves_html <- sprintf('<!doctype html><html><head><meta charset="utf-8"><style>
  html, body { margin:0; padding:0; width:100%%; height:100%%; background:#ffffff;
               overflow:hidden; }
  body { display:flex; flex-direction:column; justify-content:center; align-items:center;
         font-family:"Latin Modern Roman","CMU Serif","Times New Roman",Times,serif;
         color:#1F3864; }
  .vals { display:flex; gap:0; }
  .cell { width:130px; text-align:center; font-size:54px; }
  .med  { color:%s; }
  .medbox { display:inline-block; border:3px solid %s; border-radius:8px;
            padding:0 16px; }
  .braces { display:flex; gap:0; margin-top:10px; }
  .brace { height:22px; border-top:3px solid %s; border-left:3px solid %s;
           border-right:3px solid %s; }
  .w2 { width:242px; margin:0 9px; }
  .spacer { width:130px; }
  .labels { display:flex; gap:0; margin-top:8px; }
  .lab { text-align:center; font-size:30px; }
  .lab2 { width:260px; }
  .excl { width:130px; font-size:22px; color:#8C8C8C; }
  .ans { display:flex; gap:0; margin-top:26px; }
  .a2 { width:260px; text-align:center; font-size:32px; color:%s; }
</style></head><body>
  <div class="vals">
    <div class="cell">2</div><div class="cell">4</div>
    <div class="cell med"><span class="medbox">5</span></div>
    <div class="cell">7</div><div class="cell">12</div>
  </div>
  <div class="braces">
    <div class="brace w2"></div><div class="spacer"></div><div class="brace w2"></div>
  </div>
  <div class="labels">
    <div class="lab lab2">lower half</div>
    <div class="lab excl">median<br>left out</div>
    <div class="lab lab2">upper half</div>
  </div>
  <div class="ans">
    <div class="a2">Q1 = (2+4)/2 = 3</div>
    <div class="spacer"></div>
    <div class="a2">Q3 = (7+12)/2 = 9.5</div>
  </div>
</body></html>', RED, RED, "#4472C4", "#4472C4", "#4472C4", RED)

# The window is sized to the CONTENT. At 780x400 the figure sat in the middle of
# a tall white field, and once placed on a slide the whitespace, not the numbers,
# set how large it could be drawn.
shoot(halves_html, file.path(assets_dir, "l6_quartile_halves.png"),
      780, 262, "l6halves_chrome_profile")

fmt <- function(v) formatC(v, format = "fg", digits = 3)
methods_html <- sprintf('<!doctype html><html><head><meta charset="utf-8"><style>
  html, body { margin:0; padding:0; width:100%%; height:100%%; background:#ffffff;
               overflow:hidden; }
  body { display:flex; flex-direction:column; justify-content:center;
         font-family:"Latin Modern Roman","CMU Serif","Times New Roman",Times,serif;
         font-size:30px; color:#000000; }
  table { width:100%%; border-collapse:collapse; table-layout:fixed; }
  td { height:56px; padding:0 16px; white-space:nowrap; line-height:56px; }
  th { font-weight:normal; border-bottom:1px solid #000000; padding:0 16px;
       line-height:48px; height:60px; vertical-align:bottom; }
  thead tr { border-top:2.5px solid #000000; }
  tbody tr:last-child { border-bottom:2.5px solid #000000; }
  .left  { text-align:left; }
  .right { text-align:right; }
  .ours  { color:%s; font-weight:bold; }
</style></head><body>
<table>
<colgroup><col style="width:52%%"><col style="width:16%%"><col style="width:16%%"><col style="width:16%%"></colgroup>
<thead><tr><th class="left">Method</th><th class="right">Q1</th><th class="right">Median</th><th class="right">Q3</th></tr></thead>
<tbody>
<tr class="ours"><td class="left">Median of halves &ndash; this class</td><td class="right">%s</td><td class="right">%s</td><td class="right">%s</td></tr>
<tr><td class="left">R <span style="font-family:Consolas,monospace">quantile()</span> default</td><td class="right">%s</td><td class="right">%s</td><td class="right">%s</td></tr>
<tr><td class="left">R <span style="font-family:Consolas,monospace">boxplot()</span> hinges</td><td class="right">%s</td><td class="right">%s</td><td class="right">%s</td></tr>
</tbody></table>
</body></html>', RED,
  fmt(q_halves[1]), fmt(q_halves[2]), fmt(q_halves[3]),
  fmt(q_r7[1]),     fmt(q_r7[2]),     fmt(q_r7[3]),
  fmt(q_hinges[1]), fmt(q_hinges[2]), fmt(q_hinges[3]))

shoot(methods_html, file.path(assets_dir, "l6_quartile_methods.png"),
      900, 300, "l6methods_chrome_profile")

#----------------------------------------------------------------------------
#8 and 9. the running example, x = {2, 4, 5, 7, 12}
#
#  l6_deviation_dotplot.png  each value's deviation from the mean, drawn
#  l6_boxplot_class.png      the five number summary and boxplot for it
#
#From the IQR slide onward the lecture is worked on the board, all of it on this
#one small dataset, and these two figures are what the class needs in their notes
#afterwards. Every number below is also computed on the board:
#
#    n = 5      mean 6      range 10
#    Q1 3   Q2 5   Q3 9.5   IQR 6.5      five number summary 2, 3, 5, 9.5, 12
#    deviations  -4 -2 -1 1 6   (sum 0)
#    squares     16  4  1 1 36  (sum of squares 58)
#    s^2 = 58/4 = 14.5          s = 3.808
#    fences -6.75 and 19.25, so nothing is flagged
#
#WORTH KNOWING BEFORE YOU PRESENT IT: R's boxplot() draws this box at 4 and 7,
#not at 3 and 9.5, because it uses Tukey's hinges. That is not a mistake in the
#figure - it is the same split the quartile-convention slide opens the lecture
#with, showing up again in the picture. The boxplot below is drawn from the
#CLASS method via draw_box(), and the slide says so.

cls <- c(2, 4, 5, 7, 12)
cls_mean <- mean(cls)
cls_dev  <- cls - cls_mean
cls_ss   <- sum(cls_dev^2)
s_cls    <- five_num(cls)

stopifnot(
  cls_mean == 6, max(cls) - min(cls) == 10,
  identical(cls_dev, c(-4, -2, -1, 1, 6)), sum(cls_dev) == 0,
  cls_ss == 58, cls_ss / (length(cls) - 1) == 14.5,
  abs(sd(cls) - 3.8079) < 1e-4,
  s_cls$Q1 == 3, s_cls$Q2 == 5, s_cls$Q3 == 9.5, s_cls$IQR == 6.5,
  s_cls$lo_fence == -6.75, s_cls$hi_fence == 19.25,
  length(s_cls$outliers) == 0,
  s_cls$lo_whisker == 2, s_cls$hi_whisker == 12)

#----------------------------------------------------------------------------
#8. what a deviation is
#
#   The values sit on a number line with the mean drawn through them, and each
#   deviation is the horizontal gap from the value to that line, labelled with
#   its signed size. Two of the five are positive and three negative, and the
#   figure is the quickest way to see why they have to cancel.

dev_df <- data.frame(x = cls, dev = cls_dev,
                     lab = sprintf("%+g", cls_dev),
                     side = ifelse(cls_dev < 0, "below", "above"))

p_dev <- ggplot(dev_df) +
  annotate("segment", x = cls_mean, xend = cls_mean, y = 0.42, yend = 1.62,
           colour = RED, linewidth = 1.1) +
  annotate("text", x = cls_mean, y = 1.72, colour = RED, size = 4.6,
           label = "mean = 6") +
  geom_segment(aes(x = x, xend = cls_mean, y = seq_along(cls) * 0.22 + 0.34,
                   yend = seq_along(cls) * 0.22 + 0.34),
               linetype = "dotted", linewidth = 0.8, colour = NAVY) +
  geom_point(aes(x = x, y = seq_along(cls) * 0.22 + 0.34), size = 5,
             shape = 21, fill = PALE, colour = NAVY, stroke = 1.1) +
  geom_text(aes(x = (x + cls_mean) / 2, y = seq_along(cls) * 0.22 + 0.44,
                label = lab), colour = NAVY, size = 4.3) +
  # No second row of value labels: the axis already carries 2, 4, 5, 7 and 12
  # directly under the points, and printing them twice just crowds the figure.
  scale_x_continuous(limits = c(0.2, 13.4), breaks = seq(0, 13, 1)) +
  scale_y_continuous(NULL, breaks = NULL, limits = c(0.40, 1.84)) +
  labs(x = "x") +
  theme_minimal(base_size = 15) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title.x = element_text(size = 14, colour = NAVY),
        axis.text.x = element_text(size = 13))
ggsave(file.path(assets_dir, "l6_deviation_dotplot.png"), p_dev,
       width = 9, height = 4, dpi = 200, bg = "white")

#----------------------------------------------------------------------------
#9. the boxplot for the running example
#
#   The fences land at -6.75 and 19.25, well outside the data, so no point is
#   flagged and both whiskers reach the extremes. Min and Q1 are only one unit
#   apart, so their labels are staggered - side by side at this scale they
#   overprint.

p_cls <- draw_box(s_cls, "x", xlim = c(-8.5, 20.5), breaks = seq(-8, 20, 2)) +
  geom_vline(xintercept = c(s_cls$lo_fence, s_cls$hi_fence),
             linetype = "dashed", colour = RED, linewidth = 0.7) +
  annotate("text", x = s_cls$lo_fence, y = 0.64, colour = RED, size = 3.8,
           hjust = 0, vjust = 0.5, label = "  Q1 - 1.5(IQR) = -6.75") +
  annotate("text", x = s_cls$hi_fence, y = 0.64, colour = RED, size = 3.8,
           hjust = 1, vjust = 0.5, label = "Q3 + 1.5(IQR) = 19.25  ") +
  annotate("segment", x = c(2, 3, 5, 9.5, 12), xend = c(2, 3, 5, 9.5, 12),
           y = tick_y, yend = tick_y + 0.06, colour = NAVY, linewidth = 0.5) +
  annotate("text", x = c(2, 5, 9.5, 12), y = lab_y, colour = NAVY, size = 4.1,
           vjust = 0, label = c("Min\n2", "Med\n5", "Q3\n9.5", "Max\n12")) +
  annotate("text", x = 3, y = lab_y + 0.20, colour = NAVY, size = 4.1,
           vjust = 0, label = "Q1 = 3") +
  annotate("segment", x = s_cls$Q1, xend = s_cls$Q3, y = -0.36, yend = -0.36,
           colour = BLUE, linewidth = 0.7,
           arrow = arrow(ends = "both", length = unit(0.16, "cm"))) +
  annotate("text", x = s_cls$Q2, y = -0.48, colour = BLUE, size = 4,
           label = "IQR = 9.5 - 3 = 6.5")
ggsave(file.path(assets_dir, "l6_boxplot_class.png"), p_cls,
       width = 9, height = 4, dpi = 200, bg = "white")

#----------------------------------------------------------------------------
#7. sampling variation
#
#  l6_sampling_variation.png  five samples of ten die rolls, each with its own
#                             mean and standard deviation
#
#This is the figure for the slide that introduces the standard error. The point
#it has to make is concrete rather than theoretical: the class has just computed
#x-bar = 3.8 and s = 1.619 from ONE sample of ten rolls, and the natural question
#is what happens if you roll ten more times. Showing four more samples answers it
#without a word of theory - every statistic in the table moves.
#
#Row 1 is deliberately the sample from the "Try it out" slide, so the table
#starts on a number the class has already worked out by hand.
#
#The true SD of a fair die is sqrt(35/12) = 1.708, so the standard error of the
#mean of ten rolls is 1.708/sqrt(10) = 0.540. The spread of the five sample means
#below should look like that, and the check at the bottom of this script prints
#it next to the estimate s/sqrt(n) = 0.512 taken from row 1.

set.seed(251)

ours <- c(1, 2, 3, 3, 4, 4, 4, 5, 6, 6)
stopifnot(abs(mean(ours) - 3.8) < 1e-9, abs(sd(ours) - 1.6193) < 1e-3)

samples <- c(list(ours), replicate(4, sort(sample(1:6, 10, replace = TRUE)),
                                   simplify = FALSE))

se_est  <- sd(ours) / sqrt(10)                 # what the slide prints
se_true <- sqrt(35 / 12) / sqrt(10)            # the value it is estimating
stopifnot(abs(se_est - 0.512) < 5e-4)

srows <- vapply(seq_along(samples), function(i) {
  s <- samples[[i]]
  lab <- if (i == 1) "our sample" else paste("sample", i)
  cls <- if (i == 1) ' class="ours"' else ""
  paste0("<tr", cls, ">",
         sprintf('<td class="left">%s</td>', lab),
         sprintf('<td class="mono">%s</td>', paste(s, collapse = " ")),
         sprintf('<td class="right">%.2f</td>', mean(s)),
         sprintf('<td class="right">%.2f</td>', sd(s)),
         "</tr>")
}, character(1))

sampling_html <- sprintf('<!doctype html><html><head><meta charset="utf-8"><style>
  html, body { margin:0; padding:0; width:100%%; height:100%%; background:#ffffff;
               overflow:hidden; }
  body { display:flex; flex-direction:column; justify-content:center;
         font-family:"Latin Modern Roman","CMU Serif","Times New Roman",Times,serif;
         font-size:26px; color:#000000; }
  table { width:100%%; border-collapse:collapse; table-layout:fixed; }
  td { height:48px; padding:0 14px; white-space:nowrap; line-height:48px; }
  th { font-weight:normal; border-bottom:1px solid #000000; padding:0 14px;
       line-height:42px; height:52px; vertical-align:bottom; }
  thead tr { border-top:2.5px solid #000000; }
  tbody tr:last-child { border-bottom:2.5px solid #000000; }
  .left  { text-align:left; }
  .right { text-align:right; }
  .mono  { font-family:Consolas,"Courier New",monospace; font-size:23px;
           text-align:center; letter-spacing:1px; }
  .ours  { color:%s; font-weight:bold; }
</style></head><body>
<table>
<colgroup><col style="width:20%%"><col style="width:48%%"><col style="width:16%%"><col style="width:16%%"></colgroup>
<thead><tr><th class="left"></th><th class="mono">ten rolls</th><th class="right">mean</th><th class="right">s</th></tr></thead>
<tbody>%s</tbody></table>
</body></html>', RED, paste0(srows, collapse = ""))

shoot(sampling_html, file.path(assets_dir, "l6_sampling_variation.png"),
      920, 340, "l6samp_chrome_profile")

#----------------------------------------------------------------------------
message("Wrote 9 figures to ", assets_dir)
message(sprintf("\n  sampling variation, five samples of ten die rolls:"))
message(sprintf("    sample means : %s",
                paste(sprintf("%.2f", vapply(samples, mean, numeric(1))), collapse = ", ")))
message(sprintf("    s values     : %s",
                paste(sprintf("%.2f", vapply(samples, sd, numeric(1))), collapse = ", ")))
message(sprintf("    SE from our sample  s/sqrt(n) = %.3f", se_est))
message(sprintf("    true SE for a fair die        = %.3f  (the value it estimates)", se_true))
message(sprintf("\n  quartile conventions on {2, 4, 5, 7, 12}:"))
message(sprintf("    median of halves (this class) : %s", paste(q_halves, collapse = ", ")))
message(sprintf("    R quantile() default (type 7) : %s", paste(q_r7, collapse = ", ")))
message(sprintf("    R boxplot() / Tukey hinges    : %s", paste(q_hinges, collapse = ", ")))
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
            "l6_boxplot_outlier.png", "l6_boxplot_shapes.png",
            "l6_quartile_halves.png", "l6_quartile_methods.png",
            "l6_sampling_variation.png", "l6_deviation_dotplot.png",
            "l6_boxplot_class.png")) {
  d <- dim(png::readPNG(file.path(assets_dir, f)))
  message(sprintf("  %-26s %d x %d px   aspect %.4f  (slide frame must match)",
                  f, d[2], d[1], d[2] / d[1]))
}
