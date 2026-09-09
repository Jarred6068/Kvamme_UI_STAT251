#Builds the figures for the Week 3 Lecture 7 deck (Friday 9/11/2026), the last
#lecture of Exam 1 material: the cumulative distribution, and reading
#percentiles and quartiles off it.
#
#  l7_cereal_crf_table.png    x, f(x), rf(x), crf(x) for cereal sugar
#  l7_cdf_cereal.png          the step plot, plain
#  l7_cdf_cereal_quartiles.png  the same plot with Q1, Q2, Q3 read off it
#  l7_cdf_even_rule.png       why the 90th percentile is 15.5 and not 15
#  l7_cdf_exam.png            practice - the 15 exam scores, no answers
#  l7_cdf_exam_answer.png     the answer, with the three quartiles marked
#
#TWO DATASETS, chosen so that between them they cover both branches of the rule
#the class has to learn.
#
#  cereal sugar (docs/Data/cereal.csv, n = 20, EVEN) is the worked example.
#  Because n is even, several crf values land EXACTLY on a percentile, and the
#  percentile is then the midpoint of that horizontal bar:
#
#      crf hits 0.50 exactly at x = 9, next value 10  ->  Q2 = 9.5
#      crf hits 0.75 exactly at x = 12, next value 14 ->  Q3 = 13
#      crf hits 0.90 exactly at x = 15, next value 16 ->  90th = 15.5
#      crf never hits 0.25 (0.20 -> 0.30), so step up  ->  Q1 = 4
#
#  the 15 exam scores (n = 15, ODD) are the practice. No crf lands on a
#  quartile, so every one is read straight off the first value whose crf passes
#  the target, and the answers are the SAME Q1 65, Q2 73, Q3 78 the class
#  computed by halving in Lecture 6. That is the point of using them: the two
#  methods have to agree, and here the students can see that they do.
#
#Every number the slides print is asserted below against the hand method the
#course teaches, so the figures cannot drift out of step with the board. Note
#that R's quantile() default (type = 7) does NOT reproduce these - it gives
#Q3 = 12.5 for the cereal sugar rather than 13. Do not "fix" this by switching
#to quantile(); type = 2 is the one that matches, and the hand method is the
#definition.

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
data_dir   <- file.path(dirname(assets_dir), "Data")

NAVY <- "#1F3864"
BLUE <- "#4472C4"
RED  <- "#C00000"
GREY <- "#8C8C8C"

#----------------------------------------------------------------------------
#The crf table, and reading a percentile off it.

crf_table <- function(x) {
  tb  <- table(x)
  val <- as.numeric(names(tb))
  f   <- as.integer(tb)
  data.frame(x = val, f = f, rf = f / length(x), crf = cumsum(f) / length(x))
}

#The rule the deck teaches, in code.
#
#  Walk up the y axis to p and read across. If a horizontal bar sits EXACTLY at
#  height p, the percentile is the midpoint of that bar - the average of the
#  value the bar starts at and the next value up. Otherwise the bar at height p
#  is the one belonging to the first value whose crf has passed p, and the
#  percentile is that value.
percentile_from_crf <- function(tab, p, tol = 1e-9) {
  hit <- which(abs(tab$crf - p) < tol)
  if (length(hit) && hit[1] < nrow(tab)) {
    return((tab$x[hit[1]] + tab$x[hit[1] + 1]) / 2)
  }
  tab$x[which(tab$crf >= p - tol)[1]]
}

#The halving method from Lecture 5, kept here only to check the crf reading.
hand_quartiles <- function(x) {
  xs <- sort(x); n <- length(xs); half <- n %/% 2
  c(Q1 = median(xs[1:half]), Q2 = median(xs), Q3 = median(xs[(n - half + 1):n]))
}

#----------------------------------------------------------------------------
#Data, and the checks the slides depend on.

cereal <- read.csv(file.path(data_dir, "cereal.csv"), stringsAsFactors = FALSE)
sugar  <- cereal$Sugar
stopifnot(length(sugar) == 20, length(sugar) %% 2 == 0)

exam <- c(61, 61, 65, 65, 66, 68, 69, 73, 74, 75, 76, 78, 79, 90, 94)
stopifnot(length(exam) == 15, length(exam) %% 2 == 1)

tab_sugar <- crf_table(sugar)
tab_exam  <- crf_table(exam)

P_SHOWN <- c(0.10, 0.25, 0.30, 0.50, 0.75, 0.90)
pct_sugar <- vapply(P_SHOWN, function(p) percentile_from_crf(tab_sugar, p), numeric(1))
#These six are the numbers printed on the slide and in the Week 3 notes.
stopifnot(identical(pct_sugar, c(2, 4, 4.5, 9.5, 13, 15.5)))

q_sugar_crf  <- vapply(c(.25, .50, .75), function(p) percentile_from_crf(tab_sugar, p), numeric(1))
q_sugar_hand <- unname(hand_quartiles(sugar))
stopifnot(identical(q_sugar_crf, q_sugar_hand), identical(q_sugar_crf, c(4, 9.5, 13)))

q_exam_crf  <- vapply(c(.25, .50, .75), function(p) percentile_from_crf(tab_exam, p), numeric(1))
q_exam_hand <- unname(hand_quartiles(exam))
stopifnot(identical(q_exam_crf, q_exam_hand), identical(q_exam_crf, c(65, 73, 78)))

#----------------------------------------------------------------------------
#The step plot.
#
#  geom_step(direction = "hv") is what makes this a cumulative distribution
#  rather than a line chart: from each value the curve runs FLAT to the right
#  until the next observed value, then jumps. The flat run is the whole point -
#  it is the horizontal bar the percentile rule reads off.
#
#  A leading segment at height 0 is drawn in front of the first value so the
#  plot shows the curve starting from zero rather than appearing at the first
#  data point already part-way up.

step_plot <- function(tab, xlab, xlim, xbreaks, ybreaks = seq(0, 1, 0.1)) {
  lead <- data.frame(x = xlim[1], crf = 0)
  ggplot() +
    geom_step(data = rbind(lead, tab[, c("x", "crf")]), aes(x = x, y = crf),
              direction = "hv", colour = NAVY, linewidth = 0.9) +
    geom_point(data = tab, aes(x = x, y = crf), size = 2.6, colour = NAVY) +
    scale_x_continuous(limits = xlim, breaks = xbreaks) +
    scale_y_continuous(limits = c(0, 1), breaks = ybreaks) +
    labs(x = xlab, y = "Cumulative relative frequency") +
    theme_minimal(base_size = 15) +
    theme(panel.grid.minor = element_blank(),
          axis.title = element_text(size = 14, colour = NAVY))
}

#Dotted guides from the y axis across to the curve and down to the value, which
#is the motion the class makes with a finger on the projected slide.
guides_for <- function(p, v, xlim, colour) {
  list(
    annotate("segment", x = xlim[1], xend = v, y = p, yend = p,
             linetype = "dotted", linewidth = 0.7, colour = colour),
    annotate("segment", x = v, xend = v, y = p, yend = 0,
             linetype = "dotted", linewidth = 0.7, colour = colour)
  )
}

SUGAR_XLIM <- c(-1, 19)
SUGAR_BRK  <- seq(0, 18, 2)

#----------------------------------------------------------------------------
#1. the plain step plot

p_plain <- step_plot(tab_sugar, "Sugar content (g)", SUGAR_XLIM, SUGAR_BRK)
ggsave(file.path(assets_dir, "l7_cdf_cereal.png"), p_plain,
       width = 8, height = 5, dpi = 200, bg = "white")

#----------------------------------------------------------------------------
#2. the same plot with the three quartiles read off it

p_q <- step_plot(tab_sugar, "Sugar content (g)", SUGAR_XLIM, SUGAR_BRK) +
  guides_for(0.25, q_sugar_crf[1], SUGAR_XLIM, BLUE) +
  guides_for(0.50, q_sugar_crf[2], SUGAR_XLIM, RED) +
  guides_for(0.75, q_sugar_crf[3], SUGAR_XLIM, BLUE) +
  annotate("point", x = q_sugar_crf, y = c(0.25, 0.50, 0.75), size = 3.4,
           shape = 21, fill = "white", colour = c(BLUE, RED, BLUE), stroke = 1.2) +
  annotate("text", x = q_sugar_crf, y = c(0.25, 0.50, 0.75) - 0.055,
           colour = c(BLUE, RED, BLUE), size = 4.3, hjust = -0.08,
           label = c("Q1 = 4", "Q2 = 9.5", "Q3 = 13"))
ggsave(file.path(assets_dir, "l7_cdf_cereal_quartiles.png"), p_q,
       width = 8, height = 5, dpi = 200, bg = "white")

#----------------------------------------------------------------------------
#3. the even-n rule, at the 90th percentile
#
#   Zoomed to the top right corner so the horizontal bar from 15 to 16 fills
#   enough of the frame to be argued over. crf reaches exactly 0.90 at x = 15,
#   so reading across at 0.90 lands ON a bar rather than crossing a jump, and
#   the percentile is the midpoint of that bar - 15.5, not 15.

#The zoom is done with coord_cartesian, NOT by narrowing the scale limits.
#scale_*_continuous(limits = ...) DROPS every row outside the window, which
#would delete the points the step line is built from and leave the curve
#starting in mid-air at the left edge of the zoom. coord_cartesian keeps all the
#data and only changes the viewport.
p_even <- step_plot(tab_sugar, "Sugar content (g)", SUGAR_XLIM, 12:18,
                    ybreaks = seq(0, 1, 0.05)) +
  coord_cartesian(xlim = c(11.4, 18.6), ylim = c(0.6, 1.0)) +
  annotate("segment", x = 11.4, xend = 16, y = 0.90, yend = 0.90,
           linetype = "dotted", linewidth = 0.8, colour = RED) +
  annotate("segment", x = 15, xend = 16, y = 0.90, yend = 0.90,
           linewidth = 2.2, colour = RED, alpha = 0.35) +
  annotate("point", x = 15.5, y = 0.90, size = 4, shape = 21, fill = "white",
           colour = RED, stroke = 1.3) +
  annotate("segment", x = 15.5, xend = 15.5, y = 0.90, yend = 0.615,
           linetype = "dotted", linewidth = 0.8, colour = RED) +
  annotate("text", x = 13.4, y = 0.925, colour = RED, size = 4.4,
           label = "the bar sits exactly at 0.90") +
  annotate("text", x = 15.5, y = 0.655, colour = RED, size = 4.6,
           label = "90th percentile = (15 + 16)/2 = 15.5")
ggsave(file.path(assets_dir, "l7_cdf_even_rule.png"), p_even,
       width = 8, height = 5, dpi = 200, bg = "white")

#----------------------------------------------------------------------------
#4 and 5. the practice - the 15 exam scores from Lecture 6, and its answer

EXAM_XLIM <- c(58, 97)
EXAM_BRK  <- seq(60, 95, 5)

p_ex <- step_plot(tab_exam, "Exam score", EXAM_XLIM, EXAM_BRK)
ggsave(file.path(assets_dir, "l7_cdf_exam.png"), p_ex,
       width = 8, height = 5, dpi = 200, bg = "white")

p_ex_ans <- step_plot(tab_exam, "Exam score", EXAM_XLIM, EXAM_BRK) +
  guides_for(0.25, q_exam_crf[1], EXAM_XLIM, BLUE) +
  guides_for(0.50, q_exam_crf[2], EXAM_XLIM, RED) +
  guides_for(0.75, q_exam_crf[3], EXAM_XLIM, BLUE) +
  annotate("point", x = q_exam_crf, y = c(0.25, 0.50, 0.75), size = 3.4,
           shape = 21, fill = "white", colour = c(BLUE, RED, BLUE), stroke = 1.2) +
  annotate("text", x = q_exam_crf, y = c(0.25, 0.50, 0.75) - 0.055,
           colour = c(BLUE, RED, BLUE), size = 4.3, hjust = -0.08,
           label = c("Q1 = 65", "Q2 = 73", "Q3 = 78")) +
  annotate("text", x = 96, y = 0.06, colour = GREY, size = 4, hjust = 1,
           label = "the same quartiles the halving method gave in Lecture 6")
ggsave(file.path(assets_dir, "l7_cdf_exam_answer.png"), p_ex_ans,
       width = 8, height = 5, dpi = 200, bg = "white")

#----------------------------------------------------------------------------
#6. the crf table for cereal sugar
#
#   Rendered through headless Chrome, the same way make_lecture5_quartile_table.R
#   does it, so it matches the serif tables already in the Week 2 and 3 decks.
#   The rows the quartiles are read from are marked, and the two that require
#   the midpoint rule say so.

MARK <- RED
CANVAS_W <- 760L
CANVAS_H <- 620L

qlabel <- function(v) {
  if (identical(v, 4))   return("&#8592; crf passes 0.25")
  if (identical(v, 9))   return("&#8592; crf = 0.50 exactly")
  if (identical(v, 12))  return("&#8592; crf = 0.75 exactly")
  ""
}

rows <- vapply(seq_len(nrow(tab_sugar)), function(i) {
  note <- qlabel(tab_sugar$x[i])
  cls  <- if (nzchar(note)) "right hit" else "right"
  paste0("<tr>",
         sprintf('<td class="right">%g</td>', tab_sugar$x[i]),
         sprintf('<td class="right">%d</td>', tab_sugar$f[i]),
         sprintf('<td class="right">%.2f</td>', tab_sugar$rf[i]),
         sprintf('<td class="%s">%.2f</td>', cls, tab_sugar$crf[i]),
         sprintf('<td class="left"><span class="q">%s</span></td>', note),
         "</tr>")
}, character(1))

html <- sprintf('<!doctype html><html><head><meta charset="utf-8"><style>
  html, body { margin:0; padding:0; width:100%%; height:100%%;
               background:#ffffff; overflow:hidden; }
  body { display:flex; flex-direction:column; justify-content:center;
         font-family:"Latin Modern Roman","CMU Serif","Times New Roman",Times,serif;
         font-size:22px; color:#000000; }
  table { width:100%%; border-collapse:collapse; table-layout:fixed; }
  td { height:33px; padding:0 10px; white-space:nowrap; line-height:33px; }
  th { font-weight:normal; border-bottom:1px solid #000000; padding:0 10px;
       line-height:30px; height:38px; vertical-align:bottom; }
  thead tr { border-top:2.5px solid #000000; }
  tbody tr:last-child { border-bottom:2.5px solid #000000; }
  .left  { text-align:left; }
  .right { text-align:right; }
  .hit   { color:%s; font-weight:bold; }
  .q     { color:%s; font-weight:bold; font-size:18px; padding-left:4px; }
</style></head><body>
<table>
<colgroup><col style="width:11%%"><col style="width:13%%"><col style="width:16%%"><col style="width:18%%"><col style="width:42%%"></colgroup>
<thead><tr><th class="right">x</th><th class="right">f(x)</th><th class="right">rf(x)</th><th class="right">crf(x)</th><th class="left"></th></tr></thead>
<tbody>%s</tbody></table>
</body></html>', MARK, MARK, paste0(rows, collapse = ""))

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

out <- file.path(assets_dir, "l7_cereal_crf_table.png")
tmp <- tempfile(fileext = ".html")
writeLines(html, tmp, useBytes = TRUE)
status <- system2(find_chrome(), c(
  "--headless=new", "--disable-gpu", "--hide-scrollbars",
  paste0("--user-data-dir=", shQuote(file.path(tempdir(), "l7crf_chrome_profile"))),
  "--force-device-scale-factor=2", "--virtual-time-budget=4000",
  paste0("--window-size=", CANVAS_W, ",", CANVAS_H),
  paste0("--screenshot=", shQuote(normalizePath(out, winslash = "/", mustWork = FALSE))),
  shQuote(paste0("file:///", normalizePath(tmp, winslash = "/")))
), stdout = FALSE, stderr = FALSE)
unlink(tmp)
if (status != 0 || !file.exists(out)) stop("Chrome failed to render the table (status ", status, ")")

#----------------------------------------------------------------------------
#7. the raw data - all twenty cereals and their sugar content
#
#   Set in two columns of ten so it fits beside the step plot rather than
#   running the height of the slide. Sorted by sugar, because the first thing
#   the class does with it is order the values.

ord <- cereal[order(cereal$Sugar, cereal$Cereal), c("Cereal", "Sugar")]
half <- nrow(ord) / 2
stopifnot(half == 10)

drows <- vapply(seq_len(half), function(i) {
  j <- i + half
  paste0("<tr>",
         sprintf('<td class="left">%s</td><td class="right">%d</td>',
                 ord$Cereal[i], ord$Sugar[i]),
         '<td class="gap"></td>',
         sprintf('<td class="left">%s</td><td class="right">%d</td>',
                 ord$Cereal[j], ord$Sugar[j]),
         "</tr>")
}, character(1))

dhtml <- sprintf('<!doctype html><html><head><meta charset="utf-8"><style>
  html, body { margin:0; padding:0; width:100%%; height:100%%;
               background:#ffffff; overflow:hidden; }
  body { display:flex; flex-direction:column; justify-content:center;
         font-family:"Latin Modern Roman","CMU Serif","Times New Roman",Times,serif;
         font-size:20px; color:#000000; }
  table { width:100%%; border-collapse:collapse; table-layout:fixed; }
  td { height:30px; padding:0 8px; white-space:nowrap; line-height:30px;
       overflow:hidden; text-overflow:ellipsis; }
  th { font-weight:normal; border-bottom:1px solid #000000; padding:0 8px;
       line-height:28px; height:36px; vertical-align:bottom; }
  thead tr { border-top:2.5px solid #000000; }
  tbody tr:last-child { border-bottom:2.5px solid #000000; }
  .left  { text-align:left; }
  .right { text-align:right; }
  .gap   { width:26px; }
</style></head><body>
<table>
<colgroup><col style="width:31%%"><col style="width:12%%"><col style="width:6%%"><col style="width:37%%"><col style="width:14%%"></colgroup>
<thead><tr><th class="left">Cereal</th><th class="right">Sugar (g)</th><th class="gap"></th><th class="left">Cereal</th><th class="right">Sugar (g)</th></tr></thead>
<tbody>%s</tbody></table>
</body></html>', paste0(drows, collapse = ""))

dout <- file.path(assets_dir, "l7_cereal_data_table.png")
dtmp <- tempfile(fileext = ".html")
writeLines(dhtml, dtmp, useBytes = TRUE)
dstatus <- system2(find_chrome(), c(
  "--headless=new", "--disable-gpu", "--hide-scrollbars",
  paste0("--user-data-dir=", shQuote(file.path(tempdir(), "l7data_chrome_profile"))),
  "--force-device-scale-factor=2", "--virtual-time-budget=4000",
  "--window-size=980,400",
  paste0("--screenshot=", shQuote(normalizePath(dout, winslash = "/", mustWork = FALSE))),
  shQuote(paste0("file:///", normalizePath(dtmp, winslash = "/")))
), stdout = FALSE, stderr = FALSE)
unlink(dtmp)
if (dstatus != 0 || !file.exists(dout)) stop("Chrome failed to render the data table (status ", dstatus, ")")

#----------------------------------------------------------------------------
message("Wrote 7 figures to ", assets_dir)
message("\nChecks (these are the numbers on the slides and in the Week 3 notes):")
message("  cereal sugar, n = 20 (even)")
message(sprintf("    percentiles %s = %s",
                paste0(P_SHOWN * 100, "th", collapse = ", "),
                paste(pct_sugar, collapse = ", ")))
message(sprintf("    quartiles from crf %s  /  by halving %s   (must match)",
                paste(q_sugar_crf, collapse = ", "), paste(q_sugar_hand, collapse = ", ")))
message("  exam scores, n = 15 (odd)")
message(sprintf("    quartiles from crf %s  /  by halving %s   (must match)",
                paste(q_exam_crf, collapse = ", "), paste(q_exam_hand, collapse = ", ")))
message(sprintf("  cereal sugar IQR = Q3 - Q1 = %g - %g = %g   (the 2024 slide said 8; it is 9)",
                q_sugar_crf[3], q_sugar_crf[1], q_sugar_crf[3] - q_sugar_crf[1]))
for (f in c("l7_cereal_data_table.png", "l7_cereal_crf_table.png", "l7_cdf_cereal.png",
            "l7_cdf_cereal_quartiles.png", "l7_cdf_even_rule.png",
            "l7_cdf_exam.png", "l7_cdf_exam_answer.png")) {
  d <- dim(png::readPNG(file.path(assets_dir, f)))
  message(sprintf("  %-28s %d x %d px   aspect %.4f  (slide frame must match)",
                  f, d[2], d[1], d[2] / d[1]))
}
