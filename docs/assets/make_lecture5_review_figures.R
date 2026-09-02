#Builds the review figures for the Week 2 Lecture 5 deck (Friday 9/4), where the
#class rebuilds all three quantitative plots by hand from one small dataset
#before moving on to measures of location and position.
#
#  l5_review_freq_table.png   the binned frequency table
#  l5_review_dotplot.png      dot plot of the ten values
#  l5_review_histogram.png    histogram on the same five bins
#
#The stem-and-leaf plot is NOT generated here. It is text, and it goes on the
#slide as a monospace text box so it looks like what students write by hand.
#
#THE DATA. A random sample of ten Old Faithful waiting times, drawn from base R's
#`faithful` (272 eruptions, Azzalini & Bowman 1990) - the same dataset the class
#met on Wednesday as the bimodality example:
#
#    x = {45, 51, 52, 56, 77, 78, 78, 78, 79, 84}
#
#set.seed(21) is not arbitrary: the seed was chosen so the sample teaches three
#things at once, and every one of them is visible in these figures.
#
#  1. Binned in fives of width 10 the counts are 1, 3, 0, 5, 1 - there is an
#     EMPTY bin at [60, 70). The gap shows up in all three plots.
#  2. The mean is 67.8, which falls inside that empty gap. No eruption waited
#     67.8 minutes. This is the bimodal panel of Lecture 4 slide 4 all over
#     again, now on data the class binned themselves.
#  3. With n = 272 on Wednesday the two humps were obvious. With n = 10 the shape
#     is nearly invisible, which is the argument for computing numbers rather
#     than eyeballing a picture - i.e. for the rest of this lecture.
#
#QUARTILES - the deck teaches the hand method: split the ordered data at the
#median, then take the median of each half. For n = 10 that gives Q1 = 52,
#Q3 = 78, IQR = 26. R's default quantile(type = 7) gives 53, 78 and IQR 25
#instead. Anything drawn or quoted here uses the HAND method, so the figures
#agree with the board. Do not "fix" this by switching to quantile().

set.seed(21)

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

BLUE <- "#4472C4"
EDGE <- "#2F4E7D"

x <- sort(sample(faithful$waiting, 10))
stopifnot(identical(as.numeric(x), c(45, 51, 52, 56, 77, 78, 78, 78, 79, 84)))

BREAKS <- seq(40, 90, by = 10)
counts <- as.integer(table(cut(x, breaks = BREAKS, right = FALSE)))
labels <- sprintf("%d to under %d", head(BREAKS, -1), tail(BREAKS, -1))

base_theme <- theme_minimal(base_size = 15) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

#----------------------------------------------------------------------------
#1. dot plot - one dot per observation, stacked where values repeat.
#
#   geom_dotplot rather than geom_point: it sizes and stacks the dots in DATA
#   units, so the three 78s stack touching each other the way a student draws
#   them. Plain geom_point puts the stack on an arbitrary y scale and the dots
#   end up floating far apart at whatever figure height happens to be in use.
#   binwidth = 1 because the data are whole minutes.

df <- data.frame(x = x)

p_dot <- ggplot(df, aes(x = x)) +
  geom_dotplot(binwidth = 1, dotsize = 1.4, stackratio = 1.05,
               fill = BLUE, colour = EDGE, stroke = 0.6) +
  scale_x_continuous(breaks = seq(40, 90, 5), limits = c(42, 88)) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(x = "Waiting time (minutes)") +
  base_theme +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_blank())
ggsave(file.path(assets_dir, "l5_review_dotplot.png"), p_dot,
       width = 10, height = 2, dpi = 200, bg = "white")

#----------------------------------------------------------------------------
#2. histogram - the SAME five bins as the frequency table, so the class can see
#   the table become the picture. Bars are drawn from the counts rather than
#   from geom_histogram so the empty [60, 70) bin is guaranteed to occupy space
#   on the axis instead of silently closing up.

hist_df <- data.frame(mid = head(BREAKS, -1) + 5, count = counts)

p_hist <- ggplot(hist_df, aes(x = mid, y = count)) +
  geom_col(width = 10, fill = BLUE, colour = EDGE, linewidth = 0.6) +
  scale_x_continuous(breaks = BREAKS) +
  scale_y_continuous(breaks = 0:5, expand = expansion(mult = c(0, 0.08))) +
  labs(x = "Waiting time (minutes)", y = "Frequency") +
  base_theme
ggsave(file.path(assets_dir, "l5_review_histogram.png"), p_hist,
       width = 10, height = 4, dpi = 200, bg = "white")

#----------------------------------------------------------------------------
#3. frequency table - rendered through headless Chrome, the same way
#   make_teens_frequency_table.R does it, so the table matches the serif look of
#   the tables already in the Week 2 decks. Chrome is already a build dependency.

CANVAS_W <- 900L
CANVAS_H <- 400L
FONT_PX  <- 26L
ROW_PX   <- 44L

cell <- function(txt, cls) sprintf('<td class="%s">%s</td>', cls, txt)

rows <- vapply(seq_along(counts), function(i) {
  paste0("<tr>",
         cell(labels[i], "left"),
         cell(sprintf("%d", counts[i]), "right"),
         cell(sprintf("%.1f", counts[i] / length(x)), "right"),
         "</tr>")
}, character(1))

total <- paste0('<tr class="total">', cell("Total", "left"),
                cell(sprintf("%d", sum(counts)), "right"),
                cell("1.0", "right"), "</tr>")

html <- sprintf('<!doctype html><html><head><meta charset="utf-8"><style>
  html, body { margin:0; padding:0; width:100%%; height:100%%;
               background:#ffffff; overflow:hidden; }
  body { display:flex; flex-direction:column; justify-content:center;
         font-family:"Latin Modern Roman","CMU Serif","Times New Roman",Times,serif;
         font-size:%dpx; color:#000000; }
  table { width:100%%; border-collapse:collapse; table-layout:fixed; }
  td { height:%dpx; padding:0 14px; white-space:nowrap; line-height:%dpx; }
  th { font-weight:normal; border-bottom:1px solid #000000; padding:0 14px;
       line-height:%dpx; height:%dpx; vertical-align:bottom; }
  thead tr { border-top:2.5px solid #000000; }
  tbody tr:last-child { border-bottom:2.5px solid #000000; }
  tr.total { border-top:1px solid #000000; }
  .left  { text-align:left; }
  .right { text-align:right; }
</style></head><body>
<table>
<colgroup><col style="width:46%%"><col style="width:27%%"><col style="width:27%%"></colgroup>
<thead><tr><th class="left">Waiting time (minutes)</th><th class="right">Frequency</th><th class="right">Relative<br>Frequency</th></tr></thead>
<tbody>%s%s</tbody></table>
</body></html>', FONT_PX, ROW_PX, ROW_PX, 34L, 68L,
  paste0(rows, collapse = ""), total)

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

out_png <- file.path(assets_dir, "l5_review_freq_table.png")
tmp <- tempfile(fileext = ".html")
writeLines(html, tmp, useBytes = TRUE)
status <- system2(find_chrome(), c(
  "--headless=new", "--disable-gpu", "--hide-scrollbars",
  paste0("--user-data-dir=", shQuote(file.path(tempdir(), "l5table_chrome_profile"))),
  "--force-device-scale-factor=2", "--virtual-time-budget=4000",
  paste0("--window-size=", CANVAS_W, ",", CANVAS_H),
  paste0("--screenshot=", shQuote(normalizePath(out_png, winslash = "/", mustWork = FALSE))),
  shQuote(paste0("file:///", normalizePath(tmp, winslash = "/")))
), stdout = FALSE, stderr = FALSE)
unlink(tmp)
if (status != 0 || !file.exists(out_png))
  stop("Chrome failed to render the frequency table (status ", status, ")")

#----------------------------------------------------------------------------
#Everything the slides claim, printed so it can be checked against the figures.
#Quartiles by the HAND method taught in the deck, not quantile().
q1 <- median(x[1:5]); q3 <- median(x[6:10])
tb <- table(x)

message("Wrote three Lecture 5 review figures to ", assets_dir)
for (f in c("l5_review_dotplot.png", "l5_review_histogram.png", "l5_review_freq_table.png"))
  message("  ", f, " (", format(file.size(file.path(assets_dir, f)) %/% 1024L,
                                big.mark = ","), " KB)")

message("\nChecks:")
message("  x            = ", paste(x, collapse = ", "))
message("  n            = ", length(x))
message("  bin counts   = ", paste(counts, collapse = ", "),
        "   (the 0 is the empty 60s bin)")
message(sprintf("  mean         = %.1f   <- falls in the empty bin", mean(x)))
message(sprintf("  median       = %.1f", median(x)))
message("  mode         = ", names(tb)[which.max(tb)],
        " (appears ", max(tb), " times)")
message(sprintf("  range        = %d   (min %d, max %d)", diff(range(x)), min(x), max(x)))
message(sprintf("  Q1, Q3, IQR  = %g, %g, %g   (hand method)", q1, q3, q3 - q1))
message(sprintf("  sd, variance = %.2f, %.2f", sd(x), var(x)))
