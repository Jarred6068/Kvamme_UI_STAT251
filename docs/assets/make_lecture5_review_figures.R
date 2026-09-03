#Builds the review figures for the Week 2 Lecture 5 deck (Friday 9/4), where the
#class rebuilds all three quantitative plots by hand from one small dataset
#before moving on to measures of location and position.
#
#  l5_review_games_table.png  the ten games and their scores (the raw data)
#  l5_review_freq_table.png   the binned frequency table
#  l5_review_dotplot.png      dot plot of the ten scores
#  l5_review_histogram.png    histogram on the same four bins
#
#The stem-and-leaf plot is NOT generated here. It is text, and it goes on the
#slide as a monospace text box so it looks like what students write by hand.
#
#THE DATA. Metacritic critic scores for a random sample of ten best-selling video
#games, drawn from assets/video_game_critic_scores.csv:
#
#    x = {68, 72, 76, 76, 76, 83, 85, 87, 92, 95}
#
#PROVENANCE. video_game_critic_scores.csv is derived from the VGChartz sales data
#joined to Metacritic ratings - the dataset published on Kaggle as "Video Game
#Sales with Ratings" (rush4ratio) and mirrored at
#github.com/Bakikhan/Video-Game-Sales-Dataset. From its 16,719 rows we kept every
#title that (a) has a Metacritic critic score, (b) has a global sales figure, and
#(c) sold at least 5 million copies, keeping one row per title - its best-selling
#platform. That leaves 127 well-known games, which is the pool sampled here.
#
#This is REAL data, not simulated. Two honest caveats worth knowing before you
#present it: the snapshot ends in 2016, so the newest title in the pool is FIFA
#17 and several games predate the students; and restricting to 5M+ sellers means
#the scores skew high (49 to 98) relative to games in general.
#
#set.seed(8) is not arbitrary. The draw was chosen because it separates the three
#measures of center in the textbook order, on data the class binned themselves:
#
#    mode 76  <  median 79.5  <  mean 81.0
#
#That is exactly the "skewed right: the tail pulls the mean right" panel from
#Lecture 4 slide 4. Three games tie at 76, so the mode is unambiguous, and the
#bins come out 1, 4, 3, 2 - a peak in the 70s with a tail running to the 90s.
#
#QUARTILES - the deck teaches the hand method: split the ordered data at the
#median, then take the median of each half. For n = 10 that gives Q1 = 76,
#Q3 = 87, IQR = 11. R's default quantile(type = 7) disagrees. Anything drawn or
#quoted here uses the HAND method so the figures agree with the board. Do not
#"fix" this by switching to quantile().

set.seed(8)

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

pool <- read.csv(file.path(assets_dir, "video_game_critic_scores.csv"),
                 stringsAsFactors = FALSE, encoding = "UTF-8")
samp <- pool[sample(nrow(pool), 10), ]
samp <- samp[order(samp$Critic_Score), ]
x <- samp$Critic_Score
stopifnot(identical(as.numeric(x), c(68, 72, 76, 76, 76, 83, 85, 87, 92, 95)))

BREAKS <- seq(60, 100, by = 10)
counts <- as.integer(table(cut(x, breaks = BREAKS, right = FALSE)))
labels <- sprintf("%d to under %d", head(BREAKS, -1), tail(BREAKS, -1))

base_theme <- theme_minimal(base_size = 15) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

#----------------------------------------------------------------------------
#1. dot plot
#
#   geom_dotplot rather than geom_point: it sizes and stacks the dots in DATA
#   units, so the three games tied at 76 stack touching each other the way a
#   student draws them. binwidth = 1 because scores are whole numbers.

p_dot <- ggplot(data.frame(x = x), aes(x = x)) +
  geom_dotplot(binwidth = 1, dotsize = 1.4, stackratio = 1.05,
               fill = BLUE, colour = EDGE, stroke = 0.6) +
  scale_x_continuous(breaks = seq(65, 100, 5), limits = c(65, 98)) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(x = "Metacritic critic score") +
  base_theme +
  theme(panel.grid.major.y = element_blank(), axis.text.y = element_blank())
ggsave(file.path(assets_dir, "l5_review_dotplot.png"), p_dot,
       width = 10, height = 2, dpi = 200, bg = "white")

#----------------------------------------------------------------------------
#2. histogram - the SAME four bins as the frequency table, drawn from the counts
#   so an empty bin would still occupy space on the axis rather than closing up.

p_hist <- ggplot(data.frame(mid = head(BREAKS, -1) + 5, count = counts),
                 aes(x = mid, y = count)) +
  geom_col(width = 10, fill = BLUE, colour = EDGE, linewidth = 0.6) +
  scale_x_continuous(breaks = BREAKS) +
  scale_y_continuous(breaks = 0:5, expand = expansion(mult = c(0, 0.08))) +
  labs(x = "Metacritic critic score", y = "Frequency") +
  base_theme
ggsave(file.path(assets_dir, "l5_review_histogram.png"), p_hist,
       width = 10, height = 4, dpi = 200, bg = "white")

#----------------------------------------------------------------------------
#3. the two tables, rendered through headless Chrome the same way
#   make_teens_frequency_table.R does it, so they match the serif tables already
#   in the Week 2 decks. Chrome is already a build dependency.

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

CSS <- '
  html, body { margin:0; padding:0; width:100%%; height:100%%;
               background:#ffffff; overflow:hidden; }
  body { display:flex; flex-direction:column; justify-content:center;
         font-family:"Latin Modern Roman","CMU Serif","Times New Roman",Times,serif;
         font-size:%dpx; color:#000000; }
  table { width:100%%; border-collapse:collapse; table-layout:fixed; }
  td { height:%dpx; padding:0 14px; white-space:nowrap; overflow:hidden;
       text-overflow:ellipsis; line-height:%dpx; }
  th { font-weight:normal; border-bottom:1px solid #000000; padding:0 14px;
       line-height:%dpx; height:%dpx; vertical-align:bottom; }
  thead tr { border-top:2.5px solid #000000; }
  tbody tr:last-child { border-bottom:2.5px solid #000000; }
  tr.total { border-top:1px solid #000000; }
  .left  { text-align:left; }
  .right { text-align:right; }
'

shoot <- function(html, out_png, w, h) {
  tmp <- tempfile(fileext = ".html")
  writeLines(html, tmp, useBytes = TRUE)
  status <- system2(find_chrome(), c(
    "--headless=new", "--disable-gpu", "--hide-scrollbars",
    paste0("--user-data-dir=", shQuote(file.path(tempdir(), "l5table_chrome_profile"))),
    "--force-device-scale-factor=2", "--virtual-time-budget=4000",
    paste0("--window-size=", w, ",", h),
    paste0("--screenshot=", shQuote(normalizePath(out_png, winslash = "/", mustWork = FALSE))),
    shQuote(paste0("file:///", normalizePath(tmp, winslash = "/")))
  ), stdout = FALSE, stderr = FALSE)
  unlink(tmp)
  if (status != 0 || !file.exists(out_png))
    stop("Chrome failed to render ", basename(out_png), " (status ", status, ")")
}

cell <- function(txt, cls) sprintf('<td class="%s">%s</td>', cls, txt)

# --- the raw data: ten games and their scores
game_rows <- vapply(seq_len(nrow(samp)), function(i) {
  paste0("<tr>",
         cell(samp$Name[i], "left"),
         cell(samp$Platform[i], "left"),
         cell(sprintf("%d", samp$Critic_Score[i]), "right"),
         "</tr>")
}, character(1))

games_html <- sprintf('<!doctype html><html><head><meta charset="utf-8"><style>%s</style></head><body>
<table>
<colgroup><col style="width:60%%"><col style="width:18%%"><col style="width:22%%"></colgroup>
<thead><tr><th class="left">Game</th><th class="left">Platform</th><th class="right">Score</th></tr></thead>
<tbody>%s</tbody></table></body></html>',
  sprintf(CSS, 24L, 42L, 42L, 34L, 34L), paste0(game_rows, collapse = ""))
shoot(games_html, file.path(assets_dir, "l5_review_games_table.png"), 900L, 560L)

# --- the binned frequency table
freq_rows <- vapply(seq_along(counts), function(i) {
  paste0("<tr>",
         cell(labels[i], "left"),
         cell(sprintf("%d", counts[i]), "right"),
         cell(sprintf("%.1f", counts[i] / length(x)), "right"),
         "</tr>")
}, character(1))
freq_total <- paste0('<tr class="total">', cell("Total", "left"),
                     cell(sprintf("%d", sum(counts)), "right"),
                     cell("1.0", "right"), "</tr>")

freq_html <- sprintf('<!doctype html><html><head><meta charset="utf-8"><style>%s</style></head><body>
<table>
<colgroup><col style="width:46%%"><col style="width:27%%"><col style="width:27%%"></colgroup>
<thead><tr><th class="left">Critic score</th><th class="right">Frequency</th><th class="right">Relative<br>Frequency</th></tr></thead>
<tbody>%s%s</tbody></table></body></html>',
  sprintf(CSS, 24L, 44L, 44L, 32L, 64L),
  paste0(freq_rows, collapse = ""), freq_total)
shoot(freq_html, file.path(assets_dir, "l5_review_freq_table.png"), 700L, 340L)

#----------------------------------------------------------------------------
#Everything the slides claim, printed so it can be checked against the figures.
#Quartiles by the HAND method taught in the deck, not quantile().
q1 <- median(x[1:5]); q3 <- median(x[6:10])
tb <- table(x)

message("Wrote four Lecture 5 review figures to ", assets_dir)
for (f in c("l5_review_games_table.png", "l5_review_freq_table.png",
            "l5_review_dotplot.png", "l5_review_histogram.png"))
  message("  ", f, " (", format(file.size(file.path(assets_dir, f)) %/% 1024L,
                                big.mark = ","), " KB)")

message("\nChecks:")
message("  x            = ", paste(x, collapse = ", "))
message("  n            = ", length(x))
message("  bin counts   = ", paste(counts, collapse = ", "))
message("  mode         = ", names(tb)[which.max(tb)], " (appears ", max(tb), " times)")
message(sprintf("  median       = %.1f", median(x)))
message(sprintf("  mean         = %.1f", mean(x)))
message(sprintf("  ordering     = mode %s < median %.1f < mean %.1f  (skewed right)",
                names(tb)[which.max(tb)], median(x), mean(x)))
message(sprintf("  range        = %d   (min %d, max %d)", diff(range(x)), min(x), max(x)))
message(sprintf("  Q1, Q3, IQR  = %g, %g, %g   (hand method)", q1, q3, q3 - q1))
message(sprintf("  sd, variance = %.2f, %.2f", sd(x), var(x)))
