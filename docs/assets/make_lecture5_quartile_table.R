#Builds the frequency table used on the Week 2 Lecture 5 slide that ties the
#quartiles to the cumulative relative frequency column.
#
#  l5_quartile_crf_table.png   x, f(x), rf(x), crf(x), with the row each
#                              quartile is read from marked
#
#THE DATA are the ten die rolls the class has already met twice in this deck -
#on the "Practice 2" slide, where they compute the mean three ways, the median
#and the mode, and whose f(x)/rf(x) table is already printed there:
#
#    Data = 1, 2, 3, 3, 4, 4, 4, 5, 6, 6      (n = 10)
#
#    x   f   rf    crf
#    1   1   0.1   0.1
#    2   1   0.1   0.2
#    3   2   0.2   0.4   <- Q1
#    4   3   0.3   0.7   <- Q2
#    5   1   0.1   0.8   <- Q3
#    6   2   0.2   1.0
#
#WHY THIS DATASET AND NOT THE ONE ON THE PREVIOUS SLIDE. The "Ex. Quartiles"
#slide uses 20 die rolls, and for that sample the two ways of getting a quartile
#DISAGREE: the halving method taught in the deck gives Q1 = 2, Q2 = 2.5,
#Q3 = 4.5, while reading the first value whose crf reaches 0.25 / 0.50 / 0.75
#gives 2, 2 and 4. The disagreement is real - several of its crf values land
#exactly on a quartile boundary, and the two conventions break the tie
#differently. Building the slide on that sample would teach an equivalence that
#does not hold.
#
#These ten rolls agree under both methods:
#
#    halving   Q1 = median(1,2,3,3,4) = 3   Q2 = (4+4)/2 = 4   Q3 = median(4,4,5,6,6) = 5
#    from crf  first crf >= 0.25 -> 3       >= 0.50 -> 4       >= 0.75 -> 5
#
#so the slide can make its point cleanly. The slide still carries a one-line
#caveat about exact ties, so nobody generalises the rule too far.
#
#Rendered through headless Chrome, the same way make_teens_frequency_table.R
#does it, so the table matches the serif tables already in the Week 2 decks.

x <- c(1, 2, 3, 3, 4, 4, 4, 5, 6, 6)
stopifnot(length(x) == 10)

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

tb  <- table(x)
val <- as.integer(names(tb))
f   <- as.integer(tb)
rf  <- f / length(x)
crf <- cumsum(rf)

# The quartile is the FIRST value whose crf reaches the target proportion.
first_at <- function(p) val[which(crf >= p - 1e-9)[1]]
Q1 <- first_at(0.25); Q2 <- first_at(0.50); Q3 <- first_at(0.75)

# Cross-check against the halving method the deck teaches, so the figure cannot
# quietly drift out of agreement with the slides around it.
xs <- sort(x); n <- length(xs)
h1 <- median(xs[1:(n %/% 2)]); h2 <- median(xs); h3 <- median(xs[(n %/% 2 + 1):n])
if (!identical(c(Q1, Q2, Q3), as.integer(c(h1, h2, h3)))) {
  stop(sprintf("The two methods disagree on this sample: crf gives %s, halving gives %s",
               paste(c(Q1, Q2, Q3), collapse = ", "), paste(c(h1, h2, h3), collapse = ", ")))
}

MARK <- "#C00000"
CANVAS_W <- 760L
CANVAS_H <- 420L

cell <- function(txt, cls) sprintf('<td class="%s">%s</td>', cls, txt)

rows <- vapply(seq_along(val), function(i) {
  q <- if (val[i] == Q1) "Q1" else if (val[i] == Q2) "Q2" else if (val[i] == Q3) "Q3" else ""
  mark <- if (nzchar(q)) sprintf('<span class="q">&#8592; %s</span>', q) else ""
  cls  <- if (nzchar(q)) "right hit" else "right"
  paste0("<tr>",
         cell(val[i], "right"),
         cell(f[i], "right"),
         cell(sprintf("%.1f", rf[i]), "right"),
         cell(sprintf("%.1f", crf[i]), cls),
         cell(mark, "left"),
         "</tr>")
}, character(1))

html <- sprintf('<!doctype html><html><head><meta charset="utf-8"><style>
  html, body { margin:0; padding:0; width:100%%; height:100%%;
               background:#ffffff; overflow:hidden; }
  body { display:flex; flex-direction:column; justify-content:center;
         font-family:"Latin Modern Roman","CMU Serif","Times New Roman",Times,serif;
         font-size:26px; color:#000000; }
  table { width:100%%; border-collapse:collapse; table-layout:fixed; }
  td { height:44px; padding:0 12px; white-space:nowrap; line-height:44px; }
  th { font-weight:normal; border-bottom:1px solid #000000; padding:0 12px;
       line-height:36px; height:44px; vertical-align:bottom; }
  thead tr { border-top:2.5px solid #000000; }
  tbody tr:last-child { border-bottom:2.5px solid #000000; }
  .left  { text-align:left; }
  .right { text-align:right; }
  .hit   { color:%s; font-weight:bold; }
  .q     { color:%s; font-weight:bold; font-size:22px; padding-left:6px; }
</style></head><body>
<table>
<colgroup><col style="width:15%%"><col style="width:17%%"><col style="width:20%%"><col style="width:23%%"><col style="width:25%%"></colgroup>
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

out <- file.path(assets_dir, "l5_quartile_crf_table.png")
tmp <- tempfile(fileext = ".html")
writeLines(html, tmp, useBytes = TRUE)
status <- system2(find_chrome(), c(
  "--headless=new", "--disable-gpu", "--hide-scrollbars",
  paste0("--user-data-dir=", shQuote(file.path(tempdir(), "l5quart_chrome_profile"))),
  "--force-device-scale-factor=2", "--virtual-time-budget=4000",
  paste0("--window-size=", CANVAS_W, ",", CANVAS_H),
  paste0("--screenshot=", shQuote(normalizePath(out, winslash = "/", mustWork = FALSE))),
  shQuote(paste0("file:///", normalizePath(tmp, winslash = "/")))
), stdout = FALSE, stderr = FALSE)
unlink(tmp)
if (status != 0 || !file.exists(out)) stop("Chrome failed to render the table (status ", status, ")")

message("Wrote ", out, " (", format(file.size(out) %/% 1024L, big.mark = ","), " KB)")
message("\nChecks:")
message("  data        = ", paste(sort(x), collapse = ", "))
message("  crf         = ", paste(sprintf("%.1f", crf), collapse = ", "))
message(sprintf("  from crf    : Q1 = %d, Q2 = %d, Q3 = %d", Q1, Q2, Q3))
message(sprintf("  by halving  : Q1 = %g, Q2 = %g, Q3 = %g   (must match)", h1, h2, h3))
message(sprintf("  aspect      = %.4f  (slide frame must match)", CANVAS_W / CANVAS_H))
