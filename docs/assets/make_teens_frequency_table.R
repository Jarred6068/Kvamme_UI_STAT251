#Builds the teen cell-phone frequency table shown on slide 6 of the Week 2
#Lecture 3 deck (docs/lecture_slides/Week 2/Week2_Lecture3_Slides_1_19_2024.pptx).
#
#The slide used to carry a 757x295 px picture of this table with no total row,
#stretched to 6.95 in wide (about 109 dpi, which is why it looked soft). This
#script regenerates it with the Total row added and at twice the pixel density.
#
#The counts are the published Pew Research margins for "Are you losing focus in
#class by checking your cell phone?" (n = 743 U.S. teens aged 13-17), the same
#numbers behind docs/Data/teens_cell_phone_focus_simulated.csv:
#
#    Never 289 · Rarely 216 · Sometimes 178 · Often 60
#
#Cumulative relative frequency is meaningful here because the response scale is
#ordinal. The Total row deliberately leaves that column blank - a cumulative
#column has no total, and students ask about it every year.
#
#THREE images come out of this, not one. Slide 6 builds the table a column at a
#time alongside the definition of each column, so stage 1 has Response and
#Frequency, stage 2 adds Relative Frequency and stage 3 adds Cumulative. The
#layout is FIXED with explicit column widths and the later columns are rendered
#as empty cells rather than dropped, so all three images have identical geometry
#and can be stacked on the slide - each reveal simply covers the one before it.
#Stage 3 sits on top, which is also what a PDF export of the deck shows.
#
#IMPORTANT - the canvas size below fixes the aspect ratio of the output, and
#slide24.xml stretches the picture into a frame with that same aspect. Change
#CANVAS and you must change the frame in the slide to match, or the table comes
#out squashed. Rendering goes through headless Chrome for the same reason as
#make_lecture2_slide3_tables.R: Chrome is already a build dependency.

CANVAS_W <- 820L
CANVAS_H <- 340L
FONT_PX  <- 26L
ROW_PX   <- 42L
HEAD_PX  <- 34L     #per header line; the header runs to three lines

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

COUNTS <- c(Never = 289L, Rarely = 216L, Sometimes = 178L, Often = 60L)
n <- sum(COUNTS)
rf  <- COUNTS / n
crf <- cumsum(rf)

#stage 1 = Response + Frequency, 2 adds Relative, 3 adds Cumulative.
cell <- function(txt, cls) sprintf('<td class="%s">%s</td>', cls, txt)

build_html <- function(stage) {
  rows <- vapply(seq_along(COUNTS), function(i) {
    cells <- c(cell(names(COUNTS)[i], "left"),
               cell(sprintf("%d", COUNTS[[i]]), "right"),
               cell(if (stage >= 2) sprintf("%.2f", rf[[i]]) else "", "right"),
               cell(if (stage >= 3) sprintf("%.2f", crf[[i]]) else "", "right"))
    paste0("<tr>", paste0(cells, collapse = ""), "</tr>")
  }, character(1))

  total <- paste0('<tr class="total">',
                  cell("Total", "left"),
                  cell(sprintf("%d", n), "right"),
                  cell(if (stage >= 2) sprintf("%.2f", sum(rf)) else "", "right"),
                  cell("", "right"), "</tr>")

  heads <- c('<th class="left">Response</th>',
             '<th class="right">Frequency</th>',
             if (stage >= 2) '<th class="right">Relative<br>Frequency</th>'
             else '<th class="right"></th>',
             if (stage >= 3) '<th class="right">Cumulative<br>Relative<br>Frequency</th>'
             else '<th class="right"></th>')

  sprintf('<!doctype html><html><head><meta charset="utf-8"><style>
  html, body { margin:0; padding:0; width:100%%; height:100%%;
               background:#ffffff; overflow:hidden; }
  body { display:flex; flex-direction:column; justify-content:center;
         font-family:"Latin Modern Roman","CMU Serif","Latin Modern Roman 10",
                     "Times New Roman",Times,serif;
         font-size:%dpx; color:#000000; }
  table { width:100%%; border-collapse:collapse; table-layout:fixed; }
  td { height:%dpx; padding:0 10px; white-space:nowrap; line-height:%dpx; }
  th { font-weight:normal; border-bottom:1px solid #000000; padding:0 10px;
       line-height:%dpx; height:%dpx; vertical-align:bottom; }
  thead tr { border-top:2.5px solid #000000; }
  tbody tr:last-child { border-bottom:2.5px solid #000000; }
  tr.total { border-top:1px solid #000000; }
  .left  { text-align:left; }
  .right { text-align:right; }
</style></head><body>
<table>
<colgroup><col style="width:26%%"><col style="width:22%%"><col style="width:26%%"><col style="width:26%%"></colgroup>
<thead><tr>%s</tr></thead><tbody>%s%s</tbody></table>
</body></html>', FONT_PX, ROW_PX, ROW_PX, HEAD_PX, 3L * HEAD_PX,
    paste0(heads, collapse = ""), paste0(rows, collapse = ""), total)
}

#Chrome discovery and screenshotting: same approach as make_lecture2_slide3_tables.R,
#including the throwaway profile so headless Chrome does not fight the user's own
#running Chrome over the profile lock.
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

shoot <- function(html, out_png) {
  tmp <- tempfile(fileext = ".html")
  writeLines(html, tmp, useBytes = TRUE)
  on.exit(unlink(tmp), add = TRUE)
  status <- system2(find_chrome(), c(
    "--headless=new", "--disable-gpu", "--hide-scrollbars",
    paste0("--user-data-dir=", shQuote(file.path(tempdir(), "freqtable_chrome_profile"))),
    "--force-device-scale-factor=2", "--virtual-time-budget=4000",
    paste0("--window-size=", CANVAS_W, ",", CANVAS_H),
    paste0("--screenshot=", shQuote(normalizePath(out_png, winslash = "/", mustWork = FALSE))),
    shQuote(paste0("file:///", normalizePath(tmp, winslash = "/")))
  ), stdout = FALSE, stderr = FALSE)
  if (status != 0 || !file.exists(out_png))
    stop("Chrome failed to render ", basename(out_png), " (status ", status, ")")
  message("  ", basename(out_png), " (", format(file.size(out_png) %/% 1024L, big.mark = ","), " KB)")
}

for (stage in 1:3)
  shoot(build_html(stage),
        file.path(assets_dir, sprintf("teens_frequency_table_%d.png", stage)))

message("Slide frame aspect must be ", sprintf("%.4f", CANVAS_W / CANVAS_H),
        " - for a 6358151 EMU width that is ",
        round(6358151 * CANVAS_H / CANVAS_W), " EMU tall.")
