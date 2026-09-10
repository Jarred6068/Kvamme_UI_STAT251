# Build the student-facing Exam 1 handouts: study guide, review problems, and
# review solutions. Each is produced in BOTH formats -
#
#   .html  - what the website links from the navbar
#   .pdf   - what gets uploaded to Canvas for students to download
#
# This is the fast path. docs/build_site.R produces exactly the same files as
# part of a full site build, but that takes ten minutes because it re-renders
# every page and reprints twenty PDFs. Use this one while you are still editing;
# run build_site.R before you publish.
#
# Usage, from anywhere:
#   source("docs/build_review.R")
# or from a terminal:
#   Rscript docs/build_review.R
#
# The PDFs are PRINTED from the rendered HTML through headless Chrome rather
# than rendered with pdf_document. See _chrome.R for why - short version: these
# pages build their tables with kableExtra and mark their answers with raw
# <span> tags, and pandoc silently drops all raw HTML when the target is LaTeX.

# Resolve this script's own directory so the working directory does not matter.
this_file <- tryCatch(
  normalizePath(sub("^--file=", "",
    grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1]),
    mustWork = TRUE),
  error = function(e) NULL
)
if (is.null(this_file) || is.na(this_file)) {
  # Fallback for the RStudio "Source" button.
  this_file <- normalizePath(sys.frames()[[1]]$ofile, mustWork = TRUE)
}

old_wd <- getwd()
setwd(dirname(this_file))

if (!rmarkdown::pandoc_available()) {
  Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools")
}

tryCatch({
  source("_chrome.R", local = TRUE)   # find_chrome(), print_pdf(), REVIEW_PAGES

  # ---- 1. HTML ------------------------------------------------------------
  # Rendered from INSIDE each page's directory rather than by passing
  # output_dir from docs/. Those arguments leave the working directory at
  # docs/, so a page reading '../assets/...' resolves it against the repo root
  # instead. Rendering in place keeps every relative path meaning what it says,
  # and picks up the brand config from each directory's _output.yml.
  #
  # One at a time inside tryCatch so a single failure does not cost the others.
  # The in-page stopifnot() guards are the real test here: if a value stops
  # agreeing with the answer stated beside it, the render fails loudly rather
  # than producing a handout with a wrong answer in it.
  failures <- character(0)
  for (entry in REVIEW_PAGES) {
    page <- file.path(entry[[1]], paste0(entry[[2]], ".Rmd"))
    message("Rendering: ", page)
    here <- setwd(entry[[1]])
    tryCatch(
      rmarkdown::render(paste0(entry[[2]], ".Rmd"),
                        output_format = "html_document", quiet = TRUE),
      error = function(e) {
        failures[[page]] <<- conditionMessage(e)
        message("  FAILED: ", conditionMessage(e))
      }
    )
    setwd(here)
  }

  # ---- 2. PDF -------------------------------------------------------------
  chrome <- find_chrome()
  if (!nzchar(chrome)) {
    message("\nChrome not found - skipping PDF generation.",
            "\n  (set CHROME_BIN to the chrome.exe path to enable it)")
  } else {
    message("\nPrinting PDFs with: ", chrome)
    profile <- file.path(tempdir(), "build_review_chrome_profile")
    for (entry in REVIEW_PAGES) {
      here <- setwd(entry[[1]])
      print_pdf(paste0(entry[[2]], ".html"), paste0(entry[[2]], ".pdf"),
                chrome, label = file.path(entry[[1]], paste0(entry[[2]], ".pdf")),
                profile = profile)
      setwd(here)
    }
  }

  if (length(failures)) {
    message("\n", strrep("-", 70))
    message(length(failures), " page(s) FAILED to render:")
    for (f in names(failures)) message("  ", f, "\n    ", failures[[f]])
    message(strrep("-", 70))
  } else {
    message("\nAll ", length(REVIEW_PAGES), " handouts built (.html and .pdf).")
  }
}, finally = setwd(old_wd))
