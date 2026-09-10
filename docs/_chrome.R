# Shared headless-Chrome PDF printer.
#
# Every PDF on this site is printed from ALREADY-RENDERED HTML rather than
# rendered with pdf_document, and that is deliberate. The pages build their
# tables with kableExtra's kable_styling(), which emits raw HTML, and several use
# raw <table>/<span>/<br> markup directly. pandoc DROPS raw HTML when the target
# is LaTeX - silently, with a zero exit status - so a pdf_document render
# produces a PDF containing the prose and none of the tables. Printing the HTML
# keeps all of it and picks up the `@media print` rules in assets/brand/site.css.
#
# Sourced by build_site.R and build_review.R. The leading underscore marks it as
# a helper rather than a page or an entry point; nothing renders it.

# Locate a Chrome-family browser, or return "" if there is none. CHROME_BIN wins
# when it is set and points at a real file.
find_chrome <- function() {
  chrome <- Sys.getenv("CHROME_BIN")
  if (nzchar(chrome) && file.exists(chrome)) return(chrome)

  candidates <- c(
    "C:/Program Files/Google/Chrome/Application/chrome.exe",
    "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe",
    "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe",
    Sys.which(c("google-chrome", "chromium", "chromium-browser"))
  )
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(candidates)) candidates[[1]] else ""
}

# Print one HTML file to PDF. `html` and `pdf` are paths relative to the current
# working directory. Returns TRUE on success; on failure it reports and leaves
# any previous PDF untouched, so a broken run never destroys a good file.
#
# `profile` is a throwaway user-data directory: without one, headless Chrome can
# refuse to start or fight over the lock when the user already has Chrome open,
# which is the normal case on this machine.
print_pdf <- function(html, pdf, chrome, label = html,
                      profile = file.path(tempdir(), "build_chrome_profile"),
                      virtual_time = 15000) {
  if (!file.exists(html)) {
    message("  SKIPPED: ", label, " (no ", html, ")")
    return(invisible(FALSE))
  }
  html_abs <- normalizePath(html, winslash = "/", mustWork = TRUE)
  pdf_abs  <- normalizePath(pdf,  winslash = "/", mustWork = FALSE)

  status <- system2(chrome, c(
    "--headless=new",
    "--disable-gpu",
    paste0("--user-data-dir=", shQuote(profile)),
    # Drop Chrome's own URL/date/page-number furniture; the print CSS and the
    # page itself supply everything that belongs in the output.
    "--no-pdf-header-footer",
    # The pages pull MathJax and Google Fonts. Give the network a moment to
    # settle so formulas are typeset rather than left as raw LaTeX.
    paste0("--virtual-time-budget=", virtual_time),
    paste0("--print-to-pdf=", shQuote(pdf_abs)),
    shQuote(paste0("file:///", html_abs))
  ), stdout = FALSE, stderr = FALSE)

  if (status == 0 && file.exists(pdf_abs)) {
    message("  ", label, " (",
            format(file.size(pdf_abs) %/% 1024L, big.mark = ","), " KB)")
    invisible(TRUE)
  } else {
    message("  FAILED: ", label, " (chrome exit status ", status,
            ") - kept the previous file")
    invisible(FALSE)
  }
}

# The student-facing Exam 1 handouts, as directory/page pairs. Both build_site.R
# and build_review.R print these, so the list lives in one place: a page added
# here is picked up by the full site build and the fast path alike.
REVIEW_PAGES <- list(
  c("study_guides",    "exam1_study_guide"),
  c("Review_problems", "exam1_review_problems"),
  c("Review_problems", "exam1_review_problems_solns")
)
