#Set our working directory to the folder this script lives in (docs/).
#Resolving it from the script's own location keeps this working no matter
#where the repo sits on disk or what the current working directory is.
this_file <- tryCatch(
  # When run via Rscript / source(): use the --file= arg or the sourced path.
  normalizePath(sub("^--file=", "",
    grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1]),
    mustWork = TRUE),
  error = function(e) NULL
)
if (is.null(this_file) || is.na(this_file)) {
  # Fallback for RStudio "Source" button, which sets this variable.
  this_file <- normalizePath(sys.frames()[[1]]$ofile, mustWork = TRUE)
}
#Remember where we started so we can restore it afterward — otherwise a
#failed/partial render leaves the session stuck in docs/ and re-running
#source("docs/build_site.R") can't find the (now docs/docs/) script.
old_wd <- getwd()
setwd(dirname(this_file))

#If pandoc isn't already on the path (e.g. running from a plain terminal
#rather than RStudio), point R at RStudio's bundled copy.
if (!rmarkdown::pandoc_available()) {
  Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools")
}

#render your sweet site, restoring the original working directory whether the
#render succeeds or errors out.
tryCatch({
  rmarkdown::render_site()

  #render_site() only builds the .Rmd files in docs/ itself - it does not
  #recurse. The pages under homeworks/, misc/, Review_problems/ and
  #study_guides/ have to be rendered individually, picking up the brand config
  #from each directory's _output.yml.
  #
  #Rendered one at a time inside tryCatch rather than with sapply: the stock
  #site generator aborts the whole build on the first failure, and a single
  #missing package (these pages need kableExtra, latex2exp, gridExtra, ggpubr,
  #ggthemes, plyr, ggvenn and surveydata between them) should not cost you the
  #other 28 pages. Failures are collected and reported at the end instead.
  subdirs <- c("homeworks", "misc", "Review_problems", "study_guides")

  failures <- list()
  rendered <- 0L
  for (subdir in subdirs) {
    #Render from INSIDE each directory rather than passing output_file /
    #output_dir from docs/. Those arguments leave the working directory at
    #docs/, so a page reading '../Data/whatever.csv' resolves it to the repo
    #root instead of docs/Data and dies with "cannot open the connection".
    #Non-Parametric-Tests-For-Two-Samples is the page that trips on this.
    #Rendering from within the directory keeps every relative path in the .Rmd
    #meaning what it says.
    here <- setwd(subdir)

    #Underscore-prefixed .Rmd files are CHILD documents, included by another
    #page via a `child =` chunk - they are not pages in their own right and do
    #not stand up on their own (they read variables the parent defines). Skip
    #them, the same way render_site() already ignores files starting with `_`.
    #Review_problems/_exam1_review_body.Rmd is the one that matters: it holds
    #the exam 1 review problems once, and the problems and solutions pages both
    #include it with a SOLUTIONS flag set, so the answers cannot drift away from
    #the questions they belong to.
    for (page in list.files(pattern = "^[^_].*\\.Rmd$")) {
      #This one source has spaces in its filename but the navbar links the
      #hyphenated name, so it was renamed by hand at some point. Rendering it
      #normally would quietly produce a NEW spaced-name file and leave the
      #linked page stale and unstyled.
      out <- if (page == "Non Parametric Tests For Two Samples.Rmd") {
        "Non-Parametric-Tests-For-Two-Samples.html"
      } else NULL

      message("Rendering: ", file.path(subdir, page))
      rendered <- rendered + 1L
      tryCatch(
        rmarkdown::render(page, output_format = "html_document",
                          output_file = out, quiet = TRUE),
        error = function(e) failures[[file.path(subdir, page)]] <<- conditionMessage(e)
      )
    }

    setwd(here)
  }

  if (length(failures)) {
    message("\n", strrep("-", 70))
    message(length(failures), " page(s) FAILED to render and kept their previous HTML:")
    for (f in names(failures)) message("  ", f, "\n    ", failures[[f]])
    message(strrep("-", 70))
  } else {
    message("\nAll ", rendered, " subdirectory pages rendered.")
  }

  #-------------------------------------------------------------------------
  #PDF copies of the Syllabus-menu pages and the weekly lecture notes.
  #
  #These are printed from the ALREADY-RENDERED HTML through headless Chrome
  #rather than rendered with pdf_document, and that is deliberate.
  #Course_Schedule.Rmd is built almost entirely from raw HTML tables (two
  #<table>s, ~120 <td>) and university_resources.Rmd uses raw <img>/<br>.
  #pandoc DROPS raw HTML when the target is LaTeX, so a pdf_document render
  #of those pages produces a PDF containing the prose and none of the
  #tables - silently, with no warning and a zero exit status.
  #
  #The lecture notes are the same story: they build their tables with
  #kableExtra's kable_styling(), which emits HTML, and twelve of the sixteen
  #set `always_allow_html: true` - the flag whose whole purpose is to silence
  #pandoc's warning that it is about to drop raw HTML on the way to LaTeX.
  #That is why the `pdf_document: default` line in their front matter is
  #commented out; do not just uncomment it.
  #
  #Printing the HTML instead keeps the tables and picks up the `@media print`
  #block in assets/brand/site.css (section 21), which already hides the
  #navbar / theme toggle / skip link and forces the light palette.
  #
  #Chrome is optional: if it is not found the PDFs keep their previous
  #contents and the build still succeeds. Set CHROME_BIN to override.
  pdf_pages <- c("syllabus", "Course_Schedule", "sac_schedule", "university_resources",
                 paste0("lecturemenu", 1:16))

  #Chrome-finding and the print call itself live in _chrome.R, shared with
  #build_review.R so the logic is written once. That file also carries the
  #explanation of why these are PRINTED from HTML rather than rendered with
  #pdf_document - short version: pandoc drops raw HTML on the way to LaTeX, so
  #a pdf_document render would throw away every kableExtra table.
  source("_chrome.R", local = TRUE)
  chrome <- find_chrome()

  if (!nzchar(chrome)) {
    message("\nChrome not found - skipping PDF generation for: ",
            paste(pdf_pages, collapse = ", "),
            "\n  (set CHROME_BIN to the chrome.exe path to enable it)")
  } else {
    message("\nPrinting PDFs with: ", chrome)
    profile <- file.path(tempdir(), "build_site_chrome_profile")

    for (page in pdf_pages) {
      print_pdf(paste0(page, ".html"), paste0(page, ".pdf"),
                chrome, label = paste0(page, ".pdf"), profile = profile)
    }

    #The Exam 1 handouts. These are student-facing and get uploaded to Canvas
    #as PDFs, so they need both formats: the .html is what the website links,
    #the .pdf is what students download. Printed from inside each page's own
    #directory so the relative asset paths in the HTML still resolve.
    message("\nPrinting Exam 1 handout PDFs:")
    for (entry in REVIEW_PAGES) {
      here <- setwd(entry[[1]])
      print_pdf(paste0(entry[[2]], ".html"), paste0(entry[[2]], ".pdf"),
                chrome, label = file.path(entry[[1]], paste0(entry[[2]], ".pdf")),
                profile = profile)
      setwd(here)
    }
  }
}, finally = setwd(old_wd))

