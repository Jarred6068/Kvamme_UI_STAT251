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

    for (page in list.files(pattern = "\\.Rmd$")) {
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

  chrome <- Sys.getenv("CHROME_BIN")
  if (!nzchar(chrome) || !file.exists(chrome)) {
    candidates <- c(
      "C:/Program Files/Google/Chrome/Application/chrome.exe",
      "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe",
      "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe",
      Sys.which(c("google-chrome", "chromium", "chromium-browser"))
    )
    candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
    chrome <- if (length(candidates)) candidates[[1]] else ""
  }

  if (!nzchar(chrome)) {
    message("\nChrome not found - skipping PDF generation for: ",
            paste(pdf_pages, collapse = ", "),
            "\n  (set CHROME_BIN to the chrome.exe path to enable it)")
  } else {
    message("\nPrinting PDFs with: ", chrome)

    #Its own throwaway profile directory. Without this, headless Chrome can
    #refuse to start or fight over the lock when the user already has Chrome
    #open, which is the normal case on this machine.
    profile <- file.path(tempdir(), "build_site_chrome_profile")

    for (page in pdf_pages) {
      html <- normalizePath(paste0(page, ".html"), winslash = "/",
                            mustWork = TRUE)
      pdf  <- normalizePath(paste0(page, ".pdf"), winslash = "/",
                            mustWork = FALSE)

      status <- system2(chrome, c(
        "--headless=new",
        "--disable-gpu",
        paste0("--user-data-dir=", shQuote(profile)),
        #Drop Chrome's own URL/date/page-number furniture; the print CSS and
        #the page itself supply everything that belongs in the output.
        "--no-pdf-header-footer",
        #The pages pull MathJax and Google Fonts. Give the network a moment
        #to settle so the PDF is not typeset in a fallback face.
        "--virtual-time-budget=15000",
        paste0("--print-to-pdf=", shQuote(pdf)),
        shQuote(paste0("file:///", html))
      ), stdout = FALSE, stderr = FALSE)

      if (status == 0 && file.exists(pdf)) {
        message("  ", page, ".pdf (",
                format(file.size(pdf) %/% 1024L, big.mark = ","), " KB)")
      } else {
        message("  FAILED: ", page, ".pdf (chrome exit status ", status,
                ") - kept the previous file")
      }
    }
  }
}, finally = setwd(old_wd))

