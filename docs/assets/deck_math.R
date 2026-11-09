#Swaps the @@k@@ formula tokens in a lecture deck's slide XML for native PowerPoint equations.
#
#  Rscript deck_math.R <unzipped deck dir> <math.json>
#
#math.json is the list [{id, tex}] the build script wrote while it replaced each $...$ in its spec with
#@@id@@. For every slide shape that holds a token, the shape is wrapped in mc:AlternateContent:
#  Choice (PowerPoint 2010+): the token runs are split, and each token becomes an inline <a14:m> equation
#                             that inherits the run's size, bold and colour (font: Cambria Math);
#  Fallback:                  the same shape with a plain-text version of each formula.
#Both copies keep the shape id, so the click animations still point at the shape. Only the slide XML
#files that change are rewritten. See latex2omml.R for the supported LaTeX.

args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2)
deck_dir <- args[1]; math <- jsonlite::fromJSON(args[2])
this_file <- normalizePath(sub('^--file=', '', grep('^--file=', commandArgs(FALSE), value = TRUE)[1]), winslash = '/')
source(file.path(dirname(this_file), 'latex2omml.R'), encoding = 'UTF-8')
tex_of <- setNames(math$tex, math$id)

NS_MC <- 'http://schemas.openxmlformats.org/markup-compatibility/2006'
NS_A14 <- 'http://schemas.microsoft.com/office/drawing/2010/main'

#a run's <a:rPr>, turned into the one used inside the equation: same size/bold/colour, Cambria Math
math_rpr <- function(rpr) {
  if (!nchar(rpr)) rpr <- '<a:rPr lang="en-US"/>'
  rpr <- sub('^<a:rPr', '<a:rPr', rpr)
  rpr <- gsub('<a:(latin|ea|cs|sym)[^>]*/>', '', rpr)
  rpr <- gsub(' (err|dirty|smtClean|baseline)="[^"]*"', '', rpr)
  if (grepl('/>$', rpr) && !grepl('</a:rPr>', rpr)) rpr <- sub('/>$', '></a:rPr>', rpr)
  sub('</a:rPr>$', '<a:latin typeface="Cambria Math" panose="02040503050406030204" pitchFamily="18" charset="0"/></a:rPr>', rpr)
}

RUN_RE <- '<a:r>(<a:rPr[^>]*/>|<a:rPr[^>]*>.*?</a:rPr>)?<a:t>([^<]*)</a:t></a:r>'

#rewrite every run in an <p:sp> that holds a token; mode = 'math' or 'plain'
rewrite_runs <- function(sp, mode) {
  m <- gregexpr(RUN_RE, sp, perl = TRUE)[[1]]
  if (m[1] == -1) return(sp)
  starts <- as.integer(m); lens <- attr(m, 'match.length')
  out <- character(); last <- 1
  for (i in seq_along(starts)) {
    runx <- substr(sp, starts[i], starts[i] + lens[i] - 1)
    out <- c(out, substr(sp, last, starts[i] - 1)); last <- starts[i] + lens[i]
    rr <- regmatches(runx, regexec(RUN_RE, runx, perl = TRUE))[[1]]
    rpr <- rr[2]; txt <- rr[3]
    if (!grepl('@@[0-9]+@@', txt)) { out <- c(out, runx); next }
    pieces <- regmatches(txt, gregexpr('@@[0-9]+@@|[^@]+|@', txt))[[1]]
    for (pc in pieces) {
      if (grepl('^@@[0-9]+@@$', pc)) {
        tex <- tex_of[[gsub('@', '', pc)]]
        if (mode == 'math') out <- c(out, latex2omml(tex, math_rpr(rpr), display = (pc == txt)))   # alone in its run: display style
        else out <- c(out, sprintf('<a:r>%s<a:t>%s</a:t></a:r>', rpr, xml_esc(latex2plain(tex))))
      } else out <- c(out, sprintf('<a:r>%s<a:t>%s</a:t></a:r>', rpr, pc))
    }
  }
  paste(c(out, substr(sp, last, nchar(sp))), collapse = '')
}

slides <- list.files(file.path(deck_dir, 'ppt', 'slides'), pattern = '^slide[0-9]+\\.xml$', full.names = TRUE)
n_eq <- 0
for (f in slides) {
  x <- paste(readLines(f, encoding = 'UTF-8', warn = FALSE), collapse = '\n')
  if (!grepl('@@[0-9]+@@', x)) next
  sp_re <- '<p:sp>.*?</p:sp>'
  m <- gregexpr(sp_re, x, perl = TRUE)[[1]]
  starts <- as.integer(m); lens <- attr(m, 'match.length')
  out <- character(); last <- 1
  for (i in seq_along(starts)) {
    sp <- substr(x, starts[i], starts[i] + lens[i] - 1)
    out <- c(out, substr(x, last, starts[i] - 1)); last <- starts[i] + lens[i]
    if (!grepl('@@[0-9]+@@', sp)) { out <- c(out, sp); next }
    n_eq <- n_eq + length(gregexpr('@@[0-9]+@@', sp)[[1]])
    out <- c(out, sprintf('<mc:AlternateContent xmlns:mc="%s"><mc:Choice xmlns:a14="%s" Requires="a14">%s</mc:Choice><mc:Fallback>%s</mc:Fallback></mc:AlternateContent>',
                          NS_MC, NS_A14, rewrite_runs(sp, 'math'), rewrite_runs(sp, 'plain')))
  }
  x <- paste(c(out, substr(x, last, nchar(x))), collapse = '')
  stopifnot(!grepl('@@[0-9]+@@', gsub('<mc:Fallback>.*?</mc:Fallback>', '', x, perl = TRUE)))
  con <- file(f, open = 'wb'); writeBin(charToRaw(enc2utf8(x)), con); close(con)
  cat(basename(f), '\n')
}
cat('equations:', n_eq, '\n')
