#LaTeX -> OMML (Office Math) for the hand-built lecture decks.
#
#The decks are built through PowerPoint COM, which cannot create equations. So the build script writes a
#placeholder token (@@k@@) wherever a formula goes, and deck_math.R swaps each token for a native PowerPoint
#equation produced here. The result is real typeset math (Cambria Math, stacked fractions, radicals that
#grow), editable in PowerPoint's equation editor, and the click-reveal animations are untouched.
#
#Supported LaTeX (the subset the course slides use):
#  groups {..}; scripts x_a, x^b, x_a^b; \frac{a}{b}; \sqrt{a}; \hat{a}; \bar{a}; \text{..}, \mathrm{..};
#  \left( .. \right) (also [ ] |); Greek letters; \pm \times \neq \leq \geq \approx \sim \cdot \to \infty
#  \mid \ldots \min \max; spacing \, \; \quad.
#Letters become math-italic Unicode, as PowerPoint stores them; \text and digits stay upright.

xml_esc <- function(s) { s <- gsub('&', '&amp;', s, fixed = TRUE); s <- gsub('<', '&lt;', s, fixed = TRUE); gsub('>', '&gt;', s, fixed = TRUE) }

it_char <- function(ch) {
  cp <- utf8ToInt(ch)
  if (ch == 'h') return(intToUtf8(0x210E))
  if (cp >= 97 && cp <= 122) return(intToUtf8(0x1D44E + cp - 97))
  if (cp >= 65 && cp <= 90)  return(intToUtf8(0x1D434 + cp - 65))
  greek <- c('α' = 0x1D6FC, 'β' = 0x1D6FD, 'γ' = 0x1D6FE, 'δ' = 0x1D6FF, 'θ' = 0x1D703, 'μ' = 0x1D707,
             'π' = 0x1D70B, 'ρ' = 0x1D70C, 'σ' = 0x1D70E, 'χ' = 0x1D712, 'λ' = 0x1D706)
  if (ch %in% names(greek)) return(intToUtf8(greek[[ch]]))
  ch
}

SYM <- c(alpha = 'α', beta = 'β', gamma = 'γ', delta = 'δ', theta = 'θ', mu = 'μ', pi = 'π', rho = 'ρ',
         sigma = 'σ', chi = 'χ', lambda = 'λ', Sigma = 'Σ', pm = '±', times = '×', neq = '≠', ne = '≠',
         leq = '≤', le = '≤', geq = '≥', ge = '≥', approx = '≈', sim = '∼', cdot = '⋅', to = '→',
         infty = '∞', mid = '|', ldots = '…', lt = '<', gt = '>', minus = '−')
UPRIGHT_FUN <- c('min', 'max', 'log', 'ln')

latex2omml <- function(tex, rpr, display = FALSE) {
  # display = TRUE when the formula is alone on its line: it is set left-justified in display style, with
  # stacked full-size fractions. Inline, a fraction is written a/b (as in LaTeX text), because a stacked
  # inline fraction shrinks below the decks' 16pt minimum.
  # rpr: the <a:rPr .../> (or <a:rPr>..</a:rPr>) every run and control character gets
  pos <- 1; n <- nchar(tex); chars <- strsplit(tex, '')[[1]]
  ctrl <- sprintf('<m:ctrlPr>%s</m:ctrlPr>', rpr)
  run <- function(txt, upright = FALSE) {
    if (!nchar(txt)) return('')
    sty <- if (upright) '<m:rPr><m:sty m:val="p"/></m:rPr>' else ''
    if (!upright) txt <- paste(vapply(strsplit(txt, '')[[1]], it_char, ''), collapse = '')
    sprintf('<m:r>%s%s<m:t>%s</m:t></m:r>', sty, rpr, xml_esc(txt))
  }
  peek <- function() if (pos <= n) chars[pos] else ''
  read_cmd <- function() {           # after a backslash
    st <- pos
    if (pos <= n && grepl('[A-Za-z]', chars[pos])) { while (pos <= n && grepl('[A-Za-z]', chars[pos])) pos <<- pos + 1 }
    else pos <<- pos + 1
    paste(chars[st:(pos - 1)], collapse = '')
  }
  skip_ws <- function() while (pos <= n && chars[pos] == ' ') pos <<- pos + 1
  read_raw_group <- function() {     # {...} as raw text (for \text)
    skip_ws(); stopifnot(chars[pos] == '{'); depth <- 0; st <- pos + 1
    repeat { if (chars[pos] == '{') depth <- depth + 1; if (chars[pos] == '}') { depth <- depth - 1; if (depth == 0) break }; pos <<- pos + 1 }
    out <- if (pos > st) paste(chars[st:(pos - 1)], collapse = '') else ''; pos <<- pos + 1; out
  }
  # one argument: a {group} or a single token
  parse_arg <- function() {
    skip_ws()
    if (peek() == '{') { pos <<- pos + 1; s <- parse_seq('}'); pos <<- pos + 1; return(s) }
    parse_atom()
  }
  parse_atom <- function() {
    ch <- peek()
    if (ch == '{') { pos <<- pos + 1; s <- parse_seq('}'); pos <<- pos + 1; return(s) }
    if (ch == '\\') {
      pos <<- pos + 1; cmd <- read_cmd()
      if (cmd %in% c('frac', 'tfrac')) {   # \tfrac is always a/b, e.g. a fraction nested inside a stacked one
        st <- pos; a <- parse_arg(); ra <- substr(tex, st, pos - 1)
        st <- pos; b <- parse_arg(); rb <- substr(tex, st, pos - 1)
        if (display && cmd == 'frac') return(sprintf('<m:f><m:fPr>%s</m:fPr><m:num>%s</m:num><m:den>%s</m:den></m:f>', ctrl, a, b))
        # inline: a/b, with parentheses round a compound numerator or denominator
        simple <- function(r) !grepl('[-+ ]|\\\\(times|cdot|pm)', sub('^[{]?-', '', trimws(r)))   # a leading minus is fine
        wrap <- function(e, r) if (simple(r)) e else sprintf('<m:d><m:dPr>%s</m:dPr><m:e>%s</m:e></m:d>', ctrl, e)
        return(paste0(wrap(a, ra), run('/', TRUE), wrap(b, rb))) }
      if (cmd == 'sqrt') { a <- parse_arg()
        return(sprintf('<m:rad><m:radPr><m:degHide m:val="1"/>%s</m:radPr><m:deg/><m:e>%s</m:e></m:rad>', ctrl, a)) }
      if (cmd == 'bar') { a <- parse_arg()
        return(sprintf('<m:bar><m:barPr><m:pos m:val="top"/>%s</m:barPr><m:e>%s</m:e></m:bar>', ctrl, a)) }
      if (cmd == 'hat') { a <- parse_arg(); acc <- '\u0302'
        return(sprintf('<m:acc><m:accPr><m:chr m:val="%s"/>%s</m:accPr><m:e>%s</m:e></m:acc>', acc, ctrl, a)) }
      if (cmd %in% c('text', 'mathrm')) return(run(read_raw_group(), upright = TRUE))
      if (cmd == 'left') {
        skip_ws(); open <- read_delim(); body <- parse_seq('\\right'); pos <<- pos + nchar('\\right'); skip_ws(); close <- read_delim()
        return(sprintf('<m:d><m:dPr><m:begChr m:val="%s"/><m:endChr m:val="%s"/>%s</m:dPr><m:e>%s</m:e></m:d>',
                       xml_esc(open), xml_esc(close), ctrl, body)) }
      if (cmd %in% UPRIGHT_FUN) return(run(cmd, upright = TRUE))
      if (cmd == ',') return(run('\u2009', TRUE))
      if (cmd == ';') return(run('\u2005', TRUE))
      if (cmd == 'quad') return(run('\u2003', TRUE))
      if (cmd %in% c('{', '}', '%', '$', '#', '_')) return(run(cmd, TRUE))
      if (cmd %in% names(SYM)) { s <- SYM[[cmd]]; return(run(s, upright = !grepl('[α-ωΑ-Ω]', s) || s == 'Σ')) }
      stop('latex2omml: unsupported command \\', cmd, ' in: ', tex)
    }
    if (grepl('[0-9.]', ch)) {       # a whole number such as 0.3819 is one run, so it is not spaced apart
      st <- pos; while (pos <= n && grepl('[0-9.]', chars[pos])) pos <<- pos + 1
      return(run(paste(chars[st:(pos - 1)], collapse = ''), TRUE))
    }
    pos <<- pos + 1
    if (grepl('[A-Za-z]', ch)) return(run(ch))
    if (ch == '-') return(run('\u2212', TRUE))
    if (ch == ' ') return('')
    run(ch, TRUE)
  }
  read_delim <- function() {
    if (peek() == '\\') { pos <<- pos + 1; d <- read_cmd(); return(switch(d, '{' = '{', '}' = '}', '|' = '‖', '.' = '', d)) }
    d <- chars[pos]; pos <<- pos + 1; if (d == '.') '' else d
  }
  parse_seq <- function(stop_at) {
    out <- character()
    while (pos <= n) {
      if (stop_at == '}' && peek() == '}') break
      if (stop_at == '\\right' && substr(tex, pos, pos + 5) == '\\right') break
      base <- parse_atom()
      sub <- NULL; sup <- NULL
      repeat {
        skip_ws0 <- pos
        if (peek() == '_') { pos <<- pos + 1; sub <- parse_arg() }
        else if (peek() == '^') { pos <<- pos + 1; sup <- parse_arg() }
        else break
      }
      if (!is.null(sub) && !is.null(sup))
        base <- sprintf('<m:sSubSup><m:sSubSupPr>%s</m:sSubSupPr><m:e>%s</m:e><m:sub>%s</m:sub><m:sup>%s</m:sup></m:sSubSup>', ctrl, base, sub, sup)
      else if (!is.null(sub)) base <- sprintf('<m:sSub><m:sSubPr>%s</m:sSubPr><m:e>%s</m:e><m:sub>%s</m:sub></m:sSub>', ctrl, base, sub)
      else if (!is.null(sup)) base <- sprintf('<m:sSup><m:sSupPr>%s</m:sSupPr><m:e>%s</m:e><m:sup>%s</m:sup></m:sSup>', ctrl, base, sup)
      out <- c(out, base)
    }
    paste(out, collapse = '')
  }
  body <- parse_seq('')
  if (display) return(sprintf(paste0('<a14:m><m:oMathPara xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math">',
                                     '<m:oMathParaPr><m:jc m:val="left"/></m:oMathParaPr><m:oMath>%s</m:oMath></m:oMathPara></a14:m>'), body))
  sprintf('<a14:m><m:oMath xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math">%s</m:oMath></a14:m>', body)
}

#a plain-text version for the mc:Fallback copy (only very old PowerPoint shows it)
latex2plain <- function(tex) {
  s <- tex
  s <- gsub('\\\\(text|mathrm)\\{([^}]*)\\}', '\\2', s)
  s <- gsub('\\\\frac\\{([^{}]*)\\}\\{([^{}]*)\\}', '(\\1)/(\\2)', s)
  s <- gsub('\\\\sqrt', '√', s); s <- gsub('\\\\hat\\{([^}]*)\\}', '\\1\u0302', s); s <- gsub('\\\\bar\\{([^}]*)\\}', '\\1\u0304', s)
  for (k in names(SYM)) s <- gsub(paste0('\\\\', k, '(?![A-Za-z])'), SYM[[k]], s, perl = TRUE)
  s <- gsub('\\\\(left|right)', '', s); s <- gsub('\\\\[,;]|\\\\quad', ' ', s)
  s <- gsub('[{}]', '', s); s <- gsub('[_^]', '', s); gsub('\\\\', '', s)
}
