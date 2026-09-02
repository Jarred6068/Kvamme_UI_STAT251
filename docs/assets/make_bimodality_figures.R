#Builds the three bimodality example figures for the Week 2 Lecture 4 deck
#(slides 5, 6 and 7), which replace the single overcrowded inherited slide.
#
#  bimodal_oldfaithful.png   Old Faithful waiting times, then split by eruption
#                            duration
#  bimodal_political.png     attitudes on a contested issue, then split by party
#                            SIMULATED - see the note below
#  bimodal_penguins.png      penguin flipper length, then split by species
#
#Every figure has the SAME two-panel shape on purpose:
#
#    left  = the variable on its own, showing two humps
#    right = the same variable split by a grouping variable, each group unimodal
#
#That repetition is the lesson. Two humps is not a curiosity of the histogram, it
#is a signature that the sample is drawn from two (or more) distinct
#sub-populations. The three examples walk from "we can guess the grouping
#variable" (Old Faithful) through "the grouping variable is the whole story"
#(politics) to "we recorded the grouping variable, so we can prove it" (penguins).
#
#DATA PROVENANCE - this matters, because two of these are real and one is not:
#
#  Old Faithful  REAL. base R `faithful`, 272 eruptions, Azzalini & Bowman (1990).
#  Penguins      REAL. assets/penguins_palmer.csv, 344 birds, Gorman, Williams &
#                Fraser (2014), via the palmerpenguins package. Committed to the
#                repo rather than loaded from a package so the figure rebuilds
#                without a network or a new dependency.
#  Political     SIMULATED, and labelled as such on the figure itself. It is a
#                schematic of what polarization looks like, NOT survey data. The
#                Pew typology report cited on the slide is offered to students as
#                further reading, not as the source of these numbers - do not
#                relabel this panel as Pew data.

set.seed(2026)

suppressPackageStartupMessages({
  library(ggplot2)
  library(ggpubr)
})

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
ORNG <- "#ED7D31"
GRN  <- "#1F7A4D"
DEM  <- "#2E5FA3"
REP  <- "#C00000"

base_theme <- theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        plot.title = element_text(size = 15, face = "bold"),
        plot.subtitle = element_text(size = 12.5, colour = "grey25"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12.5))

#Left panel: the variable on its own. One colour, because at this stage we do not
#yet know that there is anything to split on.
plain <- function(df, xvar, bins, title, subtitle, xlab) {
  ggplot(df, aes(x = .data[[xvar]])) +
    geom_histogram(bins = bins, fill = BLUE, colour = "white", linewidth = 0.3) +
    labs(title = title, subtitle = subtitle, x = xlab, y = "Frequency") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
    base_theme + theme(legend.position = "none")
}

#Right panel: the same values, same bins, stacked and coloured by the group.
#Stacked rather than dodged so the bars still add up to the left-hand histogram -
#the picture has to read as "the same data, coloured in".
split <- function(df, xvar, grp, bins, title, subtitle, xlab, pal) {
  ggplot(df, aes(x = .data[[xvar]], fill = .data[[grp]])) +
    geom_histogram(bins = bins, colour = "white", linewidth = 0.3) +
    scale_fill_manual(values = pal) +
    labs(title = title, subtitle = subtitle, x = xlab, y = "Frequency") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
    base_theme
}

save2 <- function(a, b, file) {
  #align = "hv" keeps the two plot panels the same size. Without it the legend
  #under the right panel steals height from it alone, and the two histograms sit
  #at visibly different heights - which reads as though the axes differ when the
  #whole point is that both panels show the very same values.
  fig <- ggarrange(a, b, nrow = 1, ncol = 2, align = "hv")
  out <- file.path(assets_dir, file)
  ggsave(out, fig, width = 11.5, height = 4.6, dpi = 200, bg = "white")
  message("  ", file, " (", format(file.size(out) %/% 1024L, big.mark = ","), " KB)")
}

message("Writing bimodality figures to ", assets_dir)

#----------------------------------------------------------------------------
#1. Old Faithful - the grouping variable is one we can guess at, then check.
#   Short eruptions are followed by short waits; the 3-minute cut is the standard
#   split of this dataset and falls in the empty gap of the eruptions histogram.

of <- faithful
of$Eruption <- factor(ifelse(of$eruptions < 3, "Short eruption (< 3 min)",
                                               "Long eruption (3 min or more)"),
                      levels = c("Short eruption (< 3 min)",
                                 "Long eruption (3 min or more)"))
save2(
  plain(of, "waiting", 26,
        "Old Faithful: waiting time between eruptions",
        "272 eruptions. Two clear humps - about 55 minutes, and about 80",
        "Waiting time (minutes)"),
  split(of, "waiting", "Eruption", 26,
        "The same waits, split by how long the eruption lasted",
        "Each group is a single hump. The gap was two kinds of eruption",
        "Waiting time (minutes)",
        c("Short eruption (< 3 min)" = ORNG,
          "Long eruption (3 min or more)" = BLUE)),
  "bimodal_oldfaithful.png")

#----------------------------------------------------------------------------
#2. Politics - SIMULATED. See the provenance note at the top of this file.
#   Two partisan groups, each internally unimodal, separated far enough that the
#   pooled distribution dips in the middle.

npol <- 4000L
party <- factor(sample(c("Democrat", "Republican"), npol, replace = TRUE),
                levels = c("Democrat", "Republican"))
score <- ifelse(party == "Democrat",
                rnorm(npol, 32, 13), rnorm(npol, 68, 13))
score <- pmin(pmax(score, 0), 100)   #the scale is bounded at 0 and 100
pol <- data.frame(score = score, Party = party)

save2(
  plain(pol, "score", 30,
        "Attitudes on a contested political issue",
        "Simulated for illustration. The middle is thinner than either side",
        "Position on the issue (0 = strongly oppose, 100 = strongly favor)"),
  split(pol, "score", "Party", 30,
        "The same responses, split by party",
        "Polarization IS population structure: two groups, each one hump",
        "Position on the issue (0 = strongly oppose, 100 = strongly favor)",
        c("Democrat" = DEM, "Republican" = REP)),
  "bimodal_political.png")

#----------------------------------------------------------------------------
#3. Penguins - the payoff. Here the grouping variable was actually recorded, so
#   the claim "two humps means two populations" can be checked rather than argued.

pg <- read.csv(file.path(assets_dir, "penguins_palmer.csv"))
pg <- pg[!is.na(pg$flipper_length_mm), ]
pg$Species <- factor(pg$species, levels = c("Adelie", "Chinstrap", "Gentoo"))

save2(
  plain(pg, "flipper_length_mm", 26,
        "Penguin flipper length, Palmer Archipelago",
        paste0(nrow(pg), " penguins. Two humps, and nothing in particular ",
               "at 205 mm"),
        "Flipper length (mm)"),
  split(pg, "flipper_length_mm", "Species", 26,
        "The same flippers, split by species",
        "Gentoo are simply bigger birds. The gap was three species all along",
        "Flipper length (mm)",
        c("Adelie" = BLUE, "Chinstrap" = ORNG, "Gentoo" = GRN)),
  "bimodal_penguins.png")

#----------------------------------------------------------------------------
#Numbers quoted on the slides, printed so they can be checked against the figures.
message("\nChecks:")
message(sprintf("  Old Faithful  n = %d, waits by eruption type: short %.0f min, long %.0f min",
                nrow(of),
                mean(of$waiting[of$Eruption == "Short eruption (< 3 min)"]),
                mean(of$waiting[of$Eruption == "Long eruption (3 min or more)"])))
message(sprintf("  Penguins      n = %d (Adelie %d, Chinstrap %d, Gentoo %d)",
                nrow(pg), sum(pg$Species == "Adelie"),
                sum(pg$Species == "Chinstrap"), sum(pg$Species == "Gentoo")))
message(sprintf("  Penguins      mean flipper: Adelie %.0f, Chinstrap %.0f, Gentoo %.0f mm",
                mean(pg$flipper_length_mm[pg$Species == "Adelie"]),
                mean(pg$flipper_length_mm[pg$Species == "Chinstrap"]),
                mean(pg$flipper_length_mm[pg$Species == "Gentoo"])))
