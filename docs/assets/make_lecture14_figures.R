#Builds the figures for the Week 6 Lecture 14 deck (10/2/2026): statistical
#significance (carried from Lecture 13) and an introduction to probability, random
#variables and discrete probability distributions. Every other Lecture 14 figure is
#carried over from make_lecture13_figures.R.
#
#  l14_three_tosses.png  the probability distribution of X = number of heads in three
#                        tosses of a fair coin, each bar labelled with its probability
#
#  l14_figure_sizes.csv  the size, in points, each figure is drawn at
#
#TYPE SIZE. Same rule as the other make_lectureN_figures.R scripts: nothing on a slide
#is under 16pt, so the figure is drawn at exactly its on-slide size (points / 72 inches)
#and every text size goes through tsz() or el(), which refuse anything smaller.

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

NAVY <- "#1F3864"; PALE <- "#D9E5F7"
MIN_PT <- 16
tsz <- function(pt) { stopifnot(pt >= MIN_PT); pt / ggplot2::.pt }
el  <- function(pt, ...) { stopifnot(pt >= MIN_PT); element_text(size = pt, ...) }

sizes <- data.frame(file = character(), w_pt = numeric(), h_pt = numeric(),
                    stringsAsFactors = FALSE)
save_fig <- function(p, file, w_pt, h_pt) {
  ggsave(file.path(assets_dir, file), p, width = w_pt / 72, height = h_pt / 72,
         dpi = 300, bg = "white")
  sizes[nrow(sizes) + 1, ] <<- list(file, w_pt, h_pt)
}

#the three-toss distribution, counted from the 8 outcomes rather than from a formula.
#Three tosses (Oct 2 2026, was four) so the sample space can be listed on the board.
#NOTE Homework 2 Part III asks for P(at least one head) in 3 flips, so the slides
#never compute P(X >= 1) - the questions below are P(X >= 2), P(X <= 1), P(1 <= X <= 2).
seqs <- expand.grid(rep(list(c("H", "T")), 3), stringsAsFactors = FALSE)
heads <- rowSums(seqs == "H")
ways <- as.vector(table(factor(heads, levels = 0:3)))
stopifnot(nrow(seqs) == 8, identical(ways, c(1L, 3L, 3L, 1L)),
          sum(ways[3:4]) == 4, sum(ways[1:2]) == 4, sum(ways[2:3]) == 6,
          sum(0:3 * ways / 8) == 1.5, sum((0:3 - 1.5)^2 * ways / 8) == 0.75)
d <- data.frame(x = 0:3, p = ways / 8, lab = paste0(ways, "/8"))
p3 <- ggplot(d, aes(factor(x), p)) +
  geom_col(width = 0.7, fill = PALE, colour = "grey35", linewidth = 0.4) +
  geom_text(aes(label = lab), vjust = -0.4, size = tsz(18), colour = NAVY, fontface = "bold") +
  scale_y_continuous(breaks = c(0, 0.125, 0.25, 0.375), labels = c("0", ".125", ".25", ".375")) +
  coord_cartesian(ylim = c(0, 0.47), expand = FALSE) +
  labs(x = "x = number of heads", y = "P(x)") +
  theme_classic() +
  theme(axis.title = el(16), axis.text = el(16), plot.margin = margin(8, 10, 2, 6))
save_fig(p3, "l14_three_tosses.png", 560, 225)

#----------------------------------------------------------------------------
#l14_venn.png: Venn diagrams in the style of the 2024 Lecture 11/12 slides - a grey
#"The Sample Space" box, event A pink, event B blue. Top: A and B overlap, the overlap
#is the intersection. Bottom: disjoint events, no overlap.
PINK <- "#F4B6B6"; LBLUE <- "#B4CFE6"; BOX <- "#F2F2F2"; EDGE <- "#1F2F4F"
ell <- function(cx, cy, a, b, n = 200) {
  t <- seq(0, 2 * pi, length.out = n); data.frame(x = cx + a * cos(t), y = cy + b * sin(t)) }
venn_panel <- function(overlap, title_lab, mid_lab) {
  ax <- if (overlap) 3.9 else 3.0; bx <- if (overlap) 6.1 else 7.0
  A <- ell(ax, 2.0, 1.9, 1.25); B <- ell(bx, 2.0, 1.9, 1.25)
  p <- ggplot() +
    annotate("rect", xmin = 0, xmax = 10, ymin = 0, ymax = 4.4, fill = BOX, colour = "black", linewidth = 1) +
    geom_polygon(data = A, aes(x, y), fill = PINK, alpha = 0.85, colour = EDGE, linewidth = 1.1) +
    geom_polygon(data = B, aes(x, y), fill = LBLUE, alpha = 0.75, colour = EDGE, linewidth = 1.1) +
    annotate("text", x = 0.3, y = 4.05, label = "The Sample Space", hjust = 0, size = tsz(16), fontface = "bold") +
    annotate("text", x = ax - (if (overlap) 0.7 else 0), y = 2.0, label = "A", size = tsz(20), fontface = "bold") +
    annotate("text", x = bx + (if (overlap) 0.7 else 0), y = 2.0, label = "B", size = tsz(20), fontface = "bold") +
    annotate("text", x = 9.7, y = 4.05, label = title_lab, hjust = 1, size = tsz(16), colour = EDGE, fontface = "bold")
  if (!is.null(mid_lab)) p <- p + annotate("text", x = 5, y = 0.35, label = mid_lab, size = tsz(16), colour = EDGE)
  p + coord_fixed(xlim = c(0, 10), ylim = c(0, 4.4), expand = FALSE) + theme_void() +
    theme(plot.margin = margin(3, 3, 3, 3))
}
v_top <- venn_panel(TRUE,  "overlap = A ∩ B", NULL)
v_bot <- venn_panel(FALSE, "disjoint: A ∩ B = ∅", NULL)
save_fig(ggpubr::ggarrange(v_top, v_bot, ncol = 1), "l14_venn.png", 380, 340)

write.csv(sizes, file.path(assets_dir, "l14_figure_sizes.csv"), row.names = FALSE)
cat("wrote", nrow(sizes), "figures to", assets_dir, "\n")
