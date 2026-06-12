library(ggplot2)

col_sup  <- "#7F77DD"
col_ni   <- "#1D9E75"
col_eq   <- "#D85A30"
col_axis <- "#888780"
col_line <- "#2C2C2A"

# CI data
cis <- data.frame(
  label = factor(c("Superiority", "Non-inferiority", "Equivalence"),
                 levels = c("Equivalence", "Non-inferiority", "Superiority")),
  lo    = c(0.05, -0.10, -0.13),
  hi    = c(0.37,  0.24,  0.14),
  mid   = c(0.21,  0.07,  0.005),
  y     = c(3, 2, 1),
  color = c(col_sup, col_ni, col_eq)
)

ggplot() +
  
  # shading: superiority H0 (left of 0)
  annotate("rect", xmin = -0.55, xmax = 0,
           ymin = 2.6, ymax = 3.4, fill = col_sup, alpha = 0.10) +
  
  # shading: non-inferiority H0 (left of -delta)
  annotate("rect", xmin = -0.55, xmax = -0.20,
           ymin = 1.6, ymax = 2.4, fill = col_ni, alpha = 0.10) +
  
  # shading: equivalence H0 (both tails)
  annotate("rect", xmin = -0.55, xmax = -0.20,
           ymin = 0.6, ymax = 1.4, fill = col_eq, alpha = 0.10) +
  annotate("rect", xmin = 0.20, xmax = 0.55,
           ymin = 0.6, ymax = 1.4, fill = col_eq, alpha = 0.10) +
  
  # zero line
  geom_vline(xintercept = 0, colour = col_line,
             linewidth = 0.5, alpha = 0.45, linetype = "solid") +
  
  # NI margin
  geom_segment(aes(x = -0.20, xend = -0.20, y = 1.6, yend = 2.4),
               colour = col_ni, linewidth = 0.7, linetype = "dashed", alpha = 0.8) +
  
  # Equivalence margins
  geom_segment(aes(x = -0.20, xend = -0.20, y = 0.6, yend = 1.4),
               colour = col_eq, linewidth = 0.7, linetype = "dashed", alpha = 0.8) +
  geom_segment(aes(x =  0.20, xend =  0.20, y = 0.6, yend = 1.4),
               colour = col_eq, linewidth = 0.7, linetype = "dashed", alpha = 0.8) +
  
  # CI bars
  geom_segment(data = cis,
               aes(x = lo, xend = hi, y = y, yend = y, colour = label),
               linewidth = 1.6, lineend = "round", show.legend = FALSE) +
  geom_segment(data = cis,
               aes(x = lo, xend = lo, y = y - 0.09, yend = y + 0.09, colour = label),
               linewidth = 1.1, show.legend = FALSE) +
  geom_segment(data = cis,
               aes(x = hi, xend = hi, y = y - 0.09, yend = y + 0.09, colour = label),
               linewidth = 1.1, show.legend = FALSE) +
  geom_point(data = cis,
             aes(x = mid, y = y, colour = label),
             size = 2.8, shape = 16, show.legend = FALSE) +
  
  # row labels (right side)
  geom_text(data = cis,
            aes(x = 0.57, y = y, label = label, colour = label),
            hjust = 0, size = 3.8, fontface = "bold", show.legend = FALSE) +
  
  # axis zero label
  annotate("text", x = 0, y = 0.45, label = "0",
           size = 3, colour = col_axis, hjust = 0.5) +
  
  # margin labels
  annotate("text", x = -0.20, y = 0.45, label = "-\u0394",
           size = 3, colour = col_axis, hjust = 0.5) +
  annotate("text", x =  0.20, y = 0.45, label = "+\u0394",
           size = 3, colour = col_axis, hjust = 0.5) +
  
  scale_colour_manual(values = c(
    "Superiority"     = col_sup,
    "Non-inferiority" = col_ni,
    "Equivalence"     = col_eq
  )) +
  
  coord_cartesian(xlim = c(-0.55, 0.85), ylim = c(0.3, 3.6), clip = "off") +
  
  theme_minimal(base_size = 12) +
  theme(
    panel.grid       = element_blank(),
    axis.text        = element_blank(),
    axis.ticks       = element_blank(),
    axis.title       = element_blank(),
    axis.line.x      = element_line(colour = col_axis, linewidth = 0.5),
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.margin      = margin(16, 100, 12, 20)
  )

ggsave("trial_designs.png", width = 8, height = 4.5, dpi = 200, bg = "white")