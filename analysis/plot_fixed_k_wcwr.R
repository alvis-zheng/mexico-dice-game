library(ggplot2)

data_dir <- "../results/wcwr"
player_counts <- 2:8
results_by_n <- vector("list", length(player_counts))

for (i in seq_along(player_counts)) {
  n <- player_counts[i]
  file_path <- file.path(data_dir, sprintf("WCWR_n%d.csv", n))
  results <- read.csv(file_path)

  results$n <- n
  results$is_best <- results$WCWR == max(results$WCWR, na.rm = TRUE)
  results_by_n[[i]] <- results
}

all_data <- do.call(rbind, results_by_n)
all_data$K <- factor(all_data$K, levels = sort(unique(all_data$K)))
all_data$n <- factor(all_data$n, levels = player_counts)
best_points <- subset(all_data, is_best)

n_colours <- c(
  "2" = "tomato",
  "3" = "orange",
  "4" = "limegreen",
  "5" = "skyblue",
  "6" = "dodgerblue",
  "7" = "violet",
  "8" = "purple"
)

all_line <- ggplot(all_data, aes(x = K, y = WCWR, colour = n, group = n)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  geom_point(
    data = best_points,
    aes(x = K, y = WCWR),
    inherit.aes = FALSE,
    shape = 1,
    size = 3,
    stroke = 0.7,
    colour = "black"
  ) +
  scale_colour_manual(values = n_colours, name = "Number of players") +
  guides(colour = guide_legend(nrow = 1)) +
  scale_x_discrete(breaks = levels(factor(all_data$K))[seq(1, 21, 2)]) +
  scale_y_continuous(
    limits = c(0, 0.55),
    labels = scales::label_percent(accuracy = 1)
  ) +
  labs(x = "Fixed threshold K", y = "Worst-case win probability") +
  theme_minimal(base_size = 16) +
  theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      legend.position = "top",
      legend.title.position = "left", 
      legend.box.spacing = grid::unit(0, "pt")
  )

print(all_line)

ggsave("fixed_k_worst_case_win_probability.png", plot = all_line, width = 7.5, height = 5, dpi = 300)