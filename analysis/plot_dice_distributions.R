library(ggplot2)

set.seed(123)

roll_dice <- function(n) {
  die1 <- sample(1:6, n, replace = TRUE)
  die2 <- sample(1:6, n, replace = TRUE)
  10 * pmax(die1, die2) + pmin(die1, die2)
}


simulate_best <- function(n_rolls, simulations = 100000) {
  results <- replicate(simulations, {max(roll_dice(n_rolls))})
  data.frame(score = results)
}


dist_1 <- simulate_best(1)
dist_2 <- simulate_best(2)
dist_3 <- simulate_best(3)


plot_data <- rbind(
  transform(dist_1, rolls = "1 roll"),
  transform(dist_2, rolls = "2 rolls"),
  transform(dist_3, rolls = "3 rolls")
)

score_levels <- sort(unique(plot_data$score))
plot_data$score_index <- match(plot_data$score, score_levels)
tick_positions <- seq(1, length(score_levels), by = 2)

fig4 <- ggplot(plot_data, aes(x = score_index, colour = rolls)) +
  geom_hline(
    yintercept = 0.5,
    linetype = "dashed",
    colour = "grey55",
    linewidth = 0.5
  ) +
  stat_ecdf(geom = "step", linewidth = 1.1) +
  scale_colour_manual(values = c(
    "1 roll" = "#0099ff",
    "2 rolls" = "#D55E00",
    "3 rolls" = "#7B2CBF"
  )) +
  scale_x_continuous(breaks = tick_positions, labels = score_levels[tick_positions]) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25), labels = scales::label_percent(accuracy = 1)) +
  labs(x = "Best dice score", y = "Cumulative probability", colour = NULL) +
  theme_minimal(base_size = 16) +
  theme(legend.position = "top", panel.grid.minor = element_blank())

print(fig4)

ggsave("best_score_cumulative_distribution.png", plot = fig4, width = 6.5, height = 4.2, dpi = 300)