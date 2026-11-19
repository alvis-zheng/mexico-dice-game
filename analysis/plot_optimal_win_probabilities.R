library(ggplot2)
library(dplyr)
library(scales)

data <- read.csv("../results/optimal_strategy_win_probabilities.csv")
data$win_probability <- as.numeric(gsub("%", "", data$win_probability))

edge_data <- data %>%
  group_by(n_players) %>%
  summarise(
    first_player = win_probability[player == 1],
    average_rest = mean(win_probability[player != 1]),
    edge = first_player - average_rest,
    .groups = "drop"
  )

ggplot(edge_data, aes(x = n_players, y = edge)) +
  geom_line(color = "#0099ff", linewidth = 1.2) +
  geom_point(color = "#0099ff", size = 3) +
  geom_text(
  aes(label = sprintf("%.1f%%", edge),hjust = ifelse(n_players == 3, -0.1, 0.5)),
  vjust = -1, size = 3.5
  ) +
  scale_x_continuous(breaks = 2:8) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  labs(x = "Number of players", y = "Advantage") +
  theme_minimal(base_size = 16) +
  coord_cartesian(ylim = c(0, 20))
ggsave("first_player_advantage.png", width = 6, height = 3.75, dpi = 300)


five_player_data <- data %>%
  filter(n_players == 5)

ggplot(five_player_data,
       aes(x = factor(player), y = win_probability)) +
  geom_col(fill = "#0099ff", width = 0.65) +
  geom_hline(yintercept = 20,
             linetype = "dashed",
             color = "grey40",
             linewidth = 1) +
  geom_text(aes(label = sprintf("%.2f%%", win_probability)),
            vjust = -1.5, size = 4.5) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 26)
  ) +
  labs(x = "Starting position", y = "Advantage (percentage points)") +
  theme_minimal(base_size = 16)
ggsave("win_probability_by_position.png", width = 6, height = 3.75, dpi = 300)