library(ggplot2)

standard_p1 <- c(59.540908, 39.041568, 28.380408, 22.226391)

results <- data.frame(
  model = c(
    rep("1 die", 7),
    rep("2 dice", 4),
    rep("3 dice", 4),
    rep("4 dice", 4),
    rep("1 reroll", 4),
    rep("2 rerolls", 4),
    rep("3 rerolls", 6),
    rep("4 rerolls", 4)
  ),

  n_players = c(
    2:8,
    2:5,
    2:5,
    2:5,
    2:5,
    2:5,
    2:7,
    2:5
  ),

  p1 = c(
    # 1 die
    61.644476, 39.669737, 28.572767,
    22.356983, 18.330885, 15.517772, 13.471646,

    # 2 dice
    standard_p1,

    # 3 dice
    59.036547, 38.898376, 28.317243, 22.191047,

    # 4 dice
    58.870696, 38.855469, 28.295300, 22.176988,

    # 1 reroll
    56.055628, 36.810776, 27.119934, 21.449203,

    # 2 rerolls
    standard_p1,

    # 3 rerolls
    61.873349, 40.674863, 29.285466,
    22.758242, 18.614911, 15.740823,

    # 4 rerolls
    63.710945, 41.994157, 30.016189, 23.163070
  )
)

# Use the same advantage as in Figure 1, relative to the mean opponent.
results$baseline <- (100 - results$p1) / (results$n_players - 1)
results$advantage <- results$p1 - results$baseline

dice_data <- subset(
  results,
  model %in% c("1 die", "2 dice", "3 dice", "4 dice") &
    n_players <= 5
)

dice_data$model <- factor(
  dice_data$model,
  levels = c("1 die", "2 dice", "3 dice", "4 dice")
)

reroll_data <- subset(
  results,
  model %in% c("1 reroll", "2 rerolls", "3 rerolls", "4 rerolls") &
    n_players <= 5
)

reroll_data$model <- factor(
  reroll_data$model,
  levels = c("1 reroll", "2 rerolls", "3 rerolls", "4 rerolls")
)

# The second colour marks the standard setting in both figures.
variant_colours <- c("#D55E00", "#0099FF", "#7B2CBF", "#555555")
variant_lines <- c("solid", "solid", "dashed", "dotted")
y_breaks <- pretty(c(0, dice_data$advantage, reroll_data$advantage))

# Figure 8
p1 <- ggplot(
  dice_data,
  aes(x = n_players, y = advantage,
      colour = model, linetype = model, group = model)
) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.8) +
  scale_x_continuous(breaks = 2:5) +
  scale_y_continuous(limits = range(y_breaks), breaks = y_breaks, labels = function(x) paste0(x, "%")) +
  scale_colour_manual(values = variant_colours) +
  scale_linetype_manual(values = variant_lines) +
  labs(
    x = "Number of players",
    y = "Advantage",
    colour = "Number of dice",
    linetype = "Number of dice"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top", legend.margin = margin(b = -10), legend.key.width = grid::unit(1, "cm"))

ggsave("dice_advantage.png", p1, width = 7, height = 4.5, dpi = 300,
       bg = "white")

# Figure 9
p2 <- ggplot(
  reroll_data,
  aes(x = n_players, y = advantage,
      colour = model, linetype = model, group = model)
) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.8) +
  scale_x_continuous(breaks = 2:5) +
  scale_y_continuous(limits = range(y_breaks), breaks = y_breaks, labels = function(x) paste0(x, "%")) +
  scale_colour_manual(values = variant_colours) +
  scale_linetype_manual(values = variant_lines) +
  labs(
    x = "Number of players",
    y = "Advantage",
    colour = "Maximum rerolls",
    linetype = "Maximum rerolls"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top", legend.margin = margin(b = -10), legend.key.width = grid::unit(1, "cm"))

ggsave("reroll_advantage.png", p2, width = 7, height = 4.5, dpi = 300,
       bg = "white")
