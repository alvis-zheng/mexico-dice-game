library(data.table)
source("../src/simulation.R", local = TRUE)

# PLAYABLE STRATEGY THRESHOLDS

playable_thresholds <- data.table(
  n = 2:8,
  threshold_2 = c(44, 44, 43, 42, 41, 41, 33),
  threshold_1 = c(52, 52, 51, 44, 43, 43, 42)
)


# PLAYABLE PLAYER

playable_player <- function(previous_scores, n, i, level, max_rolls = 3) {

  if (i == 1) {
    Nrolls <- 1
    score <- roll_dice()

    while (Nrolls < max_rolls) {
      r <- max_rolls - Nrolls
      row_index <- match(n, playable_thresholds$n)

      if (is.na(row_index)) {
        stop(paste("No Playable threshold for n =", n))
      }

      if (r == 2) {
        threshold <- playable_thresholds$threshold_2[row_index]
      } else {
        threshold <- playable_thresholds$threshold_1[row_index]
      }

      if (score >= threshold) {
        break
      }

      Nrolls <- Nrolls + 1
      score <- roll_dice()
    }

    return(c(Nrolls = Nrolls, score = score))
  }


  conservative_rounds <- as.integer(sub("Playable_r", "", level))

  if (n > conservative_rounds) {result <- conservative_player(
      previous_scores, max_rolls)
  } else {
    result <- aggressive_player(
      previous_scores, max_rolls
    )
  }

  return(result)
}

# PLAYABLE STRATEGY MONTE CARLO SIMULATIONS

set.seed(123)

n_sim <- 10000
players <- LETTERS[1:8]
playable_levels <- paste0("Playable_r", 2:8)
simulation_results <- data.table()
all_winner_strategies <- list()

for (level in playable_levels) {
  cat("Simulating:", level, "\n")

  strategy_pool <- c(
    rep("R", 2),
    rep("C", 2),
    rep("A", 2),
    rep(level, 2)
  )

  winners <- character(n_sim)
  winner_strategies <- character(n_sim)

  positions <- matrix(NA_integer_,nrow = n_sim,ncol = 8)
  colnames(positions) <- players
  strategy_history <- matrix("",nrow = n_sim,ncol = 8)
  colnames(strategy_history) <- players

  for (j in seq_len(n_sim)) {
    strategies <- sample(strategy_pool)
    names(strategies) <- players

    strategy_history[j, ] <- strategies[players]

    first_player <- initial_first_player(players)

    result <- play_game(
      players,
      strategies,
      first_player
    )

    if (
      length(result$elimination_order) != 7 ||
      anyDuplicated(result$elimination_order) > 0
    ) {
      stop("Incorrect elimination order")
    }

    winners[j] <- result$winner
    winner_strategies[j] <- strategies[result$winner]
    positions[j, result$elimination_order] <- 8:2
    positions[j, result$winner] <- 1
  }

  all_winner_strategies[[level]] <- winner_strategies

  win_counts <- table(
    factor(
      winner_strategies,
      levels = c("R", "C", "A", level)
    )
  )

  win_rates <- as.numeric(win_counts) / n_sim
  names(win_rates) <- c("R", "C", "A", level)

  average_positions <- sapply(
    c("R", "C", "A", level),
    function(strategy) {
      mean(positions[strategy_history == strategy])
    }
  )

  simulation_results <- rbind(
    simulation_results,
    data.table(
      playable_level = level,
      strategy = names(win_rates),
      win_rate = as.numeric(win_rates),
      average_position = as.numeric(average_positions)
    )
  )
}

print(simulation_results)

# FINAL COMPARISON TABLE

playable_comparison <- simulation_results[
  strategy == playable_level,
  .(
    Level = playable_level,
    Win_Rate = round(win_rate, 4),
    Win_Percentage = round(win_rate * 100, 2),
    Average_Position = round(average_position, 3)
  )
]

print(playable_comparison)

# Figure 5: playable strategy win probability by switch point.
switch_counts <- sub("^Playable_r", "", playable_comparison$Level)
win_percent <- playable_comparison$Win_Percentage

png("playable_win_rate_by_switch.png", width = 6.5, height = 4,
    units = "in", res = 300, pointsize = 12)
par(mar = c(4.2, 4.8, 0.8, 0.5), mgp = c(2.7, 0.7, 0))
upper_limit <- max(60, 10 * ceiling((max(win_percent) + 3) / 10))
ticks <- seq(0, upper_limit, by = 10)
centres <- barplot(
  win_percent, names.arg = switch_counts, axes = FALSE,
  xlab = "Players remaining at switch", ylab = "Winning probability",
  ylim = c(0, upper_limit), yaxs = "i", col = "dodgerblue", border = NA
)
axis(2, at = ticks, labels = paste0(ticks, "%"), las = 1)
text(centres, win_percent, labels = sprintf("%.2f%%", win_percent),
     pos = 3, offset = 0.35, cex = 0.85)
invisible(dev.off())