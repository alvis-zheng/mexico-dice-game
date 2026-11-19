library(arrow)
library(data.table)
library(ggplot2)
source("../src/simulation.R", local = TRUE)

# Read and index the dynamic programming thresholds.
threshold_files <- sprintf("../results/threshold/threshold_n%d.parquet", 2:8)

missing_files <- threshold_files[!file.exists(threshold_files)]

if (length(missing_files) > 0) {
  stop(
    "Missing threshold files: ",
    paste(missing_files, collapse = ", "))
}

threshold_tables <- vector("list", 8L)
for (n in 2:8) {
  file <- threshold_files[n - 1L]
  tab <- as.data.table(read_parquet(file))

  if (nrow(tab) == 0L || anyNA(tab$n) || any(tab$n != n)) {stop("Invalid player count in ", file)}

  if (is.list(tab$H)) {
    tab[, H := vapply(H, function(x) paste(x, collapse = ","), character(1))]
  } else {
    tab[, H := as.character(H)]
  }

  tab[i == 1L & is.na(H), H := ""]
  tab[, H := gsub("[[:space:]]", "", H)]

  keys <- paste(tab$r, tab$L, tab$i, tab$H, sep = "|")
  if (anyDuplicated(keys)) {stop("Duplicate threshold keys in ", file)}

  values <- tab$threshold
  if ("valid_threshold" %in% names(tab)) {
    valid <- as.logical(tab$valid_threshold)
    values[is.na(valid) | !valid] <- NA_integer_
  }

  threshold_tables[[n]] <- list2env(
    setNames(as.list(values), keys),
    parent = emptyenv(),
    hash = TRUE
  )
}
rm(tab, keys, values)

# Look up the stopping threshold for a game state.
history_encoding <- "middle67"

possible_scores <- sort(unique(as.vector(
  outer(
    1:6, 1:6,
    function(a, b) 10 * pmax(a, b) + pmin(a, b)))))

compress_history <- function(H) {
  if (length(H) == 0) {
    return("")
  }

  if (anyNA(H) || !all(H %in% possible_scores)) {
    stop("H must contain actual previous dice scores")
  }

  low <- min(H)
  high <- max(H)

  middle <- H > low & H < high

  if (any(middle)) {
    if (history_encoding == "middle67") {
      H[middle] <- 67
    } else if (history_encoding == "smallest_between") {
      H[middle] <- min(
        possible_scores[
          possible_scores > low & possible_scores < high])
    } else {
      stop("Unknown history encoding")
    }
  }

  paste(H, collapse = ",")
}

get_threshold <- function(n, r, L, H, i) {
  if (length(H) != i - 1L) {stop("History length must equal i - 1")}

  history <- compress_history(H)
  key <- paste(r, L, i, history, sep = "|")
  threshold <- get0(key, envir = threshold_tables[[n]], inherits = FALSE)

  if (is.null(threshold) || is.na(threshold)) {stop("Missing or invalid threshold for n = ", n, ", key = ", key)}
  threshold
}

# Check known states to verify the threshold tables and history encoding.
stopifnot(
  get_threshold(
    n = 7, r = 2, L = 3,
    H = numeric(0), i = 1
  ) == 41,

  get_threshold(
    n = 7, r = 1, L = 3,
    H = numeric(0), i = 1
  ) == 43,

  get_threshold(
    n = 7, r = 1, L = 3,
    H = c(32, 44, 54, 51), i = 5
  ) == 33)

# Optimal: stop at or above the threshold for the current state.
optimal_player <- function(previous_scores, n, i, max_rolls = 3) {
  Nrolls <- 1
  score <- roll_dice()

  while (Nrolls < max_rolls) {
    r <- max_rolls - Nrolls

    threshold <- get_threshold(
      n = n,
      r = r,
      L = max_rolls,
      H = previous_scores,
      i = i)

    if (score >= threshold) {
      break
    }
    Nrolls <- Nrolls + 1
    score <- roll_dice()
  }

  return(c(
    Nrolls = Nrolls,
    score = score))
}

# Simulate 10,000 eight-player games with two players per strategy.
set.seed(123)

n_sim_test <- 10000
players <- LETTERS[1:8]

strategy_pool <- rep(c("R", "C", "A", "O"), each = 2)

winner_strategies <- character(n_sim_test)

strategy_history <- matrix(
  "",
  nrow = n_sim_test,
  ncol = 8)

colnames(strategy_history) <- players

positions <- matrix(
  NA_integer_,
  nrow = n_sim_test,
  ncol = 8)

colnames(positions) <- players

for (j in seq_len(n_sim_test)) {
  strategies <- sample(strategy_pool)
  names(strategies) <- players
  strategy_history[j, ] <- strategies[players]
  first_player <- initial_first_player(players)

  result <- play_game(players, strategies, first_player)

  if (length(result$elimination_order) != 7 || anyDuplicated(result$elimination_order) > 0) {
    stop("Incorrect elimination order")
  }

  winner_strategies[j] <- strategies[result$winner]

  positions[j, result$elimination_order] <- 8:2
  positions[j, result$winner] <- 1
}

# Count wins by strategy.
strategy_counts <- table(
  factor(winner_strategies, levels = c("R", "C", "A", "O"))
)

strategy_rates <- strategy_counts / n_sim_test

# Calculate each strategy's mean finishing position.
average_positions <- numeric(4)
names(average_positions) <- c("R", "C", "A", "O")

for (strategy in names(average_positions)) {
  average_positions[strategy] <- mean(
    positions[strategy_history == strategy])
}

# Print the values used in the results tables.
strategy_labels <- c("Random", "Conservative", "Aggressive", "Optimal")
win_percent <- 100 * as.numeric(strategy_rates)

strategy_summary <- data.frame(
  strategy = strategy_labels,
  win_rate_percent = round(win_percent, 2),
  mean_finishing_position = round(as.numeric(average_positions), 2)
)
print(strategy_summary, row.names = FALSE)

# Save the report figure.
plot_data <- data.frame(
  strategy = factor(strategy_labels, levels = strategy_labels),
  win_rate = win_percent
)

win_prob_plot <- ggplot(plot_data, aes(x = strategy, y = win_rate)) +
  geom_col(fill = "dodgerblue") +
  geom_text(
    aes(label = sprintf("%.2f%%", win_rate)),
    vjust = -0.4,
    size = 4.5
  ) +
  labs(x = "Strategy", y = "Winning Probability") +
  scale_y_continuous(
    limits = c(0, max(60, win_percent)),
    breaks = seq(0, 100, by = 10),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0.05, 0.10))
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 15),
    axis.title.y = element_text(margin = margin(r = 15))
  )

ggsave(
  "four_strategy_win_rates_10k.png",
  plot = win_prob_plot,
  width = 6.5, height = 4.2, units = "in", dpi = 300,
  bg = "white"
)