simulation_file <- "../src/simulation.R"
source(simulation_file, local = TRUE)

set.seed(123)
n_sim <- 10000L
strategies <- c("Random", "Fixed-K", "Conservative", "Aggressive", "Optimal", "Playable_r2")
optimal_tables <- vector("list", 8L)

for (n in 2:8) {
  tab <- arrow::read_parquet(sprintf("../results/threshold/threshold_n%d.parquet", n))
  keys <- paste(tab$r, tab$L, tab$i, tab$H, sep = "|")
  optimal_tables[[n]] <- list2env(setNames(as.list(tab$threshold), keys), hash = TRUE)
}

optimal_threshold <- function(n, r, limit, position, history) {
  if (length(history) > 2L) {history[history > min(history) & history < max(history)] <- 67L}
  key <- paste(r, limit, position, paste(history, collapse = ","), sep = "|")
  threshold <- get0(key, envir = optimal_tables[[n]], inherits = FALSE)

  if (is.null(threshold)) {
    stop("Missing Optimal threshold for n = ", n, ", key = ", key,
         call. = FALSE)
  }
  threshold
}

playable_first <- cbind(
  c(52L, 52L, 51L, 44L, 43L, 43L, 42L),
  c(44L, 44L, 43L, 42L, 41L, 41L, 33L)
)

play_turn <- function(strategy, n, position, history, limit) {
  result <- switch(strategy,
    Random = random_player(max_rolls = limit),
    Conservative = conservative_player(history, max_rolls = limit),
    Aggressive = aggressive_player(history, max_rolls = limit)
  )
  if (!is.null(result)) {
    return(c(score = result[["score"]], rolls = result[["Nrolls"]]))
  }

  for (used in seq_len(limit)) {
    score <- roll_dice()

    if (used == limit) break
    rerolls <- limit - used
    stick <- switch(strategy,
      # Choose K using the number of players remaining in this round.
      "Fixed-K" = {
        threshold <- if (n <= 4L) 52L else if (n <= 6L) 51L else 44L
        score >= threshold
      },
      Optimal = score >= optimal_threshold(n, rerolls, limit, position, history),
      Playable_r2 = {
        if (position == 1L) score >= playable_first[n - 1L, rerolls]
        else score >= min(history)
      }
    )
    if (stick) break
  }
  c(score = score, rolls = used)
}

play_game <- function(seat_strategies) {
  n <- length(seat_strategies)
  first <- sample.int(n, 1L)
  seats <- c(seq.int(first, n), seq_len(first - 1L))

  while (length(seats) > 1L) {
    n <- length(seats)
    limit <- 3L
    scores <- integer(n)

    for (position in seq_len(n)) {
      history <- scores[seq_len(position - 1L)]
      turn <- play_turn(seat_strategies[seats[position]], n, position, history, limit)
      scores[position] <- turn[["score"]]
      if (position == 1L) limit <- turn[["rolls"]]
    }

    low <- min(scores)
    high <- max(scores)
    if (low == high) next
    lowest <- which(scores == low)
    highest <- which(scores == high)
    out <- lowest[sample.int(length(lowest), 1L)]
    first <- highest[sample.int(length(highest), 1L)]
    order <- c(seq.int(first, n), seq_len(first - 1L))
    seats <- seats[order[order != out]]
  }
  seat_strategies[seats]
}

# Each matchup starts with four players per strategy. Count wins by strategy.
simulate_matchup <- function(row_strategy, column_strategy, n_games) {
  pool <- rep(c(row_strategy, column_strategy), each = 4L)
  wins <- 0L
  for (game in seq_len(n_games)) {
    seat_strategies <- sample(pool)
    if (play_game(seat_strategies) == row_strategy) wins <- wins + 1L
  }
  wins / n_games
}

head_to_head <- function(strategies, n_games) {
  n <- length(strategies)
  rates <- matrix(0.5, n, n, dimnames = list(strategies, strategies))
  for (row in seq_len(n - 1L)) {
    for (column in seq.int(row + 1L, n)) {
      p <- simulate_matchup(strategies[row], strategies[column], n_games)
      rates[row, column] <- p
      rates[column, row] <- 1 - p
    }
  }
  rates
}

plot_matchups <- function(rates) {
  strategy_labels <- c(
      "Random" = "Rand",
      "Fixed-K" = "K",
      "Conservative" = "Cons",
      "Aggressive" = "Aggr",
      "Optimal" = "DP",
      "Playable_r2" = "Play"
  )
  n <- nrow(rates)
  pos <- seq_len(n)
  colours <- colorRampPalette(c("#FF8C42", "#FFC49A", "#F5F8FC", "#8DCCF2", "#0099FF"))(100)
  breaks <- seq(0, 1, length.out = length(colours) + 1)
  midpoints <- (seq_along(colours) - 0.5) / length(colours)

  legend_height <- 120
  png("head_to_head_matrix.png", width = 1950, height = 1350 + legend_height, res = 150)
  layout(rbind(c(1, 2), c(3, 3)), widths = c(8, 1.5),
         heights = c(1350, legend_height))

  par(cex = 1, mex = 1, mar = c(7, 12, 1.5, 0.6))
  image(pos, pos, t(rates), col = colours, breaks = breaks,
        axes = FALSE, xlab = "", ylab = "")
  axis(1, at = pos, labels = strategy_labels[colnames(rates)], tick = FALSE, cex.axis = 1.5)
  axis(2, at = pos, labels = strategy_labels[rownames(rates)], tick = FALSE, las = 1, cex.axis = 1.5)

  mtext("Opponent strategy", side = 1, line = 4, cex = 1.7)
  mtext("Player strategy", side = 2, line = 7, cex = 1.7)
    
  borders <- seq(0.5, n + 0.5)
  abline(h = borders, v = borders, col = "#FFFFFFA6", lwd = 0.8)

  for (i in pos) {
    p <- rates[i, ]
    text(pos, i, sprintf("%.1f%%", 100 * p),
     font = 1, cex = 1.6,
     col = ifelse(p < 0.13 | p > 0.85, "white", "#111111"))
  }

  par(mar = c(7, 0.4, 1.5, 8))
  image(c(0, 1), breaks, matrix(midpoints, nrow = 1),
        col = colours, breaks = breaks, axes = FALSE, xlab = "", ylab = "")
  ticks <- seq(0, 1, by = 0.25)
  axis(4, at = ticks, labels = paste0(100 * ticks, "%"), las = 1, cex.axis = 1.6)
  mtext("Winning probability", side = 4, line = 5.8, cex = 1.7)

  par(mar = rep(0, 4))
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i")
 
    legend_x <- c(0.15, 0.41, 0.68)

  text(legend_x, rep(0.8, 3),
     labels = c("Play - Playable strategy", "DP - Optimal strategy", "Rand - Random"),
     adj = c(0, 0.5), cex = 1.5)

  text(legend_x, rep(0.35, 3),
     labels = c("Cons - Conservative", "K - K-threshold", "Aggr - Aggressive"),
     adj = c(0, 0.5), cex = 1.5)
  dev.off()
}

win_rates <- head_to_head(strategies, n_sim)
plot_matchups(win_rates)