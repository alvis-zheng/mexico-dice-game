# ROLLING DICE

roll_dice <- function() {
  dice <- sample(1:6, 2, replace = TRUE)
  10 * max(dice) + min(dice)
}


# RANDOM PLAYER

random_player <- function(max_rolls = 3) {
  Nrolls <- 1
  score <- roll_dice()

  while (Nrolls < max_rolls) {
    decision <- sample(0:1, 1)

    if (decision == 0) break

    Nrolls <- Nrolls + 1
    score <- roll_dice()
  }

  c(Nrolls = Nrolls, score = score)
}


# CONSERVATIVE PLAYER

conservative_player <- function(previous_scores, max_rolls = 3) {
  Nrolls <- 1
  score <- roll_dice()

  while (Nrolls < max_rolls) {
    if (length(previous_scores) > 0) {
      reference_score1 <- min(previous_scores)
      if (score >= reference_score1) break
    }

    Nrolls <- Nrolls + 1
    score <- roll_dice()
  }

  c(Nrolls = Nrolls, score = score)
}


# AGGRESSIVE PLAYER

aggressive_player <- function(previous_scores, max_rolls = 3) {
  Nrolls <- 1
  score <- roll_dice()

  while (Nrolls < max_rolls) {
    if (length(previous_scores) > 0) {
      reference_score2 <- max(previous_scores)
      if (score >= reference_score2) break
    }

    Nrolls <- Nrolls + 1
    score <- roll_dice()
  }

  c(Nrolls = Nrolls, score = score)
}


# TURN ORDER

clockwise_order <- function(players, first_player) {
  start <- match(first_player, players)

  c(
    players[start:length(players)],
    if (start > 1) players[1:(start - 1)]
  )
}


# PLAY ROUND

play_round <- function(players, first_player, strategies) {
  order <- clockwise_order(players, first_player)

  scores <- numeric(length(players))
  names(scores) <- players

  previous_scores <- numeric(0)
  roll_limit <- 3
  n <- length(players)

  for (player in order) {
    strategy <- unname(strategies[player])

    if (strategy == "R") {
      result <- random_player(max_rolls = roll_limit)

    } else if (strategy == "C") {
      result <- conservative_player(previous_scores, max_rolls = roll_limit)

    } else if (strategy == "A") {
      result <- aggressive_player(previous_scores, max_rolls = roll_limit)

    } else if (strategy == "O") {
      result <- optimal_player(
        previous_scores = previous_scores,
        n = n,
        i = match(player, order),
        max_rolls = roll_limit
      )

    } else if (grepl("^Playable_r", strategy)) {
      result <- playable_player(
        previous_scores = previous_scores,
        n = n,
        i = match(player, order),
        level = strategy,
        max_rolls = roll_limit
      )

    } else {
      stop(paste("Strategy not implemented:", strategy))
    }

    if (player == order[1]) {
      roll_limit <- unname(result["Nrolls"])
    }

    scores[player] <- unname(result["score"])
    previous_scores <- c(previous_scores, unname(scores[player]))
  }

  scores
}


# RESOLVE TIES

resolve_tie <- function(candidates, mode) {
  while (length(candidates) > 1) {
    tie_scores <- numeric(length(candidates))
    names(tie_scores) <- candidates

    for (player in candidates) {
      tie_scores[player] <- roll_dice()
    }

    if (mode == "min") {
      target <- min(tie_scores)
    } else {
      target <- max(tie_scores)
    }

    candidates <- names(tie_scores)[which(tie_scores == target)]
  }

  candidates
}


# FINISH ROUND

finish_round <- function(players, scores, first_player) {
  if (length(unique(scores)) == 1) {
    return(list(
      remaining = players,
      eliminated = character(0),
      next_first = first_player
    ))
  }

  lowest <- min(scores)

  loser_candidates <- names(scores)[which(scores == lowest)]

  if (length(loser_candidates) == 1) {
    eliminated <- loser_candidates
  } else {
    eliminated <- resolve_tie(loser_candidates, "min")
  }

  remaining <- players[players != eliminated]

  if (length(remaining) == 1) {
    return(list(
      remaining = remaining,
      eliminated = eliminated,
      next_first = remaining[1]
    ))
  }

  remaining_scores <- scores[remaining]
  highest <- max(remaining_scores)

  leader_candidates <- names(remaining_scores)[
    which(remaining_scores == highest)
  ]

  if (length(leader_candidates) == 1) {
    next_first <- leader_candidates
  } else {
    next_first <- resolve_tie(leader_candidates, "max")
  }

  list(
    remaining = remaining,
    eliminated = eliminated,
    next_first = next_first
  )
}


# PLAY GAME

play_game <- function(players, strategies, first_player) {
  round_number <- 0
  elimination_order <- character(0)

  while (length(players) > 1) {
    scores <- play_round(players, first_player, strategies)

    outcome <- finish_round(players, scores, first_player)

    elimination_order <- c(elimination_order, outcome$eliminated)

    players <- outcome$remaining
    first_player <- outcome$next_first

    round_number <- round_number + 1
  }

  list(
    winner = players[1],
    rounds = round_number,
    elimination_order = elimination_order
  )
}


# INITIAL FIRST PLAYER

initial_first_player <- function(players) {
  preliminary_scores <- numeric(length(players))
  names(preliminary_scores) <- players

  for (player in players) {
    preliminary_scores[player] <- roll_dice()
  }

  highest <- max(preliminary_scores)

  candidates <- names(preliminary_scores)[
    which(preliminary_scores == highest)
  ]

  if (length(candidates) == 1) {
    first_player <- candidates
  } else {
    first_player <- resolve_tie(candidates, "max")
  }

  first_player
}