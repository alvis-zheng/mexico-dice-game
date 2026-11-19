library(arrow)
library(dplyr)

make_threshold <- function(strategy) {
  data <- filter(strategy, r > 0)
  data$H <- vapply(
    data$H,
    function(x) paste(x, collapse = ","),
    character(1)
  )

  data <- group_by(data, n, r, L, H, i)
  result <- summarise(
    data,
    threshold = min(s[strategy == "STICK"]),
    .groups = "drop"
  )

  return(result)
}

for (n in 2:8) {
  strategy <- read_parquet(
    sprintf("results/optimal_winning_strategy/optimal_winning_strategy_n%d.parquet", n)
  )

  threshold <- make_threshold(strategy)

  write_parquet(
    threshold,
    sprintf("results/threshold/threshold_n%d.parquet", n)
  )
}