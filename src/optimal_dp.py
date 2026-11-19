from collections import Counter
from functools import lru_cache


MAX_ROLLS = 3
MAX_PLAYERS = 8

_counts = Counter(
    10 * max(a, b) + min(a, b)
    for a in range(1, 7)
    for b in range(1, 7)
)
OUTCOMES = tuple((s, c / 36) for s, c in sorted(_counts.items()))


def compress_scores(scores):
    """Compress the scores of middle players."""
    scores = tuple(scores)
    played = [value for value in scores if value is not None]

    if not played:
        return scores

    low, high = min(played), max(played)
    # Use the smallest valid score between the minimum and maximum as the common value.
    middle = next((s for s, _ in OUTCOMES if low < s < high), low)
    return tuple(
        score if score is None or score in (low, high) else middle
        for score in scores
    )


def compress_last_score(score, scores):
    """With no subsequent players to roll, the last player's score can be compressed further."""
    played = [value for value in scores if value is not None]

    if not played:
        return score

    low, high = min(played), max(played)
    if score < low:
        return OUTCOMES[0][0]
    if score > high:
        return OUTCOMES[-1][0]
    if low < score < high:
        return next(s for s, _ in OUTCOMES if low < s < high)
    return score


def expected_vector(fn, n):
    """Compute the expected win probabilities over all possible dice outcomes."""
    result = [0] * n
    for score, probability in OUTCOMES:
        values = fn(score)
        for i in range(n):
            result[i] += probability * values[i]
    return tuple(result)


class GameSolver:
    def __init__(self, n, previous=None):
        self.n = n
        self.previous = previous
        self.strategy = {}
        self.restart_value = (1 / n,) * n
        self._round_end_cached = lru_cache(None)(self._round_end)
        self._last_player_turn_cached = lru_cache(None)(self._player_turn)
        self._next_player_cached = lru_cache(None)(self._next_player)
        self.starter_decision = lru_cache(None)(self._starter_decision)
        self.reroll_value = lru_cache(None)(self._reroll_value)
        self.starter_reroll_value = lru_cache(None)(self._starter_reroll_value)

    def solve(self, tol=1e-12, max_iterations=100):
        """Solve all-equal replays using fixed-point iteration."""
        for _ in range(max_iterations):
            # Clear cached values and strategies before updating the replay estimate.
            for cached in (
                self._round_end_cached,
                self._last_player_turn_cached,
                self._next_player_cached,
                self.starter_decision,
                self.reroll_value,
                self.starter_reroll_value,
            ):
                cached.cache_clear()
            self.strategy.clear()

            values = expected_vector(
                lambda score: self.starter_decision(score, 1),
                self.n,
            )
            error = max(
                abs(new - old)
                for new, old in zip(values, self.restart_value)
            )
            if error < tol:
                return values
            self.restart_value = values
        raise RuntimeError("All-equal replay iteration did not converge.")
    
    def show_strategy(self, position, score, used, limit, scores):
        """Retrieve the optimal action for a given state."""
        if position == 0:
            key = ("starter", score, used)
        else:
            scores = compress_scores(scores)
            if position == self.n - 1:
                score = compress_last_score(score, scores)
            key = ("player", position, score, used, limit, scores)
        return self.strategy[key]

    def _starter_decision(self, score, used):
        """Choose the action that maximises the starting player's win probability."""
        scores = (score,) + (None,) * (self.n - 1)
        stop = self.next_player(1, scores, used)
        key = ("starter", score, used)

        if used < MAX_ROLLS:
            reroll = self.starter_reroll_value(used)
            if reroll[0] > stop[0]:
                self.strategy[key] = "reroll"
                return reroll
        self.strategy[key] = "stick"
        return stop

    def _starter_reroll_value(self, used):
        """Calculate the expected value of rerolling for the starting player."""
        return expected_vector(
            lambda score: self.starter_decision(score, used + 1),
            self.n,
        )

    def next_player(self, position, scores, limit):
        """Compress the current scores before checking the cached result."""
        return self._next_player_cached(position, compress_scores(scores), limit)

    def _next_player(self, position, scores, limit):
        """Average over the next player's first roll."""
        return expected_vector(
            lambda score: self.player_turn(position, score, 1, limit, scores),
            self.n,
        )

    def player_turn(self, position, score, used, limit, scores):
        """Compress the state and use a separate cache for the last player."""
        scores = compress_scores(scores)
        
        if position == self.n - 1:
            score = compress_last_score(score, scores)
            return self._last_player_turn_cached(position, score, used, limit, scores)
        return self._player_turn(position, score, used, limit, scores)

    def _player_turn(self, position, score, used, limit, scores):
        """Choose the action that maximises the current player's win probability."""
        updated = list(scores)
        updated[position] = score
        updated = tuple(updated)

        if position == self.n - 1:
            stop = self.round_end(updated)
        else:
            stop = self.next_player(position + 1, updated, limit)

        key = ("player", position, score, used, limit, scores)
        if used < limit:
            reroll = self.reroll_value(position, used, limit, scores)
            if reroll[position] > stop[position]:
                self.strategy[key] = "reroll"
                return reroll

        self.strategy[key] = "stick"
        return stop

    def _reroll_value(self, position, used, limit, scores):
        """Calculate the expected win probabilities after a reroll."""
        return expected_vector(
            lambda score: self.player_turn(
                position, score, used + 1, limit, scores
            ),
            self.n,
        )

    def round_end(self, scores):
        """Identify all players tied for the minimum and maximum scores."""
        lowest, highest = min(scores), max(scores)
        lowest_players = tuple(i for i, score in enumerate(scores) if score == lowest)
        highest_players = tuple(i for i, score in enumerate(scores) if score == highest)
        return self._round_end_cached(lowest_players, highest_players)

    def _round_end(self, lowest_players, highest_players):
        """Handle replays, elimination, and the transition to the next round."""
        n = self.n
        if len(lowest_players) == n:
            return self.restart_value

        result = [0] * n
        if n == 2:
            result[highest_players[0]] = 1
            return tuple(result)

        # Resolve ties using equal probabilities for each tied player.
        weight_out = 1 / len(lowest_players)
        weight = weight_out / len(highest_players)
        for out in lowest_players:
            for starter in highest_players:
                # Rotate the playing order to the next starter and remove the eliminated player.
                order = [(starter + offset) % n for offset in range(n)]
                order.remove(out)
                # Map the (n-1)-player win probabilities back to the original player positions.
                for position, player in enumerate(order):
                    result[player] += weight * self.previous[position]

        return tuple(result)


def solve_all(max_players=MAX_PLAYERS):
    """Solve games sequentially, using the previous player count's result."""
    results = {}
    solvers = {}
    for n in range(2, max_players + 1):
        print(f"Solving {n}-player game...", flush=True)
        solver = GameSolver(n, previous=results.get(n - 1))
        results[n] = solver.solve()
        solvers[n] = solver

    return results, solvers


if __name__ == "__main__":
    solve_all()