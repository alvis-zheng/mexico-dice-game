"""Choose a stopping threshold K for each number of remaining players.
Opponents choose actions that minimise the target player's winning probability.
"""

from collections import Counter


DICE_COUNTS = Counter(
    10 * max(a, b) + min(a, b)
    for a in range(1, 7)
    for b in range(1, 7)
)
THRESHOLDS = sorted(DICE_COUNTS)
PROBABILITIES = [DICE_COUNTS[score] / 36 for score in THRESHOLDS]
SCORE_INDICES = range(len(THRESHOLDS))
BELOW = [sum(PROBABILITIES[:i]) for i in SCORE_INDICES]
ALL_EQUAL = -1
ROLL_CAPS = (1, 2, 3)


def transitions(history, next_states, last_player):
    """Map each possible kept score to a compressed next-state index."""
    row = []
    for score in SCORE_INDICES:
        kept = history + (score,)
        low, high = min(kept), max(kept)
        if low == high:
            row.append(ALL_EQUAL)
            continue

        if last_player:
            lowest = tuple(i for i, value in enumerate(kept) if value == low)
            highest = tuple(i for i, value in enumerate(kept) if value == high)
            state = (lowest, highest)

        else:
            # Intermediate scores are equivalent here; use one representative.
            state = tuple(
                value if value == low or value == high else low + 1
                for value in kept
            )
        row.append(next_states.setdefault(state, len(next_states)))
    return tuple(row)


def build_round(players):
    """Build score transitions once for all thresholds and opening positions."""
    mixed_rows = [[] for _ in range(players)]
    equal_rows = [[] for _ in range(players)]
    histories = []

    for position in range(1, players):
        next_states = {}
        last_player = position == players - 1
        mixed_rows[position] = [
            transitions(history, next_states, last_player)
            for history in histories
        ]

        # Handle histories in which all previous players have the same score.
        equal_rows[position] = [
            transitions((score,) * position, next_states, last_player)
            for score in SCORE_INDICES
        ]
        histories = next_states

    # Final states record the tied lowest and highest player groups.
    return mixed_rows, equal_rows, list(histories)


def round_end_values(groups, players, target_player, smaller_game_win_probs):
    """Look up the smaller game's win probability for each round outcome."""
    values = []
    # Average over tied players chosen for elimination and the next start.
    for lowest, highest in groups:
        total = 0
        for eliminated in lowest:
            if eliminated == target_player:
                continue
            for starter in highest:
                seat = (target_player - starter) % players
                if (eliminated - starter) % players < seat:
                    seat -= 1
                # Continue with the precomputed win probability for one fewer player.
                total += smaller_game_win_probs[seat]
        values.append(total / (len(lowest) * len(highest)))
    return values


def roll_mean(values):
    return sum(
        p * value
        for p, value in zip(PROBABILITIES, values)
    )


def after_roll(stick_values, reroll_value, threshold_index):
    """Average the chosen action over the score that was just rolled."""
    total = 0
    if threshold_index is None:
        # Opponents choose whichever action gives the target a lower win rate.
        for i in SCORE_INDICES:
            if stick_values[i] < reroll_value:
                total += PROBABILITIES[i] * stick_values[i]
            else:
                total += PROBABILITIES[i] * reroll_value

    else:
        for i in SCORE_INDICES:
            if i < threshold_index:
                total += PROBABILITIES[i] * reroll_value
            else:
                total += PROBABILITIES[i] * stick_values[i]

    return total


def turn_value(stick_values, limit, threshold_index):
    value = roll_mean(stick_values)
    for _ in range(1, limit):
        value = after_roll(stick_values, value, threshold_index)
    return value


def win_probability(players, target_player, threshold_index, smaller_game_win_probs, round_tables):
    """Evaluate K at the current player count from a given starting position."""
    mixed_rows, equal_rows, groups = round_tables
    end = round_end_values(groups, players, target_player, smaller_game_win_probs)
    # Other players' strategies are designed to minimize the target player's win rate.
    thresholds = [None] * players
    thresholds[target_player] = threshold_index

    # Once kept scores differ, the round cannot end in an all-equal replay.
    mixed_by_limit = []
    for limit in ROLL_CAPS:
        values = [[] for _ in range(players)]
        following = end  # Solve players' decisions backwards.
        for position in range(players - 1, 0, -1):
            values[position] = []
            for row in mixed_rows[position]:
                stick = [following[i] for i in row]
                result = turn_value(stick, limit, thresholds[position])
                values[position].append(result)
            following = values[position]
        mixed_by_limit.append(values)

    # An all-equal round restarts; solve its replay value by iteration.
    replay = 0
    for _ in range(100):
        after_starter = []
        for limit, mixed in zip(ROLL_CAPS, mixed_by_limit):
            equal = [replay] * len(THRESHOLDS)
            for position in range(players - 1, 0, -1):
                following = end if position == players - 1 else mixed[position + 1]
                new_equal = []

                for score, row in enumerate(equal_rows[position]):
                    stick = []
                    
                    for i in row:
                        if i == ALL_EQUAL:
                            stick.append(equal[score])
                        else:
                            stick.append(following[i])
                    result = turn_value(
                        stick,
                        limit,
                        thresholds[position]
                    )
                    new_equal.append(result)

                equal = new_equal
            after_starter.append(equal)

        # The first player's number of rolls sets every later player's roll cap.
        value = roll_mean(after_starter[2])
        value = after_roll(after_starter[1], value, thresholds[0])
        value = after_roll(after_starter[0], value, thresholds[0])
        if abs(value - replay) < 1e-12:
            return value
        replay = value

    raise RuntimeError("Replay iteration did not converge.")


def main():
    smaller_game_win_probs = [1.0]
    summary = []

    for players in range(2, 9):
        print(f"\nSolving n={players}", flush=True)
        round_tables = build_round(players)
        seat_values_by_k = {}
        rates = {}

        # All candidates use the same continuation values for one fewer player.
        for threshold_index, k in enumerate(THRESHOLDS):
            seat_values = [
                win_probability(
                    players, seat, threshold_index,
                    smaller_game_win_probs, round_tables
                )
                for seat in range(players)
            ]
            seat_values_by_k[k] = seat_values
            rates[k] = sum(seat_values) / players
            print(f"  K={k}  worst-case win rate {rates[k]:.6%}", flush=True)

        best_k = max(rates, key=rates.get)
        smaller_game_win_probs = seat_values_by_k[best_k]
        summary.append((players, best_k, rates[best_k]))

    print("\nBest threshold by number of remaining players")
    for players, k, rate in summary:
        print(f"  n={players}  K={k}  worst-case win rate {rate:.6%}")


if __name__ == "__main__":
    main()