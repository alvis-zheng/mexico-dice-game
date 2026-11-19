from pathlib import Path
import gc
import pyarrow as pa
import pyarrow.parquet as pq
import optimal_dp as dp


MAX_ROLLS = 3
MAX_PLAYERS = 8
OUTPUT_DIR = Path("results") / "optimal_winning_strategy"


SCHEMA = pa.schema([
    ("n", pa.int8()), ("r", pa.int8()), ("L", pa.int8()),
    ("H", pa.list_(pa.int8())), ("s", pa.int8()), ("i", pa.int8()),
    ("p_stick", pa.float32()), ("p_reroll", pa.float32()),
    ("strategy", pa.string()),
])


def strategy_rows(solver):
    for key, action in solver.strategy.items():
        if key[0] == "starter":
            _, score, used = key
            position, limit, history = 0, dp.MAX_ROLLS, []
            scores = (score,) + (None,) * (solver.n - 1)
            p_stick = solver.next_player(1, scores, used)[0]
            p_reroll = (solver.starter_reroll_value(used)[0] if used < limit else None)
            actual_scores = [score]
        else:
            _, position, score, used, limit, scores = key
            played = scores[:position]
            low, high = min(played), max(played)
            # In the exported history, 67 represents scores strictly between the current minimum and maximum.
            history = [x if x in (low, high) else 67 for x in played]
            updated = list(scores)
            updated[position] = score
            
            if position == solver.n - 1:
                p_stick = solver.round_end(tuple(updated))[position]
                actual_scores = [
                    x for x, _ in dp.OUTCOMES
                    if dp.compress_last_score(x, scores) == score
                ]
            else:
                p_stick = solver.next_player(position + 1, tuple(updated), limit)[position]
                actual_scores = [score]
            p_reroll = (
                solver.reroll_value(position, used, limit, scores)[position]
                if used < limit else None
            )

        for actual_score in actual_scores:
            yield {
                "n": solver.n, "r": limit - used, "L": limit,
                "H": history, "s": actual_score, "i": position + 1,
                "p_stick": p_stick, "p_reroll": p_reroll,
                "strategy": action.upper(),
            }


def save_strategy(solver, path):
    count, batch = 0, []
    with pq.ParquetWriter(path, SCHEMA, compression="snappy") as writer:
        for row in strategy_rows(solver):
            batch.append(row)
            if len(batch) == 100000:
                writer.write_table(pa.Table.from_pylist(batch, schema=SCHEMA))
                count += len(batch)
                batch.clear()
        if batch:
            writer.write_table(pa.Table.from_pylist(batch, schema=SCHEMA))
            count += len(batch)
    return count


def main():
    dp.MAX_ROLLS = MAX_ROLLS
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    previous = None
    for n in range(2, MAX_PLAYERS + 1):
        print(f"Solving {n}-player game (maximum {MAX_ROLLS} rolls)...", flush=True)
        solver = dp.GameSolver(n, previous=previous)
        previous = solver.solve()
        assert abs(sum(previous) - 1) < 1e-12
        path = OUTPUT_DIR / f"optimal_winning_strategy_n{n}.parquet"
        count = save_strategy(solver, path)
        print(f"Saved {path}: {count} rows", flush=True)
        del solver
        gc.collect()


if __name__ == "__main__":
    main()