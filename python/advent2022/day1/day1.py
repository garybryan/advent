import heapq
from itertools import islice
from typing import Iterable, Iterator


def calories_per_elf(lines: Iterable[str]) -> Iterator[int]:
    elf_total = 0

    for line in lines:
        try:
            elf_total += int(line)
        except ValueError:
            yield elf_total
            elf_total = 0

    yield elf_total


def max_calories(lines: Iterable[str]) -> int:
    return max(calories_per_elf(lines))


def k_max(k: int, nums: Iterator[int]) -> list[int]:
    heap = [next(nums) for _ in range(k)]
    heapq.heapify(heap)

    for num in nums:
        if num > heap[0]:
            heapq.heappushpop(heap, num)

    return heap


def sum_calories_k_elves(k: int, lines: Iterable[str]) -> int:
    return sum(k_max(k, calories_per_elf(lines)))


def main() -> None:
    with open("data.txt") as f:
        print("Part 1", max_calories(f))

    with open("data.txt") as f:
        print("Part 2", sum_calories_k_elves(3, f))


if __name__ == "__main__":
    main()
