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


def main() -> None:
    with open("data.txt") as f:
        print("Part 1", max_calories(f))


if __name__ == "__main__":
    main()
