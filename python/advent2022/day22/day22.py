import re
from enum import Enum
from typing import Literal, NamedTuple, cast

TurnDirection = Literal["R", "L"]

Move = str | int

Tile = Literal[".", "#"] | None


class FacingDirection(Enum):
    R = 0
    D = 1
    L = 2
    U = 3


class Position(NamedTuple):
    x: int
    y: int
    facing: FacingDirection


def parse_path(path_text: str) -> list[Move]:
    parts = re.findall(r"(\d+|[RL])", path_text)

    def parse_part(part: str) -> Move:
        if part.isnumeric():
            return int(part)
        return part

    return list(map(parse_part, parts))


def parse_map(map_lines: list[str]) -> list[list[Tile]]:
    def parse_char(char: str) -> Tile:
        if char == " ":
            return None

        if char in (".", "#"):
            return char

        raise ValueError(f"Invalid map character: {char}")

    def parse_line(line: str) -> list[Tile]:
        return cast(list[Tile], list(map(parse_char, line)))

    return list(map(parse_line, map_lines))


def turn(position: Position, direction: TurnDirection) -> Position:
    turn_addend = 1 if direction == "R" else -1
    new_direction = FacingDirection((position.facing.value + turn_addend) % 4)
    return Position(position.x, position.y, new_direction)
