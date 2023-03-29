import re
from enum import Enum
from typing import Literal, NamedTuple, cast

TurnDirection = Literal["R", "L"]

Move = str | int

Tile = Literal[".", "#"] | None

Board = list[list[Tile]]


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


def parse_board(board_lines: list[str]) -> Board:
    def parse_char(char: str) -> Tile:
        if char == " ":
            return None

        if char in (".", "#"):
            return char

        raise ValueError(f"Invalid map character: {char}")

    def parse_line(line: str) -> list[Tile]:
        return cast(list[Tile], list(map(parse_char, line)))

    return list(map(parse_line, board_lines))


def turn(position: Position, direction: TurnDirection) -> Position:
    turn_addend = 1 if direction == "R" else -1
    new_direction = FacingDirection((position.facing.value + turn_addend) % 4)
    return Position(position.x, position.y, new_direction)


ADVANCE_VECTORS: dict[FacingDirection, tuple[int, int]] = {
    FacingDirection.R: (1, 0),
    FacingDirection.D: (0, 1),
    FacingDirection.L: (-1, 0),
    FacingDirection.U: (0, -1),
}


def advance_one(board: Board, position: Position) -> Position:
    dx, dy = ADVANCE_VECTORS[position.facing]
    return Position(position.x + dx, position.y + dy, position.facing)


def advance(board: Board, position: Position, count: int) -> Position:
    if count < 0:
        raise ValueError("Cannot advance by a negative number")

    for _ in range(count):
        position = advance_one(board, position)

    return position
