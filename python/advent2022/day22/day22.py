import re
from enum import Enum
from typing import Literal, NamedTuple, cast

TurnDir = Enum("Turn", ["R", "L"])

Move = TurnDir | int

Path = list[Move]

Tile = Literal[".", "#"] | None

Board = list[list[Tile]]


class Facing(Enum):
    R = 0
    D = 1
    L = 2
    U = 3


class Position(NamedTuple):
    y: int
    x: int
    facing: Facing


def parse_path(path_text: str) -> Path:
    parts = re.findall(r"(\d+|[RL])", path_text)

    def parse_part(part: str) -> Move:
        if part.isnumeric():
            return int(part)

        if part == "R":
            return TurnDir.R

        if part == "L":
            return TurnDir.L
        raise ValueError(f"Invalid path part: {part}")

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


def turn(position: Position, direction: TurnDir) -> Position:
    turn_addend = 1 if direction == "R" else -1
    new_direction = Facing((position.facing.value + turn_addend) % 4)
    return Position(position.y, position.x, new_direction)


ADVANCE_VECTORS: dict[Facing, tuple[int, int]] = {
    Facing.R: (0, 1),
    Facing.D: (1, 0),
    Facing.L: (0, -1),
    Facing.U: (-1, 0),
}


def advance_one(board: Board, position: Position) -> Position:
    dy, dx = ADVANCE_VECTORS[position.facing]
    new_y, new_x = position.y + dy, position.x + dx

    bottom_edge, right_edge = len(board) - 1, len(board[0]) - 1

    if new_y < 0:
        new_y = bottom_edge
    elif new_y > bottom_edge:
        new_y = 0

    if new_x < 0:
        new_x = right_edge
    elif new_x > right_edge:
        new_x = 0

    new_tile = board[new_y][new_x]

    if new_tile == "#":
        return position

    new_position = Position(new_y, new_x, position.facing)

    if new_tile is None:
        # Very inefficient, quick-and-dirty method to wrap at a wall!
        # Will improve later by pre-finding walls.
        return advance_one(board, new_position)

    return new_position


def advance(board: Board, position: Position, count: int) -> Position:
    """
    Advance as far as possible in the current direction by `count`.
    Stop moving if an obstacle is hit.

    Potential optimisation: pre-compute data about obstacles and board edges,
    allowing moves in O(1) time rather than O(count). Will do if the final
    solution is slow.
    """
    if count < 0:
        raise ValueError("Cannot advance by a negative number")

    for _ in range(count):
        new_position = advance_one(board, position)

        if new_position == position:
            return position

        position = new_position

    return position


def get_initial_position(board: Board) -> Position | None:
    """
    Get a position on the leftmost open tile on the top row of tiles, facing
    right.

    In both the test data and the real data, the top row always has tiles,
    so we could cheat, but let's do it properly for completeness.
    """

    def get_first_tile_x(row: list[Tile]) -> int | None:
        return next((x for x, tile in enumerate(row) if tile == "."), None)

    for y, row in enumerate(board):
        x = get_first_tile_x(row)

        if x is not None:
            return Position(y, x, Facing.R)
