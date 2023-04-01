import re
from enum import Enum
from functools import partial, reduce
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

        raise ValueError(f"Invalid map character: {repr(char)}")

    def parse_line(line: str) -> list[Tile]:
        return cast(list[Tile], list(map(parse_char, line)))

    return list(map(parse_line, board_lines))


def parse_lines(lines: list[str]) -> tuple[Board, Path]:
    # Assumes whole file is read into memory, which is fine for the size here.
    board_lines, path_line = lines[:-2], lines[-1]
    return parse_board(board_lines), parse_path(path_line)


def turn(position: Position, direction: TurnDir) -> Position:
    turn_addend = 1 if direction == TurnDir.R else -1
    new_direction = Facing((position.facing.value + turn_addend) % 4)
    return Position(position.y, position.x, new_direction)


ADVANCE_VECTORS: dict[Facing, tuple[int, int]] = {
    Facing.R: (0, 1),
    Facing.D: (1, 0),
    Facing.L: (0, -1),
    Facing.U: (-1, 0),
}


def wrap(board: Board, position: Position) -> Position:
    """
    Wrap around the board in the given direction if the given position is past
    a board edge or inside edge.
    """
    y, x, facing = position
    dy, dx = ADVANCE_VECTORS[facing]

    bottom_edge = len(board) - 1

    while True:
        if y < 0:
            y = bottom_edge
        elif y > bottom_edge:
            y = 0

        right_edge = len(board[y]) - 1

        # Detail that caught me out: if moving up or down to a row shorter than the
        # starting row, that row needs to be skipped as it is an inside edge.
        # Without this x <= right_edge check, it was causing a horizontal wrap
        # in this case.
        if dy == 0 or x <= right_edge:
            if x < 0:
                x = right_edge
            elif x > right_edge:
                x = 0

            if board[y][x] is not None:
                break

        y, x = y + dy, x + dx

    return Position(y, x, facing)


def advance(board: Board, position: Position, count: int) -> Position:
    """
    Advance as far as possible in the current direction by `count`.
    Stop moving if a wall is hit.

    Potential optimisation: pre-compute data about walls and edges,
    allowing moves in O(1) time rather than O(count). Will do if the final
    solution is slow.
    """
    if count < 0:
        raise ValueError("Cannot advance by a negative number")

    dy, dx = ADVANCE_VECTORS[position.facing]

    for _ in range(count):
        next_position = wrap(
            board, Position(position.y + dy, position.x + dx, position.facing)
        )

        if board[next_position.y][next_position.x] == "#":
            return position

        position = next_position

    return position


def make_move(board: Board, position: Position, move: Move) -> Position:
    if isinstance(move, int):
        return advance(board, position, move)
    return turn(position, move)


def follow_path(board: Board, path: Path, position: Position | None = None) -> Position:
    if position is None:
        position = get_initial_position(board)

    if position is None:
        raise ValueError("Board has no initial position")

    return reduce(partial(make_move, board), path, position)


def position_password(position: Position) -> int:
    return 1000 * (position.y + 1) + 4 * (position.x + 1) + position.facing.value


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


def parse_file(filename: str) -> tuple[Board, Path]:
    with open(filename) as f:
        return parse_lines(f.read().splitlines())


def part1(filename: str = "data.txt") -> None:
    board, path = parse_file(filename)
    password = position_password(follow_path(board, path))
    print("Final position password", password)


if __name__ == "__main__":
    part1()
