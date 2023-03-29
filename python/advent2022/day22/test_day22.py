import pytest
from day22 import Facing, Position, advance, parse_board, parse_path, turn


@pytest.fixture
def path_text():
    return "10R5L5R10L4R5L5"


@pytest.fixture
def board_lines():
    return [
        "        ...#",
        "        .#..",
        "        #...",
        "        ....",
        "...#.......#",
        "........#...",
        "..#....#....",
        "..........#.",
        "        ...#....",
        "        .....#..",
        "        .#......",
        "        ......#.",
    ]


def test_parse_path(path_text):
    assert parse_path(path_text) == [
        10,
        "R",
        5,
        "L",
        5,
        "R",
        10,
        "L",
        4,
        "R",
        5,
        "L",
        5,
    ]


def test_parse_board(board_lines):
    assert parse_board(board_lines) == [
        [
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            ".",
            ".",
            ".",
            "#",
        ],
        [
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            ".",
            "#",
            ".",
            ".",
        ],
        [
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            "#",
            ".",
            ".",
            ".",
        ],
        [
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            ".",
            ".",
            ".",
            ".",
        ],
        [
            ".",
            ".",
            ".",
            "#",
            ".",
            ".",
            ".",
            ".",
            ".",
            ".",
            ".",
            "#",
        ],
        [
            ".",
            ".",
            ".",
            ".",
            ".",
            ".",
            ".",
            ".",
            "#",
            ".",
            ".",
            ".",
        ],
        [
            ".",
            ".",
            "#",
            ".",
            ".",
            ".",
            ".",
            "#",
            ".",
            ".",
            ".",
            ".",
        ],
        [
            ".",
            ".",
            ".",
            ".",
            ".",
            ".",
            ".",
            ".",
            ".",
            ".",
            "#",
            ".",
        ],
        [
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            ".",
            ".",
            ".",
            "#",
            ".",
            ".",
            ".",
            ".",
        ],
        [
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            ".",
            ".",
            ".",
            ".",
            ".",
            "#",
            ".",
            ".",
        ],
        [
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            ".",
            "#",
            ".",
            ".",
            ".",
            ".",
            ".",
            ".",
        ],
        [
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            ".",
            ".",
            ".",
            ".",
            ".",
            ".",
            "#",
            ".",
        ],
    ]


@pytest.fixture
def board(board_lines):
    return parse_board(board_lines)


def test_turn():
    position = Position(4, 6, Facing.R)

    assert turn(position, "R") == Position(4, 6, Facing.D)
    assert turn(position, "L") == (4, 6, Facing.U)


def test_advance_simple(board):
    assert advance(board, Position(5, 1, Facing.D), 2) == Position(7, 1, Facing.D)
    assert advance(board, Position(5, 1, Facing.R), 2) == Position(5, 3, Facing.R)
    assert advance(board, Position(7, 5, Facing.U), 2) == Position(5, 5, Facing.U)
    assert advance(board, Position(5, 3, Facing.L), 2) == Position(5, 1, Facing.L)


def test_advance_obstacle(board):
    assert advance(board, Position(4, 0, Facing.R), 4) == Position(4, 2, Facing.R)
    assert advance(board, Position(7, 3, Facing.U), 8) == Position(5, 3, Facing.U)


def test_advance_wrap(board):
    assert advance(board, Position(4, 0, Facing.U), 2) == Position(6, 0, Facing.U)
    assert advance(board, Position(4, 0, Facing.D), 6) == Position(6, 0, Facing.D)
    assert advance(board, Position(5, 0, Facing.L), 6) == Position(5, 9, Facing.L)
    assert advance(board, Position(5, 9, Facing.R), 6) == Position(5, 3, Facing.R)
