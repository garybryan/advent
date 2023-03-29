import pytest
from day22 import (FacingDirection, Position, advance, parse_board, parse_path,
                   turn)


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
    position = Position(4, 6, FacingDirection.R)

    assert turn(position, "R") == Position(4, 6, FacingDirection.D)
    assert turn(position, "L") == (4, 6, FacingDirection.U)


def test_advance_simple(board):
    assert advance(board, Position(5, 1, FacingDirection.D), 2) == Position(
        7, 1, FacingDirection.D
    )
    assert advance(board, Position(5, 1, FacingDirection.R), 2) == Position(
        5, 3, FacingDirection.R
    )
    assert advance(board, Position(7, 5, FacingDirection.U), 2) == Position(
        5, 5, FacingDirection.U
    )
    assert advance(board, Position(5, 3, FacingDirection.L), 2) == Position(
        5, 1, FacingDirection.L
    )


def test_advance_obstacle(board):
    assert advance(board, Position(4, 0, FacingDirection.R), 4) == Position(
        4, 2, FacingDirection.R
    )
    assert advance(board, Position(7, 3, FacingDirection.U), 8) == Position(
        5, 3, FacingDirection.U
    )
