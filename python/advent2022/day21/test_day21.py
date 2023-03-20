import operator

import pytest
from day21 import init_state, parse_line, resolve_state, resolve_state_part_2


@pytest.fixture
def lines():
    return """root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32""".split(
        "\n"
    )


@pytest.fixture
def parsed_lines(lines):
    return list(map(parse_line, lines))


@pytest.fixture
def init_state_small():
    return init_state(
        map(
            parse_line,
            [
                "root: abcd + efgh",
                "abcd: 2",
                "efgh: 5",
            ],
        )
    )


@pytest.fixture
def init_state_full(parsed_lines):
    return init_state(parsed_lines)


def test_parse_line_number():
    assert parse_line("zczc: 2") == ("zczc", 2)


def test_parse_line_op():
    assert parse_line("root: pppw + sjmn") == ("root", (operator.add, "pppw", "sjmn"))


def test_init_state(init_state_small):
    assert init_state_small == {
        "root": (operator.add, "abcd", "efgh"),
        "abcd": 2,
        "efgh": 5,
    }


def test_resolve_state(init_state_small):
    assert resolve_state(init_state_small) == 7


def test_resolve_state_full(init_state_full):
    assert resolve_state(init_state_full) == 152


def test_resolve_state_part_2(init_state_full):
    assert resolve_state_part_2(init_state_full) == 301
