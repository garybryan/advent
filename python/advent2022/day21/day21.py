import functools
import operator
from typing import Callable, Iterable, NamedTuple, cast

import sympy

"""
This feels like a dependency graph resolution, or evaluation by an interpreter:
evaluate "root" by recursively evaluating its subparts.

We'll want to memoise already-resolved subparts.

Part 2:
Adds a couple of exceptions to the rules.
Root should check for equality, rather than addition (or whatever operation).
Humn's number should be ignored, and instead we need to solve for it, with the
knowledge that the two numbers for root are equal.
If we resolve these two numbers, we should be able to solve for the humn value,
as we'll end up resolving everything else and so having an equation involving
humn and numbers (such as humn - 5 = 10, so humn = 15).

How do we solve the equation? Doing it manually doesn't seem easy, but there's
a symbolic computation library to do that kind of thing, Sympy.
"""

OPERATORS = {
    "+": operator.add,
    "-": operator.sub,
    "*": operator.mul,
    "/": operator.truediv,
}

Operand = int | sympy.Expr | sympy.Eq

Operator = Callable[[Operand, Operand], Operand]


class Operation(NamedTuple):
    op: Operator
    key_1: str
    key_2: str


StateValue = int | Operation

StateEntry = tuple[str, StateValue]

State = dict[str, StateValue]


def parse_line(line: str) -> StateEntry:
    line_parts = line.split()

    try:
        name = line_parts[0][:-1]

        if len(line_parts) == 2:
            value = int(line_parts[1])
        else:
            value = Operation(OPERATORS[line_parts[2]], line_parts[1], line_parts[3])

    except (IndexError, ValueError):
        raise ValueError(f"Unable to parse line: {line}")

    return (name, value)


def init_state(entries: Iterable[StateEntry]) -> State:
    return dict(entries)


def resolve_state(state: State) -> int:
    """
    Recursively evaluate the value for the given entry.
    Uses functools.cache for memoization of repeated calls.
    """

    @functools.cache
    def resolve(from_entry: str = "root") -> Operand:
        entry = state[from_entry]

        if isinstance(entry, int):
            return entry

        return entry.op(resolve(entry.key_1), resolve(entry.key_2))

    return int(cast(float, resolve()))


def resolve_state_part_2(state: State) -> Operand:
    """
    Handle the special cases for root and humn for Part 2, and otherwise
    resolve in the same way as resolve_state.

    The special cases are handled with Sympy: humn is a Symbol, and root
    is an Eq.
    """

    @functools.cache
    def resolve(from_entry: str = "root") -> Operand:
        entry = state[from_entry]

        if from_entry == "humn":
            return sympy.Symbol("humn")

        if isinstance(entry, int):
            return entry

        if from_entry == "root":
            op = sympy.Eq
        else:
            op = entry.op

        return op(resolve(entry.key_1), resolve(entry.key_2))

    return round(sympy.nsolve(resolve(), 0))


def part1(filename: str = "data.txt") -> None:
    with open(filename) as f:
        state = init_state(map(parse_line, f))

    result = resolve_state(state)
    print(f"Part 1: root number: {result}")


def part2(filename: str = "data.txt") -> None:
    with open(filename) as f:
        state = init_state(map(parse_line, f))

    result = resolve_state_part_2(state)
    print(f"Part 2: solved for humn: {result}")


if __name__ == "__main__":
    part1()
    part2()
