import re
from typing import Literal, cast

Direction = Literal["R", "L"]
Move = str | int

Tile = Literal[".", "#"] | None


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
