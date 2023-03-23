import re
from typing import Literal

Direction = Literal["R", "L"]
Move = str | int


def parse_path(path: str) -> list[Move]:
    parts = re.findall(r"(\d+|[RL])", path)

    def parse_part(part: str) -> Move:
        if part.isnumeric():
            return int(part)
        return part

    return list(map(parse_part, parts))
