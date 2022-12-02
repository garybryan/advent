import pytest
from day1 import calories_per_elf, max_calories


@pytest.fixture
def lines():
    return iter(
        [
            "1000",
            "2000",
            "3000",
            "",
            "4000",
            "",
            "5000",
            "6000",
            "",
            "7000",
            "8000",
            "9000",
            "",
            "10000",
        ]
    )


def test_calories_per_elf(lines):
    assert list(calories_per_elf(lines)) == [6000, 4000, 11000, 24000, 10000]


def test_max_calories(lines):
    assert max_calories(lines) == 24000
