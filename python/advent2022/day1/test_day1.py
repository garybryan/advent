import pytest
from day1 import calories_per_elf, k_max, max_calories, sum_calories_k_elves


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


@pytest.fixture
def per_elf():
    return [6000, 4000, 11000, 24000, 10000]


def test_calories_per_elf(lines, per_elf):
    assert list(calories_per_elf(lines)) == per_elf


def test_max_calories(lines):
    assert max_calories(lines) == 24000


def test_k_max(per_elf):
    assert set(k_max(3, per_elf)) == {10000, 11000, 24000}
    assert set(k_max(2, per_elf)) == {11000, 24000}


def test_sum_calories_k_elves(lines):
    assert sum_calories_k_elves(3, lines) == 45000
