import pytest
from day22 import parse_path


@pytest.fixture
def path():
    return "10R5L5R10L4R5L5"


def test_parse_path(path):
    assert parse_path(path) == [10, "R", 5, "L", 5, "R", 10, "L", 4, "R", 5, "L", 5]
