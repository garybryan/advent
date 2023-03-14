import pytest
from day20 import ListItem


@pytest.fixture
def nums():
    return [1, 2, -3, 3, -2, 0, 4]


@pytest.fixture
def nums_circular(nums):
    return ListItem.from_values(nums)


@pytest.fixture
def length(nums):
    return len(nums)


@pytest.fixture
def ordered_items(nums_circular):
    return list(nums_circular.ordered_items)


def test_from_values_empty():
    assert ListItem.from_values([]) is None


def test_from_values(nums):
    head = ListItem.from_values(nums)

    assert head is not None

    assert list(head) == nums


def test_find_item_at_zero(nums_circular):
    assert nums_circular.find_item_at(0).value == nums_circular.value


def test_find_item_at_forwards(nums_circular):
    assert nums_circular.find_item_at(2).value == -3


def test_find_item_at_backwards(nums_circular):
    assert nums_circular.find_item_at(-3).value == -2


def test_move_zero(nums, nums_circular, length):
    item = nums_circular.find_item_at(5)

    item.move(length)

    assert list(nums_circular) == nums


def test_move_forwards(nums_circular, length):
    item = nums_circular.find_item_at(1)

    item.move(length)

    assert list(nums_circular) == [1, -3, 3, 2, -2, 0, 4]


def test_move_forwards_wrap(nums_circular, length):
    item = nums_circular.find_item_at(6)

    item.move(length)

    assert list(nums_circular) == [1, 2, -3, 3, 4, -2, 0]


def test_move_backwards(nums_circular, length):
    item = nums_circular.find_item_at(4)

    item.move(length)

    assert list(nums_circular) == [1, 2, -2, -3, 3, 0, 4]


def test_move_backwards_wrap(nums_circular, length):
    item = nums_circular.find_item_at(2)

    item.move(length)

    assert list(nums_circular) == [1, 2, 3, -2, 0, -3, 4]


def test_mix(nums_circular, length, ordered_items):
    nums_circular.mix(length, ordered_items)

    assert list(nums_circular.zero_item) == [0, 3, -2, 1, 2, -3, 4]


def test_coordinates(nums_circular, length, ordered_items):
    nums_circular.mix(length, ordered_items)

    assert nums_circular.zero_item.coordinates == (4, -3, 2)


def test_coordinates_sum(nums_circular, length, ordered_items):
    nums_circular.mix(length, ordered_items)

    assert nums_circular.zero_item.coordinates_sum == 3
