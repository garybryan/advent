import sys
import time
from contextlib import contextmanager
from dataclasses import dataclass
from typing import Iterable, Iterator, Optional, Self

"""
  The decryption sounds quite simple, but the trick is going to be doing it
  efficiently without having to shift around a big chunk of an array for each
  operation.

  A quick check on the input shows that duplicates are possible, which means
  no shortcuts (a mapping of unique items to indices would have made life
  easy!).

  Intuitively, using a linked list structure already seems like an improvement
  over an array, as shifting isn't needed; just remove the element and splice
  it in at the appropriate index. And since we're iterating through the whole
  list anyway, we shouldn't need to incur an extra cost to find each element.

  Since numbers can move backwards or forwards an arbitrary number of times,
  a doubly-linked-list will make sense.

  Using a linked list will however mean linear-time access to the target
  elements, while an array would give constant-time.

  I had the idea to use another data structure, like binary or 2-3-finger trees
  (Haskell's Data.Sequence). Sequence allows log-time positional access and
  changing, which beats a linked list for access and an array for removal and
  insertion. Sounds ideal...

  But there's always a but... The elements need to be processed in their
  original order, meaning we'd need to keep track of their current positions as
  they move around. Since we can't manipulate items in the data structure
  directly, this would require linear-time scans of the list, defeating any
  efficiency gain from the Sequence, or some complicated logic to keep track of
  them.

  So since we need to manipulate the structure directly, and need to move items
  in relative terms rather than absolute (to avoid also tracking indices), back
  to the doubly-linked list plan since I don't fancy implementing a 2-3 finger
  tree manually. And I'm doing this one in Python, because implementing
  pointer-based data structures in Haskell isn't much fun and isn't really what
  it's designed for.

  To process the numbers in original order, keep a queue of list items
  separately from the list itself.
"""


@dataclass
class ListItem:
    value: int
    prev: Self
    next: Self

    def __init__(
        self, value: int, prev: Optional[Self] = None, next: Optional[Self] = None
    ):
        self.value = value
        self.prev = prev or self
        self.next = next or self

    @classmethod
    def from_values(cls, values: Iterable[int]) -> Optional[Self]:
        """
        Make a circular doubly-linked list from values.
        """
        values_iter = iter(values)

        try:
            first_value = next(values_iter)
        except StopIteration:
            return None

        head = cls(first_value)
        cur = head

        for value in values_iter:
            prev = cur

            cur = ListItem(value=value, prev=prev, next=head)
            prev.next = cur

        head.prev = cur

        return head

    def __str__(self) -> str:
        return str(self.value)

    @property
    def ordered_items(self) -> Iterator[Self]:
        cur = self

        while True:
            yield cur

            cur = cur.next

            if cur == self:
                break

    def __iter__(self) -> Iterator[int]:
        for item in self.ordered_items:
            yield item.value

    @property
    def zero_item(self) -> Optional[Self]:
        return next((item for item in self.ordered_items if item.value == 0), None)

    def find_item_at(self, offset: int) -> Self:
        is_backwards = offset < 0
        abs_offset = abs(offset)

        cur = self

        while abs_offset and cur:
            cur = cur.prev if is_backwards else cur.next
            abs_offset -= 1

        return cur

    def move(self, length: int) -> None:
        """
        Move this item forwards or backwards in the list:
            - Exit early if not moving (offset is zero).
            - Find the item at the offset.
            - Remove this item from its current position by connecting its prev
              and next items together.
            - Insert it at the offset by putting it after the item currently at
              the offset.

        Length is passed so that we can use modulo (minus one, to avoid
        counting this item which will be removed) to avoid going around in
        multiple circles. This also avoids having to subtract an extra one for
        backwards moves, by making all moves go forwards.
        """
        offset = self.value % (length - 1)

        if offset == 0:
            return

        # Optimisation: Can we go backwards in fewer steps?
        # If the offset is more than half of the length, then yes.
        if offset > (length - 1) // 2:
            offset -= length

        item_at_offset = self.find_item_at(offset)

        # Remove this item...
        self.prev.next, self.next.prev = self.next, self.prev

        # ... And put it after item_at_offset.
        self.next, item_at_offset.next.prev = item_at_offset.next, self
        item_at_offset.next, self.prev = self, item_at_offset

    def mix(self, length: int, items: list[Self]) -> Self:
        """
        Move each element in original order, and return the first zero item
        based on original order, if any.
        """
        for item in items:
            item.move(length)

    @property
    def coordinates(self) -> tuple[int]:
        def find_coordinates() -> Iterator[int]:
            cur = self
            for i in range(1, 3001):
                if cur:
                    cur = cur.next
                    if cur and i % 1000 == 0:
                        yield cur.value

        return tuple(find_coordinates())

    @property
    def coordinates_sum(self) -> int:
        return sum(self.coordinates)


def values_from_file(filename: str) -> Iterator[int]:
    with open(filename) as f:
        yield from map(int, f)


@contextmanager
def timer(message: Optional[str] = None):
    start_time = time.perf_counter()
    yield
    end_time = time.perf_counter()

    total_time = round(end_time - start_time, 4)

    output = "Time"
    if message:
        output += f" for {message}"
    output += f": {total_time} ms"

    print(output, file=sys.stderr)


def part1(filename: str = "data.txt") -> None:
    values = list(values_from_file(filename))

    head = ListItem.from_values(values)

    if not head:
        print("Cannot get coordinates for empty list")
        return

    ordered_items = list(head.ordered_items)
    length = len(ordered_items)

    with timer("mix"):
        head.mix(length, ordered_items)

    print(f"Part 1: Coordinates sum: {head.zero_item.coordinates_sum}")


def part2(filename: str = "data.txt") -> None:
    decryption_key = 811589153
    values = [value * decryption_key for value in values_from_file(filename)]

    head = ListItem.from_values(values)

    if not head:
        print("Cannot get coordinates for empty list")
        return

    ordered_items = list(head.ordered_items)
    length = len(ordered_items)

    for _ in range(10):
        head.mix(length, ordered_items)

    print(f"Part 2: Coordinates sum: {head.zero_item.coordinates_sum}")


if __name__ == "__main__":
    part1()
    part2()
