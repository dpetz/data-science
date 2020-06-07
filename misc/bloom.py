import random
import string
import pytest

# Given number of n strings each consisting of  k digits.
# Find the most frequent string
# ... in O(0) complexity
# ... when memory is constraint to 1000 numbers



# Each character is one of
string.ascii_lowercase




def count_naive(strings):
    return len(set(strings))


def random_string(string_length=10, letters="1234567890"):
    """Generate a random string of fixed length """
    return ''.join(random.choice(letters) for i in range(string_length))


def test_unit_random(n=5):
    # you can write to stdout for debugging purposes, e.g.
    rs = random_string(n)
    print(f"Random string length {n}: {rs}")
    assert len(rs) == 5


def test_random(k=3):
    n = pow(k, 6)
    strings = [random_string(k) for i in range(n)]
    c = count_naive(strings)
    print(f"{c} of {n} strings of length {k} are unique.")


def test_unit_count():
    assert count_naive(["a", "b", "a"]) == 2
