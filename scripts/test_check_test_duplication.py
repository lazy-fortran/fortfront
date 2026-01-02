from __future__ import annotations

import unittest

from scripts import check_test_duplication


class TestCheckTestDuplication(unittest.TestCase):
    def test_flags_inline_source_assignment(self) -> None:
        blocks = check_test_duplication._statements_with_newlines(
            ["source = 'a' // new_line('a') // 'b'"]
        )
        self.assertEqual(len(blocks), 1)
        self.assertEqual(blocks[0], check_test_duplication.InlineBlock(start_line=1, newline_count=1))

    def test_ignores_output_assertion_newlines(self) -> None:
        blocks = check_test_duplication._statements_with_newlines(
            ["has_stmt = index(output_code, new_line('a')//'x'//new_line('a')) > 0"]
        )
        self.assertEqual(blocks, [])

    def test_flags_multiline_source_assignment(self) -> None:
        blocks = check_test_duplication._statements_with_newlines(
            [
                "source1 = 'a' // new_line('a') // &",
                "          'b' // new_line('a') // &",
                "          'c'",
            ]
        )
        self.assertEqual(len(blocks), 1)
        self.assertEqual(blocks[0], check_test_duplication.InlineBlock(start_line=1, newline_count=2))

    def test_ignores_non_source_builder(self) -> None:
        blocks = check_test_duplication._statements_with_newlines(
            ["if (len(content) > 0) content = content // new_line('a')"]
        )
        self.assertEqual(blocks, [])


if __name__ == "__main__":
    unittest.main()

