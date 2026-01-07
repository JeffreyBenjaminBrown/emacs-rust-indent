#!/usr/bin/env python3
"""
Parse tests-distilled.rs and generate/run Emacs indentation tests.
Usage: ./run-tests.py
"""

import subprocess
import sys
import re
from pathlib import Path

SCRIPT_DIR = Path(__file__).parent.resolve()
TESTS_FILE = SCRIPT_DIR / "tests-distilled.rs"

def parse_tests(filename):
    """Parse tests-distilled.rs and yield (name, ugly_lines, pretty_lines) tuples."""
    with open(filename) as f:
        lines = f.readlines()

    state = 'seeking-name'
    current_name = None
    ugly_lines = []
    pretty_lines = []

    i = 0
    while i < len(lines):
        line = lines[i].rstrip('\n')

        if state == 'seeking-name' and re.match(r'^///\s*$', line):
            state = 'reading-name'

        elif state == 'reading-name' and re.match(r'^/// (.+)$', line):
            current_name = re.match(r'^/// (.+)$', line).group(1)
            state = 'after-name'

        elif state == 'after-name' and re.match(r'^///\s*$', line):
            state = 'seeking-ugly'

        elif state in ('seeking-ugly', 'seeking-name') and re.match(r'^/// ugly\s*$', line):
            state = 'reading-ugly'
            ugly_lines = []

        elif state == 'reading-ugly' and re.match(r'^/// pretty\s*$', line):
            state = 'reading-pretty'
            pretty_lines = []

        elif state == 'reading-pretty' and re.match(r'^///\s*$', line):
            # End of this test, start of next
            if current_name and ugly_lines and pretty_lines:
                yield (current_name, ugly_lines, pretty_lines)
            current_name = None
            ugly_lines = []
            pretty_lines = []
            state = 'reading-name'

        elif state == 'reading-ugly':
            if not re.match(r'^///', line) and line.strip():
                ugly_lines.append(line)

        elif state == 'reading-pretty':
            # Skip // comment lines (explanatory)
            if re.match(r'^// ', line):
                pass
            elif not re.match(r'^///', line) and line.strip():
                pretty_lines.append(line)

        i += 1

    # Don't forget the last test
    if state == 'reading-pretty' and current_name and ugly_lines and pretty_lines:
        yield (current_name, ugly_lines, pretty_lines)

def elisp_string(s):
    """Escape a string for Emacs Lisp."""
    return '"' + s.replace('\\', '\\\\').replace('"', '\\"') + '"'

def generate_tests_el(tests):
    """Generate tests.el content from parsed tests."""
    lines = [
        ';;; tests.el --- Auto-generated tests -*- lexical-binding: t -*-',
        '',
        '(add-to-list \'load-path "/home/ubuntu/rust-indent")',
        '(require \'rust-mode)',
        '(require \'rust-prog-mode)',
        '(require \'rust-indent-fix)',
        '',
        '(setq rust-indent-offset 2)',
        '',
        '(defvar test-all-passed t)',
        '(defvar test-count 0)',
        '(defvar test-passed-count 0)',
        '',
        '(defun indent-code-string (code-string)',
        '  "Indent CODE-STRING using rust-mode and return the result."',
        '  (rust-indent-fix-enable)',
        '  (with-temp-buffer',
        '    (rust-mode)',
        '    (insert code-string)',
        '    (goto-char (point-min))',
        '    (while (not (eobp))',
        '      (funcall indent-line-function)',
        '      (forward-line 1))',
        '    (buffer-string)))',
        '',
        '(defun run-test (name ugly-code pretty-code)',
        '  "Run a single test."',
        '  (setq test-count (1+ test-count))',
        '  (let* ((result (indent-code-string ugly-code))',
        '         (pass (string= result pretty-code)))',
        '    (if pass',
        '        (progn',
        '          (setq test-passed-count (1+ test-passed-count))',
        '          (message "PASS: %s" name))',
        '      (setq test-all-passed nil)',
        '      (message "FAIL: %s" name)',
        '      (message "  Expected:")',
        '      (dolist (line (split-string pretty-code "\\n"))',
        '        (message "    |%s|" line))',
        '      (message "  Got:")',
        '      (dolist (line (split-string result "\\n"))',
        '        (message "    |%s|" line)))))',
        '',
        '(message "")',
        f'(message "=== Running {len(tests)} tests ===")',
        '(message "")',
        '',
    ]

    for name, ugly, pretty in tests:
        ugly_code = '\n'.join(ugly)
        pretty_code = '\n'.join(pretty)
        lines.append(f'(run-test {elisp_string(name)}')
        lines.append(f'  {elisp_string(ugly_code)}')
        lines.append(f'  {elisp_string(pretty_code)})')
        lines.append('')

    lines.extend([
        '(message "")',
        '(message "=== Results: %d/%d passed ===" test-passed-count test-count)',
        '(if test-all-passed',
        '    (progn',
        '      (message "=== ALL TESTS PASSED ===")',
        '      (kill-emacs 0))',
        '  (progn',
        '    (message "=== SOME TESTS FAILED ===")',
        '    (kill-emacs 1)))',
    ])

    return '\n'.join(lines)

def main():
    tests = list(parse_tests(TESTS_FILE))
    print(f"Parsed {len(tests)} tests from {TESTS_FILE.name}")

    elisp_code = generate_tests_el(tests)

    # Write tests.el so it can be inspected
    tests_el_path = SCRIPT_DIR / "tests.el"
    with open(tests_el_path, 'w') as f:
        f.write(elisp_code)
    print(f"Generated {tests_el_path}")

    # Run emacs with the generated file
    result = subprocess.run(
        ['emacs', '--batch', '-l', str(tests_el_path)],
        capture_output=True,
        text=True
    )

    print(result.stderr)
    if result.stdout:
        print(result.stdout)

    return result.returncode

if __name__ == '__main__':
    sys.exit(main())
