#!/usr/bin/env python3
import hashlib
import re
import sys
from pathlib import Path

# --- Configuration ---
# Adjust these paths if your setup differs
HOME = Path.home()
REPO_DIR = HOME / ".config/emacs/elpaca/repos"
CUSTOM_FILE = Path(
    "custom.el"
)  # Assumes script is run in same dir, or change to absolute path

# The marker that separates the auto-generated code from your manual footer
FOOTER_MARKER = ";;; Themes:"


def calculate_sha256(filepath: Path) -> str:
    """Calculates the sha256 hex digest of a file."""
    sha256 = hashlib.sha256()
    try:
        with open(filepath, "rb") as f:
            while chunk := f.read(8192):
                sha256.update(chunk)
        return sha256.hexdigest()
    except (OSError, PermissionError):
        return None


def find_elisp_files_map(root_dir: Path) -> dict[str, Path]:
    """
    Recursively finds all .el files in root_dir.
    Returns a dict mapping {sha256_hash: relative_path}.
    """
    print(f"Scanning {root_dir} for .el files...")
    sha_map = {}

    # rglob is equivalent to `fd` or `find`
    for path in root_dir.rglob("*.el"):
        if path.is_file():
            sha = calculate_sha256(path)
            if sha:
                # Store path relative to HOME for cleaner output
                try:
                    rel_path = path.relative_to(HOME)
                except ValueError:
                    rel_path = path
                sha_map[sha] = rel_path

    return sha_map


def extract_theme_shas(content: str) -> list[str]:
    """
    Finds all 64-character hex strings inside the content.
    We target the 'custom-safe-themes list specifically.
    """
    # Regex for a 64-char hex string surrounded by double quotes
    pattern = re.compile(r'"([a-f0-9]{64})"')

    # Find all matches (returns the captured group only)
    return pattern.findall(content)


def main():
    if not CUSTOM_FILE.exists():
        print(f"Error: Could not find {CUSTOM_FILE}")
        sys.exit(1)

    # 1. Read the existing custom.el
    original_content = CUSTOM_FILE.read_text(encoding="utf-8")

    # 2. Split file at the footer marker
    if FOOTER_MARKER in original_content:
        # Keep everything before the marker
        base_content = original_content.split(FOOTER_MARKER)[0].rstrip()
    else:
        # No marker found, assume whole file is the base
        base_content = original_content.rstrip()

    # 3. Find SHAs currently active in the config part (ignoring the old footer)
    active_shas = extract_theme_shas(base_content)

    if not active_shas:
        print("No theme SHAs found in custom-set-variables.")
        sys.exit(0)

    # 4. Build the map of filesystem SHAs
    # (We only do this expensive step if we actually found SHAs in the config)
    fs_sha_map = find_elisp_files_map(REPO_DIR)

    # 5. Generate the new footer
    new_footer_lines = ["\n\n" + FOOTER_MARKER]

    for sha in active_shas:
        # Look up the path, default to "???" if not found
        path_str = str(fs_sha_map.get(sha, "???"))
        new_footer_lines.append(f";; {sha}  {path_str}")

    # Add a final newline for POSIX compliance
    new_footer_lines.append("")
    new_footer = "\n".join(new_footer_lines)

    # 6. Reassemble
    new_full_content = base_content + new_footer

    # 7. Write only if changed
    if new_full_content != original_content:
        CUSTOM_FILE.write_text(new_full_content, encoding="utf-8")
        print(f"Updated {CUSTOM_FILE} with {len(active_shas)} theme annotations.")
    else:
        print(f"{CUSTOM_FILE} is already up to date.")


if __name__ == "__main__":
    main()
