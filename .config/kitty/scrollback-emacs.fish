#!/usr/bin/env fish
# Opens kitty scrollback in a running Emacs frame via emacsclient.
# Kitty sets: KITTY_PIPE_DATA={scrolled_by}:{cursor_x},{cursor_y}:{lines},{columns}

# --- Parse KITTY_PIPE_DATA ---
set _parts (string split ':' -- $KITTY_PIPE_DATA)
set _cursor (string split ',' -- $_parts[2])
set _screen (string split ',' -- $_parts[3])

set cursor_x $_cursor[1] # 1-based
set cursor_y $_cursor[2] # 1-based, relative to visible screen
set screen_lines $_screen[1]

# --- Capture scrollback ---
set tmpfile /tmp/kitty-scrollback.txt
cat >$tmpfile

# --- Compute absolute cursor line in the buffer ---
set total_lines (wc -l <$tmpfile | string trim)
set target_line (math "$total_lines - $screen_lines + $cursor_y")

# forward-line and move-to-column are both 0-based, cursor_x/target_line are 1-based
# set fl (math "$target_line - 1")
set fl $target_line # off-by-one irl
set mc (math "$cursor_x - 1")

# --- Open in Emacs ---
emacsclient -nw --eval \
    "(progn
; fast enough to do every time
(add-to-list 'revert-without-query \".*kitty-scrollback.*\")
(find-file \"$tmpfile\")
(rename-buffer \"*kitty-scrollback*\" t)

(read-only-mode -1)  ; in case it already exists
(xterm-color-colorize-buffer)
(set-buffer-modified-p nil)

(goto-char (point-min))
(forward-line $fl)
(move-to-column $mc)

(toggle-menu-bar-mode-from-frame -1)
(display-line-numbers-mode -1)
(view-mode 1)
(recenter -1)

(general-def :keymaps 'local \"q\" (lambda ()
(interactive)
(toggle-menu-bar-mode-from-frame 1)
(kill-buffer)
(save-buffers-kill-terminal))))"

rm -f $tmpfile
rm -f $tmpfile
rm -f $tmpfile
