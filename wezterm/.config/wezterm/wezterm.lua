local wezterm = require 'wezterm'
local act = wezterm.action

function do_if_file_exists(name)
   local f=io.open(name,"r")
   if f~=nil then io.close(f) else return end
   -- require(name) results in "can't load C modules in safe
   -- mode" error
   wezterm.add_to_config_reload_watch_list(name); dofile(name)
end

do_if_file_exists(wezterm.config_dir.."/extkeys.lua")
do_if_file_exists(wezterm.config_dir.."/specific.lua")

keys = {
   -- for emacs undo, prefer super w/ same keys
   {key = "-", mods = "CTRL", action = "DisableDefaultAssignment"},
   {key = "_", mods = "CTRL|SHIFT", action = "DisableDefaultAssignment"},
   {key = "=", mods = "CTRL", action = "DisableDefaultAssignment"},
   {key = "+", mods = "CTRL|SHIFT", action = "DisableDefaultAssignment"},
   {key = "Enter", mods = "ALT", action = "DisableDefaultAssignment"},
   {
      key = "P",
      mods = "CTRL",
      action = wezterm.action.QuickSelectArgs {
         label = "open url",
         patterns = {
            'https?://\\S+',
            '\\bb/[0-9]+\\S*\\b',
            '\\bcl/[0-9]+\\S*\\b',
            '\\bgo/\\S+\\b',
            '\\bgoogle3/[\\w./-]+',
         },
         action = wezterm.action_callback(function(window, pane)
               local url = window:get_selection_text_for_pane(pane)
               if not string.match(url, '^https?://') then
                  url = 'http://' .. url
               end
               wezterm.log_info("opening: " .. url)
               wezterm.open_with(url)
         end),
      },
   },
}

-- emacs copy-mode binds
local search_mode = wezterm.gui.default_key_tables().search_mode
for _, v in pairs({
   { key = 's', mods = 'CTRL', action = act.CopyMode 'NextMatch' },
   { key = 'r', mods = 'CTRL', action = act.CopyMode 'PriorMatch'},
   { key = 'g', mods = 'CTRL', action = act.CopyMode 'Close' }}) do
   table.insert(search_mode, v)
end

-- to click links, need both shift (bypass_mouse_reporting_modifiers = true) +
-- ctrl (mouse_reporting = false). somehow both are needed in tmux (mouse mode)
-- + emacs (xterm-mouse-mode)
mouse_bindings = {
   {
      event = { Up = { streak = 1, button = 'Left' } },
      mods = 'CTRL',
      action = act.OpenLinkAtMouseCursor,
      -- allowing wezterm clicks to bypass tmux/emacs mouse captures:
      mouse_reporting = true,
   },
}

-- TODO: make these default
hyperlink_rules = {
   {
      regex = "\\bhttps?://\\S*\\b",
      format = "$0",
   },
   { -- This is actually the default if you don't specify any hyperlink_rules
      regex = "\\b\\w+://(?:[\\w.-]+)\\.[a-z]{2,15}\\S*\\b",
      format = "$0",
   },
   { -- linkify email addresses
      regex = "\\b\\w+@[\\w-]+(\\.[\\w-]+)+\\b",
      format = "mailto:$0",
   },
   { -- file:// URI
      regex = "\\bfile://\\S*\\b",
      format = "$0",
   }
}

-- https://stackoverflow.com/questions/1283388/lua-how-to-merge-two-tables-overwriting-the-elements-which-are-in-both
for k,v in pairs(keys) do keys_extended_shortcuts[k] = v end
for k,v in pairs(hyperlink_rules_specific) do hyperlink_rules[k] = v end

return {
   -- text & colours
   font = wezterm.font("JetBrains Mono"),
   font_size = font_size,
   color_scheme = "Gruvbox Dark",

   -- cursor
   default_cursor_style = 'BlinkingBar',
   animation_fps = 1,
   cursor_blink_ease_in = 'Constant',
   cursor_blink_ease_out = 'Constant',
   cursor_thickness = "1.4pt",

   -- keys and links
   keys = keys,
   key_tables = {search_mode = search_mode},
   bypass_mouse_reporting_modifiers = 'SHIFT',
   mouse_bindings = mouse_bindings,
   hyperlink_rules = hyperlink_rules,

   -- window
   enable_wayland = true,
   window_background_opacity = 0.9,
   hide_tab_bar_if_only_one_tab = true,
   window_close_confirmation = "NeverPrompt",
   exit_behavior = "Close",
   enable_scroll_bar = true,
}
