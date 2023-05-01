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
   -- emacs undo. C- don't need to be unbound, prefer s- prefix for WM
   {key = "_",         mods = "CTRL|SHIFT",     action = "DisableDefaultAssignment"},
   {key = "-",         mods = "CTRL|SHIFT",     action = "DisableDefaultAssignment"},
   {key = "-",         mods = "CTRL",           action = "DisableDefaultAssignment"},
   {key = "=",         mods = "CTRL",           action = "DisableDefaultAssignment"},
   -- emacs just-one-space
   {key = "Enter",     mods = "ALT",            action = "DisableDefaultAssignment"},
   -- panes
   {key = "UpArrow",   mods = "CTRL|SHIFT",     action = "DisableDefaultAssignment"},
   {key = "DownArrow", mods = "CTRL|SHIFT",     action = "DisableDefaultAssignment"},
   {key = "%",         mods = "SHIFT|ALT|CTRL", action = "DisableDefaultAssignment"},
   {key = "o",         mods = "SUPER",          action = act.ActivatePaneDirection 'Next',},
   {key = "i",         mods = "SUPER",          action = act.ActivatePaneDirection 'Prev',},
   {key = "-",         mods = "CTRL|SUPER",     action = act.SplitVertical{   domain = 'CurrentPaneDomain' },},
   {key = "\\",        mods = "CTRL|SUPER",     action = act.SplitHorizontal{ domain = 'CurrentPaneDomain' },},
   {key = 'z',         mods = 'CTRL|SUPER',     action = wezterm.action.TogglePaneZoomState,},
   {key = 'UpArrow',   mods = 'ALT|SUPER',      action = act.AdjustPaneSize { 'Up',   1 },},
   {key = 'DownArrow', mods = 'ALT|SUPER',      action = act.AdjustPaneSize { 'Down', 1 },},
   {key = 'UpArrow',   mods = 'CTRL|SUPER',     action = act.AdjustPaneSize { 'Up',   5 },},
   {key = 'DownArrow', mods = 'CTRL|SUPER',     action = act.AdjustPaneSize { 'Down', 5 },},

   -- copy mode, a la C-Space from tmux
   {key = "Space",     mods = "CTRL|SUPER",     action = act.ActivateCopyMode },

   {
      -- key = "P", pane select -- TODO: B??
      key = "B",
      mods = "CTRL",
      action = wezterm.action.QuickSelectArgs {
         label = "open url",
         patterns = {
            '\\b\\w+://[\\w.-]+\\.[a-z]{2,15}\\S*\\b',
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
local copy_mode = wezterm.gui.default_key_tables().copy_mode
for _, v in pairs({
      { key = 'n', mods = 'CTRL', action = act.CopyMode 'MoveDown' },
      { key = 'p', mods = 'CTRL', action = act.CopyMode 'MoveUp' },
      { key = 'f', mods = 'CTRL', action = act.CopyMode 'MoveRight' },
      { key = 'b', mods = 'CTRL', action = act.CopyMode 'MoveLeft' },
      { key = 'a', mods = 'CTRL', action = act.CopyMode 'MoveToStartOfLineContent' },
      { key = 'e', mods = 'CTRL', action = act.CopyMode 'MoveToEndOfLineContent' },
      { key = 'v', mods = 'CTRL', action = act.CopyMode 'PageDown' },
      { key = 'v', mods = 'ALT',  action = act.CopyMode 'PageUp' },
      { key = 'Space', mods = 'CTRL', action = act.CopyMode{ SetSelectionMode =  'Cell' } },
      { key = 'w', mods = 'ALT', action = act.Multiple{
           { CopyTo =  'ClipboardAndPrimarySelection' },
           { CopyMode =  'Close' } } },
}) do
   table.insert(copy_mode, v)
end
local search_mode = wezterm.gui.default_key_tables().search_mode
for _, v in pairs({
      { key = 's', mods = 'CTRL', action = act.CopyMode 'NextMatch' },
      { key = 'r', mods = 'CTRL', action = act.CopyMode 'PriorMatch'},
      { key = 'g', mods = 'CTRL', action = act.CopyMode 'Close' },
      { key = "k", mods = "CTRL", action = act.CopyMode 'ClearPattern' },
}) do
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
if keys_extended_shortcuts then
   for k,v in pairs(keys) do keys_extended_shortcuts[k] = v end
end
if hyperlink_rules_specific then
   for k,v in pairs(hyperlink_rules_specific) do hyperlink_rules[k] = v end
end

return {
   -- text & colours
   font = wezterm.font("JetBrains Mono"),
   font_size = font_size,
   color_scheme = "GruvboxDark",
   -- color_scheme = "Gruvbox Light",

   -- cursor
   default_cursor_style = 'BlinkingBlock',
   cursor_thickness = "1.4pt",
   hide_mouse_cursor_when_typing = false,

   -- keys and links
   keys = keys,
   key_tables = {copy_mode = copy_mode,
                 search_mode = search_mode},
   bypass_mouse_reporting_modifiers = 'SHIFT',
   mouse_bindings = mouse_bindings,
   hyperlink_rules = hyperlink_rules,
   enable_kitty_keyboard = true,

   -- window
   window_background_opacity = 0.9,
   hide_tab_bar_if_only_one_tab = true,
   window_close_confirmation = "NeverPrompt",
   enable_scroll_bar = true,

   -- domains
   ssh_domains = {
      {name = 'box', remote_address = 'box',},
      {name = 'a',   remote_address = 'a',},
   },
   unix_domains = {{name = 'auto',},},
}
