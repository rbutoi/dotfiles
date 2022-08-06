local wezterm = require 'wezterm'
local act = wezterm.action
local extkeys  = os.getenv("HOME").."/.config/wezterm/extkeys.lua"
local specific = os.getenv("HOME").."/.config/wezterm/specific.lua"

function do_if_file_exists(name)
   local f=io.open(name,"r")
   if f~=nil then io.close(f) else return end

   dofile(name)
end

-- set this first, so it may be overwritten in specific
local hostname = wezterm.hostname();
if hostname == "Radus-Macbook-Pro.local" then
   font_size_by_host = 13.0;
else
   font_size_by_host = 10.5;
end

do_if_file_exists(extkeys)
do_if_file_exists(specific)

keys = {
   -- for emacs undo, prefer super w/ same keys
   {key = "-", mods = "CTRL", action = "DisableDefaultAssignment"},
   {key = "_", mods = "CTRL|SHIFT", action = "DisableDefaultAssignment"},
   {key = "=", mods = "CTRL", action = "DisableDefaultAssignment"},
   {key = "+", mods = "CTRL|SHIFT", action = "DisableDefaultAssignment"},
   {key = "Enter", mods = "ALT", action = "DisableDefaultAssignment"},
   {
      key = 'P',
      mods = 'CTRL',
      action = wezterm.action.QuickSelectArgs {
         label = 'open url',
         patterns = {
            'https?://\\S+',
         },
         action = wezterm.action_callback(function(window, pane)
               local url = window:get_selection_text_for_pane(pane)
               wezterm.log_info('opening: ' .. url)
               wezterm.open_with(url)
         end),
      },
   },
}

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

hyperlink_rules = {
   {
      regex = "\\bhttps?://\\S*\\b",
      format = "$0",
   }, { -- This is actually the default if you don't specify any hyperlink_rules
      regex = "\\b\\w+://(?:[\\w.-]+)\\.[a-z]{2,15}\\S*\\b",
      format = "$0",
   }, { -- linkify email addresses
      regex = "\\b\\w+@[\\w-]+(\\.[\\w-]+)+\\b",
      format = "mailto:$0",
   }, { -- file:// URI
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
   font_size = font_size_by_host,
   color_scheme = "Gruvbox Dark",

   -- keys and links
   keys = keys,
   bypass_mouse_reporting_modifiers = 'SHIFT',
   mouse_bindings = mouse_bindings,
   hyperlink_rules = hyperlink_rules,

   -- window
   enable_wayland = true,
   window_background_opacity = 0.9,
   hide_tab_bar_if_only_one_tab = true,
   window_close_confirmation = "NeverPrompt",
   exit_behavior = "Close",
}
