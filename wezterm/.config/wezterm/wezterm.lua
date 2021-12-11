local wezterm = require 'wezterm';
dofile(os.getenv("HOME").."/.config/wezterm/extkeys.lua");
dofile(os.getenv("HOME").."/.config/wezterm/specific.lua");

keys = {
   -- defaults to toggle fullscreen, which sway can do with <super> f. also need
   -- this keycode:
   -- https://github.com/Canop/broot/issues/86#issuecomment-801877468
   {key="Enter", mods="ALT", action=wezterm.action{SendString="\x1b\x0d"}},
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
   font_size = 10.5,
   color_scheme = "Gruvbox Dark",
   -- window
   enable_wayland = true,
   window_background_opacity = 0.9,
   hide_tab_bar_if_only_one_tab = true,
   window_close_confirmation = "NeverPrompt",
   exit_behavior = "Close",
   -- keys and links
   keys = keys,
   hyperlink_rules = hyperlink_rules
}
