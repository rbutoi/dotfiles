local wezterm = require 'wezterm';
dofile(os.getenv("HOME").."/.config/wezterm/extkeys.lua");

return {
   -- text & colours
   font = wezterm.font("JetBrains Mono"),
   font_size = 11,
   color_scheme = "Gruvbox Dark",
   -- window
   enable_wayland = true,
   window_background_opacity = 0.9,
   hide_tab_bar_if_only_one_tab = true,
   -- $TERM - includes 24-bit colour support
   term = "wezterm",
   keys = keys_extended_shortcuts,
}
