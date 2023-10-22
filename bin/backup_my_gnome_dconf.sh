#!/bin/sh

set -euf

mkdir -p ~/.config/gnome_dconf_backup && cd ~/.config/gnome_dconf_backup

echo -e "#!/bin/sh\n" > restore.sh
chmod +x restore.sh

for i in desktop/a11y desktop/interface desktop/input-sources \
  desktop/peripherals desktop/sound desktop/wm eog/ui gedit mutter nautilus \
  pomodoro/preferences settings-daemon shell/extensions shell/keybindings; do

  p=/org/gnome/$i/
  fp=$(echo $p | sed 's|/|_|g').ini

          dconf dump $p    > $fp
  printf "dconf load %-40s < $fp\n" "$p" >> restore.sh
done
