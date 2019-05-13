#!/bin/bash

os=$(lsb_release -si)
outname=package_list_$(lsb_release -sd | sed 's/"//g' | sed 's/[ /]/_/g')

case $os in
Arch)
# remove commonly-install packaage groups
    comm > $outname.txt <(pacman -Qqe | sort) \
         <(pacman -Qg base base-devel xorg xorg-fonts | cut -d' ' -f2 | sort) -23
    ;;
Raspbian)
    apt-mark showmanual > $outname.txt
    ;;
esac
