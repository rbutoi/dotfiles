#!/bin/bash

os=$(lsb_release -si)
outname=package_list_$(lsb_release -sd | sed 's/"//g' | sed 's/[ /]/_/g')

case $os in
Arch)
    comm <(pacman -Qqe | sort) <(pacman -Qg | cut -d' ' -f2 | sort) -23 > $outname.txt
    ;;
Raspbian)
    apt-mark showmanual > $outname.txt
    ;;
esac
