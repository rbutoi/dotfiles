#!/bin/bash

os=$(uname)
if [ $os == Darwin ]; then
    outname=package_list_$os
else
    outname=package_list_$(lsb_release -sd | sed 's/"//g' | sed 's/[ /]/_/g')_$(hostname -s).txt
    os=$(lsb_release -si)
fi

case $os in
Arch)
    # remove commonly-installed package groups
    comm > $outname <(pacman -Qqe | sort) \
         <(pacman -Qg base-devel xorg xorg-fonts | cut -d' ' -f2 | sort) -23
    ;;
Raspbian)
    apt-mark showmanual > $outname
    ;;
Darwin)
    brew leaves > $outname
    ;;
esac
