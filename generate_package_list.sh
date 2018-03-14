#!/bin/bash
comm <(pacman -Qqe | sort) <(pacman -Qg | cut -d' ' -f2 | sort) -23 > package_list.txt
