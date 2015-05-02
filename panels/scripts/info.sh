#/bin/bash

UPTIME=$( uptime | sed 's/.* up //' | sed 's/[0-9]* us.*//' | sed 's/ day, /d /'\
         | sed 's/ days, /d /' | sed 's/:/h /' | sed 's/ min//'\
         |  sed 's/,/m/' | sed 's/  / /')
PACKAGES=$(pacman -Q | wc -l)
UPDATED=$(awk '/upgraded/ {line=$0;} END { $0=line; gsub(/[\[\]]/,"",$0); \
         printf "%s",$1;}' /var/log/pacman.log)

(
echo "Updated: $UPDATED"
echo " Packages: $PACKAGES"
echo " Uptime: $UPTIME"
) | dzen2 -bg '#1b1d1e' -fg '#000' -xs 2 -p -x "1110" -y "695" -w "125" -l "3" -sa 'l' -ta 'c' -fn 'Droid Sans Mono for Powerline-8'\
   -title-name 'popup_sysinfo' -e 'onstart=uncollapse;button1=exit;button3=exit'
