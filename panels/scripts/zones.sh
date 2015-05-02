#/bin/bash

(
echo "UTC $(TZ=UTC date +%d\ -\ %H:%M)"
echo "   EST $(TZ=EST date +%d\ -\ %H:%M)"
echo "   LA  $(TZ=America/Los_Angeles date +%d\ -\ %H:%M)"
echo "   CET $(TZ=CET date +%d\ -\ %H:%M)"
) | dzen2 -xs 2 -bg '#afdf00' -fg '#000' -p -x "1242" -y "695" -w "122" -l "3" -sa 'l' -ta 'c' -fn 'Droid Sans Mono for Powerline-8'\
  -title-name 'popup_sysinfo' -e 'onstart=uncollapse;button1=exit;button3=exit'

