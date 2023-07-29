{-# LANGUAGE PostfixOperators    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Config.Widgets where

import Xmobar
import Config.Helpers

color1 :: String = "#ff8059"

battery :: [String] -> Monitors
battery interfaces = BatteryP interfaces [ "-t", "<acstatus>"
                                         , "-S", "Off", "-d", "0", "-m", "3"
                                         , "-L", "20", "-H", "90", "-p", "3", "-l", "red"
                                         , "-W", "0"
                                         , "-f", "\xf244\xf243\xf243\xf243\xf242\xf242\xf242\xf241\xf241\xf240"
                                         , "--"
                                         , "-P"
                                         , "-a", "notify-send -u critical 'Battery running out!!!!!!'"
                                         , "-A", "22"
                                         , "-i", inIconFont "\xf1e6"
                                         , "-O", inColor color1 (inIconFont "<leftbar>  \xf1e6") <> " <left> (<timeleft>)"
                                         , "-o", inColor color1 (inIconFont "<leftbar>") <> " <left> (<timeleft>)"
                                         , "-H", "10", "-L", "7"
                                         ] (10 `seconds`)

date :: Date
date = Date (inColor color1 (inIconFont "\xf017") <> " %b %d %Y - (%H:%M) ") "date" (10 `seconds`)

wireless :: String -> Monitors
wireless interface = Wireless interface [ "-t", inColor color1 (inIconFont "\xf1eb") <> " <essid> <quality>"
                                        , "-L","30"
                                        , "-H","90"
                                        , "--low", "red"
                                        , "--normal","yellow"
                                        , "--high","green"
                                        ] (10 `seconds`)

brightness :: Monitors
brightness = Brightness [ "-t", inColor color1 (inAltIconFont "\xf0eb") <> " <percent>%"
                        , "--"
                        , "-D", "intel_backlight"
                        ] (1 `seconds`)

diskUsage :: Monitors
diskUsage = DiskU [("/", inColor color1 (inAltIconFont "\xf0c7") <> " <free> free")] [] (10 `minutes`)

coreTemp :: Monitors
coreTemp = CoreTemp [ "-t", inColor color1 (inAltIconFont "\xf2c9") <> " <core0>C" ] (5 `seconds`)

memory :: Monitors
memory = Memory [ "-t", inColor color1 (inAltIconFont "\xf538") <> " <used>M (<usedratio>%)" ] (2 `seconds`)

cpu :: Monitors
cpu = Cpu [ "-t", inColor color1 (inAltIconFont "\xf2db") <> " <total>%"
          , "-H", "50"
          , "--high", "red"
          ] (2 `seconds`)

trayer :: Command
trayer = Com "$XDG_CONFIG_HOME/xmobar/trayer-padding-icon.sh" [] "trayerpad" 20
