-- {{{ Main
theme = {}
theme.confdir       = awful.util.getdir("config")
theme.wallpaper_cmd = { "sh /home/drot/.fehbg" }
-- }}}

-- {{{ Font
theme.font      = "Terminus 9"
-- }}}

-- {{{ Colors
theme.bg_normal     = "#000000"
theme.bg_focus      = "#000000"
theme.bg_urgent     = "#000000"

theme.fg_normal     = "#F2F2F2"
theme.fg_focus      = "#CEFFAC"
theme.fg_urgent     = "#B6DCFF"

theme.border_width  = "1"
theme.border_normal = "#FFB6B0"
theme.border_focus  = "#7C7C7C"
theme.border_marked = "#CEFFAC"
-- }}}

-- {{{ Widgets
theme.fg_widget           = "#CEFFAC"
theme.fg_center_widget    = "#CEFFAC"
theme.fg_end_widget       = "#CEFFAC"
theme.border_widget       = theme.bg_normal
-- }}}

-- {{{ Taglist
theme.taglist_squares_sel = theme.confdir .. "/icons/taglist/squarefz.png"
theme.taglist_squares_unsel = theme.confdir .. "/icons/taglist/squareza.png"
-- }}}

-- {{{ Layout
theme.layout_tile       = theme.confdir .. "/icons/layouts/tile.png"
theme.layout_tileleft   = theme.confdir .. "/icons/layouts/tileleft.png"
theme.layout_tilebottom = theme.confdir .. "/icons/layouts/tilebottom.png"
theme.layout_tiletop    = theme.confdir .. "/icons/layouts/tiletop.png"
theme.layout_max        = theme.confdir .. "/icons/layouts/max.png"
theme.layout_magnifier  = theme.confdir .. "/icons/layouts/magnifier.png"
theme.layout_floating   = theme.confdir .. "/icons/layouts/floating.png"
-- }}}

-- {{{ Widget icons
theme.widget_cpu    = theme.confdir .. "/icons/cpu.png"
theme.widget_mem    = theme.confdir .. "/icons/ram.png"
theme.widget_fs     = theme.confdir .. "/icons/disk.png"
theme.widget_net    = theme.confdir .. "/icons/down.png"
theme.widget_netup  = theme.confdir .. "/icons/up.png"
theme.widget_mail   = theme.confdir .. "/icons/mail.png"
theme.widget_date   = theme.confdir .. "/icons/time.png"
theme.widget_sep    = theme.confdir .. "/icons/separator.png"
theme.widget_music  = theme.confdir .. "/icons/mpd.png"
theme.widget_weather  = theme.confdir .. "/icons/temp.png"
theme.widget_uptime  = theme.confdir .. "/icons/ac.png"
theme.widget_vol  = theme.confdir .. "/icons/vol.png"
theme.widget_pkg  = theme.confdir .. "/icons/pac.png"
-- }}}

return theme
