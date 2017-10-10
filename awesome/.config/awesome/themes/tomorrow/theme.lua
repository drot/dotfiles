---------------------------
-- Tomorrow awesome theme --
---------------------------

local theme_assets = require("beautiful.theme_assets")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi

local gfs = require("gears.filesystem")
local themes_path = gfs.get_configuration_dir()

local theme = {}

theme.font = "Iosevka Term 12"

theme.icon_theme = "Numix-Square"

theme.wallpaper = themes_path.."themes/tomorrow/background.png"

theme.bg_normal = "#282a2e"
theme.bg_focus = "#cc6666"
theme.bg_urgent = "#f0c674"
theme.bg_minimize = "#373b41"
theme.bg_systray = theme.bg_normal

theme.fg_normal = "#c5c8c6"
theme.fg_focus = "#1d1f21"
theme.fg_urgent = "#ffffff"
theme.fg_minimize = "#969896"

theme.useless_gap = dpi(0)
theme.border_width = dpi(1)
theme.border_normal = "#1d1f21"
theme.border_focus = theme.bg_focus
theme.border_marked = "#f0c674"

-- Generate taglist squares
local taglist_square_size = dpi(6)
theme.taglist_squares_sel = theme_assets.taglist_squares_sel(
   taglist_square_size, theme.fg_normal
)
theme.taglist_squares_unsel = theme_assets.taglist_squares_unsel(
   taglist_square_size, theme.fg_normal
)

-- Variables set for theming the menu:
-- menu_[bg|fg]_[normal|focus]
-- menu_[border_color|border_width]
theme.menu_submenu_icon = themes_path.."themes/tomorrow/icons/menu/submenu.png"
theme.menu_height = dpi(21)
theme.menu_width = dpi(126)

-- You can add as many variables as
-- you wish and access them by using
-- beautiful.variable in your rc.lua
--theme.bg_widget = "#cc0000"

-- Titlebar
theme.titlebar_close_button_normal = themes_path.."themes/tomorrow/icons/titlebar/close_normal.png"
theme.titlebar_close_button_focus  = themes_path.."themes/tomorrow/icons/titlebar/close_focus.png"

theme.titlebar_minimize_button_normal = themes_path.."themes/tomorrow/icons/titlebar/minimize_normal.png"
theme.titlebar_minimize_button_focus  = themes_path.."themes/tomorrow/icons/titlebar/minimize_focus.png"

theme.titlebar_ontop_button_normal_inactive = themes_path.."themes/tomorrow/icons/titlebar/ontop_normal_inactive.png"
theme.titlebar_ontop_button_focus_inactive  = themes_path.."themes/tomorrow/icons/titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_active = themes_path.."themes/tomorrow/icons/titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_active  = themes_path.."themes/tomorrow/icons/titlebar/ontop_focus_active.png"

theme.titlebar_sticky_button_normal_inactive = themes_path.."themes/tomorrow/icons/titlebar/sticky_normal_inactive.png"
theme.titlebar_sticky_button_focus_inactive  = themes_path.."themes/tomorrow/icons/titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_active = themes_path.."themes/tomorrow/icons/titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_active  = themes_path.."themes/tomorrow/icons/titlebar/sticky_focus_active.png"

theme.titlebar_floating_button_normal_inactive = themes_path.."themes/tomorrow/icons/titlebar/floating_normal_inactive.png"
theme.titlebar_floating_button_focus_inactive  = themes_path.."themes/tomorrow/icons/titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_active = themes_path.."themes/tomorrow/icons/titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_active  = themes_path.."themes/tomorrow/icons/titlebar/floating_focus_active.png"

theme.titlebar_maximized_button_normal_inactive = themes_path.."themes/tomorrow/icons/titlebar/maximized_normal_inactive.png"
theme.titlebar_maximized_button_focus_inactive  = themes_path.."themes/tomorrow/icons/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_active = themes_path.."themes/tomorrow/icons/titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_active  = themes_path.."themes/tomorrow/icons/titlebar/maximized_focus_active.png"

-- Layout
theme.layout_fairh = themes_path.."themes/tomorrow/icons/layouts/fairhw.png"
theme.layout_fairv = themes_path.."themes/tomorrow/icons/layouts/fairvw.png"
theme.layout_floating  = themes_path.."themes/tomorrow/icons/layouts/floatingw.png"
theme.layout_magnifier = themes_path.."themes/tomorrow/icons/layouts/magnifierw.png"
theme.layout_max = themes_path.."themes/tomorrow/icons/layouts/maxw.png"
theme.layout_fullscreen = themes_path.."themes/tomorrow/icons/layouts/fullscreenw.png"
theme.layout_tilebottom = themes_path.."themes/tomorrow/icons/layouts/tilebottomw.png"
theme.layout_tileleft   = themes_path.."themes/tomorrow/icons/layouts/tileleftw.png"
theme.layout_tile = themes_path.."themes/tomorrow/icons/layouts/tilew.png"
theme.layout_tiletop = themes_path.."themes/tomorrow/icons/layouts/tiletopw.png"
theme.layout_spiral  = themes_path.."themes/tomorrow/icons/layouts/spiralw.png"
theme.layout_dwindle = themes_path.."themes/tomorrow/icons/layouts/dwindlew.png"
theme.layout_cornernw = themes_path.."themes/tomorrow/icons/layouts/cornernww.png"
theme.layout_cornerne = themes_path.."themes/tomorrow/icons/layouts/cornernew.png"
theme.layout_cornersw = themes_path.."themes/tomorrow/icons/layouts/cornersww.png"
theme.layout_cornerse = themes_path.."themes/tomorrow/icons/layouts/cornersew.png"

-- Generate Awesome icon
theme.awesome_icon = theme_assets.awesome_icon(
   theme.menu_height, theme.bg_focus, theme.fg_focus
)

return theme
