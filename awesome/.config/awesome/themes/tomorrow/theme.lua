---------------------------
-- Tomorrow awesome theme --
---------------------------

local theme_assets = require("beautiful.theme_assets")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi

local gfs = require("gears.filesystem")
local themes_path = gfs.get_configuration_dir()

local gshape = require("gears.shape")

local theme = {}

theme.font = "Iosevka Term SS05 12"

theme.icon_theme = "Papirus"

-- Main colors
theme.bg_normal = "#1d1f21"
theme.bg_focus = "#282a2e"
theme.bg_urgent = "#cc6666"
theme.bg_minimize = "#373b41"
theme.bg_systray = theme.bg_normal

theme.fg_normal = "#c5c8c6"
theme.fg_focus = "#81a2be"
theme.fg_urgent = "#ffffff"
theme.fg_minimize = "#969896"

-- Wallpaper
theme.wallpaper = theme_assets.wallpaper(theme.bg_normal, theme.bg_focus, theme.bg_minimize)

-- Window borders
theme.useless_gap = dpi(0)
theme.border_width = dpi(1)
theme.border_normal = "#282a2e"
theme.border_focus = "#969896"
theme.border_marked = theme.bg_urgent

-- Titlebar
theme.titlebar_bg = "#222427"

-- Wibox
theme.wibar_border_width = dpi(1)
theme.wibar_border_color = theme.border_normal

-- Hotkeys display
theme.hotkeys_font = theme.font
theme.hotkeys_description_font = theme.font
theme.hotkeys_modifiers_fg = "#f0c674"

-- Prompt cursor
theme.prompt_fg_cursor = theme.bg_normal
theme.prompt_bg_cursor = theme.fg_normal

-- Generate taglist squares
local taglist_square_size = dpi(5)
theme.taglist_squares_sel = theme_assets.taglist_squares_sel(
   taglist_square_size, theme.fg_normal
)
theme.taglist_squares_unsel = theme_assets.taglist_squares_unsel(
   taglist_square_size, theme.fg_normal
)

-- Taglist
theme.taglist_bg_focus = theme.bg_normal

-- Tasklist
theme.tasklist_spacing = 1
theme.tasklist_shape = gshape.octogon
theme.tasklist_shape_border_width = dpi(1)
theme.tasklist_font_minimized = "Iosevka Term SS05 Italic 12"
theme.tasklist_shape_border_color = theme.bg_minimize
theme.tasklist_shape_border_color_minimized = theme.fg_minimize
theme.tasklist_bg_normal = theme.bg_focus
theme.tasklist_bg_focus = theme.titlebar_bg
theme.tasklist_shape_border_color_urgent = theme.bg_urgent
theme.tasklist_sticky = "[∇]"
theme.tasklist_ontop = "[†]"
theme.tasklist_above = "[↑]"
theme.tasklist_below = "[↓]"
theme.tasklist_floating = "[≈]"
theme.tasklist_maximized = "[×]"
theme.tasklist_maximized_horizontal = "[↔]"
theme.tasklist_maximized_vertical = "[↕]"

-- Tooltip
theme.tooltip_border_color = theme.border_focus

-- Menu
theme.menu_height = dpi(24)
theme.menu_width = dpi(132)
theme.menu_border_width = dpi(2)
theme.menu_border_color = theme.border_focus

-- Menu icons
theme.awesome_menu_icon = themes_path .. "themes/tomorrow/icons/menu/awesomemenu.png"
theme.menu_submenu_icon = themes_path .. "themes/tomorrow/icons/menu/submenu.png"
theme.menu_web = themes_path .. "themes/tomorrow/icons/menu/web.png"
theme.menu_office = themes_path .. "themes/tomorrow/icons/menu/office.png"
theme.menu_graphics = themes_path .. "themes/tomorrow/icons/menu/graphics.png"
theme.menu_util = themes_path .. "themes/tomorrow/icons/menu/util.png"
theme.menu_term = themes_path .. "themes/tomorrow/icons/menu/term.png"
theme.menu_emacs = themes_path .. "themes/tomorrow/icons/menu/emacs.png"
theme.menu_qutebrowser = themes_path .. "themes/tomorrow/icons/menu/qutebrowser.png"
theme.menu_firefox = themes_path .. "themes/tomorrow/icons/menu/firefox.png"
theme.menu_pidgin = themes_path .. "themes/tomorrow/icons/menu/pidgin.png"
theme.menu_torbrowser = themes_path .. "themes/tomorrow/icons/menu/torbrowser.png"
theme.menu_pdf = themes_path .. "themes/tomorrow/icons/menu/pdf.png"
theme.menu_writer = themes_path .. "themes/tomorrow/icons/menu/writer.png"
theme.menu_gimp = themes_path .. "themes/tomorrow/icons/menu/gimp.png"
theme.menu_pqiv = themes_path .. "themes/tomorrow/icons/menu/pqiv.png"
theme.menu_fman = themes_path .. "themes/tomorrow/icons/menu/fman.png"
theme.menu_pavu = themes_path .. "themes/tomorrow/icons/menu/pavu.png"
theme.menu_ssr = themes_path .. "themes/tomorrow/icons/menu/ssr.png"
theme.menu_hotkeys = themes_path .. "themes/tomorrow/icons/menu/hotkeys.png"
theme.menu_manual = themes_path .. "themes/tomorrow/icons/menu/manual.png"
theme.menu_edit = themes_path .. "themes/tomorrow/icons/menu/edit.png"
theme.menu_restart = themes_path .. "themes/tomorrow/icons/menu/restart.png"
theme.menu_quit = themes_path .. "themes/tomorrow/icons/menu/quit.png"

-- Titlebar
theme.titlebar_close_button_normal = themes_path .. "themes/tomorrow/icons/titlebar/close_normal.png"
theme.titlebar_close_button_focus = themes_path .. "themes/tomorrow/icons/titlebar/close_focus.png"

theme.titlebar_minimize_button_normal = themes_path .. "themes/tomorrow/icons/titlebar/minimize_normal.png"
theme.titlebar_minimize_button_focus = themes_path .. "themes/tomorrow/icons/titlebar/minimize_focus.png"

theme.titlebar_ontop_button_normal_inactive = themes_path .. "themes/tomorrow/icons/titlebar/ontop_normal_inactive.png"
theme.titlebar_ontop_button_focus_inactive = themes_path .. "themes/tomorrow/icons/titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_active = themes_path .. "themes/tomorrow/icons/titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_active = themes_path .. "themes/tomorrow/icons/titlebar/ontop_focus_active.png"

theme.titlebar_sticky_button_normal_inactive = themes_path .. "themes/tomorrow/icons/titlebar/sticky_normal_inactive.png"
theme.titlebar_sticky_button_focus_inactive = themes_path .. "themes/tomorrow/icons/titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_active = themes_path .. "themes/tomorrow/icons/titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_active = themes_path .. "themes/tomorrow/icons/titlebar/sticky_focus_active.png"

theme.titlebar_floating_button_normal_inactive = themes_path .. "themes/tomorrow/icons/titlebar/floating_normal_inactive.png"
theme.titlebar_floating_button_focus_inactive = themes_path .. "themes/tomorrow/icons/titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_active = themes_path .. "themes/tomorrow/icons/titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_active = themes_path .. "themes/tomorrow/icons/titlebar/floating_focus_active.png"

theme.titlebar_maximized_button_normal_inactive = themes_path .. "themes/tomorrow/icons/titlebar/maximized_normal_inactive.png"
theme.titlebar_maximized_button_focus_inactive = themes_path .. "themes/tomorrow/icons/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_active = themes_path .. "themes/tomorrow/icons/titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_active = themes_path .. "themes/tomorrow/icons/titlebar/maximized_focus_active.png"

-- Layouts
theme.layout_cornerne = themes_path .. "themes/tomorrow/icons/layouts/cornerne.png"
theme.layout_cornernw = themes_path .. "themes/tomorrow/icons/layouts/cornernw.png"
theme.layout_cornerse = themes_path .. "themes/tomorrow/icons/layouts/cornerse.png"
theme.layout_cornersw = themes_path .. "themes/tomorrow/icons/layouts/cornersw.png"
theme.layout_dwindle = themes_path .. "themes/tomorrow/icons/layouts/dwindle.png"
theme.layout_fairh = themes_path .. "themes/tomorrow/icons/layouts/fairh.png"
theme.layout_fairv = themes_path .. "themes/tomorrow/icons/layouts/fairv.png"
theme.layout_floating  = themes_path .. "themes/tomorrow/icons/layouts/floating.png"
theme.layout_fullscreen = themes_path .. "themes/tomorrow/icons/layouts/fullscreen.png"
theme.layout_magnifier = themes_path .. "themes/tomorrow/icons/layouts/magnifier.png"
theme.layout_max = themes_path .. "themes/tomorrow/icons/layouts/max.png"
theme.layout_spiral  = themes_path .. "themes/tomorrow/icons/layouts/spiral.png"
theme.layout_tilebottom = themes_path .. "themes/tomorrow/icons/layouts/tilebottom.png"
theme.layout_tileleft   = themes_path .. "themes/tomorrow/icons/layouts/tileleft.png"
theme.layout_tile = themes_path .. "themes/tomorrow/icons/layouts/tile.png"
theme.layout_tiletop = themes_path .. "themes/tomorrow/icons/layouts/tiletop.png"

-- Awesome icon
theme.awesome_icon = theme_assets.awesome_icon(16, theme.fg_focus, theme.bg_normal)

-- Widgets
theme.widget_value = "#f0c674"
theme.widget_cpu = themes_path .. "themes/tomorrow/icons/widgets/cpu.png"
theme.widget_memory = themes_path .. "themes/tomorrow/icons/widgets/memory.png"
theme.widget_temperature = themes_path .. "themes/tomorrow/icons/widgets/temperature.png"
theme.widget_fs = themes_path .. "themes/tomorrow/icons/widgets/fs.png"
theme.widget_volume = themes_path .. "themes/tomorrow/icons/widgets/volume.png"
theme.widget_clock = themes_path .. "themes/tomorrow/icons/widgets/clock.png"

return theme
