-------------------------------
--  "Zenburn" awesome theme  --
--    By Adrian C. (anrxc)   --
-------------------------------

-- {{{ Main
theme = {}
theme.icons_dir = themes_dir .. "zenburn/icons/"
theme.wallpaper = themes_dir .. "zenburn/zenburn.png"
-- }}}

-- {{{ Font
theme.font = "Terminus 10"
-- }}}

-- {{{ Colors
theme.fg_normal = "#dcdccc"
theme.fg_focus = "#f0dfaf"
theme.fg_urgent = "#cc9393"
theme.bg_normal = "#3f3f3f"
theme.bg_focus = "#1e2320"
theme.bg_urgent = "#3f3f3f"
theme.bg_systray = theme.bg_normal
-- }}}

-- {{{ Borders
theme.border_width = 1
theme.border_normal = "#1e2320"
theme.border_focus = "#6f6f6f"
theme.border_marked = "#cc9393"
-- }}}

-- {{{ Tasklist
theme.tasklist_disable_icon = true
theme.tasklist_floating = ""
theme.tasklist_maximized_horizontal = ""
theme.tasklist_maximized_vertical = ""
-- }}}

-- {{{ Widgets
theme.fg_widget = "#aecf96"
theme.bg_widget = "#494b4f"
theme.center_widget = "#88a175"
theme.end_widget = "#ff5656"
theme.border_widget = "#6f6f6f"
-- }}}

-- {{{ Menu
theme.menu_height = 21
theme.menu_width = 126
-- }}}

-- {{{ Icons
-- {{{ Taglist
theme.taglist_squares_sel = theme.icons_dir .. "taglist/squaref.png"
theme.taglist_squares_unsel = theme.icons_dir .. "taglist/squareu.png"
-- }}}

-- {{{ Separator
theme.widget_sep = theme.icons_dir .. "separator.png"
-- }}}

-- {{{ Layout
theme.layout_fairh = theme.icons_dir .. "layouts/fairh.png"
theme.layout_fairv = theme.icons_dir .. "layouts/fairv.png"
theme.layout_floating = theme.icons_dir .. "layouts/floating.png"
theme.layout_magnifier = theme.icons_dir .. "layouts/magnifier.png"
theme.layout_max = theme.icons_dir .. "layouts/max.png"
theme.layout_fullscreen = theme.icons_dir .. "layouts/fullscreen.png"
theme.layout_tilebottom = theme.icons_dir .. "layouts/tilebottom.png"
theme.layout_tileleft = theme.icons_dir .. "layouts/tileleft.png"
theme.layout_tile = theme.icons_dir .. "layouts/tile.png"
theme.layout_tiletop = theme.icons_dir .. "layouts/tiletop.png"
theme.layout_spiral = theme.icons_dir .. "layouts/spiral.png"
theme.layout_dwindle = theme.icons_dir .. "layouts/dwindle.png"
-- }}}

-- {{{ Widgets
theme.widget_cpu = theme.icons_dir .. "widgets/cpu.png"
theme.widget_mem = theme.icons_dir .. "widgets/mem.png"
theme.widget_temp = theme.icons_dir .. "widgets/temp.png"
theme.widget_disk = theme.icons_dir .. "widgets/disk.png"
theme.widget_vol = theme.icons_dir .. "widgets/vol.png"
theme.widget_date = theme.icons_dir .. "widgets/date.png"
-- }}}

-- {{{ Menu
theme.menu_submenu_icon = theme.icons_dir .. "menu/submenu.png"
theme.menu_work = theme.icons_dir .. "menu/work.png"
theme.menu_web = theme.icons_dir .. "menu/web.png"
theme.menu_office = theme.icons_dir .. "menu/office.png"
theme.menu_util = theme.icons_dir .. "menu/util.png"
theme.menu_term = theme.icons_dir .. "menu/term.png"
theme.menu_emacs = theme.icons_dir .. "menu/emacs.png"
theme.menu_browser = theme.icons_dir .. "menu/browser.png"
theme.menu_skype = theme.icons_dir .. "menu/skype.png"
theme.menu_pdf = theme.icons_dir .. "menu/pdf.png"
theme.menu_writer = theme.icons_dir .. "menu/writer.png"
theme.menu_gimp = theme.icons_dir .. "menu/gimp.png"
theme.menu_fman = theme.icons_dir .. "menu/fman.png"
theme.menu_pavu = theme.icons_dir .. "menu/pavu.png"
theme.menu_manual = theme.icons_dir .. "menu/manual.png"
theme.menu_edit = theme.icons_dir .. "menu/edit.png"
theme.menu_restart = theme.icons_dir .. "menu/restart.png"
theme.menu_quit = theme.icons_dir .. "menu/quit.png"
--}}}

-- {{{ Awesome icon
theme.awesome_icon = theme.icons_dir .. "awesome.png"
-- }}}

return theme
