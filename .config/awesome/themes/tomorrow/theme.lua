---------------------------
-- Tomorrow awesome theme --
---------------------------

theme = {}

theme.font = "Terminus 10"
theme.icons_dir = os.getenv("HOME") .. "/.config/awesome/icons"

theme.wallpaper = "/home/drot/Pictures/dead.jpg"

theme.bg_normal = "#1f1f1f"
theme.bg_focus = "#1f1f1f"
theme.bg_urgent = "#f2777a"
theme.bg_minimize = "#1f1f1f"
theme.bg_systray = theme.bg_normal

theme.fg_normal = "#cccccc"
theme.fg_focus = "#ffd75f"
theme.fg_urgent = "#ffffff"
theme.fg_minimize = "#515151"

theme.border_width = 1
theme.border_normal = "#2f2f2f"
theme.border_focus = "#2f2f2f"
theme.border_marked = "#1f1f1f"

-- awesome icon
theme.awesome_icon = theme.icons_dir .. "/awesome.png"

-- Display the taglist squares
theme.taglist_squares_sel = theme.icons_dir .. "/taglist/squaref.png"
theme.taglist_squares_unsel = theme.icons_dir .. "/taglist/squareu.png"

-- Variables set for theming the menu:
theme.menu_submenu_icon = theme.icons_dir .. "/submenu.png"
theme.menu_height = 15
theme.menu_width = 140

-- Tasklist
theme.tasklist_floating = ""
theme.tasklist_maximized_horizontal = ""
theme.tasklist_maximized_vertical = "" 

-- You can use your own layout icons like this:
theme.layout_fairh = theme.icons_dir .. "/layouts/fairh.png"
theme.layout_fairv = theme.icons_dir .. "/layouts/fairv.png"
theme.layout_floating = theme.icons_dir .. "/layouts/floating.png"
theme.layout_magnifier = theme.icons_dir .. "/layouts/magnifier.png"
theme.layout_max = theme.icons_dir .. "/layouts/max.png"
theme.layout_fullscreen = theme.icons_dir .. "/layouts/fullscreen.png"
theme.layout_tilebottom = theme.icons_dir .. "/layouts/tilebottom.png"
theme.layout_tileleft = theme.icons_dir .. "/layouts/tileleft.png"
theme.layout_tile = theme.icons_dir .. "/layouts/tile.png"
theme.layout_tiletop = theme.icons_dir .. "/layouts/tiletop.png"
theme.layout_spiral = theme.icons_dir .. "/layouts/spiral.png"
theme.layout_dwindle = theme.icons_dir .. "/layouts/dwindle.png"

-- Widget separator
theme.widget_sep = theme.icons_dir .. "/separator.png"

-- Widget icons
theme.widget_cpu = theme.icons_dir .. "/widgets/cpu.png"
theme.widget_mem = theme.icons_dir .. "/widgets/mem.png"
theme.widget_temp = theme.icons_dir .. "/widgets/temp.png"
theme.widget_disk = theme.icons_dir .. "/widgets/disk.png"
theme.widget_date = theme.icons_dir .. "/widgets/date.png"
theme.widget_vol = theme.icons_dir .. "/widgets/vol.png"

return theme
