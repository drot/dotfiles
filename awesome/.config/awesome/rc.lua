-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")

-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")
local menubar = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup")
-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
require("awful.hotkeys_popup.keys")
-- Vicious widget library
local vicious = require("vicious")

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
   naughty.notify({ preset = naughty.config.presets.critical,
                    title = "Oops, there were errors during startup!",
                    text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
   local in_error = false
   awesome.connect_signal("debug::error", function (err)
                             -- Make sure we don't go into an endless error loop
                             if in_error then return end
                             in_error = true

                             naughty.notify({ preset = naughty.config.presets.critical,
                                              title = "Oops, an error happened!",
                                              text = tostring(err) })
                             in_error = false
   end)
end
-- }}}

-- {{{ Variable definitions
-- Themes define colours, icons, font and wallpapers.
beautiful.init(gears.filesystem.get_configuration_dir() .. "themes/tomorrow/theme.lua")

-- This is used later as the default terminal and editor to run.
local terminal = "st"
local editor = os.getenv("EDITOR")
local editor_cmd = terminal .. " -e " .. editor .. " -t"

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
local modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
   awful.layout.suit.floating,
   awful.layout.suit.tile,
   awful.layout.suit.tile.left,
   awful.layout.suit.tile.bottom,
   awful.layout.suit.tile.top,
   awful.layout.suit.fair,
   awful.layout.suit.fair.horizontal,
   awful.layout.suit.spiral,
   awful.layout.suit.spiral.dwindle,
   awful.layout.suit.max,
   awful.layout.suit.max.fullscreen,
   awful.layout.suit.magnifier,
   awful.layout.suit.corner.nw,
   -- awful.layout.suit.corner.ne,
   -- awful.layout.suit.corner.sw,
   -- awful.layout.suit.corner.se,
}
-- }}}

-- {{{ Helper functions
local function client_menu_toggle_fn()
   local instance = nil

   return function ()
      if instance and instance.wibox.visible then
         instance:hide()
         instance = nil
      else
         instance = awful.menu.clients({ theme = { width = 250 } })
      end
   end
end
-- }}}

-- {{{ Notification configuration
naughty.config.presets.low.bg = beautiful.bg_normal
naughty.config.presets.low.fg = beautiful.fg_normal
naughty.config.presets.low.border_color = beautiful.border_color
naughty.config.presets.normal.bg = beautiful.bg_focus
naughty.config.presets.normal.fg = beautiful.fg_focus
naughty.config.presets.normal.border_color = beautiful.border_focus
naughty.config.presets.critical.bg = beautiful.bg_urgent
naughty.config.presets.critical.fg = beautiful.fg_urgent
naughty.config.presets.critical.border_color = beautiful.bg_normal
-- }}}

-- {{{ Menu
-- Create a launcher widget and a main menu
local mygraphicsmenu = {
   { "feh", "feh -t Pictures", beautiful.menu_feh },
   { "GIMP", "gimp", beautiful.menu_gimp }
}

local myofficemenu = {
   { "Writer", "lowriter", beautiful.menu_writer },
   { "Zathura", "zathura", beautiful.menu_pdf }
}

local myutilmenu = {
   { "PCManFM", "pcmanfm", beautiful.menu_fman },
   { "Pavucontrol", "pavucontrol", beautiful.menu_pavu },
   { "SSR", "simplescreenrecorder", beautiful.menu_ssr },
   { "Virt Manager", "virt-manager", beautiful.menu_virt }
}

local mywebmenu = {
   { "Pidgin", "pidgin", beautiful.menu_pidgin },
   { "Ripcord", "ripcord", beautiful.menu_ripcord },
   { "Tor Browser", "torbrowser-launcher", beautiful.menu_torbrowser }
}

local myawesomemenu = {
   { "Hotkeys", function() return false, hotkeys_popup.show_help end, beautiful.menu_hotkeys },
   { "Manual", terminal .. " -e man awesome", beautiful.menu_manual },
   { "Edit config", editor_cmd .. " " .. awesome.conffile, beautiful.menu_edit },
   { "Restart", awesome.restart, beautiful.menu_restart },
   { "Quit", function() awesome.quit() end, beautiful.menu_quit }
}

local mymainmenu = awful.menu(
   {
      items = {
         { "Terminal", terminal, beautiful.menu_term },
         { "Emacs", "emacsclient -c", beautiful.menu_emacs },
         { "Firefox", "firefox", beautiful.menu_firefox },
         { "Graphics", mygraphicsmenu, beautiful.menu_graphics },
         { "Office", myofficemenu, beautiful.menu_office },
         { "Utilities", myutilmenu, beautiful.menu_util },
         { "Web", mywebmenu, beautiful.menu_web },
         { "awesome", myawesomemenu, beautiful.awesome_menu_icon }
      }
   }
)

local mylauncher = wibox.widget {
   top = 4,
   bottom = 4,
   left = 4,
   right = 2,
   widget = wibox.container.margin,
   { widget = awful.widget.launcher({ image = beautiful.awesome_icon,
                                      menu = mymainmenu }) }
}

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}

-- {{{ Wibar

-- Cache CPU usage widgets
vicious.cache(vicious.widgets.cpu)

-- Create a CPU usage icon widget
local cpu_icon = wibox.widget {
   top = 4,
   bottom = 4,
   left = 2,
   right = 2,
   widget = wibox.container.margin,
   { image = beautiful.widget_cpu,
     widget = wibox.widget.imagebox }
}

-- Set CPU usage text value
local cpu_text_value = wibox.widget.textbox()
vicious.register(cpu_text_value, vicious.widgets.cpu, "$1%", 4)

-- Create a CPU usage text widget
local cpu_text_widget = wibox.widget {
   top = 2,
   bottom = 2,
   left = 2,
   right = 2,
   widget = wibox.container.margin,
   { shape = gears.shape.rectangle,
     bg = beautiful.bg_focus,
     shape_border_color = beautiful.bg_minimize,
     shape_border_width = beautiful.border_width,
     widget = wibox.container.background,
     { left = 6,
       right = 6,
       widget = wibox.container.margin,
       { widget = cpu_text_value } } }
}

-- Create a CPU usage graph widget
local cpu_graph = wibox.widget {
   forced_height = 6,
   forced_width = 48,
   stack = true,
   max_value = 100,
   stack_colors = { "#cc6666", "#f0c674", "#81a2be", "#c5c8c6" },
   background_color = beautiful.border_normal,
   border_color = beautiful.bg_minimize,
   widget = wibox.widget.graph
}

-- Set CPU usage graph value
local cpu_cores_graph = wibox.widget.graph()
vicious.register(cpu_cores_graph, vicious.widgets.cpu,
                 function (widget, args)
                    cpu_graph:add_value(args[2], 1) -- Core 1, color 1
                    cpu_graph:add_value(args[3], 2) -- Core 2, color 2
                    cpu_graph:add_value(args[4], 3) -- Core 3, color 3
                    cpu_graph:add_value(args[5], 4) -- Core 4, color 4
                 end, 4)

-- Create a background for the CPU usage widget
local cpu_widget = wibox.widget {
   top = 2,
   bottom = 2,
   left = 2,
   right = 2,
   widget = wibox.container.margin,
   { shape = gears.shape.rectangle,
     bg = beautiful.bg_normal,
     shape_border_color = beautiful.bg_minimize,
     shape_border_width = beautiful.border_width,
     widget = wibox.container.background,
     { top = 2,
       bottom = 2,
       left = 6,
       right = 6,
       widget = wibox.container.margin,
       { widget = cpu_graph } } }
}

-- Cache memory usage widgets
vicious.cache(vicious.widgets.mem)

-- Create a memory usage icon widget
local memory_icon = wibox.widget {
   top = 4,
   bottom = 4,
   left = 2,
   right = 2,
   widget = wibox.container.margin,
   { image = beautiful.widget_memory,
     widget = wibox.widget.imagebox }
}

-- Set memory usage text value
local memory_text_value = wibox.widget.textbox()
vicious.register(memory_text_value, vicious.widgets.mem, "$1%", 12)

-- Create a memory usage text widget
local memory_text_widget = wibox.widget {
   top = 2,
   bottom = 2,
   left = 2,
   right = 2,
   widget = wibox.container.margin,
   { shape = gears.shape.rectangle,
     bg = beautiful.bg_focus,
     shape_border_color = beautiful.bg_minimize,
     shape_border_width = beautiful.border_width,
     widget = wibox.container.background,
     { left = 6,
       right = 6,
       widget = wibox.container.margin,
       { widget = memory_text_value } } }
}

-- Create a memory usage chart widget
local memory_chart = wibox.widget {
   max_value = 1,
   value = 0.25,
   thickness = 4,
   border_width = 1,
   bg = beautiful.bg_minimize,
   border_color = beautiful.bg_normal,
   colors = { beautiful.widget_value },
   widget = wibox.container.arcchart
}

-- Set memory usage chart value
vicious.register(memory_chart, vicious.widgets.mem, "$1", 12)

-- Create a memory usage chart background
local memory_widget = wibox.widget {
   top = 2,
   bottom = 2,
   left = 2,
   right = 2,
   widget = wibox.container.margin,
   { shape = gears.shape.rectangle,
     bg = beautiful.bg_normal,
     shape_border_color = beautiful.bg_minimize,
     shape_border_width = beautiful.border_width,
     widget = wibox.container.background,
     { top = 2,
       bottom = 2,
       left = 6,
       right = 6,
       widget = wibox.container.margin,
       { widget = memory_chart } } }
}

-- Cache temperature value widgets
vicious.cache(vicious.widgets.thermal)

-- Create a temperature icon widget
local temperature_icon = wibox.widget {
   top = 4,
   bottom = 4,
   left = 2,
   right = 2,
   widget = wibox.container.margin,
   { image = beautiful.widget_temperature,
     widget = wibox.widget.imagebox }
}

-- Set temperature text value
local temperature_text_value = wibox.widget.textbox()
vicious.register(temperature_text_value, vicious.widgets.thermal, "$1°C", 20, { "coretemp.0/hwmon/hwmon0", "core" })

-- Create temperature text widget
local temperature_text_widget = wibox.widget {
   top = 2,
   bottom = 2,
   left = 2,
   right = 2,
   widget = wibox.container.margin,
   { shape = gears.shape.rectangle,
     bg = beautiful.bg_focus,
     shape_border_color = beautiful.bg_minimize,
     shape_border_width = beautiful.border_width,
     widget = wibox.container.background,
     { left = 6,
       right = 6,
       widget = wibox.container.margin,
       { widget = temperature_text_value } } }
}

-- Create a temperature bar widget
local temperature_bar = wibox.widget {
   forced_height = 12,
   margins = { top = 2,
               bottom = 2,
               left = 2,
               right = 2 },
   bar_shape = gears.shape.rounded_bar,
   shape = gears.shape.rounded_bar,
   paddings = 1,
   background_color = beautiful.border_normal,
   color = beautiful.widget_value,
   border_width = 1,
   border_color = beautiful.bg_minimize,
   widget = wibox.widget.progressbar
}

-- Set temperature bar value
vicious.register(temperature_bar, vicious.widgets.thermal, "$1", 20, { "coretemp.0/hwmon/hwmon0", "core" })

-- Rotate temperature bar widget
local temperature_widget = wibox.container.rotate(temperature_bar, "east")

-- Create a file system usage icon widget
local fs_icon = wibox.widget {
   top = 4,
   bottom = 4,
   left = 2,
   right = 2,
   widget = wibox.container.margin,
   { image = beautiful.widget_fs,
     widget = wibox.widget.imagebox }
}

-- Cache file system usage widgets
vicious.cache(vicious.widgets.fs)

-- Set file system usage text value
local fs_text_value = wibox.widget.textbox()
vicious.register(fs_text_value, vicious.widgets.fs, "${/ used_p}%", 300)

-- Create a file system usage text widget
local fs_text_widget = wibox.widget {
   top = 2,
   bottom = 2,
   left = 2,
   right = 2,
   widget = wibox.container.margin,
   { shape = gears.shape.rectangle,
     bg = beautiful.bg_focus,
     shape_border_color = beautiful.bg_minimize,
     shape_border_width = beautiful.border_width,
     widget = wibox.container.background,
     { left = 6,
       right = 6,
       widget = wibox.container.margin,
       { widget = fs_text_value } } }
}

-- Create a file system usage chart widget
local fs_chart = wibox.widget {
   max_value = 1,
   value = 0.25,
   thickness = 4,
   border_width = 1,
   bg = beautiful.bg_minimize,
   border_color = beautiful.bg_normal,
   colors = { beautiful.widget_value },
   widget = wibox.container.arcchart
}

-- Set file system usage chart value
vicious.register(fs_chart, vicious.widgets.fs, "${/ used_p}", 300)

-- Create a file system usage chart background
local fs_widget = wibox.widget {
   top = 2,
   bottom = 2,
   left = 2,
   right = 2,
   widget = wibox.container.margin,
   { shape = gears.shape.rectangle,
     bg = beautiful.bg_normal,
     shape_border_color = beautiful.bg_minimize,
     shape_border_width = beautiful.border_width,
     widget = wibox.container.background,
     { top = 2,
       bottom = 2,
       left = 6,
       right = 6,
       widget = wibox.container.margin,
       { widget = fs_chart } } }
}

-- Cache volume value widgets
vicious.cache(vicious.contrib.pulse)

-- Create a volume icon widget
local volume_icon = wibox.widget {
   top = 4,
   bottom = 4,
   left = 2,
   right = 2,
   widget = wibox.container.margin,
   { image = beautiful.widget_volume,
     widget = wibox.widget.imagebox }
}

-- Set volume text value
local volume_text_value = wibox.widget.textbox()
vicious.register(volume_text_value, vicious.contrib.pulse, "$1%", 6)

-- Create a volume text widget
local volume_text_widget = wibox.widget {
   top = 2,
   bottom = 2,
   left = 2,
   right = 2,
   widget = wibox.container.margin,
   { shape = gears.shape.rectangle,
     bg = beautiful.bg_focus,
     shape_border_color = beautiful.bg_minimize,
     shape_border_width = beautiful.border_width,
     widget = wibox.container.background,
     { left = 6,
       right = 6,
       widget = wibox.container.margin,
       { widget = volume_text_value } } }
}

-- Create volume bar widget
local volume_bar = wibox.widget {
   forced_height = 16,
   paddings = 1,
   margins = { top = 2,
               bottom = 2,
               left = 2,
               right = 2 },
   background_color = beautiful.border_normal,
   color = beautiful.widget_value,
   border_width = 1,
   border_color = beautiful.bg_minimize,
   widget = wibox.widget.progressbar
}

-- Set volume bar value
vicious.register(volume_bar, vicious.contrib.pulse, "$1", 6)

-- Set volume bar widget rotation
local volume_widget = wibox.container.rotate(volume_bar, "east")

-- Create a text clock icon widget
local clock_icon = wibox.widget {
   top = 4,
   bottom = 4,
   left = 2,
   right = 2,
   widget = wibox.container.margin,
   { image = beautiful.widget_clock,
     widget = wibox.widget.imagebox }
}

-- Create a text clock widget
local clock_widget = wibox.widget {
   top = 2,
   bottom = 2,
   left = 2,
   right = 2,
   widget = wibox.container.margin,
   { shape = gears.shape.rectangle,
     bg = beautiful.bg_focus,
     shape_border_color = beautiful.bg_minimize,
     shape_border_width = beautiful.border_width,
     widget = wibox.container.background,
     { left = 6,
       right = 6,
       widget = wibox.container.margin,
       { widget = wibox.widget.textclock("<span foreground='#f0c674'>%d-%m/%H:%M</span>") } } }
}

-- Buttonize widget
local month_calendar = awful.widget.calendar_popup.month({ font = beautiful.font })
month_calendar:attach(clock_widget, "br", { on_hover = false})

-- Create a wibox for each screen and add it
local taglist_buttons = gears.table.join(
   awful.button({ }, 1, function(t) t:view_only() end),
   awful.button({ modkey }, 1, function(t)
         if client.focus then
            client.focus:move_to_tag(t)
         end
   end),
   awful.button({ }, 3, awful.tag.viewtoggle),
   awful.button({ modkey }, 3, function(t)
         if client.focus then
            client.focus:toggle_tag(t)
         end
   end),
   awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
   awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
)

local tasklist_buttons = gears.table.join(
   awful.button({ }, 1, function (c)
         if c == client.focus then
            c.minimized = true
         else
            c:emit_signal(
               "request::activate",
               "tasklist",
               {raise = true}
            )
         end
   end),
   awful.button({ }, 3, function()
         awful.menu.client_list({ theme = { width = 250 } })
   end),
   awful.button({ }, 4, function ()
         awful.client.focus.byidx(1)
   end),
   awful.button({ }, 5, function ()
         awful.client.focus.byidx(-1)
end))

local function set_wallpaper(s)
   -- Wallpaper
   if beautiful.wallpaper then
      local wallpaper = beautiful.wallpaper
      -- If wallpaper is a function, call it with the screen
      if type(wallpaper) == "function" then
         wallpaper = wallpaper(s)
      end
      gears.wallpaper.maximized(wallpaper, s, true)
   end
end

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", set_wallpaper)

awful.screen.connect_for_each_screen(function(s)
      -- Wallpaper
      set_wallpaper(s)

      -- Add first tag
      awful.tag.add("1", {
                       icon               = beautiful.tag_term,
                       layout             = awful.layout.suit.tile,
                       screen             = s,
                       selected           = true,
      })

      -- Add second tag
      awful.tag.add("2", {
                       icon = beautiful.tag_web,
                       layout = awful.layout.suit.floating,
                       screen = s,
      })

      -- Add third tag
      awful.tag.add("3", {
                       icon = beautiful.tag_editor,
                       layout = awful.layout.suit.floating,
                       screen = s,
      })

      -- Add fourth tag
      awful.tag.add("4", {
                       icon = beautiful.tag_office,
                       layout = awful.layout.suit.floating,
                       screen = s,
      })

      -- Add fifth tag
      awful.tag.add("5", {
                       icon = beautiful.tag_utils,
                       layout = awful.layout.suit.floating,
                       screen = s,
      })

      -- Add sixth tag
      awful.tag.add("6", {
                       icon = beautiful.tag_misc,
                       layout = awful.layout.suit.floating,
                       screen = s,
      })

      -- Create a promptbox for each screen
      s.mypromptbox = awful.widget.prompt()
      -- Create an imagebox widget which will contain an icon indicating which layout we're using.
      -- We need one layoutbox per screen.
      s.mylayoutbox = wibox.container.margin(awful.widget.layoutbox(s), 2, 2, 4, 4)
      s.mylayoutbox:buttons(gears.table.join(
                               awful.button({ }, 1, function () awful.layout.inc( 1) end),
                               awful.button({ }, 3, function () awful.layout.inc(-1) end),
                               awful.button({ }, 4, function () awful.layout.inc( 1) end),
                               awful.button({ }, 5, function () awful.layout.inc(-1) end)))
      -- Create a taglist widget
      s.mytaglist = wibox.widget {
         top = 2,
         bottom = 2,
         left = 2,
         right = 2,
         widget = wibox.container.margin,
         { shape = gears.shape.rectangle,
           bg = beautiful.bg_focus,
           shape_border_color = beautiful.bg_minimize,
           shape_border_width = beautiful.border_width,
           widget = wibox.container.background,
           { top = 2,
             bottom = 2,
             left = 4,
             right = 4,
             widget = wibox.container.margin,
             { widget = awful.widget.taglist(s, awful.widget.taglist.filter.all, taglist_buttons) } } }
      }

      -- Create a tasklist widget
      s.mytasklist = wibox.container.margin(
         awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, tasklist_buttons), 2, 2, 2, 2
      )

      -- Create the wibox
      s.mywibox = awful.wibar({ position = "bottom", height = 24, screen = s })

      -- Add widgets to the wibox
      s.mywibox:setup {
         layout = wibox.layout.align.horizontal,
         { -- Left widgets
            layout = wibox.layout.fixed.horizontal,
            mylauncher,
            s.mytaglist,
            s.mylayoutbox,
            s.mypromptbox,
         },
         s.mytasklist, -- Middle widget
         { -- Right widgets
            layout = wibox.layout.fixed.horizontal,
            cpu_icon,
            cpu_text_widget,
            cpu_widget,
            memory_icon,
            memory_text_widget,
            memory_widget,
            temperature_icon,
            temperature_text_widget,
            temperature_widget,
            fs_icon,
            fs_text_widget,
            fs_widget,
            volume_icon,
            volume_text_widget,
            volume_widget,
            clock_icon,
            clock_widget,
            wibox.widget.systray(),
         },
      }
end)
-- }}}

-- {{{ Mouse bindings
root.buttons(gears.table.join(
                awful.button({ }, 3, function () mymainmenu:toggle() end),
                awful.button({ }, 4, awful.tag.viewnext),
                awful.button({ }, 5, awful.tag.viewprev)
))
-- }}}

-- {{{ Key bindings
local globalkeys = gears.table.join(
   awful.key({ modkey,           }, "s",      hotkeys_popup.show_help,
      {description="show help", group="awesome"}),
   awful.key({ modkey,           }, "Left",   awful.tag.viewprev,
      {description = "view previous", group = "tag"}),
   awful.key({ modkey,           }, "Right",  awful.tag.viewnext,
      {description = "view next", group = "tag"}),
   awful.key({ modkey,           }, "Escape", awful.tag.history.restore,
      {description = "go back", group = "tag"}),

   awful.key({ modkey,           }, "j",
      function ()
         awful.client.focus.byidx( 1)
      end,
      {description = "focus next by index", group = "client"}
   ),
   awful.key({ modkey,           }, "k",
      function ()
         awful.client.focus.byidx(-1)
      end,
      {description = "focus previous by index", group = "client"}
   ),
   awful.key({ modkey,           }, "w", function () mymainmenu:show() end,
      {description = "show main menu", group = "awesome"}),

   -- Layout manipulation
   awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end,
      {description = "swap with next client by index", group = "client"}),
   awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end,
      {description = "swap with previous client by index", group = "client"}),
   awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end,
      {description = "focus the next screen", group = "screen"}),
   awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end,
      {description = "focus the previous screen", group = "screen"}),
   awful.key({ modkey,           }, "u", awful.client.urgent.jumpto,
      {description = "jump to urgent client", group = "client"}),
   awful.key({ modkey,           }, "Tab",
      function ()
         awful.client.focus.history.previous()
         if client.focus then
            client.focus:raise()
         end
      end,
      {description = "go back", group = "client"}),

   -- Standard program
   awful.key({ modkey,           }, "Return", function () awful.spawn(terminal) end,
      {description = "open a terminal", group = "launcher"}),
   awful.key({ modkey, "Control" }, "r", awesome.restart,
      {description = "reload awesome", group = "awesome"}),
   awful.key({ modkey, "Shift"   }, "q", awesome.quit,
      {description = "quit awesome", group = "awesome"}),

   awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)          end,
      {description = "increase master width factor", group = "layout"}),
   awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)          end,
      {description = "decrease master width factor", group = "layout"}),
   awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1, nil, true) end,
      {description = "increase the number of master clients", group = "layout"}),
   awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1, nil, true) end,
      {description = "decrease the number of master clients", group = "layout"}),
   awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1, nil, true)    end,
      {description = "increase the number of columns", group = "layout"}),
   awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1, nil, true)    end,
      {description = "decrease the number of columns", group = "layout"}),
   awful.key({ modkey,           }, "space", function () awful.layout.inc( 1)                end,
      {description = "select next", group = "layout"}),
   awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(-1)                end,
      {description = "select previous", group = "layout"}),

   awful.key({ modkey, "Control" }, "n",
      function ()
         local c = awful.client.restore()
         -- Focus restored client
         if c then
            client.focus = c
            c:raise()
         end
      end,
      {description = "restore minimized", group = "client"}),

   -- Prompt
   awful.key({ modkey },            "r",     function () awful.screen.focused().mypromptbox:run() end,
      {description = "run prompt", group = "launcher"}),

   awful.key({ modkey }, "x",
      function ()
         awful.prompt.run {
            prompt       = "Run Lua code: ",
            textbox      = awful.screen.focused().mypromptbox.widget,
            exe_callback = awful.util.eval,
            history_path = awful.util.get_cache_dir() .. "/history_eval"
         }
      end,
      {description = "lua execute prompt", group = "awesome"}),
   -- Menubar
   awful.key({ modkey }, "p", function() menubar.show() end,
      {description = "show the menubar", group = "launcher"}),

   -- Volume control
   awful.key({ }, "XF86AudioRaiseVolume", function () awful.spawn.with_shell("pactl set-sink-volume 0 +5%") end,
      {description = "increase volume", group = "volume"}),
   awful.key({ }, "XF86AudioLowerVolume", function () awful.spawn.with_shell("pactl set-sink-volume 0 -5%") end,
      {description = "lower volume", group = "volume"}),
   awful.key({ }, "XF86AudioMute", function () awful.spawn.with_shell("pactl set-sink-mute 0 toggle") end,
      {description = "mute volume", group = "volume"}),

   -- Clipboard
   awful.key({ modkey }, "Insert", function () awful.spawn.with_shell("clipmenusel") end,
      {description = "clipboard menu", group = "clipboard"}),

   -- Screenshot grabbing
   awful.key({ }, "Print", function () awful.spawn.with_shell("maim -u /tmp/screenshot-$(date +%s).png") end,
      {description = "screenshot desktop", group = "screenshot"}),
   awful.key({ modkey }, "Print", function () awful.spawn.with_shell("maim -su -f png | curl -s -F'file=@-' https://0x0.st | tr -d '\n' | xsel -b") end,
      {description = "screenshot selection", group = "screenshot"})
)

local clientkeys = gears.table.join(
   awful.key({ modkey,           }, "f",
      function (c)
         c.fullscreen = not c.fullscreen
         c:raise()
      end,
      {description = "toggle fullscreen", group = "client"}),
   awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end,
      {description = "close", group = "client"}),
   awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ,
      {description = "toggle floating", group = "client"}),
   awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end,
      {description = "move to master", group = "client"}),
   awful.key({ modkey,           }, "o",      function (c) c:move_to_screen()               end,
      {description = "move to screen", group = "client"}),
   awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end,
      {description = "toggle keep on top", group = "client"}),
   awful.key({ modkey,           }, "n",
      function (c)
         -- The client currently has the input focus, so it cannot be
         -- minimized, since minimized clients can't have the focus.
         c.minimized = true
      end ,
      {description = "minimize", group = "client"}),
   awful.key({ modkey,           }, "m",
      function (c)
         c.maximized = not c.maximized
         c:raise()
      end ,
      {description = "(un)maximize", group = "client"}),
   awful.key({ modkey, "Control" }, "m",
      function (c)
         c.maximized_vertical = not c.maximized_vertical
         c:raise()
      end ,
      {description = "(un)maximize vertically", group = "client"}),
   awful.key({ modkey, "Shift"   }, "m",
      function (c)
         c.maximized_horizontal = not c.maximized_horizontal
         c:raise()
      end ,
      {description = "(un)maximize horizontally", group = "client"})
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it work on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
   globalkeys = gears.table.join(globalkeys,
                                 -- View tag only.
                                 awful.key({ modkey }, "#" .. i + 9,
                                    function ()
                                       local screen = awful.screen.focused()
                                       local tag = screen.tags[i]
                                       if tag then
                                          tag:view_only()
                                       end
                                    end,
                                    {description = "view tag #"..i, group = "tag"}),
                                 -- Toggle tag display.
                                 awful.key({ modkey, "Control" }, "#" .. i + 9,
                                    function ()
                                       local screen = awful.screen.focused()
                                       local tag = screen.tags[i]
                                       if tag then
                                          awful.tag.viewtoggle(tag)
                                       end
                                    end,
                                    {description = "toggle tag #" .. i, group = "tag"}),
                                 -- Move client to tag.
                                 awful.key({ modkey, "Shift" }, "#" .. i + 9,
                                    function ()
                                       if client.focus then
                                          local tag = client.focus.screen.tags[i]
                                          if tag then
                                             client.focus:move_to_tag(tag)
                                          end
                                       end
                                    end,
                                    {description = "move focused client to tag #"..i, group = "tag"}),
                                 -- Toggle tag on focused client.
                                 awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                                    function ()
                                       if client.focus then
                                          local tag = client.focus.screen.tags[i]
                                          if tag then
                                             client.focus:toggle_tag(tag)
                                          end
                                       end
                                    end,
                                    {description = "toggle focused client on tag #" .. i, group = "tag"})
   )
end

local clientbuttons = gears.table.join(
   awful.button({ }, 1, function (c)
         c:emit_signal("request::activate", "mouse_click", {raise = true})
   end),
   awful.button({ modkey }, 1, function (c)
         c:emit_signal("request::activate", "mouse_click", {raise = true})
         awful.mouse.client.move(c)
   end),
   awful.button({ modkey }, 3, function (c)
         c:emit_signal("request::activate", "mouse_click", {raise = true})
         awful.mouse.client.resize(c)
   end)
)

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
   -- All clients will match this rule.
   { rule = { },
     properties = { border_width = beautiful.border_width,
                    border_color = beautiful.border_normal,
                    focus = awful.client.focus.filter,
                    size_hints_honor = false,
                    raise = true,
                    keys = clientkeys,
                    buttons = clientbuttons,
                    screen = awful.screen.preferred,
                    placement = awful.placement.no_overlap+awful.placement.no_offscreen
     }
    },

   -- Floating clients.
   { rule_any = {
        instance = {
           "copyq", -- Includes session name in class.
           "pinentry",
        },
        class = {
           "Pidgin",
           "mpv"
        },
        name = {
           "Event Tester", -- xev
        },
        role = {
           "pop-up", -- e.g. Google Chrome's (detached) Developer Tools
        }
   }, properties = { floating = true }},

   -- Add titlebars to normal clients and dialogs
   { rule_any = { type = { "normal", "dialog" }
                }, properties = { titlebars_enabled = true }
   },

   -- Set Firefox to always map on the tag named "2" on screen 1
   { rule = { class = "Firefox" },
     properties = { screen = 1, tag = "2", titlebars_enabled = false } },
   { rule = { class = "Tor Browser" },
     properties = { screen = 1, tag = "2", titlebars_enabled = false } },
   -- Wine
   { rule = { class = "Wine" },
     properties = { titlebars_enabled = false } },
   -- Map the rest of the applications
   { rule = { class = "Emacs" },
     properties = { screen = 1, tag = "3", maximized = true } },
   { rule = { class = "libreoffice" },
     properties = { screen = 1, tag = "4" } },
   { rule = { class = "Zathura" },
     properties = { screen = 1, tag = "4" } },
   { rule = { class = "Pavucontrol" },
     properties = { screen = 1, tag = "5" } },
   { rule = { class = "Gimp" },
     properties = { screen = 1, tag = "5" } },
   { rule = { class = "Pcmanfm" },
     properties = { screen = 1, tag = "5" } },
   { rule = { class = "Ripcord" },
     properties = { screen = 1, tag = "6" } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c)
                         -- Set the windows at the slave,
                         -- i.e. put it at the end of others instead of setting it master.
                         -- if not awesome.startup then awful.client.setslave(c) end

                         if awesome.startup and
                            not c.size_hints.user_position
                         and not c.size_hints.program_position then
                            -- Prevent clients from being unreachable after screen count changes.
                            awful.placement.no_offscreen(c)
                         end
end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
                         -- buttons for the titlebar
                         local buttons = gears.table.join(
                            awful.button({ }, 1, function()
                                  c:emit_signal("request::activate", "titlebar", {raise = true})
                                  awful.mouse.client.move(c)
                            end),
                            awful.button({ }, 3, function()
                                  c:emit_signal("request::activate", "titlebar", {raise = true})
                                  awful.mouse.client.resize(c)
                            end)
                         )

                         awful.titlebar(c, { size = 24 }) : setup {
                            { -- Left
                               awful.titlebar.widget.iconwidget(c),
                               buttons = buttons,
                               layout  = wibox.layout.fixed.horizontal
                            },
                            { -- Middle
                               { -- Title
                                  align  = "center",
                                  widget = awful.titlebar.widget.titlewidget(c)
                               },
                               buttons = buttons,
                               layout  = wibox.layout.flex.horizontal
                            },
                            { -- Right
                               awful.titlebar.widget.floatingbutton (c),
                               awful.titlebar.widget.stickybutton   (c),
                               awful.titlebar.widget.ontopbutton    (c),
                               awful.titlebar.widget.minimizebutton (c),
                               awful.titlebar.widget.maximizedbutton(c),
                               awful.titlebar.widget.closebutton    (c),
                               layout = wibox.layout.fixed.horizontal()
                            },
                            layout = wibox.layout.align.horizontal
                                                                  }
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}
