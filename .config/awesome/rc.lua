-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
awful.rules = require("awful.rules")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")
-- Vicious widget library
local vicious = require("vicious")
-- Scratchpad manager
local scratch = require("scratch")

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
                                              text = err })
                             in_error = false
                                          end)
end
-- }}}

-- {{{ Variable definitions
-- Themes define colours, icons, and wallpapers
themes_dir = os.getenv("HOME") .. "/.config/awesome/themes"
beautiful.init(themes_dir .. "/jellybeans/theme.lua")

-- This is used later as the default terminal and editor to run.
terminal = "st"
editor = os.getenv("EDITOR")

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
local layouts =
   {
      awful.layout.suit.tile,
      awful.layout.suit.tile.left,
      awful.layout.suit.tile.bottom,
      awful.layout.suit.tile.top,
      awful.layout.suit.floating,
      awful.layout.suit.fair,
      awful.layout.suit.fair.horizontal,
      awful.layout.suit.spiral,
      awful.layout.suit.spiral.dwindle,
      awful.layout.suit.max,
      awful.layout.suit.max.fullscreen,
      awful.layout.suit.magnifier
   }
-- }}}

-- {{{ Wallpaper
if beautiful.wallpaper then
   for s = 1, screen.count() do
      gears.wallpaper.maximized(beautiful.wallpaper, s, true)
   end
end
-- }}}

-- {{{ Tags
-- Define a tag table which hold all screen tags.
tags = {
   names = { "1", "2", "3", "4", "5" },
   layout = { layouts[1], layouts[10], layouts[1], layouts[5], layouts[10] }
}
for s = 1, screen.count() do
   tags[s] = awful.tag(tags.names, s, tags.layout)
end
-- }}}

-- {{{ Menu
-- Create a laucher widget and a main menu
myawesomemenu = {
   { "Manual", terminal .. " -e 'man awesome'", beautiful.menu_manual },
   { "Edit rc", editor .. " " .. awesome.conffile, beautiful.menu_edit },
   { "Restart", awesome.restart, beautiful.menu_restart },
   { "Quit", awesome.quit, beautiful.menu_quit }
}

mymainmenu = awful.menu({ items = { { "st", terminal, beautiful.menu_term },
                                    { "Conkeror", "conkeror", beautiful.menu_browser },
                                    { "Emacs", "emacs", beautiful.menu_emacs },
                                    { "GIMP", "gimp", beautiful.menu_gimp },
                                    { "awesome", myawesomemenu, beautiful.awesome_icon }
                                    
}
                        })

mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
                                     menu = mymainmenu })

-- {{{ Separator
separator = wibox.widget.imagebox()
separator:set_image(beautiful.widget_sep)
-- }}}

-- {{{ CPU usage widgets
cpuicon = wibox.widget.imagebox()
cpuicon:set_image(beautiful.widget_cpu)
-- Initialize widgets
cpuwidget = wibox.widget.textbox()
cpugraph = awful.widget.graph()
-- Graph properties
cpugraph:set_width(45)
cpugraph:set_background_color(beautiful.bg_normal)
cpugraph:set_color(beautiful.fg_focus)
cpugraph:set_border_color(beautiful.border_normal)
-- Enable caching
vicious.cache(vicious.widgets.cpu)
-- Register widgets
vicious.register(cpuwidget, vicious.widgets.cpu, "$1%", 2)
vicious.register(cpugraph, vicious.widgets.cpu, "$1", 2)
-- }}}

-- {{{ Memory usage widgets
memicon = wibox.widget.imagebox()
memicon:set_image(beautiful.widget_mem)
-- Initialize widgets
memwidget = wibox.widget.textbox()
membar = awful.widget.progressbar()
-- Progressbar properties
membar:set_vertical(true):set_ticks(true)
membar:set_width(15):set_ticks_size(3)
membar:set_background_color(beautiful.bg_normal)
membar:set_color(beautiful.fg_focus)
membar:set_border_color(beautiful.border_normal)
-- Enable caching
vicious.cache(vicious.widgets.mem)
-- Register widgets
vicious.register(memwidget, vicious.widgets.mem, "$1%", 4)
vicious.register(membar, vicious.widgets.mem, "$1", 4)
-- }}}

-- {{{ CPU temperature widgets
tempicon = wibox.widget.imagebox()
tempicon:set_image(beautiful.widget_temp)
--Initialize widgets
tempwidget = wibox.widget.textbox()
tempbar = awful.widget.progressbar()
-- Graph properties
tempbar:set_vertical(true):set_ticks(true)
tempbar:set_width(15):set_ticks_size(3)
tempbar:set_background_color(beautiful.bg_normal)
tempbar:set_color(beautiful.fg_focus)
tempbar:set_border_color(beautiful.border_normal)
-- Enable caching
vicious.cache(vicious.widgets.thermal)
-- Register widgets
vicious.register(tempwidget, vicious.widgets.thermal, "$1°", 20, "thermal_zone0")
vicious.register(tempbar, vicious.widgets.thermal, "$1", 20, "thermal_zone0")
-- }}}

-- {{{ Disk usage widgets
diskicon = wibox.widget.imagebox()
diskicon:set_image(beautiful.widget_disk)
-- Initialize widgets
dperc = { r = wibox.widget.textbox(), h = wibox.widget.textbox() }
dusage = { r = awful.widget.progressbar(), h = awful.widget.progressbar() }
-- Progressbar properties
for _, dstyle in pairs(dusage) do
   dstyle:set_vertical(true):set_ticks(true)
   dstyle:set_width(7):set_ticks_size(3)
   dstyle:set_background_color(beautiful.bg_normal)
   dstyle:set_color(beautiful.fg_focus)
   dstyle:set_border_color(beautiful.border_normal)
end
-- Enable caching
vicious.cache(vicious.widgets.fs)
-- Register widgets
vicious.register(dperc.r, vicious.widgets.fs, "${/ used_p}%", 300)
vicious.register(dperc.h, vicious.widgets.fs, "${/home used_p}%", 300)
vicious.register(dusage.r, vicious.widgets.fs, "${/ used_p}", 300)
vicious.register(dusage.h, vicious.widgets.fs, "${/home used_p}", 300)
-- }}}

-- {{{ Volume widgets
volicon = wibox.widget.imagebox()
volicon:set_image(beautiful.widget_vol)
-- Initialize widgets
volwidget = wibox.widget.textbox()
volbar = awful.widget.progressbar()
-- Graph properties
volbar:set_vertical(true):set_ticks(true)
volbar:set_width(15):set_ticks_size(3)
volbar:set_background_color(beautiful.bg_normal)
volbar:set_color(beautiful.fg_focus)
volbar:set_border_color(beautiful.border_normal)
-- Enable caching
vicious.cache(vicious.contrib.pulse)
-- Register widgets
vicious.register(volwidget, vicious.contrib.pulse, "$1%", 2, "alsa_output.pci-0000_00_11.5.analog-stereo")
vicious.register(volbar, vicious.contrib.pulse, "$1", 2, "alsa_output.pci-0000_00_11.5.analog-stereo")
volbar:buttons(awful.util.table.join(
                  awful.button({ }, 4, function ()
                                  vicious.contrib.pulse.add(5,"alsa_output.pci-0000_00_11.5.analog-stereo") end),
                  awful.button({ }, 5, function ()
                                  vicious.contrib.pulse.add(-5,"alsa_output.pci-0000_00_11.5.analog-stereo") end)
                                    ))
-- }}}

-- {{{ Date widget
dateicon = wibox.widget.imagebox()
dateicon:set_image(beautiful.widget_date)
-- Initialize widget
datewidget = wibox.widget.textbox()
-- Register widget
vicious.register(datewidget, vicious.widgets.date, "%d-%m/%R", 60)
-- }}}

-- {{{ Wibox
-- Create a wibox for each screen and add it
mywibox = {}
mypromptbox = {}
mylayoutbox = {}
mytaglist = {}
mytaglist.buttons = awful.util.table.join(
   awful.button({ }, 1, awful.tag.viewonly),
   awful.button({ modkey }, 1, awful.client.movetotag),
   awful.button({ }, 3, awful.tag.viewtoggle),
   awful.button({ modkey }, 3, awful.client.toggletag),
   awful.button({ }, 4, function(t) awful.tag.viewnext(awful.tag.getscreen(t)) end),
   awful.button({ }, 5, function(t) awful.tag.viewprev(awful.tag.getscreen(t)) end)
)
mytasklist = {}
mytasklist.buttons = awful.util.table.join(
   awful.button({ }, 1, function (c)
                   if c == client.focus then
                      c.minimized = true
                   else
                      -- Without this, the following
                      -- :isvisible() makes no sense
                      c.minimized = false
                      if not c:isvisible() then
                         awful.tag.viewonly(c:tags()[1])
                      end
                      -- This will also un-minimize
                      -- the client, if needed
                      client.focus = c
                      c:raise()
                   end
                        end),
   awful.button({ }, 3, function ()
                   if instance then
                      instance:hide()
                      instance = nil
                   else
                      instance = awful.menu.clients({ width=250 })
                   end
                        end),
   awful.button({ }, 4, function ()
                   awful.client.focus.byidx(1)
                   if client.focus then client.focus:raise() end
                        end),
   awful.button({ }, 5, function ()
                   awful.client.focus.byidx(-1)
                   if client.focus then client.focus:raise() end
                        end))

for s = 1, screen.count() do
   -- Create a promptbox for each screen
   mypromptbox[s] = awful.widget.prompt()
   -- Create an imagebox widget which will contains an icon indicating which layout we're using.
   -- We need one layoutbox per screen.
   mylayoutbox[s] = awful.widget.layoutbox(s)
   mylayoutbox[s]:buttons(awful.util.table.join(
                             awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
                             awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
                             awful.button({ }, 4, function () awful.layout.inc(layouts, 1) end),
                             awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)))
   -- Create a taglist widget
   mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.filter.all, mytaglist.buttons)

   -- Create a tasklist widget
   mytasklist[s] = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, mytasklist.buttons)

   -- Create the wibox
   mywibox[s] = awful.wibox({ position = "top", screen = s })

   -- Widgets that are aligned to the left
   local left_layout = wibox.layout.fixed.horizontal()
   left_layout:add(mylauncher)
   left_layout:add(mytaglist[s])
   left_layout:add(mylayoutbox[s])
   left_layout:add(mypromptbox[s])

   -- Widgets that are aligned to the right
   local right_layout = wibox.layout.fixed.horizontal()
   right_layout:add(cpuicon)
   right_layout:add(cpuwidget)
   right_layout:add(cpugraph)
   right_layout:add(separator)
   right_layout:add(memicon)
   right_layout:add(memwidget)
   right_layout:add(membar)
   right_layout:add(separator)
   right_layout:add(tempicon)
   right_layout:add(tempwidget)
   right_layout:add(tempbar)
   right_layout:add(separator)
   right_layout:add(diskicon)
   right_layout:add(dperc.r)
   right_layout:add(dusage.r)
   right_layout:add(dperc.h)
   right_layout:add(dusage.h)
   right_layout:add(separator)
   right_layout:add(volicon)
   right_layout:add(volwidget)
   right_layout:add(volbar)
   right_layout:add(separator)
   right_layout:add(dateicon)
   right_layout:add(datewidget)
   right_layout:add(wibox.widget.systray())

   -- Now bring it all together (with the tasklist in the middle)
   local layout = wibox.layout.align.horizontal()
   layout:set_left(left_layout)
   layout:set_middle(mytasklist[s])
   layout:set_right(right_layout)

   mywibox[s]:set_widget(layout)
end
-- }}}

-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
                awful.button({ }, 3, function () mymainmenu:toggle() end),
                awful.button({ }, 4, awful.tag.viewnext),
                awful.button({ }, 5, awful.tag.viewprev)
                                  ))
-- }}}

-- {{{ Key bindings
globalkeys = awful.util.table.join(
   awful.key({ modkey,           }, "Left",   awful.tag.viewprev       ),
   awful.key({ modkey,           }, "Right",  awful.tag.viewnext       ),
   awful.key({ modkey,           }, "Escape", awful.tag.history.restore),

   awful.key({ modkey,           }, "j",
             function ()
                awful.client.focus.byidx( 1)
                if client.focus then client.focus:raise() end
             end),
   awful.key({ modkey,           }, "k",
             function ()
                awful.client.focus.byidx(-1)
                if client.focus then client.focus:raise() end
             end),
   awful.key({ modkey,           }, "w", function () mymainmenu:show() end),

   -- Layout manipulation
   awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end),
   awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end),
   awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end),
   awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end),
   awful.key({ modkey,           }, "u", awful.client.urgent.jumpto),
   awful.key({ modkey,           }, "Tab",
             function ()
                awful.client.focus.history.previous()
                if client.focus then
                   client.focus:raise()
                end
             end),

   -- Standard program
   awful.key({ modkey,           }, "Return", function () awful.util.spawn(terminal) end),
   awful.key({ modkey, "Control" }, "r", awesome.restart),
   awful.key({ modkey, "Shift"   }, "q", awesome.quit),

   awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)    end),
   awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)    end),
   awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1)      end),
   awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1)      end),
   awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1)         end),
   awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1)         end),
   awful.key({ modkey,           }, "space", function () awful.layout.inc(layouts,  1) end),
   awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(layouts, -1) end),

   awful.key({ modkey, "Control" }, "n", awful.client.restore),

   -- Prompt
   awful.key({ modkey }, "p", function () mypromptbox[mouse.screen]:run() end),

   -- Scratchpad
   awful.key({ modkey }, "s", function () scratch.drop(terminal .. " -e ncmpcpp", "center", "center", 600, 300) end),

   awful.key({ modkey }, "x",
             function ()
                awful.prompt.run({ prompt = "Run Lua code: " },
                                 mypromptbox[mouse.screen].widget,
                                 awful.util.eval, nil,
                                 awful.util.getdir("cache") .. "/history_eval")
             end)
)

clientkeys = awful.util.table.join(
   awful.key({ modkey,           }, "f",      function (c) c.fullscreen = not c.fullscreen  end),
   awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end),
   awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ),
   awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end),
   awful.key({ modkey,           }, "o",      awful.client.movetoscreen                        ),
   awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end),
   awful.key({ modkey,           }, "n",
             function (c)
                -- The client currently has the input focus, so it cannot be
                -- minimized, since minimized clients can't have the focus.
                c.minimized = true
             end),
   awful.key({ modkey,           }, "m",
             function (c)
                c.maximized_horizontal = not c.maximized_horizontal
                c.maximized_vertical   = not c.maximized_vertical
             end)
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
   globalkeys = awful.util.table.join(globalkeys,
                                      awful.key({ modkey }, "#" .. i + 9,
                                                function ()
                                                   local screen = mouse.screen
                                                   local tag = awful.tag.gettags(screen)[i]
                                                   if tag then
                                                      awful.tag.viewonly(tag)
                                                   end
                                                end),
                                      awful.key({ modkey, "Control" }, "#" .. i + 9,
                                                function ()
                                                   local screen = mouse.screen
                                                   local tag = awful.tag.gettags(screen)[i]
                                                   if tag then
                                                      awful.tag.viewtoggle(tag)
                                                   end
                                                end),
                                      awful.key({ modkey, "Shift" }, "#" .. i + 9,
                                                function ()
                                                   local tag = awful.tag.gettags(client.focus.screen)[i]
                                                   if client.focus and tag then
                                                      awful.client.movetotag(tag)
                                                   end
                                                end),
                                      awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                                                function ()
                                                   local tag = awful.tag.gettags(client.focus.screen)[i]
                                                   if client.focus and tag then
                                                      awful.client.toggletag(tag)
                                                   end
                                                end))
end

clientbuttons = awful.util.table.join(
   awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
   awful.button({ modkey }, 1, awful.mouse.client.move),
   awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
awful.rules.rules = {
   -- All clients will match this rule.
   { rule = { },
     properties = { border_width = beautiful.border_width,
                    border_color = beautiful.border_normal,
                    focus = awful.client.focus.filter,
                    keys = clientkeys,
                    buttons = clientbuttons,
                    size_hints_honor = false } },
   { rule = { class = "pinentry" },
     properties = { floating = true } },
   -- Set applications to always map on specified tags
   { rule = { class = "Conkeror" },
     properties = { tag = tags[1][2] } },
   { rule = { class = "Emacs" },
     properties = { tag = tags[1][3] } },
   { rule = { class = "mpv" },
     properties = { tag = tags[1][4] } },
   { rule = { class = "Gimp" },
     properties = { tag = tags[1][5] } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c, startup)
                         if not startup then

                            -- Put windows in a smart way, only if they do not set an initial position.
                            if not c.size_hints.user_position and not c.size_hints.program_position then
                               awful.placement.no_overlap(c)
                               awful.placement.no_offscreen(c)
                            end
                         end
                                end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}
