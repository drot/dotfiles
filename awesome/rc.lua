-- Standard awesome library
require("awful")
require("awful.autofocus")
require("awful.rules")
-- Theme handling library
require("beautiful")
-- Widget display
require("vicious")
-- Scratchpad manager
require("scratch")
-- Dynamic tagging
require("eminent")

-- {{{ Variable definitions
-- Themes define colours, icons, and wallpapers
beautiful.init("/home/dr/.config/awesome/themes/gigamo/theme.lua")

-- This is used later as the default terminal to run.
terminal = "urxvtc"

-- Default modkey.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
layouts = {
   awful.layout.suit.tile,        -- 1
   awful.layout.suit.tile.left,   -- 2
   awful.layout.suit.tile.bottom, -- 3
   awful.layout.suit.tile.top,    -- 4
   awful.layout.suit.max,         -- 5
   awful.layout.suit.magnifier,   -- 6
   awful.layout.suit.floating     -- 7
}
-- }}}

-- {{{ Tags
-- Define a tag table which hold all screen tags.
tags = {
   names  = { "1", "2", "3", "4", "5", "6", "7", "8", "9" },
   layout = { layouts[1], layouts[6], layouts[1], layouts[7], layouts[1], layouts[1], layouts[1], layouts[1], layouts[1] }
     }

for s = 1, screen.count() do
   tags[s] = awful.tag(tags.names, s, tags.layout)
end
-- }}}

-- Wibox

-- {{{ Reusable separators
separator = widget({ type = "imagebox" })
separator.image = image(beautiful.widget_sep)
-- }}}

-- {{{ CPU usage and temperature
cpuicon = widget({ type = "imagebox" })
cpuicon.image = image(beautiful.widget_cpu)
-- Initialize widget
cpugraph  = awful.widget.graph()
tzswidget = widget({ type = "textbox" })
-- Graph properties
cpugraph:set_width(40):set_height(13)
cpugraph:set_background_color(beautiful.fg_off_widget)
cpugraph:set_gradient_colors({ beautiful.fg_end_widget,
beautiful.fg_center_widget, beautiful.fg_widget })
-- Register widgets
vicious.register(cpugraph,  vicious.widgets.cpu,     "$1")
vicious.register(tzswidget, vicious.widgets.thermal, "$1°", 19, "thermal_zone0")
-- }}}

-- {{{ Memory usage
memicon = widget({ type = "imagebox" })
memicon.image = image(beautiful.widget_mem)
-- Initialize widget
membar = awful.widget.progressbar()
-- Pogressbar properties
membar:set_vertical(true):set_ticks(true)
membar:set_height(14):set_width(10):set_ticks_size(1)
membar:set_background_color(beautiful.fg_off_widget)
membar:set_border_color(beautiful.border_widget)
membar:set_gradient_colors({ beautiful.fg_widget,
beautiful.fg_center_widget, beautiful.fg_end_widget})
-- Register widget
vicious.register(membar, vicious.widgets.mem, "$1", 13)
-- }}}

-- {{{ Volume level
volicon = widget({ type = "imagebox" })
volicon.image = image(beautiful.widget_vol)
-- Initialize widgets
volbar    = awful.widget.progressbar()
volwidget = widget({ type = "textbox" })
-- Progressbar properties
volbar:set_vertical(true):set_ticks(true)
volbar:set_height(14):set_width(10):set_ticks_size(1)
volbar:set_border_color(beautiful.border_widget)
volbar:set_gradient_colors({ beautiful.fg_widget,
   beautiful.fg_center_widget, beautiful.fg_end_widget
}) -- Enable caching
vicious.cache(vicious.widgets.volume)
-- Register widgets
vicious.register(volbar, vicious.widgets.volume, "$1", 2, "PCM")
vicious.register(volwidget, vicious.widgets.volume, " $1%", 2, "PCM")
-- Register buttons
volbar.widget:buttons(awful.util.table.join(
 awful.button({ }, 1, function () awful.util.spawn("amixer -q set PCM 2dB+", false) end),
 awful.button({ }, 3, function () awful.util.spawn("amixer -q set PCM 2dB-", false) end)
)) -- Register assigned buttons
volwidget:buttons(volbar.widget:buttons())
-- }}}

-- {{{ Uptime widget
uptimeicon = widget({ type = "imagebox" })
uptimeicon.image = image(beautiful.widget_uptime)
uptimewidget = widget({ type = 'textbox' })
vicious.register(uptimewidget, vicious.widgets.uptime, "$4/$5/$6, $1d $2:$3", 61)
-- }}}

-- {{{ Network usage
dnicon = widget({ type = "imagebox" })
upicon = widget({ type = "imagebox" })
dnicon.image = image(beautiful.widget_net)
upicon.image = image(beautiful.widget_netup)
-- Initialize widget
netwidget = widget({ type = "textbox" })
-- Register widget
vicious.register(netwidget, vicious.widgets.net, "${eth0 down_kb}/${eth0 up_kb}", 3)
-- }}}

-- {{{ Filesystem usage
-- Initialize widget
fsicon = widget({ type = "imagebox" })
fsicon.image = image(beautiful.widget_fs)
fs = {b = awful.widget.progressbar(), r = awful.widget.progressbar(),
      v = awful.widget.progressbar(), h = awful.widget.progressbar()}
-- Progressbar properties
for _, w in pairs(fs) do
   w:set_vertical(true):set_ticks(true)
   w:set_height(14):set_width(6):set_ticks_size(1)
   w:set_background_color(beautiful.fg_off_widget)
   w:set_border_color(beautiful.border_widget)
   w:set_gradient_colors({ beautiful.fg_widget,beautiful.fg_center_widget, beautiful.fg_end_widget})
end
-- Enable caching
vicious.cache(vicious.widgets.fs)
-- Register widget
vicious.register(fs.b, vicious.widgets.fs, "${/boot used_p}", 500)
vicious.register(fs.r, vicious.widgets.fs, "${/ used_p}", 500)
vicious.register(fs.v, vicious.widgets.fs, "${/var used_p}", 500)
vicious.register(fs.h, vicious.widgets.fs, "${/home used_p}", 500)
-- }}}

-- {{{ Pkg update widget
-- Initialize widget
pkgicon = widget({ type = "imagebox" })
pkgicon.image = image(beautiful.widget_pkg)
pkgwidget = widget({ type = "textbox"})
-- Register widget
vicious.register(pkgwidget, vicious.widgets.pkg, "$1 updates", 3700, "Arch")
-- }}}

-- {{{ GMail widget
-- Initialize widget
gmailicon = widget({ type = "imagebox" })
gmailicon.image = image(beautiful.widget_mail)
gmailwidget = widget({ type = "textbox" })
--Register widget
vicious.register(gmailwidget, vicious.widgets.gmail, "${count}, ${subject}", 300)
-- }}}

-- {{{ Date widget
-- Initialize widget
dateicon = widget({ type = "imagebox" })
dateicon.image = image(beautiful.widget_date)
datewidget = widget({ type = "textbox" })
-- Register widget
vicious.register(datewidget, vicious.widgets.date, "%d-%m/%R", 60)
-- }}}

-- {{{ Weather widget
-- Initialize widget
weathericon = widget({ type = "imagebox" })
weathericon.image = image(beautiful.widget_weather)
weatherwidget = widget({ type = "textbox" })
-- Register widget
vicious.register(weatherwidget, vicious.widgets.weather, "${tempc}°, ${sky}", 3600, "LQMO")
-- }}}

-- {{{ MPD widget
-- Initialize widget
mpdicon = widget({ type = "imagebox" })
mpdicon.image = image(beautiful.widget_music)
mpdwidget = widget({ type = 'textbox' })
-- Register widget
vicious.register(mpdwidget, vicious.widgets.mpd, "${Artist} - ${Title}")
-- }}}

-- {{{ Wibox initialisation
wibox_top = {}
wibox_bottom = {}
promptbox = {}
layoutbox = {}
taglist   = {}
taglist.buttons = awful.util.table.join(
	awful.button({ }, 1, awful.tag.viewonly),
	awful.button({ modkey }, 1, awful.client.movetotag),
	awful.button({ }, 3, awful.tag.viewtoggle),
	awful.button({ modkey }, 3, awful.client.toggletag),
	awful.button({ }, 4, awful.tag.viewnext),
	awful.button({ }, 5, awful.tag.viewprev))

tasklist = {}
tasklist.buttons = awful.util.table.join(
	awful.button({ }, 1, function (c)
	if not c:isvisible() then
		awful.tag.viewonly(c:tags()[1])
	end
		client.focus = c
		c:raise()
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
	-- Create a promptbox
	promptbox[s] = awful.widget.prompt({ layout = awful.widget.layout.horizontal.leftright })
	-- Create a layoutbox
	layoutbox[s] = awful.widget.layoutbox(s)
	layoutbox[s]:buttons(awful.util.table.join(
	awful.button({ }, 1, function () awful.layout.inc(layouts, 1)  end),
	awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
	awful.button({ }, 4, function () awful.layout.inc(layouts, 1)  end),
	awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)
	))
	-- Create the taglist
	taglist[s] = awful.widget.taglist(s, awful.widget.taglist.label.all, taglist.buttons)
	-- Create a tasklist widget
	tasklist[s] = awful.widget.tasklist(function(c)
					       return awful.widget.tasklist.label.currenttags(c, s)
					    end, tasklist.buttons)

	-- Create the wiboxen
	-- Top
	wibox_top[s] = awful.wibox({screen = s,
	fg = beautiful.fg_normal, height = 14,
	bg = beautiful.bg_normal, position = "top",
	border_color = beautiful.border_focus,
	border_width = beautiful.border_width
})
	-- Bottom
	wibox_bottom[s] = awful.wibox({screen = s,
	fg = beautiful.fg_normal, height = 14,
	bg = beautiful.bg_normal, position = "bottom",
	border_color = beautiful.border_focus,
	border_width = beautiful.border_width
})

-- Add widgets to the wiboxen
wibox_top[s].widgets = {
	{   taglist[s], layoutbox[s], promptbox[s],
	    ["layout"] = awful.widget.layout.horizontal.leftright
	},
	datewidget, dateicon,
	separator, upicon, netwidget, dnicon,
	separator, fs.h.widget, fs.v.widget, fs.r.widget, fs.b.widget, fsicon,
	separator, membar.widget, memicon,
	separator, cpugraph.widget, tzswidget, cpuicon,
	tasklist[s],
	["layout"] = awful.widget.layout.horizontal.rightleft
}

wibox_bottom[s].widgets = {
   { pkgicon, pkgwidget, separator,
      weathericon, weatherwidget, separator,
      gmailicon, gmailwidget,
      ["layout"] = awful.widget.layout.horizontal.leftright
   },
   uptimewidget, uptimeicon, separator,
   volwidget, volbar.widget, volicon, separator,
   mpdwidget, mpdicon,
   ["layout"] = awful.widget.layout.horizontal.rightleft
}
end
-- }}}

-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
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
    awful.key({ modkey            }, "s", function () scratch.drop("urxvtc -e ncmpcpp", "bottom") end),
    awful.key({ modkey, "Control" }, "r", awesome.restart),
    awful.key({ modkey, "Shift"   }, "q", awesome.quit),

    awful.key({ modkey,           }, "l", function () awful.tag.incmwfact( 0.05)    end),
    awful.key({ modkey,           }, "h", function () awful.tag.incmwfact(-0.05)    end),
    awful.key({ modkey, "Shift"   }, "h", function () awful.tag.incnmaster( 1)      end),
    awful.key({ modkey, "Shift"   }, "l", function () awful.tag.incnmaster(-1)      end),
    awful.key({ modkey, "Control" }, "h", function () awful.tag.incncol( 1)         end),
    awful.key({ modkey, "Control" }, "l", function () awful.tag.incncol(-1)         end),
    awful.key({ modkey,           }, "space", function () awful.layout.inc(layouts,  1) end),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(layouts, -1) end),

    -- Prompt
    awful.key({ modkey }, "r", function () promptbox[mouse.screen]:run() end),

    awful.key({ modkey }, "x",
	      function ()
		  awful.prompt.run({ prompt = "Run Lua code: " },
		  promptbox[mouse.screen].widget,
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
    awful.key({ modkey, "Shift"   }, "r",      function (c) c:redraw()                       end),
    awful.key({ modkey,           }, "n",      function (c) c.minimized = not c.minimized    end),
    awful.key({ modkey,           }, "m",
	function (c)
	    c.maximized_horizontal = not c.maximized_horizontal
	    c.maximized_vertical   = not c.maximized_vertical
	end)
)

-- Compute the maximum number of digit we need, limited to 9
keynumber = 0
for s = 1, screen.count() do
   keynumber = math.min(9, math.max(#tags[s], keynumber));
end

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, keynumber do
    globalkeys = awful.util.table.join(globalkeys,
	awful.key({ modkey }, "#" .. i + 9,
		  function ()
			local screen = mouse.screen
			if tags[screen][i] then
			    awful.tag.viewonly(tags[screen][i])
			end
		  end),
	awful.key({ modkey, "Control" }, "#" .. i + 9,
		  function ()
		      local screen = mouse.screen
		      if tags[screen][i] then
			  awful.tag.viewtoggle(tags[screen][i])
		      end
		  end),
	awful.key({ modkey, "Shift" }, "#" .. i + 9,
		  function ()
		      if client.focus and tags[client.focus.screen][i] then
			  awful.client.movetotag(tags[client.focus.screen][i])
		      end
		  end),
	awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
		  function ()
		      if client.focus and tags[client.focus.screen][i] then
			  awful.client.toggletag(tags[client.focus.screen][i])
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
      properties = {
	 border_width = beautiful.border_width,
	 border_color = beautiful.border_normal,
	 focus = true, size_hints_honor = false,
	 keys = clientkeys, buttons = clientbuttons }
   },
   { rule = { class = "MPlayer" },
      properties = { floating = true } },
   { rule = { class = "Gimp" },
      properties = { tag = tags[screen.count()][5] } },
   { rule = { class = "Conkeror" },
      properties = { tag = tags[screen.count()][2] } },
   { rule = { class = "Emacs" },
      properties = { tag = tags[screen.count()][3] } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.add_signal("manage", function (c, startup)

    -- Enable sloppy focus
    c:add_signal("mouse::enter", function(c)
	if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
	    and awful.client.focus.filter(c) then
	    client.focus = c
	end
    end)

    if not startup then
	-- Set the windows at the slave,
	-- i.e. put it at the end of others instead of setting it master.
	-- awful.client.setslave(c)

	-- Put windows in a smart way, only if they does not set an initial position.
	if not c.size_hints.user_position and not c.size_hints.program_position then
	    awful.placement.no_overlap(c)
	    awful.placement.no_offscreen(c)
	end
    end
end)

client.add_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.add_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)

for s = 1, screen.count() do screen[s]:add_signal("arrange", function ()
    local clients = awful.client.visible(s)
    local layout = awful.layout.getname(awful.layout.get(s))

    for _, c in pairs(clients) do -- Floaters are always on top
	if   awful.client.floating.get(c) or layout == "floating"
	then if not c.fullscreen then c.above       =  true  end
	else                          c.above       =  false end
    end
  end)
end
-- }}}
