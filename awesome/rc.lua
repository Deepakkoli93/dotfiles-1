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

-- Minimum producitivity
local min_prod = 50
local timer_interval = 600

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


-- {{{ my functions

-- html escaping function
local function htmlize(s)
   s = s:gsub('&', '&amp;')
   s = s:gsub('<', '&lt;')
   s = s:gsub('>', '&gt;')
   return s
end

  -- beautify markup
function beautify_markup(text, color, font, size)
   return "<span foreground=\"" .. color .. "\" font =\"" ..
      font .."\" size=\"" .. size .. "\"> " .. text .. " </span>"
end

-- dictionary lookup + Lyrics with mpc  #TODO
-- Doesn't work well with the computing dictionary.
-- modify the show_x script to handle the other script 
-- Add support for scrolling when text overflows the notification area #TODO
local dict_window = nil
local more_scroll = nil -- TODO
function lookup(dict)
   awful.prompt.run({ fg_cursor = "black",bg_cursor="orange", prompt = "<span color='#008DFA'>Word:</span> " }, 
                    mypromptbox_r[mouse.screen].widget,
                    function(word)
                       local f = io.popen("~/dotfiles/local/bin/show_x '" .. word .. "' '"
                                             .. dict .. "' 2>&1 &")
                       local fr = ""
                       for line in f:lines() do
                          fr = fr .. line .. '\n'
                       end
                       f:close()
                       -- destroy if there is an old window already
					   if dict_window ~= nil then
                          naughty.destroy(dict_window)
					   end
						
                       -- new window pops up now!
                       dict_window = naughty.notify({ text = '<span font_desc="Comic Sans MS 10" foreground="white">'
                                                         .. fr ..'</span>', 
                                                      timeout = 0,
                                                      width = 440 })
                    end,
                    nil, 
                    awful.util.getdir("cache") .. "/dict") 
end
  -- remove dictionary lookup window
function destroy_dictwindow()
   if dict_window ~= nil then
      naughty.destroy(dict_window)
      dict_window = nil
   end
end

-- Be Limitless Productivity 
-- Alert me if my productivity on we is lower than 80 percent
function check_focus()
   local f = io.popen("~/dotfiles/local/bin/get_productivity ")
   local productivity = tonumber(f:read())
   f:close()

   if (productivity < min_prod) then
      naughty.notify({ text = "<span foreground=\"White\" font_desc=\"Comic Sans MS 200\"> FOCUS</span>", 
                       timeout=10, 
                       width=1600, 
                       position="top_right",
                       margin=100 }) 
   end
end
-- timer for Be Limitless
local prod_timer = timer({ timeout = timer_interval })
prod_timer:connect_signal("timeout", function () check_focus() end)

-- }}}

-- {{{ Variable definitions
-- Themes define colours, icons, and wallpapers
beautiful.init("/home/nj/.config/awesome/themes/default/theme.lua")

-- This is used later as the default terminal and editor to run.
terminal = "gnome-terminal -e tmux new"
screenlock = "xscreensaver-command -lock"
editor = os.getenv("EDITOR") or "vim"
editor_cmd = "gnome-terminal" .. " -e " .. editor

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
local layouts =
{
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
tags = {}
for s = 1, screen.count() do
    -- Each screen has its own tag table.
   tags[s] = awful.tag({ "α", "β", "γ" }, s, layouts[1])
end
-- }}}

-- {{{ Menu
-- Create a laucher widget and a main menu
myawesomemenu = {
   { "restart", awesome.restart },
   { "quit", awesome.quit }
}

mymainmenu = awful.menu({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
                                    { "terminal", terminal },
                                    { "keil", "wine /home/nj/.wine/drive_c/Keil/UV4/UV4.exe"},
                                    { "chrome", "google-chrome-stable"}
                                  }
                        })

mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
                                     menu = mymainmenu })

-- {{{ Wibox
-- Create a textclock widget
mytextclock = awful.widget.textclock()

-- create a battery widget
batterywidget = wibox.widget.textbox()    
batterywidget:set_text(" | Battery | ")    
-- callback function to be called periodically for battery status
-- update markup to include some colors or icons
function batteryInfo(adapter)
   spacer = " "
   local fcur = io.open("/sys/class/power_supply/"..adapter.."/charge_now")    
   local fcap = io.open("/sys/class/power_supply/"..adapter.."/charge_full")
   local fsta = io.open("/sys/class/power_supply/"..adapter.."/status")
   local cur = fcur:read()
   local cap = fcap:read()
   local sta = fsta:read()
   local battery = math.floor(cur * 100 / cap)
   local textinfo = ""

   if sta:match("Charging") then
      textinfo = "β ("..battery.."%) ↑"
   elseif sta:match("Discharging") then
      textinfo = "γ (" .. battery .. "%) ↓"
      -- if battery level too low notify me
      if tonumber(battery) < 10 then
         naughty.notify({ title      = "Battery Warning"
                          , text       = "Battery low! " .. battery .. "% left!"
                          , timeout    = 5
                          , position   = "top_right"
                          , fg         = beautiful.fg_focus
                          , bg         = beautiful.bg_focus
                        })
      end
   else
      textinfo = "α (".. battery .. "%)"
   end

   textinfo = beautify_markup(textinfo,"orange", "Purisa 10", "large")
   batterywidget:set_markup(textinfo)
   fcur:close()
   fcap:close()
   fsta:close()
end

batterywidgettimer = timer({ timeout = 10 })    
batterywidgettimer:connect_signal("timeout", function ()
                                     batteryInfo("BAT0")
end)
batterywidgettimer:start()
    
-- Create a wibox for each screen and add it
mywibox = {}
mypromptbox = {}
mypromptbox_r = {}
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
    -- Create a left promptbox for each screen
    mypromptbox[s] = awful.widget.prompt()
	-- Create a right promptbox for each screen
	mypromptbox_r[s] = awful.widget.prompt()
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
    left_layout:add(mypromptbox[s])

    -- Widgets that are aligned to the right
    local right_layout = wibox.layout.fixed.horizontal()
	right_layout:add(mypromptbox_r[s])
    if s == 1 then right_layout:add(wibox.widget.systray()) end
    right_layout:add(batterywidget)
    right_layout:add(mytextclock)
    right_layout:add(mylayoutbox[s])

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
    awful.key({ modkey,           }, ",",   awful.tag.viewprev       ),
    awful.key({ modkey,           }, ".",  awful.tag.viewnext       ),
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
	awful.key({ modkey, "Shift"   }, "Return", function() awful.util.spawn("emacs") end),
    awful.key({ modkey, "Shift"	  }, ";", function () awful.util.spawn(screenlock) end),
    awful.key({ modkey, "Control" }, "r", awesome.restart),
    awful.key({ modkey, "Shift"   }, "q", awesome.quit),
    

    -- refocus | Be Limitless
    awful.key({ modkey,           }, "[", 
              function() 
                 prod_timer:start() 
                 naughty.notify({ text = "Monitoring Your Web Activity Now...", width = 400 })
              end),
    awful.key({ modkey,           }, "]", 
              function() 
                 prod_timer:stop() 
                 naughty.notify({ text = "Stopped Monitoring!", width = 400 })
              end),

	-- dicionary lookup 
	awful.key({ modkey, 		  }, "i", 
              function ()
                 lookup('WordNet')
              end),
    awful.key({ modkey, "Shift"   }, "i", destroy_dictwindow),
    awful.key({ modkey,           }, "y", 
              function ()
                 lookup('Free On-Line Dictionary of Computing')
              end),

    -- increase and decrese volume with mode4 +/-
    awful.key({modkey,           }, "-", 
              function () 
                 awful.util.spawn("ponymix decrease 5")
              end),
    awful.key({modkey, "Shift"   }, "=", 
              function ()
                 awful.util.spawn("ponymix increase 5")
              end),
    awful.key({modkey,           }, "=", 
              function ()
                 awful.util.spawn("ponymix set-volume 60")
              end),

    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)    end),
    awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)    end),
    awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1)      end),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1)      end),
    awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1)         end),
    awful.key({ modkey, "control" }, "l",     function () awful.tag.incncol(-1)         end),
    awful.key({ modkey,           }, "space", function () awful.layout.inc(layouts,  1) end),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(layouts, -1) end),

    awful.key({ modkey, "Control" }, "n", awful.client.restore),

    -- Prompt
    awful.key({ modkey },            "r",     function () mypromptbox[mouse.screen]:run() end),

    awful.key({ modkey }, "x",
              function ()
                  awful.prompt.run({ prompt = "Run Lua code: " },
                  mypromptbox[mouse.screen].widget,
                  awful.util.eval, nil,
                  awful.util.getdir("cache") .. "/history_eval")
              end),
    -- use dmenu instead of menubar
    awful.key({ modkey }, "p", function() awful.util.spawn("dmenu_run -l 2") end),

    -- hide/show wibox
    awful.key({ modkey }, "b", function ()
                 mywibox[mouse.screen].visible = not mywibox[mouse.screen].visible
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

-- Bind keys to tags. This works in a strange way.
-- to associate number key i to tag i we must do a mapping to ("#" .. i + 9)
-- I am mapping tag 1 -> (1+9); 2 -> (6+9); 3 -> (9+9) as I have only three tags
-- I will use this quadratic equation: -i^2 -8i + 3
for i = 1, 3 do
   keyvalue = (-1*i^2) + (8*i) + 3
   globalkeys = awful.util.table.join(globalkeys,
        awful.key({ modkey }, "#" .. keyvalue,
                  function ()
                        local screen = mouse.screen
                        local tag = awful.tag.gettags(screen)[i]
                        if tag then
                           awful.tag.viewonly(tag)
                        end
                  end),
        awful.key({ modkey, "Control" }, "#" .. keyvalue,
                  function ()
                      local screen = mouse.screen
                      local tag = awful.tag.gettags(screen)[i]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end),
        awful.key({ modkey, "Shift" }, "#" .. keyvalue,
                  function ()
                      if client.focus then
                          local tag = awful.tag.gettags(client.focus.screen)[i]
                          if tag then
                              awful.client.movetotag(tag)
                          end
                     end
                  end),
        awful.key({ modkey, "Control", "Shift" }, "#" .. keyvalue,
                  function ()
                      if client.focus then
                          local tag = awful.tag.gettags(client.focus.screen)[i]
                          if tag then
                              awful.client.toggletag(tag)
                          end
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
					 maximized_vertical=false,
					 maximized_horizontal=false,
                     buttons = clientbuttons } },
    { rule = { class = "MPlayer" },
      properties = { floating = true } },
    { rule = { class = "pinentry" },
      properties = { floating = true } },
    { rule = { class = "gimp" },
      properties = { floating = true } },
    -- Set Firefox to always map on tags number 2 of screen 1.
    -- { rule = { class = "Firefox" },
    --   properties = { tag = tags[1][2] } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c, startup)
    -- Enable sloppy focus
    c:connect_signal("mouse::enter", function(c)
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

    local titlebars_enabled = false
    if titlebars_enabled and (c.type == "normal" or c.type == "dialog") then
        -- buttons for the titlebar
        local buttons = awful.util.table.join(
                awful.button({ }, 1, function()
                    client.focus = c
                    c:raise()
                    awful.mouse.client.move(c)
                end),
                awful.button({ }, 3, function()
                    client.focus = c
                    c:raise()
                    awful.mouse.client.resize(c)
                end)
                )

        -- Widgets that are aligned to the left
        local left_layout = wibox.layout.fixed.horizontal()
        left_layout:add(awful.titlebar.widget.iconwidget(c))
        left_layout:buttons(buttons)

        -- Widgets that are aligned to the right
        local right_layout = wibox.layout.fixed.horizontal()
        right_layout:add(awful.titlebar.widget.floatingbutton(c))
        right_layout:add(awful.titlebar.widget.maximizedbutton(c))
        right_layout:add(awful.titlebar.widget.stickybutton(c))
        right_layout:add(awful.titlebar.widget.ontopbutton(c))
        right_layout:add(awful.titlebar.widget.closebutton(c))

        -- The title goes in the middle
        local middle_layout = wibox.layout.flex.horizontal()
        local title = awful.titlebar.widget.titlewidget(c)
        title:set_align("center")
        middle_layout:add(title)
        middle_layout:buttons(buttons)

        -- Now bring it all together
        local layout = wibox.layout.align.horizontal()
        layout:set_left(left_layout)
        layout:set_right(right_layout)
        layout:set_middle(middle_layout)

        awful.titlebar(c):set_widget(layout)
    end
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)

-- Spawn emacs now with the org schedule
awful.util.spawn("emacs ~/org/work.org")

-- Start the prod-timer on startup
prod_timer:start()

-- }}}
