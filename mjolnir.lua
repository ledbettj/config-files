
mjolnir.application = require "mjolnir.application"
mjolnir.window      = require "mjolnir.window"
mjolnir.hotkey      = require "mjolnir.hotkey"
mjolnir.fnutils     = require "mjolnir.fnutils"
mjolnir.geometry    = require "mjolnir.geometry"
mjolnir.screen      = require "mjolnir.screen"

-- find the main window belonging to the application with title 'title'
function winfromtitle(title)
   local apps = mjolnir.application.runningapplications()
   for _, app in pairs(apps) do
      if app:title() == title then
         return app:mainwindow()
      end
   end

   return nil
end

-- move the given window to the given screen, keeping the same relative
-- dimensions and placement.
function wintoscreen(win, screen)
   if not win or not screen then
      return false
   end

   local old_screen       = win:screen()
   local old_screen_frame = old_screen:frame()
   local win_frame        = win:frame()
   local new_screen       = screen
   local new_frame        = new_screen:frame()
   local off_x_pct        = (win_frame.x - old_screen_frame.x) / (old_screen_frame.w)
   local off_y_pct        = (win_frame.y - old_screen_frame.y) / (old_screen_frame.h)

   local w_pct = win_frame.w / old_screen_frame.w
   local h_pct = win_frame.h / old_screen_frame.h

   win_frame.x = new_frame.x + (off_x_pct * new_frame.w)
   win_frame.y = new_frame.y + (off_y_pct * new_frame.h)
   win_frame.w = w_pct * new_frame.w
   win_frame.h = h_pct * new_frame.h

   win:setframe(win_frame)
end

-- move the given window to the position ('topleft', 'topright', etc),
-- also moving it to the provided screen if any.
function move(win, where, screen)
   if not win or not where then
      return false
   end
   wintoscreen(win, screen)
   _G[where](win)
end

function topleft(win)
   win:movetounit(mjolnir.geometry.rect(0, 0, 0.5, 0.5))
end

function top(win)
   win:movetounit(mjolnir.geometry.rect(0, 0, 1, 0.5))
end

function topright(win)
   win:movetounit(mjolnir.geometry.rect(0.5, 0, 0.5, 0.5))
end

function left(win)
   win:movetounit(mjolnir.geometry.rect(0, 0, 0.5, 1))
end

function full(win)
   win:movetounit(mjolnir.geometry.rect(0, 0, 1, 1))
end

function right(win)
   win:movetounit(mjolnir.geometry.rect(0.5, 0, 0.5, 1))
end

function bottomleft(win)
   win:movetounit(mjolnir.geometry.rect(0, 0.5, 0.5, 0.5))
end

function bottom(win)
   win:movetounit(mjolnir.geometry.rect(0, 0.5, 1, 0.5))
end

function bottomright(win)
   win:movetounit(mjolnir.geometry.rect(0.5, 0.5, 0.5, 0.5))
end

mjolnir.hotkey.bind({"ctrl", "alt"}, "pad*",
   function()
      local screens = mjolnir.screen.allscreens()
      local apps = {
         ["Google Chrome"] = {"topright", screens[1]},
         ["Emacs"]         = {"left",     screens[1]},
         ["iTerm"]         = {"bottomright", screens[1]},
         ["HipChat"]       = {"left", screens[2]},
         ["Slack"]         = {"full", screens[2]},
         ["Mail"]          = {"right",  screens[2]},
         ["Mailbox (Beta)"]= {"right",  screens[2]}
      }

      for name, pos in pairs(apps) do
         local w = winfromtitle(name)
         if w then
            move(w, pos[1], pos[2])
         end
      end
   end
)

mjolnir.hotkey.bind({"ctrl", "alt"}, "pad7",
   function()
      topleft(mjolnir.window.focusedwindow())
   end
)

mjolnir.hotkey.bind({"ctrl", "alt"}, "pad8",
   function()
      top(mjolnir.window.focusedwindow())
   end
)

mjolnir.hotkey.bind({"ctrl", "alt"}, "pad9",
   function()
      topright(mjolnir.window.focusedwindow())
   end
)

mjolnir.hotkey.bind({"ctrl", "alt"}, "pad4",
   function()
      left(mjolnir.window.focusedwindow())
   end
)

mjolnir.hotkey.bind({"ctrl", "alt"}, "pad5",
   function()
      full(mjolnir.window.focusedwindow())
   end
)

mjolnir.hotkey.bind({"ctrl", "alt"}, "pad6",
   function()
      right(mjolnir.window.focusedwindow())
   end
)

mjolnir.hotkey.bind({"ctrl", "alt"}, "pad1",
   function()
      bottomleft(mjolnir.window.focusedwindow())
   end
)

mjolnir.hotkey.bind({"ctrl", "alt"}, "pad2",
   function()
      bottom(mjolnir.window.focusedwindow())
   end
)

mjolnir.hotkey.bind({"ctrl", "alt"}, "pad3",
   function()
      bottomright(mjolnir.window.focusedwindow())
   end
)

mjolnir.hotkey.bind({"ctrl", "alt"}, "padenter",
   function()
      local win = mjolnir.window.focusedwindow()
      wintoscreen(win, win:screen():next())
   end
)

mjolnir.hotkey.bind({"ctrl", "alt"}, "left",
   function()
      left(mjolnir.window.focusedwindow())
   end
)

mjolnir.hotkey.bind({"ctrl", "alt"}, "right",
   function()
      right(mjolnir.window.focusedwindow())
   end
)

mjolnir.hotkey.bind({"ctrl", "alt"}, "up",
   function()
      top(mjolnir.window.focusedwindow())
   end
)
mjolnir.hotkey.bind({"ctrl", "alt"}, "down",
   function()
      bottom(mjolnir.window.focusedwindow())
   end
)
