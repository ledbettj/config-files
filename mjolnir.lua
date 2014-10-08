
mjolnir.application = require "mjolnir.application"
mjolnir.window      = require "mjolnir.window"
mjolnir.hotkey      = require "mjolnir.hotkey"
mjolnir.fnutils     = require "mjolnir.fnutils"
mjolnir.geometry    = require "mjolnir.geometry"
mjolnir.screen      = require "mjolnir.screen"

function window_by_title(title)
   local apps = mjolnir.application.runningapplications()
   local w
   for _, app in pairs(apps) do
      if app:title() == title then
         return app:mainwindow()
      end
   end

   return nil
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
      local apps = {
         ["Google Chrome"] = "topright",
         ["Emacs"]         = "left",
         ["iTerm"]         = "bottomright",
         ["HipChat"]       = "left",
         ["Mail"]          = "right"
      }

      for name, pos in pairs(apps) do
         local w = window_by_title(name)
         if w then
            _G[pos](w)
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
      local screen = win:screen()
      local screen_frame = screen:frame()
      local win_frame = win:frame()

      local new_screen = screen:next()
      local new_frame  = new_screen:frame()

      local off_x_pct = (win_frame.x - screen_frame.x) / (screen_frame.w)
      local off_y_pct = (win_frame.y - screen_frame.y) / (screen_frame.h)

      local w_pct = win_frame.w / screen_frame.w
      local h_pct = win_frame.h / screen_frame.h

      win_frame.x = new_frame.x + (off_x_pct * new_frame.w)
      win_frame.y = new_frame.y + (off_y_pct * new_frame.h)
      win_frame.w = w_pct * new_frame.w
      win_frame.h = h_pct * new_frame.h

      win:setframe(win_frame)
   end
)

