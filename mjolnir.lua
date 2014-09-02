
mjolnir.application = require "mjolnir.application"
mjolnir.window      = require "mjolnir.window"
mjolnir.hotkey      = require "mjolnir.hotkey"
mjolnir.fnutils     = require "mjolnir.fnutils"
mjolnir.geometry    = require "mjolnir.geometry"
mjolnir.screen      = require "mjolnir.screen"

mjolnir.hotkey.bind({"ctrl", "alt"}, "pad7",
   function()
      local win = mjolnir.window.focusedwindow()
      win:movetounit(mjolnir.geometry.rect(0, 0, 0.5, 0.5))
   end
)

mjolnir.hotkey.bind({"ctrl", "alt"}, "pad8",
   function()
      local win = mjolnir.window.focusedwindow()
      win:movetounit(mjolnir.geometry.rect(0, 0, 1, 0.5))
   end
)

mjolnir.hotkey.bind({"ctrl", "alt"}, "pad9",
   function()
      local win = mjolnir.window.focusedwindow()
      win:movetounit(mjolnir.geometry.rect(0.5, 0, 0.5, 0.5))
   end
)

mjolnir.hotkey.bind({"ctrl", "alt"}, "pad4",
   function()
      local win = mjolnir.window.focusedwindow()
      win:movetounit(mjolnir.geometry.rect(0, 0, 0.5, 1))
   end
)

mjolnir.hotkey.bind({"ctrl", "alt"}, "pad5",
   function()
      local win = mjolnir.window.focusedwindow()
      win:movetounit(mjolnir.geometry.rect(0, 0, 1, 1))
   end
)

mjolnir.hotkey.bind({"ctrl", "alt"}, "pad6",
   function()
      local win = mjolnir.window.focusedwindow()
      win:movetounit(mjolnir.geometry.rect(0.5, 0, 0.5, 1))
   end
)

mjolnir.hotkey.bind({"ctrl", "alt"}, "pad1",
   function()
      local win = mjolnir.window.focusedwindow()
      win:movetounit(mjolnir.geometry.rect(0, 0.5, 0.5, 0.5))
   end
)

mjolnir.hotkey.bind({"ctrl", "alt"}, "pad2",
   function()
      local win = mjolnir.window.focusedwindow()
      win:movetounit(mjolnir.geometry.rect(0, 0.5, 1, 0.5))
   end
)

mjolnir.hotkey.bind({"ctrl", "alt"}, "pad3",
   function()
      local win = mjolnir.window.focusedwindow()
      win:movetounit(mjolnir.geometry.rect(0.5, 0.5, 0.5, 0.5))
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

