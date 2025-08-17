local laucnOrFocusTerm = function()
  local term = hs.application.get("WezTerm")
  if term and term:isFrontmost() then
    term:hide()
  else
    hs.application.launchOrFocus("/Applications/WezTerm.app")
  end
end
hs.hotkey.bind({ "ctrl" }, "`", laucnOrFocusTerm)
hs.hotkey.bind({ "ctrl" }, "escape", laucnOrFocusTerm)
