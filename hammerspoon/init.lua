hs.hotkey.bind({ "ctrl" }, "`", function()
  local term = hs.application.get("WezTerm")
  if term and term:isFrontmost() then
    term:hide()
  else
    hs.application.launchOrFocus("/Applications/WezTerm.app")
  end
end)
