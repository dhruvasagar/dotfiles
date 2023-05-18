hs.hotkey.bind({ "ctrl" }, "`", function()
  local kitty = hs.application.get("Kitty")
  if kitty and kitty:isFrontmost() then
    kitty:hide()
  else
    hs.application.launchOrFocus("/Applications/kitty.app")
  end
end)
