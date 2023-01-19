hs.hotkey.bind({ "ctrl" }, "`", function()
	local alacritty = hs.application.get("Alacritty")
	if alacritty and alacritty:isFrontmost() then
		alacritty:hide()
	else
		hs.application.launchOrFocus("/Applications/Alacritty.app")
	end
end)
