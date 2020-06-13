import dracula.draw

# Load existing settings made via :set
# config.load_autoconfig()
config.set('auto_save.session', True)
# config.set('window.hide_decoration', True)
config.set('colors.webpage.prefers_color_scheme_dark', True)
config.set('fonts.web.size.default', 14)

config.bind(',m', 'spawn mpv {url}')
config.bind(',M', 'hint links spawn mpv {hint-url}')

dracula.draw.blood(c)
# , {
#     'spacing': {
#         'vertical': 6,
#         'horizontal': 8
#     }
# })
