import dracula.draw
from qutebrowser.api import interceptor

# Load existing settings made via :set
config.load_autoconfig()
config.set('auto_save.session', True)
config.set('window.hide_decoration', True)
config.set('fonts.web.size.default', 14)

config.bind(',m', 'spawn mpv {url}')
config.bind(',M', 'hint links spawn mpv {hint-url}')

config.bind(',pp', 'spawn --userscript qute-pass -d')
config.bind(',pu', 'spawn --userscript qute-pass --username-only')
config.bind(',pa', 'spawn --userscript qute-pass --password-only')
config.bind(',po', 'spawn --userscript qute-pass --otp-only')

dracula.draw.blood(c)
def filter_yt(info: interceptor.Request):
    """Block the given request if necessary."""
    url = info.request_url
    if (url.host() == "www.youtube.com"
            and url.path() == "/get_video_info"
            and "&adformat=" in url.query()):
        info.block()
interceptor.register(filter_yt)
