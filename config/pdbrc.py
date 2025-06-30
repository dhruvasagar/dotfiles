import pdb


class Config(pdb.DefaultConfig):
    use_pygments = True
    pygments_formatter_class = "pygments.formatters.TerminalTrueColorFormatter"
    pygments_formatter_kwargs = {"style": "monokai"}
