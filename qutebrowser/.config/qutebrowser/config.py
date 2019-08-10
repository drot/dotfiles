# Autogenerated config.py
# Documentation:
#   qute://help/configuring.html
#   qute://help/settings.html

# Uncomment this to still load settings configured via autoconfig.yml
# config.load_autoconfig()

# Enable JavaScript.
# Type: Bool
config.set('content.javascript.enabled', True, 'file://*')

# Enable JavaScript.
# Type: Bool
config.set('content.javascript.enabled', True, 'chrome://*/*')

# Enable JavaScript.
# Type: Bool
config.set('content.javascript.enabled', True, 'qute://*/*')

# Height (in pixels or as percentage of the window) of the completion.
# Type: PercOrInt
c.completion.height = '25%'

# Editor (and arguments) to use for the `open-editor` command. The
# following placeholders are defined: * `{file}`: Filename of the file
# to be edited. * `{line}`: Line in which the caret is found in the
# text. * `{column}`: Column in which the caret is found in the text. *
# `{line0}`: Same as `{line}`, but starting from index 0. * `{column0}`:
# Same as `{column}`, but starting from index 0.
# Type: ShellCommand
c.editor.command = ['emacsclient', '-c', '{file}']

# CSS border value for hints.
# Type: String
c.hints.border = '1px solid #1d1f21'

# Open new tabs (middleclick/ctrl+click) in the background.
# Type: Bool
c.tabs.background = True

# Which tab to select when the focused tab is removed.
# Type: SelectOnRemove
# Valid values:
#   - prev: Select the tab which came before the closed one (left in horizontal, above in vertical).
#   - next: Select the tab which came after the closed one (right in horizontal, below in vertical).
#   - last-used: Select the previously selected tab.
c.tabs.select_on_remove = 'prev'

# Page(s) to open at the start.
# Type: List of FuzzyUrl, or FuzzyUrl
c.url.start_pages = 'about:blank'

# Text color of the completion widget. May be a single color to use for
# all columns or a list of three colors, one for each column.
# Type: List of QtColor, or QtColor
c.colors.completion.fg = '#969896'

# Background color of the completion widget for odd rows.
# Type: QssColor
c.colors.completion.odd.bg = '#282a2e'

# Background color of the completion widget for even rows.
# Type: QssColor
c.colors.completion.even.bg = '#1d1f21'

# Foreground color of completion widget category headers.
# Type: QtColor
c.colors.completion.category.fg = '#f0c674'

# Background color of the completion widget category headers.
# Type: QssColor
c.colors.completion.category.bg = '#1d1f21'

# Top border color of the completion widget category headers.
# Type: QssColor
c.colors.completion.category.border.top = '#969896'

# Bottom border color of the completion widget category headers.
# Type: QssColor
c.colors.completion.category.border.bottom = '#969896'

# Foreground color of the selected completion item.
# Type: QtColor
c.colors.completion.item.selected.fg = '#1d1f21'

# Background color of the selected completion item.
# Type: QssColor
c.colors.completion.item.selected.bg = '#f0c674'

# Top border color of the selected completion item.
# Type: QssColor
c.colors.completion.item.selected.border.top = '#1d1f21'

# Bottom border color of the selected completion item.
# Type: QssColor
c.colors.completion.item.selected.border.bottom = '#1d1f21'

# Foreground color of the matched text in the completion.
# Type: QtColor
c.colors.completion.match.fg = 'white'

# Color of the scrollbar handle in the completion view.
# Type: QssColor
c.colors.completion.scrollbar.fg = '#c5c8c6'

# Color of the scrollbar in the completion view.
# Type: QssColor
c.colors.completion.scrollbar.bg = '#1d1f21'

# Background color for the download bar.
# Type: QssColor
c.colors.downloads.bar.bg = '#1d1f21'

# Color gradient start for download text.
# Type: QtColor
c.colors.downloads.start.fg = '#1d1f21'

# Color gradient start for download backgrounds.
# Type: QtColor
c.colors.downloads.start.bg = '#81a2be'

# Color gradient end for download text.
# Type: QtColor
c.colors.downloads.stop.fg = '#1d1f21'

# Color gradient stop for download backgrounds.
# Type: QtColor
c.colors.downloads.stop.bg = '#b5bd68'

# Foreground color for downloads with errors.
# Type: QtColor
c.colors.downloads.error.fg = 'white'

# Background color for downloads with errors.
# Type: QtColor
c.colors.downloads.error.bg = '#cc6666'

# Font color for hints.
# Type: QssColor
c.colors.hints.fg = '#1d1f21'

# Background color for hints. Note that you can use a `rgba(...)` value
# for transparency.
# Type: QssColor
c.colors.hints.bg = '#f0c674'

# Font color for the matched part of hints.
# Type: QtColor
c.colors.hints.match.fg = 'white'

# Text color for the keyhint widget.
# Type: QssColor
c.colors.keyhint.fg = '#c5c8c6'

# Highlight color for keys to complete the current keychain.
# Type: QssColor
c.colors.keyhint.suffix.fg = '#f0c674'

# Background color of the keyhint widget.
# Type: QssColor
c.colors.keyhint.bg = '#1d1f21'

# Foreground color of an error message.
# Type: QssColor
c.colors.messages.error.fg = 'white'

# Background color of an error message.
# Type: QssColor
c.colors.messages.error.bg = '#cc6666'

# Border color of an error message.
# Type: QssColor
c.colors.messages.error.border = '#cc6666'

# Foreground color of a warning message.
# Type: QssColor
c.colors.messages.warning.fg = '#1d1f21'

# Background color of a warning message.
# Type: QssColor
c.colors.messages.warning.bg = '#f0c674'

# Border color of a warning message.
# Type: QssColor
c.colors.messages.warning.border = '#f0c674'

# Foreground color of an info message.
# Type: QssColor
c.colors.messages.info.fg = '#c5c8c6'

# Background color of an info message.
# Type: QssColor
c.colors.messages.info.bg = '#373b41'

# Border color of an info message.
# Type: QssColor
c.colors.messages.info.border = '#1d1f21'

# Foreground color for prompts.
# Type: QssColor
c.colors.prompts.fg = '#c5c8c6'

# Border used around UI elements in prompts.
# Type: String
c.colors.prompts.border = '#282a2e'

# Background color for prompts.
# Type: QssColor
c.colors.prompts.bg = '#1d1f21'

# Background color for the selected item in filename prompts.
# Type: QssColor
c.colors.prompts.selected.bg = '#373b41'

# Foreground color of the statusbar.
# Type: QssColor
c.colors.statusbar.normal.fg = '#c5c8c6'

# Background color of the statusbar.
# Type: QssColor
c.colors.statusbar.normal.bg = '#1d1f21'

# Foreground color of the statusbar in insert mode.
# Type: QssColor
c.colors.statusbar.insert.fg = '#c5c8c6'

# Background color of the statusbar in insert mode.
# Type: QssColor
c.colors.statusbar.insert.bg = '#373b41'

# Foreground color of the statusbar in passthrough mode.
# Type: QssColor
c.colors.statusbar.passthrough.fg = '#1d1f21'

# Background color of the statusbar in passthrough mode.
# Type: QssColor
c.colors.statusbar.passthrough.bg = '#8abeb7'

# Foreground color of the statusbar in private browsing mode.
# Type: QssColor
c.colors.statusbar.private.fg = '#1d1f21'

# Background color of the statusbar in private browsing mode.
# Type: QssColor
c.colors.statusbar.private.bg = '#969896'

# Foreground color of the statusbar in command mode.
# Type: QssColor
c.colors.statusbar.command.fg = '#c5c8c6'

# Background color of the statusbar in command mode.
# Type: QssColor
c.colors.statusbar.command.bg = '#1d1f21'

# Foreground color of the statusbar in private browsing + command mode.
# Type: QssColor
c.colors.statusbar.command.private.fg = '#c5c8c6'

# Background color of the statusbar in private browsing + command mode.
# Type: QssColor
c.colors.statusbar.command.private.bg = '#1d1f21'

# Foreground color of the statusbar in caret mode.
# Type: QssColor
c.colors.statusbar.caret.fg = '#1d1f21'

# Background color of the statusbar in caret mode.
# Type: QssColor
c.colors.statusbar.caret.bg = '#b294bb'

# Foreground color of the statusbar in caret mode with a selection.
# Type: QssColor
c.colors.statusbar.caret.selection.fg = '#1d1f21'

# Background color of the statusbar in caret mode with a selection.
# Type: QssColor
c.colors.statusbar.caret.selection.bg = '#81a2be'

# Background color of the progress bar.
# Type: QssColor
c.colors.statusbar.progress.bg = '#81a2be'

# Default foreground color of the URL in the statusbar.
# Type: QssColor
c.colors.statusbar.url.fg = '#c5c8c6'

# Foreground color of the URL in the statusbar on error.
# Type: QssColor
c.colors.statusbar.url.error.fg = '#cc6666'

# Foreground color of the URL in the statusbar for hovered links.
# Type: QssColor
c.colors.statusbar.url.hover.fg = '#c5c8c6'

# Foreground color of the URL in the statusbar on successful load
# (http).
# Type: QssColor
c.colors.statusbar.url.success.http.fg = 'white'

# Foreground color of the URL in the statusbar on successful load
# (https).
# Type: QssColor
c.colors.statusbar.url.success.https.fg = '#f0c674'

# Foreground color of the URL in the statusbar when there's a warning.
# Type: QssColor
c.colors.statusbar.url.warn.fg = '#b294bb'

# Background color of the tab bar.
# Type: QssColor
c.colors.tabs.bar.bg = '#1d1f21'

# Color gradient start for the tab indicator.
# Type: QtColor
c.colors.tabs.indicator.start = '#81a2be'

# Color gradient end for the tab indicator.
# Type: QtColor
c.colors.tabs.indicator.stop = '#b5bd68'

# Color for the tab indicator on errors.
# Type: QtColor
c.colors.tabs.indicator.error = '#cc6666'

# Foreground color of unselected odd tabs.
# Type: QtColor
c.colors.tabs.odd.fg = '#c5c8c6'

# Background color of unselected odd tabs.
# Type: QtColor
c.colors.tabs.odd.bg = '#4d4d4c'

# Foreground color of unselected even tabs.
# Type: QtColor
c.colors.tabs.even.fg = '#e0e0e0'

# Background color of unselected even tabs.
# Type: QtColor
c.colors.tabs.even.bg = '#8e908c'

# Foreground color of selected odd tabs.
# Type: QtColor
c.colors.tabs.selected.odd.fg = '#81a2be'

# Background color of selected odd tabs.
# Type: QtColor
c.colors.tabs.selected.odd.bg = '#1d1f21'

# Foreground color of selected even tabs.
# Type: QtColor
c.colors.tabs.selected.even.fg = '#81a2be'

# Background color of selected even tabs.
# Type: QtColor
c.colors.tabs.selected.even.bg = '#282a2e'

# Background color of pinned unselected odd tabs.
# Type: QtColor
c.colors.tabs.pinned.odd.bg = '#b5bd68'

# Background color of pinned unselected even tabs.
# Type: QtColor
c.colors.tabs.pinned.even.bg = '#de935f'

# Foreground color of pinned selected odd tabs.
# Type: QtColor
c.colors.tabs.pinned.selected.odd.fg = '#969896'

# Background color of pinned selected odd tabs.
# Type: QtColor
c.colors.tabs.pinned.selected.odd.bg = '#1d1f21'

# Foreground color of pinned selected even tabs.
# Type: QtColor
c.colors.tabs.pinned.selected.even.fg = '#969896'

# Background color of pinned selected even tabs.
# Type: QtColor
c.colors.tabs.pinned.selected.even.bg = '#1d1f21'

# Default monospace fonts. Whenever "monospace" is used in a font
# setting, it's replaced with the fonts listed here.
# Type: Font
c.fonts.monospace = '"Iosevka Term SS05"'

# Font used in the completion widget.
# Type: Font
c.fonts.completion.entry = '12pt monospace'

# Font used in the completion categories.
# Type: Font
c.fonts.completion.category = 'bold 12pt monospace'

# Font used for the debugging console.
# Type: QtFont
c.fonts.debug_console = '12pt monospace'

# Font used for the downloadbar.
# Type: Font
c.fonts.downloads = '12pt monospace'

# Font used for the hints.
# Type: Font
c.fonts.hints = 'bold 12pt monospace'

# Font used in the keyhint widget.
# Type: Font
c.fonts.keyhint = '12pt monospace'

# Font used for error messages.
# Type: Font
c.fonts.messages.error = '12pt monospace'

# Font used for info messages.
# Type: Font
c.fonts.messages.info = '12pt monospace'

# Font used for warning messages.
# Type: Font
c.fonts.messages.warning = '12pt monospace'

# Font used for prompts.
# Type: Font
c.fonts.prompts = '12pt sans-serif'

# Font used in the statusbar.
# Type: Font
c.fonts.statusbar = '12pt monospace'

# Font used in the tab bar.
# Type: QtFont
c.fonts.tabs = '12pt monospace'

# Bindings for normal mode
config.bind(',m', 'spawn umpv {url}')
config.bind(';M', 'hint links spawn umpv {hint-url}')
