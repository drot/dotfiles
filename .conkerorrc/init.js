// -*- mode: Javascript -*-

// Modules
require("new-tabs.js");
require("clicks-in-new-buffer.js");
require("block-content-focus-change.js");
require("favicon");

// The default page for new buffers.
homepage = "about:blank";

// Improve hinting keys
hint_digits = "aoeuhtns";

// Display selected hint URL
hints_display_url_panel = true;

// Hinting color
hint_background_color = "transparent";
register_user_stylesheet(
    "data:text/css," +
	escape (
	    "span.__conkeror_hint {" +
		" border: 1px solid black !important;" +
		" color: #333 !important;" +
		" background-color: yellow !important;" +
		"}"));

// Reduce JavaScript output
session_pref("browser.dom.window.dump.enabled", false);

// Don't require a whitelist to install extensions
session_pref("xpinstall.whitelist.required", false);

// Default directory for downloads and shell commands
cwd = get_home_directory();
cwd.append('Downloads');

// Firefox user agent
session_pref("general.useragent.compatMode.firefox", true);

// Don't enable formfill
session_pref("browser.formfill.enable", false);

// Block auto focus
block_content_focus_change_duration = 20;

// Open clicks in buffers in the background
clicks_in_new_buffer_target = OPEN_NEW_BUFFER_BACKGROUND; 
clicks_in_new_buffer_button = 1;

// Auto completion in the minibuffer
minibuffer_auto_complete_default = true;
url_completion_use_history = true;
url_completion_use_bookmarks = true;

// Load download buffers in the background
download_buffer_automatic_open_target = OPEN_NEW_BUFFER_BACKGROUND;

// Open new urls in new buffer
url_remoting_fn = load_url_in_new_buffer;

// Prevent quitting by accident
can_kill_last_buffer = false;

// Display number of buffers being loaded
add_hook("mode_line_hook", mode_line_adder(loading_count_widget), true);

// Display number of present buffers and which is the current
add_hook("mode_line_hook", mode_line_adder(buffer_count_widget), true);

// Remove the clock
remove_hook("mode_line_hook", mode_line_adder(clock_widget));

// External editor
editor_shell_command = "emacsclient";

// View source in external editor
view_source_use_external_editor = true;

// Favicons
add_hook("mode_line_hook", mode_line_adder(buffer_icon_widget), true);
read_buffer_show_icons = true;

// Delete existing webjumps
existing_webjumps = ['answers', 'bugzilla', 'buildd', 'buildd-ports', 'clhs',
                     'cliki', 'clusty', 'debbugs', 'debfile', 'debpopcon',
                     'debpts', 'debqa', 'duckduckgo', 'ebay', 'freshmeat',
                     'image', 'kuro5hin', 'lucky', 'maps', 'mdc',
                     'ratpoisonwiki', 'savannah', 'slang', 'slashdot', 'sourceforge',
                     'stumpwmwiki', 'ubuntubugs','ubuntufile', 'ubuntupkg', 'yahoo'];
for (x in existing_webjumps) {
    delete webjumps[existing_webjumps[x]];
}

// Webjumps
define_webjump("imdb", "http://imdb.com/find?q=%s");
define_webjump("youtube", "http://www.youtube.com/results?search_query=%s&search=Search");

// Clear history
function clear_history () {
    var history = Cc["@mozilla.org/browser/nav-history-service;1"]
        .getService(Ci.nsIBrowserHistory);
    history.removeAllPages();
};
interactive("clear-history", "Clear the history.", clear_history);
