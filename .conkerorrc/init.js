// -*- mode: Javascript -*-

// Modules
require("favicon");
require("new-tabs.js");
require("mode-line.js");
require("clicks-in-new-buffer.js");
require("block-content-focus-change.js");

// Theme
theme_load_paths.unshift("~/.conkerorrc/themes/");
theme_unload("default");
theme_load("tango");

// The default page for new buffers.
homepage = "about:blank";

// Improve hinting keys
hint_digits = "aoeuhtns";

// Display selected hint URL
hints_display_url_panel = true;
hints_minibuffer_annotation_mode(true);

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

// Delete history after 30 days
session_pref('browser.history_expire_days', 30);

// Don't check compatibility for extensions
session_pref('extensions.checkCompatibility', false);

// Don't require a whitelist to install extensions
session_pref("xpinstall.whitelist.required", false);

// Check for security updates
user_pref("extensions.checkUpdateSecurity", true);

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
url_completion_use_history = true;
url_completion_use_bookmarks = true;
url_completion_use_webjumps = true;
minibuffer_auto_complete_default = true;

// Load download buffers in the background
download_buffer_automatic_open_target = OPEN_NEW_BUFFER_BACKGROUND;

// Open new urls in new buffer
url_remoting_fn = load_url_in_new_buffer;

// Prevent quitting by accident
can_kill_last_buffer = false;

// Remove the clock and set the modeline
remove_hook("mode_line_hook", mode_line_adder(clock_widget));
add_hook("mode_line_hook", mode_line_adder(buffer_icon_widget), true);
add_hook("mode_line_hook", mode_line_adder(loading_count_widget), true);
add_hook("mode_line_hook", mode_line_adder(buffer_count_widget), true);
add_hook("mode_line_hook", mode_line_adder(zoom_widget));
add_hook("mode_line_hook", mode_line_adder(downloads_status_widget));
read_buffer_show_icons = true;

// External editor
editor_shell_command = "emacsclient";

// View source in external editor
view_source_use_external_editor = true;

// Delete existing webjumps
var unused_webjumps = ['answers', 'buildd','buildd-ports','clhs','cliki',
                       'clusty','creativecommons','debbugs','debfile','debpkg',
                       'debpopcon','debpts','debqa','freshmeat','kuro5hin',
                       'launchpad','lucky','ratpoisonwiki','sadelicious',
                       'scholar','sdelicious','slashdot','sourceforge',
                       'stumpwmwiki','ubuntubugs','ubuntufile','ubuntupkg',
                       'wiktionary','yahoo','bugzilla','ebay'
                      ];

for (var i=0; i<unused_webjumps.length; i++)
{
    delete webjumps[unused_webjumps[i]];
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
