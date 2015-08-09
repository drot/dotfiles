// Theme
require("new-tabs.js");
theme_load_paths.unshift("~/.conkerorrc/themes/");
theme_unload("default");
theme_load("zenburn");

// The default page for new buffers.
homepage = "about:blank";

// Disable some spurious Javascript debugger warnings
session_pref("javascript.options.strict", false);

// Delete history after 30 days
session_pref('browser.history_expire_days', 30);

// Don't check compatibility for extensions
session_pref('extensions.checkCompatibility', false);

// Don't require a whitelist to install extensions
session_pref("xpinstall.whitelist.required", false);

// Check for security updates
user_pref("extensions.checkUpdateSecurity", true);

// Firefox user agent
session_pref("general.useragent.compatMode.firefox", true);

// Enable tracking protection
session_pref("privacy.trackingprotection.enabled", true)

// Disable geolocation
session_pref("geo.enabled", false)

// Resize images automatically
session_pref("browser.enable_automatic_image_resizing", true);

// Decrease RAM usage when opening large images
session_pref("image.mem.max_decoded_image_kb", 51200);

// Reduce Javascript memory usage
session_pref("javascript.options.mem.max", 51200);
session_pref("javascript.options.mem.high_water_mark", 30);

// Enable spell checking
session_pref("layout.spellcheckDefault", 1);

// Prevent web pages from stealing focus
require("block-content-focus-change.js");

// Default directory for downloads and shell commands
cwd = get_home_directory();
cwd.append('Downloads');

// View source in external editor
editor_shell_command = "emacsclient";

// View PDFs in Zathura
external_content_handlers.set("application/pdf", "zathura");

// Improve hinting keys
hint_digits = "asdfghjkl";

// Enable automatic selection
hints_auto_exit_delay = 200;

// Use center scrolling for Isearch
isearch_scroll_center_vertically = true;

// Hint colors
hint_background_color = "#705050";
active_hint_background_color = "#DCDCCC";
img_hint_background_color = "#705050";
active_img_hint_background_color = "#DCDCCC";

// Big hints
register_user_stylesheet(
    "data:text/css," +
        escape(
            "@namespace url(\"http://www.w3.org/1999/xhtml\");\n" +
                "span.__conkeror_hint {\n"+
                " font-size: 14px !important;\n"+
                " line-height: 14px !important;\n"+
                "}"));
register_user_stylesheet(
    "data:text/css," +
        escape (
            "span.__conkeror_hint {" +
                " border: 1px solid #6f6f6f !important;" +
                " color: #f0dfaf !important;" +
                " background-color: #3f3f3f !important;" +
                "}"));

// Open clicks in buffers in the background
require("clicks-in-new-buffer.js");
clicks_in_new_buffer_target = OPEN_NEW_BUFFER_BACKGROUND;
clicks_in_new_buffer_button = 1;

// Load download buffers in the background
download_buffer_automatic_open_target = OPEN_NEW_BUFFER_BACKGROUND;

// Open external links in a new buffer
url_remoting_fn = load_url_in_new_buffer;

// Prevent quitting by accident
can_kill_last_buffer = false;

// Remove the clock and set the mode line
require("mode-line.js");
require("favicon.js");
remove_hook("mode_line_hook", mode_line_adder(clock_widget));
add_hook("mode_line_hook", mode_line_adder(buffer_icon_widget), true);
add_hook("mode_line_hook", mode_line_adder(loading_count_widget), true);
add_hook("mode_line_hook", mode_line_adder(buffer_count_widget), true);
add_hook("mode_line_hook", mode_line_adder(zoom_widget));
read_buffer_show_icons = true;

// Auto completion in the minibuffer
url_completion_use_history = true;
url_completion_use_bookmarks = true;
url_completion_use_webjumps = true;
minibuffer_auto_complete_default = true;

// Restore killed buffer function
var kill_buffer_original = kill_buffer_original || kill_buffer;
var killed_buffer_urls = [];

kill_buffer = function (buffer, force) {
    if (buffer.display_uri_string) {
        killed_buffer_urls.push(buffer.display_uri_string);
    }

    kill_buffer_original(buffer,force);
};

interactive("restore-killed-buffer-url", "Loads url from a previously killed buffer",
            function restore_killed_buffer_url (I) {
                if (killed_buffer_urls.length !== 0) {
                    var url = yield I.minibuffer.read(
                        $prompt = "Restore killed url:",
                        $completer = new all_word_completer($completions = killed_buffer_urls),
                        $default_completion = killed_buffer_urls[killed_buffer_urls.length - 1],
                        $auto_complete = "url",
                        $auto_complete_initial = true,
                        $auto_complete_delay = 0,
                        $require_match = true);

                    load_url_in_new_buffer(url);
                } else {
                    I.window.minibuffer.message("No killed buffer urls");
                }
            });

define_key(content_buffer_normal_keymap, "C-x u", "restore-killed-buffer-url");

// Eye guide
require('eye-guide.js');
define_key(content_buffer_normal_keymap, "space", "eye-guide-scroll-down");
define_key(content_buffer_normal_keymap, "S-space", "eye-guide-scroll-up");


// Delete existing webjumps
var unused_webjumps = ['answers', 'buildd','buildd-ports','clhs','cliki',
                       'clusty','creativecommons','debbugs','debfile','debpkg',
                       'debpopcon','debpts','debqa','freshmeat','kuro5hin',
                       'launchpad','lucky','ratpoisonwiki','sadelicious',
                       'scholar','sdelicious','slashdot','sourceforge',
                       'stumpwmwiki','ubuntubugs','ubuntufile','ubuntupkg',
                       'wiktionary','yahoo','bugzilla','ebay'
                      ];

for (var i = 0; i < unused_webjumps.length; i++)
{
    delete webjumps[unused_webjumps[i]];
}

// Webjumps
define_webjump("archwiki", "https://wiki.archlinux.org/index.php?search=%s",
               $alternative="http://www.archlinux.org");
define_webjump("arch-package", "https://www.archlinux.org/packages/?sort=&q=%s&maintainer=&flagged=",
               $alternative="https://www.archlinux.org/packages");
define_webjump("github", "http://github.com/search?q=%s&type=Everything");
define_webjump("youtube", "http://www.youtube.com/results?search_query=%s&search=Search");
define_webjump("youtube-user", "http://youtube.com/profile_videos?user=%s");

// Selection searches
function create_selection_search(webjump, key) {
    interactive(
        "internet-search-" + webjump,
        "Search for selected string with " + webjump,
        function (I) {
            var term;
            if (I.buffer.top_frame.getSelection() == "")
                term = yield I.minibuffer.read_url($prompt = "Search with " + webjump + ":",
                                                   $select = false,
                                                   $initial_value = webjump + " ");
            else
                term = webjump + " " + I.buffer.top_frame.getSelection();
            browser_object_follow(I.buffer, OPEN_NEW_BUFFER, term);
        });
    define_key(content_buffer_normal_keymap, key, "internet-search-" + webjump);

    interactive(
        "internet-search-" + webjump + "-prompted",
        "Search for a string with " + webjump,
        function (I) {
            var term = yield I.minibuffer.read_url($prompt = "Search with " + webjump + ":",
                                                   $select = false,
                                                   $initial_value = webjump + " ");
            browser_object_follow(I.buffer, OPEN_NEW_BUFFER, term);
        });
}

create_selection_search("archwiki", "C-c a");
create_selection_search("arch-package", "C-c p");
create_selection_search("dictionary", "C-c d");
create_selection_search("image", "C-c i");
create_selection_search("github", "C-c h");
create_selection_search("google", "C-c g");
create_selection_search("slang", "C-c s");
create_selection_search("wikipedia", "C-c w");
create_selection_search("youtube", "C-c y");
create_selection_search("youtube-user", "C-c u");
