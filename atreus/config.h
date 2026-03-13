/* SPDX-License-Identifier: GPL-2.0-or-later */

#pragma once

#define VIAL_KEYBOARD_UID {0x6B, 0xBA, 0xFA, 0x7D, 0xDA, 0x53, 0xF5, 0x0C}

#define VIAL_UNLOCK_COMBO_ROWS { 0, 0, 3,  3, 3 }
#define VIAL_UNLOCK_COMBO_COLS { 0, 1, 7, 10, 11}

#define DYNAMIC_KEYMAP_LAYER_COUNT 4

/* Enable Permissive Hold for fast Emacs rolling */
#define PERMISSIVE_HOLD

/* Enable Retro Tapping for faster modifiers */
#define RETRO_TAPPING

/* Optional: Tune the timing (Default is 200ms) */
/* 135ms is usually snappier for Mod-Tap */
#define TAPPING_TERM 135
