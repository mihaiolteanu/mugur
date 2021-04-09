;;; mugur.el --- A high-level configurator for the ErgoDox EZ keyboard -*- lexical-binding: t -*-

;; Copyright (C) 2020 Mihai Olteanu

;; Author: Mihai Olteanu <mihai_olteanu@fastmail.fm>
;; Version: 1.0
;; Package-Requires: ((emacs "26.1") (s "1.12.0") (anaphora "1.0.4"))
;; Keywords: multimedia
;; URL: https://github.com/mihaiolteanu/mugur

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Generate all the keymaps, enums, arrays, make files and everything needed for
;; building a hex file and flashing a qmk powered keyboard.  See the github
;; README for details and usage.

;;; Code:

(require 's)
(require 'anaphora)

(defgroup mugur ()
  "ErgoDox EZ keyboard configurator"
  :group 'tools
  :prefix "mugur-")

;; qmk paths, keyboards and keymaps
(defcustom mugur-qmk-path nil
  "Path to the qmk firmware source code."
  :type '(string :tag "path")
  :group 'mugur)

(defcustom mugur-keyboard-name nil
  "The name of the qmk keyboard in use."
  :type '(string :tag "name")
  :group 'mugur)

(defcustom mugur-layout-name nil
  "The layout name used in the keymaps matrix.
Check the 'uint16_t keymaps' matrix in the default keymap.c of
your keyboard. Some have just \"LAYOUT\", others
\"LAYOUT_ergodox\", etc."
  :type '(string :tag "name")
  :group 'mugur)

(defcustom mugur-keymap-name "mugur"
  "The name of the mugur generated keymap"
  :type '(string :tag "name")
  :group 'mugur)

;; Rules
(defcustom mugur-leader-enable "no"
  "Enable the leader key functionality."
  :type '(boolean :tag "enable")
  :group 'mugur)

(defcustom mugur-rgblight-enable "no"
  "Enable the rgblight functionality."
  :type '(boolean :tag "enable")
  :group 'mugur)

(defcustom mugur-forcenkro-enable "yes"
  "Enable the force nkro functionality."
  :type '(boolean :tag "enable")
  :group 'mugur)

;; Configs
(defcustom mugur-tapping-term 180
  "Tapping term, in ms"
  :type '(integer :tag "ms")
  :group 'mugur)

(defcustom mugur-combo-term 100
  "Combo term, in ms"
  :type '(integer :tag "ms")
  :group 'mugur)

;; Others
(defcustom mugur-user-defined-keys nil
  "User defined keys for long or often used combinations."
  :type  '(alist :tag "keys")
  :group 'mugur)

(setf mugur-qmk-path      "/home/mihai/projects/qmk_firmware"
      mugur-keyboard-name "ergodox_ez"
      mugur-layout-name   "LAYOUT_ergodox"
      mugur-keymap-name   "mugur_test")

;; Mugur Kecodes and their transformation into qmk equivalent
(defun mugur--keycode (key)
  "Transform the mugur `KEY' into the qmk keycode equivalent."
  (or (mugur--letter   key)
      (mugur--digit    key)
      (mugur--f        key)
      (mugur--symbol   key)
      ;; Try to interpret strings of length one as characters (i.e. ">" as ?\>)
      (aand (stringp key)
            (= (length key) 1)
            (mugur--keycode (string-to-char key)))
      (mugur--oneshot  key)
      (mugur--layer    key)
      (mugur--modifier key)
      (mugur--modtap   key)
      (mugur--macro    key)
      (mugur--user-defined key)
      (and (listp key)                  ;Destructure '(k) and search for 'k
           (mugur--keycode (car key)))))

(defun mugur--letter (key)
  "Match where `KEY' is 'letter, \"letter\", '(letter) o \"letter\".
Letters from a-z, lowercase only."
  (aand (pcase key
          ((pred symbolp) key)
          ((pred stringp) key)
          (`(,k) k))        
        (if (symbolp it)
            (symbol-name it)
          it)
        (and (string-match "^[a-z]$" it)
             it)
        (format "KC_%s" (upcase it))))

(defun mugur--digit (key)
  "Match where `KEY' is 'digit, \"digit\", or '(digit).
Digits from 0-9."
  (aand (pcase key
          ((pred integerp) (number-to-string key))
          ((pred stringp ) key)
          (`(,k) (and (integerp k)
                      (number-to-string k))))
        (and (string-match "^[0-9]$" it)
             it)
        (format "KC_%s" it)))

(defun mugur--f (key)
  "Match where `KEY' is 'fxy \"fxy\" or '(fxy).
xy from 0-24, f lowercase or uppercase."
  (aand (pcase key
          ((pred symbolp) (symbol-name key))
          ((pred stringp) key)
          (`(,k) (and (symbolp k)
                      (symbol-name k))))
        (s-match (rx bol
                     (or (seq "f1" digit)
                         (seq "f2" (in "0-4"))
                         (seq "f"  digit))                    
                     eol)
                 it)
        (format "KC_%s" (upcase (car it)))))

(defun mugur--symbol (key)
  "Match `KEY' to any of the qmk keycodes, if any."
  (pcase key
    ;; Punctuation
    ((or 'enter             'ent       ) "KC_ENTER"               ) ;Return (Enter)
    ((or 'escape            'esc       ) "KC_ESCAPE"              ) ;Escape
    (    'bspace                         "KC_BSPACE"              ) ;Delete (Backspace)
    ((or 'tab               'tab       ) "KC_TAB"                 ) ;Tab
    ((or 'space             'spc       ) "KC_SPACE"               ) ;Spacebar
    ((or 'minus             ?\-)         "KC_MINUS"               ) ;- and _
    ((or 'equal             ?\=)         "KC_EQUAL"               ) ;= and +
    ((or 'lbracket          ?\[        ) "KC_LBRACKET"            ) ;[ and {
    ((or 'rbracket          ?\]        ) "KC_RBRACKET"            ) ;] and }
    ((or 'bslash            ?\\        ) "KC_BSLASH"              ) ;\ and |
    (    'nonus-hash                     "KC_NONUS_HASH"          ) ;Non-US # and ~
    ((or 'scolon            ?\;        ) "KC_SCOLON"              ) ;; and :
    ((or 'quote             ?\'        ) "KC_QUOTE"               ) ;' and
    ((or 'grave             ?\`        ) "KC_GRAVE"               ) ;` and ~, JIS Zenkaku/Hankaku
    ((or 'comma             ?\,        ) "KC_COMMA"               ) ;, and <
    ((or 'dot               ?\.        ) "KC_DOT"                 ) ;. and >
    ((or 'slash             ?\/        ) "KC_SLASH"               ) ;/ and ?

    ;; Lock keys
    ((or 'capslock          'caps      ) "KC_CAPSLOCK"            ) ;Caps Lock
    ((or 'scrollock         'slck      ) "KC_SCROLLOCK"           ) ;Scroll Lock, Brightness Down (macOS)
    ((or 'numlock           'nlck      ) "KC_NUMLOCK"             ) ;Keypad Num Lock and Clear
    ((or 'locking_caps      'lcap      ) "KC_LOCKING_CAPS"        ) ;Locking Caps Lock
    ((or 'locking_num       'lnum      ) "KC_LOCKING_NUM"         ) ;Locking Num Lock
    ((or 'locking_scroll    'lscr      ) "KC_LOCKING_SCROLL"      ) ;Locking Sroll Lock

    ;; Modifiers
    ((or 'lctl              'C         ) "KC_LCTL"                ) ;Left Control
    ((or 'lalt              'M         ) "KC_LALT"                ) ;Left Alt
    ((or 'lshift            'S         ) "KC_LSFT"                ) ;Left Shift
    ((or 'lgui              'G         ) "KC_LGUI"                ) ;Left GUI (Windows/Command/Meta key)
    ((or 'rctl              'rctrl     ) "KC_RCTRL"               ) ;Right Control
    ((or 'ralt              'ropt      ) "KC_RALT"                ) ;Right Alt (Option/AltGr)
    ((or 'rshift            'rsft      ) "KC_RSHIFT"              ) ;Right Shift
    ((or 'rgui              'rcmd      ) "KC_RGUI"                ) ;Right GUI (Windows/Command/Meta key)

    ;; International
    ((or 'ro                'int1      ) "INT1"                   ) ;JIS \ and _
    ((or 'kana              'int2      ) "INT2"                   ) ;JIS Katakana/Hiragana
    ((or 'jyen              'int3      ) "INT3"                   ) ;JIS ¥ and |
    ((or 'henk              'int4      ) "INT4"                   ) ;JIS Henkan
    ((or 'mhen              'int5      ) "INT5"                   ) ;JIS Muhenkan
    (    'int6                           "INT6"                   ) ;JIS Numpad ,
    (    'int7                           "INT7"                   ) ;International 7
    (    'int8                           "INT8"                   ) ;International 8
    (    'int9                           "INT9"                   ) ;International 9
    ((or 'lang1             'haen      ) "LANG1"                  ) ;Hangul/English
    ((or 'lang2             'hanj      ) "LANG2"                  ) ;Hanja
    (    'lang3                          "LANG3"                  ) ;JIS Katakana
    (    'lang4                          "LANG4"                  ) ;JIS Hiragana
    (    'lang5                          "LANG5"                  ) ;JIS Zenkaku/Hankaku
    (    'lang6                          "LANG6"                  ) ;Language 6
    (    'lang7                          "LANG7"                  ) ;Language 7
    (    'lang8                          "LANG8"                  ) ;Language 8
    (    'lang9                          "LANG9"                  ) ;Language 9

    ;; Commands
    ((or 'pscreen           'pscr      ) "KC_PSCREEN"             ) ;Print Screen
    ((or 'pause             'brk       ) "KC_PAUSE"               ) ;Pause, Brightness Up (macOS) 
    ((or 'insert            'ins       ) "KC_INSERT"              ) ;Insert
    (    'home                           "KC_HOME"                ) ;Home
    (    'pgup                           "KC_PGUP"                ) ;Page Up
    ((or 'delete            'del       ) "KC_DELETE"              ) ;Forward Delete
    (    'end                            "KC_END"                 ) ;End
    ((or 'pgdown            'pgdn      ) "KC_PGDOWN"              ) ;Page Down
    (    'right                          "KC_RIGHT"               ) ;Right Arrow
    (    'left                           "KC_LEFT"                ) ;Left Arrow
    (    'down                           "KC_DOWN"                ) ;Down Arrow
    (    'up                             "KC_UP"                  ) ;Up Arrow
    ((or 'application       'app       ) "KC_APPLICATION"         ) ;Application (Windows Context Menu Key)
    (    'power                          "KC_POWER"               ) ;System Power
    ((or 'execute           'exec      ) "KC_EXECUTE"             ) ;Execute
    (    'help                           "KC_HELP"                ) ;Help
    (    'menu                           "KC_MENU"                ) ;Menu
    ((or 'select            'slct      ) "KC_SELECT"              ) ;Select
    (    'stop                           "KC_STOP"                ) ;Stop
    ((or 'again             'agin      ) "KC_AGAIN"               ) ;Again
    (    'undo                           "KC_UNDO"                ) ;Undo
    (    'cut                            "KC_CUT"                 ) ;Cut
    (    'copy                           "KC_COPY"                ) ;Copy
    ((or 'paste             'pste      ) "KC_PASTE"               ) ;Paste
    (    'find                           "KC_FIND"                ) ;Find
    (    '_mute                          "KC__MUTE"               ) ;Mute
    (    '_volup                         "KC__VOLUP"              ) ;Volume Up
    (    '_voldown                       "KC__VOLDOWN"            ) ;Volume Down
    ((or 'alt_erase         'eras      ) "KC_ALT_erase"           ) ;Aternate Erase
    (    'sysreq                         "KC_SYSREQ"              ) ;SysReq/Attention
    (    'cancel                         "KC_CANCEL"              ) ;Cancel
    ((or 'clear             'clr       ) "KC_CLEAR"               ) ;Clear
    (    'prior                          "KC_PRIOR"               ) ;Prior
    (    'return                         "KC_RETURN"              ) ;Return
    (    'separator                      "KC_SEPARATOR"           ) ;Separator
    (    'out                            "KC_OUT"                 ) ;Out
    (    'oper                           "KC_OPER"                ) ;Open
    (    'clear_again                    "KC_CLEAR_again"         ) ;Clear/Again
    (    'crsel                          "KC_CRSEL"               ) ;CrSel/Props
    (    'exsel                          "KC_EXSEL"               ) ;ExSel

    ;; Media Keys
    ((or 'system-power      'pwr       ) "KC_SYSTEM_POWER"        ) ;System Power Down
    ((or 'system-sleep      'slep      ) "KC_SYSTEM_SLEEP"        ) ;System Sleep
    ((or 'system-wake       'wake      ) "KC_SYSTEM_WAKE"         ) ;System Wake
    ((or 'audio-mute        'mute      ) "KC_AUDIO_MUTE"          ) ;Mute
    ((or 'vol-up            'volu      ) "KC_AUDIO_VOL_UP"        ) ;Volume Up
    ((or 'vol-down          'vold      ) "KC_AUDIO_VOL_DOWN"      ) ;Volume Down
    ((or 'next-track        'mnxt      ) "KC_MEDIA_NEXT_TRACK"    ) ;Next Track
    ((or 'prev-track        'mprv      ) "KC_MEDIA_PREV_TRACK"    ) ;Previous Track
    ((or 'media-stop        'mstp      ) "KC_MEDIA_STOP"          ) ;Stop Track
    ((or 'media-play-pause  'mply      ) "KC_MEDIA_PLAY_PAUSE"    ) ;Play/Pause Track
    ((or 'media-select      'msel      ) "KC_MEDIA_SELECT"        ) ;Launch Media Player
    ((or 'media-eject       'ejct      ) "KC_MEDIA_EJECT"         ) ;Eject
    (    'mail                           "KC_MAIL"                ) ;Launch Mail
    ((or 'calculator        'calc      ) "KC_CALCULATOR"          ) ;Launch Calculator
    ((or 'my-computer       'mycm      ) "KC_MY_COMPUTER"         ) ;Launch My Computer
    ((or 'www-search        'wsch      ) "KC_WWW_SEARCH"          ) ;Browser Search
    ((or 'www-home          'whom      ) "KC_WWW_HOME"            ) ;Browser Home
    ((or 'www-back          'wbak      ) "KC_WWW_BACK"            ) ;Browser Back
    ((or 'www-forward       'wfwd      ) "KC_WWW_FORWARD"         ) ;Browser Forward
    ((or 'www-stop          'wstp      ) "KC_WWW_STOP"            ) ;Browser Stop
    ((or 'www-refresh       'wref      ) "KC_WWW_REFRESH"         ) ;Browser Refresh
    ((or 'www-favorites     'wfav      ) "KC_WWW_FAVORITES"       ) ;Browser Favorites
    ((or 'fast-forward      'mffd      ) "KC_MEDIA_FAST_FORWARD"  ) ;Next Track
    ((or 'rewind            'mrwd      ) "KC_MEDIA_REWIND"        ) ;Previous Track
    ((or 'brigthness-up     'briu      ) "KC_BRIGTHNESS_UP"       ) ;Brightness Up
    ((or 'brigthness-down   'brid      ) "KC_BRIGTHNESS_DOWN"     ) ;Brightness Down

    ;; Number Pad
    ((or 'kp_slash          'psls      ) "KP_SLASH"               ) ;Keypad /
    ((or 'kp_asterisk       'past      ) "KP_ASTERISK"            ) ;Keypad *
    ((or 'kp_minus          'pmns      ) "KP_MINUS"               ) ;Keypad -
    ((or 'kp_plus           'ppls      ) "KP_PLUS"                ) ;Keypad +
    ((or 'kp_enter          'pent      ) "KP_ENTER"               ) ;Enter
    ((or 'kp_1              'p1        ) "KP_1"                   ) ;Keypad 1 and End
    ((or 'kp_2              'p2        ) "KP_2"                   ) ;Keypad 2 and Down Arrow
    ((or 'kp_3              'p3        ) "KP_3"                   ) ;Keypad 3 and Page Down
    ((or 'kp_4              'p4        ) "KP_4"                   ) ;Keypad 4 and Left Arrow 
    ((or 'kp_5              'p5        ) "KP_5"                   ) ;Keypad 5
    ((or 'kp_6              'p6        ) "KP_6"                   ) ;Keypad 6 and Right Arrow
    ((or 'kp_7              'p7        ) "KP_7"                   ) ;Keypad 7 and Home
    ((or 'kp_8              'p8        ) "KP_8"                   ) ;Keypad 8 and Up Arrow
    ((or 'kp_9              'p9        ) "KP_9"                   ) ;Keypad 9 and Page Up
    ((or 'kp_0              'p0        ) "KP_0"                   ) ;Keypad 0 and Insert
    ((or 'kp_dot            'pdot      ) "KP_DOT"                 ) ;Keypad . and Delete
    ((or 'kp_equal          'peql      ) "KP_EQUAL"               ) ;Keypad = 
    ((or 'kp_comma          'pcmm      ) "KP_COMMA"               ) ;Keypad ,
    (    'kp_equal_as400                 "KP_EQUAL_AS400"         ) ;Keypad = on AS/400 keyboards

    ;; Special Keys
    ((or 'no                '---       ) "KC_NO"                  ) ;Ignore this key (NOOP)
    ((or 'trns              'nil       ) "KC_TRANSPARENT"         ) ;Use the next lowest non-transparent key

    ;; Quantum Keycodes
    (    'reset                          "RESET"                  ) ;Put the keyboard into bootloader mode for flashing
    (    'debug                          "DEBUG"                  ) ;Toggle debug mode
    ((or 'eeprom-reset      'eep_rst   ) "EEPROM_RESET"           ) ;Reinitializes the keyboard’s EEPROM (persistent memory)
        
    ;; Dynamic Macros
    ((or 'dyn_rec_start1    'dm_rec1   ) "KC_DYN_REC_START1"      ) ;Start recording Macro 1
    ((or 'dyn_rec_start2    'dm_rec2   ) "KC_DYN_REC_START1"      ) ;Start recording Macro 2
    ((or 'dyn_macro_play1   'dm_ply1   ) "KC_DYN_MACRO_PLAY1"     ) ;Replay Macro 1
    ((or 'dyn_macro_play2   'dm_ply2   ) "KC_DYN_MACRO_PLAY1"     ) ;Replay Macro 2
    ((or 'dyn_rec_stop      'dm_rstp   ) "KC_DYN_REC_STOP"        ) ;Finish the macro that is currently being recorded.

    ;; Grave Escape
    ((or 'gesc              'grave_esc ) "KC_GESC"                ) ;Escape when pressed, ` when Shift or GUI are held

    ;; Leader Key
    (    'lead                           "KC_LEADER"              ) ;The Leader Key

    ;; Mouse Keys
    ((or 'ms_up             'ms_u      ) "KC_MS_UP"               ) ;Move cursor up
    ((or 'ms_down           'ms_d      ) "KC_MS_DOWN"             ) ;Move cursor down
    ((or 'ms_left           'ms_l      ) "KC_MS_LEFT"             ) ;Move cursor left
    ((or 'ms_right          'ms_r      ) "KC_MS_RIGHT"            ) ;Move cursor right
    ((or 'ms_btn1           'btn1      ) "KC_MS_BTN1"             ) ;Press button 1
    ((or 'ms_btn2           'btn2      ) "KC_MS_BTN2"             ) ;Press button 2
    ((or 'ms_btn3           'btn3      ) "KC_MS_BTN3"             ) ;Press button 3
    ((or 'ms_btn4           'btn4      ) "KC_MS_BTN4"             ) ;Press button 4
    ((or 'ms_btn5           'btn5      ) "KC_MS_BTN5"             ) ;Press button 5
    ((or 'ms_btn6           'btn6      ) "KC_MS_BTN6"             ) ;Press button 6
    ((or 'ms_btn7           'btn7      ) "KC_MS_BTN7"             ) ;Press button 7
    ((or 'ms_btn8           'btn8      ) "KC_MS_BTN8"             ) ;Press button 8
    ((or 'ms_wh_up          'wh_u      ) "KC_MS_WH_UP"            ) ;Move wheel up
    ((or 'ms_wh_down        'wh_d      ) "KC_MS_WH_DOWN"          ) ;Move wheel down
    ((or 'ms_wh_left        'wh_l      ) "KC_MS_WH_LEFT"          ) ;Move wheel left
    ((or 'ms_wh_right       'wh_r      ) "KC_MS_WH_RIGHT"         ) ;Move wheel right
    ((or 'ms_accel0         'acl0      ) "KC_MS_ACCEL0"           ) ;Set speed to 0
    ((or 'ms_accel1         'acl1      ) "KC_MS_ACCEL1"           ) ;Set speed to 1
    ((or 'ms_accel2         'acl2      ) "KC_MS_ACCEL2"           ) ;Set speed to 2
        
    ;; Space Cadet
    (    'lspo                           "KC_LSPO"                ) ;Left Shift when held, ( when tapped
    (    'rspc                           "KC_RSPC"                ) ;Right Shift when held, ) when tapped
    (    'lcpo                           "KC_LCPO"                ) ;Left Control when held, ( when tapped
    (    'rcpc                           "KC_RCPC"                ) ;Right Control when held, ) when tapped
    (    'lapo                           "KC_LAPO"                ) ;Left Alt when held, ( when tapped
    (    'rapc                           "KC_RAPC"                ) ;Right Alt when held, ) when tapped
    (    'sftent                         "KC_SFTENT"              ) ;Right Shift when held, Enter when tapped

    ;; US ANSI Shifted Symbols
    ((or 'tilde              ?\~       ) "KC_TILDE"               ) ;~
    ((or 'exclaim            ?\!       ) "KC_EXCLAIM"             ) ;!
    ((or 'at                 ?\@       ) "KC_AT"                  ) ;@
    ((or 'hash               ?\#       ) "KC_HASH"                ) ;#
    ((or 'dollar             ?\$       ) "KC_DOLLAR"              ) ;$
    ((or 'percent            ?\%       ) "KC_PERCENT"             ) ;%
    ((or 'circumflex         ?\^       ) "KC_CIRCUMFLEX"          ) ;^
    ((or 'ampersand          ?\&       ) "KC_AMPERSAND"           ) ;&
    ((or 'asterisk           ?\*       ) "KC_ASTERISK"            ) ;*
    ((or 'lparen             ?\(       ) "KC_LEFT_PAREN"          ) ;(
    ((or 'rparen             ?\)       ) "KC_RIGHT_PAREN"         ) ;)
    ((or 'under              ?\_       ) "KC_UNDERSCORE"          ) ;_
    ((or 'plus               ?\+       ) "KC_PLUS"                ) ;+
    ((or 'left_curly         ?\{       ) "KC_LEFT_CURLY_BRACE"    ) ;{
    ((or 'right_curly        ?\}       ) "KC_RIGHT_CURLY_BRACE"   ) ;}
    ((or 'pipe               ?\|       ) "KC_PIPE"                ) ;|
    ((or 'colon              ?\:       ) "KC_COLON"               ) ;:
    ((or 'double_quote       ?\"       ) "KC_DOUBLE_QUOTE"        ) ;"
    ((or 'left_angle         ?\<       ) "KC_LEFT_ANGLE_BRACKET"  ) ;<
    ((or 'right_angle        ?\>       ) "KC_RIGHT_ANGLE_BRACKET" ) ;>
    ((or 'question           ?\?       ) "KC_QUESTION"            ) ;?

    ;; RGB Ligthing
    (    'rgb_tog                        "RGB_TOG"                ) ;Toggle RGB lighting on or off
    ((or 'rgb_mode_forward  'rgb_mod   ) "RGB_MOD"                ) ;Cycle through modes, reverse direction when Shift is held 
    ((or 'rgb_mode_reverse  'rgb_mod   ) "RGB_RMOD"               ) ;Cycle through modes in reverse, forward direction when Shift is held
    (    'rgb_hui                        "RGB_HUI"                ) ;Increase hue, decrease hue when Shift is held
    (    'rgb_hud                        "RGB_HUD"                ) ;Decrease hue, increase hue when Shift is held
    (    'rgb_sai                        "RGB_SAI"                ) ;Increase saturation, decrease saturation when Shift is held
    (    'rgb_sad                        "RGB_SAD"                ) ;Decrease saturation, increase saturation when Shift is held
    (    'rgb_vai                        "RGB_VAI"                ) ;Increase value (brightness), decrease value when Shift is held
    (    'rgb_vad                        "RGB_VAD"                ) ;Decrease value (brightness), increase value when Shift is held
    ((or 'rgb_mode_plain    'rgb_m_p   ) "RGB_MOIDE_PLAIN"        ) ;Static (no animation) mode
    ((or 'rgb_mode_breathe  'rgb_m_b   ) "RGB_MODE_BREATHE"       ) ;Breathing animation mode
    ((or 'rgb_mode_rainbow  'rgb_m_r   ) "RGB_MODE_RAINBOW"       ) ;Rainbow animation mode
    ((or 'rgb_mode_swirl    'rgb_m_sw  ) "RGB_MODE_SWIRL"         ) ;Swirl animation mode
    ((or 'rgb_mode_snake    'rgb_m_sn  ) "RGB_MODE_SNAKE"         ) ;Snake animation mode
    ((or 'rgb_mode_knight   'rgb_m_k   ) "RGB_MODE_KNIGHT"        ) ;"Knight Rider" animation mode
    ((or 'rgb_mode_xmas     'rgb_m_x   ) "RGB_MODE_XMAS"          ) ;Christmas animation mode
    ((or 'rgb_mode_gradient 'rgb_m_g   ) "RGB_MODE_GRADIENT"      ) ;Static gradient animation mode
    ((or 'rgb_mode_rgbtest  'rgb_m_t   ) "RGB_MODE_RGBTEST"       ) ;Red, Green, Blue test animation mode

    ;; Key Lock
    (    'lock                           "KC_LOCK"                ) ;Hold down the next key pressed, until the key is pressed again
    ))


;; Helper functions for MODs. Useful for multiple features.
(defun mugur--mod (mod)
  "Transform the mugur `MOD' to the qmk equivalent.
https://docs.qmk.fm/#/mod_tap"
  (pcase mod
    ((or 'C "C") "MOD_LCTL")
    ((or 'M "M") "MOD_LALT")
    ((or 'G "G") "MOD_LGUI")
    ((or 'S "S") "MOD_LSFT")))

(defun mugur--mods (mods)
  "Return the qmk modifiers if all `MODS' are transformable as such.
Return nil otherwise."
  (aand (mapcar #'mugur--mod mods)
        (and (not (member nil it))
             it)))

(defun mugur--mods-plus-key (key)
  (aand (pcase key
          (`(,key) (and (symbolp key)
                        (symbol-name key)))
          ((pred symbolp) (symbol-name key))
          ((pred listp)   (mapconcat #'symbol-name key "-")))
        ;; Now we have a "MOD1-MOD2-kc" string, or similar
        (and (s-match "-" it)
             (s-split "-" it))        
        (let* ((mods (mugur--mods (butlast it)))
               ;; Avoid situations where "space" would result in
               ;; SEND_STRING(\"space)\") and not KC_SPACE, for example.
               (key (or (mugur--keycode (intern (car (last it))))
                        (mugur--keycode (car (last it))))))
          (and mods key
               `(,@mods ,key)))))

(defun mugur--modifier (key)
  "Hold down modifier and press key.
Transform C-M-a into LCTL(LALT(KC_A)).  Return
nil if any of the modifiers or keys are invalid."
  (aand (symbolp key)
        (mugur--mods-plus-key key)
        (--reduce-r (format "%s(%s)" it acc)
                    it)
        (s-replace "MOD_" "" it)))

(defun mugur--modtap (key)
  "Modifier(s) when held, key when tapped
'(C M a), C+M when held, a when tapped."
  (aand (listp key)
        (mugur--mods-plus-key key)
        (format "MT(%s, %s)"
                (--reduce-r (format "%s | %s" it acc)
                            (butlast it))
                (car (last it)))))


(mugur--macro '(C-M-u C-space x))
(mugur--macro '(C-u "blaa"))
(mugur--modifier 'C-a)

(defun mugur--macro (key)
  (or
   ;;A list of more than one element
   (aand (and (listp key) 
              (> (length key) 1))
         ;;..transform each element into a valid SEND_STRING entry.
         (mapcar (lambda (k) 
              (or
               ;;A simple string is sent as such.
               (and (stringp k)       
                    (format "\"%s\"" k))

               ;; One or more modifiers keys plus a simple key becomes SS_ + TAP_
               ;; i.e. C-M-a becomes SS_LCTL(SS_LALT(SS_TAP(X_A)))
               (aand (mugur--mods-plus-key k)
                     ;; Wrap the last key around SS_TAP
                     `(,@(butlast it)
                       ,(format "SS_TAP(%s)" (car (last it))))                     
                     (--reduce-r (format "%s(%s)" it acc) it)
                     (s-replace "MOD_" "SS_" it)
                     (s-replace "KC_" "X_" it))

               ;; A simple key becomes SS_TAP(X_KEY)
               (aand (mugur--keycode k)
                     (format "SS_TAP(%s)"
                             (s-replace "KC_" "X_" it)))))
            key)
         ;; Only a wf macro if all the elements have a SEND_STRING equivalent
         (and (not (member nil it))
              (format "SEND_STRING(%s)"
                      (mapconcat #'identity it " "))))
   ;;A simple string
   (and (stringp key)
        (format "SEND_STRING(\"%s\")" key))))

(defun mugur--user-defined (key)
  "User defined `KEY'."
  (aand (alist-get key mugur-user-defined-keys)
        (mugur--keycode it)))

(defun mugur--oneshot (key)
  (pcase key
    ((and `(,'osm ,m)
          (guard (mugur--mod m)))
     (format "OSM(%s)" (mugur--mod m)))
    (`(,'osl ,x) (format "OSL(%s)" x))))

(defun mugur--layer (key)
  (pcase key
    (`(,(or 'df 'mo 'osl 'tg 'tt) ,layer)     
     (format "%s(%s)"
             (upcase (symbol-name (car key)))
             layer))
    (`(lm ,layer ,mod)
     (aand (mugur--mod mod)
           (format "LM(%s, %s)" layer it)))
    (`(lt ,layer ,kc)
     (aand (mugur--keycode kc)
           (format "LT(%s, %s)" layer it)))))


;; Mugur Layers and their transformation into qmk equivalent
(defconst mugur--rules-mk
  "LEADER_ENABLE = ${leader-enable}
   RGBLIGHT_ENABLE = ${rgblight-enable}
   FORCE_NKRO = ${force-nkro}")

(defconst mugur--config-h
  "#undef TAPPING_TERM
   #define TAPPING_TERM ${tapping-term}
   #define LEADER_TIMEOUT ${leader-timeout}

   /* Mouse */
   //#define MOUSEKEY_INTERVAL    ${mousekey-interval}
   //#define MOUSEKEY_DELAY       ${mousekey-delay}
   //#define MOUSEKEY_TIME_TO_MAX ${mousekey-time-to-max}
   //#define MOUSEKEY_MAX_SPEED 7 ${mousekey-max-speed}
   //#define MOUSEKEY_WHEEL_DELAY ${mousekey-wheel-delay}
   
   #define FORCE_NKRO
   #undef RGBLIGHT_ANIMATIONS
")

(defconst mugur--keymap-c
  "#include QMK_KEYBOARD_H
   #include \"version.h\"
   
   enum layer_codes {
       ${layer-codes}
   };
   
   enum custom_keycodes {
       EPRM = SAFE_RANGE,
       ${custom-keycodes}
   };
   
   const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
       ${layers}
   };
   
   bool process_record_user(uint16_t keycode, keyrecord_t *record) {
       if (record->event.pressed) {
           switch (keycode) {
   	case EPRM:
   	    eeconfig_init();
   	    return false;
           ${macro-keys}
   		
   	}
       }
      return true;
   }
  ")

(defconst mugur--macro-key
  "case ${key-name}:
       ${contents};
       return false;
")

(defconst mugur--layout-keymap
  "[${layer-name}] = ${layout-name}(${layer-keys})")

(defun mugur--to-string (val)
  (pcase val
    ((pred numberp)    (number-to-string val))
    ((pred stringp)    val)
    ((pred symbolp)    (symbol-name val))
    ((pred characterp) (char-to-string val))    
    (`(,v) (mugur--force-string v))))

(defun mugur--transform-layer (layer)
  "Transform the mugur keys in the `LAYER' list to their qmk equivalent.
The car of the `LAYER' is the layer name, which is preserved."
  (aand (--map (or (mugur--keycode it)
                   (error (format "Invalid mugur keycode, %s" it)))
               (cdr layer))
        `(,(or (mugur--to-string (car layer))
               (error (format "Invalid layer name, %s" (car layer))))
          ,@it)))

(let ((layers '(((base) a "what" c d e)
                ("numbers" 1 2 3 "this is" 5))))
  (mapcar #'mugur--transform-layer layers))

(defun mugur--write-file (name values)
  (with-temp-file
      (aand (concat (file-name-as-directory mugur-qmk-path)
                    (file-name-as-directory "keyboards")
                    (file-name-as-directory mugur-keyboard-name)
                    (file-name-as-directory "keymaps")
                    (file-name-as-directory mugur-keymap-name))
            (and (or (file-directory-p it)
                     (make-directory it))
                 it)
            (concat it name))
    (aand (s-format (pcase name
                      ("rules.mk" mugur--rules-mk)
                      ("config.h" mugur--config-h)
                      ("keymap.c" mugur--keymap-c))
                    'aget
                    values)
          (insert it))))

(defun mugur--write-rules-mk ()
  (mugur--write-file
   "rules.mk"
   `((leader-enable   . ,mugur-leader-enable)
     (rgblight-enable . ,mugur-rgblight-enable)
     (force-nkro      . ,mugur-forcenkro-enable))))

(defun mugur--write-config-h ()
  (mugur--write-file
   "config.h"
   `((tapping-term    . ,mugur-tapping-term)
     (combo-term      . ,mugur-combo-term)
     (rgblight-enable . ,mugur-rgblight-enable)
     (force-nkro      . ,mugur-forcenkro-enable))))

(defun mugur--write-keymap-c (layout)
  (let* ((qmk-layout-raw
          (mapcar #'mugur--transform-layer layout))
         (qmk-layout
          (--tree-map (if (and (stringp it)
                               (s-match "^SEND_STRING" it))
                          (format "MACRO_%s" (md5 it))
                        it)
                      qmk-layout-raw) )
         (macros (--map (list it (format "MACRO_%s" (md5 it)))
                        (--filter (and (stringp it)
                                       (s-match "^SEND_STRING" it))
                                  (-flatten qmk-layout-raw)))))

    (mugur--write-file
     "keymap.c"
     `((layer-codes
        . ,(--reduce-r (format "%s, \n       %s"
                               (upcase acc) (upcase it))
                       (mapcar #'car layout)))

       (custom-keycodes
        . ,(--reduce-r (format "%s, \n       %s" acc it)
                       (mapcar #'cadr macros)))

       (layers
        . ,(--reduce-r (format "%s, \n       %s" it acc)
                       (mapcar (lambda (k)
                            (s-format mugur--layout-keymap 'aget
                                      `((layer-name . ,(upcase (car k)))
                                        (layout-name . ,(or mugur-layout-name
                                                            (error "mugur-layout-name not set.")))
                                        (layer-keys . ,(--reduce-r (format "%s, %s" it acc) (cdr k))))))
                          qmk-layout)))

       (macro-keys
        . ,(--reduce-r (format "%s\n         %s" it acc)
                       (mapcar (lambda (k)
                            (s-format mugur--macro-key 'aget
                                      `((key-name . ,(cadr k))
                                        (contents . ,(car k)))))
                          macros)))))
    
    ))

(defun mugur-mugur (layout)
  (mugur--write-rules-mk)
  (mugur--write-keymap-c layout)
  )


(provide 'mugur)

;;; mugur.el ends here
