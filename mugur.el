;;; mugur.el --- Simplify the generation of keymaps for qmk-powered keyboards -*- lexical-binding: t -*-

;; Copyright (C) 2020 Mihai Olteanu

;; Author: Mihai Olteanu <mihai_olteanu@fastmail.fm>
;; Version: 1.0
;; Package-Requires: ((emacs "26.1") (s "1.12.0"))
;; Keywords: multimedia
;; URL: https://github.com/mihaiolteanu/vuiet

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



;;; Code:

(require 's)

(defgroup mugur ()
  "qmk keyboard configurator"
  :group 'tools
  :prefix "mugur-")

(defcustom mugur-qmk-path nil
  "Path to where you git cloned the qmk firmware source code 
(https://github.com/qmk/qmk_firmware)"
  :type '(string :tag "path")
  :group 'mugur)

(defconst mugur-layout-vertical nil)
(defconst mugur-layout-horizontal nil)

(defconst mugur--supported-keycodes
  '(("Letters and Numbers"
     (a) (b) (c) (d) (e) (f) (g) (h) (i) (j) (k) (l) (m)
     (n) (o) (p) (q) (r) (s) (t) (u) (v) (w) (x) (y) (z)           
     (1) (2) (3) (4) (5) (6) (7) (8) (9) (0))
    
    ("Function Keys"
     (F1)  (F2)  (F3)  (F4)  (F5)  (F6)  (F7)  (F8)  (F9)  (F10)
     (F11) (F12) (F13) (F14) (F15) (F16) (F17) (F18) (F19) (F20)
     (F21) (F22) (F23) (F24))

    ("Punctuation"
     (ENT "enter") (enter) (ESC "escape") (escape) (bspace)
     (TAB "tab") (tab) (SPC "space") (space)
     (- "minus") (= "equal")
     (lbracket "lbracket") ("[" "lbracket")
     (rbracket "rbracket") ("]" "rbracket")
     (bslash) ("\\" "bslash")
     (nonus-hash "nonus_hash")
     (colon "scolon") (";" "scolon") (quote) ("'" "quote")
     (grave "grave") ("`" "grave")
     (comma "comma") ("," "comma") (dot "dot") ("." "dot")
     (slash) ("/" "slash"))
    
    ("Shifted Keys"
     (~ "tilde") (! "exclaim") (@ "at")
     (hash) ("#" "hash") ($ "dollar") (% "percent")
     (^ "circumflex") (& "ampersand") (* "asterix")
     (lparen "left_paren") (rparen "right_paren")
     ("(" "left_paren") (")" "right_paren")
     (_ "underscore") (+ "plus")
     ({ "left_curly_brace") (} "right_curly_brace")
     (| "pipe") (: "colon") ("\"" "double_quote") (double_quote "double_quote")
     (< "left_angle_bracket") (> "right_angle_bracket")
     (question) ("?" "question"))
    
    ("Modifiers"
     (C "lctl") (M "lalt")
     (S "lsft") (G "lgui")
     (C-M "lca") (C-M-S "meh") (C-M-G "hypr"))

    ("Commands"
     (insert) (home) (prior "pgup") (delete) (end) (next "pgdown")
     (right) (left) (down) (up))

    ("Media Keys"
     (vol_up "audio_vol_up") (vol_down "audio_vol_down")
     (mute "audio_mute") (stop "media_stop"))

    ("Mouse Keys"
     (ms_up) (ms_down) (ms_left) (ms_right)
     (ms_btn1) (ms_btn2) (ms_btn3) (ms_btn4) (ms_btn5)
     (ms_wh-up) (ms_wh-down) (ms_wh-left) (ms_wh-right)
     (ms_accel1) (ms_accel2) (ms_accel3))
    
    ("Special Keys"
     (--- "_x_") (() "___"))))

(let ((mugur--keycodes (make-hash-table :test 'equal)))
  (defun mugur--keycode-string (keycode)
    (if (= (length keycode) 2)
        (upcase (cadr keycode))
      (if (numberp (car keycode))
          (number-to-string (car keycode))
        (symbol-name (car keycode)))))
  
  (defun mugur--set-keycodes ()
    "Add all the keycodes into a hashtable."
    (dolist (categories mugur--supported-keycodes)
      (dolist (entry (cdr categories))
          (puthash (car entry)
                   (upcase (mugur--keycode-string entry))
                   mugur--keycodes))))

  (defun mugur--keycode-raw (key)
    (if (not (hash-table-empty-p mugur--keycodes))
        (awhen (gethash key mugur--keycodes)
          it)
      ;; First call, update the hash table.
      (mugur--set-keycodes)
      (mugur--keycode-raw key)))

  (defun mugur--key-in-category-p (category key)
    (cl-find key
     (cdr (cl-find category
                   mugur--supported-keycodes
                   :test #'string-equal :key #'car))
     :key #'car))

  (defun mugur--modifier-key-p (key)
    (mugur--key-in-category-p "Modifiers" key))
  
  (defun mugur--special-key-p (key)
    (mugur--key-in-category-p "Special Keys" key))
  
  (cl-defun mugur--keycode (key &key (ss nil) (mod nil))
    (awhen (mugur--keycode-raw key)
      (if (mugur--special-key-p key)
          it
        (if (mugur--modifier-key-p key)
            (if ss
                (concat "SS_" it)
              (if mod
                  (concat "MOD_" it)
                it))
          (if ss
              (format "SS_TAP(X_%s)" it)              
            (concat "KC_" it))))))

  (cl-defun mugur--key-or-sequence (key &key (ss nil))
    "Generate simple keys or key sequences, like M-x or C-M-a.
If SS is t, generate the key sequence as needed by SEND_STRING
macros."
    (cond ((awhen (mugur--keycode key :ss ss) it))
          ((s-contains? "-" (if (symbolp key)
                                (symbol-name key)
                              ""))
           (let* ((s (s-split "-" (symbol-name key)))
                  (prefix (s-join "-" (butlast s))))
             (if (mugur--modifier-key-p (intern prefix))
                 (mugur--modifier+key (intern prefix)
                               (intern (car (last s)))
                               :ss ss)
               nil)))
          ((and (stringp key) ss) (format "\"%s\"" key))
          (t nil)))

  (defun mugur-keycodes ()
    "Display all the supported keycodes in a new buffer."
    (interactive)
    (let ((b (get-buffer-create "keycodes.org")))
      (with-current-buffer b
          (org-mode)
        (erase-buffer)
        (dolist (category mugur--supported-keycodes)
          (insert (format "* %s\n\n" (car category)))
          (let ((max (cl-loop for entry in (cdr category)
                              maximize (length (mugur--keycode-string entry)))))
            (dolist (entry (cdr category))            
              (insert (format (concat "\t%-" (number-to-string max)
                                      "S --> %s\n")
                              (car entry) (mugur--keycode-string entry)))))
          (insert "\n")))
      (switch-to-buffer b))))

(defun mugur--modtap (mod key)
  "MOD when held, KEY when tapped."
  (s-format "MT($0, $1)" 'elt
            (list (mugur--keycode mod :mod t)
                  (mugur--keycode key))))

(cl-defun mugur--modifier+key (mod key &key (ss nil))
  "Hold MOD and press KEY."
  (s-format "$0($1)" 'elt
            (list (mugur--keycode mod :ss ss)
                  (if ss
                      (format "\"%s\"" (symbol-name key))
                    (mugur--keycode key)))))

(defun mugur--one-shot-mod (mod)
  "Hold down MOD for one key press only."
  (format "OSM(%s)" (mugur--keycode mod :mod t)))

(defun mugur--one-shot-layer (layer)
  "Switch to LAYER for one key press only."
  (format "OSL(%s)" (upcase (symbol-name layer))))

;;;; Macros
(cl-defstruct mugur--macro
  name expansion)

(defun mugur--macro-transform-keys (keys)
  (mapcar (lambda (key)
       (mugur--key-or-sequence key :ss t))
     keys))

(defun mugur--macro-define (entry)
  (cl-reduce
   (lambda (item1 item2)
     (concat item1 " " item2))
   (mugur--macro-transform-keys entry)))

(defun mugur--macro (entry)
  (let ((expansion (mugur--macro-define entry)))
    (make-mugur--macro
     :name (format "SS_MACRO_%s" (upcase (md5 expansion)))
     :expansion (mugur--macro-define entry))))

(defun mugur--extract-macros (keys)
  (cl-remove-duplicates
   (remove
    nil
    (mapcar (lambda (key)
         (let ((tr (mugur--transform-key key)))
           (if (s-contains-p "SS_MACRO_" tr)
               (mugur--macro key)
             nil)))
       keys))
   :key #'mugur--macro-name
   :test #'string-equal))

;;;; Combos
(cl-defstruct mugur--combo
  name keys expansion)

(defun mugur--combo-define (combo)
  (let* ((keycodes (mapcar #'mugur--keycode (butlast combo)))
         (last (last combo))
         (ss (mugur--macro-transform-keys
              (if (listp (car last))
                  (car last)
                last))))
    (list keycodes ss)))

(defun mugur--combo (combo name)
  (let ((c (mugur--combo-define combo)))
    (make-mugur--combo
     :name name
     :keys (cl-reduce (lambda (item1 item2)
                        (concat item1 ", " item2))
                      (car (butlast c)))
     :expansion (car (last c)))))

;; Tap Dance
(cl-defstruct mugur--tapdance
  name key-name key1 key2)

(defun mugur--tapdance-pp (key1 key2)
  (and (and key1 key2)
       (and (not (mugur--modifier-key-p key1))
            (mugur--keycode key1))
       (and (not (mugur--modifier-key-p key2))
            (or (mugur--keycode key2)
                (symbolp key2)))
       t))

(defun mugur--tapdance (keys)
  (when (= (length keys) 2)
    (let ((key1 (car keys))
          (key2 (cadr keys)))
      (when (mugur--tapdance-pp key1 key2)
        (make-mugur--tapdance
         :name (format "TD_%s_%s"
                       (mugur--keycode-raw key1)
                       (or (mugur--keycode-raw key2)
                           (upcase (symbol-name key2))))
         :key-name (format "TD(TD_%s_%s)"
                           (mugur--keycode-raw key1)
                           (or (mugur--keycode-raw key2)
                               (upcase (symbol-name key2)))) 
         :key1 (mugur--keycode key1)
         :key2 (or (mugur--keycode key2)
                   (upcase (symbol-name key2))))))))

(defun mugur--tapdance-extract (keys)
  (cl-remove-duplicates
   (remove nil
           (mapcar (lambda (key)
                (aif (mugur--tapdance key)
                    it
                  nil))
              keys))
   :key #'mugur--tapdance-key-name
   :test #'string-equal))

;;;; Layer Switching
(defun mugur--layer-switching-codes ()
  '(((df layer)  "Set the base (default) layer.")
    ((mo layer)  "Momentarily turn on layer when pressed (requires KC_TRNS on destination layer).")
    ((osl layer) "Momentarily activates layer until a key is pressed. See One Shot Keys for details.")
    ((tg layer)  "Toggle layer on or off.")
    ((to layer)  "Turns on layer and turns off all other layers, except the default layer.")
    ((tt layer)  "Normally acts like MO unless it's tapped multiple times, which toggles layer on.")
    ((lm layer mod) "Momentarily turn on layer (like MO) with mod active as well.")
    ((lt layer kc) "Turn on layer when held, kc when tapped")))

(defun mugur--layer-switch-p (key)
  (cl-member key (mugur--layer-switching-codes)
             :key #'caar))

(defun mugur--layer-switch (action layer &optional key-or-mod)
  "Generate code to switch to the given LAYER."
  (if key-or-mod
      (format "%s(%s, %s)"
                (upcase (symbol-name action))
                (upcase (symbol-name layer))
                (mugur--keycode key-or-mod))
    (format "%s(%s)"
            (upcase (symbol-name action))
            (upcase (symbol-name layer)))))

(defun mugur-layer-switching ()
  "Display all the layer switching codes in a new buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "layer-switching-codes")
    (org-mode)
    (local-set-key (kbd "q") 'kill-current-buffer)
    (insert "* Layer Switching Codes\n\n")
    (mapc (lambda (code)
         (insert (format "%-15s - %s\n" (car code) (cadr code))))
       (mugur--layer-switching-codes))
    (switch-to-buffer (get-buffer-create "layer-switching-codes"))))


;;;; Keymaps, Layers and Transformations.
(defun mugur--transform-key (key)
  "Transform a keymap KEY to the qmk equivalent."
  (pcase key
    (`() (mugur--keycode '()))
    ((and `(,key)
          (guard (mugur--key-or-sequence key)))
     (mugur--key-or-sequence key))
    ((and `(,modifier ,key)
          (guard (mugur--modifier-key-p modifier)))
     (mugur--modtap modifier key))
    ((and `(,key1 ,key2)
          (guard (mugur--tapdance key)))
     (mugur--tapdance-key-name (mugur--tapdance (list key1 key2))))
    (`(osm ,mod) (mugur--one-shot-mod mod))
    (`(osl ,layer) (mugur--one-shot-layer layer))
    ((and `(,action ,layer)
          (guard (mugur--layer-switch-p action)))
     (mugur--layer-switch action layer))
    ((and `(,action ,layer ,key-or-mod)
          (guard (mugur--layer-switch-p action)))
     (mugur--layer-switch action layer key-or-mod))
    (_ (mugur--macro-name (mugur--macro key)))))

(defun mugur--transform-keys (keys)
  (mapcar #'mugur--transform-key keys))

(cl-defstruct mugur--layer
  name
  index
  keys
  leds
  orientation)

(cl-defstruct mugur--keymap
  name
  keyboard
  layers
  combos
  macros
  tapdances)

(cl-defun mugur--new-layer (name index keys &key (leds nil) (orientation 'horizontal))
  (make-mugur--layer
   :name name
   :index index
   :keys keys
   :leds leds
   :orientation orientation))

(cl-defun mugur--new-keymap (&key name keyboard layers
                           (combos nil) (macros nil) (tapdances nil))
  (make-mugur--keymap
   :name name
   :keyboard keyboard
   :layers layers
   :combos combos
   :macros macros
   :tapdances tapdances))

(let (keymaps)
  (defun mugur--leds (layer)
    (if (= (length (cadr layer)) 3)
        (cadr layer)
      nil))

  (defun mugur--keys (layer)
    (if (= (length (cadr layer)) 3)
        (caddr layer)
      (cadr layer)))

  (defun mugur--replace-custom-keys (custom-keys keys)
    (let ((names (mapcar #'car custom-keys)))
      (print names)
      (mapcar (lambda (key)
           (if (member (car key) names)
               (cadr (cl-find (car key) custom-keys :key #'car))
             key))
         keys)))

;;;###autoload
  (cl-defun mugur-keymap (name keyboard &key
                               (layers nil)
                               (combos nil)       
                               (custom-keys nil))
    "Define a qmk keymap."
    (cl-pushnew
     (mugur--new-keymap
      :name name
      :keyboard keyboard
      :layers
      (let ((index 0))
        (mapcar (lambda (layer)
             (let ((name (car layer))
                   (leds (mugur--leds layer))
                   (keys (mugur--keys layer)))
               (setf index (+ 1 index))
               (mugur--new-layer (upcase name) index
                                 (mugur--transform-keys
                                  (mugur--replace-custom-keys custom-keys keys))
                                 :leds leds)))
           layers))
      
      :combos
      (let ((index 0))
        (mapcar (lambda (combo)
             (setf index (+ 1 index))
             (mugur--combo combo (format "COMBO_%s" index)))
           combos))

      :macros
      (cl-remove-duplicates
       (apply #'append
              (mapcar (lambda (layer)
                   (mugur--extract-macros
                    (mugur--replace-custom-keys
                     custom-keys (mugur--keys layer))))
                 layers))
       :key #'mugur--macro-name
       :test #'string-equal)

      :tapdances
      (cl-remove-duplicates
       (apply #'append
              (mapcar (lambda (layer)
                   (mugur--tapdance-extract
                    (mugur--replace-custom-keys
                     custom-keys (mugur--keys layer))))
                 layers))
       :key #'mugur--tapdance-name
       :test #'string-equal))
     keymaps))

  (defun mugur--keymaps-all ()
    keymaps))

;;;; C Code Generators
(defun mugur--c-custom-keycodes (macros)
  (with-temp-buffer
    (insert "enum custom_keycodes {\n\tEPRM = SAFE_RANGE,\n")
    (cl-dolist (keycode macros)
      (insert (format "\t%s,\n"
                      (upcase (mugur--macro-name keycode)))))
    (insert "};\n\n")
    (buffer-string)))

(defun mugur--c-process-record-user (macros)
  (with-temp-buffer
    (insert "bool process_record_user(uint16_t keycode, keyrecord_t *record) {\n")
    (insert "\tif (record->event.pressed) {\n")
    (insert "\t\tswitch (keycode) {\n")
    (insert "\t\tcase EPRM:\n")
    (insert "\t\t\teeconfig_init();\n")
    (insert "\t\t\treturn false;\n")
    (cl-dolist (macro macros)
      (insert (format "\t\tcase %s:\n" (mugur--macro-name macro)))
      (insert (format "\t\t\tSEND_STRING(%s);\n" (mugur--macro-expansion macro)))
      (insert "\t\t\treturn false;\n"))
    (insert "\t\t}\n\t}\n\treturn true;\n}\n\n")
    (buffer-string)))

(defun mugur--c-tapdance-enum (tapdances)
  (with-temp-buffer
    (insert "enum {\n")
    (cl-dolist (tapdance tapdances)
      (insert (format "\t%s,\n" (mugur--tapdance-name tapdance))))
    (insert "};\n\n")
    (buffer-string)))

(defun mugur--c-tapdance-actions (tapdances)
  (with-temp-buffer
    (insert "qk_tap_dance_action_t tap_dance_actions[] = {\n")
    (cl-dolist (tapdance tapdances)
      (insert
       (if (s-contains-p "KC_" (mugur--tapdance-key2 tapdance))
           (format "\t[%s] = ACTION_TAP_DANCE_DOUBLE(%s, %s),\n"
                      (mugur--tapdance-name tapdance)
                      (mugur--tapdance-key1 tapdance)
                      (mugur--tapdance-key2 tapdance))
         ;; This is a layer, not a key
         (format "\t[%s] = ACTION_TAP_DANCE_LAYER_TOGGLE(%s, %s),\n"
                      (mugur--tapdance-name tapdance)
                      (mugur--tapdance-key1 tapdance)
                      (mugur--tapdance-key2 tapdance)))))
    (insert "};\n\n")
    (buffer-string)))

(defun mugur--c-combos-combo-events (combos)
  (with-temp-buffer
    (insert "enum combo_events {\n")
    (cl-dolist (combo combos)
      (insert (format "\t%s,\n" (upcase (mugur--combo-name combo)))))
    (insert "};\n\n")
    (buffer-string)))

(defun mugur--c-combos-progmem (combos)
  (with-temp-buffer
    (cl-dolist (combo combos)
      (insert
       (format "const uint16_t PROGMEM %s_combo[] = {%s, COMBO_END};\n"
               (mugur--combo-name combo) (mugur--combo-keys combo))))
    (insert "\n")
    (buffer-string)))

(defun mugur--c-combos-key-combos (combos)
  (with-temp-buffer
    (insert "combo_t key_combos[COMBO_COUNT] = {\n")
    (cl-dolist (combo combos)
      (insert (format "\t[%s] = COMBO_ACTION(%s_combo),\n"
                      (upcase (mugur--combo-name combo))
                      (mugur--combo-name combo))))
    (insert "};\n\n")
    (buffer-string)))

(defun mugur--c-combos-process-combo-event (combos)
  (with-temp-buffer
    (insert "void process_combo_event(uint8_t combo_index, bool pressed) {\n")
    (insert "\tswitch(combo_index) {\n")
    (cl-dolist (combo combos)
      (insert (format "\tcase %s:\n" (upcase (mugur--combo-name combo))))
      (insert "\t\tif (pressed) {\n")
      (insert (format "\t\t\tSEND_STRING%s;\n" (mugur--combo-expansion combo)))
      (insert "\t\t}\n")
      (insert "\t\tbreak;\n"))
    (insert "\t}\n")
    (insert "}\n\n")
    (buffer-string)))

(defun mugur--c-layer-codes (layers)
  (with-temp-buffer
    (let ((layers (mapcar #'mugur--layer-name layers)))
      (insert "enum layer_codes {\n")
      (insert (format "\t%s = 0,\n" (car layers)))
      (setf layers (cdr layers))
      (cl-dolist (layer layers)
        (insert (format "\t%s,\n" layer)))
      (insert "};\n\n"))
    (buffer-string)))

(defun mugur--c-matrix-init-user ()
  "Runs just one time when the keyboard inits.
Nothing else to add here at the moment."
  "void matrix_init_user(void) {
#ifdef RGBLIGHT_COLOR_LAYER_0
  rgblight_setrgb(RGBLIGHT_COLOR_LAYER_0);
#endif
};

")

(defun mugur--c-layer-state-set-user (keymap)
  "Runs whenever there is a layer state change."
  (with-temp-buffer
    (insert "layer_state_t layer_state_set_user(layer_state_t state) {\n")
    (insert "\tergodox_board_led_off();\n")
    (insert "\tergodox_right_led_1_off();\n")
    (insert "\tergodox_right_led_2_off();\n")
    (insert "\tergodox_right_led_3_off();\n\n")
    (insert "\tuint8_t layer = biton32(state);\n")
    (insert "\tswitch(layer) {\n")
    (cl-dolist (layer (mugur--keymap-layers (car (mugur--keymaps-all))))
      (insert (format "\t\tcase %s:\n" (mugur--layer-name layer)))
      (awhen (mugur--layer-leds layer)
        (cl-dotimes (i (length it))
          (when (= (nth i it) 1)
            (insert (format "\t\t\tergodox_right_led_%s_on();\n" i)))))
      (insert "\t\t\tbreak;\n"))
    (insert "\t}\n")
    (insert "\treturn state;\n")
    (insert "};\n\n")
    (buffer-string)))

(defun mugur--keyboard-layout (keymap layer)
  "Return the correct keyboard layout based on the LAYER keyboard
and orientation."
  (let* ((keyboard (s-replace "_" "-" (mugur--keymap-keyboard keymap))))
    (unless (or mugur-layout-horizontal
                mugur-layout-vertical)
      (load-file (format "%slayouts/%s.el"
                         (file-name-directory (locate-library "mugur"))
                         keyboard)))
    (if (equal (mugur--layer-orientation layer)
               'vertical)
        mugur-layout-vertical
      mugur-layout-horizontal)))

(defun mugur--c-keymaps (keymap)
  (with-temp-buffer  
    (insert "const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {\n\n")
    (insert
     (cl-reduce
      (lambda (item1 item2)
        (concat item1 ", \n\n" item2))
      (mapcar (lambda (layer)
           (s-format (mugur--keyboard-layout keymap layer)
                     'elt
                     (cons (mugur--layer-name layer)
                           (mugur--layer-keys layer))))
         (mugur--keymap-layers keymap))))
    (insert "\n};\n\n\n")
    (buffer-string)))

(defun mugur--c-file-path (file keymap keyboard)
  (concat (file-name-as-directory mugur-qmk-path)
          (file-name-as-directory (format "keyboards/%s/keymaps" keyboard))
          (file-name-as-directory keymap)
          file))

(defun mugur--generate-keymap-file (keymap)
  (with-temp-file (mugur--c-file-path
                   "keymap.c"
                   (mugur--keymap-name keymap)
                   (mugur--keymap-keyboard keymap))
    (insert "#include QMK_KEYBOARD_H\n")
    (insert "#include \"version.h\"\n\n")
    (insert "#define ___ KC_TRNS\n")
    (insert "#define _X_ KC_NO\n\n")
    
    (insert (mugur--c-layer-codes (mugur--keymap-layers keymap)))
    (insert (mugur--c-custom-keycodes (mugur--keymap-macros keymap)))
    (when (mugur--keymap-tapdances keymap)
      (insert (mugur--c-tapdance-enum (mugur--keymap-tapdances keymap)))
      (insert (mugur--c-tapdance-actions (mugur--keymap-tapdances keymap))))
    (insert (mugur--c-combos-combo-events (mugur--keymap-combos keymap)))
    (insert (mugur--c-combos-progmem      (mugur--keymap-combos keymap)))
    (insert (mugur--c-combos-key-combos   (mugur--keymap-combos keymap)))
    (insert (mugur--c-combos-process-combo-event (mugur--keymap-combos keymap)))    
    (insert (mugur--c-keymaps             keymap))
    (insert (mugur--c-process-record-user (mugur--keymap-macros keymap)))
    (insert (mugur--c-matrix-init-user))
    (insert (mugur--c-layer-state-set-user keymap))))

(defun mugur--generate-config-file (keymap)
  (with-temp-file (mugur--c-file-path
                   "config.h"
                   (mugur--keymap-name keymap)
                   (mugur--keymap-keyboard keymap))
    (insert "#undef TAPPING_TERM
#define TAPPING_TERM 180
#define COMBO_TERM 100
#define FORCE_NKRO
#undef RGBLIGHT_ANIMATIONS
")
    (awhen (mugur--keymap-combos keymap)
      (insert (format "#define COMBO_COUNT %s\n"
                      (length it))))))

(defun mugur--generate-rules-file (keymap)
  (with-temp-file (mugur--c-file-path
                   "rules.mk"
                   (mugur--keymap-name keymap)
                   (mugur--keymap-keyboard keymap))
    (when (mugur--keymap-tapdances keymap)
      (insert "TAP_DANCE_ENABLE = yes\n"))
    (when (mugur--keymap-combos keymap)
      (insert "COMBO_ENABLE = yes\n"))
    (insert "FORCE_NKRO = yes\n")
    (insert "RGBLIGHT_ENABLE = no\n")))

;;;###autoload
(defun mugur-generate-keymap (keymap)
  (mugur--generate-keymap-file keymap)
  (mugur--generate-config-file keymap)
  (mugur--generate-rules-file  keymap))

;;;###autoload
(defun mugur-make-keymap (keymap)
  (interactive)
  (progn
    (let ((b (generate-new-buffer "make mykeyboard")))
      (with-current-buffer b
        (compilation-mode)        
        (start-process "make" b "make"
                       "-C"
                       mugur-qmk-path
                       (format "%s:%s"
                               (mugur--keymap-keyboard keymap)
                               (mugur--keymap-name keymap))))
      (switch-to-buffer "make mykeyboard"))))

(defun mugur--select-keymap ()
  (let* ((keymap
          (completing-read "Select-keymap: "
                           (mapcar (lambda (keymap)
                                (format "%s - %s"
                                        (mugur--keymap-name keymap)
                                        (mugur--keymap-keyboard keymap)))
                              (mugur--keymaps-all))))
         (selection (and keymap (s-split "-" keymap)))
         (name (and selection (s-trim (car selection))))
         (keyboard (and keymap (s-trim (cadr selection)))))
    (when (and name keyboard)
      (let* ((keyboard-maps
              (cl-find keyboard (mugur--keymaps-all)
                       :key #'mugur--keymap-keyboard
                       :test #'string-equal))
             (keyboard-keymap
              (when keyboard-maps
                (if (listp keyboard-maps)
                    (cl-find name
                             :key #'mugur--keymap-name
                             :test #'string-equal)
                  keyboard-maps))))
        keyboard-keymap))))

;;;###autoload
(defun mugur-flash-keymap ()
  (interactive)
  (let* ((keymap (mugur--select-keymap))
         (hex (format "%s/.build/%s_%s.hex"
                      mugur-qmk-path
                      (mugur--keymap-keyboard keymap)
                      (mugur--keymap-name keymap))))
    (progn (start-process "flashing"
                          "flash mykeyboard"
                          "wally-cli"
                          hex)
           (switch-to-buffer "flash mykeyboard"))))

;;;###autoload
(defun mugur-build ()
  (interactive)
  (let ((keymap (mugur--select-keymap)))    
    (mugur-generate-keymap keymap)
    (mugur-make-keymap   keymap)))

(provide 'mugur)

;;; mugur.el ends here
