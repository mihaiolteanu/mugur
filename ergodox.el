;;; ergodox.el --- Generate keymaps and config files for qmk keyboards -*- lexical-binding: t -*-

(require 's)

(defgroup mugur ()
  "qmk keyboard configurator"
  :group 'tools
  :prefix "mugur-")

(defcustom qmk-path nil
  "Path to where you git cloned the qmk firmware source code 
(https://github.com/qmk/qmk_firmware)"
  :type '(string :tag "path")
  :group 'mugur)

(setf qmk-path "~/projects/qmk_firmware")

(defconst supported-keycodes
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

(let ((keycodes (make-hash-table :test 'equal)))
  (defun keycode-string (keycode)
    (if (= (length keycode) 2)
        (upcase (cadr keycode))
      (if (numberp (car keycode))
          (number-to-string (car keycode))
        (symbol-name (car keycode)))))
  
  (defun set-keycodes ()
    "Add all the keycodes into a hashtable."
    (dolist (categories supported-keycodes)
      (dolist (entry (cdr categories))
          (puthash (car entry)
                   (upcase (keycode-string entry))
                   keycodes))))

  (defun keycode-raw (key)
    (if (not (hash-table-empty-p keycodes))
        (awhen (gethash key keycodes)
          it)
      ;; First call, update the hash table.
      (set-keycodes)
      (keycode-raw key)))

  (defun key-in-category-p (category key)
    (cl-find key
     (cdr (cl-find category
                   supported-keycodes
                   :test #'string-equal :key #'car))
     :key #'car))

  (defun modifier-key-p (key)
    (key-in-category-p "Modifiers" key))
  
  (defun special-key-p (key)
    (key-in-category-p "Special Keys" key))
  
  (cl-defun keycode (key &key (ss nil) (mod nil))
    (awhen (keycode-raw key)
      (if (special-key-p key)
          it
        (if (modifier-key-p key)
            (if ss
                (concat "SS_" it)
              (if mod
                  (concat "MOD_" it)
                it))
          (if ss
              (format "SS_TAP(X_%s)" it)              
            (concat "KC_" it))))))

  (cl-defun key-or-sequence (key &key (ss nil))
    "Generate simple keys or key sequences, like M-x or C-M-a.
If SS is t, generate the key sequence as needed by SEND_STRING
macros."
    (cond ((awhen (keycode key :ss ss) it))
          ((s-contains? "-" (if (symbolp key)
                                (symbol-name key)
                              ""))
           (let* ((s (s-split "-" (symbol-name key)))
                  (prefix (s-join "-" (butlast s))))
             (if (modifier-key-p (intern prefix))
                 (modifier+key (intern prefix)
                               (intern (car (last s)))
                               :ss ss)
               nil)))
          ((and (stringp key) ss) (format "\"%s\"" key))
          (t nil)))

  (defun gendoc-keycodes ()
    (interactive)
    (let ((b (get-buffer-create "keycodes.org")))
      (with-current-buffer b
          (org-mode)
        (erase-buffer)
        (dolist (category supported-keycodes)
          (insert (format "* %s\n\n" (car category)))
          (let ((max (cl-loop for entry in (cdr category)
                              maximize (length (keycode-string entry)))))
            (dolist (entry (cdr category))            
              (insert (format (concat "\t%-" (number-to-string max)
                                      "S --> %s\n")
                              (car entry) (keycode-string entry)))))
          (insert "\n")))
      (switch-to-buffer b)))
  
  (ert-deftest keycodes-should-not-error ()
    (dolist (category supported-keycodes)
      (dolist (entry (cdr category))
        (should (keycode (car entry)))))))

(defun modtap (mod key)
  "MOD when held, KEY when tapped."
  (s-format "MT($0, $1)" 'elt
            (list (keycode mod :mod t)
                  (keycode key))))

(cl-defun modifier+key (mod key &key (ss nil))
  "Hold MOD and press KEY."
  (s-format "$0($1)" 'elt
            (list (keycode mod :ss ss)
                  (if ss
                      (format "\"%s\"" (symbol-name key))
                    (keycode key)))))

(defun one-shot-mod (mod)
  "Hold down MOD for one key press only."
  (format "OSM(%s)" (keycode mod :mod t)))

(defun one-shot-layer (layer)
  "Switch to LAYER for one key press only."
  (format "OSL(%s)" (upcase (symbol-name layer))))

;;;; Macros
(cl-defstruct ss-macro-entry
  name expansion)

(defun ss-macro-transform-keys (keys)
  (mapcar (lambda (key)
       (key-or-sequence key :ss t))
     keys))

(defun ss-macro-define (entry)
  (cl-reduce
   (lambda (item1 item2)
     (concat item1 " " item2))
   (ss-macro-transform-keys entry)))

(defun ss-macro (entry)
  (let ((expansion (ss-macro-define entry)))
    (make-ss-macro-entry
     :name (format "SS_MACRO_%s" (upcase (md5 expansion)))
     :expansion (ss-macro-define entry))))

(defun extract-macros (keys)
  (remove
   nil
   (mapcar (lambda (key)
        (let ((tr (transform-key key)))
          (if (s-contains-p "SS_MACRO_" tr)
              (ss-macro key)
            nil)))
      keys)))

(ert-deftest test-ss-macro ()
  (cl-dolist (test
       '((("you do" C-x) "\"you do\" SS_LCTL(\"x\")")
         ((M-x a)        "SS_LALT(\"x\") SS_TAP(X_A)")
         ((M-x a b)      "SS_LALT(\"x\") SS_TAP(X_A) SS_TAP(X_B)")
         ((M-x "this" a) "SS_LALT(\"x\") \"this\" SS_TAP(X_A)")
         ))
    (should (equal (ss-macro-define (car test))
                   (cadr test)))))


;;;; Combos
(cl-defstruct combo
  name keys expansion)

(defun combo-define (combo)
  (let* ((keycodes (mapcar #'keycode (butlast combo)))
         (last (last combo))
         (ss (ss-macro-transform-keys
              (if (listp (car last))
                  (car last)
                last))))
    (list keycodes ss)))

(defun combo (combo name)
  (let ((c (combo-define combo)))
    (make-combo :name name
                :keys (cl-reduce (lambda (item1 item2)
                                   (concat item1 ", " item2))
                                 (car (butlast c)))
                :expansion (car (last c)))))

(ert-deftest test-combo-define ()
  (cl-dolist (test
       '(((a x "whatever") (("KC_A" "KC_X") ("\"whatever\"")))
         ((a x ("whatever")) (("KC_A" "KC_X") ("\"whatever\"")))
         ((a x (x "whatever")) (("KC_A" "KC_X") ("SS_TAP(X_X)" "\"whatever\"")))
         ((a x C-x) (("KC_A" "KC_X") ("SS_LCTL(\"x\")")))
         ((a x (C-x "whatever")) (("KC_A" "KC_X") ("SS_LCTL(\"x\")" "\"whatever\"")))))
    (should (equal (combo-define (car test))
                   (cadr test)))))

;;;; Layer Switching
(defun layer-switching-codes ()
  '(((df layer)  "Set the base (default) layer.")
    ((mo layer)  "Momentarily turn on layer when pressed (requires KC_TRNS on destination layer).")
    ((osl layer) "Momentarily activates layer until a key is pressed. See One Shot Keys for details.")
    ((tg layer)  "Toggle layer on or off.")
    ((to layer)  "Turns on layer and turns off all other layers, except the default layer.")
    ((tt layer)  "Normally acts like MO unless it's tapped multiple times, which toggles layer on.")
    ((lm layer mod) "Momentarily turn on layer (like MO) with mod active as well.")
    ((lt layer kc) "Turn on layer when held, kc when tapped")))

(defun layer-switch-p (key)
  (cl-member key (layer-switching-codes)
             :key #'caar))

(defun layer-switch (action layer &optional key-or-mod)
  "Generate code to switch to the given LAYER."
  (if key-or-mod
      (format "%s(%s, %s)"
                (upcase (symbol-name action))
                (upcase (symbol-name layer))
                (keycode key-or-mod))
    (format "%s(%s)"
            (upcase (symbol-name action))
            (upcase (symbol-name layer)))))

(defun gendoc-layer-switching ()
  (interactive)
  (with-current-buffer (get-buffer-create "layer-switching-codes")
    (org-mode)
    (local-set-key (kbd "q") 'kill-current-buffer)
    (insert "* Layer Switching Codes\n\n")
    (mapc (lambda (code)
         (insert (format "%-15s - %s\n" (car code) (cadr code))))
       (layer-switching-codes))
    (switch-to-buffer (get-buffer-create "layer-switching-codes"))))


;;;; Keymaps, Layers and Transformations.
(defun transform-key (key)
  "Transform a keymap KEY to the qmk equivalent."
  (pcase key
    (`() (keycode '()))
    ((and `(,key)
          (guard (key-or-sequence key)))
     (key-or-sequence key))
    ((and `(,modifier ,key)
          (guard (modifier-key-p modifier)))
     (modtap modifier key))
    (`(osm ,mod) (one-shot-mod mod))
    (`(osl ,layer) (one-shot-layer layer))
    ((and `(,action ,layer)
          (guard (layer-switch-p action)))
     (layer-switch action layer))
    ((and `(,action ,layer ,key-or-mod)
          (guard (layer-switch-p action)))
     (layer-switch action layer key-or-mod))
    (_ (ss-macro-entry-name (ss-macro key)))))

(defun transform-keys (keys)
  (mapcar #'transform-key keys))

(ert-deftest test-transform-key ()
  (cl-dolist (test
       '((()      "___")
         ((c)     "KC_C")
         ((C)     "LCTL")
         ((M-a)   "LALT(KC_A)")
         ((C-M-a) "LCA(KC_A)")         
         ((M a)   "MT(MOD_LALT, KC_A)"
         (("what you do") "\"what you do\""))))
    (should (equal (transform-key (car test))
                   (cadr test)))))

(cl-defstruct layer
  name
  index
  keys
  leds
  orientation)

(cl-defstruct keymap
  name
  keyboard
  layers
  combos
  macros)

(cl-defun new-layer (name index keys &key (leds nil) (orientation 'horizontal))
  (make-layer :name name
              :index index
              :keys keys
              :leds leds
              :orientation orientation))

(cl-defun new-keymap (&key name keyboard layers (combos nil) (macros nil))
  (make-keymap :name name
               :keyboard keyboard
               :layers layers
               :combos combos
               :macros macros))

(let (keymaps)
  (defun leds (layer)
    (if (= (length (cadr layer)) 3)
        (cadr layer)
      nil))

  (defun keys (layer)
    (if (= (length (cadr layer)) 3)
        (caddr layer)
      (cadr layer)))

  (cadr (cl-find 'em-split '((mybspace (lt xwindow bspace))
                          (em-split (C-x 3)))
              :key #'car))

  (defun replace-custom-keys (custom-keys keys)
    (let ((names (mapcar #'car custom-keys)))
      (print names)
      (mapcar (lambda (key)
           (if (member (car key) names)
               (cadr (cl-find (car key) custom-keys :key #'car))
             key))
         keys)))
  
  (cl-defun mugur-keymap (name keyboard &key
                               (layers nil)
                               (combos nil)                               
                               (custom-keys nil))

    (let* ((macros
            (mapcar #'car (remove nil
                    (mapcar (lambda (layer)
                         (extract-macros
                          (replace-custom-keys
                           custom-keys (keys layer))))
                       layers)))))      
      (cl-pushnew
       (new-keymap
        :name name
        :keyboard keyboard
        
        :layers
        (let ((index 0))
          (mapcar (lambda (layer)
               (let ((name (car layer))
                     (leds (leds layer))
                     (keys (keys layer)))
                 (setf index (+ 1 index))
                 (new-layer (upcase name) index
                            (transform-keys
                             (replace-custom-keys custom-keys keys))
                            :leds leds)))
             layers))
        
        :combos
        (let ((index 0))
          (mapcar (lambda (combo)
               (setf index (+ 1 index))
               (combo combo (format "COMBO_%s" index)))
             combos))

        :macros macros        
        )
       keymaps)))

  (defun keymaps-all ()
    keymaps))

;;;; C Code Generators
(defun c-custom-keycodes (ss-macros)
  (with-temp-buffer
    (insert "enum custom_keycodes {\n\tEPRM = SAFE_RANGE,\n")
    (cl-dolist (keycode ss-macros)
      (insert (format "\t%s,\n"
                      (upcase (ss-macro-entry-name keycode)))))
    (insert "};\n\n")
    (buffer-string)))

(defun c-process-record-user (ss-macros)
  (with-temp-buffer
    (insert "bool process_record_user(uint16_t keycode, keyrecord_t *record) {\n")
    (insert "\tif (record->event.pressed) {\n")
    (insert "\t\tswitch (keycode) {\n")
    (insert "\t\tcase EPRM:\n")
    (insert "\t\t\teeconfig_init();\n")
    (insert "\t\t\treturn false;\n")
    (cl-dolist (macro ss-macros)
      (insert (format "\t\tcase %s:\n" (ss-macro-entry-name macro)))
      (insert (format "\t\t\tSEND_STRING(%s);\n" (ss-macro-entry-expansion macro)))
      (insert "\t\t\treturn false;\n"))
    (insert "\t\t}\n\t}\n\treturn true;\n}\n\n")
    (buffer-string)))

(defun c-combos-combo-events (combos)
  (with-temp-buffer
    (insert "enum combo_events {\n")
    (cl-dolist (combo combos)
      (insert (format "\t%s,\n" (upcase (combo-name combo)))))
    (insert "};\n\n")
    (buffer-string)))

(defun c-combos-progmem (combos)
  (with-temp-buffer
    (cl-dolist (combo combos)
      (insert
       (format "const uint16_t PROGMEM %s_combo[] = {%s, COMBO_END};\n"
               (combo-name combo) (combo-keys combo))))
    (insert "\n")
    (buffer-string)))

(defun c-combos-key-combos (combos)
  (with-temp-buffer
    (insert "combo_t key_combos[COMBO_COUNT] = {\n")
    (cl-dolist (combo combos)
      (insert (format "\t[%s] = COMBO_ACTION(%s_combo),\n"
                      (upcase (combo-name combo))
                      (combo-name combo))))
    (insert "};\n\n")
    (buffer-string)))

(defun c-combos-process-combo-event (combos)
  (with-temp-buffer
    (insert "void process_combo_event(uint8_t combo_index, bool pressed) {\n")
    (insert "\tswitch(combo_index) {\n")
    (cl-dolist (combo combos)
      (insert (format "\tcase %s:\n" (upcase (combo-name combo))))
      (insert "\t\tif (pressed) {\n")
      (insert (format "\t\t\tSEND_STRING%s;\n" (combo-expansion combo)))
      (insert "\t\t}\n")
      (insert "\t\tbreak;\n"))
    (insert "\t}\n")
    (insert "}\n\n")
    (buffer-string)))

(defun c-layer-codes (layers)
  (with-temp-buffer
    (let ((layers (mapcar #'layer-name layers)))
      (insert "enum layer_codes {\n")
      (insert (format "\t%s = 0,\n" (car layers)))
      (setf layers (cdr layers))
      (cl-dolist (layer layers)
        (insert (format "\t%s,\n" layer)))
      (insert "};\n\n"))
    (buffer-string)))

(defun c-keymaps (layers)
  (with-temp-buffer  
    (insert "const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {\n\n")
    (insert
     (cl-reduce
      (lambda (item1 item2)
        (concat item1 ", \n\n" item2))
      (mapcar (lambda (layer)
           (s-format (if (equal (layer-orientation layer)
                                'vertical)
                         ergodox-layout
                       ergodox-layout-horizontal)
                     'elt
                     (cons (layer-name layer)
                           (layer-keys layer))))
         layers)))
    (insert "\n};\n\n\n")
    (buffer-string)))


;;;; Layouts
(defconst ergodox-layout
  "[$0] = LAYOUT_ergodox(
    $1,  $2,  $3,  $4,  $5,  $6,  $7,
    $8,  $9,  $10, $11, $12, $13, $14,
    $15, $16, $17, $18, $19, $20,
    $21, $22, $23, $24, $25, $26, $27,
    $28, $29, $30, $31, $32,
                             $33, $34,
                                  $35,
                        $36, $37, $38,
    // ----------------------------------------------
    $39, $40, $41, $42, $43, $44, $45,
    $46, $47, $48, $49, $50, $51, $52,
    $53, $54, $55, $56, $57, $58,
    $59, $60, $61, $62, $63, $64, $65,
    $66, $67, $68, $69, $70,
    $71, $72,
    $73,
    $74, $75, $76)")

(defconst ergodox-layout-horizontal
  "[$0] = LAYOUT_ergodox(
    $1,  $2,  $3,  $4,  $5,  $6,  $7,    $8,  $9,  $10, $11, $12, $13, $14,
    $15, $16, $17, $18, $19, $20, $21,   $22, $23, $24, $25, $26, $27, $28,
    $29, $30, $31, $32, $33, $34,             $35, $36, $37, $38, $39, $40,
    $41, $42, $43, $44, $45, $46, $47,   $48, $49, $50, $51, $52, $53, $54,
    $55, $56, $57, $58, $59,                       $60, $61, $62, $63, $64, 
                             $65, $66,   $67, $68,
                                  $69,   $70, 
                        $71, $72, $73,   $74, $75, $76)")


(mugur-keymap "elisp" "ergodox_ez"
  :combos '((left right escape)
            (x y (C-x "now")))
  
  :custom-keys '((mybspace (lt xwindow bspace))
                 (em-split (C-x 3)))
  
  :layers
  '(("base"
    ((---)        (---) (---) (---) (---) (---) (---)    (---) (---)   (---)          (---)       (---) (---) (---)
     (---)        (---)  (w)   (e)   (r)  (t)   (---)    (---)  (y) (lt numeric u) (lt numeric i)  (o)  (---) (---)
     (---)         (a)  (G t) (M d) (C f) (g)                   (h)    (C j)       (lt symbols k) (M l)  (p)  (---)
     (osm S)       (z)   (x)   (c)   (v)  (b)   (---)    (---)  (n)     (m)          (comma)      (dot)  (q)  (osm S)
     (tg xwindow) (---) (---) (---) (---)                              (---)          (---)       (---) (---) (---)
     
                                          (---) (---)    (---) (---)
                                                (M-x)    (C-z)
                  (mybspace) (lt xwindow space) (tab)    (lt xwindow escape) (lt xwindow enter) (---)))

  ("xwindow" (0 1 0)
    ((em-split) ( ) ( ) ( ) ( ) ( ) ( )     (a b c) ( ) ( )   ( )  ( )   ( )  ( )
     ( ) ( ) ( ) ( ) ( ) ( ) ( )     ( ) ( ) ( )  (G-b) ( )   ( )  ( )
     ( ) ( ) ( ) ( ) ( ) ( )             ( ) (F4) (F3) (G-t)  (F5) ( )
     ( ) ( ) ( ) ( ) ( ) ( ) ( )     ( ) ( ) ( )  ( )   ( )   ( )  ( )
     ( ) ( ) ( ) ( ) ( )                     ( )  ( )   ( )   ( )  ( )
                         ( ) ( )     ( ) ( )
                             ( )     ( )
                     ( ) ( ) ( )     ( ) ( ) ( )))
  
  ("numeric"
    (( ) ( ) ( ) ( ) ( ) ( ) ( )     ( ) ( ) ( ) ( ) ( ) ( ) ( )
     ( ) ( ) (1) (2) (3) ( ) ( )     ( ) ( ) ( ) ( ) ( ) ( ) ( )
     ( ) (0) (4) (5) (6) ( )             ( ) ( ) ( ) ( ) ( ) ( )
     ( ) (0) (7) (8) (9) ( ) ( )     ( ) ( ) ( ) ( ) ( ) ( ) ( )
     ( ) ( ) ( ) ( ) ( )                     ( ) ( ) ( ) ( ) ( )
                         ( ) ( )     ( ) ( )
                             ( )     ( )
                     ( ) ( ) ( )     ( ) ( ) ( )))

  ("symbols"
    (( ) ( ) ("[") ("]") ({) (}) ( )     ( ) ( ) ( ) ( ) ( ) ( ) (C-x ENT)
     ( ) ( ) ( )   ( )   ( ) ( ) ( )     ( ) ( ) ( ) ( ) ( ) ( ) ( )
     ( ) ( ) ( )   ( )   ( ) ( )             ( ) ( ) ( ) ( ) ( ) ( )
     ( ) ( ) ( )   ( )   ( ) ( ) ( )     ( ) ( ) ( ) ( ) ( ) ( ) ( )
     ( ) ( ) ( )   ( )   ( )                     ( ) ( ) ( ) ( ) ( )
                             ( ) ( )     ( ) ( )
                                 ( )     ( )
                         ( ) ( ) ( )     ( ) ( ) ( )))))


(defun c-file-path (file keymap keyboard)
  (concat (file-name-as-directory qmk-path)
          (file-name-as-directory (format "keyboards/%s/keymaps" keyboard))
          (file-name-as-directory keymap)
          file))

(defun generate-keymap-file (keymap)
  (with-temp-file (c-file-path "keymap.c"
                               (keymap-name keymap)
                               (keymap-keyboard keymap))
    (insert "#include QMK_KEYBOARD_H\n")
    (insert "#include \"version.h\"\n\n")
    (insert "#define ___ KC_TRNS\n")
    (insert "#define _X_ KC_NO\n\n")
    
    (insert (c-layer-codes         (keymap-layers keymap)))
    (insert (c-custom-keycodes     (keymap-macros keymap)))
    (insert (c-combos-combo-events (keymap-combos keymap)))
    (insert (c-combos-progmem      (keymap-combos keymap)))
    (insert (c-combos-key-combos   (keymap-combos keymap)))
    (insert (c-combos-process-combo-event (keymap-combos keymap)))    
    (insert (c-keymaps             (keymap-layers keymap)))
    (insert (c-process-record-user (keymap-macros keymap)))
    ))

(defun generate-config-file (keymap)
  (with-temp-file (c-file-path "config.h"
                               (keymap-name keymap)
                               (keymap-keyboard keymap))
    (insert "#undef TAPPING_TERM
#define TAPPING_TERM 180
#define COMBO_TERM 100
#define FORCE_NKRO
#undef RGBLIGHT_ANIMATIONS
")
    (awhen (keymap-combos keymap)
      (insert (format "#define COMBO_COUNT %s\n"
                      (length it))))))

(defun generate-rules-file (keymap)
  (with-temp-file (c-file-path "rules.mk"
                               (keymap-name keymap)
                               (keymap-keyboard keymap))
    (insert "TAP_DANCE_ENABLE = no
FORCE_NKRO = yes
RGBLIGHT_ENABLE = no
")
    (awhen (keymap-combos keymap)
      (insert (format "COMBO_ENABLE = %s\n"
                      (if it "yes" "no"))))))

(defun generate-all (keymap)
  (generate-keymap-file keymap)
  (generate-config-file keymap)
  (generate-rules-file keymap))


(generate-keymap-file (car (keymaps-all)))
(generate-all (car (keymaps-all)))

;; (define-layer "template"
;;   '(( ) ( ) ( ) ( ) ( ) ( ) ( )
;;     ( ) ( ) ( ) ( ) ( ) ( ) ( )
;;     ( ) ( ) ( ) ( ) ( ) ( )
;;     ( ) ( ) ( ) ( ) ( ) ( ) ( )
;;     ( ) ( ) ( ) ( ) ( )
;;                         ( ) ( )
;;                             ( )
;;                     ( ) ( ) ( )
;;  ;; ---------------------------
;;     ( ) ( ) ( ) ( ) ( ) ( ) ( )
;;     ( ) ( ) ( ) ( ) ( ) ( ) ( )
;;         ( ) ( ) ( ) ( ) ( ) ( )
;;     ( ) ( ) ( ) ( ) ( ) ( ) ( )
;;             ( ) ( ) ( ) ( ) ( )
;;     ( ) ( )
;;     ( )
;;     ( ) ( ) ( )
;;     ))



