;;; ergodox.el --- Configure and flash the Ergodox keyboard -*- lexical-binding: t -*-

(require 's)

(defconst supported-keycodes
  '(("Letters and Numbers"
     (a) (b) (c) (d) (e) (f) (g) (h) (i) (j) (k) (l) (m) (n)
     (o) (p) (q) (r) (s) (t) (u) (v) (w) (x) (y) (z)           
     (1) (2) (3) (4) (5) (6) (7) (8) (9) (0))
    
    ("Function Keys"
     (F1)  (F2)  (F3)  (F4)  (F5)  (F6)  (F7)  (F8)  (F9)  (F10)
     (F11) (F12) (F13) (F14) (F15) (F16) (F17) (F18) (F19) (F20)
     (F21) (F22) (F23) (F24))

    ("Punctuation"
     (ENT "enter") (ESC "escape") (bspace) (TAB "tab")
     (space "space") (- "minus") (= "equal")
     (lbracket "lbracket") ("[" "lbracket")
     (rbracket "rbracket") ("]" "rbracket") (\ "bslash")
     (nonus-hash "nonus_hash") (colon "scolon") (quote "quote") (grave "grave")
     (comma "comma") (dot "dot") (/ "slash"))
    
    ("Shifted Keys"
     (~ "tilde") (! "exclaim") (@ "at")
     (hash) ($ "dollar") (% "percent")
     (^ "circumflex") (& "ampersand") (* "asterix")
     (left-paren "left_paren") (right-paren "right_paren")
     (_ "underscore") (+ "plus")
     ({ "left_curly_brace") (} "right_curly_brace")
     (| "pipe") (: "colon") (double-quote "double_quote")
     (< "left_angle_bracket") (> "right_angle_bracket")
     (? "question"))
    
    ("Modifiers"
     (C "lctl") (M "lalt")
     (S "lshift") (G "lgui")
     (C-M "lca") (C-M-S "meh") (C-M-G "hypr"))

    ("Commands"
     (insert) (home) (prior "pgup") (delete) (end) (next "pgdown")
     (right) (left) (down) (up))

    ("Media Keys"
     (vol_up "audio_vol_up") (vol_down "audio_vol_down")
     (mute "audio_mute") (stop "media_stop"))

    ("Mouse Keys"
     (ms-up) (ms-down) (ms-left) (ms-right)
     (ms-btn1) (ms-btn2) (ms-btn3) (ms-btn4) (ms-btn5)
     (ms-wh-up) (ms-wh-down) (ms-wh-left) (ms-wh-right)
     (ms-accel1) (ms-accel2) (ms-accel3))
    
    ("Special Keys"
     (--- "_x_") (() "___"))))

(let ((keycodes (make-hash-table :test 'equal)))
  (defun set-keycodes ()
    "Add all keycodes in hashtable."
    (cl-dolist (categories supported-keycodes)
      (cl-dolist (entry (cdr categories))
          (puthash (car entry)
                   (if (= (length entry) 2)
                       (upcase (cadr entry))
                     (if (numberp (car entry))
                         (number-to-string (car entry))
                       (upcase (symbol-name (car entry)))))
                   keycodes))))

  (defun keycode-raw (key)
    (if (not (hash-table-empty-p keycodes))
        (awhen (gethash key keycodes)
          it)
      ;; First call, update the hash table.
      (set-keycodes)
      (keycode-raw key)))

  (defun key-in-category? (category key)
    (cl-find key
     (cdr (cl-find category
                   supported-keycodes
                   :test #'string-equal :key #'car))
     :key #'car))

  (defun modifier-key? (key)
    (key-in-category? "Modifiers" key))
  
  (defun special-key? (key)
    (key-in-category? "Special Keys" key))
  
  (defun keycode (key)
    (awhen (keycode-raw key)
      ;; Return the special keys as is.
      (if (special-key? key)          
          it
        (concat "KC_" it))))
  
  (defun keycode-x (key)
    "Keycodes for send_string macros."
    (awhen (keycode-raw key)
      (concat "X_" it)))

  (defun modifier-key-or-combo (combo)
    (cond ((modifier-key? combo) (modifier-key combo))
          ((s-contains? "-" (if (symbolp combo)
                                (symbol-name combo)
                              ""))
           (let* ((s (s-split "-" (symbol-name combo)))
                  (prefix (s-join "-" (butlast s))))
             (if (modifier-key? (intern prefix))
                 (modifier (intern prefix)
                           (intern (car (last s))))
               nil)))
          nil))
  
  (defun modifier-key (key)
    "Ctrl, Alt and the like."
    (when (modifier-key? key)
      (keycode-raw key)))

  (defun modifier-key-mod (key)
    (format "MOD_%s" (modifier-key key)))

  (defun modifier-key-ss (key)
    (format "SS_%s" (modifier-key key))))


(defun layer-toggle (layer key)
  "LAYER when held, KEY when tapped."
  (s-format "LT($0, $1)" 'elt
            (list layer (keycode key))))

(defun modtap (mod key)
  "MOD when held, KEY when tapped."
  (s-format "MT($0, $1)" 'elt
            (list (modifier-key-mod mod)
                  (keycode key))))

(defun modifier (mod key)
  "Hold MOD and press KEY."
  (s-format "$0($1)" 'elt
            (list (modifier-key mod)
                  (keycode key))))

(defun layer-switch (action layer)
  "Switch to the given LAYER."
  (format "%s(%s)"
          (upcase (symbol-name action))
          (upcase (symbol-name layer))))

(defun layer-switch-lm-or-lt (action layer mod-or-key)
  "Switch to the given LAYER (with tap or mod)"
  (let ((action-str (symbol-name action)))
    (format "%s(%s, %s)"
            (upcase (symbol-name action))
            (upcase (symbol-name layer))
            (if (string-equal action-str "lm")
                (modifier-key mod-or-key)
              (keycode mod-or-key)))))

(defun one-shot-mod (mod)
  "Hold down MOD for one key press only."
  (format "OSM(%s)" (modifier-key-mod mod)))

(defun one-shot-layer (layer)
  "Switch to LAYER for one key press only."
  (format "OSL(%s)" (upcase (symbol-name layer))))


(cl-defstruct keycode
  key macro)

(let (keycodes)
  (defun define-macro (key macro)
    (cl-pushnew
     (make-keycode :key key
                   :macro macro)
     keycodes
     :test #'equal)))

(defun transform-key (key)
  (pcase key
    (`() (keycode '()))
    ((and `(,mod-or-combo)
          (guard (modifier-key-or-combo mod-or-combo)))
     (modifier-key-or-combo mod-or-combo))
    (`(,s) (keycode s))
    ((and `(,modifier ,key)
          (guard (modifier-key? modifier)))
     (modtap modifier key))
    (`(osm ,mod) (one-shot-mod mod))
    (`(osl ,layer) (one-shot-layer layer))
    ((and `(,action ,layer)
          (guard (member action '(df mo osl tg to tt))))
     (layer-switch action layer))
    ((and `(,action ,layer ,mod-or-key)
          (guard (member action '(lm lt))))
     (layer-switch-lm-or-lt action layer mod-or-key))))

(ert-deftest test-transform-key ()
  (cl-dolist (test
       '((()    "___")
         ((C)   "LCTL")
         ((M-a) "LALT(KC_A)")
         ((M a) "MT(MOD_LALT, KC_A)")))
    (should (equal (transform-key (car test))
                   (cadr test)))))

(defun transform-keys (keys)
  (mapcar #'transform-key keys))

(cl-defstruct layer
  name pos keys)

(let (layers)
  (defun define-layer (name pos keys)
    (cl-pushnew
     (make-layer :name (upcase name)
                 :pos pos
                 :keys (transform-keys keys))
     layers
     :test (lambda (old new)
             (string-equal (layer-name old)
                           (layer-name new)))))

  (defun all-layers ()
    (cl-sort (copy-sequence layers)
             #'< :key #'layer-pos)))

(defun generate-layer-codes-enum ()
  (let ((layers (mapcar #'layer-name (all-layers))))
    (insert "enum layer_codes {\n")
    (insert (format "\t%s = 0,\n" (car layers)))
    (setf layers (cdr layers))
    (cl-dolist (layer (butlast layers))
      (insert (format "\t%s,\n" layer)))
    (insert (format "\t%s\n" (car (last layers))))
    (insert "};\n\n")))

(defconst ergodox-layout-template
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

(defun generate-keymaps-matrix ()
  (let ((layers (all-layers)))
    (insert "const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {\n\n")
    (insert (cl-reduce (lambda (item1 item2)
                         (concat item1 ", \n\n" item2))
                       (mapcar (lambda (layer)
                            (s-format ergodox-layout-template
                                      'elt
                                      (cons (layer-name layer)
                                            (layer-keys layer))))
                          layers))))
  (insert "\n};"))

(define-layer "base" 0
 '((---)        (---) (---) (---) (---)     (---) (---)
   (---)        (---)  (w)   (e)   (r)  (t) (---)
   (---)         (a)  (G t) (M d) (C f) (g)
   (osm S)       (z)   (x)   (c)   (v)  (b) (---)
   (tg emacs_l) (---) (---) (---) (---)
   
                                            (---) (---)
                                                  (M-x)
                (lt emacs_r DEL) (lt xwindow SPC) (TAB)
   ;; ------------------------------------------------------------------   
   (---) (---)   (---)         (---)       (---) (---) (---)
   (---)  (y) (lt num_up u) (lt numeric i)  (o)  (---) (---)
          (h)    (C j)      (lt symbols k) (M l)  (p)  (---)
   (---)  (n)     (m)         (comma)      (dot)  (q)  (osm S)
                 (---)         (---)       (---) (---) (---)

   (---) (---)
   (C-z)
   (lt mdia ESC) (lt emacs_l ENT) (---)))

(define-layer "xwindow" 1
  '(( ) ( ) ( ) ( ) ( ) ( ) ( )
    ( ) ( ) ( ) ( ) ( ) ( ) ( )
    ( ) ( ) ( ) ( ) ( ) ( )
    ( ) ( ) ( ) ( ) ( ) ( ) ( )
    ( ) ( ) ( ) ( ) ( )
                        ( ) ( )
                            ( )
                    ( ) ( ) ( )
 ;; ---------------------------------
    ( ) ( ) ( )   ( )  ( )   ( )  ( )
    ( ) ( ) ( )  (G-b) ( )   ( )  ( )
        ( ) (F4) (F3)  (G-t) (F5) ( )
    ( ) ( ) ( )  ( )   ( )   ( )  ( )
            ( )  ( )   ( )   ( )  ( )
    ( ) ( )
    ( )
    ( ) ( ) ( )
    ))


(define-layer "numeric" 4
  '(( ) ( ) ( ) ( ) ( ) ( ) ( )
    ( ) ( ) (1) (2) (3) ( ) ( )
    ( ) (0) (4) (5) (6) ( )
    ( ) (0) (7) (8) (9) ( ) ( )
    ( ) ( ) ( ) ( ) ( )
                        ( ) ( )
                            ( )
                    ( ) ( ) ( )
 ;; ---------------------------
    ( ) ( ) ( ) ( ) ( ) ( ) ( )
    ( ) ( ) ( ) ( ) ( ) ( ) ( )
        ( ) ( ) ( ) ( ) ( ) ( )
    ( ) ( ) ( ) ( ) ( ) ( ) ( )
            ( ) ( ) ( ) ( ) ( )
    ( ) ( )
    ( )
    ( ) ( ) ( )
    ))


(define-layer "symbols" 6
  '(( ) ( ) ("[") ("]") ({) (}) ( )
    ( ) ( ) ( ) ( ) ( ) ( ) ( )
    ( ) ( ) ( ) ( ) ( ) ( )
    ( ) ( ) ( ) ( ) ( ) ( ) ( )
    ( ) ( ) ( ) ( ) ( )
                        ( ) ( )
                            ( )
                    ( ) ( ) ( )
 ;; ---------------------------
    ( ) ( ) ( ) ( ) ( ) ( ) ( )
    ( ) ( ) ( ) ( ) ( ) ( ) ( )
        ( ) ( ) ( ) ( ) ( ) ( )
    ( ) ( ) ( ) ( ) ( ) ( ) ( )
            ( ) ( ) ( ) ( ) ( )
    ( ) ( )
    ( )
    ( ) ( ) ( )
    ))

(defconst keymap-filename "./elisp-keymap.c")

(with-temp-file keymap-filename
  (generate-layer-codes-enum)
  (generate-keymaps-matrix))


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


