(ert-deftest mugur-keycodes-should-not-error ()
  (dolist (category mugur--supported-keycodes)
    (dolist (entry (cdr category))
      (should (mugur--keycode (car entry))))))

(ert-deftest mugur-test-tapdance-p ()
  "Correctly interpret tapdances."
  (cl-dolist (test
       '(((x y) "TD_X_Y")
         ((x emacs) "TD_X_EMACS")
         ((x "emacs") nil)
         ((C x) nil)
         ((C M) nil)))
    (should (equal (aif (mugur--tapdance (car test))
                       (mugur--tapdance-name it)
                     nil)
                   (cadr test)))))

(ert-deftest mugur-test-macro--define ()
  "Test the send_string macro defines."
  (cl-dolist (test
       '((("you do" C-x) "\"you do\" SS_LCTL(SS_TAP(X_X))")
         ((M-x a)        "SS_LALT(SS_TAP(X_X)) SS_TAP(X_A)")
         ((M-x a b)      "SS_LALT(SS_TAP(X_X)) SS_TAP(X_A) SS_TAP(X_B)")
         ((M-x "this" a) "SS_LALT(SS_TAP(X_X)) \"this\" SS_TAP(X_A)")
         ((C-x 3) "SS_LCTL(SS_TAP(X_X)) SS_TAP(X_3)")
         ((C-x {) "SS_LCTL(SS_TAP(X_X)) \"{\"")))
    (should (equal (mugur--macro-define (car test))
                   (cadr test)))))

(ert-deftest mugur-test-combo-define ()
  (cl-dolist (test
       '(((a x "whatever") (("KC_A" "KC_X") ("\"whatever\"")))
         ((a x ("whatever")) (("KC_A" "KC_X") ("\"whatever\"")))
         ((a x (x "whatever")) (("KC_A" "KC_X") ("SS_TAP(X_X)" "\"whatever\"")))
         ((a x C-x) (("KC_A" "KC_X") ("SS_LCTL(SS_TAP(X_X))")))
         ((a x (C-x "whatever")) (("KC_A" "KC_X") ("SS_LCTL(SS_TAP(X_X))" "\"whatever\"")))))
    (should (equal (mugur--combo-define (car test))
                   (cadr test)))))

(ert-deftest mugur-test-transform-key ()
  (cl-dolist (test
       '((()      "___")
         ((c)     "KC_C")
         ((C)     "KC_LCTL")
         ((M-a)   "LALT(KC_A)")
         ((C-M-a) "LCA(KC_A)")
         ((x y)   "TD(TD_X_Y)")
         (("what you do") "SS_MACRO_F20F55CF099E6BE80B9D823C1C609006")
         ((M a)   "MT(MOD_LALT, KC_A)")))
    (should (equal (mugur--transform-key (car test))
                   (cadr test)))))

(ert-deftest mugur-test-leds-keys-orientation-extraction-1 ()
  (let ((layer '("xwindow" (0 1 1) horizontal
                 ((a) (b) (c) (d) (e)))))
    (should (equal (mugur--leds layer)
                   '(0 1 1)))
    (should (equal (mugur--keys layer)
                   '((a) (b) (c) (d) (e))))
    (should (equal (mugur--orientation layer)
                   'horizontal))))

(ert-deftest mugur-test-leds-keys-orientation-extraction-2 ()
  (let ((layer '("xwindow" (0 1 1)
                 ((a) (b) (c) (d) (e)))))
    (should (equal (mugur--leds layer)
                   '(0 1 1)))
    (should (equal (mugur--keys layer)
                   '((a) (b) (c) (d) (e))))
    (should (equal (mugur--orientation layer)
                   nil))))

(ert-deftest mugur-test-leds-keys-orientation-extraction-3 ()
  (let ((layer '("xwindow" horizontal
                 ((a) (b) (c) (d) (e)))))
    (should (equal (mugur--leds layer)
                   nil))
    (should (equal (mugur--keys layer)
                   '((a) (b) (c) (d) (e))))
    (should (equal (mugur--orientation layer)
                   'horizontal))))

(ert-deftest mugur-test-leds-keys-orientation-extraction-4 ()
  (let ((layer '("xwindow"
                 ((a) (b) (c) (d) (e)))))
    (should (equal (mugur--leds layer)
                   nil))
    (should (equal (mugur--keys layer)
                   '((a) (b) (c) (d) (e))))
    (should (equal (mugur--orientation layer)
                   nil))))

;; Test the generated C code.
(ert-deftest mugur-test-macro-c ()
  (let* ((macros (mapcar #'mugur--macro '((a b c) ("whatever"))))
         (custom-keycodes (mugur--c-custom-keycodes macros))
         (process-record-user (mugur--c-process-record-user macros)))
    (should (string-equal custom-keycodes
                          "enum custom_keycodes {
	EPRM = SAFE_RANGE,
	SS_MACRO_AD7F480974FAECF2966EEDEDC4AE5C19,
	SS_MACRO_69F3FEEF1D831D39BA5F6820F90ED473,
};

"))
    (should (string-equal process-record-user
                          "bool process_record_user(uint16_t keycode, keyrecord_t *record) {
	if (record->event.pressed) {
		switch (keycode) {
		case EPRM:
			eeconfig_init();
			return false;
		case SS_MACRO_AD7F480974FAECF2966EEDEDC4AE5C19:
			SEND_STRING(SS_TAP(X_A) SS_TAP(X_B) SS_TAP(X_C));
			return false;
		case SS_MACRO_69F3FEEF1D831D39BA5F6820F90ED473:
			SEND_STRING(\"whatever\");
			return false;
		}
	}
	return true;
}

"))
    ))

(ert-deftest mugur-test-tapdance-c ()
  (let ((tapdances
         (mapcar #'mugur--tapdance
            '((x y)
              (a b)
              (a emacs_layer)))))
    (should
     (string-equal
      (mugur--c-tapdance-enum tapdances)
      "enum {
	TD_X_Y,
	TD_A_B,
	TD_A_EMACS_LAYER,
};

"))
    (should
     (string-equal
      (mugur--c-tapdance-actions tapdances)
      "qk_tap_dance_action_t tap_dance_actions[] = {
	[TD_X_Y] = ACTION_TAP_DANCE_DOUBLE(KC_X, KC_Y),
	[TD_A_B] = ACTION_TAP_DANCE_DOUBLE(KC_A, KC_B),
	[TD_A_EMACS_LAYER] = ACTION_TAP_DANCE_LAYER_TOGGLE(KC_A, EMACS_LAYER),
};

"))))
