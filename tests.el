(ert-deftest keycodes-should-not-error ()
    (dolist (category supported-keycodes)
      (dolist (entry (cdr category))
        (should (keycode (car entry))))))

(ert-deftest test-tapdance-p ()
  "Correctly interpret tapdances."
  (cl-dolist (test
       '(((x y) "TD_X_Y")
         ((x emacs) "TD_X_EMACS")
         ((x "emacs") nil)
         ((C x) nil)
         ((C M) nil)))
    (should (equal (aif (tapdance (car test))
                       (tapdance-name it)
                     nil)
                   (cadr test)))))

(ert-deftest test-macro ()
  (cl-dolist (test
       '((("you do" C-x) "\"you do\" SS_LCTL(\"x\")")
         ((M-x a)        "SS_LALT(\"x\") SS_TAP(X_A)")
         ((M-x a b)      "SS_LALT(\"x\") SS_TAP(X_A) SS_TAP(X_B)")
         ((M-x "this" a) "SS_LALT(\"x\") \"this\" SS_TAP(X_A)")
         ))
    (should (equal (macro-define (car test))
                   (cadr test)))))

(ert-deftest test-combo-define ()
  (cl-dolist (test
       '(((a x "whatever") (("KC_A" "KC_X") ("\"whatever\"")))
         ((a x ("whatever")) (("KC_A" "KC_X") ("\"whatever\"")))
         ((a x (x "whatever")) (("KC_A" "KC_X") ("SS_TAP(X_X)" "\"whatever\"")))
         ((a x C-x) (("KC_A" "KC_X") ("SS_LCTL(\"x\")")))
         ((a x (C-x "whatever")) (("KC_A" "KC_X") ("SS_LCTL(\"x\")" "\"whatever\"")))))
    (should (equal (combo-define (car test))
                   (cadr test)))))

(ert-deftest test-transform-key ()
  (cl-dolist (test
       '((()      "___")
         ((c)     "KC_C")
         ((C)     "LCTL")
         ((M-a)   "LALT(KC_A)")
         ((C-M-a) "LCA(KC_A)")
         ((x y)   "TD(TD_X_Y)")
         (("what you do") "SS_MACRO_F20F55CF099E6BE80B9D823C1C609006")
         ((M a)   "MT(MOD_LALT, KC_A)")))
    (should (equal (transform-key (car test))
                   (cadr test)))))

;; Test the generated C code.
(ert-deftest test-macro-c ()
  (let* ((macros (mapcar #'macro '((a b c) ("whatever"))))
         (custom-keycodes (c-custom-keycodes macros))
         (process-record-user (c-process-record-user macros)))
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

(ert-deftest test-tapdance-c ()
  (let ((tapdances
         (mapcar #'tapdance
            '((x y)
              (a b)
              (a emacs_layer)))))
    (should
     (string-equal
      (c-tapdance-enum tapdances)
      "enum {
	TD_X_Y,
	TD_A_B,
	TD_A_EMACS_LAYER,
};

"))
    (should
     (string-equal
      (c-tapdance-actions tapdances)
      "qk_tap_dance_action_t tap_dance_actions[] = {
	[TD_X_Y] = ACTION_TAP_DANCE_DOUBLE(KC_X, KC_Y),
	[TD_A_B] = ACTION_TAP_DANCE_DOUBLE(KC_A, KC_B),
	[TD_A_EMACS_LAYER] = ACTION_TAP_DANCE_LAYER_TOGGLE(KC_A, EMACS_LAYER),
};

"))))
