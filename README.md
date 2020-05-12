# Overview

(Disclaimer: This is still in beta. Expect bugs! Issues, suggestions,
collaborations or thanks are highly welcomed at this stage.)

Mugur is a high-level mini-language for configuring and generating keymaps for
[qmk-powered keyboards](https://beta.docs.qmk.fm/). A simplified example to get
things moving,

```emacs-lisp
(mugur-keymap "example-keymap" "5plus2"

 ;; Configuration options.
 :tapping-term 200
 :rgblight-animations nil
 
 ;; Combos and user-defined mugur-keys. 
 :combos '((left right escape))
 :with-keys '((em-split (C-x 3))
              (em-unsplit (C-x 0)))

 ;; Each one of these is a layer that has a name and a list of mugur-keys
 :layers
 '(;; A shot at a qwerty layout, with the first key doing nothing, the following
   ;; three acting like w, e and r when tappend but as modifier keys when held,
   ;; a simple t key and a key that sends y when tapped once but q when tapped
   ;; twice in quick succession. The last two keys is a backspace that
   ;; momentarily switches to the numbers layer when held and a key that
   ;; permanentely switches to the emacs layer.
   ("base"
    ((---) (C w) (M e) (G r) (t) (y q)
             (lt numbers bspace) (to emacs)))

   ;; A non-interesting numbers layer. Notice the last two keys are
   ;; transparent, they are not assigned to anything, meaning they do whatever
   ;; the layer above it does.
   ("numbers"
    ((1) (2) (3) (4) (5) (0)
                     ( ) ( )))

   ;; A nice try for a layer to simplify emacs usage. The first two keys are
   ;; for window management and their names and definition actually comes from
   ;; the :with-keys parameter. The following two is the beginning and end of
   ;; line movement functions, followed by a simple TAB and a way to insert
   ;; some test list that you use often in your developement. The last two keys
   ;; is chaninging buffers and a means to go back to the base layer.
   ("emacs"
    ((em-split) (em-unsplit) (C-a) (C-e) (TAB) ("'(1 2 3)")
                                       (C-x b) (to base)))))
```

The example defines a new keymap for an hypothetical keyboard with only 7 keys,
the "5plus2". Each keymap has a name, a keyboard and a list of layers plus
additional configuration options. You can have as many keymaps as you want and
then build and flash them independently. The keyboard name must be one of the
supported keyboards from /layers folder. I only have the Ergodox Ez. Whoever has
something else, is free to add to the /layers folder. 

If you want to start a real keyboard config, see the template for each keyboard in the /layouts
folder (here is the template for the
[ergodox](https://github.com/mihaiolteanu/mugur/blob/master/layouts/ergodox-ez.el#L33)
keyboard, for example). For my own try at configuring my Ergodox, see [my
init.el file](https://github.com/mihaiolteanu/.emacs.d/blob/master/init.el#L486)

A `mugur-keymap` layer is a list of mugur-keys, as I call them, equal to the
number of keys on your qmk keyboard. Each mugur-key definition can be as simple
as `(k)`, which means send `k` when tapped, or can be a macro like
`("myemailaddress@me.com")` which will send the respective string. All mugur-keys
are specified as a lisp list. Mugur implements a mini-language to interpret the
given key and do different things depending on its type. Mugur generates all the
C code and files needed by the qmk compiler and offers a simplified and higher
level interface for all the functionality that qmk offers without having to
touch any line of C.

# Install

Git clone it, for now, until this package  will be submitted to MELPA.
```bash
git clone https://github.com/mihaiolteanu/mugur ~/.emacs.d/lisp/mugur
```

Add mugur to your load-path

```emacs-lisp
(add-to-list 'load-path "~/.emacs.d/lisp/mugur")
```

And set the `mugur-qmk-path` to point to where you've cloned the qmk source
code.

```emacs-lisp
(setf mugur-qmk-path "/home/mihai/projects/qmk_firmware")
```

# Supported keys in the mugur-keymap layers 

The following are all qmk features that are supported by mugur by directly
specifying a mugur-key in the `mugur-keymap` definition. A mugur-key is a list
that contains one or more qmk keycodes, modifiers, special symbols and the
like. Depending on the context, the same keycode might mean different
things. Bellow are the definitions of these mugur-keys which highlight the
supported features of mugur.

## Simple keys ([qmk](https://beta.docs.qmk.fm/using-qmk/simple-keycodes))

**(key)**

This defines a simple, normal key, like in a classic keyboard. All the normal keys
are supported, including all the letters, numbers, punctuation marks, commands
(like enter) and mouse keys. Most of them can be specified as is, like (x),
(enter) or (^) but some, like the open parenthesis for example, has to be
specified either as a string, like ("(") or (lparens). The full list of
supported keycodes can be checked out by calling `mugur-doc-keycodes`. The right
hand side on that list corresponds to the qmk's [simple
keycodes](https://beta.docs.qmk.fm/using-qmk/simple-keycodes/keycodes) list, but
without the KC_ prefix.

## Mod-Tap ([qmk](https://beta.docs.qmk.fm/using-qmk/advanced-keycodes/mod_tap))

**(modifier key)**

Send the key when tapped, as above, but act like the modifier key when held
. The modifiers are C, M, G, and S for Control, Alt, Win and Shift. Combinations like C-M,
C-M-S or C-M-G are also possible. Consult the `mugur-doc-keycodes` list for the
supported modifiers.

The modifier key has to be in uppercase and has to be the first in the list,
otherwise `(c a)` means a totally different thing.

## Modifier Keys ([qmk](https://beta.docs.qmk.fm/using-qmk/simple-keycodes/feature_advanced_keycodes))

**(modifier-key)**

Hold down the modifier and press key at the same time. For example, `(C-a)` will
send `C-a` when tapped. That is, send the `a` keycode with C (Control) pressed.

## Layers ([qmk](https://beta.docs.qmk.fm/using-qmk/software-features/feature_layers))

**(tg layer)**\
**(lt layer mod-or-key)**

Send key when tapped, momentarily switch to layer when held, for example. There
are a lot of variants for these layer-switching keys. Check out the
`mugur-doc-layer-switching` or the official qmk documentation linked above for
the complete list

Unlike the previous examples where the key definition is context aware, in this
key, the first element of the key must be one of **lt**, **to**, etc. to unambiguously
define what kind of layer switching you want. 

For example, `(lt symbols a)` will send `a` when tapped and momentarily switch to the
`symbols` layer when held. The `symbols` layers must be available in the
`mugur-keymap` list of layers.

## One Shot Layer ([qmk](https://beta.docs.qmk.fm/using-qmk/software-features/one_shot_keys))

**(osl layer)**

Momentarily activates layer until a key is pressed, after which, if goes back to
the original layer.

## One Shot Modifier ([qmk](https://beta.docs.qmk.fm/using-qmk/software-features/one_shot_keys))

**(osm modifier)**

Similar to the one shot layer, momentarily activates the modifier key until a key is
pressed, after which, it deactivates it. Useful for Shift modifiers, for example, to
insert uppercase letters.

## Tapdance ([qmk](https://beta.docs.qmk.fm/using-qmk/software-features/feature_tap_dance))

**(key1 key2)**\
**(key layer)**

The tapdance feature is vast and featureful. Mugur only supports one simple case
where you can send two different characters with a single key, for example.

That is, `(a b)` will send `a` when tapped once and `b` when tapped twice in quick
succession. An alternative is to specify a layer as the second key, `(a
symbols)`, which will switch to the `symbols` layer when tapped twice.

## Macros ([qmk](https://beta.docs.qmk.fm/using-qmk/advanced-keycodes/feature_macros))

**(any number of keys, modifiers or strings)**

If your key definition doesn't mach any definition from above, mugur will infer
that your key is more than likely a macro. With a macro, you can send your email
address when you press a key, or send any other key combination.

For example, `(C-e "this is awesome." enter)` would define a key that, when
tapped, will call C-e, then send "this is awesome" followed by Enter.

These two are equivalent macros: `(a b c)`, `("a b c")`.

## User-defined mugur-key names

**(mykey)**

All the above key definitions can be given a name. This is useful when they
become quite large and are hard to see or destroy the visual look of the layer
(long macros, for example).  `mugur-keymap` supports the `:with-keys` argument
for these cases. For example these are two keys that can be used as (myspace)
and (em-split) in the `mugur-keymap` layers,

```emacs-lisp
:with-keys '((mybspace (lt xwindow bspace))
             (em-split (C-x 3)))

```
The first element of each list is the new key name and the second element is
anything that could have been specified directly in the `mugur-keymap`
layers. In short, this is just a list of shortcuts.

## Combos ([qmk](https://beta.docs.qmk.fm/using-qmk/software-features/feature_combo))

**(key1 key2 action-or-key)**

Combos are not keys you can use in the `mugur-keymap` layers, but specify what
happens when you press two keys at the same time.

`mugur-keymap` supports a keyboard argument named `:combos` for these cases,

```emacs-lisp
:combos '((left right escape)
          (x y (C-x "now")))
```
In the above case, pressing `left` and `right` and the same time will send the
`escape` key and pressing `x` and `y` will send C-x followed by "now".

# Layer general config options

These two options can be specified anywhere after the layer name in the
`mugur-kemap` entry. These do not affect the mugur-key definitions.

## Turn on LEDs on layer switching

When defining the `mugur-keymap`, you can add a list of three elements after a
layer name but before the key definitions list. A list like `(1 0 0)` will turn
on the first led on your keyboard when that layer is active. You can turn on any
LED, all the LEDs or none. This feature is only supported for Ergodox Ez
keyboards for now. PRs accepted for other keyboards.

## Keymap orientation

Since the base layer usually has lots and lots of keys, it might be better to
split the keyboard halves vertically for a better view. For layers where most
of the keys are transparent keys, the layout can be more compact and a
horizontal split might be more convenient. The default is horizontal, but you
can change this behavior by simply adding a `vertical` (or confirm it by adding
`horizontal`) between the layer name and the actual layer keys.

# Configuration options

Besides the actual layers, you can specify a list of additional config options
for each `mugur-keymap` (you can have as many `mugur-keymaps` as you
want). These are all implemented as keyword arguments, and include
`tapping-term`, `combo-term`, `rgblight-enable`, `rgblight-animations` and
`force-nkro`. The meaning and functionality of these arguments should be checked
in the [qmk documentation](https://beta.docs.qmk.fm/developing-qmk/qmk-reference/config_options#behaviors-that-can-be-configured). More such config options can be added in the
future, as the need arises. Open an issue if you need something from the qmk
extensive list of options.

# Generate, build and flash the qmk keyboards

All the following functionalities lets you select one of your keymaps, as
defined with `mugur-keymap`. If you only have one keymap defined, that keymap is
used by default.

## Generate keymap

`mugur-generate`: Generate the C files in the qmk_path/keymap-name folder. These
are the same files that you would write by hand but are now generated by mugur
based on the keymap specified with `mugur-keymap`.

## Make keymap

`mugur-make`: Call `make` on the generated qmk layout. A new `compile-mode`
buffer is opened with the compile results.

## Build keymap

`mugur-build`: Generate and make the keymap. This is equivalent to
calling `mugur-generate` and `mugur-make` one after another.

## Flash keymap

`mugur-flash`: Flash the keymap (actually the generated hex file). Currently
only supported for Ergodox Ez keyboards. For other boards the flashing process
might be different. Consider opening an issue or a PR if you own other keyboards
and you want this feature supported for them as well.

