# Overview

(Disclaimer: This is still in beta. Expect bugs! Issues, suggestions,
collaborations or thanks are highly welcomed at this stage.)

Mugur is a high-level mini-language for configuring and generating keymaps for
[qmk-powered keyboards](https://beta.docs.qmk.fm/). The list of currently supported keyboards an be
found in the /layouts folder.

```emacs-lisp
(mugur-keymap "example keymap" "5plus2"
  :tapping-term 200
  :layers
  '(("base"
     ((q) (C w) (M e) (G r) (t) (y)
        (lt numbers bspace) (M-x)))

    ("numbers"
     ((1) (2) (3) (4) (5) (1 2 3 "here we go")
                                   ( ) ( )))))
```

The example defines a new keymap for an hypotetical keyboard with only 7 keys,
the "5plus2". Each keymap has a name, a keyboard and a list of layers plus
additional configuration options. The keyboard name must be one of the supported
keyboards from /layers folder. I only have the Ergodox Ez. Whoever has something
else, is free to add to the /layers folder.

A `mugur-keymap` layer is a list of keys, equal to the number of keys on your qmk
keyboard. Each key can be as simple as (k), which means send `k` when tapped, or
can be a macro like ("myemailaddress@me.com") which will send the respective
string. All keys are specified as a lisp list. Mugur implements a mini language
to interpret the given key and do different things depending on its type. 
Mugur generates all the C code and files needed by the qmk compiler and offers a
simplified and higher level interface for all the functionality that qmk offers
withough having to touch any line of C.

# Supported keys in the mugur-keymap layers 

All these keys can be used in the `mugur-keymap`. Unlike the qmk C code which
requires specifing enums, adding arrays or adding defines, there is nothing else
for you to do here, besides specifying the functionality you want for the
respective hey, since mugur generates all the needed C code behind the
scenes.

## Simple keys

qmk page: [Simple Keycodes](https://beta.docs.qmk.fm/using-qmk/simple-keycodes)

All the normal keys are supported, including all the letters, numbers,
punctuation marks, commands (like enter) and mouse keys. Most of them can be
specified as is, like (x), (enter) or (^) but some, like the open paranthesis
for example, has to be specified either as a string, like ("(") or
(lparens). The full list of supported keycodes can be checked out by calling
`mugur-doc-keycodes`. The right hand side on that list corresponds to the qmk's
[simple keycodes](https://beta.docs.qmk.fm/using-qmk/simple-keycodes/keycodes) list, but without the KC_ prefix.

## Send keycode when tapped, act like Modifier when held

qmk page: [Mod-Tap](https://beta.docs.qmk.fm/using-qmk/advanced-keycodes/mod_tap)

One of the qmk's strong features is keys that do multiple things. One of them is
keys that act as a normal key when tapped but like a modifier key when held.

For example, `(C a)` will send `a` when tapped (pressed and released) and will
act like the Control key (C key in Emacs-speak) when held. The other modifiers
are M, G (win), and S (Shift). Combinations like C-M, C-M-S or C-M-G are also
possible. Consult the `mugur-doc-keycodes` list for the supported modifiers.

Be careful, though. The modifier key has to be in uppercase and has to be the
first in the list, otherwise `(c a)` means a totally different thing.

## Press both a modifier and a simple key

qmk page: [Modifier Keys](https://beta.docs.qmk.fm/using-qmk/simple-keycodes/feature_advanced_keycodes)

With this feature, a single key press can act like a keyboard shortcut. For
example, `(C-a)` will send `C-a` when tapped. That is, send the `a` keycode with
C (Control) pressed.

## Toggle or switch to layers

qmk page: [Layers](
[qmk](https://beta.docs.qmk.fm/using-qmk/software-features/feature_layers))

These keys can act like a normal key when tapped, but can momentarily switch to
a given layer when held, for example. There are a lot of variants for these
layer-switching keys. Check out the `mugur-doc-layer-switching` or the official
qmk documentation linked above.

Unlike the previous examples where the key definition is context aware, in this
key, the first element of the key must be one of lt, to, etc. to unanbiguously
define what kind of layer switching you want. 

For example, `(lt symbols a)` will send `a` when tapped and momentarily switch to the
`symbols` layer when held. The `symbols` layers must be available in the
`mugur-keymap` list of layers.

## A single key to replace two keys ()

qmk page: [Tapdance](https://beta.docs.qmk.fm/using-qmk/software-features/feature_tap_dance)

The tapdance feature is vast and featureful. Mugur only supports one simple case
wher you can send two different characters with a single key, for example.

That is, `(a b)` will send `a` when tapped once and `b` when tapped twice in quick
succession. An alternative is to specify a layer as the second key, `(a
symbols)`, which will switch to the `symbols` layer when tapped twice.

## Send any string or sequence of keys of any length

qmk page: [Macros]([qmk](https://beta.docs.qmk.fm/using-qmk/advanced-keycodes/feature_macros))

If your key definition doesn't mach any definition from above, mugur will infer
that your key is more than likely a macro. With a macro, you can send your email
address when you press a key, or send any other key combination.

For example, `(C-e "this is awesome." enter)` would define a key that, when
tapped, will call C-e, then send "this is awesome" followed by Enter.

These two are equivalent macros: `(a b c)`, `("a b c")`.

## Do something different when two keys are pressed at the same time

qmk page: [Combos](https://beta.docs.qmk.fm/using-qmk/software-features/feature_combo)

Combos are not keys you can use in the `mugur-keymap` layers, but specify what
happens when you press two keys at the same time.

`mugur-keymap` supports a keyboard argument named `:combos`. Each entry in that
list is a combo. For example `(left right esc)` specifies a combo, `(x y (C-x
"now"))` another one.  In the above case, pressing `left` and `right` and the
same time will send the `escape` key and pressing `x` and `y` will send C-x
followed by "now".

## User-defined key names

All the above key definitions can be given a name. This is useful when the
become quite large and are hard to see or destroy the visual look of the layer.
`mugur-keymap` supports the `:with-keys` argument for those cases. For example
these are two keys that can be used as (myspace) and (em-split) in the
`mugur-keymap` layers,

```emacs-lisp
:with-keys '((mybspace (lt xwindow bspace))
             (em-split (C-x 3)))

```
The first element of each list is the new key name and the second element is
anything that could have been specified directly in the `mugur-keymap`
layers. In short, this is just a list of shortcuts.


# Layer general config options

These two options can be specified anywhere after the layer name in the
`mugur-kemay` entry. These do not affect the key definitions.

## Turn on LEDs on layer switching

When defining the `mugur-keymap`, you can add a list of three elements after a
layer name but before the key definitions. A list like `(1 0 0)` will turn on
the first led on your keyboard when that layer is active. You can turn on any
led, all the leds or none. This feature is only supported for Ergodox Ez
keyboards for now. PRs accepted for other keyboards.

## Keymap orientation

Since the base layer usually has lots and lots of keys, it might be better to
split the keyboard halves vertically for a better view. For layers where most
of the keys are transparent keys, the layout can be more compact and a
horizontal split might be more convenient. The default is horizontal, but you
can change this behaviour by simply adding a `vertical` (or confirm it by adding
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

