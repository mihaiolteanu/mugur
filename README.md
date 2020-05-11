# Overview

A high-level interface for configuring and generating keymaps for qmk-powered
keyboards.

A short and simplified example,

```emacs-lisp
(mugur-keymap "my keymap" "5plus2"
  :layers
  '(("base"
     ((q) (C w) (M e) (G r) (t) (y)
        (lt numbers bspace) (M-x)))

    ("numbers"
     ((1) (2) (3) (4) (5) (1 2 3 "here we go")
                                   ( ) ( )))))
```

The example defines a new keymap for an hypotetical keyboard with only 7 keys,
the "5plus2". Each keymap has a name, a keyboard and a list of layers. The
keyboard name must be one of the supported keyboards from /layers folder. I only
have the Ergodox Ez. Whoever has something else, is free to add to the /layers
folder.

A mugur-keymap layer is a list of keys, equal to the number of keys on your qmk
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
for you to do here, since mugur generates all the needed C code behind the
scenes.

## Simple keys ([qmk](https://beta.docs.qmk.fm/using-qmk/simple-keycodes))

`(k)` will send `k` when pressed. The full list can also be checked out by
calling `mugur-keycodes`.

## A modifier and a simple key ([Mod-Tap](https://beta.docs.qmk.fm/using-qmk/advanced-keycodes/mod_tap))

`(C a)` will send `a` when tapped (pressed and released) and will act like the
Control key (C key in Emacs-speak) when held. The other modifiers are M, for
Alt, G, for Win key and S for Shift. Combinations like C-M, C-M-S or C-M-G are
also possible. This has to be in uppercase, since `(c a)` means another
thing.

## Modifier keys ([qmk](https://beta.docs.qmk.fm/using-qmk/simple-keycodes/feature_advanced_keycodes))

`(C-a)` will send `C-a` when tapped. That is, send the `a` key with C pressed. 

## Layers ([qmk](https://beta.docs.qmk.fm/using-qmk/software-features/feature_layers))

`(lt symbols a)` will send `a` when tapped and momentarily switch to the
`symbols` layer when held. The `symbols` layers must be available in the
`mugur-keymap` list of layers. The full list of layer-switching functionality
can also be checket out by calling `mugur-layer-switching`.

## Tap dance ([qmk](https://beta.docs.qmk.fm/using-qmk/software-features/feature_tap_dance))

`(a b)` will send `a` when tapped once and `b` when tapped twice in quick
succession. An alternative is to specify a layer as the second key, `(a
symbols)`, which will switch to the `symbols` layer when tapped twice.

## Macros ([qmk](https://beta.docs.qmk.fm/using-qmk/advanced-keycodes/feature_macros))

`(a b c)` or `("a b c")` will send "a b c" when tapped. You can send any number
of keys, modifier keys or strings with this combination. For example, `(M-x)
"switch to bu" enter` will call the `M-x` emacs command, then it will fill out
some text and then press Enter.

## Combos ([qmk](https://beta.docs.qmk.fm/using-qmk/software-features/feature_combo))

`mugur-keymap` supports a keyboard argument named `:combos`. Each entry in that
list is a combo. For example `(left right esc)` specifies a combo, `(x y (C-x
"now"))` another one. These are not keys you can use in the `mugur-keymap`
layers, but specifie what happens when you press two keys at the same time. In
the above case, pressing `left` and `right` and the same time will send the
`escape` key and pressing `x` and `y` will send C-x followed by "now".

## With-keys 

You might happen to have long macros, layers names or maybe you just want to
name your keys. `mugur-keymap` supports the `:with-keys` argument for those
cases. For example these are two keys that can be used as (myspace) and
(em-split) in the `mugur-keymap` layers,

```emacs-lisp
:with-keys '((mybspace (lt xwindow bspace))
             (em-split (C-x 3)))

```
The first element of each list is the new key name and the second element is
anything that could have been specified directly in the `mugur-keymap`
layers. In short, this is just a list of shortcuts.

## Turn on LEDs on layer switching

When defining the `mugur-keymap`, you can add a list of three elements right
after a layer name. A list like `(1 0 0)` will turn on the first led on your
keyboard when that layer is active. You can turn on any led, all the leds or
none. This feature is only supported for Ergodox Ez keyboards for now. PRs
accepted for other keyboards.


# Configuration options

Besides the actual layers, you can specify a list of additional config options
for each `mugur-keymap`, since you can have as many `mugur-keymaps` as you
want. These are all implemented as keyword arguments, and include
`tapping-term`, `combo-term`, `rgblight-enable`, `rgblight-animations` and
`force-nkro`. The meaning and functionality of these arguments should be checked
in the [qmk documentation](https://beta.docs.qmk.fm/developing-qmk/qmk-reference/config_options#behaviors-that-can-be-configured). More such config options can be added in the
future, as the need arises. Open an issue if you need something from the qmk
extensive list of options.


