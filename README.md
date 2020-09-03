# Overview

Mugur is a high-level [ErgoDox EZ](https://ergodox-ez.com/) Configurator for
Emacs users. 

Mugur offers a consistent interface for all the ErgoDox features.  With the C
code configurator, for example, if you want to specify an Ergodox macro key, you
first have to give your key a unique name, then write some C code to implmenet
this new key, and then finally use it in one of your layers. If you want a
mod-tap key, the modifier keycode is different than if you'd just wanted to have
a simple modifier key. With mugur, all these features are defined in a
consistent way by having all the keys be just simple lists, defined directly in
your layers. No extra code, no extra functions. Mugur interprets these mugur-keys
differently, depending on the context, and generates the correct C code to
implement their semantics.

A mugur-key is just a list of symbols, and can be `(k)` for sending the `k`
character when tapped, or `(C k)` for sending `k` when tapped but acting like
Control when held (mod-tap), a string like `("my_email_address@me.com")`, a key
combination like the often used `(C-u C-space)`, or even an fbound Emacs
function, like `(other-window)`, among other things (see all features
[below](#supported-keys-in-the-mugur-keymap-layers)).

This is a simplified example that defines an Ergodox configuration, and contains
macros, simple keys, mod-tap, layer changes, combos and emacs fbound symbols
(emacs functions). For a more complex example, see the
[mugur-keymap](https://github.com/mihaiolteanu/.emacs.d/blob/master/init.el#L487)
configuration from my init.el file, the configuration that I'm using to flash my
ErgoDox.

```emacs-lisp
(mugur-keymap "ergodox_ez" "mugur"
  :tapping-term 200
  :rgblight-animations nil

  :combos '((left right escape)
            (x y (C-x "pressed both x and y at once")))
  
  :with-keys '((mybspace (lt numeric bspace))
               (emacs-split (C-x 3)))
  
  :layers
  '(("base" vertical
     ((---)   (vol-down) (vol-up) (---) (---) (---) (reset) 
      (---)      (q)        (w)     (e)   (r)  (t)   (---) 
      (---)      (a)       (G t)   (M d) (C f) (g) 
      (osm S)    (z)        (x)     (c)   (v)  (b)   (---) 
      (---)     (---)      (---)   (---) (---) 
                  (other-window) (save-buffers-kill-emacs) 
                                                     (M-x)
                       (mybspace) (lt numeric space) (tab))

     (---) (---) (---)  (---)  (---) (---)  (---)
     (---)  (y)   (u)    (i)    (o)  (---)  (---)
            (h)  (C j)  (M k)  (G l)  (p)   (---)
     (---)  (n)   (m)  (comma) (dot) (---) (osm S)
                 (---)  (---)  (---) (---)  (---)
     (emacs-split) (---)
     (C-u C-space)
     (escape) (enter) (---))
  
  ("numeric"
    (( ) ( ) ( ) ( ) ( ) ( ) ( )     ( ) ( ) ( ) ( ) ( ) ( ) ( )
     ( ) ( ) (1) (2) (3) ( ) ( )     ( ) ( ) ( ) ( ) ( ) ( ) ( )
     ( ) ( ) (4) (5) (6) ( )             ( ) ( ) ( ) ( ) ( ) ( )
     ( ) (0) (7) (8) (9) ( ) ( )     ( ) ( ) ( ) ( ) ( ) ( ) ( )
     ( ) ( ) ( ) ( ) ( )                     ( ) ( ) ( ) ( ) ( )
                         ( ) ( )     ( ) ( )
                         (1 2 3)     ("one two three")
                     ( ) ( ) ( )     ( ) ( ) ( )))))
                     
```

# Install

Git clone it, for now, until this package  will be available from MELPA.
```bash
git clone https://github.com/mihaiolteanu/mugur ~/.emacs.d/lisp/mugur
```

Add mugur to your load-path

```emacs-lisp
(add-to-list 'load-path "~/.emacs.d/lisp/mugur")
```

Set the `mugur-qmk-path` to point to the location of your [qmk source
code](https://github.com/qmk/qmk_firmware).

```emacs-lisp
(setf mugur-qmk-path "/home/mihai/projects/qmk_firmware")
```

You'll also need [wally-cli](https://github.com/zsa/wally) if you want to flash
directly from Emacs.

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

## Emacs functions

**(fbound-emacs-symbol)**

Specify an fbound symbol (a function name) directly in the key definition. Mugur
keeps an internal list of exotic and unbound key sequences (kbd's) which it can
bind to the functions specified in the layers definition.

That is, you specify (sp-next-sexp) as a key definition, for example, mugur
finds an available key sequence, say C-F5, and puts that in your keymap. When
you flash your keyboard, mugur generates an .el file that contains bind-key
forms for all such keys. When flashing, mugur loads this file for you, but when
you start or restart Emacs you'll have to manually load this file by calling
`mugur-load-keybindings` in your init.el file or interactively.

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
for each `mugur-keymap`. These are all implemented as keyword arguments, and
include `tapping-term`, `combo-term`, `rgblight-enable`, `rgblight-animations`
and `force-nkro`. The meaning and functionality of these arguments should be
checked in the [qmk
documentation](https://beta.docs.qmk.fm/developing-qmk/qmk-reference/config_options#behaviors-that-can-be-configured). More
such config options can be added in the future, as the need arises. Open an
issue if you need something from the qmk extensive list of options.

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

