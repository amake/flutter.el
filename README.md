[![MELPA](https://melpa.org/packages/flutter-badge.svg)](https://melpa.org/#/flutter)

# flutter.el

Emacs tools for working with the [Flutter SDK](https://flutter.io/)

# Features

## Run your app

Flutter.el helps you run the `flutter` binary interactively as an inferior
process. It's designed to work together with
[`dart-mode`](https://github.com/bradyt/dart-mode): for instance the example
configuration below binds `flutter-run-or-hot-reload` to <kbd>C-M-x</kbd> in
`dart-mode`. While editing your Dart code, just hit <kbd>C-M-x</kbd> to either
run your app, or if it's already running, to hot-reload it.

## Localize your app

Any non-trivial app will require localization, but if you're like me you
probably write your code first and worry about externalizing your strings
(moving their definitions to a separate file) later.

Flutter.el comes with some helpful features to make externalizing strings
easier, assuming you are following [best
practices](https://flutter.dev/docs/development/accessibility-and-localization/internationalization):

- `flutter-l10n-externalize-all`: A function that interactively does the
  following for each string literal in the current buffer:
  1. Prompts you to give an ID (class property name) to the string,
     e.g. `myString`
  2. Replaces the string literal with a reference to the localizations class,
     e.g. `MyLocalizations.of(context).myString`
  3. Deletes any `const` keywords that apply to the reference
  4. Appends the original string content as an end-of-line comment
  5. Appends a definition for the string to the localizations class, e.g.

    ```
    String get myString => Intl.message('Hello, world!', name: 'myString');
    ```
- `flutter-l10n-externalize-at-point`: Does all of the above but for the string
  literal at point only; kills the definition instead of adding it to the
  localizations class.

# Installation

You can install from [MELPA](https://melpa.org/#/flutter) with `package.el`:

```
M-x package-install flutter
```

# Configuration

Set `flutter-sdk-path` to the location of your Flutter SDK. This isn't even
necessary if the `flutter` binary is on your `PATH`.

To use the localization tools you will want to customize these variables for
your project:

- `flutter-l10n-classname`: The name of the localizations class where strings
  are defined
- `flutter-l10n-file`: The relative path from project root to the file that
  defines the localizations class. Should include the leading `lib/`.

## Example

Using [`use-package`](https://jwiegley.github.io/use-package/) and assuming you
put the Flutter SDK in `/Applications/flutter`:

```elisp
(use-package lsp-mode
  :commands lsp)

(use-package dart-mode
  :hook (dart-mode . lsp)
  :after lsp
  :ensure-system-package (dart_language_server . "pub global activate dart_language_server")
  :custom
  (dart-format-on-save t)
  (dart-sdk-path "/Applications/flutter/bin/cache/dart-sdk/"))

(use-package flutter
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-M-x" . #'flutter-run-or-hot-reload))
  :custom
  (flutter-sdk-path "/Applications/flutter/"))
```

A sample `.dir-locals.el` to go in your project root:

```elisp
((dart-mode
  (flutter-l10n-classname . "AppLocalizations")
  (flutter-l10n-file . "lib/app_l10n.dart")))
```

# License
GPL-3
