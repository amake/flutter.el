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
  5. Appends a definition for the string to the template ARB file, e.g.

    ```
    "myString": "Hello, world!",
    ```
- `flutter-l10n-externalize-at-point`: Does all of the above but for the string
  literal at point only; kills the definition instead of adding it to the
  template ARB file.

## Lint your localization strings

A [Flycheck](https://www.flycheck.org/) checker calling
`intl_translation:extract_to_arb` from the
[`intl_translation`](https://pub.dev/packages/intl_translation) package is
available for making sure your strings are defined in the correct way. This is
published as a separate, optional package:
[flutter-l10n-flycheck](https://melpa.org/#/flutter-l10n-flycheck).

## Run tests

Flutter.el helps you run tests of your flutter application. There are 3 commands
available:

- `flutter-test-all` - run all tests from a flutter project.
- `flutter-test-current-file` - run all tests inside the current file.
- `flutter-test-at-point` - run single test or group of tests at point.

Easily activate keybindings for these with the `flutter-test-mode` minor
mode.

# Installation

You can install from [MELPA](https://melpa.org/#/flutter) with `package.el`:

```
M-x package-install flutter
```

and, optionally

```
M-x package-install flutter-l10n-flycheck
```

# Configuration

Set `flutter-sdk-path` to the location of your Flutter SDK. This isn't even
necessary if the `flutter` binary is on your `PATH`.

The localization tools will read your `/l10n.yaml` file by default, but
alternatively you can set the following variables:

- `flutter-l10n-arb-dir`: The relative path from project root where your ARB
  files are stored
- `flutter-l10n-template-arb-file`: The name of the ARB file that represents the
  default (source) language for your app
- `flutter-l10n-output-localization-file`: The name of the generated Dart file
  that you use in your app

If using flutter-l10n-flycheck be sure to call `flutter-l10n-flycheck-setup` and
set your L10N file to use the checker: `M-x add-file-local-variable-prop-line
flycheck-checker intl_translation`

## Example

Using [`use-package`](https://jwiegley.github.io/use-package/) and assuming you
put the Flutter SDK in `/Applications/flutter`:

```elisp
;; Assuming usage with dart-mode
(use-package dart-mode
  ;; Optional
  :hook (dart-mode . flutter-test-mode))

(use-package flutter
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-M-x" . #'flutter-run-or-hot-reload))
  :custom
  (flutter-sdk-path "/Applications/flutter/"))

;; Optional
(use-package flutter-l10n-flycheck
  :after flutter
  :config
  (flutter-l10n-flycheck-setup))
```

The propline for your L10N file:

```dart
// -*- flycheck-checker: intl_translation; -*-
```

# License
GPL-3
