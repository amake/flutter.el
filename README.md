[![MELPA](https://melpa.org/packages/flutter-badge.svg)](https://melpa.org/#/flutter)

# flutter.el

Emacs tools for working with the [Flutter SDK](https://flutter.io/)

# Features

Flutter.el helps you run the `flutter` binary interactively as an inferior
process. It's designed to work together with
[`dart-mode`](https://github.com/bradyt/dart-mode): for instance the example
configuration below binds `flutter-run-or-hot-reload` to <kbd>C-M-x</kbd> in
`dart-mode`. While editing your Dart code, just hit <kbd>C-M-x</kbd> to either
run your app, or if it's already running, to hot-reload it.

# Installation

You can install from [MELPA](https://melpa.org/#/flutter) with `package.el`:

```
M-x package-install flutter
```

# Configuration

Set `flutter-sdk-path` to the location of your Flutter SDK. This isn't even
necessary if the `flutter` binary is on your `PATH`.

## Example

Using [`use-package`](https://jwiegley.github.io/use-package/) and assuming you
put the Flutter SDK in `/Applications/flutter`:

```elisp
(use-package dart-mode
  :custom
  (dart-format-on-save t)
  (dart-enable-analysis-server t)
  (dart-sdk-path "/Applications/flutter/bin/cache/dart-sdk/"))

(use-package flutter
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-M-x" . #'flutter-run-or-hot-reload))
  :custom
  (flutter-sdk-path "/Applications/flutter/"))
```

# License
GPL-3
