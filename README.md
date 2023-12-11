[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![MELPA](https://melpa.org/packages/define-it-badge.svg)](https://melpa.org/#/define-it)
[![MELPA Stable](https://stable.melpa.org/packages/define-it-badge.svg)](https://stable.melpa.org/#/define-it)

# define-it
> Define, translate, wiki the word.

[![CI](https://github.com/jcs-elpa/define-it/actions/workflows/test.yml/badge.svg)](https://github.com/jcs-elpa/define-it/actions/workflows/test.yml)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [define-it](#define-it)
    - [📌 Dependencies](#📌-dependencies)
    - [🛎️ Services](#🛎️-services)
    - [🔨 Usage](#🔨-usage)
    - [🔧 Customization](#🔧-customization)
        - [Showing/Displaying](#showingdisplaying)
        - [Output](#output)
        - [Google Translate](#google-translate)
    - [Contribute](#contribute)

<!-- markdown-toc end -->

*P.S. Inspired by [Amazon Kindle](https://en.wikipedia.org/wiki/Amazon_Kindle)'s reading experiences.*

## 📌 Dependencies

* [define-word](https://github.com/abo-abo/define-word)
* [google-translate](https://github.com/atykhonov/google-translate)
* [wiki-summary](https://github.com/jozefg/wiki-summary.el)

## 🛎️ Services

* [Google Translate](https://translate.google.com/)
* [Wikipedia](https://www.wikipedia.org/)

## 🔨 Usage

These are the function calls available.

* `define-it`
* `define-it-at-point`

## 🔧 Customization

### Showing/Displaying

You can customize these variables below for controlling the displayed on the menu.

* `define-it-show-dictionary-definition`
* `define-it-show-google-translate`
* `define-it-show-wiki-summary`

### Output

There are two ways of outputing the menu. `pop` will output it in tooltip. `view`
will output it in the another buffer.

```el
(setq define-it-output-choice 'view)  ; Output with buffer.
(setq define-it-output-choice 'pop)   ; Output with tooltip.
```

### Google Translate

If you don't want to select the `source` and `destination` every time,
you should consider set these variables below like this.

```el
(setq google-translate-default-source-language "auto")  ; Auto detect language.
(setq google-translate-default-target-language "en")    ; Set your target language.
```

## 🛠️ Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple)](https://github.com/bbatsov/emacs-lisp-style-guide)
[![Donate on paypal](https://img.shields.io/badge/paypal-donate-1?logo=paypal&color=blue)](https://www.paypal.me/jcs090218)
[![Become a patron](https://img.shields.io/badge/patreon-become%20a%20patron-orange.svg?logo=patreon)](https://www.patreon.com/jcs090218)

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!

### 🔬 Development

To run the test locally, you will need the following tools:

- [Eask](https://emacs-eask.github.io/)
- [Make](https://www.gnu.org/software/make/) (optional)

Install all dependencies and development dependencies:

```sh
$ eask install-deps --dev
```

To test package's installation:

```sh
$ eask package
$ eask install
```

To test compilation:

```sh
$ eask compile
```

**🪧 The following steps are optional, but we recommend you follow these lint results!**

The built-in `checkdoc` linter:

```sh
$ eask lint checkdoc
```

The standard `package` linter:

```sh
$ eask lint package
```

*📝 P.S. For more information, find the Eask manual at https://emacs-eask.github.io/.*

## ⚜️ License

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

See [`LICENSE`](./LICENSE.txt) for details.
