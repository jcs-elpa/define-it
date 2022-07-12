[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![MELPA](https://melpa.org/packages/define-it-badge.svg)](https://melpa.org/#/define-it)
[![MELPA Stable](https://stable.melpa.org/packages/define-it-badge.svg)](https://stable.melpa.org/#/define-it)

# define-it
> Define, translate, wiki the word.

[![CI](https://github.com/jcs-elpa/define-it/actions/workflows/test.yml/badge.svg)](https://github.com/jcs-elpa/define-it/actions/workflows/test.yml)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [define-it](#define-it)
    - [Dependencies](#dependencies)
    - [Services](#services)
    - [Usage](#usage)
        - [About Showing/Displaying](#about-showingdisplaying)
        - [About Output](#about-output)
        - [About Delimiter](#about-delimiter)
        - [About Google Translate](#about-google-translate)
    - [Contribution](#contribution)

<!-- markdown-toc end -->

*P.S. Inspired by [Amazon Kindle](https://en.wikipedia.org/wiki/Amazon_Kindle)'s reading experiences.*

## Dependencies

* [google-translate](https://github.com/atykhonov/google-translate)
* [wiki-summary](https://github.com/jozefg/wiki-summary.el)

## Services

* [Collins Dictionary](https://www.collinsdictionary.com/)
* [Google Translate](https://translate.google.com/)
* [Wikipedia](https://www.wikipedia.org/)

## Usage

These are the function calls available.

* `define-it`
* `define-it-at-point`

## Customization

### About Showing/Displaying

You can customize these variables below for controlling the displayed on the menu.

* `define-it-show-dictionary-definition`
* `define-it-show-google-translate`
* `define-it-show-wiki-summary`

### About Output

There are two ways of outputing the menu. `pop` will output it in tooltip. `view`
will output it in the another buffer.

```el
(setq define-it-output-choice 'view)  ; Output with buffer.
(setq define-it-output-choice 'pop)   ; Output with tooltip.
```

### About Google Translate

If you don't want to select the `source` and `destination` every time,
you should consider set these variables below like this.

```el
(setq google-translate-default-source-language "auto")  ; Auto detect language.
(setq google-translate-default-target-language "en")    ; Set your target language.
```

## Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple)](https://github.com/bbatsov/emacs-lisp-style-guide)
[![Donate on paypal](https://img.shields.io/badge/paypal-donate-1?logo=paypal&color=blue)](https://www.paypal.me/jcs090218)

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
