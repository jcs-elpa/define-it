[![Build Status](https://travis-ci.com/jcs090218/define-it.svg?branch=master)](https://travis-ci.com/jcs090218/define-it)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)


# define-it
> Define the word

<p align="center">

</p>

*P.S. Inspired by [Amazon Kindle](https://en.wikipedia.org/wiki/Amazon_Kindle)'s reading experiences.*


## Dependencies

* [google-translate](https://github.com/atykhonov/google-translate)
* [wiki-summary](https://github.com/jozefg/wiki-summary.el)


## Usage

You can customize these variables below for controlling the displayed on the menu.

* `define-it-show-dictionary-definition`
* `define-it-show-google-translate`
* `define-it-show-wiki-summary`

You can customize `define-it-delimiter-string` variable for changing
the delimiter look like.

```el
String that separates each information section.
(setq define-it-delimiter-string "\n=>------\n")
```

### About Google Translate

If you wouldn't want to select the `source` and `destination` every time,
you should consider set these variables below like this.

```el
;; Auto detect language.
(setq google-translate-default-source-language "auto")
;; Set your target language.
(setq google-translate-default-target-language "en")
```


## Contribution

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
