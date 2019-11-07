[![Build Status](https://travis-ci.com/jcs090218/define-it.svg?branch=master)](https://travis-ci.com/jcs090218/define-it)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)


# define-it
> Define, translate, wiki the word.

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

There are two way of outputing the menu. `pop` will output it in tooltip. `view`
will output it in the another buffer.

```el
(setq define-it-output-choice 'view)  ; Output with buffer.
(setq define-it-output-choice 'pop)   ; Output with tooltip.
```

### About Delimiter

There are two type of delimiter string that you can customize.

* `define-it-delimiter-header`  [FOR HEADER]
* `define-it-delimiter-info`    [FOR OTHERS]

If you don't want the header to be displayed toggle `define-it-show-header`
variable.

```el
(setq define-it-show-header t)  ; Default is shown
```


### About Google Translate

If you wouldn't want to select the `source` and `destination` every time,
you should consider set these variables below like this.

```el
(setq google-translate-default-source-language "auto")  ; Auto detect language.
(setq google-translate-default-target-language "en")    ; Set your target language.
```


## Contribution

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
