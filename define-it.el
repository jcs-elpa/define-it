;;; define-it.el --- Define the word.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Shen, Jen-Chieh
;; Created date 2019-10-24 11:18:00

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Define the word.
;; Keyword: dictionary explanation search wiki
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3") (s "1.12.0") (request "0.3.0") (google-translate "0.11.18") (popup "0.5.3") (pos-tip "0.4.6") (wiki-summary "0.1"))
;; URL: https://github.com/jcs090218/define-it

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Define the word.
;;

;;; Code:

(require 's)

(require 'request)
(require 'pos-tip)
(require 'popup)

(require 'google-translate)
(require 'wiki-summary)

(eval-when-compile
  ;; This stops the compiler from complaining.
  (defvar url-http-end-of-headers))


(defgroup define-it nil
  "Define the word."
  :prefix "define-it-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs090218/define-it"))

(defface define-it-pop-tip-color
  '((t (:foreground "#000000" :background "#FFF08A")))
  "Pop tip color, for graphic mode only."
  :group 'define-it)
(defvar define-it-pop-tip-color 'define-it-pop-tip-color)

(defcustom define-it-output-choice 'pop
  "Option to show output."
  :type '(choice (const :tag "pop" pop)
                 (const :tag "view" view))
  :group 'define-it)

(defcustom define-it-delimiter-string "\n=>------\n"
  "String that display for delimiter."
  :type 'string
  :group 'define-it)

(defcustom define-it-show-dictionary-definition t
  "Option to show dictionary definition."
  :type 'boolean
  :group 'define-it)

(defcustom define-it-show-google-translate t
  "Option to show google-translate."
  :type 'boolean
  :group 'define-it)

(defcustom define-it-show-wiki-summary t
  "Option to show wiki summary."
  :type 'boolean
  :group 'define-it)

(defcustom define-it-pop-tip-width-percentage 0.4
  "Pop tip width in percentage, default is 0.4."
  :type 'number
  :group 'define-it)

(defvar define-it--define-timer nil
  "Timer for defining.")
(defvar define-it--update-time 0.1
  "Run every this seconds until all information is received.")

(defvar define-it--current-word "" "Record the search word.")

(defvar define-it--dictionary-it nil "Flag to check if dictionary search done.")
(defvar define-it--google-translated nil "Flag to check if google translated done.")
(defvar define-it--wiki-summarized nil "Flag to check if wiki summary done.")

(defvar define-it--dictionary-content "" "Dictionary content string.")
(defvar define-it--google-translated-content "" "Google translate content string.")
(defvar define-it--wiki-summary-content "" "Wiki summary content string.")


(defun define-it--get-dictionary-definition-as-string (search-str)
  "Return the dictionary definition as a string with SEARCH-STR."
  (setq define-it--dictionary-it nil)
  (setq define-it--dictionary-content "Definition")
  (setq define-it--dictionary-it t))

(defun define-it--get-google-translate-as-string (search-str)
  "Return the google translate as a string with SEARCH-STR."
  (setq define-it--google-translated nil)
  (let* ((langs (google-translate-read-args nil nil))
         (source-language (car langs))
         (target-language (cadr langs))
         (kill-ring kill-ring))  ; Preserved `kill-ring'.
    (google-translate-translate source-language target-language search-str 'kill-ring)
    (setq define-it--google-translated-content (nth 0 kill-ring)))
  (setq define-it--google-translated t))

(defun define-it--get-wiki-summary-as-string (search-str)
  "Return the wiki summary as a string with SEARCH-STR."
  (setq define-it--wiki-summarized nil)
  (url-retrieve
   (wiki-summary/make-api-query search-str)
   (lambda (_events)
     (message "")  ; Clear the annoying minibuffer display.
     (goto-char url-http-end-of-headers)
     (let* ((json-object-type 'plist) (json-key-type 'symbol) (json-array-type 'vector)
            (result (json-read))
            (summary (wiki-summary/extract-summary result)))
       (setq define-it--wiki-summary-content (if summary summary "No article found")))
     (setq define-it--wiki-summarized t))))

(defun define-it--form-info-format ()
  "Form the info format."
  (concat
   (when define-it-show-dictionary-definition
     (concat "%s" define-it-delimiter-string))
   (when define-it-show-google-translate
     (concat "%s" define-it-delimiter-string))
   (when define-it-show-wiki-summary "%s")))

(defun define-it--get-definition ()
  "Use all services/resources to get the definition string."
  (format (define-it--form-info-format)
          define-it--dictionary-content
          define-it--google-translated-content
          define-it--wiki-summary-content))

(cl-defun define-it--in-pop (content &key point (timeout 300))
  "Define in the pop with CONTENT.
The location POINT. TIMEOUT for not forever delay."
  (if (display-graphic-p)
      (progn
        (pos-tip-show
         (pos-tip-fill-string content (* (frame-width) define-it-pop-tip-width-percentage))
         define-it-pop-tip-color point nil timeout)
        (define-it--kill-timer))
    (popup-tip content :point point :around t :scroll-bar t :margin t)))

(defun define-it--in-buffer (content)
  "Define in the buffer with CONTENT."
  (let* ((name (format "*define-it:%s*" define-it--current-word))
         (buf (if (get-buffer name) (get-buffer name) (generate-new-buffer name))))
    (with-current-buffer buf
      (view-mode -1)
      (delete-region (point-min) (point-max))  ; Remove all content.
      (insert content) (insert "\n")
      (goto-char (point-min))
      (text-mode)
      (view-mode 1)
      (visual-line-mode 1))
    (pop-to-buffer buf)))

(defun define-it--received-info-p ()
  "Check if received all informations."
  (let ((checker t))
    (when define-it-show-dictionary-definition
      (unless define-it--dictionary-it (setq checker nil)))
    (when define-it-show-google-translate
      (unless define-it--google-translated (setq checker nil)))
    (when define-it-show-wiki-summary
      (unless define-it--wiki-summarized (setq checker nil)))
    checker))

(defun define-it--display-info ()
  "Display the info after receving all necessary information."
  (if (define-it--received-info-p)
      (cl-case define-it-output-choice
        ('pop
         (define-it--in-pop (define-it--get-definition) :point (point)))
        (t
         (define-it--in-buffer (define-it--get-definition))))
    (define-it--reset-timer)))

(defun define-it--kill-timer ()
  "Kill the timer."
  (when (timerp define-it--define-timer)
    (cancel-timer define-it--define-timer)
    (setq define-it--define-timer nil)))

(defun define-it--reset-timer ()
  "Reset the timer for searching."
  (define-it--kill-timer)
  (setq define-it--define-timer
        (run-with-timer define-it--update-time nil 'define-it--display-info)))

;;;###autoload
(defun define-it (word)
  "Define by inputing WORD."
  (interactive "MWord: \ni\nP")
  (unless word (user-error "[WARNINGS] Invalid search string: %s" word))
  (setq define-it--current-word word)
  (progn  ; Call all events for receving all info.
    (define-it--get-dictionary-definition-as-string word)
    (define-it--get-google-translate-as-string word)
    (define-it--get-wiki-summary-as-string word))
  (define-it--reset-timer))

;;;###autoload
(defun define-it-at-point ()
  "Use `define-it' to define word at point."
  (interactive)
  (if (use-region-p)
      (define-it (buffer-substring-no-properties (region-beginning) (region-end)))
    (define-it (ignore-errors (substring-no-properties (thing-at-point 'word))))))


(provide 'define-it)
;;; define-it.el ends here
