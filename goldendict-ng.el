;;; goldendict-ng.el --- Search GoldenDict from within Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2023

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/goldendict-ng
;; Package-Requires: ((emacs "24.1"))
;; Version: 0.2.0

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package uses the command line interface provided by `goldendict-ng'
;; (https://xiaoyifang.github.io/goldendict-ng/topic_commandline/) to enable
;; searching GoldenDict dictionaries from within Emacs.

;;; Code:

;;;; User options

(defgroup goldendict-ng ()
  "Basic commands to search GoldenDict dictionaries."
  :group 'files
  :link '(url-link :tag "Homepage" "https://github.com/benthamite/goldendict-ng"))

(defcustom goldendict-ng-executable "goldendict-ng"
  "Path to the `goldendict-ng' executable."
  :group 'goldendict-ng
  :type 'file)

;;;;; Group-related user options

(defcustom goldendict-ng-groups '()
  "Association list of of dictionary groups and their languages.
The first element of the association list is the name of the group and the
second element is the source language of the dictionaries in that group.

Here is an example:

  '((\"English dictionaries\" . \"en\")
    (\"English encyclopedias\" . \"en\")
    (\"French dictionaries\" . \"fr\")
    (\"French encyclopedias\" . \"fr\"))

Note that the group prompt will be bypassed if the list is empty, as it is by
default."
  :group 'goldendict-ng
  :type '(alist :key-type string :value-type string))

(defcustom goldendict-ng-groups-enforce nil
  "Whether to allow only groups listed in `goldendict-ng-groups'.
If non-nil, force the user to select one of the completion candidates.

This user option has no effect if `goldendict-ng-groups' is empty, as it is by
default."
  :group 'goldendict-ng
  :type 'boolean)

(defcustom goldendict-ng-narrow-groups-to-matching-languages nil
  "Whether to narrow the groups to those whose language matches the search string.
If non-nil, restrict the list of groups offered as completion candidates to the
groups in the `goldendict-ng-groups' user option whose language matches one of
the languages detected in the search string.

This user option has no effect if `goldendict-ng-groups' is empty, as it is by
default."
  :group 'goldendict-ng
  :type 'boolean)

(defcustom goldendict-ng-show-all-group t
  "Whether to display the \"All\" group in addition to the user-specified groups.
GoldenDict features a special \"All\" group that, when selected, shows all the
dictionaries in the database. If set to non-nil, display this group next to the
groups defined in `goldendict-ng-groups'.

This user option has no effect if `goldendict-ng-groups' is empty, as it is by
default."
  :group 'goldendict-ng
  :type 'boolean)

(defcustom goldendict-ng-auto-select-sole-candidate nil
  "Whether to automatically select a group when it is the sole candidate."
  :group 'goldendict-ng
  :type 'boolean)

;;;;; Other user options

(defcustom goldendict-ng-use-active-region 'initial-input
  "Whether to use the active region when performing a search.
If set to `bypass-prompt', perform a search using the text in the active region
straight away. If set to `initial-input' or any other non-nil value, use the
text in the active region as the initial input for the search prompt."
  :group 'goldendict-ng
  :type '(choice
	  (const :tag "Do not use the active region" nil)
	  (const :tag "Use the active region, bypassing the search prompt" bypass-prompt)
	  (const :tag "Use the active region as the initial input" initial-input)))

(defcustom goldendict-ng-use-word-at-point 'initial-input
  "Whether to use the thing at point when performing a search.
If set to `bypass-prompt', perform a search using the thing at point straight
away. If set to `initial-input' or any other non-nil value, use the thing at
point as the initial input for the search prompt.

Note that if `goldendict-ng-use-active-region' is non-nil and a region is
active, the settings for that user option will take precedence."
  :group 'goldendict-ng
  :type '(choice
	  (const :tag "Do not use the thing at point" nil)
	  (const :tag "Use the thing at point, bypassing the search prompt" bypass-prompt)
	  (const :tag "Use the thing at point as the initial input" initial-input)))

(defcustom goldendict-ng-main-window nil
  "Whether to force the word to be translated in the main window."
  :group 'goldendict-ng
  :type 'boolean)

(defcustom goldendict-ng-scanpopup nil
  "Whether to Force the word to be translated in scanpopup."
  :group 'goldendict-ng
  :type 'boolean)

(defcustom goldendict-ng-reset-window-state nil
  "Whether to reset the window state."
  :group 'goldendict-ng
  :type 'boolean)

(defcustom goldendict-ng-no-tts nil
  "Whether to disable TTS."
  :group 'goldendict-ng
  :type 'boolean)

;;;;; Obsolete variables

(make-obsolete-variable 'goldendict-ng-groups-prompt nil "0.2.0")
(make-obsolete-variable 'goldendict-ng-narrow-groups-to-matching-langs
			'goldendict-ng-narrow-groups-to-matching-languages
			"0.2.0")
(make-obsolete-variable 'goldendict-ng-initial-input-use-active-region nil "0.2.0")
(make-obsolete-variable 'goldendict-ng-initial-input-use-word-at-point nil "0.2.0")

;;;; Functions

;;;;; Search

;;;###autoload
(defun goldendict-ng-search ()
  "Search GoldenDict."
  (interactive)
  (goldendict-ng-check-executable-exists)
  (cond ((goldendict-ng-bypass-prompt-string-in-region-p)
	 (goldendict-ng-search-string (goldendict-ng-get-string-in-region)))
	((goldendict-ng-bypass-prompt-word-at-point-p)
	 (goldendict-ng-search-string (goldendict-ng-get-word-at-point)))
	(t
	 (goldendict-ng-search-string (goldendict-ng-read-string)))))

(defun goldendict-ng-check-executable-exists ()
  "Signal a user error if the `goldendict-ng' executable is not found."
  (unless (executable-find goldendict-ng-executable)
    (user-error "`goldendict-ng' not found. Please set `goldendict-ng-executable'")))

(defun goldendict-ng-search-string (string)
  "Search GoldenDict for string STRING."
  (goldendict-ng-check-string-nonempty string)
  (let ((command (format "%s %s" goldendict-ng-executable (shell-quote-argument string))))
    (call-process-shell-command (concat command
					(goldendict-ng-set-group-name-flag string)
					(goldendict-ng-set-main-window-flag)
					(goldendict-ng-set-scanpopup-flag)
					(goldendict-ng-set-reset-window-state-flag)
					(goldendict-ng-set-no-tts-flag)
					" &")
				nil 0)))

(defun goldendict-ng-check-string-nonempty (string)
  "Signal a user error if STRING is an empty string."
  (when (string-empty-p string)
    (user-error "Please provide a nonempty search string")))

(defun goldendict-ng-bypass-prompt-string-in-region-p ()
  "Return t iff the prompt should be bypassed with the string in region."
  (and (region-active-p)
       (eq goldendict-ng-use-active-region 'bypass-prompt)))

(defun goldendict-ng-get-string-in-region ()
  "If the region is active, get the string in this region."
  (when (and (region-active-p) goldendict-ng-use-active-region)
    (pcase major-mode
      ('pdf-view-mode
       (car (pdf-view-active-region-text)))
      (_
       (buffer-substring-no-properties
	(region-beginning) (region-end))))))

(defun goldendict-ng-bypass-prompt-word-at-point-p ()
  "Return t iff the prompt should be bypassed with the word at point."
  (and (thing-at-point 'word t)
       (eq goldendict-ng-use-word-at-point 'bypass-prompt)))

(defun goldendict-ng-get-word-at-point ()
  "If point is on a word, return it."
  (when goldendict-ng-use-word-at-point
    (thing-at-point 'word t)))

(defun goldendict-ng-read-string ()
  "Read a string."
  (read-string "Search string: " (goldendict-ng-get-initial-input)))

(defun goldendict-ng-get-initial-input ()
  "Get the default search string."
  (or (goldendict-ng-get-string-in-region)
      (goldendict-ng-get-word-at-point)))

;;;;; Flags

;;;;;; --group-name

(defun goldendict-ng-set-group-name-flag (string)
  "Set the value of the `group-name' flag.
STRING is the search string."
  (if (null goldendict-ng-groups)
      ""
    (let* ((candidates (goldendict-ng-get-group-candidates string))
	   (selection (goldendict-ng-get-group-selection candidates)))
      (format " --group-name %s" (shell-quote-argument selection)))))

(defun goldendict-ng-get-group-candidates (string)
  "Return the groups to be offered as completion candidates for STRING."
  (let ((user-groups
	 (if goldendict-ng-narrow-groups-to-matching-languages
	     (goldendict-ng-get-matching-groups string)
	   (mapcar 'car goldendict-ng-groups))))
    (when goldendict-ng-show-all-group
      (push "All" user-groups))
    user-groups))

(defun goldendict-ng-get-group-selection (candidates)
  "Return the selection for the group CANDIDATES."
  (if (and goldendict-ng-auto-select-sole-candidate (length= candidates 1))
      (car candidates)
    (completing-read "Group: " candidates nil goldendict-ng-groups-enforce)))

;;;;;;; Language detection

(defun goldendict-ng-get-matching-groups (string)
  "Return the groups whose languages are detected in STRING."
  (let ((result '()))
    (dolist (pair goldendict-ng-groups)
      (when (member (cdr pair) (goldendict-ng-get-matching-languages string))
	(push (car pair) result)))
    result))

(defun goldendict-ng-get-matching-languages (string)
  "Return a list of relevant languages detected in STRING.
The languages to be checked against STRING are each of the languages set in
`goldendict-ng-groups', i.e., the list of languages returned by
`goldendict-ng-get-unique-languages'."
  (let (result)
    (dolist (lang (goldendict-ng-get-unique-languages))
      (when (goldendict-ng-string-is-in-language-p string lang)
	(setq result (cons lang result))))
    result))

(defun goldendict-ng-get-unique-languages ()
  "Return a list of unique language values in `goldendict-ng-groups'."
  (delq nil (delete-dups (mapcar 'cdr goldendict-ng-groups))))

(defun goldendict-ng-string-is-in-language-p (string language)
  "Return t iff each word in STRING exists in LANGUAGE."
  (let ((results (mapcar (lambda (word)
			   (goldendict-ng-word-is-in-language-p word language))
			 (split-string string))))
    (not (member nil results))))

(defun goldendict-ng-word-is-in-language-p (word language)
  "Return t iff WORD exists in LANGUAGE."
  (unless (executable-find "aspell")
    (user-error "Language detection requires `GNU Aspell'. Go here to install it:
`http://aspell.net/'"))
  (with-temp-buffer
    (call-process-shell-command
     (format "echo %s | aspell --lang=%s list" (shell-quote-argument word) language) nil t)
    (<= (buffer-size) 1)))

;;;;;; --main-window

(defun goldendict-ng-set-main-window-flag ()
  "Set the value of the `main-window' flag."
  (if goldendict-ng-main-window " --main-window" ""))

;;;;;; --scanpopup

(defun goldendict-ng-set-scanpopup-flag ()
  "Set the value of the `' flag."
  (if goldendict-ng-scanpopup " --scanpopup" ""))

;;;;;; --reset-window-state

(defun goldendict-ng-set-reset-window-state-flag ()
  "Set the value of the `reset-window-state' flag."
  (if goldendict-ng-reset-window-state " --reset-window-state" ""))

;;;;;; --no-tts

(defun goldendict-ng-set-no-tts-flag ()
  "Set the value of the `no-tts' flag."
  (if goldendict-ng-no-tts " --no-tts" ""))

;;;;; Misc

;;;###autoload
(defun goldendict-ng-version ()
  "Print version info."
  (interactive)
  (require 'find-func)
  (let ((emacs-version
	 (with-temp-buffer
	   (insert-file-contents (find-library-name "goldendict-ng"))
	   (let ((contents (buffer-string)))
	     (cond ((string-match ";; Version: \\([^ ;]+\\)" contents)
		    (match-string-no-properties 1 contents))
		   ((string-match "-pkg.el\"[ \f\t\n\r\v]*(([\f\t\n\r\v]*)[ \f\t\n\r\v]*\"\\([^ ;]+\\)\"" contents)
		    (match-string-no-properties 1 contents))))))
	(app-version
	 (shell-command-to-string (format "%s --version" goldendict-ng-executable))))
    (message "goldendict-ng Emacs package version %s\n\n%s" (string-trim emacs-version) app-version)))

;;;;; Obsolete functions

(make-obsolete 'goldendict-ng-set-initial-input nil "0.2.0")
(make-obsolete 'goldendict-ng-group-name-flag nil "0.2.0")
(make-obsolete 'goldendict-ng-no-tts-flag nil "0.2.0")

(provide 'goldendict-ng)

;;; goldendict-ng.el ends here
