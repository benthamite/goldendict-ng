;;; goldendict-ng.el --- Search GoldenDict from within Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2023

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/goldendict-ng
;; Package-Requires: ((emacs "24.1"))
;; Version: 0.1

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

;; This package uses the command line interface provided by `goldendict-ng' (https://xiaoyifang.github.io/goldendict-ng/topic_commandline/) to enable searching GoldenDict dictionaries from within Emacs.

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

(defcustom goldendict-ng-groups '()
  "Association list of of dictionary groups and their languages.
The first element of the association list is the name of the group and the
second element is the language of the dictionaries in that group.

Here is an example:

  '((\"English dictionaries\" . \"en\")
    (\"English encyclopedias\" . \"en\")
    (\"French dictionaries\" . \"fr\")
    (\"French encyclopedias\" . \"fr\"))

Note that the group prompt will be bypassed if the list is empty, as it is by
default."
  :group 'goldendict-ng
  :type '(alist :key-type string :value-type string))

(make-obsolete-variable 'goldendict-ng-groups-prompt nil "0.2.0")

(defcustom goldendict-ng-groups-enforce nil
  "Whether to allow only groups listed in `goldendict-ng-groups'.
If non-nil, force the user to select one of the completion candidates.

This user option has no effect if `goldendict-ng-groups' is empty, as it is by
default."
  :group 'goldendict-ng
  :type 'boolean)

(defcustom goldendict-ng-narrow-groups-to-matching-langs nil
  "Whether to narrow the groups to those whose language matches the search string.
If non-nil, restrict the list of groups offered as completion candidates to the
groups in the `goldendict-ng-groups' user option whose `:language' property
matches one of the languages detected in the search string.

This user option has no effect if `goldendict-ng-groups' is empty, as it is by
default."
  :group 'goldendict-ng
  :type 'boolean)

(defcustom goldendict-ng-show-all-group t
  "Whether to display the \"All\" group in addition to the user-specified groups.
GoldenDict features a speciall \"All\" group that, when selected, shows all the
dictionaries in the database. If set to non-nil, display this group next to the
groups defined in `goldendict-ng-groups'.

This user option has no effect if `goldendict-ng-groups' is empty, as it is by
default."
  :group 'goldendict-ng
  :type 'boolean)

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

(make-obsolete-variable 'goldendict-ng-initial-input-use-active-region nil "0.2.0")

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

(make-obsolete-variable 'goldendict-ng-initial-input-use-word-at-point nil "0.2.0")

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

;;;; functions

;;;;; main

;;;###autoload
(defun goldendict-ng-search ()
  "Search GoldenDict."
  (interactive)
  (cond ((goldendict-ng-bypass-prompt-string-in-region-p)
	 (goldendict-ng-search-string (goldendict-ng-get-string-in-region)))
	((goldendict-ng-bypass-prompt-word-at-point-p)
	 (goldendict-ng-search-string (goldendict-ng-get-word-at-point)))
	(t
	 (goldendict-ng-search-string
	  (read-string "Search string: " (goldendict-ng-get-initial-input))))))

(make-obsolete 'goldendict-ng-set-initial-input nil "0.2.0")

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

(defun goldendict-ng-bypass-prompt-string-in-region-p ()
  "Return t iff the prompt should be bypassed with the string in region."
  (and (region-active-p)
       (eq goldendict-ng-use-active-region 'bypass-prompt)))

(defun goldendict-ng-bypass-prompt-word-at-point-p ()
  "Return t iff the prompt should be bypassed with the word at point."
  (and (thing-at-point 'word t)
       (eq goldendict-ng-use-word-at-point 'bypass-prompt)))

(defun goldendict-ng-get-string-in-region ()
  "If the region is active, get the string in this region."
  (when (and (region-active-p) goldendict-ng-use-active-region)
    (buffer-substring-no-properties
     (region-beginning) (region-end))))

(defun goldendict-ng-get-word-at-point ()
  "If point is on a word, return it."
  (when goldendict-ng-use-word-at-point
    (thing-at-point 'word t)))

(defun goldendict-ng-get-initial-input ()
  "Get the default search string."
  (goldendict-ng-check-executable-exists)
  (or (goldendict-ng-get-string-in-region)
      (goldendict-ng-get-word-at-point)))

(defun goldendict-ng-check-executable-exists ()
  "Signal a user error unless the `goldendict-ng' executable is found."
  (unless (executable-find goldendict-ng-executable)
    (user-error "`goldendict-ng' not found. Please set `goldendict-ng-executable'")))

(defun goldendict-ng-check-string-nonempty (string)
  "Signal a user error if STRING is an empty string."
  (when (string-empty-p string)
    (user-error "Please provide a search string")))


(provide 'goldendict-ng)

;;; goldendict-ng.el ends here
