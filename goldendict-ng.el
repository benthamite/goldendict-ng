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
  "Groups to offer for completion if `goldendict-ng-group-prompt' is non-nil."
  :group 'goldendict-ng
  :type 'list)

(defcustom goldendict-ng-groups-prompt t
  "Whether to prompt for a group when performing a search."
  :group 'goldendict-ng
  :type 'boolean)

(defcustom goldendict-ng-groups-enforce t
  "Whether to allow only groups listed in `goldendict-ng-groups'."
  :group 'goldendict-ng
  :type 'boolean)

(defcustom goldendict-ng-disable-tts nil
  "Whether to disable TTS."
  :group 'goldendict-ng
  :type 'boolean)

;;;; functions

;;;###autoload
(defun goldendict-ng-search (string)
  "Search GoldenDict for STRING."
  (interactive "sSearch string: ")
  (when (string-empty-p string)
    (error "Please provide a search string"))
  (let ((command (format "%s %s" goldendict-ng-executable (shell-quote-argument string))))
    (call-process-shell-command (concat command
					(goldendict-ng-group-name-flag)
					(goldendict-ng-no-tts-flag)
					" &")
				nil 0)))

(defun goldendict-ng-check-executable-exists ()
  "Signal a user error unless the `goldendict-ng' executable exists."
  (unless (executable-find goldendict-ng-executable)
    (user-error "`goldendict-ng' not found. Please set `goldendict-ng-executable'")))
(defun goldendict-ng-group-name-flag ()
  "Return the `group-name' flag if `goldendict-ng-group-prompt' is non-nil."
  (if goldendict-ng-groups-prompt
      (let* ((require-match (and goldendict-ng-groups-enforce
				 (null goldendict-ng-groups)))
             (group
              (completing-read "Group: " goldendict-ng-groups nil require-match)))
        (format " --group-name %s" (shell-quote-argument group)))
    ""))

(defun goldendict-ng-no-tts-flag ()
  "Return the `no-tts' flag if `goldendict-ng-disable-tts' is non-nil."
  (if goldendict-ng-disable-tts
      " --no-tts"
    ""))

(provide 'goldendict-ng)

;;; goldendict-ng.el ends here
