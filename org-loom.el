;;; org-loom.el --- Virtual table top for solo roleplaying journaling -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Scott Trinh <scott@scotttrinh.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (org "9.0"))
;; Keywords: games, outlines
;; URL: https://github.com/scotttrinh/org-loom

;; This file is not part of GNU Emacs.

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

;; org-loom provides a virtual table top specifically designed for solo
;; roleplaying journaling within Org mode.  It integrates with Org mode's
;; structure to create an immersive and organized environment for solo
;; roleplaying games and narrative journaling.

;; Features (planned):
;; - Character management and tracking
;; - Scene and session organization
;; - Dice rolling and random generators
;; - Campaign and world building tools
;; - Integration with Org mode's capture systems

;;; Code:

(require 'org)

;;; Customization

(defgroup org-loom nil
  "Virtual table top for solo roleplaying journaling."
  :group 'org
  :prefix "org-loom-"
  :link '(url-link "https://github.com/yourusername/org-loom"))

(defcustom org-loom-data-directory
  (expand-file-name "org-loom" user-emacs-directory)
  "Directory to store org-loom data files."
  :type 'directory
  :group 'org-loom)

(defcustom org-loom-default-character-template
  "* Character: %n\n** Stats\n** Background\n** Notes\n"
  "Default template for new characters."
  :type 'string
  :group 'org-loom)

;;; Core Variables

(defvar org-loom-version "0.1.0"
  "Version of org-loom package.")

(defvar org-loom-current-campaign nil
  "Current active campaign.")

(defvar org-loom-current-session nil
  "Current active session.")

;;; Core Functions

;;;###autoload
(defun org-loom-version ()
  "Return the version of org-loom."
  (interactive)
  (message "org-loom version %s" org-loom-version))

;;;###autoload
(defun org-loom-setup ()
  "Set up org-loom in the current Org buffer."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "org-loom can only be used in Org mode buffers"))
  ;; Setup code will go here
  (message "org-loom setup complete"))

;;; Keybindings

(defvar org-loom-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Keybindings will be added here
    map)
  "Keymap for org-loom minor mode.")

;;; Minor Mode

;;;###autoload
(define-minor-mode org-loom-mode
  "Minor mode for org-loom virtual table top.
\\{org-loom-mode-map}"
  :lighter " Loom"
  :keymap org-loom-mode-map
  :group 'org-loom
  (if org-loom-mode
      (org-loom--enable)
    (org-loom--disable)))

(defun org-loom--enable ()
  "Enable org-loom mode."
  ;; Enable functionality will go here
  (message "org-loom mode enabled"))

(defun org-loom--disable ()
  "Disable org-loom mode."
  ;; Disable functionality will go here
  (message "org-loom mode disabled"))

;;; Interactive Commands

;;;###autoload
(defun org-loom-new-campaign ()
  "Create a new campaign."
  (interactive)
  ;; Implementation will go here
  (message "Creating new campaign..."))

;;;###autoload
(defun org-loom-new-session ()
  "Create a new session."
  (interactive)
  ;; Implementation will go here
  (message "Creating new session..."))

;;;###autoload
(defun org-loom-new-character ()
  "Create a new character."
  (interactive)
  ;; Implementation will go here
  (message "Creating new character..."))

;;; Utility Functions

(defun org-loom--ensure-data-directory ()
  "Ensure the org-loom data directory exists."
  (unless (file-exists-p org-loom-data-directory)
    (make-directory org-loom-data-directory t)))

;;; Dice Notation Parsing

(defun org-loom-parse-dice-notation (dice-string)
  "Parse dice notation string and return components.

DICE-STRING should be in format like '2d6', 'd20', '3d8+2', '4dF-1', etc.

Returns an alist with the following keys if parsing succeeds:
  - :count - number of dice (defaults to 1 if not specified)
  - :type - die type (number or string like 'F' for fudge dice)  
  - :modifier - numeric modifier (0 if not specified)
  - :modifier-sign - sign of modifier (+ or -, nil if no modifier)

Returns nil if the string doesn't match dice notation."
  (when (and dice-string (stringp dice-string))
    (let ((dice-regex "^\\([0-9]+\\)?d\\([0-9]+\\|[a-zA-Z]+\\)\\([-+][0-9]+\\)?$"))
      (when (string-match dice-regex dice-string)
        (let* ((count-str (match-string 1 dice-string))
               (type-str (match-string 2 dice-string))
               (modifier-str (match-string 3 dice-string))
               (count (if count-str (string-to-number count-str) 1))
               (type (if (string-match "^[0-9]+$" type-str)
                        (string-to-number type-str)
                      type-str))
               (modifier 0)
               (modifier-sign nil))
          (when modifier-str
            (setq modifier-sign (substring modifier-str 0 1))
            (setq modifier (string-to-number (substring modifier-str 1)))
            (when (string= modifier-sign "-")
              (setq modifier (- modifier))))
          `((:count . ,count)
            (:type . ,type)
            (:modifier . ,modifier)
            (:modifier-sign . ,modifier-sign)))))))

;;; Hooks

(defvar org-loom-mode-hook nil
  "Hook run when org-loom-mode is enabled.")

;;; Footer

(provide 'org-loom)

;;; org-loom.el ends here 