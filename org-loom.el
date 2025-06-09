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

(defcustom org-loom-custom-dice-types
  '(("F" . (:sides (-1 -1 0 0 1 1) :combiner org-loom--sum-numbers))
    ("MythicAdjective" . (:sides ("Scary" "Dry" "Cold" "Hot" "Warm" "Bright" 
                                  "Dark" "Loud" "Quiet" "Sharp" "Dull" "Fast"
                                  "Slow" "Big" "Small" "Heavy" "Light")
                          :combiner org-loom--join-strings)))
  "Custom dice type definitions.

Each entry is of the form (TYPE . DEFINITION) where:
- TYPE is a string identifying the dice type (e.g. \"F\" for Fudge dice)
- DEFINITION is a plist with:
  - :sides - list of possible values for each side of the die
  - :combiner - function to combine multiple roll results

The combiner function should accept a list of individual roll results
and return a combined result. Built-in combiners include:
- org-loom--sum-numbers (numeric addition)
- org-loom--join-strings (string concatenation with spaces)

Example custom dice:
- Fudge dice: \"F\" with sides (-1 -1 0 0 1 1) and combiner org-loom--sum-numbers
- Oracle dice: \"MythicAdjective\" with word sides and string combiner"
  :type '(alist :key-type string
                :value-type (plist :key-type symbol 
                                   :value-type (choice list function)))
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

(defun org-loom--join-strings (string-list)
  "Join a list of strings with spaces.
This is a built-in combiner function for custom dice that return strings."
  (mapconcat #'identity string-list " "))

(defun org-loom--sum-numbers (number-list)
  "Sum a list of numbers.
This is a built-in combiner function for custom dice that return numbers."
  (apply #'+ number-list))

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

(defun org-loom-roll-dice (dice-alist)
  "Roll dice based on parsed dice notation alist.

DICE-ALIST should be the output from `org-loom-parse-dice-notation'.

Returns an alist with the following keys if rolling succeeds:
  - :rolls - list of individual die roll results
  - :modifier - the modifier value applied (0 if none, or nil for custom dice)
  - :total - combined result (sum for numeric, combined value for custom dice)

For numeric dice, :total is the sum of rolls plus modifier.
For custom dice, :total is the result of applying the dice's combiner function
to the roll results, and :modifier will be nil.

Returns nil if the dice cannot be rolled (e.g., unknown custom dice type,
invalid count, etc.)."
  (when (and dice-alist
             (listp dice-alist))
    (let ((count (alist-get :count dice-alist))
          (type (alist-get :type dice-alist))
          (modifier (alist-get :modifier dice-alist)))
      (cond
       ;; Numeric dice
       ((and count type modifier
             (numberp count)
             (numberp type)
             (numberp modifier)
             (> count 0)
             (> type 0))
        (let ((rolls '())
              (roll-sum 0))
          ;; Roll each die
          (dotimes (_ count)
            (let ((roll (1+ (random type))))
              (push roll rolls)
              (setq roll-sum (+ roll-sum roll))))
          ;; Reverse rolls to maintain order
          (setq rolls (nreverse rolls))
          ;; Calculate total with modifier
          (let ((total (+ roll-sum modifier)))
            `((:rolls . ,rolls)
              (:modifier . ,modifier)
              (:total . ,total)))))
       
       ;; Custom dice
       ((and count type
             (numberp count)
             (stringp type)
             (> count 0))
        (let ((custom-def (alist-get type org-loom-custom-dice-types nil nil #'string=)))
          (when custom-def
            (let ((sides (plist-get custom-def :sides))
                  (combiner (plist-get custom-def :combiner)))
              (when (and sides combiner (> (length sides) 0))
                (let ((rolls '()))
                  ;; Roll each die
                  (dotimes (_ count)
                    (let ((roll (nth (random (length sides)) sides)))
                      (push roll rolls)))
                  ;; Reverse rolls to maintain order
                  (setq rolls (nreverse rolls))
                  ;; Combine results using the dice's combiner
                  (let ((total (funcall combiner rolls)))
                    `((:rolls . ,rolls)
                      (:modifier . nil)
                      (:total . ,total)))))))))))))

(defun org-loom-roll-dice-notation (dice-string)
  "Parse and roll dice from dice notation string.

DICE-STRING should be in dice notation format like '2d6+3'.

This is a convenience function that combines `org-loom-parse-dice-notation'
and `org-loom-roll-dice'.

Returns the same format as `org-loom-roll-dice' or nil if parsing
or rolling fails."
  (when-let ((parsed (org-loom-parse-dice-notation dice-string)))
    (org-loom-roll-dice parsed)))

;;; Hooks

(defvar org-loom-mode-hook nil
  "Hook run when org-loom-mode is enabled.")

;;; Footer

(provide 'org-loom)

;;; org-loom.el ends here 