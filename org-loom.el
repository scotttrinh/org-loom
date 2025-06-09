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

;;; Event Ontology

;;; Core Event Types
;; These are the fundamental event categories that represent state changes

(defconst org-loom-event-types
  '(character-state-changed    ; Character stats, health, resources changed
    relationship-changed       ; NPC relationships, bonds, reputation changed  
    world-state-changed       ; Locations, situations, environmental changes
    campaign-progression      ; Threads, milestones, story arcs
    resource-transaction      ; Items, currency, equipment gained/lost
    mechanical-state-changed  ; Initiative, active effects, temporary modifiers
    advancement-earned)       ; Character advancement, skill increases, new abilities
  "Core event types representing fundamental state changes in the game.

These symbols are used as the primary :event-type in all org-loom events.
Plugins should use these standard types when possible, and may define
subtypes using the :semantic-type field for more specific categorization.")

;;; Semantic Categories  
;; Finer-grained categorization for loose coupling between plugins

(defconst org-loom-semantic-types
  '(;; Character mechanics
    damage-dealt           ; Physical/mental harm inflicted
    healing-received       ; Recovery of health/stress
    skill-test            ; Any kind of ability check
    character-growth      ; Advancement, learning, improvement
    
    ;; Social dynamics
    reputation-shift      ; Standing with groups/individuals changed
    relationship-formed   ; New bonds, enemies, allies
    social-conflict       ; Arguments, negotiations, persuasion
    
    ;; Narrative progression  
    information-discovered ; Clues, secrets, knowledge gained
    mystery-solved        ; Questions answered, puzzles completed
    plot-advanced         ; Story threads progressed
    complication-introduced ; New problems, obstacles, challenges
    
    ;; World interaction
    location-discovered   ; New places explored
    environment-changed   ; World state alterations
    resource-discovered   ; Treasure, items, opportunities found
    
    ;; Mechanical systems
    initiative-changed    ; Turn order, timing modifications
    effect-applied        ; Temporary bonuses, penalties, conditions
    conflict-resolved)    ; Combat, contests, challenges completed
  "Semantic event categories for plugin interoperability.

These symbols provide a standardized vocabulary that allows different
plugins to understand and react to events from other plugins without
tight coupling. Use as :semantic-type in event plists.")

;;; Standard Event Structure
;; All events should follow this basic structure with plugin-specific extensions

(defconst org-loom-event-schema
  '(:event-type          ; Required: symbol from org-loom-event-types
    :semantic-type       ; Optional: symbol from org-loom-semantic-types  
    :source-plugin       ; Required: symbol identifying the plugin that generated this event
    :timestamp          ; Required: ISO timestamp string
    :source-location    ; Optional: org-mode link to where event originated
    
    ;; Standard actor/target roles
    :actor              ; Who performed the action
    :target             ; Who/what was affected 
    :witnesses          ; Who observed (list)
    :affected-parties   ; Additional entities impacted (list)
    
    ;; Standard outcome information
    :outcome            ; Symbol: success, failure, partial-success, success-with-cost
    :magnitude          ; Numeric: how significant the outcome (0-10 scale)
    :consequences       ; List of follow-up effects
    
    ;; Plugin extension space
    ;; Plugins should add their specific data using :plugin-name-data keys
    ;; e.g. :fate-data, :mythic-data, :ironsworn-data)
  "Standard structure for org-loom events.

All events should include the required fields and may include optional
standard fields. Plugins should extend events using :plugin-name-data
keys to maintain loose coupling.")

;;; Event Utility Functions

(defun org-loom-create-event (event-type &rest plist)
  "Create a standardized org-loom event.

EVENT-TYPE should be a symbol from `org-loom-event-types'.
PLIST contains additional event data following the schema in
`org-loom-event-schema'.

Automatically adds :timestamp if not provided."
  (let ((event (copy-sequence plist)))
    (plist-put event :event-type event-type)
    (unless (plist-get event :timestamp)
      (plist-put event :timestamp (format-time-string "%Y-%m-%dT%H:%M:%S")))
    event))

(defun org-loom-event-matches-p (event &rest criteria)
  "Check if EVENT matches all provided CRITERIA.

CRITERIA should be key-value pairs that must all match for the
event to be considered a match. Supports partial matching for lists.

Example: (org-loom-event-matches-p event :event-type 'character-state-changed 
                                         :semantic-type 'damage-dealt)"
  (let ((matches t))
    (while (and criteria matches)
      (let ((key (car criteria))
            (value (cadr criteria)))
        (let ((event-value (plist-get event key)))
          (setq matches (cond
                         ((and (listp event-value) (not (listp value)))
                          ;; Event value is list, criteria is single item - check membership
                          (member value event-value))
                         ((and (listp event-value) (listp value))
                          ;; Both are lists - check intersection
                          (seq-intersection event-value value))
                         (t
                          ;; Simple equality check
                          (equal event-value value)))))
        (setq criteria (cddr criteria))))
    matches))

(defvar org-loom-event-handlers '()
  "List of event handler functions.

Each handler should be a function that accepts an event plist.
Handlers are called in order when events are emitted.")

(defun org-loom-emit-event (event)
  "Emit an EVENT to all registered handlers.

EVENT should be a plist following the org-loom event schema.
All functions in `org-loom-event-handlers' will be called with
the event as their argument."
  (dolist (handler org-loom-event-handlers)
    (condition-case err
        (funcall handler event)
      (error 
       (message "org-loom event handler error: %s" err)))))

(defun org-loom-register-event-handler (handler)
  "Register an event HANDLER function.

HANDLER should be a function that accepts an event plist.
It will be called for all events emitted through `org-loom-emit-event'."
  (unless (member handler org-loom-event-handlers)
    (push handler org-loom-event-handlers)))

(defun org-loom-unregister-event-handler (handler)
  "Unregister an event HANDLER function."
  (setq org-loom-event-handlers (delq handler org-loom-event-handlers)))

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