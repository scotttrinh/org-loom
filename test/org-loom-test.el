;;; org-loom-test.el --- Tests for org-loom -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: Your Name <your.email@example.com>

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

;; Test suite for org-loom using ERT (Emacs Lisp Regression Testing).

;;; Code:

(require 'ert)
(require 'org-loom)

;;; Basic functionality tests

(ert-deftest org-loom-test-version ()
  "Test that org-loom-version returns the correct version."
  (should (string= org-loom-version "0.1.0")))

(ert-deftest org-loom-test-data-directory-customization ()
  "Test that org-loom-data-directory is properly configured."
  (should (stringp org-loom-data-directory))
  (should (file-name-absolute-p org-loom-data-directory)))

(ert-deftest org-loom-test-character-template ()
  "Test that default character template is a string."
  (should (stringp org-loom-default-character-template))
  (should (string-match-p "Character:" org-loom-default-character-template)))

(ert-deftest org-loom-test-mode-map ()
  "Test that org-loom-mode-map is properly defined."
  (should (keymapp org-loom-mode-map)))

;;; Mode tests

(ert-deftest org-loom-test-mode-enable-disable ()
  "Test enabling and disabling org-loom-mode."
  (with-temp-buffer
    (org-mode)
    (org-loom-mode 1)
    (should org-loom-mode)
    (org-loom-mode -1)
    (should-not org-loom-mode)))

;;; Utility function tests

(ert-deftest org-loom-test-ensure-data-directory ()
  "Test that org-loom--ensure-data-directory works correctly."
  (let ((org-loom-data-directory (make-temp-file "org-loom-test" t)))
    (unwind-protect
        (progn
          (delete-directory org-loom-data-directory)
          (should-not (file-exists-p org-loom-data-directory))
          (org-loom--ensure-data-directory)
          (should (file-exists-p org-loom-data-directory))
          (should (file-directory-p org-loom-data-directory)))
      (when (file-exists-p org-loom-data-directory)
        (delete-directory org-loom-data-directory t)))))

;;; Interactive command tests

(ert-deftest org-loom-test-new-campaign-command ()
  "Test that org-loom-new-campaign command exists and is interactive."
  (should (commandp 'org-loom-new-campaign)))

(ert-deftest org-loom-test-new-session-command ()
  "Test that org-loom-new-session command exists and is interactive."
  (should (commandp 'org-loom-new-session)))

(ert-deftest org-loom-test-new-character-command ()
  "Test that org-loom-new-character command exists and is interactive."
  (should (commandp 'org-loom-new-character)))

(ert-deftest org-loom-test-setup-command ()
  "Test that org-loom-setup command exists and is interactive."
  (should (commandp 'org-loom-setup)))

(provide 'org-loom-test)

;;; org-loom-test.el ends here 