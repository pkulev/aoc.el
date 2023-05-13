;;; aoc.el --- Advent of Code leaderboard viewer -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Pavel Kulyov

;; Author: Pavel Kulyov <kulyov.pavel@gmail.com>
;; Maintainer: Pavel Kulyov <kulyov.pavel@gmail.com>
;; Version: 0.4.0
;; Keywords: convenience
;; URL: https://www.github.com/pkulev/aoc.el.git
;; Package-Requires: ((emacs "25.1") (ht "2.4"))

;; This file is NOT part of GNU/Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Advent of Code leaderboard viewer for Emacs.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'ht)

(defgroup aoc nil
  "Advent of Code leaderboard viewer."
  :prefix "aoc-"
  :group 'tools)

(defcustom aoc-private-request-interval
  900
  "Request interval (author asks to keep 15min interval)."
  :group 'aoc
  :type 'integer)

(defcustom aoc-private-leaderboard-url
  "https://adventofcode.com/%s/leaderboard/private/view/%s.json"
  "Private leaderboard URL template."
  :group 'aoc
  :type 'string)

(defcustom aoc-private-leaderboard-ids
  nil
  "Private leaderboard IDs."
  :group 'aoc
  :safe t
  :type '(repeat string))

(defvar-local aoc-private--leaderboard-id nil
  "Private leaderboard ID.")

(defcustom aoc-user-session-id
  nil
  "AoC session ID.
Use browser's devtools to get it from cookies."
  :group 'aoc
  :local t
  :safe t
  :type 'string)

(defvar aoc-private--buffer-name "*aoc::pb::%s*"
  "Private leaderboard buffer name template.")

(defvar aoc-private--list-format [("Place" 5)
                                  ("Score" 5)
                                  ("Stars" 25)
                                  ("Name" 40)]
  "Tabulated list header.")

(defvar aoc-private--rows nil
  "Tabulated list contents.")

(defvar aoc-private--timer nil
  "AoC private leaderboard update timer object.")

(defvar aoc-task-max 25
  "Number of tasks, hope it won't change.")

(defun aoc--today ()
  "Return today date in form of (day month year)."
  (seq-let (_ _ _ d m y) (decode-time (current-time))
    (list d m y)))

(defun aoc--last-event-year ()
  "Return the year of the last active event.

This includes current year if it's already a December."
  (seq-let (_ month year) (aoc--today)
    (if (= month 12)
        year
      (1- year))))

(defcustom aoc-private-leaderboard-year
  (aoc--last-event-year)
  "Private leaderboard year.

Automatically set to the year of the last active event on
initialization."
  :group 'aoc
  :local t
  :safe t
  :type 'integer)

(defun aoc-private-get-url (year id)
  "Format private URL with YEAR and ID."
  (declare (side-effect-free t))
  (format aoc-private-leaderboard-url year id))

(defun aoc-private-get-url-current ()
  "Format private URL with current YEAR and ID."
  (declare (side-effect-free t))
  (format aoc-private-leaderboard-url
          aoc-private-leaderboard-year
          aoc-private--leaderboard-id))

(defun aoc-private--get-curl-command ()
  "Construct default CURL command with cookies set."
  (format "curl -s --cookie 'session=%s' '%s'"
          aoc-user-session-id
          (aoc-private-get-url-current)))

(defun aoc-private-get-data ()
  "Get private leaderboard data."
  (let ((json-object-type 'hash-table)
        (raw (shell-command-to-string (aoc-private--get-curl-command))))
    (json-read-from-string raw)))

(defun aoc-private-get-buffer-name (year)
  "Return private leaderboard buffer name for the provided YEAR."
  (format aoc-private--buffer-name year))

(defvar aoc--users nil
  "User data from JSON.")

(defun aoc-get-users (data)
  "Return table users from DATA."
  (declare (side-effect-free t))
  (ht-get data "members"))

(defun aoc-list-users (data)
  "Return table users from DATA sorted descending by score."
  (declare (side-effect-free t))
  (cl-sort (ht-values (aoc-get-users data))
           '>
           :key 'aoc-user-get-local-score))

(defun aoc-get-user (user-name)
  "Return user data table by USER-NAME."
  (declare (side-effect-free t))
  (cl-second (ht-find (lambda (_ user) (string= (downcase (ht-get user "name"))
                                                (downcase user-name)))
                      (aoc-get-users aoc--users))))

(defun aoc-user-get-id (user)
  "Return ID of USER."
  (declare (side-effect-free t))
  (ht-get user "id"))

(defun aoc-user-get-name (user)
  "Return name of USER."
  (declare (side-effect-free t))
  (ht-get user "name"))

(defun aoc-user-get-stars (user)
  "Return stars amount of USER."
  (declare (side-effect-free t))
  (ht-get user "stars"))

(defun aoc-user-get-local-score (user)
  "Return local score of USER."
  (declare (side-effect-free t))
  (ht-get user "local_score"))

(defun aoc-user-get-global-score (user)
  "Return global score of USER."
  (declare (side-effect-free t))
  (ht-get user "global_score"))

(defun aoc-user-get-tasks (user)
  "Return completion-day-level (tasks info) table of USER."
  (declare (side-effect-free t))
  (ht-get user "completion_day_level"))

(defun aoc-user-list-tasks (user)
  "Return list of USER tasks sorted by task number."
  (declare (side-effect-free t))
  (let ((tasks (ht->alist (aoc-user-get-tasks user))))
    (cl-sort tasks 'string-lessp :key 'cl-first)))

(defun aoc-user-get-last-star-ts (user)
  "Return last star timestamp table of USER."
  (declare (side-effect-free t))
  (ht-get user "last_star_ts"))

(defun aoc-user--tasks->stars (user)
  "Return stars string for USER tasks."
  (declare (side-effect-free t))
  (let ((tasks (aoc-user-get-tasks user))
        (max-task-num (aoc-task-current-max)))
    (cl-loop for num in (number-sequence 1 max-task-num)
             collect (aoc--task-propertize-star
                      (ht-get tasks (number-to-string num) (ht))))))

(defun aoc-user--get-star-string (user)
  "Return propertized star string for USER."
  (declare (side-effect-free t))
  (apply 'concat (aoc-user--tasks->stars user)))

(defun aoc-user->vector (user)
  "Convert USER from hash-table to vector."
  (vector (number-to-string (or (aoc-user-get-local-score user) 0))
          (aoc-user--get-star-string user)
          (aoc-user-get-name user)))

(defun aoc-task-current-max ()
  "Return max available tasks for the year."
  (seq-let (day month year) (aoc--today)
    (cond ((and (= month 12)
                (= year aoc-private-leaderboard-year))
           day)
          ((< aoc-private-leaderboard-year year)
           aoc-task-max))))

(defun aoc-task-gold? (task)
  "Return whether both parts of TASK were solved."
  (declare (side-effect-free t))
  (and (ht-get task "1")
       (ht-get task "2")))

(defun aoc-task-silver? (task)
  "Return whether only first part of TASK was solved."
  (declare (side-effect-free t))
  (and (ht-get task "1")
       (not (ht-get task "2"))))

(defun aoc--task-propertize-star (task)
  "Return propertized star for TASK, silver for 1, gold for 1 + 2."
  (let ((property (list :foreground
                        (cond ((aoc-task-silver? task) "Light steel blue")
                              ((aoc-task-gold? task) "gold")
                              (t "dim grey")))))
    (propertize "*" 'font-lock-face property)))

(defun aoc-private--get-rows ()
  "Return rows for tabulated list."
  (append '((nil ["" "" "         1111111111222222" ""])
            (nil ["" "" "1234567890123456789012345" ""]))
          (cl-loop for user in (aoc-list-users aoc--users)
                   with idx = 1
                   collect (list nil
                                 (vconcat (vector (number-to-string idx))
                                          (aoc-user->vector user)))
                   do (setq idx (1+ idx)))))

(defun aoc-private--refresh ()
  "Refresh data and recompute table contents."
  (setq aoc--users (aoc-private-get-data)
        tabulated-list-entries (aoc-private--get-rows)))

(defun aoc-private-board-next-year ()
  "Switch current board to the next year."
  (interactive)
  (let ((current-year (cl-third (aoc--today)))
        (year (1+ aoc-private-leaderboard-year)))
    (when (> year current-year)
      (error "No information about the future event"))
    (setq aoc-private-leaderboard-year year)
    (rename-buffer (aoc-private-get-buffer-name year))
    (revert-buffer)))

(defun aoc-private-board-prev-year ()
  "Switch current board to the previous year."
  (interactive)
  (let ((year (1- aoc-private-leaderboard-year)))
    (when (< year 2015)
      (error "No events before 2015"))
    (setq aoc-private-leaderboard-year year)
    (rename-buffer (aoc-private-get-buffer-name year))
    (revert-buffer)))

;;;###autoload
(defun aoc-private-list-board ()
  "Show AoC private board."
  (interactive)
  (let ((buffer (get-buffer-create (aoc-private-get-buffer-name
                                    aoc-private-leaderboard-year))))
    (with-current-buffer buffer
      (aoc-private-board-mode)
      (setq aoc-private--leaderboard-id
            (completing-read "Leaderboard ID: " aoc-private-leaderboard-ids))
      (aoc-private--refresh)
      (tabulated-list-print))
    (pop-to-buffer buffer)))

(defvar aoc-private-board-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "b" 'aoc-private-board-prev-year)
    (define-key map "f" 'aoc-private-board-next-year)
    map)
  "Major mode key map.")

(define-derived-mode aoc-private-board-mode tabulated-list-mode
  "aoc:pb"
  (setq tabulated-list-format aoc-private--list-format)
  (add-hook 'tabulated-list-revert-hook 'aoc-private--refresh)
  (tabulated-list-init-header))

(provide 'aoc)

;;; aoc.el ends here
