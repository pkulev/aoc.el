;;; aoc.el --- Advent of Code leaderboard viewer

;; Copyright (C) 2021 Pavel Kulyov

;; Author: Pavel Kulyov <kulyov.pavel@gmail.com>
;; Maintainer: Pavel Kulyov <kulyov.pavel@gmail.com>
;; Version: 0.1.0
;; Keywords: convenience
;; URL: https://www.github.com/pkulev/aoc.el.git
;; Package-Requires: ((emacs "24.3") (json "1.4") (ht "2.4"))

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

(defcustom aoc-private-leaderboard-id
  nil
  "Private leaderboard ID."
  :group 'aoc
  :type 'string)

(defcustom aoc-private-leaderboard-year
  2021
  "Private leaderboard year."
  :group 'aoc
  :type 'int)

(defcustom aoc-user-session-id
  nil
  "AoC session ID.
Use browser's devtools to get it from cookies."
  :group 'aoc
  :type 'string)


(defvar aoc-private--list-format [("Place" 5)
                                  ("Score" 5)
                                  ("Stars" 40)
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
  (let ((today (decode-time (current-time))))
    (list (decoded-time-day today)
          (decoded-time-month today)
          (decoded-time-year today))))

(defun aoc-private-get-url (year id)
  "Format private URL with YEAR and ID."
  (declare (side-effect-free t))
  (format aoc-private-leaderboard-url year id))

(defun aoc-private-get-url-current ()
  "Format private URL with current YEAR and ID."
  (declare (side-effect-free t))
  (format aoc-private-leaderboard-url
          aoc-private-leaderboard-year
          aoc-private-leaderboard-id))

(defun aoc-private--get-curl-command ()
  (format "curl -s --cookie 'session=%s' '%s'"
          aoc-user-session-id
          (aoc-private-get-url-current)))

(defun aoc-private-get-data ()
  (let ((json-object-type 'hash-table)
        (raw (shell-command-to-string (aoc-private--get-curl-command))))
    (json-read-from-string raw)))

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
  (cl-second (ht-find (lambda (id user) (string= (downcase (ht-get user "name"))
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
  (let* ((sorted-tasks (aoc-user-list-tasks user))
         (tasks (aoc-user-get-tasks user))
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

;; TODO: what year? Hardcoded? Choosed from table?
(defun aoc-task-current-max ()
  "Return max available tasks for the year."
  (seq-let (day month year) (today)
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
                        (cond ((aoc-task-silver? task) "silver")
                              ((aoc-task-gold? task) "gold")
                              (t (face-attribute 'shadow :foreground))))))
    (propertize (if (char-displayable-p ?★)"★" "*")
                'font-lock-face
                property)))

(defun aoc-private--get-rows ()
  "Return rows for tabulated list."
  (cl-loop for user in (aoc-list-users aoc--users)
      with idx = 1
      collect (list nil (vconcat (vector (number-to-string idx)) (aoc-user->vector user)))
      do (setq idx (1+ idx))))

(defun aoc-private--update-rows ()
  "Update private leaderboard tabulated list rows."
  (setq aoc-private--rows (aoc-private--get-rows)))


;;;###autoload
(defun aoc-private-list-board ()
  (interactive)
  (switch-to-buffer "*Private leaderboard*")
  (aoc-private-board-mode))

(defvar aoc-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Major mode key map.")

(define-derived-mode aoc-private-board-mode tabulated-list-mode
  "aoc:pb"
  (setq aoc--users (aoc-private-get-data))
  (aoc-private--update-rows)
  (let ((columns aoc-private--list-format)
        (rows aoc-private--rows))
    (setq tabulated-list-format columns)
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)))


(provide 'aoc)

;;; aoc.el ends here
