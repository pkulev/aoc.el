;;; aoc.el --- Advent of Code leaderboard viewer for Emacs.

;; Copyright (C) 2021 Pavel Kulyov

;; Author: Pavel Kulyov <kulyov.pavel@gmail.com>
;; Maintainer: Pavel Kulyov <kulyov.pavel@gmail.com>
;; Version: 0.1.0
;; Keywords: convinience
;; URL: https://www.github.com/pkulev/aoc.el.git
;; Package-Requires: (json ht)

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

(defvar aoc--users (aoc--get-data)
  "User data from JSON.")

(defun aoc-private-get-url (year id)
  "Format private URL with YEAR and ID."
  (declare (side-effect-free t))
  (format aoc-private-leaderboard-url year id))

(defun aoc-get-users (data)
  "Return table users from DATA."
  (declare (side-effect-free t))
  (ht-get data "members"))

(defun aoc-list-users (data)
  "Return table users from DATA."
  (declare (side-effect-free t))
  (ht-values (aoc-get-users data)))

(defun aoc-get-user (user-name)
  "Return user data table by USER-NAME."
  (declare (side-effect-free t))
  (second (ht-find (lambda (id user) (string= (downcase (ht-get user "name"))
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
    (cl-sort tasks 'string-lessp :key 'car)))

(defun aoc-user-get-last-star-ts (user)
  "Return last star timestamp table of USER."
  (declare (side-effect-free t))
  (ht-get user "last_star_ts"))

(defun aoc-user--tasks-to-stars (user)
  "Return stars string for USER tasks."
  (declare (side-effect-free t))
  (let* ((sorted-tasks (aoc-user-list-tasks user))
         (tasks (aoc-user-get-tasks user))
         (max-task-num (string-to-number (first (first (last sorted-tasks))))))
    (cl-loop for num in (number-sequence 1 max-task-num)
             collect (aoc--task-get-star-property
                      (ht-get tasks (number-to-string num))))))

(defun aoc-user--get-star-string (user)
  (let ((stars (aoc-user--tasks-to-stars user)))
    ))

(defun aoc-user->vector (user)
  "Convert USER from hash-table to vector."
  (vector (aoc-user-get-local-score user)
          (aoc-user-get-stars user)
          (aoc-user-get-name user)))


;; TODO: replace with getting new JSON via API
(defun aoc--get-data ()
  (let ((json-object-type 'hash-table))
    (json-read-file "resp.json")))

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
                              (t "white"))))))
  (propertize "✭" 'font-lock-face property))

(defvar aoc-private--list-format [("Place" 5)
                                  ("Score" 5)
                                  ("Stars" 40)
                                  ("Name" 40)])

(defvar aoc-private--rows '((nil ["1"
                                  "70"
                                  "**********"
                                  "Creohex"])
                            (nil ["2"
                                  "28"
                                  "******"
                                  "Pavel Kulyov"])
                            (nil ["3"
                                  "18"
                                  "*****"
                                  "Alex Egorov"])
                            (nil ["4"
                                  "7"
                                  "***."
                                  "Имя Фамилия"])))

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
  "AoC private"
  (let ((columns aoc-private--list-format)
        (rows aoc-private--rows))
    (setq tabulated-list-format columns)
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)))


(insert (propertize "test-thing" 'font-lock-face '(:foreground "red")))

(provide 'aoc)

;;; aoc.el ends here
