(require 'ert)
(require 'ht)

(cl-defun ksort (seq pred &key key)
  (when (null key)
    (set key #'identity))
  (sort seq (lambda (a b)
              (funcall #'pred
                       (funcall #'key a)
                       (funcall #'key b)))))


(defvar test-aoc-private--mocked-data
  (ht ("event" "2020")
      ("owner_id" "64430")
      ("members" (ht ("64430" (ht ("name" "Creohex")
                                  ("stars" 20)
                                  ("local_score" 70)))
                     ("819754" (ht ("name" "Pavel Kulyov")
                                   ("stars" 12)
                                   ("local_score" 28)))))))

(ert-deftest test-aoc-private-get-url ()
  (should (equal (aoc-private-get-url 2020 64430)
                 "https://adventofcode.com/2020/leaderboard/private/view/64430.json")))

(ert-deftest test-aoc-get-users ()
  (let ((expected (list (ht ("name" "Creohex")
                            ("stars" 20)
                            ("local_score" 70))
                        (ht ("name" "Pavel Kulyov")
                            ("stars" 12)
                            ("local_score" 28))))
        (result (aoc-get-users test-aoc-private--mocked-data)))
    (should (cl-every #'identity
                      (map 'list #'ht-equal? expected result)))))

(ert-deftest test-aoc-user->vector ()
  (should (equal (aoc-user->vector (ht ("name" "Creohex")
                                       ("stars" 20)
                                       ("local_score" 70)))
                 [70 20 "Creohex"])))

(ert-deftest test-aoc-task-gold? ()
  (should ( (aoc-task-gold? (gt ("1" (ht ("get_star_ts" "1607422713"))
                                 "2" (ht ("get_star_ts" "1607422750"))))))))
