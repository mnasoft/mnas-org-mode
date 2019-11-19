;;;; mnas-org-mode.lisp

(in-package #:mnas-org-mode)

(defparameter *hl-sample* "[[D:/home/_namatv/_WorkPlan/2019/32/2019-01-12_082508.trd][2019-01-12 08:25:08.trd]]")
(defparameter *l-sample* "D:/home/_namatv/_WorkPlan/2019/32/2019-01-12_082508.trd")
(defparameter *d-sample* "2019-01-12 08:25:08.trd")

(defun hiper-link->link (h-link)
  "Возвращает ссылочную часть ссылки, заданной в формате org-mode.
Пример использования:
Пример использования:
 (mnas-org-mode:hiper-link->link mnas-org-mode::*hl-sample*) => \"D:/home/_namatv/_WorkPlan/2019/32/2019-01-12_082508.trd\"
"
  (declare ((or string) h-link))
  (string-trim "[]" (cl-ppcre:scan-to-strings "^\\[\\[.*\\]\\[" h-link)))

(defun hiper-link->description (h-link)
  "Возвращает описательную часть ссылки, заданной в формате org-mode.
Пример использования:
 (mnas-org-mode:hiper-link->description mnas-org-mode::*hl-sample*) => \"2019-01-12 08:25:08.trd\"
"
  (declare ((or string) h-link))
  (string-trim "[]" (cl-ppcre:scan-to-strings "\\]\\[.*\\]\\]$" h-link)))

(defun make-hiper-link (link description)
  "Формирует ссылку в формате org-mode.
Пример использования:
 (mnas-org-mode::make-hiper-link mnas-org-mode::*l-sample* mnas-org-mode::*d-sample*)=> \"[[D:/home/_namatv/_WorkPlan/2019/32/2019-01-12_082508.trd][2019-01-12 08:25:08.trd]]\"
"
  (declare ((or string) link description))
  (format nil "[[~A][~A]]" link description))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *date-sample*          "<2019-01-12 Сб>")
(defparameter *date-sample-wrong-01* "<2018-02-15>")
(defparameter *date-sample-wrong-02* "<2017-03-18>")
(defparameter *time-sample*          "08:59:35")

(defun date->date (org-date)
  "Выполняет преобразование даты в формате org-mode в список чисел.
Примеры использования:
 (date->date *date-sample*)          -> (2019 1 12)
 (date->date *date-sample-wrong-01*) -> (2018 2 15)
 (date->date *date-sample-wrong-02*) -> (2017 3 18)
"
  (declare ((or string) org-date))
  (let* ((s-lst (cdr (cl-ppcre:split "^<|-| |>$" org-date)))
	 (year  (parse-integer (first s-lst)))
	 (month (parse-integer (second s-lst)))
         (day   (parse-integer (third s-lst))))
    (list year month day)))

(defun time->time (org-time)
  "Выполняет преобразование времени в формате org-mode в список чисел.
Пример использования:
 (time->time *time-sample*) => (8 59 35)"
  (declare ((or string) org-time))
  (mapcar #'parse-integer (cl-ppcre:split ":" org-time)))

(defun date-time->utime (date time)
  "Возвращает ссылочную часть ссылки, заданной в формате org-mode.
Пример использования:
  (date-time->utime *date-sample* *time-sample*) => 3756265175
"
  ;; (date-time->utime "<2019-02-07 Чт>" "09:34:30")
  ;; (date-time->utime "<2019-02-07>"    "10:35:39")
  ;; (date->date "<2019-02-07>")
  (declare ((or string) date time))
  (append (date->date date) (time->time time))
  (apply #'encode-universal-time (nreverse (append (date->date date) (time->time time)))))

;;;;

(defparameter *day-of-week*    '((0 "Пн") (1 "Вт") (2 "Ср") (3 "Чт") (4 "Пт") (5 "Сб") (6 "Вс")))


(defparameter *day-of-week-en* '((0 "Mo") (1 "Tu") (2 "We") (3 "Th") (4 "Fr") (5 "Sa") (6 "Su")))
(defparameter *day-of-week-ru* '((0 "Пн") (1 "Вт") (2 "Ср") (3 "Чт") (4 "Пт") (5 "Сб") (6 "Вс")))
(defparameter *day-of-week-ua* '((0 "Пн") (1 "Вт") (2 "Ср") (3 "Чт") (4 "Пт") (5 "Сб") (6 "Нд")))

(defun day-of-week (numder) (cadr (assoc numder *day-of-week*)))

(defun utime->date (utime)
  "Преобразует универсальное время в формат даты org-mode.
Пример использования:
 (decode-universal-time 3756265175)
 (utime->date 3756265175) => \"<2019-1-12 Сб>\"
 (utime->date 3756265175) 
"
  (declare ((or integer) utime))
  (multiple-value-bind (ss mm hh dd mon yy w-day) (decode-universal-time utime)
      (declare (ignore  ss mm hh ))
      (format nil "<~4,'0D-~2,'0D-~2,'0D ~A>" yy mon dd (day-of-week w-day))))

(defun utime->time (utime)
  "Преобразует универсальное время в формат времени (не совсем org-mode).
Пример использования:
 (utime->time 3756265175) => \"08:59:35\"
"
  (declare ((or integer) utime))
  (multiple-value-bind (ss mm hh dd mon yy w-day) (decode-universal-time utime)
    (declare (ignore  dd mon yy w-day ))
    (format nil "~2,'0D:~2,'0D:~2,'0D" hh mm ss)))

(export 'table-to-org)
(defun table-to-org (table &optional (stream t))
  "Экспотр таблицы в формат Org"
  (format stream "~{~{|~S~}|~%~}" table))


