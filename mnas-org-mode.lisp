;;;; mnas-org-mode.lisp

(in-package #:mnas-org-mode)

(annot:enable-annot-syntax)

(defparameter *hl-sample* "[[D:/home/_namatv/_WorkPlan/2019/32/2019-01-12_082508.trd][2019-01-12 08:25:08.trd]]"
  "Пример гиперссылки в формате org.")

(defparameter *l-sample* "D:/home/_namatv/_WorkPlan/2019/32/2019-01-12_082508.trd"
    "Пример пути к файлу тренда.")

(defparameter *d-sample* "2019-01-12 08:25:08.trd"
  "Пример даты и времени.")

(defparameter *date-sample*          "<2019-01-12 Сб>"
      "Пример даты.")

(defparameter *date-sample-wrong-01* "<2018-02-15>"
      "Пример даты, содержащий ошибку (отсутстсует день недели).")

(defparameter *date-sample-wrong-02* "<2017-03-18>"
    "Пример даты, содержащий ошибку (отсутстсует день недели).")

(defparameter *time-sample*          "08:59:35"
  "Пример времени.")

@export
@annot.doc:doc
"@b(Описание:) hiper-link->link возвращает ссылочную часть ссылки, заданной в формате org-mode.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (mnas-org-mode:hiper-link->link mnas-org-mode::*hl-sample*) => \"D:/home/_namatv/_WorkPlan/2019/32/2019-01-12_082508.trd\"
@end(code)
"
(defun hiper-link->link (h-link)
  (declare ((or string) h-link))
  (string-trim "[]" (cl-ppcre:scan-to-strings "^\\[\\[.*\\]\\[" h-link)))

@export
@annot.doc:doc
"Возвращает описательную часть ссылки, заданной в формате org-mode.
Пример использования:
 (mnas-org-mode:hiper-link->description mnas-org-mode::*hl-sample*) => \"2019-01-12 08:25:08.trd\"
"
(defun hiper-link->description (h-link)
  (declare ((or string) h-link))
  (string-trim "[]" (cl-ppcre:scan-to-strings "\\]\\[.*\\]\\]$" h-link)))

@export
@annot.doc:doc
"Формирует ссылку в формате org-mode.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (make-hiper-link *l-sample* *d-sample*) => 
 \"[[D:/home/_namatv/_WorkPlan/2019/32/2019-01-12_082508.trd][2019-01-12 08:25:08.trd]]\"
@end(code)
"
(defun make-hiper-link (link description)
  (declare ((or string) link description))
  (format nil "[[~A][~A]]" link description))

@export
@annot.doc:doc
"@b(Описание:) date->date выполняет преобразование даты в 
формате org-mode в список чисел.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (date->date *date-sample*)          -> (2019 1 12)
 (date->date *date-sample-wrong-01*) -> (2018 2 15)
 (date->date *date-sample-wrong-02*) -> (2017 3 18)
@end(code)
"
(defun date->date (org-date)
  (declare ((or string) org-date))
  (let* ((s-lst (cdr (cl-ppcre:split "^<|-| |>$" org-date)))
	 (year  (parse-integer (first s-lst)))
	 (month (parse-integer (second s-lst)))
         (day   (parse-integer (third s-lst))))
    (list year month day)))

@export
(defun time->time (org-time)
  "Выполняет преобразование времени в формате org-mode в список чисел.
Пример использования:
 (time->time *time-sample*) => (8 59 35)"
  (declare ((or string) org-time))
  (mapcar #'parse-integer (cl-ppcre:split ":" org-time)))

@export
@annot.doc:doc
  "@b(Описание:) date-time->utime возвращает ссылочную часть ссылки, 
заданной в формате org-mode.

 @b(Пример использования:)
@begin[lang=lisp](code)
  (date-time->utime *date-sample* *time-sample*) => 3756265175
  (date-time->utime \"<2019-02-07 Чт>\" \"09:34:30\")
  (date-time->utime \"<2019-02-07>\"    \"10:35:39\")
  (date->date \"<2019-02-07>\")
@end(code)
"
(defun date-time->utime (date time)
  (declare ((or string) date time))
  (append (date->date date) (time->time time))
  (apply #'encode-universal-time (nreverse (append (date->date date) (time->time time)))))

(defparameter *day-of-week-en* '((0 "Mo") (1 "Tu") (2 "We") (3 "Th") (4 "Fr") (5 "Sa") (6 "Su"))
  "Короткие наименования дней недели на английском языке.")

(defparameter *day-of-week-ru* '((0 "Пн") (1 "Вт") (2 "Ср") (3 "Чт") (4 "Пт") (5 "Сб") (6 "Вс"))
  "Короткие наименования дней недели на русском языке.")

(defparameter *day-of-week-ua* '((0 "Пн") (1 "Вт") (2 "Ср") (3 "Чт") (4 "Пт") (5 "Сб") (6 "Нд"))
  "Короткие наименования дней недели на украинском языке.")

(defparameter *day-of-week*  *day-of-week-ru*
  "Короткие наименования дней недели на текущем языке.")

@export
@annot.doc:doc
"@b(Описание:) day-of-week возвращает короткое строковое представление дня недели 
по номеру дня.

 @b(Переменые:)
@begin(list)
 @item(number - номер дня недели 0 - Пн; 1 - Вт; ...; 6 - Вс.)
@end(list)
 @b(Пример использования:)
@begin[lang=lisp](code)
(day-of-week 6) => \"Вс\"
@end(code)
"
(defun day-of-week (number) (cadr (assoc number *day-of-week*)))


@export
@annot.doc:doc
"@b(Описание:) utime->date преобразует универсальное время @b(utime) 
в формат даты org-mode.

@b(Пример использования:)
@begin[lang=lisp](code)
 (decode-universal-time 3756265175) => 35, 59, 8, 12, 1, 2019, 5, NIL, -2 
 (utime->date 3756265175) => \"<2019-1-12 Сб>\"
@end(code)
"
(defun utime->date (utime)
  
  (declare ((or integer) utime))
  (multiple-value-bind (ss mm hh dd mon yy w-day) (decode-universal-time utime)
      (declare (ignore  ss mm hh ))
      (format nil "<~4,'0D-~2,'0D-~2,'0D ~A>" yy mon dd (day-of-week w-day))))

@export
@annot.doc:doc
"@b(Описание:) utime->time преобразует универсальное время в строковое представление времени ΗΗ:MM:SS (не совсем org-mode).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (utime->time 3756265175) => \"08:59:35\"
@end(code)
"
(defun utime->time (utime)
  (declare ((or integer) utime))
  (multiple-value-bind (ss mm hh dd mon yy w-day) (decode-universal-time utime)
    (declare (ignore  dd mon yy w-day ))
    (format nil "~2,'0D:~2,'0D:~2,'0D" hh mm ss)))

@export
@annot.doc:doc
"@b(Описание:) table-to-org экспортирует таблицу в формат Org.

 @b(Переменые:)
@begin(list)
 @item(table  - 2d список;)
 @item(stream - поток вывода.)
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (table-to-org '((1  2 3)(3 4 5)(5 6 7)))  =>
 |1|2|3|
 |3|4|5|
 |5|6|7|
@end(code)
"
(defun table-to-org (table &optional (stream t))
  (format stream "~{~{|~S~}|~%~}" table))
