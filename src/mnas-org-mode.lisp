;;;; mnas-org-mode.lisp

(defpackage :mnas-org-mode
  (:use #:cl)
  (:export utime->date
           day-of-week
           date->date
           table-to-org
           utime->time
           date-time->utime
           time->time
           utime->date-time
           org-date-time->utime
           )
  (:export make-hiper-link
           hiper-link->link
           hiper-link->description
           )
  (:export table-col-by-header
           table-row-by-header
           table-cell-by-header
           table-col-names
           table-row-names
           )
  (:export table-of-files)
  )

(in-package :mnas-org-mode)

(setf sb-impl::*default-external-format* :utf8)

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

(defparameter *date-time-sample* "<2021-01-14 Чт 10:35>"
  "Пример времени.")

(defun hiper-link->link (h-link)
  "@b(Описание:) hiper-link->link возвращает ссылочную часть ссылки, заданной в формате org-mode.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (mnas-org-mode:hiper-link->link mnas-org-mode::*hl-sample*) => \"D:/home/_namatv/_WorkPlan/2019/32/2019-01-12_082508.trd\"
@end(code)
"
  (declare ((or string) h-link))
  (ppcre:regex-replace-all
   "^file:"
   (string-trim "[]" (cl-ppcre:scan-to-strings "^\\[\\[.*\\]\\[" h-link))
  ""))

(defun hiper-link->description (h-link)
  "@b(Описание:) функция @b(hiper-link->description) возвращает описательную часть ссылки,
заданной в формате org-mode.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (mnas-org-mode:hiper-link->description mnas-org-mode::*hl-sample*) => \"2019-01-12 08:25:08.trd\"
@end(code)
"
  (declare ((or string) h-link))
  (string-trim "[]" (cl-ppcre:scan-to-strings "\\]\\[.*\\]\\]$" h-link)))
 
(defun make-hiper-link (link description)
  "Формирует ссылку в формате org-mode.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (make-hiper-link *l-sample* *d-sample*) => 
 \"[[D:/home/_namatv/_WorkPlan/2019/32/2019-01-12_082508.trd][2019-01-12 08:25:08.trd]]\"
@end(code)
"
  (declare ((or string) link description))
  (format nil "[[~A][~A]]" link description))

(defun date->date (org-date)
  "@b(Описание:) date->date выполняет преобразование даты в 
формате org-mode в список чисел.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (date->date *date-sample*)          -> (2019 1 12)
 (date->date *date-sample-wrong-01*) -> (2018 2 15)
 (date->date *date-sample-wrong-02*) -> (2017 3 18)
@end(code)
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
  (declare ((or string) date time))
  (append (date->date date) (time->time time))
  (apply #'encode-universal-time (nreverse (append (date->date date) (time->time time)))))

(defun org-date-time->utime (date-time)
  "@b(Описание:) функция @b(org-date-time->utime) возвращает универсальное время.

 @b(Переменые:)
@begin(list)
@iterm(date-time - строка, содержащая дату и время в формате org.) 
@end(list)

 @b(Пример использования:)
@begin[lang=lisp](code)
 (org-date-time->utime *date-time-sample*)
@end(code)
"
  (declare ((or string) date-time))
  (let ((date-day-time (mnas-string:split " []<>" date-time)))
    (date-time->utime
     (concatenate 'string "<" (first date-day-time) ">")
     (concatenate 'string (third date-day-time) ":00"))))

(defparameter *day-of-week-en* '((0 "Mo") (1 "Tu") (2 "We") (3 "Th") (4 "Fr") (5 "Sa") (6 "Su"))
  "Короткие наименования дней недели на английском языке.")

(defparameter *day-of-week-ru* '((0 "Пн") (1 "Вт") (2 "Ср") (3 "Чт") (4 "Пт") (5 "Сб") (6 "Вс"))
  "Короткие наименования дней недели на русском языке.")

(defparameter *day-of-week-ua* '((0 "Пн") (1 "Вт") (2 "Ср") (3 "Чт") (4 "Пт") (5 "Сб") (6 "Нд"))
  "Короткие наименования дней недели на украинском языке.")

(defparameter *day-of-week*  *day-of-week-ru*
  "Короткие наименования дней недели на текущем языке.")

(defun day-of-week (number)
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
  (cadr (assoc number *day-of-week*)))

(defun utime->date (utime)
  "@b(Описание:) utime->date преобразует универсальное время @b(utime) 
в формат даты org-mode.

@b(Пример использования:)
@begin[lang=lisp](code)
 (decode-universal-time 3756265175) => 35, 59, 8, 12, 1, 2019, 5, NIL, -2 
 (utime->date 3756265175) => \"<2019-1-12 Сб>\"
@end(code)
"
  (declare ((or integer) utime))
  (multiple-value-bind (ss mm hh dd mon yy w-day) (decode-universal-time utime)
    (declare (ignore  ss mm hh ))
    (format nil "<~4,'0D-~2,'0D-~2,'0D ~A>" yy mon dd (day-of-week w-day))))

(defun utime->time (utime)
  "@b(Описание:) utime->time преобразует универсальное время в строковое представление времени ΗΗ:MM:SS (не совсем org-mode).

 @b(Пример использования:)
@begin[lang=lisp](code)
 (utime->time 3756265175) => \"08:59:35\"
@end(code)
"
  (declare ((or integer) utime))
  (multiple-value-bind (ss mm hh dd mon yy w-day) (decode-universal-time utime)
    (declare (ignore  dd mon yy w-day ))
    (format nil "~2,'0D:~2,'0D:~2,'0D" hh mm ss)))

(defun utime->date-time (utime)
  (list (utime->date utime)
	(utime->time utime)))

(defun table-to-org (table &optional (stream t))
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
  (format stream "~{~{|~S~}|~%~}" table))

(defun table-col-by-header (col-name table)
  (let ((pos (position col-name  (first table) :test #'equal)))
    (cdr
     (loop :for row :in table
           :collect
           (nth pos row)))))

(defun table-row-by-header (row-name table)
  (cdr (assoc row-name table :test #'equal)))

(defun table-cell-by-header (row-name col-name table)
  (let ((pos (position col-name  (first table) :test #'equal)))
    (nth pos (assoc row-name table :test #'equal))))

(defun table-col-names (table)
  (first table))

(defun table-row-names (table)
  (loop :for row :in table :collect (first row)))

(defun table-of-files (dir mask &key (full-path nil))
  "  @b(Описание:) функция @b(table-of-files) возвращает список
  файлов из каталога @b(dir), с именами, соответствующими маске
  @b(mask), в формате пригодном для вставки в документ org.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (table-of-files
 \"//n142012/home/_avpete/Работа в Виннице/ДА32/Испытания сентябрь 2021 (шестая сборка)/Протоколы испытаний ДА32Л №1 сборка 6/21.09.21\"
 \"*.pdf\")
@end(code) "
  (loop :for i :in (directory (concatenate 'string  dir "/" mask))
        :collect
        (list
         (if full-path 
             (concatenate 'string "[[" (namestring i) "]]")
             (concatenate 'string "[[" (namestring i) "]"
                          "[=" (pathname-name i) "." (pathname-type i) "=]]")))))

(defun table-of-files (dir mask &key (full-path nil) (link t))
  "  @b(Описание:) функция @b(table-of-files) возвращает список
  файлов из каталога @b(dir), с именами, соответствующими маске
  @b(mask), в формате пригодном для вставки в документ org.

 @b(Пример использования:)
@begin[lang=lisp](code)
 (table-of-files
 \"//n142012/home/_avpete/Работа в Виннице/ДА32/Испытания сентябрь 2021 (шестая сборка)/Протоколы испытаний ДА32Л №1 сборка 6/21.09.21\"
 \"*.pdf\")
@end(code) "
  (loop :for i :in (directory (concatenate 'string  dir "/" mask))
        :collect
        (list
         (cond
           ((and full-path link)
            (concatenate
             'string
             "[[" (namestring i) "]]"))
           ((and (null full-path) link)
            (concatenate
             'string
             "[[" (namestring i) "]" "[=" (pathname-name i) "." (pathname-type i) "=]]"))
           ((and (null link))
            (concatenate
             'string "=" (ppcre:regex-replace-all "/" (namestring i) "\\") "="))))))

