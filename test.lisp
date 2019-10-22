;;;; mnas-org-mode.lisp

(in-package #:mnas-org-mode)

(defun encode-string (str)
  (let ((lst (map 'list #'(lambda (el) el) str))
	(rez nil))
    (map 'nil
	 #'(lambda (el)
	     (if (and (characterp (caar rez)) (char= (caar rez) el))
		 (incf (second (first rez)))
		 (push (list el 1) rez)))
	 lst)
    (nreverse rez)))

(defun org-level (str)
  (when (and (characterp (first (first (encode-string str))))
	     (char= #\*  (first (first (encode-string str)))))
    (second (first (encode-string str)))))

(defun read-line-trim (is &optional (char-bag " "))
  (string-trim " " (read-line *is*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require :str)

(defparameter *is* (open "d:/PRG/msys32/home/namatv/struct.org" :external-format :utf8))

(defparameter *rez* nil)

(let* ((str (read-line-trim *is*))
       (lvl (org-level str)))
  (defparameter *str* str)
  (defparameter *lvl* lvl))

(when (= *lvl* 1)
  (push (list *str* ()) *rez*))


(defun split-by-level (lst level)
  )


(defun foo (is )
  (let ((rez nil))
    
    ))

(org-level (read-line-trim *is*))

(with-open-file (is "d:/PRG/msys32/home/namatv/struct.org")
  (do ((rez nil))
      

      )
  )


(defun read-file-line-by-line (file)
  (let ((rez nil))
    (with-open-file (str file :direction :input :external-format :utf-8)
      (do ((line (read-line str nil 'eof) (read-line str nil 'eof)))
	  ((eql line 'eof) (nreverse rez))
	(push line rez)))))

(defun org-have-level (lst lvl)
  (dolist (str lst)
    (when (= (org-level str) lvl) (return t))))

(defun org-max-level (lst)
  (apply #'min
  (mapcan #'(lambda (str)
	      (list (org-level str)))
	  lst)))

(defun org-split-list-by-level (lst lvl)
  (let ((rez nil)
	(tmp (reverse lst))
	(itm nil))
    (dolist (str tmp)
      (push str itm)
      (when (= (org-level str) lvl)
	(push itm rez)
	(setf itm nil)))
    (nreverse rez)))


(org-split-list-by-level
 (map 'list
      #'(lambda (el)
	  (string-trim " " el))
      (read-file-line-by-line "d:/PRG/msys32/home/namatv/struct.org"))
 1)


(org-split-list-by-level (lst lvl)
