;; -*- lexical-binding: t -*-

(provide 'aozora-tategaki)

(defcustom aozora-tategaki-default-cols 40
  "デフォルトの縦表示文字数"
  :group 'aozora
  :type 'integer)

(defcustom aozora-tategaki-default-linum 10
  "デフォルトの横表示文字数"
  :group 'aozora
  :type 'integer)

(defvar aozora-tategaki-cols aozora-tategaki-default-cols)
(defvar aozora-tategaki-linum aozora-tategaki-default-linum)

(defvar aozora-tategaki-blank-char #x3000)

(defvar aozora-tategaki-buffer)
(defvar aozora-tategaki-current-line)
(defvar aozora-tategaki-current-column)
(defvar aozora-tategaki-current-page)
(defvar aozora-tategaki-indent)
(defvar aozora-tategaki-indent-turn)
(defvar aozora-tategaki-top-point)

(defvar aozora-tategaki-underground nil)
(defvar aozora-tategaki-center-flag nil)

(defvar aozora-tategaki-ruby-chars-left nil)
(defvar aozora-tategaki-ruby-chars-right nil)
(defvar aozora-tategaki-last-caption-point)

(defvar aozora-tategaki-max-cols)

(defvar aozora-tategaki-replace-table
  (eval-when-compile
    (cl-flet ((f (x)
		 (cond ((stringp x)
			(aref x 0))
		       (t
			x))))
      (mapcar (lambda (x)
		(cons (f (car x))
		      (cdr x)))
	      '(("（" . "︵")
		("）" . "︶")
		("—" . "｜")
		("ー" . "｜")
		("〔" . "︹")
		("〕" . "︺")
		("「" . "﹁")
		("」" . "﹂")
		("。" . "︒")
		("、" . "‘")
		("『" . "﹃")
		("』" . "﹄"))))))

(defun aozora-tategaki-init (&optional buffer)
  (buffer-disable-undo buffer)
  (setq aozora-tategaki-buffer
	(or buffer (current-buffer)))
  (setq aozora-tategaki-current-line 1)
  (setq aozora-tategaki-current-column 0)
  (setq aozora-tategaki-current-page 1)
  (setq aozora-tategaki-indent 0)
  (setq aozora-tategaki-indent-turn 0)
  (setq aozora-tategaki-max-cols 0))

(defun aozora-tategaki-set-cols (&optional cols)
  (setq aozora-tategaki-cols
	(if cols
	    (+ aozora-tategaki-indent cols)
	  aozora-tategaki-default-cols)))

(defun aozora-tategaki-set-indent (indent &optional indent-turn)
  (setq aozora-tategaki-indent indent)
  (setq aozora-tategaki-indent-turn (or indent-turn indent)))

(defun aozora-tategaki-add-indent (indent &optional indent-turn)
  (cl-incf aozora-tategaki-indent indent)
  (setq aozora-tategaki-indent-turn (or indent-turn indent)))

(defun aozora-tategaki-set-underground (n)
  (setq aozora-tategaki-underground n))

(defun aozora-tategaki-set-center ()
  (setq aozora-tategaki-center-flag t))

(defun aozora-tategaki-page-top-p ()
  (and (= aozora-tategaki-current-line 1)
       (= aozora-tategaki-current-column 0)))

(defun aozora-tategaki-insert-text (text)
  (if aozora-tategaki-underground
      (aozora-tategaki-insert-text-under
       text
       aozora-tategaki-underground)
    (with-current-buffer aozora-tategaki-buffer
      (setq aozora-tategaki-top-point (point))
      (if (zerop (length text))
	  (aozora-tategaki-newline)
	(aozora-tategaki--insert-text-internal
	 text
	 aozora-tategaki-indent)))))

(defun aozora-tategaki-insert-text-under (text margin)
  (with-current-buffer aozora-tategaki-buffer
    (setq aozora-tategaki-top-point (point))
    (aozora-tategaki--insert-text-internal
     text
     (- aozora-tategaki-cols
	(% (length text) aozora-tategaki-cols)
	margin))))

(defun aozora-tategaki--insert-text-internal (text indent)
  (cl-flet ((update-ruby (gvar ruby-str face)
			 (unless face
			   (setq face 'aozora-ruby-face))
			 (when ruby-str
			   (set gvar
				(nconc (symbol-value gvar)
				       (mapcar (lambda (c)
						 (list face c))
					       (cl-coerce ruby-str 'list)))))))
    (dotimes (_ indent)
      (aozora-tategaki--insert-letter aozora-tategaki-blank-char))
    (let ((i 0)
	  (len (length text)))
      (while (< i len)
	(let ((right (get-text-property i 'right text))
	      (left (get-text-property i 'left text))
	      (face (get-text-property i 'face text))
	      (aside-dir (get-text-property i 'aside-dir text))
	      (ruby-left (get-text-property i 'ruby-left text))
	      (ruby-right (get-text-property i 'ruby-right text)))
	  (update-ruby 'aozora-tategaki-ruby-chars-left ruby-left face)
	  (update-ruby 'aozora-tategaki-ruby-chars-right ruby-right face)
	  (aozora-tategaki--insert-letter (aref text i)
					  right
					  left
					  face
					  aside-dir)
	  (cl-incf i))))
    (unless (= aozora-tategaki-current-column 0)
      (aozora-tategaki-newline))))

(defun aozora-tategaki--insert-letter (c &optional right left face aside-dir)
  (aozora-tategaki--insert-letter-1 c right left face aside-dir)
  (if (< (cl-incf aozora-tategaki-current-column)
	 aozora-tategaki-cols)
      (if (eobp)
	  (insert "\n")
	(forward-line 1))
    (aozora-tategaki-newline)
    (dotimes (_ aozora-tategaki-indent-turn)
      (aozora-tategaki--insert-letter aozora-tategaki-blank-char))))

(defun aozora-tategaki--insert-letter-1 (c &optional right left face aside-dir)
  (let (left-face right-face)
    (unless right (setq right aozora-tategaki-blank-char))
    (unless left (setq left aozora-tategaki-blank-char))
    (when aozora-tategaki-ruby-chars-right
      (cl-multiple-value-setq (right-face right)
	(pop aozora-tategaki-ruby-chars-right)))
    (when aozora-tategaki-ruby-chars-left
      (cl-multiple-value-setq (left-face left)
	(pop aozora-tategaki-ruby-chars-left)))
    (let ((p (point)))
      (aozora-tategaki--insert-char left)
      (when left-face
	(put-text-property p (point) 'face left-face)))
    (let ((p (point)))
      (cond ((eq aside-dir 'right)
	     (insert " ")
	     (aozora-tategaki--insert-char c)
	     (put-text-property (1+ p) (+ 2 p) 'display '(height 0.5)))
	    ((eq aside-dir 'left)
	     (aozora-tategaki--insert-char c)
	     (put-text-property p (1+ p) 'display '(height 0.5))
	     (insert " "))
	    (t
	     (aozora-tategaki--insert-char c)))
      (when face
	(put-text-property p (point) 'face face)))
    (let ((p (point)))
      (aozora-tategaki--insert-char right)
      (when right-face
	(put-text-property p (point) 'face right-face)))))

(defun aozora-tategaki--fill-line ()
  (dotimes (_ (- aozora-tategaki-max-cols aozora-tategaki-current-column))
    (aozora-tategaki--insert-letter-1 aozora-tategaki-blank-char
				      nil nil nil nil)
    (if (eobp)
	(insert "\n")
      (forward-line 1))))

(defun aozora-tategaki-newline ()
  (when (< aozora-tategaki-max-cols aozora-tategaki-cols)
    (setq aozora-tategaki-max-cols aozora-tategaki-cols))
  (aozora-tategaki--fill-line)
  (if (< aozora-tategaki-linum
	 (cl-incf aozora-tategaki-current-line))
      (aozora-tategaki-newpage)
    (goto-char aozora-tategaki-top-point)
    (setq aozora-tategaki-current-column 0)))

(defun aozora-tategaki-newpage ()
  (setq aozora-tategaki-max-cols 0)
  (with-current-buffer aozora-tategaki-buffer
    (dotimes (_ (let ((n (1+ (- aozora-tategaki-linum
				aozora-tategaki-current-line))))
		  (if (not aozora-tategaki-center-flag)
		      n
		    (setq aozora-tategaki-center-flag nil)
		    (/ n 2))))
      (aozora-tategaki--fill-line)
      (goto-char aozora-tategaki-top-point))
    (goto-char (point-max))
    (unless (zerop (current-column))
      (insert "\n"))
    (aozora-tategaki-update-page)))

(defun aozora-tategaki-update-page ()
  (insert "\n" 12 "\n")
  (setq aozora-tategaki-top-point (point))
  (cl-incf aozora-tategaki-current-page)
  (setq aozora-tategaki-current-column 0)
  (setq aozora-tategaki-current-line 1))

(defun aozora-tategaki-insert-image (image caption-p)
  (with-current-buffer aozora-tategaki-buffer
    (when (/= 1 aozora-tategaki-current-line)
      (aozora-tategaki-newpage))
    (insert-image image)
    (insert "\n")
    (if caption-p
	(setq aozora-tategaki-last-caption-point (point))
      (goto-char (point-max))
      (aozora-tategaki-update-page))))

(defun aozora-tategaki-insert-caption (caption)
  (with-current-buffer aozora-tategaki-buffer
    (goto-char aozora-tategaki-last-caption-point)
    (insert caption)
    (goto-char (point-max))
    (aozora-tategaki-update-page)))

(defun aozora-tategaki-begin-rule ()
  (with-current-buffer aozora-tategaki-buffer
    (aozora-tategaki-insert-text
     (format "┐%s┘"
	     (make-string (- aozora-tategaki-cols aozora-tategaki-indent 3)
			  ?│)))))

(defun aozora-tategaki-end-rule ()
  (with-current-buffer aozora-tategaki-buffer
    (aozora-tategaki-insert-text
     (format "┌%s└"
	     (make-string (- aozora-tategaki-cols aozora-tategaki-indent 3)
			  ?│)))))

(defun aozora-tategaki--insert-char (c)
  (insert (aozora-tategaki--replace-char c)))

(defun aozora-tategaki--replace-char (c)
  (setq c (japanese-zenkaku c))
  (or (assoc-default c aozora-tategaki-replace-table) c))
