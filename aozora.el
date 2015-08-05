;; -*- lexical-binding: t -*-

(provide 'aozora)

(require 'cl-lib)

(require 'aozora-face)
(require 'aozora-tategaki)

(defgroup aozora nil
  "青空文庫ビューア"
  :prefix "aozora-"
  :group 'bunko)

(defcustom aozora-base-directory "~/.aozora"
  "青空文庫のベースディレクトリ"
  :group 'aozora
  :type 'string)

(defcustom aozora-ask-cache t
  "non-nilなら縦書きバッファ生成後にキャッシュするか尋ねる"
  :group 'aozora
  :type 'boolean)

(defcustom aozora-cache-file-name "cache.gz"
  "生成した縦書きバッファのキャッシュファイル名"
  :group 'aozora
  :type 'string)

(defcustom aozora-bookmark-file-name "bookmark"
  "その本のブックマークのファイル名"
  :group 'aozora
  :type 'string)

(defvar aozora-current-directory)
(defvar aozora-buffer-name)

(define-derived-mode aozora-list-mode nil "Aozora List"
  (setq buffer-read-only t))

(define-key aozora-list-mode-map "n" 'next-line)
(define-key aozora-list-mode-map "p" 'previous-line)
(define-key aozora-list-mode-map "\r" 'aozora-list-entry)
(define-key aozora-list-mode-map "s" 'aozora-list-sync)

(defun aozora-list ()
  (interactive)
  (with-current-buffer (get-buffer-create "*Aozora List*")
    (switch-to-buffer (current-buffer))
    (aozora-list-mode)
    (aozora-list-sync)))

(defun aozora-list-sync ()
  (interactive)
  (let ((buffer-read-only nil))
    (erase-buffer)
    (aozora-list-insert))
  (goto-char (point-min)))

(defun aozora-list-entry ()
  (interactive)
  (let ((dir (get-text-property (point) 'directory)))
    (when dir
      (aozora-view dir))))

(defun aozora-list-insert ()
  (cl-loop
   for (name dir) in (aozora-list-collect)
   do (let ((p (point)))
	(insert name)
	(put-text-property p (point) 'directory dir)
	(insert "\n"))))

(defun aozora-list-collect ()
  (let (list)
    (dolist (f (directory-files aozora-base-directory))
      (unless (or (string= "." f)
		  (string= ".." f))
	(let ((dir (expand-file-name f aozora-base-directory)))
	  (push (list f dir) list))))
    list))

(define-derived-mode aozora-view-mode nil "Aozora"
  (setq buffer-read-only t)
  (make-local-variable 'aozora-buffer-name))

(define-key aozora-view-mode-map "n" 'aozora-view-next-page)
(define-key aozora-view-mode-map "p" 'aozora-view-prev-page)
(define-key aozora-view-mode-map "B" 'aozora-view-bookmark-save)
(define-key aozora-view-mode-map "G" 'aozora-view-bookmark-jump)
(define-key aozora-view-mode-map "C" 'aozora-view-update-cache)
(define-key aozora-view-mode-map "V" 'aozora-view-open-text)

(defun aozora-view-next-page (n)
  (interactive "p")
  (forward-page n)
  (recenter 0))

(defun aozora-view-prev-page (n)
  (interactive "p")
  (backward-page n)
  (recenter 0))

(defun aozora-bookmark-load (dir)
  (let ((bookmark-file (expand-file-name aozora-bookmark-file-name dir)))
    (when (file-exists-p bookmark-file)
      (with-temp-buffer
	(insert-file-contents bookmark-file)
	(read (current-buffer))))))

(defun aozora-view-bookmark-jump ()
  (interactive)
  (let ((point (aozora-bookmark-load
		(aozora-path-concat aozora-base-directory
				    aozora-buffer-name))))
    (if point
	(when (y-or-n-p "ブックマークの位置に移動しますか")
	  (goto-char point)
	  (recenter 0))
      (message "この本にブックマークはありません"))))

(defun aozora-view-bookmark-save ()
  (interactive)
  (let ((point (point)))
    (when (y-or-n-p "この位置をブックマークしますか")
      (with-temp-file (aozora-path-concat aozora-base-directory
					  aozora-buffer-name
					  aozora-bookmark-file-name)
	(insert (number-to-string point)))
      (message "完了"))))

(defun aozora-view-update-cache ()
  (interactive)
  (when (y-or-n-p "キャッシュを更新しますか")
    (let ((aozora-ask-cache nil))
      (aozora-view
       (aozora-path-concat aozora-base-directory
			   aozora-buffer-name
			   (concat aozora-buffer-name ".txt"))))))

(defun aozora-view-open-text ()
  (interactive)
  (find-file-read-only
   (aozora-path-concat aozora-base-directory
		       aozora-buffer-name
		       (concat aozora-buffer-name ".txt"))))

(defun aozora-view (filename)
  (interactive "fFile: ")
  (let ((ext (file-name-extension filename)))
    (cond ((string= ext "zip")
	   (aozora-view-zip filename))
	  ((string= ext "txt")
	   (aozora-view-file filename))
	  ((string= ext "gz")
	   (aozora-view-cache filename))
	  ((file-directory-p filename)
	   (let* ((name (file-name-nondirectory
			 (directory-file-name
			  filename)))
		  (cache-file (expand-file-name aozora-cache-file-name
						filename))
		  (text-file (expand-file-name (format "%s.txt" name)
					       filename)))
	     (cond ((file-exists-p cache-file)
		    (aozora-view-cache cache-file))
		   ((file-exists-p text-file)
		    (aozora-view-file text-file))
		   (t
		    (error "指定したファイルは不正です")))))
	  (t
	   (error "指定したファイルは対応していません")))))

(defun aozora-view-zip (zipfile)
  (setq zipfile (expand-file-name zipfile))
  (unless (file-exists-p aozora-base-directory)
    (make-directory aozora-base-directory))
  (let* ((text-file-name (aozora-zip-take-text-file-name zipfile))
	 (dir (expand-file-name (file-name-sans-extension
				 (file-name-nondirectory text-file-name))
				aozora-base-directory)))
    (unless (zerop (call-process "unzip" nil nil nil
				 "-o"
				 "-d" dir
				 zipfile))
      (error "%sのunzipに失敗しました" zipfile))
    (aozora-view-file
     (expand-file-name text-file-name dir))))

(defun aozora-zip-take-text-file-name (zipfile)
  (with-temp-buffer
    (call-process "unzip" nil t nil
		  "-l"
		  zipfile)
    (goto-char (point-min))
    (unless (re-search-forward "[a-zA-Z0-9_-]+.txt" nil t)
      (error "指定したzipファイル内にテキストファイルが見つかりません"))
    (match-string 0)))

(defun aozora-view-file (filename)
  (aozora-view-internal
   (file-name-sans-extension (file-name-nondirectory filename))
   filename
   (lambda ()
     (cl-destructuring-bind (title text)
	 (aozora-get-contents filename)
       (aozora-tategaki-init)
       (aozora-render title)
       (aozora-tategaki-newpage)
       (aozora-render text)))
   t))

(defun aozora-view-cache (filename)
  (aozora-view-internal
   (file-name-nondirectory
    (directory-file-name
     (file-name-directory filename)))
   filename
   (lambda ()
     (aozora-cache-load filename))
   nil))

(defun aozora-view-internal (name filename render-function do-cache-p)
  (with-current-buffer (get-buffer-create
			(format "*aozora %s*"
				name))
    (let ((aozora-current-directory
	   (file-name-directory filename)))
      (switch-to-buffer (current-buffer))
      (aozora-view-mode)
      (setq aozora-buffer-name name)
      (let ((buffer-read-only nil))
	(erase-buffer)
	(funcall render-function))
      (goto-char (point-min))
      (if do-cache-p
	  (when (or (not aozora-ask-cache)
		    (y-or-n-p "キャッシュしますか"))
	    (aozora-cache-save
	     (aozora-path-concat aozora-base-directory
				 name
				 aozora-cache-file-name)))))))

(defun aozora-cache-load (filename)
  (insert
   (with-temp-buffer
     (insert-file-contents filename)
     (read (current-buffer))))
  t)

(defun aozora-cache-save (filename)
  (with-auto-compression-mode
    (write-region (prin1-to-string (buffer-string))
		  nil
		  filename)))

(defun aozora-get-contents (filename)
  (let (title text)
    (with-temp-buffer
      (insert-file-contents filename)
      (goto-char (point-min))
      (re-search-forward "^-+$")
      (setq title
	    (buffer-substring-no-properties
	     (point-min)
	     (match-beginning 0)))
      (re-search-forward "^-+$")
      (forward-char 2)
      (setq text
	    (buffer-substring-no-properties
	     (point)
	     (point-max))))
    (list title text)))

(eval-when-compile
  (defun aozora--gen-vars-parameter (parameters)
    (cl-loop for parm in parameters
	     for n from 0 by 1
	     append
	     (cl-loop for (type var) in parm
		      collect (cl-ecase type
				(beginning
				 `(,var (match-beginning ,n)))
				(end
				 `(,var (match-end ,n)))
				(string
				 `(,var (match-string ,n)))))))

  (defun aozora--gen-match-string-vars (vars)
    (cl-loop for n from 1 by 1
	     for var in vars
	     collect `(,var (match-string ,n)))))

(defmacro aozora-do-annotation-forward-ref
    (regex parameters rep-str ref-expr offset prop value)
  (let ((gprop (cl-gensym "prop"))
	(gstart (cl-gensym "start")))
    `(progn
       (goto-char (point-min))
       (let ((,gprop ,prop))
	 (while (re-search-forward ,regex nil t)
	   (let ,(aozora--gen-vars-parameter parameters)
	     (replace-match ,rep-str)
	     (let ((,gstart ,ref-expr))
	       (put-text-property ,gstart
				  (+ ,gstart ,offset)
				  ,gprop
				  ,value))))))))

(defmacro aozora-do-replace-regex (regex to-string-expr &optional noerror)
  (let ((gregex (cl-gensym "regex")))
    `(let ((,gregex ,regex))
       (goto-char (point-min))
       (while (re-search-forward ,gregex nil t)
	 (let ((rep-str ,to-string-expr))
	   (if rep-str
	       (replace-match rep-str)
	     (unless ,noerror
	       (error "Replace failed: %S %S"
		      ,gregex
		      ',to-string-expr))))))))

(defmacro aozora-do-annotation-between
    (left-regex
     right-regex
     point-parms
     left-matching-vars
     right-matching-vars
     start
     end
     prop
     value
     &optional
     body
     last-body)
  `(progn
     (goto-char (point-min))
     (while (re-search-forward ,left-regex nil t)
       (let ((,(car point-parms) (match-beginning 0))
	     ,@(aozora--gen-match-string-vars left-matching-vars))
	 (replace-match "")
	 (re-search-forward ,right-regex)
	 (let ((,(cadr point-parms) (match-beginning 0))
	       ,@(aozora--gen-match-string-vars right-matching-vars))
	   (replace-match "")
	   ,body
	   (put-text-property ,start ,end ,prop ,value)
	   ,last-body)))))

(defun aozora-render (text)
  (with-temp-buffer
    (insert text)
    (aozora-disposal-annotation)
    (aozora-render-lines)))

(defun aozora-disposal-annotation ()
  (aozora-replace-gaiji)
  (aozora-disposal-annotation-ruby)
  (aozora-remove-cannot-cope-annotation)
  (aozora-disposal-inserted-notes)
  (aozora-disposal-newline)
  (aozora-disposal-sup-or-sub)
  (aozora-disposal-annotation-mark)
  (aozora-disposal-annotation-em)
  (aozora-disposal-annotation-heading)
  (aozora-adjust-ruby)
  (aozora-replace-tategaki-text))

(defun aozora-replace-gaiji ()
  (aozora-do-replace-regex
   "※［＃[^、］]+、UCS-\\([0-9A-F]+\\)、[^］]+］"
   (char-to-string (string-to-number (match-string 1) 16))
   t))

(defun aozora-remove-cannot-cope-annotation ()
  (dolist (regex '("［＃「[^」]*」は底本では「[^」]*」］"
		   "［＃ルビの「[^」]*」は底本では「[^」]*」］"
		   "［＃[^［］]*はママ］"
		   "［＃「[^」]*」は[上下]付き小文字］"
		   "［＃[上下]付き小文字］"
		   "［＃[上下]付き小文字終わり］"
		   "［＃ここから横組み］"
		   "［＃ここで横組み終わり］"
		   "［＃「[^」]*」は横組み］"
		   "［＃横組み］"
		   "［＃横組み終わり］"
		   "［＃「[^」]+」は縦中横］"
		   "［＃縦中横］"
		   "［＃縦中横終わり］"))
    (aozora-do-replace-regex regex "")))

(defun aozora-disposal-annotation-ruby ()
  (aozora-do-annotation-forward-ref
   "\\(\\(?:｜.+?\\)\\|\\(?:[a-z`※㐀-鿿󠄀-󠇿]+々*〻?※?\\)\\)《\\(.+?\\)》"
   (((beginning start))
    ((string str))
    ((string ruby)))
   (save-match-data
     (setq str
	   (if (eq ?｜ (aref str 0))
	       (substring str 1)
	     str)))
   start 1
   'ruby-right (list ruby (length str)))
  (cl-loop
   for (regex prop) in
   '(("［＃「\\([^「」［］]*\\)」に「\\([^「」]+\\)」の注記］" ruby-right)
     ("［＃「\\([^「」［］]*\\)」の左に「\\([^「」]+\\)」の注記］" ruby-left)
     ("［＃「\\([^」]+\\)」の左に「\\([^」]+\\)」のルビ］" ruby-left))
   do (aozora-do-annotation-forward-ref
       regex
       (()
	((string str1))
	((string str2)))
       ""
       (search-backward str1)
       1
       prop
       (list str2 (length str1))))
  (cl-loop
   for (left right prop)
   in '(("［＃左にルビ付き］"
	 "［＃左に「\\([^」]+\\)」のルビ付き終わり］"
	 ruby-left)
	("［＃注記付き］"
	 "［＃「\\([^」]+\\)」の注記付き終わり］"
	 ruby-right)
	("［＃左に注記付き］"
	 "［＃左に「\\([^」]+\\)」の注記付き終わり］"
	 ruby-left))
   do (aozora-do-annotation-between
       left
       right
       (start end)
       ()
       (ruby)
       start (1+ start)
       prop (list ruby (- end start)))))

(defun aozora-adjust-ruby ()
  (switch-to-buffer (current-buffer))
  (cl-flet ((next-prop (point prop)
		       (if (get-text-property point prop)
			   point
			 (let ((point
				(next-single-property-change point prop)))
			   (if (and point
				    (null (get-text-property point prop)))
			       (next-single-property-change
				point prop)
			     point)))))
    (let (p)
      (goto-char (point-min))
      (while (setq p (next-prop (point) 'ruby-right))
	(goto-char p)
	(cl-destructuring-bind (ruby strlen)
	    (get-text-property p 'ruby-right)
	  (remove-text-properties p (1+ p) '(ruby-right nil))
	  (let ((diff (- (length ruby) strlen)))
	    (if (<= diff 0)
		(let ((p (point)))
		  (insert (prog1 (buffer-substring
				  p
				  (+ p strlen))
			    (delete-char strlen)))
		  (put-text-property p (1+ p) 'ruby-right ruby))
	      (let* ((p (point))
		     (str (buffer-substring p (+ p strlen))))
		(delete-char strlen)
		(insert (make-string (/ diff 2) #x3000)
			str
			(make-string (- diff (/ diff 2)) #x3000))
		(put-text-property p (1+ p) 'ruby-right ruby))))))
      (goto-char (point-min))
      (while (setq p (next-prop (point) 'ruby-left))
	(goto-char p)
	(cl-destructuring-bind (ruby _)
	    (get-text-property p 'ruby-left)
	  (remove-text-properties p (1+ p) '(ruby-left nil))
	  (put-text-property p (1+ p) 'ruby-left ruby))))))

(defun aozora-disposal-inserted-notes ()
  (aozora-do-replace-regex "（?［＃割り注］"
			   "（")
  (aozora-do-replace-regex "［＃割り注終わり］）?"
			   "）"))

(defun aozora-disposal-newline ()
  (aozora-do-replace-regex "［＃改行］" "\n"))

(defun aozora-disposal-sup-or-sub ()
  (aozora-disposal-annotation-forward-ref
   "［＃「%s」は行右小書き］"
   'aside-dir
   'right)
  (aozora-disposal-annotation-forward-ref
   "［＃「%s」は行左小書き］"
   'aside-dir
   'left)
  (aozora-disposal-annotation-between
   "［＃行右小書き］"
   "［＃行右小書き終わり］"
   'aside-dir
   'right)
  (aozora-disposal-annotation-between
   "［＃行左小書き］"
   "［＃行左小書き終わり］"
   'aside-dir
   'left))

(defun aozora-disposal-annotation-mark ()
  (cl-loop for (name char)
	   in '(("傍点" #xFE45)
		("白ゴマ傍点" #xFE46)
		("丸傍点" #x25CF)
		("白丸傍点" #x25CB)
		("黒三角傍点" #x25B2)
		("白三角傍点" #x25B3)
		("二重丸傍点" #x25CE)
		("蛇の目傍点" #x25C9)
		("ばつ傍点" #x2716)
		("傍線" #x2503)
		("二重傍線" #x2016)
		("鎖線" #xFF1A)
		("破線" #xFF1B)
		("波線" #xFF4C))
	   do
	   (aozora-disposal-annotation-between
	    (format "［＃%s］" name)
	    (format "［＃%s終わり］" name)
	    'right
	    char)
	   (aozora-disposal-annotation-between
	    (format "［＃左に%s］" name)
	    (format "［＃左に%s終わり］" name)
	    'left
	    char)
	   (aozora-disposal-annotation-forward-ref
	    (format "［＃「%%s」に%s］" name)
	    'right
	    char)
	   (aozora-disposal-annotation-forward-ref
	    (format "［＃「%%s」の左に%s］" name)
	    'left
	    char)))

(defun aozora-disposal-annotation-em ()
  (aozora-disposal-annotation-between
   "［＃太字］"
   "［＃太字終わり］"
   'face
   'bold)
  (aozora-disposal-annotation-between
   "［＃斜体］"
   "［＃斜体終わり］"
   'face
   'italic)
  (aozora-disposal-annotation-forward-ref
   "［＃「%s」は太字］"
   'face
   'bold)
  (aozora-disposal-annotation-forward-ref
   "［＃「%s」は斜体］"
   'face
   'italic)
  (aozora-disposal-annotation-between
   "［＃ここから太字］"
   "［＃ここで太字終わり］"
   'face
   'bold)
  (aozora-disposal-annotation-between
   "［＃ここから斜体］"
   "［＃ここで斜体終わり］"
   'face
   'italic))

(defun aozora-disposal-annotation-heading ()
  (aozora-disposal-annotation-forward-ref
   "［＃「%s」は大見出し］"
   'face
   'aozora-large-heading-face)
  (aozora-disposal-annotation-forward-ref
   "［＃「%s」は中見出し］"
   'face
   'aozora-middle-heading-face)
  (aozora-disposal-annotation-forward-ref
   "［＃「%s」は小見出し］"
   'face
   'aozora-small-heading-face)
  (aozora-disposal-annotation-between
   "［＃大見出し］"
   "［＃大見出し終わり］"
   'face
   'aozora-large-heading-face)
  (aozora-disposal-annotation-between
   "［＃中見出し］"
   "［＃中見出し終わり］"
   'face
   'aozora-middle-heading-face)
  (aozora-disposal-annotation-between
   "［＃小見出し］"
   "［＃小見出し終わり］"
   'face
   'aozora-small-heading-face)
  (aozora-disposal-annotation-between
   "［＃ここから大見出し］"
   "［＃ここで大見出し終わり］"
   'face
   'aozora-large-heading-face)
  (aozora-disposal-annotation-between
   "［＃ここから中見出し］"
   "［＃ここで中見出し終わり］"
   'face
   'aozora-middle-heading-face)
  (aozora-disposal-annotation-between
   "［＃ここから小見出し］"
   "［＃ここで小見出し終わり］"
   'face
   'aozora-small-heading-face)
  (aozora-disposal-annotation-forward-ref
   "［＃「%s」は同行大見出し］"
   'face
   'aozora-large-heading-face)
  (aozora-disposal-annotation-forward-ref
   "［＃「%s」は同行中見出し］"
   'face
   'aozora-middle-heading-face)
  (aozora-disposal-annotation-forward-ref
   "［＃「%s」は同行小見出し］"
   'face
   'aozora-small-heading-face)
  (aozora-disposal-annotation-between
   "［＃同行大見出し］"
   "［＃同行大見出し終わり］"
   'face
   'aozora-large-heading-face)
  (aozora-disposal-annotation-between
   "［＃同行中見出し］"
   "［＃同行中見出し終わり］"
   'face
   'aozora-middle-heading-face)
  (aozora-disposal-annotation-between
   "［＃同行小見出し］"
   "［＃同行小見出し終わり］"
   'face
   'aozora-small-heading-face)
  (aozora-disposal-annotation-forward-ref
   "［＃「%s」は窓大見出し］"
   'face
   'aozora-large-heading-face)
  (aozora-disposal-annotation-forward-ref
   "［＃「%s」は窓中見出し］"
   'face
   'aozora-middle-heading-face)
  (aozora-disposal-annotation-forward-ref
   "［＃「%s」は窓小見出し］"
   'face
   'aozora-small-heading-face)
  (aozora-disposal-annotation-between
   "［＃窓大見出し］"
   "［＃窓大見出し終わり］"
   'face
   'aozora-large-heading-face)
  (aozora-disposal-annotation-between
   "［＃窓中見出し］"
   "［＃窓中見出し終わり］"
   'face
   'aozora-middle-heading-face)
  (aozora-disposal-annotation-between
   "［＃窓小見出し］"
   "［＃窓小見出し終わり］"
   'face
   'aozora-small-heading-face))

(defun aozora-disposal-annotation-forward-ref
    (regex prop val)
  (aozora-do-annotation-forward-ref
    (format regex "\\([^「」［］《》]*\\)")
    (()
     ((string refstr)))
    ""
    (search-backward refstr)
    (length refstr)
    prop
    val))

(defun aozora-disposal-annotation-between (begin-str end-str prop val)
  (aozora-do-annotation-between
   begin-str
   end-str
   (start end)
   ()
   ()
   start end
   prop val))

(defun aozora-replace-tategaki-text ()
  (goto-char (point-min))
  (while (search-forward "…" nil t)
    (replace-match "・・・")))

(defun aozora-render-lines ()
  (goto-char (point-min))
  (while (not (eobp))
    (cond
     ((or (looking-at "［＃改丁］")
	  (looking-at "［＃改ページ］")
	  (looking-at "［＃改見開き］")
	  (looking-at "［＃改段］"))
      (unless (aozora-tategaki-page-top-p)
	(aozora-tategaki-newpage)))
     ((looking-at "［＃\\([０-９]+\\)字下げ］")
      (let ((begin (match-end 0))
	    (str (match-string 1)))
	(aozora-tategaki-add-indent (aozora-parse-zenkaku-digits str))
	(aozora-render-line begin (line-end-position))
	(aozora-tategaki-set-indent 0)))
     ((looking-at "［＃ここから\\([０-９]+\\)字下げ］")
      (let ((str (match-string 1)))
	(aozora-tategaki-add-indent (aozora-parse-zenkaku-digits str))))
     ((looking-at
       "［＃ここから\\([０-９]+\\)字下げ、折り返して\\([０-９]+\\)字下げ］")
      (let ((str1 (match-string 1))
	    (str2 (match-string 2)))
	(aozora-tategaki-set-indent
	 (aozora-parse-zenkaku-digits str1)
	 (aozora-parse-zenkaku-digits str2))))
     ((looking-at
       "［＃ここから改行天付き、折り返して\\([０-９]+\\)字下げ］")
      (aozora-tategaki-set-indent
       0
       (aozora-parse-zenkaku-digits (match-string 1))))
     ((looking-at "［＃ここで字下げ終わり］")
      (aozora-tategaki-set-indent 0))
     ((looking-at "［＃ここから地付き］")
      (aozora-tategaki-set-underground 0))
     ((looking-at "［＃ここから地から\\([０-９]+\\)字上げ］")
      (aozora-tategaki-set-underground
       (aozora-parse-zenkaku-digits (match-string 1))))
     ((or (looking-at "［＃ここで地付き終わり］")
	  (looking-at "［＃ここで字上げ終わり］"))
      (aozora-tategaki-set-underground nil))
     ((looking-at "［＃ここから\\([０-９]+\\)字詰め］")
      (aozora-tategaki-set-cols (aozora-parse-zenkaku-digits (match-string 1))))
     ((looking-at "［＃ここで字詰め終わり］")
      (aozora-tategaki-set-cols))
     ((looking-at "［＃ページの左右中央］")
      (aozora-tategaki-set-center))
     ((looking-at "［＃\\(「[^」]+」のキャプション付き\\|.+?\\)の.\\{1,5\\}（\\([a-zA-Z0-9_.]+\\)[^（）]*）入る］")
      (let ((alt (match-string 1))
	    (image-name (match-string 2)))
	(aozora-tategaki-insert-image
	 (create-image (expand-file-name image-name
					 aozora-current-directory))
	 (string-match "キャプション付き" alt))))
     ((looking-at "［＃ここからキャプション］")
      (let ((start (match-end 0)))
	(search-forward "［＃ここでキャプション終わり］")
	(aozora-tategaki-insert-caption
	 (buffer-substring-no-properties start (match-beginning 0)))))
     ((looking-at "［＃ここから罫囲み］")
      (aozora-tategaki-begin-rule))
     ((looking-at "［＃ここで罫囲み終わり］")
      (aozora-tategaki-end-rule))
     (t
      (aozora-render-line (line-beginning-position)
			  (line-end-position))))
    (forward-line 1)))

(defun aozora-render-line (begin end)
  (let ((line (buffer-substring begin end)))
    (cond
     ((string-match "［＃地付き］" line)
      (let ((start (match-beginning 0))
	    (end (match-end 0)))
	(when (< 0 start)
	  (aozora-tategaki-insert-text (substring line 0 start)))
	(aozora-tategaki-insert-text-under (substring line end) 0)))
     ((string-match "［＃地から\\([０-９]+\\)字上げ］" line)
      (let ((start (match-beginning 0))
	    (end (match-end 0))
	    (str (match-string 1 line)))
	(when (< 0 start)
	  (aozora-tategaki-insert-text (substring line 0 start)))
	(aozora-tategaki-insert-text-under (substring line end)
					   (aozora-parse-zenkaku-digits str))))
     ((string-match "［＃「\\([^「」［］]*\\)」はキャプション］" line)
      (aozora-tategaki-insert-caption (match-string 1 line)))
     ((string-match "［＃キャプション］\\(.*?\\)［＃キャプション終わり］" line)
      (aozora-tategaki-insert-caption (match-string 1 line)))
     (t
      (aozora-tategaki-insert-text line)))))

(defun aozora-parse-zenkaku-digits (str)
  (string-to-number (japanese-hankaku str)))

(defun aozora-path-concat (&rest path-list)
  (mapconcat 'directory-file-name
	     path-list
	     "/"))

(defun aozora-test (text)
  (let ((buffer (get-buffer-create "*aozora test*")))
    (pop-to-buffer buffer)
    (aozora-tategaki-init buffer)
    (let ((buffer-read-only nil))
      (aozora-render text))))
