(provide 'aozora-face)

(defgroup aozora-face nil
  "Aozora, Faces."
  :prefix "aozora-"
  :group 'aozora)

(defface aozora-large-heading-face
  '((t (:bold t :height 1.2)))
  "大見出しのface"
  :group 'aozora-face)

(defface aozora-middle-heading-face
  '((t (:bold t :height 1.1)))
  "中見出しのface"
  :group 'aozora-face)

(defface aozora-small-heading-face
  '((t (:bold t)))
  "小見出しのface"
  :group 'aozora-face)

(defface aozora-ruby-face
  '((((class color) (background light))
     (:foreground "darkgreen"))
    (((class color) (background dark))
     (:foreground "e0e0e0")))
  "ルビのface"
  :group 'aozora-face)
