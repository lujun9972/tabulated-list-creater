;; -*- lexical-binding: t; -*-

(require 'f)
(require 'cl-lib)

(defun tabulated-list-creater--get-entries (ht-lst &optional id-key keys)
  "根据HT-LST(hash-table组成的list)生成tabulated-list-entries的值，参数KEYS指明了哪些列,默认从tabulated-list-format中抽出出值。ID-KEYS指明了哪个key的值作为id来使用"
  (lambda ()
    (let ((keys (or keys (mapcar #'car tabulated-list-format)))
          entries)
      (dolist (ht ht-lst)
        (let ((item-id (and id-key (gethash id-key ht)))
              (entry-data (mapcar (lambda (key)
                                    (gethash key ht))
                                  keys)))
          (push (list item-id (apply #'vector entry-data)) entries)))
      (reverse entries))))

(defun tabulated-list-creater--get-format (ht-lst &optional keys)
  "根据HT-LST(hash-table组成的list)生成tabulated-list-format的值，参数KEYS指明了哪些列"
  (let* ((ht (car ht-lst))
         (keys (or keys (hash-table-keys ht)))
         (lengths (mapcar (lambda (key)
                            (length (gethash key ht)))
                          keys))
         (format-list (cl-mapcar (lambda (title lenght)
                                   (list (format "%s" title) lenght nil))
                                 keys lengths)))
    (apply #'vector format-list)))

;; (setq ht (make-hash-table))
;; (puthash 'a "A" ht)
;; (puthash 'b "B" ht)
;; (puthash 'c "C" ht)
;; (setq ht-lst (list ht))
;; (setq fn (tabulated-list-creater--get-entries ht-lst nil '(a b c)))
;; (funcall fn )
;; (tabulated-list-creater--get-format ht-lst)

;; (pp (macroexpand-1 '(tabulated-list-creater test ht-lst)))
;; (tabulated-list-creater test ht-lst)

(defmacro tabulated-list-creater (mode-name ht-lst &optional id-key keys &rest bodys)
  "根据hash-table列表生成tabulated-list-mode"
  (let* ((mode (intern (format "%s-mode" mode-name)))
         (list-command (intern (format "list-%s" mode-name))))
    `(progn
       (define-derived-mode ,mode tabulated-list-mode ,(symbol-name mode)
         ,(symbol-name mode)
         (setq tabulated-list-format (tabulated-list-creater--get-format ,ht-lst ,keys))
         (setq tabulated-list-entries (tabulated-list-creater--get-entries ,ht-lst ,id-key))
         (tabulated-list-init-header)
         ,@bodys)
       (defun ,list-command ()
         ""
         (interactive)
         (switch-to-buffer (get-buffer-create ,(format "*%s*" mode-name)))
         (,mode)
         (tabulated-list-print t)))))



(provide 'tabulated-list-creater)
