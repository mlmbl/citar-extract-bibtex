;;; citar-extract-bibtex.el --- A tool for extracting BibTeX entries from citations in Org files using Citar  -*- lexical-binding: t; -*-

;; Author: mlmbl
;; URL: https://github.com/mlmbl/citar-extract-bibtex
;; Version: 0.1
;; Keywords: citar, bibtex
;; Package-Requires: ((emacs "27.1") (org "9.6.15") (citar "20240907.1157"))

;;; Commentary:

;; This package provides the function citar-extract-bibtex-to-file,
;; which extracts the cited bibtex entries in the current org buffer
;; and writes them to a file.

;;; Code:

(require 'cl-lib)
(require 'citar)
(require 'org)

(defun citar-extract-bibtex-get-key (entry)
  "Return the citekey value from ENTRY, an alist representing a BibTeX entry."
  (let ((type-entry (assoc-string "=key=" entry)))  ;; 'assoc-string'を使って文字列キーを検索
    (if type-entry
        (cdr type-entry)
      nil)))

(defun citar-extract-bibtex-get-type (entry)
  "Return the type value (e.g., article, book) from ENTRY, an alist representing a BibTeX entry."
  (let ((type-entry (assoc-string "=type=" entry)))  ;; 'assoc-string'を使って文字列キーを検索
    (if type-entry
        (cdr type-entry)  ;; エントリが見つかれば、その値を返す
      nil)))  ;; 見つからなければnilを返す

(defun citar-extract-bibtex-get-fields (entry)
  "Extract fields from ENTRY by removing the citekey and type.
ENTRY is an alist representing a BibTeX entry, and the remaining
key-value pairs after removal are the fields."
(cl-remove-if (lambda (pair)
               (string-match "=" (format "%s" (car pair))))  ;; キーにSUBSTRINGが含まれるかチェック
             entry))

(defun citar-extract-bibtex-collect-citation ()
  "Collect cited entries from the current buffer using org-cite and citar."
  (let* ((citations
          (org-element-map (org-element-parse-buffer) 'citation
            (lambda (citation)
              (mapcar (lambda (ref)
                        (org-element-property :key ref))
                      (org-cite-get-references citation)))))
         (keys (apply #'append citations)))
    (message "Collected keys: %s" keys)  ;; Debugging output
    keys))  ;; Return the collected keys

(defun citar-extract-bibtex-format-fields (fields)
  "Generate a formatted string from the fields in FIELDS.
Each field is formatted as 'key = {value},' on a separate line.
FIELDS is an alist representing the fields of a BibTeX entry."
  (mapconcat
   (lambda (pair)
     (format "%s = {%s}," (car pair) (cdr pair)))
   fields
   "\n"))

(defun citar-extract-bibtex-format-entry (citekey)
  "Reconstruct the BibTeX entry for CITEKEY from the citar-get-entry data."
  (let* ((entry (citar-get-entry citekey))  ;; BibTeXエントリーのalistを取得
         (entry-type (citar-extract-bibtex-get-type entry))  ;; @type{ を取得
         (entry-key (citar-extract-bibtex-get-key entry))  ;; citekeyを取得
         (fields (citar-extract-bibtex-get-fields entry)))  ;; =type= と =key= を除外したフィールドを取得
    (concat
     (format "@%s{%s,\n" entry-type entry-key)  ;; @type{citekey, を生成
     (citar-extract-bibtex-format-fields (reverse fields))  ;; フィールドを逆順にしてフォーマット
     "\n}")))  ;; 閉じる

(defun citar-extract-bibtex-to-file (output-file)
  "Collect all cited BibTeX entries from the current Org buffer and export them to OUTPUT-FILE."
  (interactive "FSave to file: ")
  ;; 1. Collect all citekeys from the Org buffer
  (let ((citekeys (citar-extract-bibtex-collect-citation)))  ;; Collect cited keys
    (with-current-buffer (get-buffer-create "*Collected BibTeX Entries*")
      (erase-buffer))  ;; Clear the buffer before starting

    ;; 2. For each citekey, find the corresponding BibTeX entry and copy it
    (dolist (key citekeys)
      (message "Processing citekey: %s" key)
      (condition-case nil
          (let ((bibtex-entry (citar-extract-bibtex-format-entry key)))  ;; Reconstruct the BibTeX entry
            (with-current-buffer "*Collected BibTeX Entries*"
              (goto-char (point-max))  ;; Move to the end of the buffer
              (insert bibtex-entry "\n\n")))  ;; Insert the BibTeX entry
        (error (message "Failed to process citekey: %s" key))))

    ;; 3. Write the collected BibTeX entries to the specified output file
    (with-current-buffer "*Collected BibTeX Entries*"
      (write-region (point-min) (point-max) output-file))
    (message "BibTeX entries saved to %s" output-file)))

;;; citar-extract-bibtex.el ends here
