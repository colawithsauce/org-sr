;;; org-sr.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 ColaWithSauce
;;
;; Author: ColaWithSauce <cola_with_sauce@foxmail.com>
;; Maintainer: ColaWithSauce <cola_with_sauce@foxmail.com>
;; Created: March 08, 2023
;; Modified: March 08, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/colawithsauce/org-sr
;; Package-Requires: ((emacs "28.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'org)
(require 'uuidgen)
(require 'emacsql)
(require 'request)
(require 'emacsql-sqlite)
(require 'org-element)
(require 'parse-time)
(require 'dash)

(defgroup org-sr nil
  "Card abstraction of org-mode."
  :group 'org)

(defcustom org-sr-directory org-directory
  "Directory that for cards you want to revise."
  :group 'org-sr
  :type 'string)

(defcustom org-sr-algorithm-port 8970
  "Address and port of applying algorithm."
  :group 'org-sr
  :type 'integer)

(defcustom org-sr-files-exclude-regexp nil
  "Exclude regex expression for card files."
  :group 'org-sr
  :type 'string)

(defcustom org-sr-db-location (file-name-concat user-emacs-directory "org-sr.db")
  "Database file location for org-sr."
  :group 'org-sr
  :type 'string)

(defcustom org-sr-global-data-location (file-name-concat user-emacs-directory "org-sr-global.json")
  "Path to file that store user global data.")


;;; Utils
(defun org-sr-util-cl-struct-to-alist (struct)
  "Convert a cl-struct STRUCT instance to an alist."
  (let* ((type (type-of struct))
         (slot-list (mapcar #'car (cdr (cl-struct-slot-info type))))
         (slots ()))
    (dolist (slot slot-list)
      (let ((value (cl-struct-slot-value type slot struct)))
        (when value
          (push (cons slot value) slots))))
    slots))

(defun org-sr-util-cl-struct-from-alist (type alist)
  "Convert an ALIST into cl-struct TYPE."
  (let* ((slot-list (mapcar #'car (cdr (cl-struct-slot-info type))))
         (struct (funcall (intern (format "make-%s" type)))))
    (dolist (slot slot-list)
      (let ((value (alist-get slot alist)))
        (setf (cl-struct-slot-value type slot struct) value)))
    struct))

;;; Database
(defvar org-sr-db--connection nil
  "Database connection.")

(defun org-sr-db--get-connection ()
  "Get stable connection to database."
  (if (and org-sr-db--connection
           (file-exists-p org-sr-db-location)
           (emacsql-live-p org-sr-db--connection))
      org-sr-db--connection
    nil))

(defconst org-sr-schemata
  '((files
     [(file :primary-key :not-null)
      (hash :not-null)])
    (card-data
     ([(id :primary-key :not-null)
       (file :not-null)
       (due integer)
       interval
       difficulty
       stability
       retrievability
       grade
       lapses
       reps
       (review integer)]
      (:foreign-key [file] :references files [file] :on-delete :cascade))))
  "Schema table of org-sr.")

(defun org-sr-db--init (conn)
  "Initialize database with CONN with default schema table."
  (pcase-dolist (`(,table ,schema) org-sr-schemata)
    (emacsql conn [:create-table $i1 $S2] table schema)))

(defun org-sr-db--close ()
  "Close connection to database."
  (let ((db (org-sr-db--get-connection)))
    (when (and db
           (emacsql-live-p db))
    (emacsql-close db))))

(defun org-sr-db ()
  "The entry point of Org-roam-sr.

if the connection doesn't exist, then creat the connection, and
return the connection."
  (unless (and (org-sr-db--get-connection)
               (emacsql-live-p (org-sr-db--get-connection)))
    (let ((init-db (not (file-exists-p org-sr-db-location))))
      (make-directory (file-name-directory org-sr-db-location) t)
      (let ((conn (emacsql-sqlite org-sr-db-location)))
        (emacsql conn [:pragma (= foreign_keys ON)])
        (when-let* ((process (emacsql-process conn))
                    (_ (processp process)))
          (set-process-query-on-exit-flag process nil))
        (setq org-sr-db--connection conn)
        (when init-db
          (org-sr-db--init conn)))))
  (org-sr-db--get-connection))

(defun org-sr-db-query (sql &optional args)
  "Make SQL query with ARGS in database."
  (funcall #'emacsql (org-sr-db) sql args))

(defun org-sr-db-list-files ()
  "Get the spaced review file list."
  (directory-files-recursively
   org-sr-directory "^.*\.org$"))

(defun org-sr-db-card-p ()
  "If heading at point is a card."
  (let ((tags (org-get-tags)))
    (member "card" tags)))

(defun org-sr-db-map-cards (fns)
  "Mapping functions FNS onto all cards within current file."
  (org-with-wide-buffer
   (org-map-entries
    (lambda ()
      (dolist (fn fns)
        (when (org-sr-db-card-p)
          (funcall fn)))))))

(defun org-sr-db--current-file-list ()
  "Get current file list in database."
  (let ((current-files (org-sr-db-query [:select [file hash] :from files]))
        (ht (make-hash-table :test #'equal)))
    (dolist (row current-files)
      (puthash (car row) (cadr row) ht))
    ht))

(defun org-sr-db--file-hash (file-path)
  "Compute the hash of FILE-PATH."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file-path)
    (secure-hash 'sha1 (current-buffer))))

(defun org-sr-db-clear-file (&optional file)
  "Clear file FILE data from database."
  (setq file (or file (buffer-file-name)))
  (org-sr-db-query
   [:delete :from files
    :where (= file $s1)]
   file))

(defun org-sr-db-insert-file (&optional file-hash file-path)
  "Insert file of FILE-PATH into database."
  (let* ((file-path (or file-path (buffer-file-name))))
    (org-sr-db-query [:insert-into files :values $v1]
                     (vector file-path file-hash))))

(defun org-sr-db-insert-card-data ()
  "Insert card data at point into database."
  (let ((id (org-sr-db-get-card-id))
        (file (buffer-file-name)))
    (when (org-sr-db-query [:select * :from card-data
                            :where (= id $s1)] id)
      (error "Card %S already contained!" id))
    (if-let* ((data-list (org-entry-get (point) "CARD_DATA"))
              (data-list (string-split data-list ",")))
        (pcase-let* ((`(,due ,interval ,difficulty ,stability ,retrievability
                       ,grade ,lapses ,reps ,review) data-list)
                     (due (time-to-seconds (encode-time (iso8601-parse due))))
                     (review (time-to-seconds (encode-time (iso8601-parse review)))))
          (org-sr-db-query
           [:insert :into card-data
            :values $v1]
           (vector id file due
                   (string-to-number interval)
                   (string-to-number difficulty)
                   (string-to-number stability)
                   (string-to-number retrievability)
                   (string-to-number grade)
                   (string-to-number lapses)
                   (string-to-number reps)
                   review)))
      (org-sr-db-query
       [:insert-into card-data
                     [id file] :values $v1]
       (vector id file)))))

(defun org-sr-db-clear-card-data (&optional id)
  "Clear cache for card ID in database."
  (setq id (or id (org-sr-db-get-card-id)))
  (org-sr-db-query
   [:delete :from card-data
    :where (= id $s1)] id))

(defun org-sr-db-get-card-id ()
  "Get card id of card at point, if doesn't exist yet, creat one."
  (if-let ((id (org-entry-get (point) "CARD_ID")))
      id
    (let ((id (uuidgen-4)))
      (org-entry-put (point) "CARD_ID" id)
      (save-buffer)
      id)))

;;;###autoload
(defun org-sr-db-sync (&optional force)
  "Sync database with files.

If FORCE, force it."
  (interactive)
  ;; Force reconnect
  (org-sr-db--close)
  (when force (delete-file org-sr-db-location))
  (org-sr-db)
  ;; Get those deleted file list
  (let ((current-files (org-sr-db--current-file-list))
        (physical-files (org-sr-db-list-files))
        (modified-files nil))
    (dolist (file physical-files)
      (let ((content-hash (org-sr-db--file-hash file))) ; get modified files
        (unless (string= (gethash file current-files)
                         content-hash)
          (push file modified-files)))
      (remhash file current-files))    ; current - physical = desolated
    (dolist (file (hash-table-keys current-files))
      (org-sr-db-clear-file file))
    (dolist (file modified-files)
      (condition-case err
          (org-sr-db-update-file file)
        (error
         (org-sr-db-clear-file file)
         (lwarn 'org-sr :error "Failed to process file %s with error %s removing..."
                file (error-message-string err)))))))

(defun org-sr-db-update-file (&optional file-path)
  "Update informations about FILE-PATH in database.

If no FILE-PATH, use current file."
  (setq file-path (or file-path (buffer-file-name)))
  (let ((file-hash (org-sr-db--file-hash file-path))
        (db-hash (caar (org-sr-db-query
                       [:select hash :from files
                        :where (= file $s1)] file-path))))
    (unless (string= file-hash db-hash)
      (with-current-buffer (find-file-noselect file-path)
        (org-sr-db-clear-file)
        (org-sr-db-insert-file file-hash)
        (org-sr-db-map-cards
         (list #'org-sr-db-insert-card-data))))))

;;; Card & card-data
(cl-defstruct org-sr-card-data
  "Algorithm parameters."
  id file due interval difficulty stability retrievability
  grade lapses reps review)

(cl-defmethod org-sr-populate ((card-data org-sr-card-data))
  "Populate card data CARD-DATA according to database."
  (when-let* ((id (cl-struct-slot-value 'org-sr-card-data 'id card-data))
              (card-data-info (car (org-sr-db-query
                                    [:select [file due interval difficulty
                                                   stability retrievability
                                                   grade lapses reps review]
                                     :from card-data
                                     :where (= id $s1)
                                     :limit 1]
                                    id))))
    (pcase-let* ((`(,file ,due ,interval ,difficulty ,stability
                   ,retrievability ,grade ,lapses ,reps ,review)
                 card-data-info)
                 (due (format-time-string "%FT%TZ" (seconds-to-time due) "UTC"))
                 (review (format-time-string "%FT%TZ" (seconds-to-time review) "UTC")))
      (setf (org-sr-card-data-file card-data) file
            (org-sr-card-data-due card-data) due
            (org-sr-card-data-interval card-data) interval
            (org-sr-card-data-difficulty card-data) difficulty
            (org-sr-card-data-stability card-data) stability
            (org-sr-card-data-retrievability card-data) retrievability
            (org-sr-card-data-grade card-data) grade
            (org-sr-card-data-lapses card-data) lapses
            (org-sr-card-data-reps card-data) reps
            (org-sr-card-data-review card-data) review)
      card-data)))

(defun org-sr-card-data-exist-p (card-data)
  "If the card CARD-DATA still phisycally exist, return its position."
  (let* ((id (org-sr-card-data-id card-data))
         (file (org-sr-card-data-file card-data)))
    (when (file-exists-p file)
      (with-current-buffer (find-file-noselect file)
        (org-with-wide-buffer
         (org-map-entries (lambda () (point))
                          (format "+CARD_ID=%S" id)
                          'file))))))

(defun org-sr-card-data-uninitialized-list ()
  "Return the list of all uninitialized cards."
  (let ((id-list (org-sr-db-query
                  [:select id :from card-data :where due :is :null])))
    (mapcar
     (lambda (x) (org-sr-populate (make-org-sr-card-data :id (car x))))
     id-list)))

(defun org-sr-card-data-today-list ()
  "Return the list of all card-data that dued today and before."
  (let* ((time-sec (time-to-seconds (current-time)))
         (id-list
          (org-sr-db-query
           [:select id :from card-data
            :where (< due $s1)
            :order-by due] time-sec)))
    (mapcar
     (lambda (x)
       (org-sr-populate (make-org-sr-card-data :id (car x))))
     id-list)))

(defun org-sr-card-data-find (card-data)
  "Open the position of card CARD-DATA."
  (let ((id (org-sr-card-data-id card-data))
        (file (org-sr-card-data-file card-data)))
    (if (org-sr-card-data-exist-p card-data)
        (let ((_ (switch-to-buffer (find-file file)))
              (pos (org-with-wide-buffer
                    (org-map-entries (lambda () (point))
                                     (format "+CARD_ID=%S" id)
                                     'file))))
          (when (org-buffer-narrowed-p) (widen))
          (goto-char (car pos)))
      (error "Card %S doesn't exist!" id))))

;;; Global data TODO
;; Do not update global data doesn't effect the algorithm, wait.
(defun org-sr-global-data-get ()
  "Get global data from file."
  (when (and (file-exists-p org-sr-global-data-location)
             (json-read-file org-sr-global-data-location))
      (json-read-file org-sr-global-data-location)))

(defun org-sr-global-data-set (data)
  "Store global data DATA into json file."
  (with-temp-buffer
    (insert (json-encode data))
    (write-file org-sr-global-data-location)))

;;; Update card factor at point
(defun org-sr-update-factors (grade)
  "Update factors of card at point with grade GRADE."
  (interactive "nYour score: ")
  (if (memq grade (list -1 0 1 2))
    (let ((card-data (org-sr-util-cl-struct-to-alist
                      (org-sr-card-data-at-point)))
          (global-data (org-sr-global-data-get)))
      (with-temp-message "Requesting ..."
        (request (concat "http://localhost:" (number-to-string org-sr-algorithm-port))
          :type "POST"
          :data (json-encode
                 `(("jsonrpc " . "2.0") ("method" . "fsrs") ("id" . 1)
                   ("params" . [,card-data
                                ,grade
                                ,global-data])))
          :headers '(("Content-Type" . "application/json"))
          :parser #'json-read
          :encoding 'utf-8
          :sync t
          :success (cl-function
                    (lambda (&key data &allow-other-keys)
                      ;; (message "%S" data)
                      (if (alist-get 'error data)
                          (message "%S" (alist-get 'error data))
                        (let* ((result (alist-get 'result data))
                               (card-data (org-sr-util-cl-struct-from-alist
                                           'org-sr-card-data (aref result 0)))
                               (global-data (aref result 1)))
                          (org-sr-global-data-set global-data)
                          (org-entry-put (point) "CARD_DATA" (org-sr-card-data-to-string card-data))
                          (save-buffer)
                          (org-sr-db-update-file)
                          (org-sr-next t))))))))
    (message "Grade must be one of -1, 0, 1, 2!")))

(defun org-sr-card-data-at-point ()
  "Get card-data at this point.

If card uninitialized, return nil."
  (if-let* ((id (org-sr-db-get-card-id))
            (data-list (org-entry-get (point) "CARD_DATA"))
            (data-list (string-split data-list ","))
            (file (buffer-file-name)))
      (pcase-let ((`(,due ,interval ,difficulty
                     ,stability ,retrievability
                     ,grade ,lapses ,reps ,review) data-list))
        (make-org-sr-card-data
         :id id :file file :due due
         :interval (string-to-number interval)
         :difficulty (string-to-number difficulty)
         :stability (string-to-number stability)
         :retrievability (string-to-number retrievability)
         :grade (string-to-number grade)
         :lapses (string-to-number lapses)
         :reps (string-to-number reps)
         :review review))
    (let ((id (org-sr-db-get-card-id)))
      (make-org-sr-card-data :id id))))

(defun org-sr-card-data-to-string (card-data)
  "Convert algorithm card-datas CARD-DATA into string."
  (let ((mid-li
         (mapcar (lambda (x)
                   (cl-struct-slot-value 'org-sr-card-data x card-data))
                 '(interval difficulty stability
                   retrievability grade lapses reps)))
        (due (org-sr-card-data-due card-data))
        (review (org-sr-card-data-review card-data)))
    (concat
     due ","
     (mapconcat (lambda (x) (concat (number-to-string x) ",")) mid-li)
     review)))

;;; UI
(defun org-sr ()
  "Start spaced review."
  (interactive)
  (org-sr-next))

(defun org-sr-next (&optional kill)
  "Switch to next card.

If KILL, kill current buffer."
  (when kill (kill-buffer))
  (let ((newcard (org-sr-card-data-uninitialized-list))
        (today (org-sr-card-data-today-list)))
    (if-let* ((li (append newcard today)))
        (progn (org-sr-card-data-find (car li))
               (org-narrow-to-subtree)
               (org-fold-hide-entry))
      (when (and (eq major-mode 'org-mode) (org-buffer-narrowed-p)) (widen))
      (message "No more cards to learn today anymore! Have a nice day!"))))

(provide 'org-sr)
;;; org-sr.el ends here
