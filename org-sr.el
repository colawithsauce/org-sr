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

;; (require 'org-sr-util)

(defgroup org-sr nil
  "Card abstraction of org-mode."
  :group 'org)

(defcustom org-sr-directory org-directory
  "Directory that for cards you want to revise."
  :group 'org-sr
  :type 'string)

(defcustom org-sr-algorithm-port "http://localhost:8970"
  "Address and port of applying algorithm."
  :group 'org-sr
  :type 'string)

(defcustom org-sr-files-exclude-regexp nil
  "Exclude regex expression for card files."
  :group 'org-sr
  :type 'string)

(defcustom org-sr-db-location (file-name-concat user-emacs-directory "org-sr.db")
  "Database file location for org-sr."
  :group 'org-sr
  :type 'string)


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
  '((card-data
     ([(id :primary-key :not-null)
       (file :not-null)
       (due text)
       interval
       difficulty
       stability
       retrievability
       grade
       lapses
       reps
       (review text)]))
    (global-data
     [(id :primary-key :not-null)
      difficultyDecay
      stabilityDecay
      increaseFactor
      requestRetention
      totalCase
      totalDiff
      totalReview
      defaultDifficulty
      defaultStability
      stabilityDataArry]))
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

(defun org-sr-db-insert-card-data ()
  "Insert card data at point into database."
  (let ((id (org-sr-db-get-card-id))
        (file (buffer-file-name))
        (today-str (format-time-string "%FT%TZ" (current-time) t)))
    (when (org-sr-db-query [:select * :from card-data
                            :where (= id $s1)] id)
      (error "Card %S already contained!" id))
    (if-let* ((data-list (org-entry-get (point) "CARD_DATA"))
                (data-list (string-split data-list ",")))
          (pcase-let ((`(,due ,interval ,difficulty ,stability ,retrievability
                         ,grade ,lapses ,reps ,review) data-list))
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
          [id file due] :values $v1]
         (vector id file today-str)))))

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
  (org-sr-db--close)
  (when force (delete-file org-sr-db-location))
  (org-sr-db)
  ;; TODO: update cache only when they changed.
  (dolist (file (org-sr-db-list-files))
    (with-current-buffer (find-file-noselect file)
      (org-sr-db-map-cards
       (list #'org-sr-db-insert-card-data)))))

(defun org-sr-db-update-file ()
  "Update informations about current file in database.")

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
    ;; TODO fill the card data
    (pcase-let ((`(,file ,due ,interval ,difficulty ,stability
                   ,retrievability ,grade ,lapses ,reps ,review)
                 card-data-info))
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

(defun org-sr-card-data-today-list ()
  "Return the list of all card-data that dued today and before."
  (let* ((time-str (format-time-string "%FT%TZ" (current-time) t))
         (id-list
          (org-sr-db-query
           (format "SELECT id FROM card_data WHERE due < DATE('%s')" time-str))))
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
          (goto-char pos))
      (error "Card %S doesn't exist!" id))))

;;; Global data TODO
;; Do not update global data doesn't effect the algorithm, wait.
(defun org-sr-db-get-global-data ()
  "Get global data."
  nil)

(defun org-sr-db-set-global-data ()
  "Set global data.")

;;; Update card factor at point
(defun org-sr-update-factors (grade)
  "Update factors of card at point with grade GRADE."
  (let ((card-data (org-sr-util-cl-struct-to-alist
                    (org-sr-card-data-at-point)))
        (global-data (org-sr-db-get-global-data)))
   (request org-sr-algorithm-port
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
                          ;; (due (org-sr-card-data-due card-data))
                          ;; (global-data nil)
                          )
                     (org-entry-put (point) "CARD_DATA" (org-sr-card-data-to-string card-data)))))))))

(defun org-sr-card-data-at-point ()
  "Get card-data at this point.

If card uninitialized, return nil."
  (if-let ((id (org-sr-db-get-card-id))
           (data-list (string-split (org-entry-get (point) "CARD_DATA") ","))
           (alist ()))
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
         :review (string-to-number  review)))
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
  ())

(provide 'org-sr)
;;; org-sr.el ends here
