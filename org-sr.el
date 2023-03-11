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

(defvar org-sr-db--connection nil
  "Stable connection to Database.")

(defun org-sr-db--get-connection ()
  "Get stable connection to database."
  (if (and org-sr-db--connection
           (emacsql-live-p org-sr-db--connection))
      org-sr-db--connection
    nil))

;;; Database things
;;  This part should define functions to init database, add into database, and
;;  remove from database, update whole database.
;; (cl-defstruct org-sr-card
;;   "Structure that represent a card to revise."
;;   id file due title contents)

(cl-defstruct org-sr-card-data
  "Algorithm parameters."
  id file due interval difficulty stability retrievability
  grade lapses reps review)

(defconst org-sr-schemata
  '((card-data
     ([(id :primary-key :not-null)
       file
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

;;; Card ID
;; (defun org-sr-card-id-show (id)
;;   "Goto the entry of given card id ID."
;;   (let* ((card-data (org-sr-populate (make-org-sr-card-data :id id)))
;;          (file (org-sr-card-data-file card-data)))
;;     (switch-to-buffer (find-file file))
;;     (let ((pos (org-sr-card-data-phisycal-pos card-data)))
;;       (goto-char pos))))

(defun org-sr-card-data-phisycal-pos (card-data)
  "If the card CARD-DATA still phisycally exist, return its position."
  (let* ((id (org-sr-card-data-id card-data))
         (file (org-sr-card-data-file card-data)))
    (with-current-buffer (find-file-noselect file)
      (org-with-wide-buffer
       (org-map-entries (lambda () (point))
                        (format "+CARD_ID=%S" id)
                        'file)))))

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

;;; Card from database
(cl-defmethod org-sr-populate ((card org-sr-card-data))
  "Populate card data CARD according to database.")

;;; Card ID from text
(defun org-sr-db-get-card-id ()
  "Get card id of card at point."
  (if-let ((id (org-entry-get (point) "CARD_ID")))
      id
    (let ((id (uuidgen-4)))
      (org-entry-put (point) "CARD_ID" id)
      id)))

;;; Card data
(defun org-sr-db-get-card-data ()
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

(defun org-sr-db-card-data-to-string (card-data)
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

(defun org-sr-db-insert-card-data ()
  "Insert card data at point into database.

If existed in database, overwrite."
  (let* ((card (org-sr-db-get-card-data))
         (id (org-sr-db-get-card-id))
         (file (buffer-file-name))
         (due (org-sr-card-data-due card))
         (interval (org-sr-card-data-interval card))
         (difficulty (org-sr-card-data-difficulty card))
         (stability (org-sr-card-data-stability card))
         (retrievability (org-sr-card-data-retrievability card))
         (grade (org-sr-card-data-grade card))
         (lapses (org-sr-card-data-lapses card))
         (reps (org-sr-card-data-reps card))
         (review (org-sr-card-data-review card)))
    (org-sr-db-query
     [:insert :into card-data
      :value $v1]
     (vector id file due interval difficulty stability retrievability grade lapses
             reps review))))

(defun org-sr-db-clear-card-data (&optional id)
  "Clear cache for card ID in database."
  (setq id (or id (org-sr-db-get-card-id)))
  (org-sr-db-query
   [:delete :from card-data
    :where (= id $s1)]
   id))

;;; Global data TODO
;; Do not update global data doesn't effect the algorithm, wait.
(defun org-sr-db-get-global-data ()
  "Get global data."
  nil)

(defun org-sr-db-set-global-data ()
  "Set global data.")

;;; Update card factor at point
(defun org-sr-card-update-factors (grade)
  "Update factors of card at point with grade GRADE."
  (let ((card-data (org-sr-util-cl-struct-to-alist
                    (org-sr-db-get-card-data)))
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
                     (org-entry-put (point) "CARD_DATA" (org-sr-db-card-data-to-string card-data)))))))))



(provide 'org-sr)
;;; org-sr.el ends here
