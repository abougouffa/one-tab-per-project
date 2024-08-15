;;; otpp-uniq.el --- Derive unique names based on paths -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Abdelhak Bougouffa
;;
;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>
;; Maintainer: Abdelhak Bougouffa <abougouffa@fedoraproject.org>
;; Created: July 07, 2024
;; Modified: July 10, 2024
;; Version: 1.0.0
;; Homepage: https://github.com/abougouffa/otpp-uniq
;; Package-Requires: ((emacs "29.1"))
;; SPDX-License-Identifier: GPL-3.0

;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This is a library that helps deriving unique names from paths. This can be
;; seen as `uniquify', but for files/directories.
;;
;; The main usecase is to track of opened directories/projects while keeping
;; abreviated names that uniquily identify the directory/project.
;;
;;; Code:

(require 'subr-x)
(require 'cl-macs)

;;; Unique name from directory

(defvar otpp-uniq-map-default (make-hash-table :test 'equal))
(defvar otpp-uniq-format "%s[%s]")

;;; Helpers

(defun otpp-uniq--get-dir-elements (dir)
  "Get elements for the DIR path."
  (butlast (reverse (file-name-split (directory-file-name (expand-file-name dir))))))

(defun otpp-uniq--unique-elements (dir1 dir2 &optional base1 base2)
  "Return unique elements of DIR1 and DIR2.
Consider custom base names BASE1 and BASE2 when non-nil."
  (let* ((els1 (otpp-uniq--get-dir-elements dir1))
         (els2 (otpp-uniq--get-dir-elements dir2)))
    (when base1 (push base1 els1))
    (when base2 (push base2 els2))
    (while-let ((el1 (car els1))
                (el2 (car els2))
                ((string= el1 el2)))
      (pop els1) (pop els2))
    (cons els1 els2)))

(cl-defun otpp-uniq--create-or-update (dir &key base rename-fn (map 'otpp-uniq-map-default))
  "Create or update a unique element for DIR.
For the meaning of :MAP, :RENAME-FN and :BASE, see `otpp-uniq-register'."
  (let* ((dir (expand-file-name dir))
         (dir-name (file-name-nondirectory (directory-file-name (expand-file-name dir))))
         (unique-map (eval map))
         (curr-element (gethash dir unique-map))
         (base (or base (alist-get 'base-name curr-element)))
         (name (or base dir-name))
         (unique-name
          (cl-loop for other-path in (hash-table-keys unique-map)
                   with len-min = most-positive-fixnum
                   with len-max = most-negative-fixnum
                   with max-path = nil
                   do
                   (let ((other-element (gethash other-path unique-map)))
                     (when (and (not (string= dir other-path)) ; not the same dir
                                (string= name (or (alist-get 'base-name other-element)
                                                  (alist-get 'dir-name other-element))))
                       (let ((dir-els (car (otpp-uniq--unique-elements dir other-path base (alist-get 'base-name other-element)))))
                         (let ((len (length dir-els)))
                           (setq len-min (min len-min len))
                           (when (> len len-max)
                             (setq len-max len
                                   max-path dir-els))))))
                   finally return
                   (let ((s (string-join
                             (reverse (butlast max-path
                                               (- (length max-path)
                                                  (1+ (- len-max len-min)))))
                             "/")))
                     (if (string-empty-p s)
                         name
                       (format otpp-uniq-format name s))))))
    (if curr-element
        (let* ((old-unique-name (assq 'unique-name curr-element))
               (old-base (assq 'base-name curr-element)))
          (when (and (functionp rename-fn) (not (equal (cdr old-unique-name) unique-name)))
            (ignore-errors (funcall rename-fn (cdr old-unique-name) unique-name)))
          (setcdr old-unique-name unique-name)
          (unless (equal (cdr old-base) base)
            (setcdr old-base base))
          nil) ; when the element already exist, update it and return nil
      (puthash dir `((dir-name . ,dir-name) (base-name . ,base) (unique-name . ,unique-name)) unique-map)
      t))) ; return t on newly created elements

(cl-defun otpp-uniq-update-all (&key rename-fn (map 'otpp-uniq-map-default))
  "Update all unique names.
This function can be called after manually modifying the hash table used
to keep track of the unique names.
For the meaning of :MAP and :RENAME-FN, see `otpp-uniq-register'."
  (let ((unique-map (eval map)))
    (dolist (path (hash-table-keys unique-map)) ; Update all the names
      (otpp-uniq--create-or-update path :map map :rename-fn rename-fn))))

;;; API

;;;###autoload
(cl-defun otpp-uniq-register (dir &key base rename-fn (map 'otpp-uniq-map-default))
  "Make a unique name derived from DIR.
If the :BASE string is provided, it will be used as a basis for the
unique name, otherwise, this will be calculated from the directory name
of DIR.
The :MAP is a symbol for the hash-table used to register the names, all
names will be renamed accordingly when needed.
The :RENAME-FN is a function of signature (OLD NEW), called before renaming
the hash-table elements."
  (append
   `((path . ,dir))
   (if-let* ((dir (expand-file-name dir))
             (dir-name (file-name-nondirectory (directory-file-name (expand-file-name dir))))
             (name (or base dir-name))
             (unique-map (eval map))
             (element (gethash dir unique-map)))
       element
     (puthash dir `((dir-name . ,dir-name) (base-name . ,base) (unique-name . ,name)) unique-map)
     (otpp-uniq-update-all :map map :rename-fn rename-fn)
     (gethash dir unique-map))))

;;;###autoload
(cl-defun otpp-uniq-unregister (dir &key rename-fn (map 'otpp-uniq-map-default))
  "Unregister a unique name derived from DIR.
For the meaning of :MAP and :RENAME-FN, see `otpp-uniq-register'."
  (let* ((dir (expand-file-name dir))
         (unique-map (eval map)))
    (remhash dir unique-map)
    (otpp-uniq-update-all :map map :rename-fn rename-fn)
    unique-map))


(provide 'otpp-uniq)
;;; otpp-uniq.el ends here
