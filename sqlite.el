;;; sqlite.el ---
;;
;; Filename: sqlite.el
;; Description:
;; Author: Christian Giménez
;; Maintainer:
;; Created: mié feb 13 11:12:31 2013 (-0300)
;; Version:
;; Last-Updated: dom feb 24 20:50:48 2013 (-0300)
;;           By: Christian
;;     Update #: 105
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   `cl'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; For usage and explanations see http://www.emacswiki.org/emacs/SQLite-el.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;; 24-Feb-2013    Christian
;;    Last-Updated: dom feb 24 20:49:45 2013 (-0300) #103 (Christian)
;;    `add-to-list' doesn't let you push new elements if they are already. We don't want this behaviour.
;; 24-Feb-2013    Christian
;;    Last-Updated: dom feb 24 20:30:10 2013 (-0300) #95 (Christian)
;;    There is a problem in the regexp `sqlite-regexp-sqlite-command'. It doesn't test the "." at the begining of the string.
;; 16-Feb-2013    Christian
;;    Last-Updated: sáb feb 16 19:22:25 2013 (-0300) #93 (Christian)
;;    Some problems with the first query are now solved!
;; 16-Feb-2013    Christian
;;    Last-Updated: sáb feb 16 18:39:10 2013 (-0300) #84 (Christian)
;;    `sqlite-init' has filename expansion. You don't need to write the absolute path of the file.
;; 16-Feb-2013    Christian
;;    Last-Updated: sáb feb 16 18:27:16 2013 (-0300) #78 (Christian)
;;    `sqlite-query' adds  ";" at the end of the query for you.
;; 16-Feb-2013    Christian
;;    Last-Updated: sáb feb 16 18:21:06 2013 (-0300) #76 (Christian)
;;    Now varios process works. Checking SQLite errors when querying works.
;; 16-Feb-2013    Christian
;;    Last-Updated: sáb feb 16 16:09:48 2013 (-0300) #41 (Christian)
;;    Adding a list for descriptor, process buffers and its files(`sqlite-process-alist').
;; 16-Feb-2013    Christian
;;    Last-Updated: sáb feb 16 02:21:02 2013 (-0300) #25 (Christian)
;;    Adding support for making various sqlite process.
;; 14-Feb-2013    Christian
;;    Last-Updated: jue feb 14 01:28:15 2013 (-0300) #4 (Christian)
;;    Added `sqlite-bye' function for finishing the sqlite process.
;; 14-Feb-2013    Christian
;;    Last-Updated: mié feb 13 11:18:46 2013 (-0300) #3 (Christian)
;;    Now output buffer doesn't appear. Sqlite connects and still works.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


(provide 'sqlite)
(eval-when-compile
  (require 'cl))


;; Adapted from http://mysite.verizon.net/mbcladwell/sqlite.html#connect

(defvar sqlite-program "sqlite3" "Full path name of the SQLITE executable.")

(defvar sqlite-output-buffer "*sqlite-output*" "Name of the SQLite output buffer.")

(defvar sqlite-process-buffer "sqlite-process" "*Name of the SQLITE process buffer. This is where SQL commands are sent.")

					; Process list storing and manipulation
					; ----------------------------------------

(defvar sqlite-process-alist nil
  "An alist that contains each descriptor with the corresponding buffers process and the file opened.
Example:
 (setq sqlite-process-alist
  '(
      (1 . '(\"*sqlite-process1*\" \"~/mydb1.sqlite\"))
      (2 . '(\"*sqlite-process2*\" \"~/databases/mydb2.sqlite\"))

  ))"
  )

(defvar sqlite-descriptor-counter 0
  "This is a counter that adds 1 for each sqlite process opened. Used for referencing each sqlite process uniquely.")

(defun sqlite-register-descriptor (descriptor buffer file)
  "Register the descriptor with the buffer given adding it into `sqlite-process-alist'."
  (add-to-list 'sqlite-process-alist (cons descriptor (list (list buffer file))))
  )

(defun sqlite-descriptor-buffer (descriptor)
  "Return the buffer asociated to the DESCRIPTOR"
  (car (cadr (assoc descriptor sqlite-process-alist)))
  )

(defun sqlite-unregister-descriptor (descriptor)
  "Remove the descriptor from the list of process buffers `sqlite-process-alist'."
  (setq sqlite-process-alist (assq-delete-all descriptor sqlite-process-alist))
  )

					; ----------------------------------------


(defun sqlite-init (db-file)
  "Initialize sqlite interface opening the DB-FILE sqlite file.
This start the process given by `sqlite-program' and prepare it for queries.

Returns the sqlite process descriptor, a unique id that you can use to retrieve the process or send a query. "
  (let ((process-buffer (concat "sqlite-process" (number-to-string sqlite-descriptor-counter))) ; name of the process buffer
	)
    (setq db-file (expand-file-name db-file))

    (apply 'make-comint
	   process-buffer
	   sqlite-program  nil `(,db-file ))

    (setq process-buffer (concat "*" process-buffer "*")) ;; Asteriscs automatically added by `make-comint'

    (sqlite-register-descriptor sqlite-descriptor-counter process-buffer db-file)
    (setq sqlite-descriptor-counter (+ sqlite-descriptor-counter 1))

    (accept-process-output (get-buffer-process process-buffer)) ;; Wait for the results and for first echo
    ;; We use CSV for parsing results.
    (comint-redirect-send-command-to-process ".mode csv" sqlite-output-buffer (get-buffer-process process-buffer) nil t)
    ;; We remove prompt.
    (comint-redirect-send-command-to-process ".prompt \"\"" sqlite-output-buffer (get-buffer-process process-buffer) nil t)
    (accept-process-output (get-buffer-process process-buffer) 1) ;; Wait for the results and for first echo
    ;; Add headers.
    (comint-redirect-send-command-to-process ".headers on" sqlite-output-buffer (get-buffer-process process-buffer) nil t)
    (accept-process-output (get-buffer-process process-buffer) 1) ;; Wait for the results and for first echo

    (get-buffer-create sqlite-output-buffer)
    )

  (- sqlite-descriptor-counter 1)
  )

(defun sqlite-bye (descriptor &optional noerror)
  "Finish the sqlite process sending the \".quit\" command.

Returns t if everything is fine.
nil if the DESCRIPTOR points to a non-existent process buffer.

If NOERROR is t, then will not signal an error when the DESCRIPTOR is not registered."
  (let ((process-buffer (sqlite-descriptor-buffer descriptor)))
    (if (get-buffer-process process-buffer)
	(progn ;; Process buffer exists... unregister it
	  (comint-redirect-send-command-to-process ".quit" sqlite-output-buffer (get-buffer-process process-buffer) nil t)
	  (sqlite-unregister-descriptor descriptor)
	  t
	  )
      (progn
	(sqlite-unregister-descriptor descriptor) ;; We unregister the descriptor nevertheless
	(unless noerror
	  (error "Process buffer doesn't exists for that descriptor")
	  )
	nil
	)
      )
    )
  )

(defconst sqlite-regexp-next-value "\\(\"[^\"]*\"\\|[^\"][^,]*\\)\\(,\\|$\\)"
  "Used when we want to take the next value. Must match up to the first \",\" that divides between column.")

(defun sqlite-take-next-value (line)
  "Take one value up to a \",\" from LINE. This considers \".
Return a list with two elements: (value rest-of-line)"
  (if (equal line "")
      nil
    (progn ;; Is not an empty line...
      (string-match sqlite-regexp-next-value line)
      (list
       (match-string-no-properties 1 line)
       (substring-no-properties line (match-end 0)))
      )
    )
  )

(defun sqlite-parse-line ()
  "Take one line from the current-buffer and parse it returning a list of elements per column."
  (let (
	(line (chomp (thing-at-point 'line)))
	(parsed nil)
	(next t)
	)
    (while next
      (setq next (sqlite-take-next-value line))
      (when next
	(add-to-list 'parsed (car next) t 'ignore)
	(setq line (cadr next))
	)
      )
    parsed
    )
  )

(defun sqlite-parse-result ()
  "Parse the results to create a list of header-list plus rows-lists.

Result: (header-list row1-list row2-list row3-list) "
  (let*  ((begin (goto-char (point-min)))		;4
	  (end (goto-char (point-max)))
	  (num-lines (count-lines begin end))
	  (counter 0)
	  (results-rows ()))
    (goto-char (point-min))
    (if (sqlite-error-line) ;; Check if it is an error line
	(error (concat "SQLite process error:" (chomp (buffer-string)))))
    ;; no error, as Fredie Mercury said: "show must go on..."
    (while ( < counter num-lines)
      (add-to-list 'results-rows (sqlite-parse-line) t 'ignore)
      (forward-line)
      (setq counter (+ 1 counter)))
    (car `(,results-rows))))

(defconst sqlite-regexp-error "Error:\\(.*\\)$"
  "This regexp must match the error return of SQLite. There must be a parenthesis surrounding the error message for matching it with:
    `match-string' 1
This is used for `sqlite-check-errors' for raising errors with messages.")

(defun sqlite-error-line ()
  "Return t if the current line at the `sqlite-output-buffer' buffer match the `sqlite-regexp-error'. Else, return nil."
  (with-current-buffer sqlite-output-buffer
    (if (string-match sqlite-regexp-error (chomp (thing-at-point 'line)))
	t
      nil)
    )
  )

(defvar sqlite-regexp-sqlite-command "^\\..*"
  "This regexp must match an SQLite command. This is used for identifying which is an SQL command and which is a proper SQLite command.")

(defun sqlite-prepare-query (sql-command)
  "Return the query prepared.

If the query start with \".\" means that is a SQLite command, so we don't add a \";\" at the end;
else, we add a \";\" beacuse it is an SQL command. Remember: two \";\" has no effect in SQLite! :)"
  (if (string-match sqlite-regexp-sqlite-command sql-command)
      sql-command
    (concat sql-command ";")))

(defun sqlite-query (descriptor sql-command)
  "Send a query to the SQLite process and return the result.

DESCRIPTOR is the Sqlite instance descriptor given by `sqlite-init'.

Result:
The result is a \"table\" like the following:

    (header-list row1-list row2-list row3-list)

"
  (let ((process-buffer (sqlite-descriptor-buffer descriptor)))
    (unless (get-buffer-process process-buffer)
      (error "SQLite process buffer doesn't exist!"))
    (with-current-buffer sqlite-output-buffer
      (erase-buffer)
      (comint-redirect-send-command-to-process
       (sqlite-prepare-query sql-command) ;; Sometimes developers don't want to add a ";" in the query...
       sqlite-output-buffer (get-buffer-process process-buffer) nil t)
      (accept-process-output (get-buffer-process process-buffer) 1)  ;need to wait to obtain results
      (let ((result (sqlite-parse-result))) ;; we want to return the result! not the erase-buffer return value
	(erase-buffer)
	result)
      )
    )
  )


(defun chomp (str)
  "Trim whitespace from string"
  (let ((s (if (symbolp str)(symbol-name str) str)))
    (save-excursion
      (while (and
	      (not (null (string-match "^\\( \\|\f\\|\t\\|\n\\)" s)))
	      (> (length s) (string-match "^\\( \\|\f\\|\t\\|\n\\)" s)))
	(setq s (replace-match "" t nil s)))
      (while (and
	      (not (null (string-match "\\( \\|\f\\|\t\\|\n\\)$" s)))
	      (> (length s) (string-match "\\( \\|\f\\|\t\\|\n\\)$" s)))
	(setq s (replace-match "" t nil s))))
    s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sqlite.el ends here
