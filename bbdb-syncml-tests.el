;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.


(bbdb-record-set-firstname (car (bbdb-records)) "jøø.")
(bbdb-create-internal "magnhild sægrov" "apalløkka" "ms@underdusken.no" nil nil nil)
(bbdb-save-db)

(bbdb-search (bbdb-records) nil nil nil )
(bbdb-record-name)

(bbdb-field-edit-get-values (car (bbdb-records)) "luid")

(bbdb-record-getprop (bbdb-records) "net")

(setq gabba (bbdb-records))

(require 'bbdb-vcard-export)
(defun garr ()
	(set-buffer (get-buffer-create "*foo"))
	(dolist (node (bbdb-records) nil)
		(bbdb-vcard-export-record-insert-vcard node)
		(insert "\n")))

(garr)

(setq bbdb-syncml-last-sync-timestamp "20030205T172452Z")

(bbdb-syncml-get-added-records)
(bbdb-syncml-debug 'gabba 
									 "the following luids are modified: %S"
									 (bbdb-syncml-get-modified-records bbdb-syncml-last-sync-timestamp)
)


(require 'bbdb-syncml)
(bbdb-syncml-initialize)

(bbdb-syncml-validate-luids)

(setq gabba (bbdb-syncml-get-next-luid))

;; DENNE SØKEFUNKSJONEN FUNGERER:
(let ((notes (cons (intern "luid") "^1$")))
	(bbdb-search (bbdb-records) nil nil nil notes))

;; FRAMSTØYT PÅ Å ENDRE RECORD BASERT PÅ SØKEFUNKSJON.
(apply 'bbdb-record-edit-field-internal 
			 (let ((notes (cons (intern "luid") "^1$")))
				 (bbdb-search (bbdb-records) nil nil nil notes))
			 'luid)
			 
; slik kan ein endre ein property for ein gjeven bbdb-record)
(let ((notes (cons (intern "luid") "^1$")))
	((let ((node bbdb-search (bbdb-records) nil nil nil notes))
		 (bbdb-record-putprop node 
											 (if (string= "" string) nil string)))



(bbdb-search (bbdb-records) nil nil nil (cons (intern "luid") "2"))

(bbdb-records)

(bbdb-search-prompt)

(require 'bbdb-syncml)

(bbdb-syncml-get-last-sync)

(let ((records (bbdb-records)))
	(while records
		(insert (bbdb-record-getprop (car records) 'company)))

(bbdb-syncml-validate-luids)