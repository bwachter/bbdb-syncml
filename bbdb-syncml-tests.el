;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.





;
; I AM HERE NOW
;
(require 'bbdb-syncml)
(require 'bbdb-vcard-export)
(require 'md5 "/home/jb/src/lisp/md5.el")
(setq bbdb-syncml-debug-level 3)
(setq syncml-debug-level 3)
(setq syncml-host "http://binningsbo.homelinux.org:8080/multisync")
(setq syncml-target-locuri "binningsbo.homelinux.org")
(setq syncml-source-locuri "jb_bbdb")
(setq syncml-target-database "addressbook")
(setq syncml-user "syncml")
(setq syncml-passwd "syncml")
(setq syncml-use-md5 'nil)
(bbdb-syncml-synchronize)

(md5 "syncml:syncml")
(base64-encode-string "syncml:syncml")
(md5 (base64-encode-string "syncml:syncml"))
(base64-encode-string (md5 "syncml:syncml"))

H79Dmt0WCk2Xkb1FjJqarg==

(if 'syncml-use-md5
    (md5 syncml-passwd)
  syncml-passwd)

wlTB98Ptd0oT5eovYGGGgg==


(bbdb-syncml-validate-luids 't)


(bbdb-syncml-get-added-records)
(bbdb-syncml-get-modified-records)


;; DENNE SØKEFUNKSJONEN FUNGERER:

(let ((notes (cons (intern "luid") "^5$")))
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








(bbdb-record-set-firstname (car (bbdb-records)) "jøø.")
(bbdb-create-internal "magnhild sægrov" "apalløkka" "ms@underdusken.no" nil nil nil)
(bbdb-save-db)

(bbdb-search (bbdb-records) nil nil nil )
(bbdb-record-name)

(bbdb-field-edit-get-values (car (bbdb-records)) "luid")

(bbdb-record-getprop (bbdb-records) "net")

(setq gabba (bbdb-records))


(defun garr ()
	(set-buffer (get-buffer-create "*foo"))
	(dolist (node (bbdb-records) nil)
		(bbdb-vcard-export-record-insert-vcard (car (bbdb-syncml-get-record-by-luid "5")))
		(insert "\n")))

(garr)

(setq bbdb-syncml-last-sync-timestamp "20030205T172452Z")

(bbdb-syncml-get-added-records)
(bbdb-syncml-debug 'gabba 
				   "the following luids are modified: %S"
				   (bbdb-syncml-get-modified-records bbdb-syncml-last-sync-timestamp)
)




(bbdb-syncml-initialize) ;; only 1st time

(setq bbdb-syncml-debug-level 2)








(bbdb-search (bbdb-records) nil nil nil (cons (intern "luid") "2"))

(bbdb-records)

(bbdb-search-prompt)

(require 'bbdb-syncml)

(bbdb-syncml-get-last-sync)

(let ((records (bbdb-records)))
	(while records
		(insert (bbdb-record-getprop (car records) 'company)))

(bbdb-syncml-validate-luids)