;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.





;
; I AM HERE NOW
;
(require 'bbdb-syncml)
(require 'bbdb-vcard-export)
(require 'md5 "/home/jb/src/lisp/md5.el")
(setq bbdb-syncml-debug-level 3
      syncml-debug-level 3
      bbdb-syncml-debug-duplicate-in-syncml-debug t
      syncml-host "http://binningsbo.homelinux.org:8080/multisync"
      syncml-target-locuri "http://binningsbo.homelinux.org:8080/multisync"
      syncml-source-locuri "bbdb://localhost/"
      syncml-target-database "addressbook"
;;      syncml-target-database "contacts"
      syncml-source-database "bbdb"
      syncml-user "syncml"
      syncml-passwd "syncml"
      syncml-use-md5 'nil)
(global-set-key [?\C-c ?\C-b ?\C-s] 'bbdb-syncml-synchronize)
(global-set-key (kbd "C-c C-x s") 'bbdb-syncml-synchronize)

(bbdb-syncml-synchronize)



(encode-coding-string "hei" 'futf-8)

(insert bbdb-syncml-package-5-doc

(dom-node-write-to-string (bbdb-syncml-process-modifications-response bbdb-syncml-package-5-doc)))
(dom-node-write-to-string bbdb-syncml-package-5-doc 2)


(md5 "syncml:syncml")
(base64-encode-string "syncml:syncml")
(md5 (base64-encode-string "syncml:syncml"))
(base64-encode-string (md5 "syncml:syncml"))
(setq syncml-alert-two-way "200")
(setq syncml-alert-slow-sync "201")

(let ((proc (start-process "wget" (format "*wget %s" syncml-host)
			   "wget" "--post-data" syncml-host))
      (with-current-buffer (process-buffer proc)
	
(shell-command "wget" (syncml-get-temp-buffer-name) "wget" "http://binningsbo.homelinux.org:8080/multisync")

(syncml-header t)

(call-process "curl" nil "hei" nil 
	      "-silent"
	      "-H"
	      "Content-Type: application/vnd.syncml+xml"
	      "-d" 
	      test-msg-1
	      "http://binningsbo.homelinux.org:8080/multisync")


)

(call-process "curl" nil "hei" nil 
;;	      (shell-quote-argument
	      "-silent"
	      "-H"
	      "Content-Type: application/vnd.syncml+xml"
	      "-d" 
	      "<?xml version='1.0' ?> <SyncML></SyncML>" 
	      "http://binningsbo.homelinux.org:8080/multisync")
)
	      

(setq test-msg-1 "<?xml version=\"1.0\"?>
<SyncML>
  <SyncHdr>
		  <VerDTD>1.1</VerDTD>
		  <VerProto>SyncML/1.1</VerProto>
		  <SessionID>17913676</SessionID>
		  <MsgID>1</MsgID>
		  <Target><LocURI>binningsbo.homelinux.org</LocURI></Target>
		  <Source><LocURI>bbdb://localhost/</LocURI></Source> 
<Cred>
<Meta><Type xmlns='syncml:metinf'>syncml:auth-basic</Type></Meta>
<Data>syncml:syncml</Data></Cred>
	</SyncHdr><SyncBody><Alert>
       <CmdID>1</CmdID>
       <Data>200</Data>
       <Item>
          <Target><LocURI>addressbook</LocURI></Target>
				  <Source><LocURI>bbdb</LocURI></Source>
				  <Meta>
					   <Anchor xmlns=\"syncml:metinf\">
						   <Last></Last>
						   <Next>20031208T230933Z</Next>
					   </Anchor>
				  </Meta>
			 </Item>
    </Alert><Final/></SyncBody>
</SyncML>")


;;  (if url
 ;;     (let ((proc (start-process "wget" (format "*wget %s*" url)
;;				 "wget" "--passive-ftp" "-nv" 
;;				 "-P" (expand-file-name loc) url)))
;;	(with-current-buffer (process-buffer proc)
;;	  (erase-buffer))
;;	(set-process-sentinel proc (lambda (proc str)
;;				     (message "wget download done"))))
;;    (message "Nothing to get"))))






H79Dmt0WCk2Xkb1FjJqarg==

(if 'syncml-use-md5
    (md5 syncml-passwd)
  syncml-passwd)

wlTB98Ptd0oT5eovYGGGgg==
(dom-node-write-to-string (dom-document-element syncml-response-doc))

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