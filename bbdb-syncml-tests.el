                                        ;
                                        ; I AM HERE NOW
                                        ;
(require 'bbdb-syncml)
(require 'vcard)
(require 'bbdb-vcard-export)
(require 'bbdb-vcard)
(require 'md5)
(setq bbdb-syncml-debug-level 2
      syncml-debug-level 2
      bbdb-syncml-debug-duplicate-in-syncml-debug t
      syncml-host "http://binjpowerbook.kicks-ass.net:8080/sync4j/sync"
      syncml-target-locuri "http://binjpowerbook.kicks-ass.net"
      syncml-source-locuri "bbdb"
      syncml-target-database "rawcontact"
      syncml-source-database "bbdb"
      syncml-user "binj"
      syncml-passwd "binj"
      syncml-use-md5 'nil
      bbdb-syncml-debug-level 4
      syncml-debug-level 2
      )
(global-set-key [?\C-c ?\C-b ?\C-s] 'bbdb-syncml-synchronize)
(global-set-key (kbd "C-c C-x s") 'bbdb-syncml-synchronize)
(global-set-key (kbd "C-c C-x S") 'bbdb-syncml-synchronize-slow)

(bbdb-syncml-synchronize)

;;  RUN THIS ONLY BEFORE SYNCing THE FIRST TIME!
(bbdb-syncml-initialize)

(describe-variable bbdb-change-hook)q

(car (bbdb-syncml-vcard-lookup-location-mapping "Mobile"))

(bbdb-syncml-vcard-get-bbdb-record-as-vcard-string (car (bbdb-records)))
(car (bbdb-record-phones (car (bbdb-records))))

(bbdb-phone-location (car (bbdb-record-phones (car (bbdb-records)))))

(setq bbdb-syncml-pkg4-doc nil)


(dom-node-write-to-string bbdb-syncml-pkg4-doc)
(setq dummy-body-node (xpath-resolve (dom-document-element bbdb-syncml-pkg4-doc)
                                     "child::SyncML/child::SyncBody/Add']"))

(setq pkg4-syncbody (xpath-resolve (dom-document-element bbdb-syncml-pkg4-doc)
                                   "descendant::SyncBody"))

(setq pkg4-statuses (xpath-resolve (car pkg4-syncbody) "child::Status"))
(setq pkg4-add-statuses (xpath-resolve (car pkg4-syncbody) "child::Status[child::Cmd='Add']"))

(length pkg4-statuses)

(dom-node-write-to-string (car pkg4-statuses))
(dom-node-write-to-string pkg4-statuses)

(mapconcat 'concat (list "12" "34" "45") " ")


(dolist (grnode pkg4-statuses)
  (message "x")
  (if (dom-node-p grnode)
      (message "yes")
    (message "no"))
  (message grnode)
  (dom-node-write-to-string grnode))



(


 (xpath-resolve dummy-body-node "Status[child::Cmd='Add']")

 (dolist (node (xpath-resolve (car dummy-body-node) "Status/SourceRef"))
   (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response
                      "This is a <Status> in response to an <Add> with luid %s"
                      (dom-node-text-content (xpath-resolve node "child::SourceRef"))))

 (dom-node-text-content (xpath-resolve bbdb-syncml-pkg4-doc "child::SourceRef"))



 (dolist (node (xpath-resolve (car dummy-body-node) "child::Status[child::Cmd='Add']"))
   (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response "This "))

 (bbdb-syncml-debug 1 'test (dom-node-write-to-string syncml-response-doc))






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




    (bbdb-vcard-snarf "
BEGIN:VCARD
VERSION:2.1
X-EVOLUTION-FILE-AS;CHARSET=UTF-8:Binningsbø, Jørgen
FN;CHARSET=UTF-8:Jørgen Binningsbø¸
N;CHARSET=UTF-8:Binningsbø¸;Jørgen
TEL;CELL:12345678
TEL;HOME:87654321
EMAIL;INTERNET:jb@pvv.org
UID:pas-id-4012D9CE00000004
END:VCARD
")


    (bbdb-vcard-format-entry "BEGIN:VCARD
VERSION:2.1
FN:Tarek Yousef
N:Yousef;Tarek;;;
END:VCARD")


    (vcard-values (vcard-parse-string "
BEGIN:VCARD
VERSION:2.1
X-EVOLUTION-FILE-AS;CHARSET=UTF-8:BinningsbÃ¸, JÃ¸rgen
FN;CHARSET=UTF-8:JÃ¸rgen BinningsbÃ¸
N;CHARSET=UTF-8:BinningsbÃ¸;JÃ¸rgen
TEL;CELL:12345678
TEL;HOME:87654321
EMAIL;INTERNET:jb@pvv.org
UID:pas-id-4012D9CE00000004
END:VCARD
") (list "tel"))


    (bbdb-vcard-phonevec (vcard-ref (vcard-parse-string "
BEGIN:VCARD
VERSION:2.1
X-EVOLUTION-FILE-AS;CHARSET=UTF-8:BinningsbÃ¸, JÃ¸rgen
FN;CHARSET=UTF-8:JÃ¸rgen BinningsbÃ¸
N;CHARSET=UTF-8:BinningsbÃ¸;JÃ¸rgen
TEL:12345678
TEL:55555555
TEL;HOME:87654321
EMAIL;INTERNET:jb@pvv.org
UID:pas-id-4012D9CE00000004
END:VCARD
") (list "tel")))

    (list "hei" "Hå")
    (vector "hei" "hå")
    (cons "Hei")

    (length (list "hei" "Hå"))
    (length (vector "hei" "hå"))
    (length (cons "Hei" "hå"))



    (bbdb-record-set-firstname (car (bbdb-records)) "jøø.")
    (bbdb-record-putprop
     (bbdb-create-internal "magnhild sægrov" "apalløkka" "ms@underdusken.no" nil nil nil)
     'luid
     "15")











    (encode-coding-string "hei" 'utf-8)

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
                        ;;            (shell-quote-argument
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
      ;;                                 "wget" "--passive-ftp" "-nv"
      ;;                                 "-P" (expand-file-name loc) url)))
      ;;        (with-current-buffer (process-buffer proc)
      ;;          (erase-buffer))
      ;;        (set-process-sentinel proc (lambda (proc str)
      ;;                                     (message "wget download done"))))
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


      (bbdb-record-getprop (car (bbdb-records)) 'luid)


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