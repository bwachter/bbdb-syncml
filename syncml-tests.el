;; this file just contains some tests used when developing bbdb-syncml

(require 'syncml)

(syncml-init)

(syncml-process-response)



(dom-node-text-content 
 (car (xpath-resolve (dom-document-element syncml-response-doc) "descendant::Status/child::Data")))

(safe-length (xpath-resolve (dom-document-element syncml-response-doc) "descendant::SyncBody/child::*"))





(setq time-stamp-format "%D")
(format-time-string "%Y%m%dT%H%M%SZ%z" )



(sync-test "/home/jb/src/lisp/s4j_init.xml")

(defun syncml-get-response-doc () 
	(dom-make-document-from-xml (car (syncml-process-response-buffer (get-buffer "*syncml-response*")))))


(setq gabbba (dom-node-text-content (dom-document-get-elements-by-tag-name syncml-response-doc "RespURI")))
(setq gab-children (dom-node-text-content (dom-document-element syncml-response-doc)))

(setq grr (dom-node-text gabba))

(setq syncml-host "http://80.203.35.204:8000/sync4j/sync")
(setq syncml-target-respuri

(setq syncml-target-locuri "80.203.35.204")



 (require 'url-http)
 (require 'url)
 (url-http-parse-headers))










;; xml-rpc related:

(require 'xml-rpc "/home/jb/src/lisp/xml-rpc.el")
(defun cb-foo (foo)
	(print (format "%s" foo))) 

(setq xml-rpc-debug 3)

(xml-rpc-method-call "http://time.xmlrpc.com/RPC2"
										 'currentTime.getCurrentTime)

(xml-rpc-method-call-async 'cb-foo
													 "http://time.xmlrpc.com/RPC2"
													 'currentTime.getCurrentTime)

(setq gabba (xml-rpc-method-call "http://time.xmlrpc.com/RPC2"
																 'currentTime.getCurrentTime))

(set-buffer (get-buffer-create "xml-rpc-response"))
(insert-buffer (xml-rpc-method-call "http://time.xmlrpc.com/RPC2"
																		 'currentTime.getCurrentTime))


 (url-http-parse-headers)


;; other stuff:

(setq init-thing-doc (dom-make-document-from-xml (car (xml-parse-file "/home/jb/src/lisp/s4j_init.xml"))))
(xml-parse-file "/home/jb/src/lisp/xmltest.xml")

(setq doc (dom-make-document-from-xml 
					 (car (xml-parse-file "/home/jb/src/lisp/xmltest.xml"))))

		
(dom-node-write-to-string doc)


(dom-document-p doc)
(dom-document-element doc)
(dom-node-first-child doc)

	
(setq doc (dom-make-document-from-xml 
						(car (xml-parse-file "/home/jb/src/lisp/xmltest.xml"))))



(defun batch-test ()
	(message "starting...")
  (set-buffer (get-buffer '"foo"))
  (insert-buffer (syncml-post-request '"http://www.yahoo.com/"))
	(message "done"))
	
(batch-test)

(setq url-request-method "POST")

(setq url-debug 't)




(defun syncml-test-parse-response (buffer-name) 
	"parses the response from the server, and puts it into the syncml-response variable"
	((set-buffer 'buffer-name)
	 
	
(setq syncml-host "http://80.203.35.204:8000/sync4j/syncccc")

(require 'syncml "/home/jb/src/lisp/syncml.el")
(syncml-header)
(syncml-debug 'hei "gabba")



		(set
		(url-retrieve syncml-host (lambda (&rest ignored)
																(url-debug 'retrieval "Synchronous fetching done (%S)" (current-buffer))
																(setq retrieval-done t
																			asynch-buffer (current-buffer))))))




(require 'syncml "/home/jb/src/lisp/syncml.el") 