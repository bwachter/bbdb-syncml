;; this file just contains some tests used when developing bbdb-syncml

(require 'dom-loadsave)

;; how to create elements:

(let* ((myxml "<node1><node2>hei</node2></node1>")
       (syncmldoc (make-dom-document
		   :name "hei"
		   :type dom-document-node)))      
  (dom-document-create-element syncmldoc "SyncHdr")
  syncmldoc)



(let* ((myxml "<node1><node2>hei</node2></node1>")
       (syncmldoc (make-dom-document
		   :name "MySyncMLDocument"
		   :type dom-document-node
		   :element (make-dom-element 
			     :name "SyncML" 
			     :type dom-element-node)))
       (mynode (make-dom-text 
		:name "SyncHdrasdfsadfa"
		:type dom-text-node
		:value "sdfasfsdfsd"))
       )
  
  (dom-node-append-child (dom-document-element syncmldoc) mynode)
  (dom-node-write-to-string syncmldoc))
;  syncmldoc)


(let* ((mynode (make-dom-element 
		:name "SyncHdr"
		:type dom-element-node))
       )
  mynode)




(let* ((syncmldoc (make-dom-document :name "MySyncMLDocument"
				     :type dom-document-node))
       (syncmlelement (dom-document-create-element syncmldoc "SyncML")))
  (setf (dom-document-owner-document syncmldoc) syncmldoc
	(dom-document-element syncmldoc) syncmlelement)
  (dom-node-append-child syncmlelement (syncml-create-data-command syncmldoc "syncml:sdfsdfj"))
  (dom-node-write-to-string syncmldoc))
  ;;  syncmldoc-element)
  syncmldoc)

(setq doc (syncml-create-syncml-document))

(let* ((mydoc (syncml-create-syncml-document))
       (crednode (syncml-create-cred-command mydoc))
       (mysyncml (dom-document-element mydoc)))
  (dom-node-append-child mysyncml 
			 (syncml-create-synchdr-command 
			  mydoc 
			  (syncml-create-target-command mydoc syncml-target-locuri)
			  (syncml-create-source-command mydoc syncml-source-locuri)
			  (syncml-create-cred-command mydoc)))
  (dom-node-append-child mysyncml (syncml-create-target-command mydoc "hei" "hå"))
  (dom-node-write-to-string mydoc))




(let* ((mydoc (syncml-create-syncml-document))
       (mysyncml (dom-document-element mydoc)))
  (setq grrr (syncml-create-metinf-type-command mydoc "hei"))			 
  (dom-node-append-child mysyncml grrr)
  (dom-node-write-to-string mydoc))

(dom-element-p grrr)
(dom-node-has-attributes (dom-node-first-child grrr))
(dom-node-has-attributes  grrr)
(dom-node-attributes grrr)
(dom-node-attributes (dom-node-first-child grrr))
(dom-attr-p )

(let (res)
  (dolist (innernode (dom-node-attributes grrr) res)
    (if (dom-attr-p innernode)
	(push (concat (if (symbolp (dom-node-name innernode))
			  (symbol-name (dom-node-name innernode))
			(dom-node-name innernode))
		      "='" 
		      (if (symbolp (dom-node-value innernode))			
			  (symbol-name (dom-node-value innernode))
			(dom-node-value innernode))
		      "'")
	      res)))
  (mapconcat 'concat res " "))



(dom-node-p doc)
(dom-document-p doc)
(dom-element-p (dom-document-element doc))

(number-p 1)


(dom-node-write-to-string (syncml-create-meta-command "syncml:sdfsdfj"))
(dom-node-write-to-string 
 (syncml-create-meta-command 
  (dom-node-append-child (make-dom-attr :name "xmlns" 
					:type dom-attribute-node
					:value "syncml:metinf")
			 (make-dom-element :name "Type" 
					   :type dom-element-node) 
			 )))
(dom-node-write-to-string 
 (syncml-create-meta-command 
  1))


  
  (dom-node-append-child (dom-document-element syncmldoc) mynode)
  (dom-node-write-to-string syncmldoc))
;  syncmldoc)




(dom-make-node-from-xml "<?xml version='1.0'?><n><n2/></n>" (make-dom-document :name "hei" :type dom-document-node))


(let* ((myxml "<node1><node2>hei</node2></node1>")
       (doc (make-dom-document
	     :name "SyncML"
	     :type dom-document-node))
       
       (node (make-dom-node dom-make-node-from-xml myxml doc)))
  (setf (dom-document-owner-document doc) doc; required in dom-add-children
	(dom-document-element doc) node)
  doc)







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
(xml-parse-file "/home/jb/src/lisp/xmltest.xml")q

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