;;; syncml.el -- An elisp implementation of a SyncML client.
;; $Id: syncml.el,v 1.7 2004/01/25 11:57:14 joergenb Exp $

;; Copyright (C) 2003 Jørgen Binningsbø 

;; Author: Jørgen Binningsbø <jb@pvv.org>
;; Maintainer: Jørgen Binningsbø <jb@pvv.org>
;; Version: 
;; Created: Jan 10 2003
;; Keywords: syncml xml network
;; URL: 

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is an attempt to let emacs behave as a syncml client.  It's
;; main focus is to allow the BBDB to be synced to any SyncML server,
;; or device.

;; This code borrows code mercilessly from other packages, notably
;; xml-rpc.el by Daniel Lundin

(require 'xml)
(require 'url)
(require 'url-util)
(require 'dom)
(require 'dom-loadsave)
(require 'xpath)

(require 'syncml-debug)
(require 'syncml-constants)
(require 'syncml-commands)

;;; Setting debug level to max:
(setq syncml-debug t)
(setq url-debug t)

(defvar syncml-host "http://localhost:8000/sync4j/sync")

(defvar syncml-target-locuri "localhost"
  "*The target host URI.")

(defvar syncml-source-locuri "bbdb@mymachine"
  "*The source host URI.)")

(defvar syncml-target-database "db"
  "*The target database")

(defvar syncml-source-database "db"
  "*The target database")

(defvar syncml-credential "notused?" 
  "*The credential name.")

(defvar syncml-user "syncml"
  "*The name of the user, sent in the <Auth> block")

(defvar syncml-passwd "syncml"
  "*The password of syncml-user.  if encrypted, this should be the MD5 or whatever representation")

(defvar syncml-use-md5 't
  "*If we shall use the md5 algorithm for the authentication")

(defvar syncml-current-sessionid ""
  "*The session id <SessionID> used by the current or just-completed SyncML session.")

(defvar syncml-current-msgid 1
  "*The message id <MsgID> used by within the current SyncML session.")

(defvar syncml-current-cmdid 1
  "*The command id <CmdID> for the current command within the current SyncML message.")

(defvar syncml-current-timestamp nil
  "*The timestamp at the start of the synchronization session. Inserted into the <Next> 
tag in the initialization package.")

(defvar syncml-previous-timestamp nil
  "*The timestamp at the previous synchronization.")

(defvar syncml-transmit-buffername "*syncml-transmit*"
  "The name of the buffer containing a syncml-message to transmit to the server")

(defvar syncml-response-buffername "*syncml-response*"
  "The name of the buffer containing the syncml-message returned from the server")

(defun syncml-create-sessionid () 
  "Sets the variable syncml-current-sessionid to a new session id. 

The session id shall be the same for a whole syncml operation.
The value is currently just a random number. 
At the same time, MsgID is set to 1, since MsgID shall be a number increasing 
from 1 within each unique session."
  (setq syncml-current-sessionid (abs (random t)))
  (setq syncml-current-msgid 1)
  (syncml-debug 1 'syncml "Created session id: %S" syncml-current-sessionid))

(defun syncml-increase-msgid (&optional sessionid) 
  "Increases the MsgID number with 1. CmdID is set to 1 at the same time."
  (setq syncml-current-msgid (+ syncml-current-msgid 1))
  (setq syncml-current-cmdid 1))

(defun syncml-increase-cmdid (&optional sessionid)
  "Increases the CmdID number with 1."
  (setq syncml-current-cmdid (+ syncml-current-cmdid 1)))


(defun syncml-init (&optional slow-sync)
  "Initialized a SyncML request. If SLOW-SYNC is 't, then a request for a slow sync is forced.

The response from server is stored in the SYNCML-RESPONSE-DOC variable, and it is the duty of 
the calling function to carry on sensible actions based on this response."
  (setq syncml-current-timestamp (format-time-string "%Y%m%dT%H%M%SZ" ))
  (syncml-create-sessionid) 
  (setq syncml-next-respuri nil)
  (syncml-send-message-with-curl (syncml-header t)))




(defun syncml-send-message-with-curl (doc &optional respuri)
  "This function sends a Document Object Model (DOM) document DOC to the syncml server, using curl (http://curl.haxx.se/) as a bearer. 
The response from the server is stored in the syncml-response-doc variable."

  (syncml-debug 1 'syncml-send-message-with-curl "Triggered")

  ;; clear whatever was in the transmit and reponse buffers before starting 
  (set-buffer (get-buffer-create syncml-response-buffername))
  (erase-buffer)
  (set-buffer (get-buffer-create syncml-transmit-buffername))
  (erase-buffer)
  (setq syncml-response-doc nil)
  
  ;; insert an xml representation of the DOM
  (insert (dom-node-write-to-string doc))
  
  (syncml-debug 1 'syncml-send-message-with-curl "Posting following message to server:\n %S" (buffer-string))

  ;; send the message to the server. the response is in SYNCML-REPONSE-BUFFER
  (if (not (null syncml-next-respuri))
      (progn (syncml-debug 1 'syncml-send-message-with-curl "Server URL: %s" syncml-next-respuri)
	     (call-process "curl" nil syncml-response-buffername nil 
			   "-silent"
			   "-H"
			   "Content-Type: application/vnd.syncml+xml; charset=\"UTF-8\""
			   "--data-binary" 
			   (encode-coding-string (concat
						  "<?xml version='1.0' encoding='UTF-8' ?>"
						  (dom-node-write-to-string doc))
						 'utf-8)
			   syncml-next-respuri))
    (progn (syncml-debug 1 'syncml-send-message-with-curl "Server URL: %s" syncml-host)
	   (call-process "curl" nil syncml-response-buffername nil 
			 "-silent"
			 "-H"
			 "Content-Type: application/vnd.syncml+xml; charset=\"UTF-8\""
			 "--data-binary" 
			 (encode-coding-string (concat
						"<?xml version='1.0' encoding='UTF-8' ?>"
						(dom-node-write-to-string doc))
					       'utf-8)
			 syncml-host)))
  
  
  ;; TODO: need to check the HTTP response code here!! 
  (set-buffer syncml-response-buffername)
  (syncml-debug 1 'syncml-send-message-with-curl "Reponse from server:\n %S" (buffer-string))
  
  ;; Process the buffer SYNCML-BUFFER. Builds up a DOM tree and stores it in syncml-response-doc."
  (syncml-debug 1 'syncml-send-message-with-curl "Parsing response and building a DOM representation.")
  
  ;; Start parsing from the <SyncML> tag
  (goto-char (point-min))
  (search-forward-regexp "<SyncML\\(>\\| xmlns=\\\"\\S *\\\">\\)") ;;multisync specifies the namespace, others i've tested don't have this
  (move-to-column 0)
  
  ;; Parse the rest of the buffer and store result in SYNCML-RESPONSE-DOC
  (setq syncml-response-doc 
	(dom-make-document-from-xml (car (xml-parse-region (point) (point-max)))))
  
  (syncml-debug 1 'syncml-send-message-with-curl "SyncML response successfully transformed into a DOM tree.")
  (syncml-debug 1 'syncml-send-message-with-curl "Syncml Message: %s" (dom-node-write-to-string syncml-response-doc))
  (syncml-debug 1 'syncml-send-message-with-curl "Searching for <RespURI> tag")
  (setq syncml-next-respuri 
	(dom-node-text-content 
	 (car (xpath-resolve (dom-document-element syncml-response-doc) "descendant::RespURI"))))
  (syncml-debug 1 'syncml-send-message-with-curl "<RespURI> is %S" syncml-next-respuri)
  
  )



  
(defun syncml-header (&optional slow-sync)  
  "Returns a syncml header <SyncHdr> tag.  If arguments are supplied, they
will be used instead of their respective global variables.

XML Definition: SyncHdr: (VerDTD, VerProto, SessionID, MsgID, Target, Source, RespURI?, NoResp?, Cred?, Meta?)"
  (let* ((syncml-transmit-doc (syncml-create-syncml-document))
	 (syncmlnode (dom-document-element syncml-transmit-doc))
	 ;; the <SyncHdr>
	 (synchdrnode (syncml-create-synchdr-command
		       syncml-transmit-doc 
		       (syncml-create-target-command syncml-transmit-doc syncml-target-locuri)
		       (syncml-create-source-command syncml-transmit-doc syncml-source-locuri)
		       (syncml-create-cred-command syncml-transmit-doc)))
	 ;; the <SyncBody>
	 (syncbodynode (syncml-create-syncbody-command syncml-transmit-doc))
	 ;; the <Alert> command
	 (alert-node (syncml-create-alert-command
		      syncml-transmit-doc 
		      nil
		      (if slow-sync
			  (syncml-create-data-command syncml-transmit-doc syncml-alert-slow-sync)
			(syncml-create-data-command syncml-transmit-doc syncml-alert-two-way))
		      (syncml-create-item-command 
		       syncml-transmit-doc
		       (syncml-create-target-command syncml-transmit-doc syncml-target-database)
		       (syncml-create-source-command syncml-transmit-doc syncml-source-database)
		       (syncml-create-meta-command syncml-transmit-doc
						   (syncml-create-metinf-anchor-command syncml-transmit-doc)))))
	 (devinf-data-node (syncml-create-data-command
			    syncml-transmit-doc
			    (syncml-create-devinf-devinf-command
			     syncml-transmit-doc
			     (syncml-create-devinf-datastore-command
			      syncml-transmit-doc
			      (syncml-create-devinf-sourceref-command syncml-transmit-doc syncml-source-database)
			      (syncml-create-devinf-rxpref-command syncml-transmit-doc 
								   (syncml-create-devinf-cttype-command
								    syncml-transmit-doc
								    "text/x-vcard")
								   (syncml-create-devinf-verct-command
								    syncml-transmit-doc
								    "2.1"))
			      (syncml-create-devinf-txpref-command syncml-transmit-doc 
								   (syncml-create-devinf-cttype-command
								    syncml-transmit-doc
								    "text/x-vcard")
								   (syncml-create-devinf-verct-command
								    syncml-transmit-doc
								     "2.1"))
			      (syncml-create-devinf-synccap-command 
			       syncml-transmit-doc
			       (list
				(syncml-create-devinf-synctype-command syncml-transmit-doc "1")
				(syncml-create-devinf-synctype-command syncml-transmit-doc "2")))))))		 
	 (put-node (syncml-create-put-command
		    syncml-transmit-doc
		    (syncml-create-item-command syncml-transmit-doc
						nil 
						(syncml-create-source-command syncml-transmit-doc "./devinf11")
						nil
						devinf-data-node)		    
		    (syncml-create-meta-command syncml-transmit-doc
						(syncml-create-metinf-type-command 
						 syncml-transmit-doc
						 "application/cnd.syncml-devinf+xml")))))
	 
    (bbdb-syncml-debug 1 'bbdb-syncml-synchronize "Done creating base DOM nodes.")
    ;; Add the <SyncHdr> and <SyncBody> nodes to the <SyncML> node.
    (dom-node-append-child syncmlnode synchdrnode)
    (dom-node-append-child syncmlnode syncbodynode)
    ;; add the <Alert> command as first child to the <SyncBody>
    (dom-node-append-child syncbodynode alert-node)
    (dom-node-append-child syncbodynode put-node)
    (bbdb-syncml-debug 1 'bbdb-syncml-synchronize "SyncML init DOM tree prepared.")
    (bbdb-syncml-debug 2 'bbdb-syncml-synchronize (dom-node-write-to-string syncml-transmit-doc 1))
    syncml-transmit-doc))
  

(defun syncml-process-response ()
  "Processes the response from the SyncML server.  
First it checks the header, and then processes each command in the body in turn."
  (syncml-debug 1 'syncml-process-response "Started")
  ;; process all commands in the <SyncBody> tag
  (dolist 
      (node (xpath-resolve (dom-document-element syncml-response-doc) 
			   "descendant::SyncBody/child::*")
	    nil)
    (syncml-process-command node)))

				
(defun syncml-process-command (node)
  "Processes the command NODE, which can be of any type (ALERT, ADD, GET, STATUS, SYNC...)"
  (syncml-debug 1 'syncml-process-command "Triggered. Is the node a dom-node?: %S" (dom-node-p node))
  (syncml-debug 1 'syncml-process-command "Is the node a dom-element?: %S" (dom-element-p node))
  (if (not (dom-element-p node))
      (throw 'wrong-type nil))
  (syncml-debug 1 'syncml-process-command "Name of node: %S" (dom-element-name node))
  (let ((node-element-name (dom-element-name node)))
    (cond ((string= "Status" node-element-name) 
	   (syncml-debug 1 'syncml-process-command "Calling syncml-process-status-command.")
	   (funcall 'syncml-process-status-command node))
	  ((string= "Alert" node-element-name) 
	   (syncml-debug 1 'syncml-process-command "Calling syncml-process-alert-command.")
	   (funcall 'syncml-process-alert-command node)))))


(defun syncml-process-status-command (node)
"Processes the NODE which must be a <Status> node

XML definition: <Status>: (CmdID, MsgRef, CmdRef, Cmd, TargetRef*, SourceRef*, Cred?, Chal?, Data, Item*)

TODO:  how to let this command control program flow ?"
(let ((syncml-cmd (dom-node-text-content (car (xpath-resolve node "child::Cmd"))))
      (syncml-data (dom-node-text-content (car (xpath-resolve node "child::Data")))))
  (syncml-debug 1 'syncml-process-status-command "This status refers to a %S command with data %S: %S" syncml-cmd syncml-data (syncml-lookup-response-code syncml-data)) 
  ;; the <Status> command is used as a response to many different request. The <Cmd> and <CmdRef> tells which.
  ;; further processing must be based on this. (ie: preserve the request in some way)
  (cond ((string= "SyncHdr" syncml-cmd)
	 (progn (syncml-debug 1 'syncml-process-status-command "CmdRef is SyncHdr")
		(cond ((string= "407" syncml-data)
		       (progn (syncml-debug 1 'syncml-process-status-command "Server said 407: Missing Credentials")) ;must break execution and resend credentials;
		       (error "Authentication rejected"))
		      ((not (string= syncml-data "212"))
		       (progn (syncml-debug 1 'syncml-process-status-command "ERROR. Server said: %S" syncml-data) 
			      (error syncml-data))))
		(syncml-debug 1 'syncml-process-status-command "We've got 212: Authentication accepted")))
	((string= "Alert" syncml-cmd)
	 (cond ((string= "404" syncml-data)
		(progn (syncml-debug 1 'syncml-process-status-command "Server said 404: Not found"))
		(error "Target database not found"))
	       ((string= "508" syncml-data)
		(progn (syncml-debug 1 'syncml-process-status-command "Server said 508: Refresh required. Initiating slow sync"))
		(setq syncml-doing-slow-sync 't))
	       ((not (string= syncml-data "200"))
		(progn (syncml-debug 1 'syncml-process-status-command "ERROR. Server said %S" syncml-data)
		       (error "Error in request"))))))
  (syncml-debug 1 'syncml-process-status-command "Finished.")))

(defun syncml-process-alert-command (node)
  "Processes the NODE which must be an <Alert> node"
  (syncml-debug 1 'syncml-process-alert-command "Triggered.")
  (let ((syncml-cmdid (dom-node-text-content (car (xpath-resolve node "child::CmdID"))))
	(syncml-data (dom-node-text-content (car (xpath-resolve node "child::Data")))))
    (syncml-debug 1 'syncml-process-alert-command "Server sent alert command %S: %S" syncml-data (syncml-lookup-alert-code syncml-data))
;;    (cond ((string= "404" syncml-data)
;;	   (progn (syncml-debug 1 'syncml-process-status-command "Server said 404: Not found"))
;;	   (error "Target database not found"))
;;	  ((string= "508" syncml-data)
;;	   (progn (syncml-debug 1 'syncml-process-status-command "Server said 508: Refresh required. Initiating slow sync"))
;;	   (setq syncml-doing-slow-sync 't))
;;	  ((not (string= syncml-data "200"))
;;	   (progn (syncml-debug 1 'syncml-process-status-command "ERROR. Server said %S" syncml-data)
;;		  (error "Error in request"))))
    ))




(defun syncml-process-response-buffer (syncml-buffer)
"Process the buffer SYNCML-BUFFER. Builds up a DOM tree and stores it in syncml-response-doc."
(syncml-debug 1 'syncml-process-response-buffer "Called.")
(set-buffer syncml-buffer)
(goto-char (point-min))
(search-forward-regexp "<SyncML\\(>\\| xmlns=\\\"\\S *\\\">\\)") ;;multisync specifies the namespace, others i've tested don't
(move-to-column 0)
(setq syncml-response-doc 
      (dom-make-document-from-xml (car (xml-parse-region (point) (point-max)))))
(syncml-debug 1 'syncml-process-response-buffer "SyncML response successfully transformed into a DOM tree"))



(defun syncml-get-temp-buffer-name ()
"Get a working buffer name such as ` *XML-RPC-<i>*' without a live process \
and empty it"
(let ((num 1)
      name buf)
  (while (progn (setq name (format " *SYNCML-%d*" num)
		      buf (get-buffer name))
		(and buf (or (get-buffer-process buf)
			     (save-excursion (set-buffer buf)
					     (> (point-max) 1)))))
    (setq num (1+ num)))
  name))



(defun syncml-lookup-response-code (code)
  "Returns the text associated with the SyncML response code CODE.
Searches the alist SYNCML-RESPONSE-CODES after CODE."
  (cdr (assoc code syncml-response-codes)))


(defun syncml-lookup-alert-code (code)
  "Returns the text associated with the SyncML alert code CODE.
Searches the alist SYNCML-ALERT-CODES after CODE."
  (cdr (assoc code syncml-alert-codes)))





;;
;; the following functions is maybe deprecated.
;; 
;; but i probably need to think more on callback.  what if a server decides to 
;; split his response in two or more messages, as allowed by the syncml spec. ?

(defun syncml-post-request (request)
"Post the REQUEST via http POST to the syncml server"
(let ((url-working-buffer (get-buffer-create
			   (syncml-get-temp-buffer-name)))
      (url-request-method "POST")
      (url-package-name "Lispmeralda-Emacs")
      (url-package-version "1.0")
      (url-request-data (concat "<?xml version=\"1.0\"?>\n" request))
      (url-request-extra-headers (cons
				  (cons  "Content-Type" "application/vnd.syncml+xml")
				  url-request-extra-headers)))
    
  (set-buffer url-working-buffer)
    
  ;; Set up asynchronous callback if requested
  (if async-callback-function
      (setq url-be-asynchronous t
	    url-current-callback-data (list async-callback-function
					    (current-buffer))
	    url-current-callback-func 'xml-rpc-request-callback-handler)
    (setq url-be-asynchronous nil))
    
  (url-retrieve server-url t)
    
  (if url-be-asynchronous
      nil
    (let ((result (xml-rpc-request-process-buffer url-working-buffer)))
      (kill-buffer (current-buffer))
      result))))




(defun syncml-init-old (&optional slow-sync)
"Initialized a SyncML request. If SLOW-SYNC is 't, then a request for a
slow sync is forced.
Returns TRUE if initialization went ok, and we can proceed with syncronization."
;;	(setq syncml-current-timestamp (format-time-string "%Y%m%dT%H%M%SZ%z" ))
(setq syncml-current-timestamp (format-time-string "%Y%m%dT%H%M%SZ" ))
(syncml-create-sessionid)
(setq url-debug 't)
(set-buffer (get-buffer-create syncml-transmit-buffername))
(erase-buffer)
(insert "\n<SyncML>")
(insert (syncml-header))
(insert "<SyncBody>")
(if slow-sync
    (insert (syncml-create-alert-command syncml-alert-slow-sync))
  (insert (syncml-create-alert-command syncml-alert-two-way)))
(insert "<Final/></SyncBody>\n</SyncML>\n")
  
(let ((url-working-buffer (get-buffer-create
			   (syncml-get-temp-buffer-name)))
      (url-request-method "POST")
      (url-package-name "Lispmeralda-Emacs")
      (url-package-version "1.0")
      (url-request-data (concat "<?xml version=\"1.0\"?>\n" (buffer-string)))
      (url-request-extra-headers (cons
				  (cons  "Content-Type" "application/vnd.syncml+xml")
				  url-request-extra-headers)))
    
  ;;		(set-buffer url-working-buffer)
  (syncml-debug 1 'syncml-post "Posting:\n %S" url-request-data) 
  (kill-buffer (get-buffer syncml-response-buffername))
  (set-buffer (get-buffer-create syncml-response-buffername))
    
  ;; this actually sends the init command to the server
  (insert-buffer (url-retrieve-synchronously syncml-host))
    
  ;; TODO: need to check the HTTP response code here!! 
  (syncml-debug 1 'syncml-post "Got:\n %S" (buffer-string))
    
  (syncml-process-response)))


(defun syncml-send-message-with-url (doc)
"This function sends a Document Object Model (DOM) document DOC to the syncml server.  The response from the 
server is stored in the syncml-response-doc variable."

(syncml-debug 1 'syncml-send-message "Triggered")

;; clear whatever was in the transmit buffer before starting 
(set-buffer (get-buffer-create syncml-transmit-buffername))
(erase-buffer)	  

;; insert an xml representation of the DOM
(insert (dom-node-write-to-string doc))
  

(let ((url-working-buffer (get-buffer-create
			   (syncml-get-temp-buffer-name)))
      (url-request-method "POST")
      (url-package-name "Lispmeralda-Emacs")
      (url-package-version "1.0")
      (url-http-transfer-encoding "iso-8859-1")
      (url-request-data (concat "<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>\n" (buffer-string)))
      (url-request-extra-headers (cons
				  (cons "Content-Type" "application/vnd.syncml+xml")
				  url-request-extra-headers)))
    
  ;;		(set-buffer url-working-buffer)
  (syncml-debug 1 'syncml-send-message "Posting to server:\n %S" url-request-data)
  (kill-buffer (get-buffer syncml-response-buffername))
  (set-buffer (get-buffer-create syncml-response-buffername))
    
  ;; this actually sends the syncml message to the server
  (insert-buffer (url-retrieve-synchronously syncml-host))
    
  ;; TODO: need to check the HTTP response code here!! 
  (syncml-debug 1 'syncml-send-message "Reponse from server:\n %S" (buffer-string))
    
  ;; Process the buffer SYNCML-BUFFER. Builds up a DOM tree and stores it in syncml-response-doc."
  (syncml-debug 1 'syncml-process-response-buffer "Parsing response and building a DOM representation.")

  ;; Start parsing from the <SyncML> tag
  (goto-char (point-min))
  (search-forward-regexp "<SyncML\\(>\\| xmlns=\\\"\\S *\\\">\\)") ;;multisync specifies the namespace, others i've tested don't have this
  (move-to-column 0)

  ;; Parse the rest of the buffer and store result in SYNCML-RESPONSE-DOC
  (setq syncml-response-doc 
	(dom-make-document-from-xml (car (xml-parse-region (point) (point-max)))))

  (syncml-debug 1 'syncml-send-message "SyncML response successfully transformed into a DOM tree.")))



(defun syncml-header-old ( &optional sessionid msgid target source)  
"Returns a syncml header <SyncHdr> tag.  If arguments are supplied, they
will be used instead of their respective global variables.

XML Definition: SyncHdr: (VerDTD, VerProto, SessionID, MsgID, Target, Source, RespURI?, NoResp?, Cred?, Meta?)
TODO: use xml-mode for this?"
(concat 
 "<SyncHdr>
		  <VerDTD>1.1</VerDTD>
		  <VerProto>SyncML/1.1</VerProto>
		  <SessionID>" (number-to-string syncml-current-sessionid) "</SessionID>
		  <MsgID>" (number-to-string syncml-current-msgid) "</MsgID>
		  <Target><LocURI>" syncml-target-locuri "</LocURI></Target>
		  <Source><LocURI>" syncml-source-locuri "</LocURI></Source> 
<Cred>
<Meta><Type xmlns='syncml:metinf'>"
 (if  syncml-use-md5
     (concat "syncml:auth-md5</Type></Meta>\n<Data>" (base64-encode-string (md5 (concat syncml-user ":" syncml-passwd))))
   (concat "syncml:auth-basic</Type></Meta>\n<Data>" syncml-user ":" syncml-passwd)) 
 "</Data></Cred>
	</SyncHdr>"))

(defun syncml-create-alert-command-old (alert-command-number &optional target-database source-database prev-timestamp next-timestamp)
"Returns a string with the <Alert> command with the given ALERT-COMMAND-NUMBER

NEED TO MAKE THESE REAL XML-ELEMENTS INSTEAD OF QUASI-TEXT

XML definition: 
Alert: (CmdID, NoResp?, Cred?, Data?, Item*)
Data: When specified in an Alert, the element type specifies the type of alert. 
Item: When specified in an Alert, the element type specifies the
      parameters for the alert type. (Target?, Source?, Meta?, Data?)
"
(concat
 "<Alert>
       <CmdID>" (number-to-string syncml-current-cmdid) "</CmdID>
       <Data>" (number-to-string alert-command-number) "</Data>
       <Item>
          <Target><LocURI>" syncml-target-database "</LocURI></Target>
				  <Source><LocURI>" syncml-source-database "</LocURI></Source>
				  <Meta>
					   <Anchor xmlns=\"syncml:metinf\">
						   <Last>" syncml-previous-timestamp "</Last>
						   <Next>" syncml-current-timestamp "</Next>
					   </Anchor>
				  </Meta>
			 </Item>
    </Alert>"))

;;(defun syncml-create-status-command (status-command-number &optional target-database source-database prev-timestamp next-timestamp) 
;;  "Returns a string with the <Status> command with the given STATUS-COMMAND-NUMBER 
;;XML definition: 
;;(CmdID, MsgRef, CmdRef, Cmd, TargetRef*, SourceRef*, Cred?, Chal?, Data, Item)jfdls."
;; (concat         
;;	  "<Status>   
;;            <CmdID>" (number-to-string syncml-current-cmdid) "</CmdID> 
;;            <MsgRef>" (number-to-string syncml-current-cmdid) "</MsgRef>
;;            <CmdRef>" (number-to-string syncml-current-cmdid) "</CmdRef> 
;;            <Cmd>" (number-to-string syncml-current-cmdid) "</Cmd>
;;            <TargetRef>" (number-to-string syncml-current-cmdid) "</TargetRef>
;;            <SourceRef>" (number-to-string syncml-current-cmdid) "</SourceRef>
;;            <Cmd>" (number-to-string syncml-current-cmdid) "</Cmd>
;;            <Data>" (number-to-string status-command-number) "</Data>
;;       <Item>
;;         <Target><LocURI>" syncml-target-database "</LocURI></Target>
;;				  <Source><LocURI>" syncml-source-database "</LocURI></Source>
;;				  <Meta>
;;					   <Anchor xmlns=\"syncml:metinf\">
;;						   <Last>" syncml-previous-timestamp "</Last>
;;						   <Next>" syncml-current-timestamp "</Next>
;;					   </Anchor>
;;				  </Meta>
;;			 </Item>
;;   </Status>"))



;;(defun syncml-create-add-command (target-locuri data)
;;  "Returns a string with the <Add> command with the given TARGET-LOCURI and DATA.
;;The TARGET-LOCURI in an <Add> command is defined to be relative to the <Target> in the parent <Sync> command.


;;NEED TO MAKE THESE REAL XML-ELEMENTS INSTEAD OF QUASI-TEXT

;;XML definition: 
;;CmdID, NoResp?, Cred?, Meta?, Item+)
;;" 
;;  (setq syncml-current-cmdid (+ syncml-current-cmdid 1));
;;  (concat "
;;     <Add> 
;;     <CmdID>" (number-to-string syncml-current-cmdid) "</CmdID>
;;   <Item>
;;	  <Source><LocURI>" target-locuri "</LocURI></Source>
;;	  <Data>
;;" data "
;;          </Data>
;;	</Item>
;;    </Add>"))





(provide 'syncml)
