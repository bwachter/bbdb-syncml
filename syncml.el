;;; syncml.el -- An elisp implementation of a SyncML client.
;; $Id: syncml.el,v 1.1 2003/02/06 14:14:22 joergenb Exp $

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

;; This code steals code mercilessly from other packages, notably
;; xml-rpc.el by Daniel Lundin

(require 'xml)
(require 'url)
(require 'url-util)
(require 'dom)
(require 'dom-loadsave)
(require 'xpath)

(require 'syncml-debug)
(require 'syncml-constants)

;;; Setting debug level to max:
(setq syncml-debug t)
(setq url-debug t)

(defvar syncml-host "http://80.203.35.204:8000/sync4j/sync")

(defvar syncml-target-locuri "80.203.35.204"
  "*The target host URI.")

(defvar syncml-source-locuri "myBBDBdatabase"
  "*The source host URI.)")

(defvar syncml-target-database "db"
	"*The target database")

(defvar syncml-source-database "db"
	"*The target database")


(defvar syncml-credential "JorgenB" 
  "*The credential name.")

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

(defun syncml-create-sessionid ()
  "Sets the variable syncml-current-sessionid to a new session id. Currently just a random number. 
At the same time, MsgID is set to 1, since MsgID shall be a number increasing 
from 1 within each unique session."
	(setq syncml-current-sessionid (abs (random t)))
	(setq syncml-current-msgid 1)
	(syncml-debug 'syncml "Created session id: %S" syncml-current-sessionid))

(defun syncml-increase-msgid (&optional sessionid) 
	"Increases the MsgID number with 1. CmdID is set to 1 at the same time."
	(setq syncml-current-msgid (+ syncml-current-msgid 1))
	(setq syncml-current-cmdid 1))

(defun syncml-increase-cmdid (&optional sessionid)
	"Increases the CmdID number with 1."
	(setq syncml-current-msgid (+ syncml-current-msgid 1)))


(defun syncml-init (&optional slow-sync)
	"Initialized a SyncML request. If SLOW-SYNC is 't, then a request for a
slow sync is forced.
Returns TRUE if initialization went ok, and we can proceed with syncronization."
;;	(setq syncml-current-timestamp (format-time-string "%Y%m%dT%H%M%SZ%z" ))
	(setq syncml-current-timestamp (format-time-string "%Y%m%dT%H%M%SZ" ))
	(syncml-create-sessionid)
	(setq url-debug 't)
	(set-buffer (get-buffer-create "*syncml-transfer*"))
	(erase-buffer)
	(insert "<SyncML>")
	(insert (syncml-header))
	(insert "<SyncBody>")
	(if slow-sync
			(insert (syncml-create-alert-command syncml-alert-slow-sync))
		(insert (syncml-create-alert-command syncml-alert-two-way)))
	(insert "<Final/></SyncBody>\n</SyncML>")
	
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
		(syncml-debug 'syncml-post "Posting:\n %S" url-request-data) 
		(kill-buffer (get-buffer "*syncml-response*"))
		(set-buffer (get-buffer-create "*syncml-response*"))
		(insert-buffer (url-retrieve-synchronously syncml-host))
		;; TODO: need to check the HTTP response code here!!
		(syncml-debug 'syncml-post "Got:\n %S" (buffer-string))
		(syncml-process-response-buffer (get-buffer "*syncml-response*"))))



(defun syncml-header ( &optional sessionid msgid target source)  
	"Returns a syncml header <SyncHdr> tag.  If arguments are supplied, they
will be used instead of their respective global variables.

XML Definition: SyncHdr: (VerDTD, VerProto, SessionID, MsgID, Target, Source, RespURI?, NoResp?, Cred?, Meta?)"
	(concat 
	 "<SyncHdr>
		  <VerDTD>1.1</VerDTD>
		  <VerProto>SyncML/1.1</VerProto>
		  <SessionID>" (number-to-string syncml-current-sessionid) "</SessionID>
		  <MsgID>" (number-to-string syncml-current-msgid) "</MsgID>
		  <Target><LocURI>" syncml-target-locuri "</LocURI></Target>
		  <Source><LocURI>" syncml-source-locuri "</LocURI></Source> 
	</SyncHdr>"))

(defun syncml-create-alert-command (alert-command-number &optional target-database source-database prev-timestamp next-timestamp)
	"Returns a string with the <Alert> command with the given ALERT-COMMAND-NUMBER

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


(defun syncml-process-response ()
	"Processes the response from the SyncML server.  Checks first the header, and then processes
each command in the body."
	(syncml-debug 'syncml-process-response "Started")
	(syncml-process-response-buffer (get-buffer "*syncml-response*"))
	(setq syncml-next-respuri 
				(dom-node-text-content 
				 (car (xpath-resolve (dom-document-element syncml-response-doc) "descendant::RespURI"))))
	(syncml-debug'syncml-process-response "<RespURI> is %S" syncml-next-respuri)

	;; process all commands in the <SyncBody> tag
	(dolist 
			(node (xpath-resolve (dom-document-element syncml-response-doc) 
													 "descendant::SyncBody/child::*")
						nil)
		(syncml-process-command node))
	(setq syncml-next-respuri nil))

				
(defun syncml-process-command (node)
	"Processes the command NODE, which can be of any type (ALERT, ADD, GET, STATUS, SYNC...)"
	(syncml-debug 'syncml-process-command "Called. Is the node a dom-node?: %S" (dom-node-p node))
	(syncml-debug 'syncml-process-command "Is the node a dom-element?: %S" (dom-element-p node))
	(if (not (dom-element-p node))
			(throw 'wrong-type nil))
	(syncml-debug 'syncml-process-command "Name of node: %S" (dom-element-name node))
	(let ((node-element-name (dom-element-name node)))
		(cond ((string= "Status" node-element-name) 
					 (syncml-debug 'syncml-process-command "Calling syncml-process-status-command.")
					 (funcall 'syncml-process-status-command node))
					((string= "Alert" node-element-name) 
					 (syncml-debug 'syncml-process-command "Calling syncml-process-alert-command.")
					 (funcall 'syncml-process-alert-command node)))))
		

(defun syncml-process-status-command (node)
	"Processes the NODE which must be a <Status> node

XML definition: <Status>: (CmdID, MsgRef, CmdRef, Cmd, TargetRef*, SourceRef*, Cred?, Chal?, Data, Item*)

TODO:  how to let this command control program flow ?"
	(let ((syncml-cmd (dom-node-text-content (car (xpath-resolve node "child::Cmd"))))
				(syncml-data (dom-node-text-content (car (xpath-resolve node "child::Data")))))
		(syncml-debug 'process-status-command "Processing status command with %S %S" syncml-cmd syncml-data) 
		;; the <Status> command is used as a response to many different request. The <Cmd> and <CmdRef> tells which.
		;; further processing must be based on this. (ie: preserve the request in some way)
		(cond ((string= "SyncHdr" syncml-cmd)
					 (if (not (string= syncml-data "212"))
							 (progn (syncml-debug 'syncml-process-status-command "ERROR. Server said: %S" syncml-data)
											(error syncml-data))))
					((string= "Alert" syncml-cmd)
					 (if (not (string= syncml-data "200"))
							 (progn (syncml-debug 'syncml-process-status-command "ERROR. Server said %S" syncml-data)
											(error "Error in request")))))))

(defun syncml-process-alert-command (node)
	"Processes the NODE which must be an <Alert> node"
	(let ((syncml-cmdid (dom-node-text-content (car (xpath-resolve node "child::CmdID"))))
				(syncml-data (dom-node-text-content (car (xpath-resolve node "child::Data")))))
		(syncml-debug 'process-status-command "Processing status command with %S %S" syncml-cmdid syncml-data) 
		))


(defun syncml-process-response-buffer (syncml-buffer)
	"Process the buffer SYNCML-BUFFER. Builds up a DOM tree and stores it in syncml-response-doc."
	(set-buffer syncml-buffer)
	(goto-char (point-min))
	(search-forward-regexp "<SyncML>")
	(move-to-column 0)
	(setq syncml-response-doc 
				(dom-make-document-from-xml (car (xml-parse-region (point) (point-max))))))



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


(provide 'syncml)
