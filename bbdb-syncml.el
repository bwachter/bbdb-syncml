;;; bbdb-syncml.el -- A SyncML client for the BBDB database.
;; $Id: bbdb-syncml.el,v 1.9 2005/04/03 20:29:11 joergenb Exp $

;; Copyright (C) 2003 Jørgen Binningsbø 

;; Author: Jørgen Binningsbø <jb@pvv.org>
;; Maintainer: Jørgen Binningsbø <jb@pvv.org>
;; Version: 
;; Created: Jan 10 2003
;; Keywords: bbdb syncml xml network
;; URL: http://savannah.nongnu.org/projects/bbdb-syncml

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


(require 'bbdb)
(require 'bbdb-com)
(require 'bbdb-syncml-debug)
(require 'bbdb-syncml-vcard)
(require 'syncml)

(setq bbdb-syncml-debug t)

(defvar bbdb-syncml-bbdb-database "~/.bbdb"
  "*The BBDB database to use for syncing.")

(defvar bbdb-syncml-mapping-file "~/.bbdb.syncml"
  "*The file used by bbdb-syncml to keep track of syncing modifications")

(defvar bbdb-syncml-mapping-buffer ""
  "*The buffer containing the mapping file.")

(defvar bbdb-syncml-next-luid nil
  "*The value of LUID to be assigned to the next bbdb record. I believe 
this MUST be unique over the lifespan of a BBDB database")

(defvar bbdb-syncml-last-sync-timestamp nil
  "*The timestamp at the previous successful synchronzation")


;; bbdb-syncml-synchronize
;;
;; this is the main function of the bbdb-syncml package.  most of the logic is here.
;;
(defun bbdb-syncml-synchronize (&rest force-slow-sync) 
  "Synchronizes the bbdb database with the SyncML server.

See chapter 5 in the 'SyncML Sync Protocol' document available from www.syncml.org"
  (interactive)

  ;; do some initialization
  (message "SyncML synchronization started...")
  (bbdb-syncml-debug 1 'bbdb-syncml-synchronize "STARTING SYNCHRONIZATION OF DATABASE: %S" bbdb-syncml-bbdb-database )
   
  ;; check last sync time -get from .bbdb.syncml
  (setq syncml-previous-timestamp (bbdb-syncml-get-last-sync))
  (setq syncml-current-timestamp (format-time-string "%Y%m%dT%H%M%SZ" ))
  (bbdb-syncml-debug 1 'bbdb-syncml-synchronize "Timestamp of last sync: %S" syncml-previous-timestamp)
  (bbdb-syncml-debug 1 'bbdb-syncml-synchronize "Timestamp of this sync: %S" syncml-current-timestamp)

  ;; validate the luids in the bbdb and put all luids fond in a list.  will abort if inconsistencies are found.
  (setq bbdb-syncml-existing-luids (bbdb-syncml-validate-luids nil))
  ;; also ensure that the mapping file lists are up-to-date
  (setq bbdb-syncml-mapping-luid-list (bbdb-syncml-read-mapping-file))
  (bbdb-syncml-debug 1 'bbdb-syncml-synchronize "LUIDs in BBDB        : %S" bbdb-syncml-existing-luids)
  (bbdb-syncml-debug 1 'bbdb-syncml-synchronize "LUIDs in mapping file: %S" bbdb-syncml-mapping-luid-list)
  
  ;; send initialization package to server.
  ;; the syncml-init function will break if an error occurred.
  (bbdb-syncml-debug 1 'bbdb-syncml-synchronize "Sending initalization package #1 to server." )
  (syncml-init force-slow-sync)
  (syncml-process-response)
  (bbdb-syncml-debug 3 'bbdb-syncml-synchronize "Package #2 recieved from server and processed. Starting to create package #3")

  ;; first, create all node which we later need to reference 
  (let* ((syncml-transmit-doc (syncml-create-syncml-document))
	 (syncmlnode (dom-document-element syncml-transmit-doc))
	 ;; the <SyncHdr>
	 (synchdrnode (syncml-create-synchdr-command
		       syncml-transmit-doc 
		       (syncml-create-target-command syncml-transmit-doc syncml-target-locuri)
		       (syncml-create-source-command syncml-transmit-doc syncml-source-locuri)))
	 ;; the <SyncBody>
	 (syncbodynode (syncml-create-syncbody-command syncml-transmit-doc))
	 ;; the <Status> in reponse to the synchdr
	 (status-synchdr-node (syncml-create-status-command 
			       syncml-transmit-doc
			       (dom-node-text-content (car (xpath-resolve (dom-document-element syncml-response-doc) 
									  "descendant::MsgID")))
			       "0" ;; <SyncHdr> doesn't have a <CmdID>
			       "SyncHdr"
			       (syncml-create-data-command 
				syncml-transmit-doc 
				(dom-node-text-content (car (xpath-resolve 
							     (dom-document-element syncml-response-doc) 
							     "descendant::Status/child::Data[position()=1]"))))
			       (syncml-create-target-command syncml-transmit-doc syncml-target-locuri)
			       (syncml-create-source-command syncml-transmit-doc syncml-source-locuri)	  
			       ))
	 ;; the <Status> in response to the <Alert>
	 (status-alert-node 
	  (syncml-create-status-command
	   syncml-transmit-doc
	   (dom-node-text-content (car (xpath-resolve (dom-document-element syncml-response-doc)
						      "descendant::MsgID")))
	   (dom-node-text-content (car (xpath-resolve (dom-document-element syncml-response-doc)
						      "descendant::Alert/child::CmdID")))
	   "Alert"
	   (syncml-create-data-command syncml-transmit-doc 
				       (dom-node-text-content (car (xpath-resolve 
								    (dom-document-element syncml-response-doc) 
								    "descendant::Status/child::Data[position()=2]"))))
	   (syncml-create-target-command syncml-transmit-doc syncml-target-database)
	   (syncml-create-source-command syncml-transmit-doc syncml-source-database)	  
	   ))       
	 ;; the <Sync> node
	 (syncnode (syncml-create-sync-command syncml-transmit-doc))

	 ;; a list of luids added since last sync
	 (bbdb-syncml-added-luids 
	  ;; if the server sent response 508 to the SYNC command, then syncing should be slow.  
	  ;; syncml-init sets the global variable SYNCML-DOING-SLOW-SYNC to 't in this case.  	  
	  (if (not (null syncml-doing-slow-sync ))
	      (progn 
		(message "Slow sync forced by server. Sending full database...")
		(bbdb-syncml-debug 1 'bbdb-syncml-synchronize "Slow sync forced by server.")
		(bbdb-syncml-get-all-records))
	    (progn
	      (message "Doing regular sync. Sending modifications...")
	      (bbdb-syncml-debug 1 'bbdb-syncml-synchronize "Doing regular sync.")	      
	      (bbdb-syncml-get-added-records))))
	 ;; a list of luids modified since last sync
	 (bbdb-syncml-modified-luids
	  (if (not (null syncml-doing-slow-sync))
	      (bbdb-syncml-debug 1 'bbdb-syncml-synchronize "Doing slow sync. Ignoring modified records.")
	    (progn
	      (bbdb-syncml-debug 1 'bbdb-syncml-synchronize "Doing regular sync. Getting modified records.")
	      (bbdb-syncml-get-modified-records syncml-previous-timestamp))))
	 ;; a list of luids deleted since last sync
	 (bbdb-syncml-deleted-luids
	  (if (not (null syncml-doing-slow-sync))
	      (bbdb-syncml-debug 1 'bbdb-syncml-synchronize "Doing slow sync. Ignoring deleted records.")
	    (progn
	      (bbdb-syncml-debug 1 'bbdb-syncml-synchronize "Doing regular sync. Getting deleted records.")
	      (bbdb-syncml-get-deleted-records)))))

    (bbdb-syncml-debug 3 'bbdb-syncml-synchronize "Done creating base package #3 nodes.")
    ;; Add the <SyncHdr> and <SyncBody> nodes to the <SyncML> node.
    (dom-node-append-child syncmlnode synchdrnode)
    (dom-node-append-child syncmlnode syncbodynode)

    ;; add the <Status> command in response for the <SyncHdr> from server, 
    ;; as first child to the <SyncBody>
    (dom-node-append-child syncbodynode status-synchdr-node)
				  
    ;; add the <Status> command in response for the <Alert> from server, 
    ;; as second child to the <SyncBody>
    (dom-node-append-child syncbodynode status-alert-node)

    ;; create a <Sync> command to hold all the <Add> commands, but only if we have modifications to send.
;;    (if (or (> (length bbdb-syncml-added-luids) 0)
;;	    (> (length bbdb-syncml-deleted-luids) 0)
;;	    (> (length bbdb-syncml-modified-luids) 0))
;;	(progn
;;	  (bbdb-syncml-debug 1 'bbdb-syncml-synchronize "We have some changes.")
;;	  (dom-node-append-child syncbodynode syncnode))
;;     (bbdb-syncml-debug 1 'bbdb-syncml-synchronize "No records at all!"))

    (dom-node-append-child syncbodynode syncnode)

    ;; The header and status commands are finished.  
    (bbdb-syncml-debug 1 'bbdb-syncml-synchronize "Skeleton DOM tree for package #3 prepared.")
    (bbdb-syncml-debug 3 'bbdb-syncml-synchronize (dom-node-write-to-string syncml-transmit-doc 1))

    ;; go through all the added bbdb records, and add them to the <Sync> element. 
    (bbdb-syncml-debug 1 'bbdb-syncml-synchronize "Processing all added bbdb records: %S" bbdb-syncml-added-luids)
    (dolist (luid bbdb-syncml-added-luids)
      (bbdb-syncml-debug 2 'bbdb-syncml-synchronize "Processing luid %S " luid)
      (let ((temp-add-node
	     (syncml-create-add-command 
	      syncml-transmit-doc 
	      (syncml-create-item-command 
	       syncml-transmit-doc
	       nil
	       (syncml-create-source-command syncml-transmit-doc luid)
	       (syncml-create-data-command syncml-transmit-doc (bbdb-syncml-vcard-get-bbdb-record-as-vcard-string
								(car (bbdb-syncml-get-record-by-luid luid)))))
	      (syncml-create-meta-command syncml-transmit-doc
					  (syncml-create-metinf-type-command
					   syncml-transmit-doc
					   "text/x-vcard")))))
	;;(bbdb-syncml-debug 3 'bbdb-syncml-synchronize "<Add>command representation: %S " temp-add-node)
	(dom-node-append-child syncnode temp-add-node)))

    ;; go through all the modified bbdb records, and add them to the <Sync> element. 
    (bbdb-syncml-debug 1 'bbdb-syncml-synchronize "Processing all modified bbdb records: %S" bbdb-syncml-modified-luids)
    (dolist (luid bbdb-syncml-modified-luids)
      (bbdb-syncml-debug 2 'bbdb-syncml-synchronize "Processing luid %S " luid)
      (let ((temp-modify-node
	     (syncml-create-replace-command 
	      syncml-transmit-doc 
	      (syncml-create-item-command 
	       syncml-transmit-doc
	       nil
	       (syncml-create-source-command syncml-transmit-doc luid)
	       (syncml-create-data-command syncml-transmit-doc (bbdb-syncml-vcard-get-bbdb-record-as-vcard-string
								(car (bbdb-syncml-get-record-by-luid luid)))))
	      (syncml-create-meta-command syncml-transmit-doc
					  (syncml-create-metinf-type-command
					   syncml-transmit-doc
					   "text/x-vcard")))))
	(bbdb-syncml-debug 3 'bbdb-syncml-synchronize "<Replace> command representation: %S " (dom-node-write-to-string temp-modify-node))
	(dom-node-append-child syncnode temp-modify-node)))

    ;; go through all the deleted bbdb records, and add them to the <Sync> element. 
    (bbdb-syncml-debug 1 'bbdb-syncml-synchronize "Processing all deleted bbdb records: %S" bbdb-syncml-deleted-luids)
    (dolist (luid bbdb-syncml-deleted-luids)
      (bbdb-syncml-debug 2 'bbdb-syncml-synchronize "Processing luid %S " luid)
      (let ((temp-delete-node
	     (syncml-create-delete-command 
	      syncml-transmit-doc 
	      (syncml-create-item-command 
	       syncml-transmit-doc
	       nil
	       (syncml-create-source-command syncml-transmit-doc luid))
	      (syncml-create-meta-command syncml-transmit-doc
					  (syncml-create-metinf-type-command
					   syncml-transmit-doc
					   "text/x-vcard")))))
	(dom-node-append-child syncnode temp-delete-node)))
	  
    ;; add a <Final> node
    (dom-node-append-child syncbodynode (syncml-create-final-command syncml-transmit-doc))
	  
    ;; finished constructing the DOM tree.
    (bbdb-syncml-debug 1 'bbdb-syncml-synchronize "Package #3 DOM tree finished. Sending to server...")
    ;;(bbdb-syncml-debug 2 'bbdb-syncml-synchronize (dom-node-write-to-string syncml-transmit-doc))
	  
    ;; send package #3 to server.  the response from the server, package #4, is stored in SYNCML-RESPONSE-DOC.
    (syncml-send-message-with-curl syncml-transmit-doc)
    (message "Server modifications recieved. Incorporating them into the BBDB and sending the result to server...")
	  
    ;; Create a skeleton for package #5 (Data Update Status package to Server).
    (setq bbdb-syncml-package-5-doc nil)
    (setq bbdb-syncml-package-5-doc (syncml-create-syncml-document))
	  
    (let* ((syncml-pkg5-node (dom-document-element bbdb-syncml-package-5-doc))
	   ;; the <SyncHdr>
	   (synchdr-pkg5-node (syncml-create-synchdr-command
			       bbdb-syncml-package-5-doc 
			       (syncml-create-target-command bbdb-syncml-package-5-doc syncml-target-locuri)
			       (syncml-create-source-command bbdb-syncml-package-5-doc syncml-source-locuri)))
	   ;;the <SyncBody>
	   (syncbody-pkg5-node (syncml-create-syncbody-command bbdb-syncml-package-5-doc)))
	    
	  
      (bbdb-syncml-debug 2 'bbdb-syncml-synchronize "Done creating base package #5 DOM nodes.")
	    
      ;; Add the <SyncHdr> and <SyncBody> nodes to the <SyncML> node.
      (dom-node-append-child syncml-pkg5-node synchdr-pkg5-node)
      (dom-node-append-child syncml-pkg5-node syncbody-pkg5-node)
	    
      ;; Add the <Status> refering to <SyncHdr>
      (dom-node-append-child syncbody-pkg5-node
			     (syncml-create-status-command 
			      bbdb-syncml-package-5-doc
			      (dom-node-text-content (car (xpath-resolve (dom-document-element syncml-response-doc) 
									 "descendant::MsgID")))
			      "0" ;; <SyncHdr> doesn't have a <CmdID>
			      "SyncHdr"
			      (syncml-create-data-command bbdb-syncml-package-5-doc 
							  (dom-node-text-content 
							   (car (xpath-resolve 
								 (dom-document-element syncml-response-doc) 
								 "descendant::Status/child::Data[position()=1]"))))
			      (syncml-create-target-command bbdb-syncml-package-5-doc syncml-target-locuri)
			      (syncml-create-source-command bbdb-syncml-package-5-doc syncml-source-locuri)))
	    
      ;; The header and status commands are finished.  
      (bbdb-syncml-debug 1 'bbdb-syncml-synchronize "Base DOM tree for package #5 prepared.")	  
      (bbdb-syncml-debug 3 'bbdb-syncml-synchronize "\n%S" (dom-node-write-to-string bbdb-syncml-package-5-doc 1))
	    
      ;; PROCESS PACKAGE #4 from server, and update BBDB-SYNCML-PACKAGE-5-DOC
      (bbdb-syncml-debug 1 'bbdb-syncml-synchronize "Processing package #4 from server...")
      (setq bbdb-syncml-pkg4-doc syncml-response-doc)      
      (bbdb-syncml-process-modifications-response bbdb-syncml-package-5-doc 
						  bbdb-syncml-added-luids 
						  bbdb-syncml-modified-luids 
						  bbdb-syncml-deleted-luids)
	    
      (dom-node-append-child syncbody-pkg5-node (syncml-create-final-command bbdb-syncml-package-5-doc)))

    (bbdb-syncml-debug 2 'bbdb-syncml-synchronize "OK luids    : %S" bbdb-syncml-pkg5-ok-luids)
    (bbdb-syncml-debug 2 'bbdb-syncml-synchronize "Not OK luids: %S" bbdb-syncml-pkg5-not-ok-luids)

    ;; finished creating package 5. sending to server.
    (bbdb-syncml-debug 2 'bbdb-syncml-synchronize "Modifications processed. Sending package #5 to server.")
    (bbdb-syncml-debug 3 'bbdb-syncml-synchronize "Package #5 is now: \n%S" (dom-node-write-to-string bbdb-syncml-package-5-doc 2))
	  
    (syncml-send-message-with-curl bbdb-syncml-package-5-doc)
    ;; send package #5 to the server.  we must get back package #6 (Map Acknowledgement to client)
    (message "Server map acknowledgement. Checking for errors...")
    ;; TODO: implement checking...
    
    ;; if all was successful, update the timestamp in the mapping file.
    (bbdb-syncml-write-mapping-file bbdb-syncml-pkg5-ok-luids)
    (bbdb-syncml-write-next-timestamp syncml-current-timestamp)

    (message "Synchronization complete!")))



(defun bbdb-syncml-synchronize-slow ()
  (interactive)
  (bbdb-syncml-synchronize t))



(defun bbdb-syncml-get-added-records (&optional timestamp)
  "Returns the LUID of records added since last sync.
An new/added record is:
a record without luid
 OR
a record with luid, but without an entry in the mapping file
is this the wisest way to check?
better for the function to return a list of records, and assign the luids temporary, and save 
locally only if a succesful sync is made? 
note: there may be returned a <status> command from the server for this.  or we can request it."
  (bbdb-syncml-debug 1 'bbdb-syncml-get-added-records "Started. ") 
  (setq bbdb-syncml-next-luid (bbdb-syncml-get-next-luid))
  
  ;; iterate over all bbdb records, and put the luid of the added ones in the list added-luid-list
  (let (added-luid-list)
    (dolist (node (bbdb-records) added-luid-list)
      (bbdb-syncml-debug 1 'bbdb-syncml-get-added-records 
			 "Checking record having name and company: %S - %S" 
			 (bbdb-record-name node) 
			 (bbdb-record-company node))
      (if (and (not (null (bbdb-record-getprop node 'luid)))
	       (member (string-to-number (bbdb-record-getprop node 'luid)) bbdb-syncml-mapping-luid-list))
	  ;; record has a luid and is present in the mapping file. It is not added since last successful sync
	  (bbdb-syncml-debug 1 'bbdb-syncml-get-added-records
			     "The LUID %S for this record already exists in the mapping file. Record is not added since last sync." 
			     (bbdb-record-getprop node 'luid))
	;; record is added since last successful sync.
	;; Create a new luid if none exists, and increment counter.
	;; The mapping file is updated only after a successful sync, so there is no need for
	;; this function to delete the luid if syncing failed.
	(bbdb-syncml-debug 1 'bbdb-syncml-get-added-records "Record is added since last successful sync. Checking if we must reassign luid.")
	(if (null (bbdb-record-getprop node 'luid))
	    (progn 
	      (bbdb-syncml-debug 1 'bbdb-syncml-get-added-records 
				 "Record %S doesn't have a luid. Assigning %S" (bbdb-record-name node) bbdb-syncml-next-luid)
	      (bbdb-record-putprop node 'luid bbdb-syncml-next-luid)
	      (bbdb-syncml-increment-luid)
	      (bbdb-syncml-debug 1 'bbdb-syncml-get-added-records "Saving BBDB database...")
	      (bbdb-save-db)))
	(bbdb-syncml-debug 1 'bbdb-syncml-get-added-records "Adding luid %S to the list of added luids." (bbdb-record-getprop node 'luid))
	(push (bbdb-record-getprop node 'luid) added-luid-list)))))



(defun bbdb-syncml-get-all-records (&optional timestamp)
  "Returns the LUID of all records. Used when doing slow sync."
  (bbdb-syncml-debug 1 'bbdb-syncml-get-all-records "Started")
  ;; ensure that the next luid is up-to-date
  (setq bbdb-syncml-next-luid (bbdb-syncml-get-next-luid))
  
  ;; if all is ok, then iterate over all bbdb records, and put all luid's into the list all-luid-list. records lacking luid should be assigned one.
  (let (all-luid-list)
    (dolist (node (bbdb-records) all-luid-list)
      (bbdb-syncml-debug 1 'bbdb-syncml-get-all-records 
			 "Checking record with name and company: %S - %S" 
			 (bbdb-record-name node) 
			 (bbdb-record-company node))
      (if (not (null (bbdb-record-getprop node 'luid)))	;; record has a luid.
	  (bbdb-syncml-debug 1 'bbdb-syncml-get-all-records
			     "Record already has LUID: %S" (bbdb-record-getprop node 'luid))      
	;; Create a new luid if none exists, and increment counter.
	;; The mapping file is updated only after a successful sync, so there is no need for
	;; this function to delete the luid if syncing failed.
	(progn 
	  (bbdb-syncml-debug 1 'bbdb-syncml-get-all-records 
			     "Record %S doesn't have a luid. Assigning %S" (bbdb-record-name node) bbdb-syncml-next-luid)
	  (bbdb-record-putprop node 'luid bbdb-syncml-next-luid)
	  (bbdb-syncml-increment-luid)
	  (bbdb-syncml-debug 1 'bbdb-syncml-get-all-records "Saving BBDB database...")
	  (bbdb-save-db)))
      (bbdb-syncml-debug 1 'bbdb-syncml-get-all-records "Adding luid %S to the list of all luids." (bbdb-record-getprop node 'luid))
      (push (bbdb-record-getprop node 'luid) all-luid-list))))




(defun bbdb-syncml-get-modified-records (last-timestamp)
  "Returns the LUID of records modified since last sync.

Checks the timestamp against the last sync value.

NOTE: This checks the bbdb property 'timestamp for each record against systemwide last-sync,  but what
if a sync for a particular record was unsuccessful at the last sync event/time? Probably,
the OK message returned by the server should be used to modify a last timestamp in the mapping file, 
and this function should use this in some way.
"
  (bbdb-syncml-debug 1 'bbdb-syncml-get-modified-records "Started with timestamp: %S" last-timestamp)
  ;; ensure that the next luid is up-to-date
  (setq bbdb-syncml-next-luid (bbdb-syncml-get-next-luid))
  (let (modified-luid-list)
    (dolist (node (bbdb-records) modified-luid-list)
      (bbdb-syncml-debug 1 'bbdb-syncml-get-modified-records 
			 "Checking node: %S - %S" 
			 (bbdb-record-name node) 
			 (bbdb-record-company node))
      (if (null (bbdb-record-getprop node 'luid))
	  ;; record does not have a luid. it is added since the TIMESTAMP, it should be 
	  ;; triggered in the bbdb-syncml-get-added-records, not in this function.  just debug.
	  (bbdb-syncml-debug 1 'bbdb-syncml-get-modified-records
			     "No LUID for record." )
	;; record does have a luid. 
	;; check if it's modified timestamp is newer than the timestamp of the last sync.
	(let ((record-timestamp (bbdb-record-getprop node 'timestamp))
	      (record-luid (bbdb-record-getprop node 'luid)))
	  (bbdb-syncml-debug 1 'bbdb-syncml-get-modified-records 
			     "Node has timestamp of %S. " record-timestamp)
	  (if (or (string< record-timestamp last-timestamp)
		  (string= record-timestamp last-timestamp))
	      ;; not changed since last synctime
	      (bbdb-syncml-debug 1 'bbdb-syncml-get-modified-records "Record not changed.")
	    (bbdb-syncml-debug 1 'bbdb-syncml-get-modified-records "Record CHANGED.")
	    (push record-luid modified-luid-list)))))
    ;; we must subtract LUIDs that also were detected by the 'bbdb-syncml-get-new-records' function:
    (dolist (element bbdb-syncml-added-luids nil)
      (bbdb-syncml-debug 2 'bbdb-syncml-get-modified-records "Checking if luid %s was present among modified records" element)
      (delete* element modified-luid-list))
    (bbdb-syncml-debug 2 'bbdb-syncml-get-modified-records "Removed all added luids from the modified ones.")
    modified-luid-list))


(defun bbdb-syncml-get-deleted-records ()
  "Returns the LUID of records deleted since last sync.  

This function checks all current bbdb-records against the mapping file.
LUIDs present in the mapping file, but NOT in the BBDB have been deleted since last sync."
  (let (deleted-luids)	
    ;; first, put all luids in (bbdb-records) into a list
    (let (bbdb-records-luids)
      (dolist (node (bbdb-records) bbdb-records-luids)
	(push (bbdb-record-getprop node 'luid) bbdb-records-luids))
      (bbdb-syncml-debug 2 'bbdb-syncml-get-deleted-records "All luids: %S" bbdb-records-luids)
 
      ;; then, iterate over all luids in the mapping list, and return all which are present 
      ;; in the mapping list, but not in (bbdb-records)
      (dolist (node bbdb-syncml-mapping-luid-list deleted-luids)
	(bbdb-syncml-debug 2 'bbdb-syncml-get-deleted-records "Node is: %S" (number-to-string node))
	(if (not (member (number-to-string node) bbdb-records-luids))
	    (progn (bbdb-syncml-debug 2 'bbdb-syncml-get-deleted-records "luid %S present in mapping list, but not among bbdb-records" node)  
		   (push node deleted-luids)))))))


(defun bbdb-syncml-initialize () 
  "Prepares the BBDB database to support SyncML. Should only be called once.
Creates the mapping file, and adds the luid field to the database.
Will not delete LUID notes field from a previuos synchronized dataset."
  (bbdb-add-new-field 'luid)
  (bbdb-save-db)
  (if (file-exists-p bbdb-syncml-mapping-file)
      (progn (bbdb-syncml-debug 1 'bbdb-syncml-initialize "Already initalized. Prompt user to re-initialize.")
	     (if (not (y-or-n-p "You have already initialized this BBDB. Do you want to re-initalize?"))
		 (progn (bbdb-syncml-debug 1 'bbdb-syncml-initialize "User requested quit")
			(error ""))
	       (bbdb-syncml-debug 1 'bbdb-syncml-initalize "User requested re-initialization"))))
  (bbdb-syncml-debug 1 'bbdb-syncml-initalize "Creating mapping file.")
  (set-buffer (find-file-noselect bbdb-syncml-mapping-file))
  (erase-buffer)
  (goto-char (point-min))
  (insert ";;; BBDB mapping file\n")
  (insert ";;;\n")
  (insert ";;; Last sync timestamp: \n")
  (insert ";;; Next LUID: 1\n")
  (insert ";;; LUIDs of records with ok sync status during last sync.\n")
  (insert ";;; luids: \n")
  (save-buffer)
  (setq bbdb-syncml-next-luid 1)
					;	(setq bbdb-syncml-last-sync
  (bbdb-syncml-debug 1 'bbdb-syncml-initalize "Assigning LUIDs to entries")
  (bbdb-syncml-assign-luids)
  )

;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; functions related to the mapping file
;;
;; this includes getting and setting of:
;; - next luid
;; - last sync timestamp (?)
;; - list of succesful luids in last sync
;;
;;;;;;;;;;;;;;;;;;;;;;;;


(defun bbdb-syncml-open-mapping-file ()
  "Opens the mapping file and sets next LUID."
  (setq bbdb-syncml-mapping-buffer (find-file-noselect bbdb-syncml-mapping-file))
  (set-buffer bbdb-syncml-mapping-buffer)
  (setq bbdb-syncml-next-luid (bbdb-syncml-get-next-luid)))

(defun bbdb-syncml-read-mapping-file () 
  "Reads the mapping file, and put the list of successful luids in a list. This list contains number, not strings.
Returns the list."
  (bbdb-syncml-open-mapping-file)
  (set-buffer bbdb-syncml-mapping-buffer)
  (goto-char (point-min))
  (if (not (re-search-forward ";;; luids: \\(.*\\)" nil t))
      (progn (bbdb-syncml-debug 1 'bbdb-syncml-read-mapping-file "Not found.")
	     (error "luids not found in mapping file"))
    (bbdb-syncml-debug 1 'bbdb-syncml-read-mapping-file "Found: %S" (match-string 1))
    (car (read-from-string (match-string 1)))))

(defun bbdb-syncml-write-mapping-file (luid-list)
  "Writes the new LUID-LIST to the mapping file.
items in the LUID-LIST should be numbers, not strings.
NOTE: should only be called after syncing is finished"
  (bbdb-syncml-debug 1 'bbdb-syncml-write-mapping-file "Started.  Luid list is %S" luid-list)
  (set-buffer (find-file-noselect bbdb-syncml-mapping-file))
  (goto-char (point-min))
  (if (not (re-search-forward ";;; luids: " nil t))
      (progn (bbdb-syncml-debug 1 'bbdb-syncml-increment-luid "Unable to find position for luids in mapping file.")
	     (error "unable to find  position for luids in mapping file!"))
    (kill-region  (point) (line-end-position))
    (let* ((number-list (mapcar 'bbdb-syncml-element-to-number luid-list))
	   (unique-list (remove-duplicates number-list))
	   (sorted-list (sort unique-list '<)))
      (bbdb-syncml-debug 2 'bbdb-syncml-write-mapping-file "Sorted, unique luid-list is: %S" sorted-list)
      (bbdb-syncml-debug 2 'bbdb-syncml-write-mapping-file "LUIDs currently present in BBDB: %S" bbdb-syncml-existing-luids)
      (insert "(" (mapconcat 'number-to-string sorted-list " ") ")")
      (save-buffer))))


(defun bbdb-syncml-get-last-sync () 
  "Gets the last sync time from the mapping file"
  (bbdb-syncml-debug 1 'bbdb-syncml-get-last-sync "Triggered")
  (set-buffer (find-file-noselect bbdb-syncml-mapping-file))
  (goto-char (point-min))
  (if (not (re-search-forward ";;; last sync timestamp: \\(.*\\)" nil t))
      (bbdb-syncml-debug 1 'bbdb-syncml-get-last-sync "Not found.")
    (bbdb-syncml-debug 1 'bbdb-syncml-get-last-sync "Found: %S" (match-string 1))
    (match-string 1)))


(defun bbdb-syncml-write-next-timestamp (timestamp)
  "Writes the TIMESTAMP to the mapping file.
NOTE: should only be called after syncing is finished and successful."
  (bbdb-syncml-debug 1 'bbdb-syncml-write-next-timestamp "Started. Timestamp is %S" timestamp)
  (set-buffer (find-file-noselect bbdb-syncml-mapping-file))
  (goto-char (point-min))
  (if (not (re-search-forward ";;; Last sync timestamp: " nil t))
      (progn (bbdb-syncml-debug 1 'bbdb-syncml-increment-luid "Unable to find position for timestamp in mapping file.")
	     (error "unable to find  position for timestamp in mapping file!"))
    (kill-region  (point) (line-end-position))
    (insert timestamp)
    (save-buffer)))



(defun bbdb-syncml-get-next-luid ()
  "Read the value of next LUID from the mapping file."
  (set-buffer (find-file-noselect bbdb-syncml-mapping-file))
  (goto-char (point-min))
  (if (not (re-search-forward ";;; next LUID: \\(.*\\)" nil t))
      (progn (bbdb-syncml-debug 1 'bbdb-syncml-get-next-luid "Not found.")
	     (error "next luid not found in file"))
    (bbdb-syncml-debug 1 'bbdb-syncml-get-next-luid "Found: %S" (match-string 1))
    (match-string 1)))

(defun bbdb-syncml-update-next-luid ()
  "Sets the variable 'bbdb-syncml-next-luid based on the mapping file"
  (setq bbdb-syncml-next-luid (bbdb-syncml-get-next-luid)))

(defun bbdb-syncml-increment-luid ()
  "Increments the current luid by one, and stores the new value in the mapping file, in the bbdb-syncml-next-luid variable, and returns the new value."
  (bbdb-syncml-update-next-luid)
  (bbdb-syncml-debug 1 'bbdb-syncml-increment-luid "Started. Current luid is %S" bbdb-syncml-next-luid)
  (set-buffer (find-file-noselect bbdb-syncml-mapping-file))
  (goto-char (point-min))
  (if (not (re-search-forward ";;; next LUID: " nil t))
      (progn (bbdb-syncml-debug 1 'bbdb-syncml-increment-luid "Unable to find next-luid position in mapping file.")
	     (error "unable to find next-luid posistion in file!"))
    (kill-region  (point) (line-end-position))
    (setq bbdb-syncml-next-luid (number-to-string (+ (string-to-number bbdb-syncml-next-luid) 
						     1)))
    (insert bbdb-syncml-next-luid)
    (save-buffer)
    (bbdb-syncml-debug 1 'bbdb-syncml-increment-luid "New luid: %S" bbdb-syncml-next-luid)))

(defun bbdb-syncml-set-next-luid (new-value) 
  "Sets the variable for next luid, and updates the mapping file.
Should normally never be called, unless the mapping is out of sync"
  (bbdb-syncml-debug 1 'bbdb-sycnml-set-next-luid "Called with new value %S" new-value)
  (setq bbdb-syncml-next-luid  new-value)
  (set-buffer (find-file-noselect bbdb-syncml-mapping-file))
  (goto-char (point-min))
  (if (not (re-search-forward ";;; next LUID: " nil t)) 
      (progn (bbdb-syncml-debug 1 'bbdb-syncml-set-next-luid "Unable to find next-luid position in mapping file.")
	     (error "unable to find next-luid posistion in file!"))
    (kill-region  (point) (line-end-position))
    (insert (number-to-string bbdb-syncml-next-luid))
    (save-buffer)
    (bbdb-syncml-debug 1 'bbdb-syncml-set-next-luid "New luid: %S" bbdb-syncml-next-luid)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; functions related to LUIDs in the bbdb file
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun bbdb-syncml-assign-luids ()
  "Reads all records in the BBDB database, and assigns LUIDs to those not having one."
  ;; first, validate that there are no errorous luids in the database
  (bbdb-syncml-debug 1 'bbdb-syncml-assign-luids "Triggered. Proceeding with validate")
  (bbdb-syncml-validate-luids 't)
  ;; ensure that the bbdb-syncml-next-luid is up-to-date
  (setq bbdb-syncml-next-luid (bbdb-syncml-get-next-luid))
  (dolist (node (bbdb-records) nil)
    (bbdb-syncml-debug 1 'bbdb-syncml-assign-luids "Validating node: %S" node)
    (if (not (null (bbdb-record-getprop node 'luid)))
	;; record has a luid. just debug.
	(bbdb-syncml-debug 1 'bbdb-syncml-assign-luids 
			   "Found LUID for %S: %S " 
			   (bbdb-record-name node) 
			   (bbdb-record-getprop node 'LUID))
      ;; record does not have a luid. Create one, and increment counter.
      (bbdb-syncml-debug 1 'bbdb-syncml-assign-luids "Node unhas luid. assigning %S." bbdb-syncml-next-luid)
      (bbdb-record-putprop node 'luid bbdb-syncml-next-luid)
      (bbdb-save-db)
      (bbdb-syncml-increment-luid)))
  (bbdb-save-db)
  (bbdb-redisplay-records)
  )

(defun bbdb-syncml-validate-luids (reassign)
  "Reads all BBDB records and checks if any LUID is above the bbdb-syncml-next-luid.
If REASSIGN is true, then the bbdb-syncml-next-luid is updated to the higest value in the
dataset + 1.
If REASSIGN is false, the function is aborted.

Returns a list with all existing luids in the BBDB."
  (bbdb-syncml-debug 1 'bbdb-syncml-validate-luids "Triggered. (bbdb-records) has %S records." (length (bbdb-records)))
  ;; should make sure we are comparing against the most current next-luid.
  (setq bbdb-syncml-next-luid (bbdb-syncml-get-next-luid))
  (bbdb-syncml-debug 1 'bbdb-syncml-validate-luids "Value of next luid: %S" bbdb-syncml-next-luid)
  (let (existing-luids)
    (dolist (node (bbdb-records) existing-luids)
      (bbdb-syncml-debug 2 'bbdb-syncml-validate-luids "examining a node: %S" node)
      (if (null (bbdb-record-getprop node 'luid))
	  ;; record does not have a luid. All is well.
	  (bbdb-syncml-debug 1 'bbdb-syncml-validate-luids "node doesn't have a LUID. all is well")
	;; record has a luid. message it for debug.
	(bbdb-syncml-debug 1 'bbdb-syncml-validate-luids "Validating BBDB. Found LUID for %S: %S " (bbdb-record-name node) (bbdb-record-getprop node 'luid))
	(if (>= (string-to-number (bbdb-record-getprop node 'luid)) (string-to-number bbdb-syncml-next-luid))
	    (progn (bbdb-syncml-debug 1 'bbdb-syncml-validate-luids 
				      "ERROR: Luid %S is greater than value of next luid %S." 
				      (bbdb-record-getprop node 'luid) bbdb-syncml-next-luid)
		   (if reassign
		       (progn (bbdb-syncml-debug 1 'bbdb-syncml-validate-luids "Reassigning luids")
			      (bbdb-syncml-set-next-luid 
			       (+ (string-to-number (bbdb-record-getprop node 'luid)) 1)))
		     (bbdb-syncml-debug 1 'bbdb-syncml-validate-luids "Not reassigning luids.  Aborting...")
		     (error "Consistency error in the syncml IDs in BBDB."))))
	(push (bbdb-record-getprop node 'luid) existing-luids)))))

(defun bbdb-syncml-create-luid-hook ()
  "This function should be called whenever a new bbdb record is created.
The value of LUID to assign to the new record is picked from the 
bbdb-syncml-mapping-file"
  ())

;;;;;;;;;;;;;;;;;;;
;;
;; working directly towards the bbdb_
;;
;;;;;;;;;;;;;;;;;;
(defun bbdb-syncml-get-record-by-luid (luid)
  "Return the record with the given LUID"
  (let ((notes (cons (intern "luid") (concat "^" luid "$"))))
    (bbdb-search (bbdb-records) nil nil nil notes)))


(defun bbdb-syncml-add-record (record)
  ())

(defun bbdb-syncml-delete-record (record)
  ())

(defun bbdb-syncml-modify-record (record)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; syncml process functions
;;
;; these functions are bbdb-specific actions to take based on the response from the syncml server.  
;; they'll be used instead of the default syncml-process-xxx-command in syncml.el, which ain't supposed to
;; know anything about the bbdb.
;;
;;;;;;;;;;;;;;;;;;

(defun bbdb-syncml-process-modifications-response (pkg5-doc added-luids modified-luids deleted-luids)
  "This functions processes the response package #4 from the server, and builds the body of package #5."
  (if (not (dom-document-p pkg5-doc))
      (throw 'wrong-type nil))
  ;; mode of operation:  as we iterate over <status>es, one <sync> and it's children <add>s, <replace>s and <delete>s, we process each in turn.
  (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response "Resetting OK and NOT-OK lists")
  (setq bbdb-syncml-pkg5-ok-luids bbdb-syncml-mapping-luid-list)
  (setq bbdb-syncml-pkg5-not-ok-luids nil)
  (dolist (node (xpath-resolve (dom-document-element syncml-response-doc) "descendant::SyncBody/child::*") nil)
    (if (not (dom-element-p node))
	(progn (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response "NODE is not a dom-element.")
	       (throw 'wrong-type nil)))
    (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response "Processing node: %S" (dom-element-name node))
    (let ((node-element-name (dom-element-name node)))
      (cond ((string= "Status" node-element-name) 
	     (let ((syncml-cmd (dom-node-text-content (car (xpath-resolve node "child::Cmd"))))
		   (syncml-data (dom-node-text-content (car (xpath-resolve node "child::Data")))))
	       (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response 
				  "Got a <Status> in response to a %S command with data %S: %S" 
				  syncml-cmd syncml-data (syncml-lookup-response-code syncml-data)) 
	       ;; The <Status> command is used as a response to many different requests. 
	       ;; The <Cmd> and <CmdRef> tells which.
	       (cond ((string= "SyncHdr" syncml-cmd)
		      (progn (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response "CmdRef is SyncHdr")
			     (cond ((string= "407" syncml-data)
				    (progn (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response 
							      "Server said 407: Missing Credentials")) 
				    ;; must break execution and resend credentials;
				    (error "Authentication rejected"))
				   ((not (or (string= syncml-data "212") ;; we accept only 200 and 212 as valid for further processing.
					     (string= syncml-data "200")))
				    (progn (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response 
							      "ERROR. Server said: %S: %S" 
							      syncml-data (syncml-lookup-response-code syncml-data)) 
					   (error (syncml-lookup-response-code syncml-data)))))
			     (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response 
						"Response code %s is OK. Continuing..." syncml-data)))
		     ((string= "Alert" syncml-cmd)
		      (cond ((string= "404" syncml-data)
			     (progn (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response 
						       "Server said 404: Not found"))
			     (error "Target database not found"))
			    ((string= "508" syncml-data)
			     (progn (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response 
						       "Server said 508: Refresh required. Initiating slow sync"))
			     (setq syncml-doing-slow-sync 't))
			    ((not (string= syncml-data "200"))
			     (progn (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response 
						       "ERROR. Server said %S" syncml-data)
				    (error "Error in request")))))
		     ((string= "Sync" syncml-cmd)
		      (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response "Response to <Sync> command."))
		     ((string= "Add" syncml-cmd)
		      (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response "Response to <Add> command.")
		      (let* ((node-luid (dom-node-text-content (car (xpath-resolve node "child::SourceRef")))))
			(bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response "<SourceRef> (luid) is %s." node-luid)
			(if (equal syncml-data "201")
			    (progn 
			      (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response 
						 "Record successfully added at server.")
			      ;; don't update record's timestamp, but add the luid to the mapping list of luids.
			      (push node-luid bbdb-syncml-pkg5-ok-luids))
			  (progn
			    (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response
					       "Record NOT successfully added at server. Will not update mapping file.")
			    ;; don't update record's timestamp. don't add luid to the mapping list.
			    (push node-luid bbdb-syncml-pkg5-not-ok-luids)
			    ))))
		     ((string= "Replace" syncml-cmd)
		      (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response "Response to <Replace> command.")
		      (let* ((node-luid (dom-node-text-content (car (xpath-resolve node "child::SourceRef")))))
			(bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response "<SourceRef> (luid) is %s." node-luid)
			(if (equal syncml-data "201")
			    (progn 
			      (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response 
						 "Record successfully modified at server.")
			      ;; don't update record's timestamp.  Luid still exists in the mapping list.
			      (push node-luid bbdb-syncml-pkg5-ok-luids))
			  (progn
			    (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response
					       "Record NOT successfully modified at server. .")
			    ;; The record must be <replace>d again during next sync. Update it's timestamp. Still in the mapping list.
			    ;; TODO: implement timestamp update
			    (push node-luid bbdb-syncml-pkg5-ok-luids)
			    ))))
		     ((string= "Delete" syncml-cmd)
		      (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response "Response to <Delete> command.")
			  
		      (let* ((node-luid (dom-node-text-content (car (xpath-resolve node "child::SourceRef")))))
			(bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response "<SourceRef> (luid) is %s." node-luid)
			(if (equal syncml-data "210")
			    ;; if successful delete, remove from mapping list. (the object itself was already deleted from bbdb before we started sync)
			    (progn (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-reponse "Record successfully deleted at server")
				   ;; TODO: add the if...
				   (setq bbdb-syncml-pkg5-ok-luids (delete (bbdb-syncml-element-to-number node-luid) bbdb-syncml-pkg5-ok-luids)))
				   ;;(bbdb-syncml-debug 2 'bbdb-syncml-process-modifications-reponse "Is node-luid a string?: %S" (stringp node-luid))
				   ;;(bbdb-syncml-debug 2 'bbdb-syncml-process-modifications-reponse "Is node-luid a number?: %S" (numberp node-luid))
				   ;;(dolist (element bbdb-syncml-pkg5-ok-luids nil)
				   ;;  (bbdb-syncml-debug 2 'bbdb-syncml-process-modifications-reponse "Is element a number?: %S" (numberp element))
				   ;;  (bbdb-syncml-debug 2 'bbdb-syncml-process-modifications-reponse "Is element a string?: %S" (stringp element)))
				   ;;(bbdb-syncml-debug 2 'bbdb-syncml-process-modifications-reponse "Deleted list: %S" 
			;;			      (remove* node-luid bbdb-syncml-pkg5-ok-luids :test 'equal))
			;;	   (bbdb-syncml-debug 2 'bbdb-syncml-process-modifications-reponse "Deleted list: %S" 
			;;			      (remove node-luid bbdb-syncml-pkg5-ok-luids))
			;;	   (bbdb-syncml-debug 2 'bbdb-syncml-process-modifications-reponse "Deleted list: %S" bbdb-syncml-pkg5-ok-luids))
			  ;; TODO: if un-successful delete, keep in mapping list  (does the server send it's copy to us, since we've already delete it...)
			  (progn (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response "Item NOT DELETED at server!")
				 (push node-luid bbdb-syncml-pkg5-not-ok-luids))))))
		 
		 
	       (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response "Finished with the <Status> node.")))
	    ;; end of processing <status>-node.
	       
	    ((string= "Alert" node-element-name) 
	     (funcall 'syncml-process-alert-command node))
	       
	    ;; ;;;;;;;;;;;;;;;;;;
	    ;;  SYNC
	    ;; ;;;;;;;;;;;;;;;;;;
	    ((string= "Sync" node-element-name) 
	     ;; For <Sync> command, we need to traverse it's childs, try to do whatever 
	     ;; requested in the BBDB, and build a response indication
	     ;; if the BBDB-modification was successfull or not.  
	     (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response "Got a <Sync> command. Processing children." )
	     
	     ;; go thourgh all the children of the <Sync> command NODE.
	     ;; they are either <Add>, <Replace> or <Delete>.
	     ;; for each child, interact with the BBDB in the proper way.
	     ;; and create <Status>-commands and add those to the pkg5 dom.
	     
	     ;; do all <Add> commands.
	     (let* ((all-ok t)
		    )
	       ;; do all <Add> commands.
	       (dolist (add-node (xpath-resolve node "child::Add")
				 nil)
		 (progn 
		   (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response "New record from server.")
		   (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response "%S" (dom-node-write-to-string add-node))
		   (let* ((newrecord (bbdb-vcard-snarf 
				      (dom-node-text-content (car (xpath-resolve add-node "child::Item/child::Data"))))))
			 
		     (bbdb-record-putprop newrecord 'luid bbdb-syncml-next-luid)
		     (bbdb-record-putprop newrecord 'creation-date syncml-current-timestamp)
		     ;; because of delay in this program, we temporarily disabling the automatic timestamp hook, as we want to use the 
		     ;; syncml-current-timestamp instead,
		     ;; otherwise this record will be tagged as modified during next sync even if no changes was made.
		     (remove-hook 'bbdb-change-hook 'bbdb-timestamp-hook)
		     (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response "BBDB-CHANGE-HOOK: %S" 
					(describe-variable 'bbdb-change-hook))
		     (sleep-for 1)
		     (bbdb-record-putprop newrecord 'timestamp syncml-current-timestamp)		     
		     (bbdb-save-db)
		     (add-hook 'bbdb-change-hook 'bbdb-timestamp-hook)

		     (bbdb-syncml-increment-luid)
		     (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response "The new record was added to BBDB: %S." (bbdb-record-name newrecord))
		     (let* ((add-status-node 
			     (syncml-create-status-command
			      pkg5-doc
			      (dom-node-text-content (car (xpath-resolve (dom-document-element syncml-response-doc) 
									 "descendant::MsgID")))
			      (dom-node-text-content (car (xpath-resolve add-node "child::CmdID")))
			      "Add"
			      (syncml-create-data-command pkg5-doc "201") ;; 201 = the requested item was added.
			      nil
			      (syncml-create-sourceref-command pkg5-doc (bbdb-record-getprop newrecord 'luid)))))
		       (bbdb-syncml-debug 2 'bbdb-syncml-process-modifications-response "Adding <Status> node for the new record to pkg5")
		       (dom-node-append-child (car (xpath-resolve (dom-document-element pkg5-doc)
								  "descendant::SyncBody"))
					      add-status-node)
		       ;; record added.  update mapping list.
		       (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response "<Status> command created for new record.")
		       (push (bbdb-record-getprop newrecord 'luid) bbdb-syncml-pkg5-ok-luids)
		       )	  
		     )))
		   
	       ;; do all <Replace> commands. delete old record in bbdb and insert a new one, setting luid equal to the old bbdb record.
	       (dolist (replace-node (xpath-resolve node "child::Replace")
				     nil)	       
		 (progn 
		   (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response "Modified record from server.")
		   (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response "%S" (dom-node-write-to-string replace-node))
		   (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response "Modified record - first deleting it from bbdb." )
		   (let* ((luid-to-delete (dom-node-text-content (car (xpath-resolve replace-node "child::Item/child::Target/child::LocURI"))))
			  (record-to-delete (car (bbdb-syncml-get-record-by-luid luid-to-delete))))
		     (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response "About to delete record with luid %s." luid-to-delete )
		     (bbdb-delete-record-internal record-to-delete)
		     (bbdb-save-db)
		     (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response "Record with luid %s deleted." luid-to-delete )
		     (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response "Modified record - about to create new record.")
		     (let* ((newrecord (bbdb-vcard-snarf 
					(dom-node-text-content (car (xpath-resolve replace-node "child::Item/child::Data"))))))			 
		       (bbdb-record-putprop newrecord 'luid luid-to-delete)
		       (bbdb-record-putprop newrecord 'creation-date syncml-current-timestamp)
		       ;; because of delay in this program, we temporarily disabling the automatic timestamp hook, as we want to use the 
		       ;; syncml-current-timestamp instead,
		       ;; otherwise this record will be tagged as modified during next sync even if no changes was made.
		       (remove-hook 'bbdb-change-hook 'bbdb-timestamp-hook)
		       (sleep-for 1)
		       (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response "BBDB-CHANGE-HOOK: %S" 
					  (describe-variable 'bbdb-change-hook))

		       (bbdb-record-putprop newrecord 'timestamp syncml-current-timestamp)		     
		       (bbdb-save-db)
		       (add-hook 'bbdb-change-hook 'bbdb-timestamp-hook)
		       (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response "Modified record - new record created with same luid %s as old." 
					  (bbdb-record-getprop newrecord 'luid))
		       (let* ((replace-status-node 
			       (syncml-create-status-command
				pkg5-doc
				(dom-node-text-content (car (xpath-resolve (dom-document-element syncml-response-doc) 
									   "descendant::MsgID")))
				(dom-node-text-content (car (xpath-resolve replace-node "child::CmdID")))
				"Replace"
				(syncml-create-data-command pkg5-doc "201") ;; 201 = the requested item was added.
				nil
				(syncml-create-sourceref-command pkg5-doc (bbdb-record-getprop newrecord 'luid)))))
			 (bbdb-syncml-debug 2 'bbdb-syncml-process-modifications-response "Adding <Status> node for the modified record to pkg5")
			 (dom-node-append-child (car (xpath-resolve (dom-document-element pkg5-doc)
								    "descendant::SyncBody"))
						replace-status-node)
			 ;; record updated.  no changes to the mapping list.
			 (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response "<Status> command created for new record.")
		     )))))

	       ;; do all <Delete> commands.
	       (dolist (delete-node (xpath-resolve node "child::Delete")
			     nil)
		 (progn 
		   (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response "Delete command from server." )
		   (let* ((luid-to-delete (dom-node-text-content (car (xpath-resolve delete-node "child::Item/child::Target/child::LocURI"))))
			  (record-to-delete (car (bbdb-syncml-get-record-by-luid luid-to-delete))))
		     (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response "About to delete record with luid %s." luid-to-delete )
		     (bbdb-delete-record-internal record-to-delete)
		     (bbdb-save-db)
		     (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response "Record with luid %s deleted." luid-to-delete )
		     (let* ((delete-status-node 
			     (syncml-create-status-command
			      pkg5-doc
			      (dom-node-text-content (car (xpath-resolve (dom-document-element syncml-response-doc) 
									 "descendant::MsgID")))
			      (dom-node-text-content (car (xpath-resolve delete-node "child::CmdID")))
			      "Delete"
			      (syncml-create-data-command pkg5-doc "210") ;; 210 = Delete without archive.
			      nil
			      (syncml-create-sourceref-command pkg5-doc luid-to-delete))))
		       (bbdb-syncml-debug 2 'bbdb-syncml-process-modifications-response "Adding <Status> node for the deleted record to pkg5")
		       (dom-node-append-child (car (xpath-resolve (dom-document-element pkg5-doc)
								  "descendant::SyncBody"))
					      delete-status-node)
		       ;; record deleted.  update mapping list.
		       (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response "<Status> command created for deleted record.")
		       (setq bbdb-syncml-pkg5-ok-luids (delete (bbdb-syncml-element-to-number luid-to-delete) bbdb-syncml-pkg5-ok-luids))
		       ))))
		   
		   
		   
	       ;; if processing of all children was succeccful, return a 200 for the <Sync> command as well. (CAN THIS BE TRUE?  
	       ;; the whole sync as such should be ok even if one indidual record was not successfully synced?)
	       ;; if processing of all children was _not_ sucessfull, i don't know what to return. read syncml standard.
	       (if (not (null all-ok))
		   (progn 
		     (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response "All sync commands completed successfully. Adding <Status> for <Sync>-command.")
		     (let* ( ;;(temp-doc (syncml-create-syncml-document))
			    ;;(temp-node (dom-document-element temp-doc))
			    (status-node 
			     (syncml-create-status-command
			      pkg5-doc
			      (dom-node-text-content (car (xpath-resolve (dom-document-element syncml-response-doc) 
									 "descendant::MsgID")))
			      (dom-node-text-content (car (xpath-resolve (dom-document-element syncml-response-doc)
									 "descendant::Sync/child::CmdID")))
			      "Sync"
			      (syncml-create-data-command pkg5-doc "200") 
			      (syncml-create-target-command pkg5-doc 
							    (dom-node-text-content 
							     (car (xpath-resolve (dom-document-element syncml-response-doc)
										 "descendant::Sync/child::Target/child::LocURI"))))
			      (syncml-create-source-command pkg5-doc 
							    (dom-node-text-content 
							     (car (xpath-resolve (dom-document-element syncml-response-doc) 
										 "descendant::Sync/child::Source/child::LocURI")))))))
		       (bbdb-syncml-debug 2 'bbdb-syncml-process-modifications-response "created status node.")
		       ;;(dom-node-append-child temp-node status-node)
		       ;;(bbdb-syncml-debug 2 'bbdb-syncml-process-modifications-response "Document is: %S." (dom-node-write-to-string status-node))
		       ;;(dom-node-insert-before temp-syncbody-node 
		       ;;				    status-node 
		       ;;				    (dom-node-first-child temp-syncbody-node)))))
			   
		       ;;	    (dom-node-test-new-child pkg5-syncbody-node status-node)
		       (dom-node-append-child (car (xpath-resolve (dom-document-element pkg5-doc)
								  "descendant::SyncBody"))
					      status-node)
		       )))))
	    (t 
	     (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response 
				"Unknown command %s. Ignoring and proceeding to next." (dom-element-name node))))

      (bbdb-syncml-debug 2 'bbdb-syncml-process-modifications-response "OK luids    : %S" bbdb-syncml-pkg5-ok-luids)
      (bbdb-syncml-debug 2 'bbdb-syncml-process-modifications-response "Not OK luids: %S" bbdb-syncml-pkg5-not-ok-luids)
      (bbdb-syncml-debug 2 'bbdb-syncml-process-modifications-response "Function finished."))))
  
	


  
(defun bbdb-syncml-element-to-number (item)
  "Returns ITEM as a number, whether it already is a number, or a string"
  (if (stringp item)
      (string-to-number item)
    (car (list item))))
  
(provide 'bbdb-syncml)
  