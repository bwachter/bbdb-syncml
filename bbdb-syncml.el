;;; bbdb-syncml.el -- A SyncML client for the BBDB database.
;; $Id: bbdb-syncml.el,v 1.7 2004/01/25 11:57:14 joergenb Exp $

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
(defun bbdb-syncml-synchronize () 
  "Synchronizes the bbdb database with the SyncML server.

See chapter 5 in the 'SyncML Sync Protocol' document available from www.syncml.org"
  (interactive)
  ;; do some initialization
  (message "SyncML synchronization started...")
  (bbdb-syncml-debug 1 'bbdb-syncml-synchronize "STARTING SYNCHRONIZATION %S" bbdb-syncml-bbdb-database )
   
  ;; check last sync time -get from .bbdb.syncml
  (setq bbdb-syncml-last-sync (bbdb-syncml-get-last-sync))
  
  ;; send initialization package to server.
  ;; the syncml-init function will break if an error occurred.
  (bbdb-syncml-debug 1 'bbdb-syncml-synchronize "Sending initalization package #1 to server." )
  (syncml-init)
  (syncml-process-response)

  ;; if the server sent response 508 to the SYNC command, then syncing should be slow.  
  ;; syncml-init sets the global variable SYNCML-DOING-SLOW-SYNC to 't in this case.  
  (if 'syncml-doing-slow-sync
      
      ;; Slow sync triggered.  We just send <add> commands for all records in the bbdb database.
      (progn
	(message "Slow sync forced by server. Sending full database...")
	(bbdb-syncml-debug 1 'bbdb-syncml-synchronize "Slow sync forced by server.")
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
				     (syncml-create-data-command syncml-transmit-doc 
								 (dom-node-text-content (car (xpath-resolve 
											      (dom-document-element syncml-response-doc) 
											      "descendant::Status/child::Data[position()=\"1\"]"))))
				     (syncml-create-target-command syncml-transmit-doc syncml-target-locuri)
				     (syncml-create-source-command syncml-transmit-doc syncml-source-locuri)	  
				     ))
	       ;; the <Status> in response to the <Alert>
	       (status-alert-node (syncml-create-status-command
				   syncml-transmit-doc
				   (dom-node-text-content (car (xpath-resolve (dom-document-element syncml-response-doc)
									      "descendant::MsgID")))
				   (dom-node-text-content (car (xpath-resolve (dom-document-element syncml-response-doc)
									      "descendant::Alert/child::CmdID")))
				   "Alert"
				   (syncml-create-data-command syncml-transmit-doc 
							       (dom-node-text-content (car (xpath-resolve 
											    (dom-document-element syncml-response-doc) 
											    "descendant::Status/child::Data[position()=\"2\"]"))))
				   (syncml-create-target-command syncml-transmit-doc syncml-target-database)
				   (syncml-create-source-command syncml-transmit-doc syncml-source-database)	  
				   ))       
	       ;; the <Sync> node
	       (syncnode (syncml-create-sync-command syncml-transmit-doc))
	       ;; a list of which luids shall be added
	       (bbdb-syncml-added-luids (bbdb-syncml-get-all-records))
	       )

	  ;; Add the <SyncHdr> and <SyncBody> nodes to the <SyncML> node.
	  (dom-node-append-child syncmlnode synchdrnode)
	  (dom-node-append-child syncmlnode syncbodynode)

	  ;; add the <Status> command in response for the <SyncHdr> from server, 
	  ;; as first child to the <SyncBody>
	  (dom-node-append-child syncbodynode status-synchdr-node)
				  
	  ;; add the <Status> command in response for the <Alert> from server, 
	  ;; as second child to the <SyncBody>
	  (dom-node-append-child syncbodynode status-alert-node)

	  ;; create a <Sync> command to hold all the <Add> commands.
	  (dom-node-append-child syncbodynode syncnode)

	  ;; The header and status commands are finished.  
	  (bbdb-syncml-debug 1 'bbdb-syncml-synchronize "Skeleton DOM tree for package #3 prepared.")
	  (bbdb-syncml-debug 2 'bbdb-syncml-synchronize (dom-node-write-to-string syncml-transmit-doc 1))

	  ;; go through all the bbdb records, and add them to the <Sync> element. 
	  (bbdb-syncml-debug 1 'bbdb-syncml-synchronize "Processing all bbdb records." )
	  (dolist (luid bbdb-syncml-added-luids)
	    (bbdb-syncml-debug 2 'bbdb-syncml-synchronize "Processing luid %S " luid)
	    (let ((temp-add-node
		   (syncml-create-add-command syncml-transmit-doc 
					      (syncml-create-item-command syncml-transmit-doc
									  nil
									  (syncml-create-source-command syncml-transmit-doc luid)
									  (syncml-create-data-command syncml-transmit-doc (bbdb-vcard-export-get-record-as-vcard
															   (car (bbdb-syncml-get-record-by-luid luid)))))
					      (syncml-create-meta-command syncml-transmit-doc
									  (syncml-create-metinf-type-command
									   syncml-transmit-doc
									   "text/x-vcard")))))
	      ;;	      (bbdb-syncml-debug 2 'bbdb-syncml-synchronize "<Add>command representation: %S " temp-add-node)
	      (dom-node-append-child syncnode temp-add-node)))
	  
	  ;; add a <Final> node
	  (dom-node-append-child syncbodynode (syncml-create-final-command syncml-transmit-doc))
	  
	  ;; finished constructing the DOM tree.
	  (bbdb-syncml-debug 1 'bbdb-syncml-synchronize "Package #3 DOM tree finished. Sending to server...")
	  (bbdb-syncml-debug 2 'bbdb-syncml-synchronize (dom-node-write-to-string syncml-transmit-doc))
	  
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
								       "descendant::Status/child::Data[position()=\"1\"]"))))
				    (syncml-create-target-command bbdb-syncml-package-5-doc syncml-target-locuri)
				    (syncml-create-source-command bbdb-syncml-package-5-doc syncml-source-locuri)))
	    
	    ;; The header and status commands are finished.  
	    (bbdb-syncml-debug 1 'bbdb-syncml-synchronize "Base DOM tree for package #5 prepared.")	  
	    (bbdb-syncml-debug 2 'bbdb-syncml-synchronize "\n%S" (dom-node-write-to-string bbdb-syncml-package-5-doc 1))
	    
	    ;; PROCESS PACKAGE #4 from server, and update BBDB-SYNCML-PACKAGE-5-DOC
	    (bbdb-syncml-debug 1 'bbdb-syncml-synchronize "Processing package #4 from server...")
	    ;;	    (let ((pk5-syncbody-node (bbdb-syncml-process-modifications-response bbdb-syncml-package-5-doc))) 	      
	    ;;(dom-node-append-child synchdr-pkg5-node (bbdb-syncml-process-modifications-response bbdb-syncml-package-5-doc)))
	    (bbdb-syncml-process-modifications-response bbdb-syncml-package-5-doc)
	    
	    (dom-node-append-child syncbody-pkg5-node (syncml-create-final-command bbdb-syncml-package-5-doc)))

	  ;; finished creating package 5. sending to server.
	  (bbdb-syncml-debug 2 'bbdb-syncml-synchronize "Modifications processed. Package #5 is now: \n%S" 
			     (dom-node-write-to-string bbdb-syncml-package-5-doc 2))
	  
	  (syncml-send-message-with-curl bbdb-syncml-package-5-doc)
	  ;; send package #5 to the server.  we must get back package #6 (Map Acknowledgement to client)
	  (message "Server map acknowledgement. Checking for errors...")
	  (message "Synchronization complete!")
	  ))				;end of slow sync
    ;;
    ;; normal fast sync
    ;; 
    (progn 
      (bbdb-syncml-debug 1 'bbdb-syncml-synchronize "Doing normal two-way sync. Preparing header" )
      (insert (syncml-header))
      ;; find all records added, modified and deleted in BBDB since last sync time 
      
      (bbdb-syncml-debug 1 'bbdb-syncml-synchronize "Processing added records." )
      (let ((bbdb-syncml-added-luids (bbdb-syncml-get-added-records)))
	(set-buffer (get-buffer-create "*syncml-transmit*"))
	(goto-char (point-max))
	(bbdb-syncml-debug 1 'bbdb-syncml-synchronize 
			   "Got this list of added luids: %S" bbdb-syncml-added-luids)
	(dolist (luid bbdb-syncml-added-luids)
	  (bbdb-syncml-debug 2 'bbdb-syncml-synchronize "Processing luid %S " luid)
	  (let ((temp-add-command (syncml-create-add-command 
				   luid 
				   (bbdb-vcard-export-get-record-as-vcard (car (bbdb-syncml-get-record-by-luid luid))))))
					;	    (bbdb-syncml-debug 2 'bbdb-syncml-synchronize "<Add> command representation: %S " (dom-node-write-to-string temp-add-command)
	    (insert temp-add-command))))     
      
      ;; should create-xxx-commands increase the <CmdID> ?     
      ;; send a sync request for those records. (package #3)

      ;; parse the responses, and write the results back to the BBDB database. (package #4)
      ;; some of the server's responses requires the client to respond back. (ie: the ADD command from
      ;; the server should be acknowledged by the MAP command from the client with the LUID assigned. (clients 
      ;; always assign luid.)
      
      ;; finally, update the last sync timestamp in the mapping file,
      ;; and send a XXX command back to the server to indicate a successful sync (?)
      
      )))    


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
  (bbdb-syncml-debug 1 'bbdb-syncml-get-added-records "Started")
  ;; ensure that the next luid is up-to-date
  (bbdb-syncml-validate-luids nil)
  (setq bbdb-syncml-next-luid (bbdb-syncml-get-next-luid))
  
  ;; also ensure that the mapping file lists are up-to-date
  (setq bbdb-syncml-mapping-luid-list (bbdb-syncml-read-mapping-file))
  
  ;; if all is ok, then iterate over all bbdb records, and put the luid of the added ones in the list added-luid-list
  (let (added-luid-list)
    (dolist (node (bbdb-records) added-luid-list)
      (bbdb-syncml-debug 1 'bbdb-syncml-get-added-records 
			 "Checking record with name and company: %S - %S" 
			 (bbdb-record-name node) 
			 (bbdb-record-company node))
      (if (and (not (null (bbdb-record-getprop node 'luid)))
	       (member (string-to-number (bbdb-record-getprop node 'luid)) bbdb-syncml-mapping-luid-list))
	  ;; record has a luid and is present in the mapping file. It is not added since last successful sync
	  (bbdb-syncml-debug 1 'bbdb-syncml-get-added-records
			     "LUID %S already exists in the mapping file." 
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
  (bbdb-syncml-validate-luids nil)
  (setq bbdb-syncml-next-luid (bbdb-syncml-get-next-luid))
  
  ;; also ensure that the mapping file lists are up-to-date
  (setq bbdb-syncml-mapping-luid-list (bbdb-syncml-read-mapping-file))
  
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
			     "Node has timestamp of %S. %S" record-timestamp last-timestamp)
	  (if (string< record-timestamp last-timestamp)
	      ;; not changed since last synctime
	      (bbdb-syncml-debug 1 'bbdb-syncml-get-modified-records "Record not changed.")
	    (bbdb-syncml-debug 1 'bbdb-syncml-get-modified-records "Record CHANGED.")
	    (push record-luid modified-luid-list)))))))


(defun bbdb-syncml-get-deleted-records ()
  "Returns the LUID of records deleted since last sync.  

This function checks all current bbdb-records against the mapping file (having the
state of the bbdb-database at the time of last sync."

  (let (deleted-luids)	

    ;; first, put all luids in (bbdb-records) into a list
    (let (bbdb-records-luids)
      (dolist (node (bbdb-records) bbdb-records-luids)
	(push (bbdb-record-getprop node 'luid) bbdb-record-luids))
			
      ;; then, iterate over all luids in the mapping list, and return all which are present in mapping, 
      ;; but not (bbdb-records)
      (dolist (node bbdb-syncml-mapping-members deleted-luids)
	(if (not (member node (bbdb-records-luids)))
	    (push node deleted-luids))))))


(defun bbdb-syncml-get-last-sync () 
  "Gets the last sync time from the mapping file"
  (bbdb-syncml-debug 1 'bbdb-syncml-get-last-sync "Triggered")
  (set-buffer (find-file-noselect bbdb-syncml-mapping-file))
  (goto-char (point-min))
  (if (not (re-search-forward ";;; last sync timestamp: \\(.*\\)" nil t))
      (bbdb-syncml-debug 1 'bbdb-syncml-get-last-sync "Not found.")
    (bbdb-syncml-debug 1 'bbdb-syncml-get-last-sync "Found: %S" (match-string 1))
    (match-string 1)))


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
  (insert ";;; Mapping below in the form:\n")
  (insert ";;; LUID - timestamp\n")
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
  "Reads the mapping file, and put the list of successful luids in a list.
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
NOTE: should only be called after syncing is finished"
  )


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
If REASSIGN is false, the function is aborted."
  (bbdb-syncml-debug 1 'bbdb-syncml-validate-luids "Triggered. (bbdb-records) has %S records." (length (bbdb-records)))
  ;; should make sure we are comparing against the most current next-luid.
  (setq bbdb-syncml-next-luid (bbdb-syncml-get-next-luid))
  (bbdb-syncml-debug 1 'bbdb-syncml-validate-luids "Value of next luid: %S" bbdb-syncml-next-luid)
  
  (dolist (node (bbdb-records) nil)
    (bbdb-syncml-debug 2 'bbdb-syncml-validate-luids "examining a node: %S" node)
    (if (null (bbdb-record-getprop node 'luid))
	;; record does not have a luid. All is well.
	(bbdb-syncml-debug 1 'bbdb-syncml-validate-luids "node doesn't have a LUID. all is well")
      ;; record has a luid. message it for debug.
      (bbdb-syncml-debug 1 'bbdb-syncml-validate-luids "Validating BBDB. Found LUID for %S: %S " (bbdb-record-name node) (bbdb-record-getprop node 'luid))
      (if (>= (string-to-number (bbdb-record-getprop node 'luid)) (string-to-number bbdb-syncml-next-luid))
	  (progn (bbdb-syncml-debug 1 'bbdb-syncml-validate-luids "ERROR: Luid %S is greater than value of next luid %S." (bbdb-record-getprop node 'luid) bbdb-syncml-next-luid)
		 (if reassign
		     (progn (bbdb-syncml-debug 1 'bbdb-syncml-validate-luids "Reassigning luids")
			    (bbdb-syncml-set-next-luid 
			     (+ (string-to-number (bbdb-record-getprop node 'luid)) 1)))
		   (bbdb-syncml-debug 1 'bbdb-syncml-validate-luids "No reassigning luids.  Aborting...")
		   (error "LUID greater than next value")))))))

(defun bbdb-syncml-create-luid-hook ()
  "This function should be called whenever a new bbdb record is created.
The value of LUID to assign to the new record is picked from the 
bbdb-syncml-mapping-file"
  ())

;;;;;;;;;;;;;;;;;;;
					;
					; working directly towards the bbdb_
					;
;;;;;;;;;;;;;;;;;;
(defun bbdb-syncml-get-record-by-luid (luid)
  "Return the record with the given LUID"
  (let ((notes (cons (intern "luid") (concat "^" luid "$"))))
    (bbdb-search (bbdb-records) nil nil nil notes)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; syncml process functions
;;
;; these functions are supposed to override (or: be used instead of) the default syncml-process-xxx-command in
;; syncml.el.  They include bbdb-specific actions to take in different scenarios, like:
;; *) if an item was successfully sync'ed (ie: server returned 200), the timestamp in bbdb should be updated.
;;
;;;;;;;;;;;;;;;;;;

(defun bbdb-syncml-process-modifications-response (pkg5-doc)
  "This functions processes the response package #4 from the server."
  (if (not (dom-document-p pkg5-doc))
      (throw 'wrong-type nil))
;;  (let ((tull-og-vas nil))
    (dolist (node (xpath-resolve (dom-document-element syncml-response-doc) 
				 "descendant::SyncBody/child::*")
		  nil)
      (progn 
	;;      bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response "Triggered. Is the node a dom-node?: %S" (dom-node-p node))
	;;      bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response "Is the node a dom-element?: %S" (dom-element-p node))
	(if (not (dom-element-p node))
	    (progn
	      (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response "NODE is not a dom-element.")
	      (throw 'wrong-type nil)))
	(bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response "Name of node: %S" (dom-element-name node))
	(let ((node-element-name (dom-element-name node)))
	  (bbdb-syncml-debug 3 'bbdb-syncml-process-modifications-response "Name of node: %S" (dom-element-name node))
	  (cond ((string= "Status" node-element-name) 
		 ;;bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response "Calling bbdb-syncml-process-status-command.")
		 (funcall 'bbdb-syncml-process-status-command node))
		((string= "Alert" node-element-name) 
		 ;;(bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response "Calling syncml-process-alert-command.")
		 (funcall 'syncml-process-alert-command node))
		((string= "Sync" node-element-name) 
		 ;; For <Sync> command, we need to traverse it's childs, try to do whatever requested in the BBDB, and build a response indication
		 ;; if the BBDB-modification was successfull or not.  
		 (bbdb-syncml-debug 3 'bbdb-syncml-process-modifications-response "Calling bbdb-syncml-process-sync-command.")
	;;	 (setq tull-og-vas (funcall 'bbdb-syncml-process-sync-command node pkg5-doc)))
		 (funcall 'bbdb-syncml-process-sync-command node pkg5-doc))
		(t 
		 (bbdb-syncml-debug 1 'bbdb-syncml-process-modifications-response "Unknown command %s. Ignoring." (dom-element-name node)))))))
    (bbdb-syncml-debug 3 'bbdb-syncml-process-modifications-response "Function finished."))
;;    tull-og-vas))
	





;; For <Status> in reponse to <Replace> and <Delete> commands, we need to make necessary changes to the BBDB


	
;;
;; bbdb-syncml-process-status-command (node)
;;
;; This function processes a <Status> node from the server.
;;
(defun bbdb-syncml-process-status-command (node)
  "Processes a <Status> NODE."
  (let ((syncml-cmd (dom-node-text-content (car (xpath-resolve node "child::Cmd"))))
	(syncml-data (dom-node-text-content (car (xpath-resolve node "child::Data")))))
    (bbdb-syncml-debug 1 'bbdb-syncml-process-status-command 
		       "Got a <Status> in response to a %S command with data %S: %S" 
		       syncml-cmd syncml-data (syncml-lookup-response-code syncml-data)) 
    ;; the <Status> command is used as a response to many different request. The <Cmd> and <CmdRef> tells which.
    ;; further processing must be based on this. (ie: preserve the request in some way)
    (cond ((string= "SyncHdr" syncml-cmd)
	   (progn (bbdb-syncml-debug 1 'bbdb-syncml-process-status-command "CmdRef is SyncHdr")
		  (cond ((string= "407" syncml-data)
			 (progn (bbdb-syncml-debug 1 'bbdb-syncml-process-status-command "Server said 407: Missing Credentials")) 
			 ;; must break execution and resend credentials;
			 (error "Authentication rejected"))
			((not (or (string= syncml-data "212") ;; we accept only 200 and 212 as valid for further processing.
				  (string= syncml-data "200")))
			 (progn (bbdb-syncml-debug 1 'bbdb-syncml-process-status-command 
						   "ERROR. Server said: %S: %S" syncml-data (syncml-lookup-response-code syncml-data)) 
				(error (syncml-lookup-response-code syncml-data)))))
		  (bbdb-syncml-debug 1 'bbdb-syncml-process-status-command "Response code %s is OK. Continuing..." syncml-data)))
	  ((string= "Alert" syncml-cmd)
	   (cond ((string= "404" syncml-data)
		  (progn (bbdb-syncml-debug 1 'bbdb-syncml-process-status-command "Server said 404: Not found"))
		  (error "Target database not found"))
		 ((string= "508" syncml-data)
		  (progn (bbdb-syncml-debug 1 'bbdb-syncml-process-status-command "Server said 508: Refresh required. Initiating slow sync"))
		  (setq syncml-doing-slow-sync 't))
		 ((not (string= syncml-data "200"))
		  (progn (bbdb-syncml-debug 1 'bbdb-syncml-process-status-command "ERROR. Server said %S" syncml-data)
			 (error "Error in request")))))
	  ((string= "Sync" syncml-cmd)
	   (bbdb-syncml-debug 1 'bbdb-syncml-process-status-command "<Sync> command."))
	  ((string= "Add" syncml-cmd)
	   (bbdb-syncml-debug 1 'bbdb-syncml-process-status-command "<Add> command.")))
    
    (bbdb-syncml-debug 1 'bbdb-syncml-process-status-command "Finished.")))



;;
;; bbdb-syncml-process-sync-command (node)
;;
;; This function processes a <Sync> command from the server.
;;
(defun bbdb-syncml-process-sync-command (syncnode pkg5-doc)
  "Processes a <Sync> NODE."
  (bbdb-syncml-debug 1 'bbdb-syncml-process-sync-command "Got a <Sync> command: %S." (dom-element-name syncnode))

  ;; go thourgh all the children of the <Sync> command NODE.
  ;; they are either <Add>, <Replace> or <Delete>.
  ;; for each child, interact with the BBDB in the proper way.
  ;; and create a 200 <Status>-command to be returned.

  ;; do all <Add> commands.
  (let* ((all-ok t)
;;	 (temp-syncbody-node (car (xpath-resolve (dom-document-element pkg5-doc)
;;						 "descendant::SyncBody"))))
	 )
    ;; do all <Add> commands.
    ;;    (dolist (node (xpath-resolve syncnode "child::Add")
    ;;		  nil)
    ;;     (progn 
    ;;	(bbdb-syncml-debug 1 'bbdb-syncml-process-sync-command "On <Add>: %S" (dom-node-name node))))
    ;;    ;; do all <Replace> commands.
    ;;    (dolist (node (xpath-resolve syncnode "child::Replace")
    ;;		  nil)
    ;;      (progn 
    ;;	(bbdb-syncml-debug 1 'bbdb-syncml-process-sync-command "On <Replace>: %S" (dom-node-name node))))
    ;;    ;; do all <Delete> commands.
    ;;    (dolist (node (xpath-resolve syncnode "child::Delete")
    ;;		  nil)
    ;;     (progn 
    ;;	(bbdb-syncml-debug 1 'bbdb-syncml-process-sync-command "On <Delete>: %S" (dom-node-name node))))
    
    (dolist (node (xpath-resolve syncnode "child::*")
		  nil)
      (bbdb-syncml-debug 3 'bbdb-syncml-process-sync-command "Child %S" (dom-node-name node)))
    
    ;; if processing of all children was succeccful, return a 200 for the <Sync> command as well.
    ;; if processing of all children was _not_ sucessfull, i don't know what to return. read syncml standard.
    (if (not (null all-ok))
	(progn 
	  (bbdb-syncml-debug 1 'bbdb-syncml-process-sync-command "All sync commands completed successfully. Adding <Status> for <Sync>-command.")
	  (let* (;;(temp-doc (syncml-create-syncml-document))
		 ;;(temp-node (dom-document-element temp-doc))
		 (status-node 
		  (syncml-create-status-command
		   pkg5-doc
		   (dom-node-text-content (car (xpath-resolve (dom-document-element syncml-response-doc) 
							      "descendant::MsgID")))
		   (dom-node-text-content (car (xpath-resolve syncnode "child::CmdID")))
		   "Sync"
		   (syncml-create-data-command pkg5-doc "200") 
		   (syncml-create-target-command pkg5-doc 
						 (dom-node-text-content 
						  (car (xpath-resolve syncnode "child::Target/child::LocURI"))))
		   (syncml-create-source-command pkg5-doc 
						 (dom-node-text-content 
						  (car (xpath-resolve syncnode "child::Source/child::LocURI")))))))
	   	    
	    (bbdb-syncml-debug 2 'bbdb-syncml-process-sync-command "created status node.")
	    ;;(dom-node-append-child temp-node status-node)
	    ;;(bbdb-syncml-debug 2 'bbdb-syncml-process-sync-command "Document is: %S." (dom-node-write-to-string status-node))
	    ;;(dom-node-insert-before temp-syncbody-node 
;;				    status-node 
;;				    (dom-node-first-child temp-syncbody-node)))))
	    
    ;;	    (dom-node-test-new-child pkg5-syncbody-node status-node)
	    (dom-node-append-child (car (xpath-resolve (dom-document-element pkg5-doc)
						       "descendant::SyncBody"))
				   status-node)
	    (bbdb-syncml-debug 2 'bbdb-syncml-process-sync-command "finished.")
	    )))))



;;    temp-syncbody-node))

;;	    )))))


  

(provide 'bbdb-syncml)
