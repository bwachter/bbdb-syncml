;;; bbdb-syncml.el -- A SyncML client for the BBDB database.
;; $Id: bbdb-syncml.el,v 1.1 2003/02/06 14:14:22 joergenb Exp $

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
	"*The timestamp at the previous successful synchronization")

(defun bbdb-syncml-synchronize () 
	"Synchronizes the bbdb database with the SyncML server."

	;;do some initialization

	;;check last sync time -get from .bbdb.syncml
	(setq bbdb-syncml-last-sync (bbdb-syncml-get-last-sync))

	;; send initializations package to server.
	;; here, we should probably do:
	;;	(let ((bbdb-init-header (syncml-header myServer myDatabase))
	;;				(bbdb-init-alert (syncml-create-alert-command syncml-constant-two-way)))
	;;		(syncml-init bbdb-init-header bbdb-init-alert))
	(syncml-init)

	;; if the user f.ex. would like to sync his bbdb both with a personal syncml database, 
	;; and perhaps a (read-only) company syncml database, i believe you could either send two 
	;; ALERT commands in the sync init message, or do it as two separate syncs, as the Target/GUID pair 
	;; would be different between the two databases.


	;; if response ok, 
	;;find all records added, modified and deleted in BBDB since last sync time 
	;;-either get from .bbdb.syncml, or (better) from timestamp in .bbdb
	;; note: if the server forced a SLOW SYNC, what then?
;;	(let ((bbdb-syncml-added-records (bbdb-syncml-get-added-records bbdb-syncml-last-sync))
;;				(bbdb-syncml-modified-records (bbdb-syncml-get-modified-records bbdb-syncml-last-sync))
;;				(bbdb-syncml-deleted-records (bbdb-syncml-get-deleted-records bbdb-syncml-last-sync)))
;;		())
	(set-buffer (get-buffer-create "*syncml-transmit*"))
	(erase-buffer)
	(let ((bbdb-syncml-added-records (bbdb-syncml-get-added-records)))
		(dolist 
				(record bbdb-syncml-added-records)
			(insert (syncml-create-add-command record))
			;; should create-xxx-commands increase the <CmdID> ?
			(insert (syncml-header))
			(syncml-transmit-xml-buffer (get-buffer "*syncml-transmit*"))
			)))


	;; send a sync request for those records.
	;; parse the responses, and write the results back to the BBDB database.
	;; if the server sent response 508 to the SYNC command, the redo the synchronization init 
	;; with a slow sync.
	;; some of the server's responses requires the client to respond back. (ie: the ADD command from
	;; the server should be replyed by the MAP command from the client with the LUID assigned. (clients 
	;; always assign luid.)

	;; finally, update the last sync timestamp in the mapping file,
	;; and send a XXX command back to the server to indicate a successful sync (?)
	)

(defun bbdb-syncml-get-added-records (&optional timestamp)
	"Returns the LUID of records added since last sync.

An added record is a record without luid. (is this the wisest way to check?)

TODO: this function assigns a new luid to the record.  this is maybe not smart.
if syncing fails, at least a rollback scheme must be used.  

better for the function to return a list of records, and assign the luids temporary, and save 
locally only if a succesful sync is made? (note: there is returned a <status> command from the server 
for this.
"
	(bbdb-syncml-debug 'bbdb-syncml-get-added-records "Started")
;; ensure that the next luid is up-to-date
	(setq bbdb-syncml-next-luid (bbdb-syncml-get-next-luid))
	(let (node-list)
		(dolist (node (bbdb-records) node-list)
			(bbdb-syncml-debug 'bbdb-syncml-get-added-records 
												 "Checking node: %S - %S" 
												 (bbdb-record-name node) 
												 (bbdb-record-company node))
			(if (not (null (bbdb-record-getprop node 'luid)))
					;; record has a luid. just debug.
					(bbdb-syncml-debug 'bbdb-syncml-get-added-records
														 "Found LUID %S " 
														 (bbdb-record-getprop node 'luid))
				;; record does not have a luid. Create one, and increment counter.
				;; what if syncing fails, then the LUID should be removed, so it will be marked
				;; for addition the next time ?
				(bbdb-syncml-debug 'bbdb-syncml-get-added-records "Node unhas luid. Assign a luid and schedule it for addition.")
				(bbdb-record-putprop node 'luid bbdb-syncml-next-luid)
				(bbdb-syncml-debug 'bbdb-syncml-get-added-records "Saving database...")
				(bbdb-save-db)
				(push bbdb-syncml-next-luid node-list)
				(bbdb-syncml-increment-luid)))))

(defun bbdb-syncml-get-modified-records (last-timestamp)
	"Returns the LUID of records modified since last sync.

Checks the timestamp against the last sync value."
	(bbdb-syncml-debug 'bbdb-syncml-get-modified-records "Started with timestamp: %S" last-timestamp)
;; ensure that the next luid is up-to-date
	(setq bbdb-syncml-next-luid (bbdb-syncml-get-next-luid))
	(let (node-list)
		(dolist (node (bbdb-records) node-list)
			(bbdb-syncml-debug 'bbdb-syncml-get-modified-records 
												 "Checking node: %S - %S" 
												 (bbdb-record-name node) 
												 (bbdb-record-company node))
			(if (null (bbdb-record-getprop node 'luid))
					;; record does not have a luid. it is added since the TIMESTAMP, it should be 
					;; triggered in the bbdb-syncml-get-added-records, not in this function.  just debug.
					(bbdb-syncml-debug 'bbdb-syncml-get-modified-records
														 "No LUID for record." )
				;; record does have a luid. 
				;; check if it's modified timestamp is newer than the timestamp of the last sync.
				(let ((record-timestamp (bbdb-record-getprop node 'timestamp))
							(record-luid (bbdb-record-getprop node 'luid)))
					(bbdb-syncml-debug 'bbdb-syncml-get-modified-records 
														 "Node has timestamp of %S. %S" record-timestamp last-timestamp)
					(if (string< record-timestamp last-timestamp)
							;; not changed since last synctime
							(bbdb-syncml-debug 'bbdb-syncml-get-modified-records "Record not changed.")
						(bbdb-syncml-debug 'bbdb-syncml-get-modified-records "Record CHANGED.")
						(push record-luid node-list)))))))


(defun bbdb-syncml-get-deleted-records ()
	"Returns the LUID of records deleted since last sync.  

This function checks all current bbdb-records against the mapping file (having the
state of the bbdb-database at the time of last sync."
	())


(defun bbdb-syncml-get-last-sync () 
	"Gets the last sync time from the mapping file"
	(bbdb-syncml-debug 'bbdb-syncml-get-last-sync "Triggered")
	(set-buffer (find-file-noselect bbdb-syncml-mapping-file))
	(goto-char (point-min))
	(if (not (re-search-forward ";;; last sync timestamp: \\(.*\\)" nil t))
			(bbdb-syncml-debug 'bbdb-syncml-get-last-sync "Not found.")
		(bbdb-syncml-debug 'bbdb-syncml-get-last-sync "Found: %S" (match-string 1))
		(match-string 1)))


(defun bbdb-syncml-initialize () 
	"Prepares the BBDB database to support SyncML. Should only be called once.
Creates the mapping file, and adds the luid field to the database."
	(bbdb-add-new-field 'luid)
	(bbdb-save-db)

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

	(bbdb-syncml-assign-luids)
	)

(defun bbdb-syncml-open-mapping-file ()
	"Opens the mapping file and sets next LUID."
	(setq bbdb-syncml-mapping buffer (find-file-noselect bbdb-syncml-mapping-file))
	(set-buffer bbdb-syncml-mapping-buffer)
	(setq bbdb-syncml-next-luid (bbdb-syncml-get-next-luid)))

(defun bbdb-syncml-get-next-luid ()
	"Read the value of next LUID from the mapping file."
	(set-buffer (find-file-noselect bbdb-syncml-mapping-file))
	(goto-char (point-min))
	(if (not (re-search-forward ";;; next LUID: \\(.*\\)" nil t))
			(progn (bbdb-syncml-debug 'bbdb-syncml-get-next-luid "Not found.")
						 (error "next luid not found in file"))
		(bbdb-syncml-debug 'bbdb-syncml-get-next-luid "Found: %S" (match-string 1))
		(match-string 1)))

(defun bbdb-syncml-update-next-luid ()
	"Sets the variable 'bbdb-syncml-next-luid based on the mapping file"
	(setq bbdb-syncml-next-luid (bbdb-syncml-get-next-luid)))

(defun bbdb-syncml-increment-luid ()
	"Increments the current luid by one, and stores the new value in the mapping file, in the bbdb-syncml-next-luid variable, and returns the new value."
	(bbdb-syncml-update-next-luid)
	(bbdb-syncml-debug 'bbdb-syncml-increment-luid "Started. Current luid is %S" bbdb-syncml-next-luid)
	(set-buffer (find-file-noselect bbdb-syncml-mapping-file))
	(goto-char (point-min))
	(if (not (re-search-forward ";;; next LUID: " nil t))
			(progn (bbdb-syncml-debug 'bbdb-syncml-increment-luid "Unable to find next-luid position in mapping file.")
						 (error "unable to find next-luid posistion in file!"))
		(kill-region  (point) (line-end-position))
		(setq bbdb-syncml-next-luid (number-to-string (+ (string-to-number bbdb-syncml-next-luid) 
																										 1)))
		(insert bbdb-syncml-next-luid)
		(save-buffer)
		(bbdb-syncml-debug 'bbdb-syncml-increment-luid "New luid: %S" bbdb-syncml-next-luid)))


(defun bbdb-syncml-assign-luids ()
	"Reads all records in the BBDB database, and assigns LUIDs to those not having one."
;; first, validate that there are no errorous luids in the database
	(bbdb-syncml-debug 'bbdb-syncml-assign-luids "Triggered. Proceeding with validate")
	(bbdb-syncml-validate-luids)
;; ensure that the bbdb-syncml-next-luid is up-to-date
	(setq bbdb-syncml-next-luid (bbdb-syncml-get-next-luid))
	(dolist (node (bbdb-records) nil)
		(bbdb-syncml-debug 'bbdb-syncml-assign-luids "Validating node: %S" node)
		(if (not (null (bbdb-record-getprop node 'luid)))
				;; record has a luid. just debug.
				(bbdb-syncml-debug 'bbdb-syncml-assign-luids 
													 "Found LUID for %S: %S " 
													 (bbdb-record-name node) 
													 (bbdb-record-getprop node 'LUID))
			;; record does not have a luid. Create one, and increment counter.
			(bbdb-syncml-debug 'bbdb-syncml-assign-luids "Node unhas luid. assigning %S." bbdb-syncml-next-luid)
			(bbdb-record-putprop node 'luid bbdb-syncml-next-luid)
			(bbdb-save-db)
			(bbdb-syncml-increment-luid)))
	(bbdb-save-db)
	(bbdb-redisplay-records)
	)

(defun bbdb-syncml-validate-luids ()
	"Reads all BBDB records and checks if any LUID is above the bbdb-syncml-next-luid."
	(bbdb-syncml-debug 'bbdb-syncml-validate-luids "Triggered. (bbdb-records) has %S records." (length (bbdb-records)))
	(bbdb-syncml-debug 'bbdb-syncml-validate-luids "Value of next luid: %S" bbdb-syncml-next-luid)
	(dolist (node (bbdb-records) nil)
		(bbdb-syncml-debug 'bbdb-syncml-validate-luids "examining a node: %S" node)
		(if (null (bbdb-record-getprop node 'luid))
				;; record does not have a luid. All is well.
				(bbdb-syncml-debug 'bbdb-syncml-validate-luids "node doesn't have a LUID. all is well")
			;; record has a luid. message it for debug.
			(bbdb-syncml-debug 'bbdb-syncml-validate-luids "Validating BBDB. Found LUID for %S: %S " (bbdb-record-name node) (bbdb-record-getprop node 'luid))
			(if (>= (string-to-number (bbdb-record-getprop node 'luid)) bbdb-syncml-next-luid)
					(progn (bbdb-syncml-debug 'bbdb-syncml-validate-luids "ERROR: Luid %S is greater than value of next luid %S." (bbdb-record-getprop node 'luid) bbdb-syncml-next-luid)
								 (error "LUID greater than next value"))))))

(defun bbdb-syncml-create-luid-hook ()
	"This function should be called whenever a new bbdb record is created.
The value of LUID to assign to the new record is picked from the 
bbdb-syncml-mapping-file"
	())

(provide 'bbdb-syncml)