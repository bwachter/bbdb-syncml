;;; syncml-commands.el -- An elisp implementation of a SyncML client. This file contains the syncml commands
;; $Id: syncml-commands.el,v 1.8 2006/04/06 20:37:05 joergenb Exp $

;; Copyright (C) 2003 Jørgen Binningsbø 

;; Author: Jørgen Binningsbø <jb at pvv.org>
;; Maintainer: Jørgen Binningsbø <jb at pvv.org>
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

;; This file contains routines for creating and manipulating the differen SyncML commands, like
;; <Sync>, <Add>, <Delete>, and so on.

(require 'xml)
(require 'dom)
(require 'dom-loadsave)

(require 'syncml-debug)
(require 'syncml-constants)

;; syncml-create-syncml-document 
(defun syncml-create-syncml-document ()
  "*Creates a new DOM document having top-level element <SyncML> and returning this element node"
  (syncml-debug 3 'syncml-create-syncml-command "Function started.")
  (let* ((syncmldoc (make-dom-document :name "MySyncMLDocument"
				       :type dom-document-node))
	 (syncmlelement (dom-document-create-element syncmldoc "SyncML")))
    (setf (dom-document-owner-document syncmldoc) syncmldoc
	  (dom-document-element syncmldoc) syncmlelement)
    syncmldoc))


;; syncml-create-synchdr-command
(defun syncml-create-synchdr-command (ownerdoc targetnode sourcenode &optional crednode metanode)
  "Returns a new <SyncHdr> DOM node. MsgID is incremented."
  (syncml-debug 3 'syncml-create-synchdr-command "Function started.")
  (syncml-increase-msgid)
  (let* ((synchdrnode (dom-document-create-element ownerdoc "SyncHdr"))
	 )
    (dom-node-append-child synchdrnode (syncml-create-verdtd-command ownerdoc))
    (dom-node-append-child synchdrnode (syncml-create-verproto-command ownerdoc))
    (dom-node-append-child synchdrnode (syncml-create-sessionid-command ownerdoc))
    (dom-node-append-child synchdrnode (syncml-create-msgid-command ownerdoc))
    (dom-node-append-child synchdrnode targetnode)
    (dom-node-append-child synchdrnode sourcenode)
    (if (not (null crednode))
	(dom-node-append-child synchdrnode crednode))
    (if (not (null metanode))
	(dom-node-append-child synchdrnode metanode))
    synchdrnode))

;; syncml-create-syncbody-command
(defun syncml-create-syncbody-command (ownerdoc)
  "*Creates a DOM element node corresponding to an empty <SyncBody> command. This node must be filled with children.

XML declaration: ((Alert | Atomic | Copy | Exec | Get | Map | Put | Results | Search | Sequence | Status | Sync | Add | Replace | Delete)+, Final?)"
  (syncml-debug 3 'syncml-create-syncbody-command "Function Started.")
  (let* ((syncbodynode (dom-document-create-element ownerdoc "SyncBody"))
	 )
    syncbodynode))
    
;; syncml-create-data-command ()
;;
;; Returns a DOM node corresponding to a SyncML <Data> command.
;; XML Declaration: (#PCDATA)
;; Parent Elements: Alert, Cred, Item, Status, Search
;;
;; Example: <Data>212<Data>   Indicates a successful authentication
(defun syncml-create-data-command (ownerdoc mydata) 
  "*Returns a DOM node corresponding to a SyncML <Data> command.

MYDATA is a string containing the #PCDATA content.
Example: <Data>212<Data>   Indicates a successful authentication."
  (syncml-debug 3 'syncml-create-data-command "Function started.")
  (let* ((datanode (dom-document-create-element ownerdoc "Data")))
    (cond ((stringp mydata)
	   (dom-node-append-child datanode (dom-document-create-text-node ownerdoc mydata)))
	  ((dom-node-p mydata)
	   (dom-node-append-child datanode mydata))
	  (t (error "Neither string nor dom-node given to the Data command.")))
    datanode))

;; syncml-create-meta-command ()
;;
;; Returns a DOM node corresponding to a SyncML <Meta> command.
;; XML Declaration: (#PCDATA)
;; Parent Elements: Add, Atomic, Chal, Copy, Cred, Delete, Get, Item, Map, Put, Replace, Results, Search, Sequence, Sync
;; 
;; The <Meta> tag can have element types as children, provided they declare namespace.
;; Example: <Meta><Data>   Indicates a successful authentication
(defun syncml-create-meta-command (ownerdoc metadata) 
  "*Returns a DOM node corresponding to a SyncML <Meta> command.

METADATA is either a string or a dom-node. if a dom-node, it's owner-document should be identical to OWNERDOC."
  (syncml-debug 3 'syncml-create-meta-command "Function started.")
  (let* ((metanode (dom-document-create-element ownerdoc "Meta")))
    (cond ((stringp metadata)
	   (dom-node-append-child metanode (dom-document-create-text-node ownerdoc metadata)))
	  ((dom-node-p metadata)
	   (dom-node-append-child metanode metadata))
	  (t (error "Neither string nor dom-node given to the Meta command.")))
    metanode))


;; syncml-create-item-ommand ()
;; 
(defun syncml-create-item-command (ownerdoc &optional targetnode sourcenode metanode datanode)
  "*Returns a DOM element node corresponding to a SyncML <Item> command"
  (syncml-debug 3 'syncml-create-item-command "Function started.")
  (let* ((itemnode (dom-document-create-element ownerdoc "Item")))
    (if (not (null targetnode))	
	(dom-node-append-child itemnode targetnode))
    (if (not (null sourcenode)) 
	(dom-node-append-child itemnode sourcenode))
    (if (not (null metanode)) 
	(dom-node-append-child itemnode metanode))
    (if (not (null datanode)) 
	(dom-node-append-child itemnode datanode))
    itemnode))


;; syncml-create-target-command () 
(defun syncml-create-target-command (ownerdoc locuri &optional locname)
  (syncml-debug 3 'syncml-create-target-command "Function started.")
  (let* ((targetnode (dom-document-create-element ownerdoc "Target"))
	 (locurinode (dom-document-create-element ownerdoc "LocURI"))
	 (locuritext (dom-document-create-text-node ownerdoc locuri))
	 (locnamenode (dom-document-create-element ownerdoc "LocName")))
    (dom-node-append-child locurinode locuritext)
    (dom-node-append-child targetnode locurinode)
    (if (not (null locname))
	(progn 
	  (dom-node-append-child locnamenode (dom-document-create-text-node ownerdoc locname))
	  (dom-node-append-child targetnode locnamenode)))
    targetnode))

;; syncml-create-source-command () 
(defun syncml-create-source-command (ownerdoc locuri &optional locname)
  (syncml-debug 3 'syncml-create-source-command "Function started.")
  (let* ((sourcenode (dom-document-create-element ownerdoc "Source"))
	 (locurinode (dom-document-create-element ownerdoc "LocURI"))
	 (locuritext (dom-document-create-text-node ownerdoc locuri))
	 (locnamenode (dom-document-create-element ownerdoc "LocName")))
    (dom-node-append-child locurinode locuritext)
    (dom-node-append-child sourcenode locurinode)
    (if (not (null locname))
	(progn 
	  (dom-node-append-child locnamenode (dom-document-create-text-node ownerdoc locname))
	  (dom-node-append-child sourcenode locnamenode)))
    sourcenode))

;; create-sourceref-command
(defun syncml-create-sourceref-command (ownerdoc pcdata)
  (syncml-debug 3 'syncml-create-source-command "Function started.")
  (let* ((sourcerefnode (dom-document-create-element ownerdoc "SourceRef"))
	 (sourcereftextnode (dom-document-create-text-node ownerdoc pcdata)))
    (dom-node-append-child sourcerefnode sourcereftextnode)
    sourcerefnode))

;; create-targetref-command
(defun syncml-create-targetref-command (ownerdoc pcdata)
  (syncml-debug 3 'syncml-create-target-command "Function started.")
  (let* ((targetrefnode (dom-document-create-element ownerdoc "TargetRef"))
	 (targetreftextnode (dom-document-create-text-node ownerdoc pcdata)))
    (dom-node-append-child targetrefnode targetreftextnode)
    targetrefnode))


;; syncml-create-cred-command ()
(defun syncml-create-cred-command (ownerdoc)
  "* Returns a DOM node equivalent of the SyncML <Cred> command.

Parent Elements: Add, Alert, Copy, Delete, Exec, Get, Put, Map, Replace, Search, Status, Sync, SyncHdr
XML declaration: (Meta?, Data)"
  (syncml-debug 3 'syncml-create-cred-command "Function started.")
  (let* ((crednode (dom-document-create-element ownerdoc "Cred")))
    (if (not (null syncml-use-md5))
	(progn
	  (dom-node-append-child crednode (syncml-create-meta-command 
					   ownerdoc
					   (syncml-create-metinf-type-command ownerdoc "syncml:auth-md5")))
	  (dom-node-append-child crednode (syncml-create-data-command ownerdoc 
								      (md5 (concat syncml-user ":" syncml-passwd))))
	  crednode)
      (progn
	(dom-node-append-child crednode (syncml-create-meta-command 
					 ownerdoc
					 (syncml-create-metinf-type-command ownerdoc "syncml:auth-basic")))
	(dom-node-append-child crednode (syncml-create-data-command ownerdoc 
								    (base64-encode-string 
								     (concat syncml-user ":" syncml-passwd))))
	  crednode)
      )))


;; syncml-create-sync-command ()
;; 
;; Returns a DOM noe corresponding to the SyncML <Sync> command.
;; XML definition: CmdID, NoResp?, Cred?, Target?, Source?, Meta?, NumberOfChanges?, (Add | Atomic | Copy | Delete | Replace | Sequence)*
(defun syncml-create-sync-command (ownerdoc &optional noresp crednode targetnode  sourcenode metanode )
  "Returns a string with the <Sync> command 

XML definition: CmdID, NoResp?, Cred?, Target?, Source?, Meta?, NumberOfChanges?, (Add | Atomic | Copy | Delete | Replace | Sequence)*"
  (syncml-debug 3 'syncml-create-sync-command "Function started.")
  (let* ((syncnode (dom-document-create-element ownerdoc "Sync"))) 
    (dom-node-append-child syncnode (syncml-create-cmdid-command ownerdoc)) 
    (if (not (null noresp))
	(dom-node-append-child syncnode (dom-document-create-element ownerdoc "NoResp")))
    (if (not (null crednode))
	(dom-node-append-child syncnode crednode))
    (if (not (null targetnode))
	(dom-node-append-child syncnode targetnode))
    (if (not (null sourcenode))
	(dom-node-append-child syncnode sourcenode))
    (if (not (null metanode))
	(dom-node-append-child syncnode metanode))
    syncnode))

;; syncml-create-add-command ()
;; 
;; Returns a DOM noe corresponding to the SyncML <Add> command.
;; XML definition: CmdID, NoResp?, Cred?, Meta?, Item+)
(defun syncml-create-add-command (ownerdoc itemnode &optional metanode crednode noresp)
  "Returns a string with the <Add> command 
XML definition: 
CmdID, NoResp?, Cred?, Meta?, Item+)"
  (syncml-debug 3 'syncml-create-add-command "Function started.")
  (let* ((addnode (dom-document-create-element ownerdoc "Add")))
    (dom-node-append-child addnode (syncml-create-cmdid-command ownerdoc)) 
    (if (not (null noresp))
	(dom-node-append-child addnode (dom-document-create-element ownerdoc "NoResp")))
    (if (not (null crednode))
	(dom-node-append-child addnode crednode))
    (if (not (null metanode))
	(dom-node-append-child addnode metanode))
    (dom-node-append-child addnode itemnode)
    addnode))


;; syncml-create-replace-command ()
;; 
;; Returns a DOM noe corresponding to the SyncML <Replace> command.
;; XML definition: CmdID, NoResp?, Cred?, Meta?, Item+)
(defun syncml-create-replace-command (ownerdoc itemnode &optional metanode crednode noresp)
  "Returns a string with the <Replace> command 
XML definition: 
CmdID, NoResp?, Cred?, Meta?, Item+)"
  (syncml-debug 3 'syncml-create-replace-command "Function started.")
  (let* ((replacenode (dom-document-create-element ownerdoc "Replace")))
    (dom-node-append-child replacenode (syncml-create-cmdid-command ownerdoc)) 
    (if (not (null noresp))
	(dom-node-append-child replacenode (dom-document-create-element ownerdoc "NoResp")))
    (if (not (null crednode))
	(dom-node-append-child replacenode crednode))
    (if (not (null metanode))
	(dom-node-append-child replacenode metanode))
    (dom-node-append-child replacenode itemnode)
    replacenode))
    

;; syncml-create-delete-command ()
;; 
;; Returns a DOM noe corresponding to the SyncML <Delete> command.
;; XML definition: (CmdID, NoResp?, Archive?, SftDel?, Cred?, Meta?, Item+)
(defun syncml-create-delete-command (ownerdoc itemnode &optional metanode crednode noresp archivenode sftdelnode)
  "Returns a string with the <Delete> command 
XML definition: (CmdID, NoResp?, Archive?, SftDel?, Cred?, Meta?, Item+)"
  (syncml-debug 3 'syncml-create-delete-command "Function started.")
  (let* ((deletenode (dom-document-create-element ownerdoc "Delete")))
    (dom-node-append-child deletenode (syncml-create-cmdid-command ownerdoc)) 
    (if (not (null noresp))
	(dom-node-append-child deletenode (dom-document-create-element ownerdoc "NoResp")))
    (if (not (null crednode))
	(dom-node-append-child deletenode crednode))
    (if (not (null metanode))
	(dom-node-append-child deletenode metanode))
    (dom-node-append-child deletenode itemnode)
    deletenode))


;; syncml-create-put-command ()
;; 
;; Returns a DOM noe corresponding to the SyncML <Put> command.
;; XML definition: (CmdID, NoResp?, Lang?, Cred?, Meta?, Item+)
(defun syncml-create-put-command (ownerdoc itemnode &optional metanode crednode noresp)
  "Returns a string with the <Put> command 
XML definition: (CmdID, NoResp?, Lang?, Cred?, Meta?, Item+) "
  (syncml-debug 3 'syncml-create-put-command "Function started.")
  (let* ((putnode (dom-document-create-element ownerdoc "Put")))
    (dom-node-append-child putnode (syncml-create-cmdid-command ownerdoc)) 
    (if (not (null noresp))
	(dom-node-append-child putnode (dom-document-create-element ownerdoc "NoResp")))
    (if (not (null crednode))
	(dom-node-append-child putnode crednode))
    (if (not (null metanode))
	(dom-node-append-child putnode metanode))
    (dom-node-append-child putnode itemnode)
    putnode))
    

;; syncml-create-map-command ()
;; 
;; Returns a DOM noe corresponding to the SyncML <Map> command.
;; XML definition: CmdID, NoResp?, Cred?, Meta?, Item+)
(defun syncml-create-map-command (ownerdoc targetnode sourcenode)
  "Returns a string with the <Map> command 
XML definition: 
CmdID, NoResp?, Cred?, Meta?, Item+)"
  (syncml-debug 3 'syncml-create-map-command "Function started.")
  (let* ((mapnode (dom-document-create-element ownerdoc "Map")))
    (dom-node-append-child mapnode (syncml-create-cmdid-command ownerdoc)) 
    (if (not (null targetnode))
	(dom-node-append-child mapnode targetnode))
    (if (not (null sourcenode))
	(dom-node-append-child mapnode sourcenode))
    mapnode))

;; syncml-create-mapitem-command ()
;; 
;; Returns a DOM noe corresponding to the SyncML <Map> command.
;; XML definition: CmdID, NoResp?, Cred?, Meta?, Item+)
(defun syncml-create-mapitem-command (ownerdoc targetnode sourcenode)
  "Returns a string with the <MapItem> command 
XML definition: 
CmdID, NoResp?, Cred?, Meta?, Item+)"
  (syncml-debug 3 'syncml-create-mapitem-command "Function started.")
  (let* ((mapitemnode (dom-document-create-element ownerdoc "MapItem")))
    (if (not (null targetnode))
	(dom-node-append-child mapitemnode targetnode))
    (if (not (null sourcenode))
	(dom-node-append-child mapitemnode sourcenode))
    mapitemnode))


;; syncml-create-status-command ()
;; 
;; Returns a DOM noe corresponding to the SyncML <Status> command.
;; XML definition: CmdID, NoResp?, Cred?, Meta?, Item+)
(defun syncml-create-status-command (ownerdoc msgref cmdref cmd datanode &optional targetrefnode sourcerefnode itemnode)
  "Returns a string with the <Status> command 
XML definition: 
CmdID, MsgRef, CmdRef, Cmd, TargetRef*, SourceRef*, Cred?, Chal?, Data, Item*)"
  (syncml-debug 3 'syncml-create-status-command "Function started")
  (let* ((statusnode (dom-document-create-element ownerdoc "Status"))
	 (msgrefnode (syncml-create-msgref-command ownerdoc msgref))
	 (cmdrefnode (syncml-create-cmdref-command ownerdoc cmdref))
	 (cmdnode    (syncml-create-cmd-command    ownerdoc cmd)))
    (dom-node-append-child statusnode (syncml-create-cmdid-command ownerdoc)) 
    (dom-node-append-child statusnode msgrefnode)
    (dom-node-append-child statusnode cmdrefnode)
    (dom-node-append-child statusnode cmdnode)
    (if (not (null targetrefnode))
	(dom-node-append-child statusnode targetrefnode))
    (if (not (null sourcerefnode))
	(dom-node-append-child statusnode sourcerefnode))
    (dom-node-append-child statusnode datanode)
    (if (not (null itemnode))
	(dom-node-append-child statusnode itemnode))
    statusnode)) 
  


(defun syncml-create-alert-command (ownerdoc &optional crednode datanode itemnode norespnode)
  "Returns a string with the <Alert> command with the given ALERT-COMMAND-NUMBER

XML definition:
Alert: (CmdID, NoResp?, Cred?, Data?, Item*)
Data: When specified in an Alert, the element type specifies the type of alert. 
Item: When specified in an Alert, the element type specifies the
      parameters for the alert type. (Target?, Source?, Meta?, Data?)
"
  (syncml-debug 3 'syncml-create-alert-command "Function started")
  (let* ((alertnode (dom-document-create-element ownerdoc "Alert"))
	 (cmdidnode (syncml-create-cmdid-command ownerdoc)))
    (dom-node-append-child alertnode cmdidnode)
    (if (not (null datanode))
	(dom-node-append-child alertnode datanode))
    (if (not (null norespnode))
	(dom-node-append-child alertnode norespnode))
    (if (not (null crednode))
	(dom-node-append-child alertnode crednode))
    (if (not (null itemnode))
	(dom-node-append-child alertnode itemnode))
    alertnode))


;; syncml-create-cmdid-command
;;
(defun syncml-create-cmdid-command (ownerdoc)
  "Increments SYNCML-CURRENT-CMDID and returns a <CmdID> node."
  (syncml-debug 3 'syncml-create-cmdid-command "Function started")
  (syncml-increase-cmdid)
  (let* ((cmdidnode (dom-document-create-element ownerdoc "CmdID")))
    (dom-node-append-child cmdidnode (dom-document-create-text-node ownerdoc syncml-current-cmdid))
    cmdidnode))

;; syncml-create-sessionid-command
;;
(defun syncml-create-sessionid-command (ownerdoc)
  "Returns a <SessionID> node.  Incrementing the sessionid is not done, should be set before sync initialization. (package #1)"
  (syncml-debug 3 'syncml-create-sessionid-command "Function started")
  (let* ((sessionidnode (dom-document-create-element ownerdoc "SessionID")))
    (dom-node-append-child sessionidnode (dom-document-create-text-node ownerdoc syncml-current-sessionid))
    sessionidnode))


;; syncml-create-msgid-command
;;
(defun syncml-create-msgid-command (ownerdoc)
  "Returns a <MsgID> node.  Incrementing the msgid is not done, should be set before starting to construct each SyncML message. (The messageid shall increase by 1 for each message sent.)"
  (syncml-debug 3 'syncml-create-msgid-command "Triggered.") 
  (let* ((msgidnode (dom-document-create-element ownerdoc "MsgID")))
    (dom-node-append-child msgidnode (dom-document-create-text-node ownerdoc syncml-current-msgid))
    msgidnode))

;; syncml-create-cmd-command
;;
(defun syncml-create-cmd-command (ownerdoc cmd)
  "Returns a <Cmd> node. "
  (syncml-debug 3 'syncml-create-cmd-command "Function started.")
  (let* ((cmdnode (dom-document-create-element ownerdoc "Cmd")))
    (dom-node-append-child cmdnode (dom-document-create-text-node ownerdoc cmd))
    cmdnode))

;; syncml-create-final-command
;;
(defun syncml-create-final-command (ownerdoc)
  "Returns a <Final> node. "
  (syncml-debug 3 'syncml-create-final-command "Function started.")
  (let* ((finalnode (dom-document-create-element ownerdoc "Final")))    
    finalnode))


;; syncml-create-cmdref-command
;;
(defun syncml-create-cmdref-command (ownerdoc cmdref)
  "Returns a <CmdRef> node."
  (syncml-debug 3 'syncml-create-cmdred-command "Triggered.") 
  (let* ((cmdrefnode (dom-document-create-element ownerdoc "CmdRef")))
    (dom-node-append-child cmdrefnode (dom-document-create-text-node ownerdoc cmdref))
    cmdrefnode))

;; syncml-create-msgref-command
;;
(defun syncml-create-msgref-command (ownerdoc msgref)
  "Returns a <MsgRef> node. "
  (syncml-debug 3 'syncml-create-msgref-command "Triggered.") 
  (let* ((msgrefnode (dom-document-create-element ownerdoc "MsgRef")))
    (dom-node-append-child msgrefnode (dom-document-create-text-node ownerdoc msgref))
    msgrefnode))


;; syncml-create-verdtd-command
;;
(defun syncml-create-verdtd-command (ownerdoc)
  "Returns a <VerDTD> node.  We only support 1.1"
  (syncml-debug 3 'syncml-create-verdtd-command "Function started.")
  (let* ((verdtdnode (dom-document-create-element ownerdoc "VerDTD")))
    (dom-node-append-child verdtdnode (dom-document-create-text-node ownerdoc "1.1"))
    verdtdnode))


;; syncml-create-verproto-command
;;
(defun syncml-create-verproto-command (ownerdoc)
  "Returns a <VerProto> node.  We only support 1.1"
  (syncml-debug 3 'syncml-create-verproto-command "Function started.")
  (let* ((verprotonode (dom-document-create-element ownerdoc "VerProto")))
    (dom-node-append-child verprotonode (dom-document-create-text-node ownerdoc "SyncML/1.1"))
    verprotonode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SyncML meta information commands
;;
;; all commands create below will be declared with 'syncml:metinf' as namespace
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; syncml-create-metinf-format-command ()
;;
;; Returns a DOM node corresponding to a SyncML <Format> command.
;; XML Declaration: (#PCDATA)
;; Parent Elements: Alert, Cred, Item, Status, Search
(defun syncml-create-metinf-format-command (ownerdoc myformat) 
  "*Returns a DOM node corresponding to a SyncML <Format> command.

MYFORMAT is a string containing the #PCDATA content.
Example: <Format>212<Format>   Indicates a successful authentication."
  (syncml-debug 3 'syncml-create-metinf-format-command "Function started.")
  (let* ((formatnode (dom-document-create-element ownerdoc "Format"))
	 (formatattr (dom-document-create-attribute ownerdoc "xmlns"))
	 (textnode (dom-document-create-text-node ownerdoc myformat)))
    (setf (dom-attr-value formatattr) "syncml:metinf"
	  (dom-node-attributes formatnode) formatattr)
    (dom-node-append-child formatnode textnode)
    formatnode))


;; syncml-create-metinf-type-command ()
;;
;; Returns a DOM node corresponding to a SyncML <Type> command.
;; XML Declaration: (#PCDATA)
;; Parent Elements: Alert, Cred, Item, Status, Search
(defun syncml-create-metinf-type-command (ownerdoc typedata) 
  "*Returns a DOM node corresponding to a SyncML <Type> command.

TYPEDATA is a string containing the #PCDATA content.
Example: <Type>212<Type>   Indicates a successful authentication."
  (syncml-debug 3 'syncml-create-metinf-type-command "Function started.")
  (let* ((typenode (dom-document-create-element ownerdoc "Type"))
	 (typeattr (dom-document-create-attribute ownerdoc "xmlns"))
	 (textnode (dom-document-create-text-node ownerdoc typedata)))
    (setf (dom-attr-value typeattr) "syncml:metinf"
	  (dom-node-attributes typenode) (list typeattr))
    (dom-node-append-child typenode textnode)
    typenode))


;; syncml-create-metinf-maxmsgsize-command ()
;;
;; Returns a DOM node corresponding to a SyncML <MaxMsgSize> command.
;; XML Declaration: (#PCDATA)
;; Parent Elements: Metinf
(defun syncml-create-metinf-maxmsgsize-command (ownerdoc) 
  "*Returns a DOM node corresponding to a SyncML <MaxMsgSize> command.
Hardcoded to : <MaxMsgSize xmlns='syncml:metinf'>200000</MaxMsgSize>   
"
  (syncml-debug 3 'syncml-create-metinf-maxmsgsize-command "Function started.")
  (let* ((maxmsgsizenode (dom-document-create-element ownerdoc "MaxMsgSize"))
	 (maxmsgsizeattr (dom-document-create-attribute ownerdoc "xmlns"))
	 (textnode (dom-document-create-text-node ownerdoc "200000")))
    (setf (dom-attr-value maxmsgsizeattr) "syncml:metinf"
	  (dom-node-attributes maxmsgsizenode) (list maxmsgsizeattr))
    (dom-node-append-child maxmsgsizenode textnode)
    maxmsgsizenode))


;; syncml-create-metinf-anchor-command ()
;;
;; Returns a DOM node corresponding to a SyncML <Anchor> command.
;; XML Declaration: (Last?, Next)
;; Parent Elements: Alert, Cred, Item, Status, Search
(defun syncml-create-metinf-anchor-command (ownerdoc) 
  "*Returns a DOM node corresponding to a SyncML <Anchor> command.

ANCHORDATA is a string containing the #PCDATA content.
Example: <Anchor>212<Anchor>   Indicates a successful authentication."
  (syncml-debug 3 'syncml-create-metinf-anchor-command "Function started.")
  (let* ((anchornode (dom-document-create-element ownerdoc "Anchor"))
	 (anchorattr (dom-document-create-attribute ownerdoc "xmlns"))
	 (lastnode (dom-document-create-element ownerdoc "Last" ))
	 (nextnode (dom-document-create-element ownerdoc "Next" )))
    (setf (dom-attr-value anchorattr) "syncml:metinf"
	  (dom-node-attributes anchornode) (list anchorattr))
    (dom-node-append-child anchornode lastnode)
    (dom-node-append-child anchornode nextnode)
    (dom-node-append-child lastnode (dom-document-create-text-node ownerdoc syncml-previous-timestamp))
    (dom-node-append-child nextnode (dom-document-create-text-node ownerdoc syncml-current-timestamp))

    anchornode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SyncML device information commands
;;

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; syncml-create-devinf-devinf-command ()
;;
;; Returns a DOM node corresponding to a SyncML <DevInf> command.
;;
;; The command will be declared with 'syncml:devinf' as namespace
;;
;; XML Declaration: (VerDTD, Man?, Mod?, OEM?, FwV?, SwV?, HwV?, 
;;                   DevID, DevTyp, UTC?, SupportLargeObjs?, 
;;                   SupportNumberOfChanges?, DataStore+, CTCap*, Ext*)
;;
(defun syncml-create-devinf-devinf-command (ownerdoc datastorenode) 
  "*Returns a DOM node corresponding to a SyncML <DevInf> command."
  
  (let* ((devinfnode (dom-document-create-element ownerdoc "DevInf"))
	 (devinfattr (dom-document-create-attribute ownerdoc "xmlns")))
    (setf (dom-attr-value devinfattr) "syncml:devinf"
	  (dom-node-attributes devinfnode) (list devinfattr))
    (dom-node-append-child devinfnode (syncml-create-devinf-verdtd-command ownerdoc))
    (dom-node-append-child devinfnode (syncml-create-devinf-man-command ownerdoc))
    (dom-node-append-child devinfnode (syncml-create-devinf-mod-command ownerdoc))
    (dom-node-append-child devinfnode (syncml-create-devinf-devid-command 
				       ownerdoc
				       syncml-source-locuri))
    (dom-node-append-child devinfnode (syncml-create-devinf-devtyp-command
				       ownerdoc))
    (dom-node-append-child devinfnode datastorenode)
    devinfnode))


;; syncml-create-devinf-datastore-command ()
;;
;; Returns a DOM node corresponding to a SyncML <DataStore> command.
;;
;; XML Declaration: (SourceRef, DisplayName?, MaxGUIDSize?, Rx-Pref, Rx*, Tx-Pref, Tx*, DSMem?, SyncCap)
(defun syncml-create-devinf-datastore-command (ownerdoc sourcerefnode maxguidsizenode rxprefnode txprefnode synccapnode) 
  "*Returns a DOM node corresponding to a SyncML <DataStore> command."
  (let* ((datastorenode (dom-document-create-element ownerdoc "DataStore")))
    (dom-node-append-child datastorenode sourcerefnode)
    (dom-node-append-child datastorenode maxguidsizenode)
    (dom-node-append-child datastorenode rxprefnode)
    (dom-node-append-child datastorenode txprefnode)
    (dom-node-append-child datastorenode synccapnode)
    datastorenode))


;; syncml-create-devinf-rxpref-command ()
;;
;; Returns a DOM node corresponding to a SyncML <Rx-Pref> command.
;;
;; XML Declaration: (CTType, VerCT)
(defun syncml-create-devinf-rxpref-command (ownerdoc cttypenode verctnode) 
  "*Returns a DOM node corresponding to a SyncML <Rx-Pref> command."
  (let* ((rxprefnode (dom-document-create-element ownerdoc "Rx-Pref")))
    (dom-node-append-child rxprefnode cttypenode)
    (dom-node-append-child rxprefnode verctnode)
    rxprefnode))

;; syncml-create-devinf-txpref-command ()
;;
;; Returns a DOM node corresponding to a SyncML <Tx-Pref> command.
;;
;; XML Declaration: (CTType, VerCT)
(defun syncml-create-devinf-txpref-command (ownerdoc cttypenode verctnode) 
  "*Returns a DOM node corresponding to a SyncML <Tx-Pref> command."
  (let* ((txprefnode (dom-document-create-element ownerdoc "Tx-Pref")))
    (dom-node-append-child txprefnode cttypenode)
    (dom-node-append-child txprefnode verctnode)
    txprefnode))


;; syncml-create-devinf-cttype-command ()
;;
;; Returns a DOM node corresponding to a SyncML <CTType> command.
;;
;; XML Declaration: (#PCDATA)
(defun syncml-create-devinf-cttype-command (ownerdoc cttypedata) 
  "*Returns a DOM node corresponding to a SyncML <CTType> command."
  (let* ((cttypenode (dom-document-create-element ownerdoc "CTType"))
	 (textnode (dom-document-create-text-node ownerdoc cttypedata)))
    (dom-node-append-child cttypenode textnode)
    cttypenode))

;; syncml-create-devinf-maxguidsize-command ()
;;
;; Returns a DOM node corresponding to a SyncML <MaxGUIDSize> command.
;;
;; XML Declaration: (#PCDATA)
(defun syncml-create-devinf-maxguidsize-command (ownerdoc maxguidsizedata) 
  "*Returns a DOM node corresponding to a SyncML <MaxGUIDSize> command."
  (let* ((maxguidsizenode (dom-document-create-element ownerdoc "MaxGUIDSize"))
	 (textnode (dom-document-create-text-node ownerdoc maxguidsizedata)))
    (dom-node-append-child maxguidsizenode textnode)
    maxguidsizenode))



;; syncml-create-devinf-verct-command ()
;;
;; Returns a DOM node corresponding to a SyncML <VerCT> command.
;;
;; XML Declaration: (#PCDATA)
(defun syncml-create-devinf-verct-command (ownerdoc verctdata) 
  "*Returns a DOM node corresponding to a SyncML <VerCT> command."
  (let* ((verctnode (dom-document-create-element ownerdoc "VerCT"))
	 (textnode (dom-document-create-text-node ownerdoc verctdata)))
    (dom-node-append-child verctnode textnode)
    verctnode))


;; syncml-create-devinf-synccap-command ()
;;
;; Returns a DOM node corresponding to a SyncML <SyncCap> command.
;;
;; XML Declaration: (#PCDATA)
(defun syncml-create-devinf-synccap-command (ownerdoc synctypenodes) 
  "*Returns a DOM node corresponding to a SyncML <SyncCap> command."
  (let* ((synccapnode (dom-document-create-element ownerdoc "SyncCap")))
    (cond ((listp synctypenodes)
	   (dolist (synctypenode synctypenodes)
	     (if (dom-node-p synctypenode)
		 (dom-node-append-child synccapnode synctypenode))
	     (syncml-debug 1 'syncml-create-devinf-synccap-command "ERROR: No dom-element")))
	  ((dom-node-p synctypenodes)
	   (dom-node-append-child synctypenodes)))
    synccapnode))

;; syncml-create-devinf-synctype-command ()
;;
;; Returns a DOM node corresponding to a SyncML <SyncType> command.
;;
;; XML Declaration: (#PCDATA)
(defun syncml-create-devinf-synctype-command (ownerdoc synctypedata) 
  "*Returns a DOM node corresponding to a SyncML <SyncType> command."
  (let* ((synctypenode (dom-document-create-element ownerdoc "SyncType"))
	 (textnode (dom-document-create-text-node ownerdoc synctypedata)))
    (dom-node-append-child synctypenode textnode)
    synctypenode))

;; syncml-create-devinf-sourceref-command ()
;;
;; Returns a DOM node corresponding to a SyncML <Sourceref> command.
;;
;; XML Declaration: (#PCDATA)
(defun syncml-create-devinf-sourceref-command (ownerdoc sourcerefdata) 
  "*Returns a DOM node corresponding to a SyncML <SourceRef> command."
  (let* ((sourcerefnode (dom-document-create-element ownerdoc "SourceRef"))
	 (textnode (dom-document-create-text-node ownerdoc sourcerefdata)))
    (dom-node-append-child sourcerefnode textnode)
    sourcerefnode))

;; syncml-create-devinf-devid-command ()
;;
;; Returns a DOM node corresponding to a SyncML <DevID> command.
;;
;; XML Declaration: (#PCDATA)
(defun syncml-create-devinf-devid-command (ownerdoc deviddata) 
  "*Returns a DOM node corresponding to a SyncML <DevID> command."
  (let* ((devidnode (dom-document-create-element ownerdoc "DevID"))
	 (textnode (dom-document-create-text-node ownerdoc deviddata)))
    (dom-node-append-child devidnode textnode)
    devidnode))    

;; syncml-create-devinf-devtyp-command ()
;;
;; Returns a DOM node corresponding to a SyncML <DevTyp> command.
;;
;; XML Declaration: (#PCDATA)
(defun syncml-create-devinf-devtyp-command (ownerdoc) 
  "*Returns a DOM node corresponding to a SyncML <DevTyp> command."
  (let* ((devtypnode (dom-document-create-element ownerdoc "DevTyp"))
	 (textnode (dom-document-create-text-node ownerdoc "workstation")))
    (dom-node-append-child devtypnode textnode)
    devtypnode))    

;; syncml-create-devinf-man-command ()
;;
;; Returns a DOM node corresponding to a SyncML <Man> command.
;;
;; XML Declaration: (#PCDATA)
(defun syncml-create-devinf-man-command (ownerdoc) 
  "*Returns a DOM node corresponding to a SyncML <Man> command."
  (let* ((mannode (dom-document-create-element ownerdoc "Man"))
	 (textnode (dom-document-create-text-node ownerdoc "syncml.el")))
    (dom-node-append-child mannode textnode)
    mannode))    

;; syncml-create-devinf-mod-command ()
;;
;; Returns a DOM node corresponding to a SyncML <Mod> command.
;;
;; XML Declaration: (#PCDATA)
(defun syncml-create-devinf-mod-command (ownerdoc) 
  "*Returns a DOM node corresponding to a SyncML <Mod> command."
  (let* ((modnode (dom-document-create-element ownerdoc "Mod"))
	 (textnode (dom-document-create-text-node ownerdoc "bbdb-syncml")))
    (dom-node-append-child modnode textnode)
    modnode))    



;; syncml-create-devinf-verdtd-command ()
;;
;; Returns a DOM node corresponding to a SyncML <VerDTD> command.
;;
;; XML Declaration: (#PCDATA)
(defun syncml-create-devinf-verdtd-command (ownerdoc) 
  "*Returns a DOM node corresponding to a SyncML <VerDTD> command."
  (let* ((verdtdnode (dom-document-create-element ownerdoc "VerDTD"))
	 (textnode (dom-document-create-text-node ownerdoc "1.1")))
    (dom-node-append-child verdtdnode textnode)
    verdtdnode))




(provide 'syncml-commands)
