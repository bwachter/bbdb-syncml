;;; syncml-commands.el -- An elisp implementation of a SyncML client. This file contains the syncml commands
;; $Id: syncml-commands.el,v 1.5 2004/01/25 11:57:14 joergenb Exp $

;; Copyright (C) 2003 J�rgen Binningsb� 

;; Author: J�rgen Binningsb� <jb at pvv.org>
;; Maintainer: J�rgen Binningsb� <jb at pvv.org>
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
  (let* ((syncmldoc (make-dom-document :name "MySyncMLDocument"
				       :type dom-document-node))
	 (syncmlelement (dom-document-create-element syncmldoc "SyncML")))
    (setf (dom-document-owner-document syncmldoc) syncmldoc
	  (dom-document-element syncmldoc) syncmlelement)
    syncmldoc))


;; syncml-create-synchdr-command
(defun syncml-create-synchdr-command (ownerdoc targetnode sourcenode &optional crednode metanode)
  "Returns a new <SyncHdr> DOM node. MsgID is incremented."
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
  (syncml-debug 3 'syncml-create-data-command "Triggered.")
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
  (syncml-debug 3 'syncml-create-target-command "Triggered.")
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
  (syncml-debug 3 'syncml-create-source-command "Triggered.")
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


;; syncml-create-cred-command ()
(defun syncml-create-cred-command (ownerdoc)
  "* Returns a DOM node equivalent of the SyncML <Cred> command.

Parent Elements: Add, Alert, Copy, Delete, Exec, Get, Put, Map, Replace, Search, Status, Sync, SyncHdr
XML declaration: (Meta?, Data)"
  (let* ((crednode (dom-document-create-element ownerdoc "Cred")))
    (dom-node-append-child crednode (syncml-create-meta-command 
				     ownerdoc
				     (syncml-create-metinf-type-command ownerdoc "syncml:basic-auth")))
    (dom-node-append-child crednode (syncml-create-data-command ownerdoc 
								(concat syncml-user ":" syncml-passwd)))
    crednode))


;; syncml-create-sync-command ()
;; 
;; Returns a DOM noe corresponding to the SyncML <Sync> command.
;; XML definition: CmdID, NoResp?, Cred?, Target?, Source?, Meta?, NumberOfChanges?, (Add | Atomic | Copy | Delete | Replace | Sequence)*
(defun syncml-create-sync-command (ownerdoc &optional noresp crednode targetnode  sourcenode metanode )
  "Returns a string with the <Sync> command 

XML definition: CmdID, NoResp?, Cred?, Target?, Source?, Meta?, NumberOfChanges?, (Add | Atomic | Copy | Delete | Replace | Sequence)*"

  (let* ((syncnode (dom-document-create-element ownerdoc "Sync"))) 
    (dom-node-append-child syncnode (syncml-create-cmdid-command ownerdoc)) 
    (if (not (null noresp))
	(dom-node-append-child syncnode (dom-document-create-element ownerdoc "NoResp")))
    (if (not (null crednode))
	(dom-node-append-child syncnode crednode))
    (if (not (null targetnode))
	(dom-node-append-child syncnode targetnode))
    (if (not (null sourcenode))
	(dom-node-append-child syncnode sourceode))
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
    


;; syncml-create-put-command ()
;; 
;; Returns a DOM noe corresponding to the SyncML <Put> command.
;; XML definition: (CmdID, NoResp?, Lang?, Cred?, Meta?, Item+)
(defun syncml-create-put-command (ownerdoc itemnode &optional metanode crednode noresp)
  "Returns a string with the <Put> command 
XML definition: (CmdID, NoResp?, Lang?, Cred?, Meta?, Item+) "
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
    


;; syncml-create-status-command ()
;; 
;; Returns a DOM noe corresponding to the SyncML <Status> command.
;; XML definition: CmdID, NoResp?, Cred?, Meta?, Item+)
(defun syncml-create-status-command (ownerdoc msgref cmdref cmd datanode &optional targetrefnode sourcerefnode itemnode)
  "Returns a string with the <Status> command 
XML definition: 
CmdID, MsgRef, CmdRef, Cmd, TargetRef*, SourceRef*, Cred?, Chal?, Data, Item*)"
  (syncml-debug 3 'syncml-create-status-command "Triggered")
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
  (let* ((alertnode (dom-document-create-element ownerdoc "Alert"))
	 (cmdidnode (syncml-create-cmdid-command ownerdoc)))
    (dom-node-append-child alertnode cmdidnode)
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
  (syncml-increase-cmdid)
  (let* ((cmdidnode (dom-document-create-element ownerdoc "CmdID")))
    (dom-node-append-child cmdidnode (dom-document-create-text-node ownerdoc syncml-current-cmdid))
    cmdidnode))

;; syncml-create-sessionid-command
;;
(defun syncml-create-sessionid-command (ownerdoc)
  "Returns a <SessionID> node.  Incrementing the sessionid is not done, should be set before sync initialization. (package #1)"
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
  (syncml-debug 3 'syncml-create-cmd-command "Triggered.")
  (let* ((cmdnode (dom-document-create-element ownerdoc "Cmd")))
    (dom-node-append-child cmdnode (dom-document-create-text-node ownerdoc cmd))
    cmdnode))

;; syncml-create-final-command
;;
(defun syncml-create-final-command (ownerdoc)
  "Returns a <Final> node. "
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
  (let* ((verdtdnode (dom-document-create-element ownerdoc "VerDTD")))
    (dom-node-append-child verdtdnode (dom-document-create-text-node ownerdoc "1.1"))
    verdtdnode))


;; syncml-create-verproto-command
;;
(defun syncml-create-verproto-command (ownerdoc)
  "Returns a <VerProto> node.  We only support 1.1"
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
  (let* ((typenode (dom-document-create-element ownerdoc "Type"))
	 (typeattr (dom-document-create-attribute ownerdoc "xmlns"))
	 (textnode (dom-document-create-text-node ownerdoc typedata)))
    (setf (dom-attr-value typeattr) "syncml:metinf"
	  (dom-node-attributes typenode) (list typeattr))
    (dom-node-append-child typenode textnode)
    typenode))


;; syncml-create-metinf-anchor-command ()
;;
;; Returns a DOM node corresponding to a SyncML <Anchor> command.
;; XML Declaration: (Last?, Next)
;; Parent Elements: Alert, Cred, Item, Status, Search
(defun syncml-create-metinf-anchor-command (ownerdoc) 
  "*Returns a DOM node corresponding to a SyncML <Anchor> command.

ANCHORDATA is a string containing the #PCDATA content.
Example: <Anchor>212<Anchor>   Indicates a successful authentication."
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
    (dom-node-append-child devinfnode datastorenode)
    devinfnode))


;; syncml-create-devinf-datastore-command ()
;;
;; Returns a DOM node corresponding to a SyncML <DataStore> command.
;;
;; XML Declaration: (SourceRef, DisplayName?, MaxGUIDSize?, Rx-Pref, Rx*, Tx-Pref, Tx*, DSMem?, SyncCap)
(defun syncml-create-devinf-datastore-command (ownerdoc sourcerefnode rxprefnode txprefnode synccapnode) 
  "*Returns a DOM node corresponding to a SyncML <DataStore> command."
  (let* ((datastorenode (dom-document-create-element ownerdoc "DataStore")))
    (dom-node-append-child datastorenode sourcerefnode)
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
  (let* ((sourcerefnode (dom-document-create-element ownerdoc "Sourceref"))
	 (textnode (dom-document-create-text-node ownerdoc sourcerefdata)))
    (dom-node-append-child sourcerefnode textnode)
    sourcerefnode))



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
    verctnode))




(provide 'syncml-commands)
