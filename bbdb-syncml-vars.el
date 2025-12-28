;; bbdb-syncml.el -- A SyncML client for the BBDB database. -*- lexical-binding: t; -*-
;; $Id: bbdb-syncml.el,v 1.10 2006/04/06 20:37:05 joergenb Exp $

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
  "*The password of syncml-user.  if encrypted, this should be the MD5 or
whatever representation")

(defvar syncml-use-md5 't
  "*If we shall use the md5 algorithm for the authentication")

(defvar syncml-current-sessionid ""
  "*The session id <SessionID> used by the current or just-completed
SyncML session.")

(defvar syncml-current-msgid 1
  "*The message id <MsgID> used by within the current SyncML session.")

(defvar syncml-current-cmdid 1
  "*The command id <CmdID> for the current command within the current
SyncML message.")

(defvar syncml-current-timestamp nil
  "*The timestamp at the start of the synchronization session. Inserted into the
<Next> tag in the initialization package.")

(defvar syncml-previous-timestamp nil
  "*The timestamp at the previous synchronization.")

(defvar syncml-transmit-buffername "*syncml-transmit*"
  "The name of the buffer containing a syncml-message to transmit to the server")

(defvar syncml-response-buffername "*syncml-response*"
  "The name of the buffer containing the syncml-message returned from the server")

(provide 'bbdb-syncml-vars)

;;; bbdb-syncml-vars.el ends here
