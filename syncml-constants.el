;;; This file just contains a list of SyncML constants
;;; Please see the SyncML Sync Protocol.
;; $Id: syncml-constants.el,v 1.1 2003/02/06 14:14:22 joergenb Exp $

;; Copyright (C) 2003 Jørgen Binningsbø 

;; Author: Jørgen Binningsbø <jb@pvv.org>
;; Maintainer: Jørgen Binningsbø <jb@pvv.org>
;; Version: 
;; Created: Jan 10 2003
;; Keywords: syncml xml network
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



;; <ALERT> Codes

;; Alert codes for user alert:
(defconst syncml-alert-display 100
	"SyncML <ALERT> Code: Show. The Data element type contains content information that should be processed and displayed through the user agent.")

;; Alert codes used at the synchronization initalization:
(defconst syncml-alert-two-way 200
	"SyncML <ALERT> Code: Specifies a client-initiated, two-way sync.")
(defconst syncml-alert-slow-sync 201
	"SyncML <ALERT> Code: Specifies a client-initiated, two-way slow-sync.")
(defconst syncml-alert-one-way-from-client 202
	"SyncML <ALERT> Code: Specifies the client-initiated, one-way only sync from the client to the server.")
(defconst syncml-alert-refresh-from-client 203
	"SyncML <ALERT> Code: Specifies the client-initiated, refresh operation for the oneway only sync from the client to the server.")
(defconst syncml-alert-one-way-from-server 204
	"SyncML <ALERT> Code: Specifies the client-initiated, one-way only sync from the server to the client.")
(defconst syncml-alert-refresh-from-server 205
	"SyncML <ALERT> Code: Specifies the client-initiated, refresh operation of the oneway only sync from the server to the client.")

;; Alert codes used by the server when alerting the sync.
(defconst syncml-alert-two-way-by-server 206
	"SyncML <ALERT> Code: Specifies a server-initiated, two-way sync.")
(defconst syncml-alert-one-way-from-client-by-server 207
	"SyncML <ALERT> Code: Specifies the server-initiated, one-way only sync from the client to the server.")
(defconst syncml-alert-refresh-from-client-by-server 208
	"SyncML <ALERT> Code: Specifies the server-initiated, refresh operation for the one-way only sync from the client to the server.")
(defconst syncml-alert-one-way-from-server-by-server 209
	"SyncML <ALERT> Code: Specifies the server-initiated, one-way only sync from the server to the client.")
(defconst syncml-alert-refresh-from-server-by-server 210
	"SyncML <ALERT> Code: Specifies the server-initiated, refresh operation of the oneway only sync from the server to the client.")

;; Special alert codes:
(defconst syncml-alert-result-alert 221
	"SyncML <ALERT> Code: Specifies a request for sync results.")
(defconst syncml-alert-next-message 222
	"SyncML <ALERT> Code: Specifies a request for the next message in the package.")
(defconst syncml-alert-no-end-of-data 223
	"SyncML <ALERT> Code: End of Data for chunked object not received.")

(provide 'syncml-constants)
