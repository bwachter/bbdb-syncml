;; This file just contains a list of SyncML constants
;; Please see the SyncML Sync Protocol.
;; $Id: syncml-constants.el,v 1.2 2004/01/17 16:30:16 joergenb Exp $

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
(defconst syncml-alert-two-way "200"
  "SyncML <ALERT> Code: Specifies a client-initiated, two-way sync.")
(defconst syncml-alert-slow-sync "201"
  "SyncML <ALERT> Code: Specifies a client-initiated, two-way slow-sync.")
(defconst syncml-alert-one-way-from-client "202"
  "SyncML <ALERT> Code: Specifies the client-initiated, one-way only sync from the client to the server.")
(defconst syncml-alert-refresh-from-client "203"
  "SyncML <ALERT> Code: Specifies the client-initiated, refresh operation for the oneway only sync from the client to the server.")
(defconst syncml-alert-one-way-from-server "204"
  "SyncML <ALERT> Code: Specifies the client-initiated, one-way only sync from the server to the client.")
(defconst syncml-alert-refresh-from-server "205"
  "SyncML <ALERT> Code: Specifies the client-initiated, refresh operation of the oneway only sync from the server to the client.")

;; Alert codes used by the server when alerting the sync.
(defconst syncml-alert-two-way-by-server "206"
  "SyncML <ALERT> Code: Specifies a server-initiated, two-way sync.")
(defconst syncml-alert-one-way-from-client-by-server "207"
  "SyncML <ALERT> Code: Specifies the server-initiated, one-way only sync from the client to the server.")
(defconst syncml-alert-refresh-from-client-by-server "208"
  "SyncML <ALERT> Code: Specifies the server-initiated, refresh operation for the one-way only sync from the client to the server.")
(defconst syncml-alert-one-way-from-server-by-server "209"
  "SyncML <ALERT> Code: Specifies the server-initiated, one-way only sync from the server to the client.")
(defconst syncml-alert-refresh-from-server-by-server "210"
  "SyncML <ALERT> Code: Specifies the server-initiated, refresh operation of the oneway only sync from the server to the client.")

;; Special alert codes:
(defconst syncml-alert-result-alert "221"
  "SyncML <ALERT> Code: Specifies a request for sync results.")
(defconst syncml-alert-next-message "222"
  "SyncML <ALERT> Code: Specifies a request for the next message in the package.")
(defconst syncml-alert-no-end-of-data "223"
  "SyncML <ALERT> Code: End of Data for chunked object not received.")


(defconst syncml-alert-codes
  '(
    ;;Alert Codes used for user alerts ")
    ("100" "DISPLAY Show. The Data element type contains content information that should be processed and displayed through the user agent.")
    ;;101-150 - Reserved for future SyncML usage.
    ;; Alert Codes used at the synchronization initialization
    ("200" "TWO-WAY Specifies a client-initiated, two-way sync. ")
    ("201" "SLOW SYNC Specifies a client-initiated, two-way slow-sync. ")
    ("202" "ONE-WAY FROM CLIENT Specifies the client-initiated, one-way only sync from the client to the server. ")
    ("203" "REFRESH FROM CLIENT Specifies the client-initiated, refresh operation for the oneway only sync from the client to the server. ")
    ("204" "ONE-WAY FROM SERVER Specifies the client-initiated, one-way only sync from the server to the client. ")
    ("205" "REFRESH FROM SERVER Specifies the client-initiated, refresh operation of the oneway only sync from the server to the client. Alert Codes used by the server when alerting the sync. ")
    ("206" "TWO-WAY BY SERVER Specifies a server-initiated, two-way sync. ")
    ("207" "ONE-WAY FROM CLIENT BY SERVER Specifies the server-initiated, one-way only sync from the client to the server. ")
    ("208" "REFRESH FROM CLIENT BY SERVER Specifies the server-initiated, refresh operation for the one-way only sync from the client to the server.
")
    ("209" "ONE-WAY FROM SERVER BY SERVER Specifies the server-initiated, one-way only sync from the server to the client. ")
    ("210" "REFRESH FROM SERVER BY SERVER Specifies the server-initiated, refresh operation of the oneway only sync from the server to the client.")
    ;; 211-220 - Reserved for future SyncML usage.
    ;; Special Alert Codes
    ("221" "RESULT ALERT Specifies a request for sync results. ")
    ("222" "NEXT MESSAGE Specifies a request for the next message in the package. ")
    ("223" "NO END OF DATA End of Data for chunked object not received.")
    ;; 224-250 - Reserved for future SyncML usage.
    ))




;;;;;;;;;;;;;;;;;
;; REPONSE CODES
;;;;;;;;;;;;;;;;;

(defconst syncml-response-codes
  ;; successfull 2xx
  '(("200" "OK. The SyncML command completed successfully")
    ("201" "Item Added. The requested item was added")
    ("202" "Accepted for processing. The request to either run a remote execution of an application or to alert a user or application was successfully performed. ")
    ("203" "Non-authoritative response. The request is being responded to by an entity other than the one targeted. The response is only to be returned when the request would have been resulted in a 200 response code from the authoritative target. ")
    ("204" "No content. The request was successfully completed but no data is being returned. The response code is also returned in response to a Get when the target has no content. ")
    ("205" "Reset content. The source should update their content. The originator of the request is being told that their content should be synchronized to get an up to date version. ")
    ("206" "Partial content. The response indicates that only part of the command was completed. If the remainder of the command can be completed later, then when completed another appropriate completion request status code SHOULD be created. ")
    ("207" "Conflict resolved with merge. The response indicates that the request created a conflict; which was resolved with a merge of the client and server instances of the data. The response includes both the Target and Source URLs in the Item of the Status. In addition, a Replace command is returned with the merged data. ")
    ("208" "Conflict resolved with client s command 'winning'. The response indicates that there was an update conflict; which was resolved by the client command winning. ")
    ("209" "Conflict resolved with duplicate. The response indicates that the request created an update conflict; which was resolved with a duplication of the client s data being created in the server database. The response includes both the target URI of the duplicate in the Item of the Status. In addition, in the case of a two-way synchronization, an Add command is returned with the duplicate data definition. ")
    ("210" "Delete without archive. The response indicates that the requested data was successfully deleted, but that it was not archived prior to deletion because this optional feature was not supported by the implementation. ")
    ("211" "Item not deleted. The requested item was not found. It may have been previously deleted. ")
    ("212" "Authentication accepted. No further authentication is needed for the remainder of the synchronization session. This response code can only be used in response to a request in which the credentials were provided. ")
    ("213" "Chunked item accepted and buffered. ")
    ("214" "Operation cancelled. The SyncML command completed successfully, but no more commands will be processed within the session. ")
    ("215" "Not executed. A command was not executed, as a result of user interaction and user chose not to accept the choice. ")
    ("216" "Atomic roll back OK. A command was inside Atomic element and Atomic failed. This command was rolled back successfully.")

    ;;  Redirection 3xx
    ("300" "Multiple choices. The requested target is one of a number of multiple alternatives requested target. The alternative SHOULD also be returned in the Item element type in the Status. ")
    ("301" "Moved permanently. The requested target has a new URI. The new URI SHOULD also be returned in the Item element type in the Status. ")
    ("302" "Found. The requested target has temporarily moved to a different URI. The original URI SHOULD continue to be used. The URI of the temporary location SHOULD also be returned in the Item element type in the Status. The requestor SHOULD confirm the identity and authority of the temporary URI to act on behalf of the original target URI. ")
    ("303" "See other. The requested target can be found at another URI. The other URI SHOULD be returned in the Item element type in the Status. ")
    ("304" "Not modified. The requested SyncML command was not executed on the target. This is an additional response that can be added to any of the other Redirection response codes. ")
    ("305" "Use proxy. The requested target MUST be accessed through the specified proxy URI. The proxy URI SHOULD also be returned in the Item element type in the Status.")

    ;; Originator Exceptions 4xx
    ("400" "Bad request. The requested command could not be performed because of malformed syntax in the command. The malformed command MAY also be returned in the Item element type in the Status. ")
    ("401" "Invalid credentials. The requested command failed because the requestor MUST provide proper authentication. If the property type of authentication was presented in the original request, then the response code indicates that the requested command has been refused for those credentials. ")
    ("402" "Payment required. The requested command failed because proper payment is required. This version of SyncML does not standardize the payment mechanism. ")
    ("403" "Forbidden. The requested command failed, but the recipient understood the requested command. Authentication will not help and the request SHOULD NOT be repeated. If the recipient wishes to make public why the request was denied, then a description MAY be specified in the Item element type in the Status. If the recipient does not wish to make public why the request was denied then the response code 404 MAY be used instead.")
    ("404" "Not found. The requested target was not found. No indication is given as to whether this is a temporary or permanent condition. The response code 410 SHOULD be used when the condition is permanent and the recipient wishes to make this fact public. This response code is also used when the recipient does not want to make public the reason for why a requested command is not allowed or when no other response code is appropriate. ")
    ("405" "Command not allowed. The requested command is not allowed on the target. The recipient SHOULD return the allowed command for the target in the Item element type in the Status. ")
    ("406" "Optional feature not supported. The requested command failed because an optional feature in the request was not supported. The unsupported feature SHOULD be specified by the Item element type in the Status. ")
    ("407" "Missing credentials. This response code is similar to 401 except that the response code indicates that the originator MUST first authenticate with the recipient. The recipient SHOULD also return the suitable challenge in the Chal element type in the Status. ")
    ("408" "Request timeout. An expected message was not received within the required period of time. The request can be repeated at another time. The RespURI can be used to specify the URI and optionally the date/time after which the originator can repeat the request. See RespURI for details. ")
    ("409" "Conflict. The requested failed because of an update conflict between the client and server versions of the data. Setting of the conflict resolution policy is outside the scope of this version of SyncML. However, identification of conflict resolution performed, if any, is within the scope. ")
    ("410" "Gone. The requested target is no longer on the recipient and no forwarding URI is known. ")
    ("411" "Size required. The requested command MUST be accompanied by byte size or length information in the Meta element type. ")
    ("412" "Incomplete command. The requested command failed on the recipient because it was incomplete or incorrectly formed. The recipient SHOULD specify the portion of the command that was incomplete or incorrect in the Item element type in the Status. ")
    ("413" "Request entity too large. The recipient is refusing to perform the requested command because the requested item is larger than the recipient is able or willing to process. If the condition is temporary, the recipient SHOULD also include a Status with the response code 418 and specify a RespURI with the response URI and optionally the date/time that the command SHOULD be repeated. ")
    ("414" "URI too long. The requested command failed because the target URI is too long for what the recipient is able or willing to process. This response code is seldom encountered, but is used when a recipient perceives that an intruder may be attempting to exploit security holes or other defects in order to threaten the recipient. ")
    ("415" "Unsupported media type or format. The unsupported content type or format SHOULD also be identified in the Item element type in the Status. ")
    ("416" "Requested size too big. The request failed because the specified byte size in the request was too big.")
    ("417" "Retry later. The request failed at this time and the originator should retry the request later. The recipient SHOULD specify a RespURI with the response URI and the date/time that the command SHOULD be repeated. ")
    ("418" "Already exists. The requested Put or Add command failed because the target already exists. ")
    ("419" "Conflict resolved with server data. The response indicates that the client request created a conflict; which was resolved by the server command winning. The normal information in the Status should be sufficient for the client to 'undo' the resolution, if it is desired. ")
    ("420" "Device full. The response indicates that the recipient has no more storage space for the remaining synchronization data. The response includes the remaining number of data that could not be returned to the originator in the Item of the Status. ")
    ("421" "Unknown search grammar. The requested command failed on the server because the specified search grammar was not known. The client SHOULD re-specify the search using a known search grammar. ")
    ("422" "Bad CGI Script. The requested command failed on the server because the CGI scripting in the LocURI was incorrectly formed. The client SHOULD re-specify the portion of the command that was incorrect in the Item element type in the Status. ")
    ("423" "Soft-delete conflict. The requested command failed because the 'Soft Deleted' item was previously 'Hard Deleted' on the server. ")
    ("424" "Size mismatch. The chunked object was received, but the size of the received object did not match the size declared within the first chunk. ")
    ("425" "Permission Denied. The requested command failed because the sender does not have adequate access control permissions (ACL) on the recipient. ")

    ;; Recipient Exception 5xx
    ("500" "Command failed. The recipient encountered an unexpected condition which prevented it from fulfilling the request ")
    ("501" "Command not implemented. The recipient does not support the command required to fulfill the request. This is the appropriate response when the recipient does not recognize the requested command and is not capable of supporting it for any resource. ")
    ("502" "Bad gateway. The recipient, while acting as a gateway or proxy, received an invalid response from the upstream recipient it accessed in attempting to fulfill the request. ")
    ("503" "Service unavailable. The recipient is currently unable to handle the request due to a temporary overloading or maintenance of the recipient. The implication is that this is a temporary condition; which will be alleviated after some delay. The recipient SHOULD specify the URI and date/time after which the originator should retry in the RespURI in the response. ")
    ("504" "Gateway timeout. The recipient, while acting as a gateway or proxy, did not receive a timely response from the upstream recipient specified by the URI (e.g. HTTP, FTP, LDAP) or some other auxiliary recipient (e.g. DNS) it needed to access in attempting to complete the request. ")
    ("505" "DTD Version not supported. The recipient does not support or refuses to support the specified version of SyncML DTD used in the request SyncML Message. The recipient MUST include the versions it does support in the Item element type in the Status. ")
    ("506" "Processing error. An application error occurred while processing the request. The originator should retry the request. The RespURI can contain the URI and date/time after which the originator can retry the request. ")
    ("507" "Atomic failed. The error caused all SyncML commands within an Atomic element type to fail. ")
    ("508" "Refresh required. An error occurred that necessitates a refresh of the current synchronization state of the client with the server. Client is requested to initiate a slow sync with the server. ")
    ("509" "Reserved for future use. ")
    ("510" "Data store failure. An error occurred while processing the request. The error is related to a failure in the recipient data store. ")
    ("511" "Server failure. A severe error occurred in the server while processing the request. The originator SHOULD NOT retry the request. ")
    ("512" "Synchronization failed. An application error occurred during the synchronization session. The originator should restart the synchronization session from the beginning. ")
    ("513" "Protocol Version not supported. The recipient does not support or refuses to support the specified version of the SyncML Synchronization Protocol used in the request SyncML Message. The recipient MUST include the versions it does support in the Item element type in the Status.")
    ("514" "Operation cancelled. The SyncML command was not completed successfully, since the operation was already cancelled before processing the command. The originator should repeat the command in the next session. ")
    ("516" "Atomic roll back failed. Command was inside Atomic element and Atomic failed. This command was not rolled back successfully. Server should take action to try to recover client back into original state. ")
    ("517" "Atomic response too large to fit. The response to an atomic command was too large to fit in a single message.")))

(provide 'syncml-constants)
