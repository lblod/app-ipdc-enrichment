;;;;;;;;;;;;;;;;;;;
;;; delta messenger
(in-package :delta-messenger)

;; (push (make-instance 'delta-logging-handler) *delta-handlers*) ;; enable if delta messages should be logged on terminal
(add-delta-messenger "http://delta-notifier/")
(setf *log-delta-messenger-message-bus-processing* nil) ;; set to t for extra messages for debugging delta messenger

;;;;;;;;;;;;;;;;;
;;; configuration
(in-package :client)
(setf *log-sparql-query-roundtrip* nil) ; change nil to t for logging requests to virtuoso (and the response)
(setf *backend* "http://triplestore:8890/sparql")

(in-package :server)
(setf *log-incoming-requests-p* nil) ; change nil to t for logging all incoming requests

;;;;;;;;;;;;;;;;
;;; prefix types
(in-package :type-cache)

(add-type-for-prefix "http://mu.semte.ch/sessions/" "http://mu.semte.ch/vocabularies/session/Session") ; each session URI will be handled for updates as if it had this mussession:Session type

;;;;;;;;;;;;;;;;;
;;; access rights

(in-package :acl)

;; these three reset the configuration, they are likely not necessary
(defparameter *access-specifications* nil)
(defparameter *graphs* nil)
(defparameter *rights* nil)

;; Prefixes used in the constraints below (not in the SPARQL queries)
(define-prefixes
  ;; Core
  :mu "http://mu.semte.ch/vocabularies/core/"
  :session "http://mu.semte.ch/vocabularies/session/"
  :ext "http://mu.semte.ch/vocabularies/ext/"
  :ipdc "https://productencatalogus.data.vlaanderen.be/ns/ipdc-lpdc#"
  :schema "http://schema.org/"
  :foaf "http://xmlns.com/foaf/0.1/"
  :adms "http://www.w3.org/ns/adms#"
  :m8g "http://data.europa.eu/m8g/"
  :locn "http://www.w3.org/ns/locn#"
  :cpsv "http://purl.org/vocab/cpsv#"
  :eli "http://data.europa.eu/eli/ontology#"
  :icr "http://lblod.data.gift/vocabularies/informationclassification/"
  ;; Custom prefix URIs here, prefix casing is ignored
  )


(define-graph public ("http://mu.semte.ch/graphs/public")
  (_ -> _)) ; public allows ANY TYPE -> ANY PREDICATE in the direction
            ; of the arrow

(define-graph org ("http://mu.semte.ch/graphs/organizations/")
  ("foaf:Person" -> _)
  ("foaf:OnlineAccount" -> _)
  ("adms:Identifier" -> _))

(define-graph ipdc ("http://mu.semte.ch/graphs/ipdc/ldes-data")
  ("ipdc:InstancePublicServiceSnapshot" -> _)
  ("ipdc:FinancialAdvantage" -> _)
  ("schema:WebSite" -> _)
  ("m8g:Requirement" -> _)
  ("m8g:Cost" -> _)
  ("m8g:Evidence" -> _)
  ("schema:ContactPoint" -> _)
  ("locn:Address" -> _)
  ("cpsv:Rule" -> _)
  ("eli:LegalResource" -> _))

;;;;;;;;;;;;;
;; User roles

(supply-allowed-group "public")

(supply-allowed-group "organization"
  :parameters ("session_group")
  :query "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
          PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
          SELECT DISTINCT ?session_group WHERE {
            <SESSION_ID> ext:sessionGroup/mu:uuid ?session_group
            }")

(supply-allowed-group "logged-in"
  :query "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
          PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
          SELECT DISTINCT ?session_group WHERE {
            <SESSION_ID> ext:sessionGroup/mu:uuid ?session_group
            }")

(grant (read)
       :to-graph public
       :for-allowed-group "public")

(grant (read)
       :to-graph org
       :for-allowed-group "organization")

(grant (read write)
       :to-graph ipdc
       :for-allowed-group "logged-in")