(in-package :gandi)

(defvar *api-key* "***_API_KEY_GOES_HERE_***")

(defvar *gandi-url* "https://dns.api.gandi.net/api/v5/domains/")

(defvar *domain-name* "example.com")
(defvar *domain-ttl* 1200)

(defvar *record-names* '("@"))

(defvar *log-format* '((:year 4) #\- (:month 2) #\- (:day 2)
                                 #\SPACE
                       (:hour 2) #\: (:min 2) #\: (:sec 2)))

(defvar *ip-detect-service* "https://bot.whatismyipaddress.com")

;; should be loaded via asdf
(defvar *public-stun-servers*)
