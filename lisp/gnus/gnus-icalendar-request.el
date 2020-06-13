(defun gnus-icalendar-event--format-attendee (attendee role)
  (when (member role '("req" "opt"))
    (format "ATTENDEE;PARTSTAT=NEEDS-ACTION;ROLE=%s-PARTICIPANT;RSVP=TRUE:mailto:%s" (upcase role) attendee)))

(defun gnus-icalendar-event--create-attendee-list (req opt)
  (concat
   (mapconcat (lambda (req) (gnus-icalendar-event--format-attendee req "req")) req "\n")
   (when opt
     "\n"
     (mapconcat (lambda (opt) (gnus-icalendar-event--format-attendee opt "opt")) opt "\n"))))

(defun gnus-icalendar-event--ical-from-event (event)
  (with-slots (summary description location organizer recur uid start-time end-time req-participants opt-participants) event
    (let ((dtstamp (format-time-string "DTSTAMP:%Y%m%dT%H%M%SZ" nil t)) ;; current UTC time
          (summary (format "SUMMARY:%s" summary))
          (description (when (and (stringp description) (not (string-empty-p description)))
                         (format "DESCRIPTION:%s"
                                 (with-temp-buffer
                                   (insert description)
                                   (beginning-of-buffer)
                                   (while (re-search-forward "\n" nil t)
                                     (replace-match "\\n" t t))
                                   (buffer-string))))) ;; TODO: How to do this properly?
          (dtstart (format-time-string "DTSTART:%Y%m%dT%H%M%SZ" start-time t)) ;; start-time in UTC
          (dtend (format-time-string "DTEND:%Y%m%dT%H%M%SZ" end-time t)) ;; end-time in UTC
          (attendee (gnus-icalendar-event--create-attendee-list req-participants opt-participants))
          (location (when (and (stringp location) (not (string-empty-p location)))
                      (format "LOCATION:%s" location)))
          (organizer (format "ORGANIZER:mailto:%s" organizer))
          (uid (format "UID:%s" uid))
          (sequence "SEQUENCE:0") ;; TODO: Consider follow-up event modifications.
          ;; TODO: handle recur
          )
      (with-temp-buffer
        (insert
         (mapconcat #'identity
                    (list "BEGIN:VEVENT"
                          dtstamp
                          dtstart
                          dtend
                          summary
                          description
                          attendee
                          location
                          organizer
                          uid
                          sequence
                          "END:VEVENT") "\n"))
        (flush-lines "^$" (point-min) (point-max))
        (buffer-string)))))

;; Vcalendar creation

;; I have not yet found a good way to create vtimezone accurately from
;; scratch. For now hardcoded for CET/CEST and crude general
;; implementation below.
(defvar gnus-icalendar-vtimezone-times
  '(CEST "BEGIN:DAYLIGHT
TZOFFSETFROM:+0100
TZOFFSETTO:+0200
TZNAME:CEST
DTSTART:19700329T020000
RRULE:FREQ=YEARLY;BYDAY=-1SU;BYMONTH=3
END:DAYLIGHT
BEGIN:STANDARD
TZOFFSETFROM:+0200
TZOFFSETTO:+0100
TZNAME:CET
DTSTART:19701025T030000
RRULE:FREQ=YEARLY;BYDAY=-1SU;BYMONTH=10
END:STANDARD"
        CET "BEGIN:DAYLIGHT
TZOFFSETFROM:+0100
TZOFFSETTO:+0200
TZNAME:CEST
DTSTART:19700329T020000
RRULE:FREQ=YEARLY;BYDAY=-1SU;BYMONTH=3
END:DAYLIGHT
BEGIN:STANDARD
TZOFFSETFROM:+0200
TZOFFSETTO:+0100
TZNAME:CET
DTSTART:19701025T030000
RRULE:FREQ=YEARLY;BYDAY=-1SU;BYMONTH=10
END:STANDARD")
  "Timezone information about standard and daylight savings time used in VCALENDAR parts.")

(defun gnus-icalendar--default-vtimezone (&optional zone)
  "Return default VTIMEZONE information for the current time zone or ZONE if provided."
  (let ((time-zone (current-time-zone nil zone)))
    (format "BEGIN:STANDARD
DTSTART:%s
TZOFFSETTO:%s
TZOFFSETFROM:+0000
TZNAME:%s
END:STANDARD"
            (format-time-string "%Y%m%dT%H%M%S" 0) ;; set effective timezone start date to epoch
            (format-time-string "%z" (current-time) time-zone) ;; time zone offset
            (cadr time-zone)
            )))

(defun gnus-icalendar--build-vcalendar-from-vevent (event)
  "Create VCALENDAR part with VEVENT part EVENT."
  (let* ((time-zone (cadr (current-time-zone)))
         (vtimezone (mapconcat #'identity `("BEGIN:VTIMEZONE"
                                            ,(format "TZID:%s" time-zone)
                                            ,(or (plist-get gnus-icalendar-vtimezone-times (intern time-zone))
                                                 (gnus-icalendar--default-vtimezone))
                                            "END:VTIMEZONE") "\n")))
    (mapconcat #'identity `("BEGIN:VCALENDAR"
                            "PRODID:Gnus"
                            "VERSION:2.0"
                            "METHOD:REQUEST"
                            ,vtimezone
                            ,event
                            "END:VCALENDAR") "\n")))

(defun gnus-icalendar-event-message-insert-request (event)
  "Insert text/calendar part into message with request for VEVENT
  specified in EVENT."
  (when (provided-mode-derived-p major-mode 'message-mode)
    (mml-insert-part "text/calendar; method=\"REQUEST\"; charset=UTF-8")
    (insert (gnus-icalendar--build-vcalendar-from-vevent
             (gnus-icalendar-event--ical-from-event event)))))

(defun gnus-icalendar-event-from-message-and-insert (&optional date location)
  "Create a event request based on the current message.

Direct recipients of the message (in To header) are interpreted
as required participants. Recipients in Cc are optional
participants. The From header is always converted to the event
organizer. Message subject is interpreted as summary and message
body (if existant) as description. Time and date of the event can
be provided as org formatted date range (only with time for now)
or will be asked for if nil. Same for location."
  (interactive)
  (if (not message-draft-article) ;; internally set by message-mode
      (message "Not in a message draft")
    (unless (or date (featurep 'org))
      (error "Timestamp creation requires org. Please load org or provide a org-styled date range"))
    (message-check-recipients) ;; check for bogus recipients
    (let* ((date (or date
                     (with-temp-buffer
                       (org-time-stamp nil)
                       (buffer-string))))
           (start-time (org-timestamp-to-time
                        (org-timestamp-from-string date) nil))
           (end-time (org-timestamp-to-time ;; set end-time if input was a time-range
                      (org-timestamp-from-string date) t))
           (end-time (if (equal end-time start-time) ;; ask for end-time if previous input was not a range
                         (org-read-date nil t nil "End time:" start-time)
                       end-time))
           ;; TODO: better differentiate date-time ranges and date (whole-day) ranges
           (uid (if (featurep 'org-id)
                    (org-id-uuid)
                  (format "%s@%s"
                          (number-to-string (abs (random)))
                          (md5 (format "%s%s%s%s"
                                       (emacs-pid)
                                       user-full-name
                                       user-mail-address
                                       (system-name))))))
           (recur nil) ;; TODO
           (location (or location (read-string "Event location: ")))
           (description (when (message-goto-body)
                          (buffer-substring (point) (point-max))))
           (summary (save-restriction
                      (message-narrow-to-headers)
                      (message-fetch-field "Subject")))
           (organizer (caar (mail-header-parse-addresses
                            (save-restriction
                              (message-narrow-to-headers)
                              (message-fetch-field "From"))))) ;; TODO insert common name for "name <mail@address.net>" addresses
           (rsvp nil) ;; TODO
           (participation-type 'non-participant) ;; TODO
           (req-participants (mapcar #'car
                                     (mail-header-parse-addresses
                                      (save-restriction
                                        (message-narrow-to-headers)
                                        (message-fetch-field "To")))))
           (opt-participants (mapcar #'car
                                     (mail-header-parse-addresses
                                      (save-restriction
                                        (message-narrow-to-headers)
                                        (message-fetch-field "Cc")))))
           (event (gnus-icalendar-event-request :uid uid
                                                :recur recur
                                                :location location
                                                :description description
                                                :summary summary
                                                :method "REQUEST"
                                                :organizer organizer
                                                :start-time start-time
                                                :end-time end-time
                                                :rsvp rsvp
                                                :participation-type participation-type
                                                :req-participants req-participants
                                                :opt-participants opt-participants)))
      (message-goto-body)
      (delete-region (point) (point-max))
      (when (not (string-empty-p description))
        (mml-insert-multipart "mixed")
        (mml-insert-part "text/plain")
        (insert description "\n")
        (re-search-forward "<#/part>\n"))
      (gnus-icalendar-event-message-insert-request event))))
