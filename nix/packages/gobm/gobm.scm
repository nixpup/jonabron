#!/usr/bin/env guile
!#
(use-modules (ice-9 rdelim)
             (ice-9 regex)
             (json)
             (ice-9 format)
             (ice-9 popen)
             (srfi srfi-1)
             (ice-9 ports))

(define args (cdr (command-line)))
(if (null? args)
    (begin (display "Usage: gobm <osu beatmap URL>\n") (exit 1)))

(define url (car args))
(define url-base (car (string-split url #\#)))

;; --- The main logic wrapped in a catch block ---
(catch #t
  (lambda ()
    ;; 1. Extract ID
    (define m (string-match "beatmapsets/([0-9]+)" url-base))
    (if (not m) (throw 'invalid-url))
    (define beatmapset-id (match:substring m 1))

    ;; 2. Fetch metadata
    (let* ((url-to-fetch (string-append "https://osu.direct/api/get_beatmaps?s=" beatmapset-id))
           (output (open-pipe* OPEN_READ "curl" "-s" "-S" url-to-fetch))
           (raw (read-string output)))
      (close-pipe output)
      (if (or (string-null? raw) (string=? raw "[]"))
          (throw 'not-found))

      (define meta-json (json-string->scm raw))
      ;; Check if vector is empty before trying to access index 0
      (if (or (not (vector? meta-json))
              (= (vector-length meta-json) 0))
          (throw 'not-found))
      ;; Check if API returned an error object instead of data
      (if (and (vector? meta-json) (> (vector-length meta-json) 0)
               (assoc-ref (vector-ref meta-json 0) "message"))
          (throw 'api-error))

      (define first-map (vector-ref meta-json 0))
      (define artist (or (assoc-ref first-map "artist") "Unknown Artist"))
      (define title (or (assoc-ref first-map "title") "Unknown Title"))

      ;; 3. Sanitize and Download
      (define pattern "[\\/\\:\\*\\?<>|]+")
      (define beatmap-name (regexp-substitute/global #f pattern (string-append artist " - " title) 'pre "" 'post))
      (define filename (string-append beatmapset-id " " beatmap-name ".osz"))

      (display (string-append "Downloading " filename " ...\n"))
      (let ((cmd (list "curl" "-L" "-o" filename (string-append "https://osu.direct/d/" beatmapset-id))))
        (if (not (zero? (apply system* cmd)))
            (throw 'download-failed))))
    (display "Download successful.\n"))

  ;; --- The error handler ---
  (lambda (key . args)
    (cond
      ((eq? key 'invalid-url) (display "Error: Could not find beatmapset ID in URL.\n"))
      ((eq? key 'not-found)   (display "Error: Beatmap not found.\n"))
      ((eq? key 'api-error)   (display "Error: API returned an internal error.\n"))
      ((eq? key 'download-failed) (display "Error: Download process failed.\n"))
      (else (display "An unexpected error occurred.\n")))
    (exit 1)))
