; xyzzygy - a clone of the game Cards Against Humanity
; Copyright (C) 2019  Andy Tockman <andy@tck.mn>
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.

#lang racket

(require crypto
         crypto/argon2
         web-server/http/id-cookie
         web-server/http/cookie-parse
         "db.rkt"
         "util.rkt")

(provide auth-register auth-verify generate-key request->user)

(crypto-factories (list argon2-factory))

; utility functions

(define (userid->cookie userid salt)
  (make-id-cookie "auth" (number->string userid)
                  #:key (bytes-append
                          (make-secret-salt/file "auth.salt")
                          (string->bytes/utf-8 salt))))

(define (request->user req)
  (cond [(findf (λ (c) (string=? "auth" (client-cookie-name c)))
                (request-cookies req))
         => (λ (c)
               (let* ([val (client-cookie-value c)]
                      [pos (string-index-right val #\&)]
                      [userid (if pos (substring val (add1 pos)) #f)])
                 (match
                   (query-maybe-row
                     db-conn
                     (select username password
                             #:from users
                             #:where (= id ,userid)))
                   [(vector username password*)
                    (if (valid-id-cookie?
                          c
                          #:name "auth"
                          #:key (bytes-append
                                  (make-secret-salt/file "auth.salt")
                                  (string->bytes/utf-8 password*)))
                      (user userid username)
                      #f)]
                   [_ #f])))]
        [else #f]))

(define (query-info conn info q)
  (cdr (assq info (simple-result-info (query conn q)))))

; authentication
; register and verify return login cookie if valid

(define (auth-register username password key fail)
  (if (zero? (query-info
               db-conn
               'affected-rows
               (delete #:from keys
                       #:where (= key ,(bytes->string key)))))
    (fail)
    (let* ([password* (pwhash 'argon2id password '((t 3) (m 102400) (p 8)))]
           [userid (query-info
                     db-conn
                     'insert-id
                     (insert #:into users
                             #:set
                             [created (julianday "now")]
                             [username ,(bytes->string username)]
                             [password ,password*]))])
      (userid->cookie userid password*))))

(define (auth-verify username password fail)
  (match (query-maybe-row
           db-conn
           (select id password
                   #:from users
                   #:where (= username ,(bytes->string username))))
         [(vector userid password*)
          (if (pwhash-verify #f password password*)
            (userid->cookie userid password*)
            (fail))]
         [_ (fail)]))

; key generation for new accounts

(define (generate-key)
  (list->string
    (sequence->list
      (sequence-map
        (λ (x) (integer->char (+ (char->integer #\A) (modulo x 26))))
        (crypto-random-bytes 8)))))
