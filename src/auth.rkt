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
         "db.rkt")

(provide auth-register auth-verify)

(crypto-factories (list argon2-factory))

; utility functions

(define (bytes->string b) (bytes->string/utf-8 b #\?))

(define (userid->cookie userid salt)
  (make-id-cookie "auth" (number->string userid)
                  #:key (bytes-append
                          (make-secret-salt/file "auth.salt")
                          salt)))

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
    (let* ([password* (kdf 'argon2id password (crypto-random-bytes 16)
                           '((t 3) (m 102400) (p 8) (key-size 32)))]
           [userid (query-info
                     db-conn
                     'insert-id
                     (insert #:into users
                             #:set
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
