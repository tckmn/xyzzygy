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

(require web-server/servlet
         web-server/servlet-env
         web-server/templates
         web-server/http/id-cookie
         db
         crypto
         crypto/argon2
         "db.rkt")

; utility functions

(define (userid->cookie userid salt)
  (make-id-cookie "auth" (number->string userid)
                  #:key (bytes-append
                          (make-secret-salt/file "auth.salt")
                          salt)))

; main dispatch table

(define-values (xyzzygy-dispatch xyzzygy-url)
  (dispatch-rules
    [("") homepage]
    [("login") login-page]
    [("about") about-page]))

; specific page logic

(define (response/html html)
  (response/output
    (λ (out) (write-string html out))))

(define (homepage req)
  (response/html (include-template "../templates/home.html")))

(define (login-page req)
  (if (bytes=? (request-method req) #"POST")
    (match (map (λ (field)
                   (bindings-assq field (request-bindings/raw req)))
                '(#"username" #"password"))
           [(list (? binding:form? username) (? binding:form? password))
            (=> fail)
            (match (query-maybe-row db-conn
                                    "SELECT id, password
                                     FROM users
                                     WHERE username = $1"
                                    (bytes->string/utf-8 username #\?))
                   [(vector userid password*)
                    (if (pwhash-verify #f password password*)
                      (redirect-to
                        "/" see-other
                        #:headers
                        (list (cookie->header (userid->cookie userid password*))))
                      (fail))]
                   [_ (fail)])]
           [_ (redirect-to "/login?failed" see-other)])
    (response/html (include-template "../templates/login.html"))))

(define (about-page req)
  (response/html (include-template "../templates/about.html")))

; main

(db-init)
(crypto-factories (list argon2-factory))

(serve/servlet xyzzygy-dispatch
               #:command-line? #t
               #:listen-ip #f
               #:servlet-regexp #rx""
               #:extra-files-paths (list (build-path "static")))
