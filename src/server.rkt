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
         "db.rkt"
         "auth.rkt")

; main dispatch table

(define-values (xyzzygy-dispatch xyzzygy-url)
  (dispatch-rules
    [("") homepage/get]
    [("login") login-page/get]
    [("login") #:method "post" login-page/post]
    [("about") about-page/get]
    [("admin" "keys") keys-page/get]
    [("admin" "keys") #:method "post" keys-page/post]))

; specific page logic

(define (response/html html)
  (response/output
    (λ (out) (write-string html out))))

(define (homepage/get req)
  (response/html (include-template "../templates/home.html")))

(define (login-page/get req)
  (let ([failed (assq 'failed (url-query (request-uri req)))])
    (response/html (include-template "../templates/login.html"))))

(define (login-page/post req)
  (match (map (λ (field)
                 (bindings-assq field (request-bindings/raw req)))
              '(#"username" #"password" #"key"))
         [(list (? binding:form? username) (? binding:form? password) (? binding:form? key))
          (=> fail)
          (redirect-to
            "/" see-other
            #:headers
            (list (cookie->header (auth-register
                                    (binding:form-value username)
                                    (binding:form-value password)
                                    (binding:form-value key) fail))))]
         [(list (? binding:form? username) (? binding:form? password) #f)
          (=> fail)
          (redirect-to
            "/" see-other
            #:headers
            (list (cookie->header (auth-verify
                                    (binding:form-value username)
                                    (binding:form-value password) fail))))]
         [_ (redirect-to "/login?failed" see-other)]))

(define (about-page/get req)
  (response/html (include-template "../templates/about.html")))

(define (keys-page/get req)
  (response/html (include-template "../templates/admin/keys.html")))

(define (keys-page/post req)
  (response/html (include-template "../templates/admin/keys.html")))

; main

(db-init)

(serve/servlet xyzzygy-dispatch
               #:command-line? #t
               #:listen-ip #f
               #:servlet-regexp #rx""
               #:extra-files-paths (list (build-path "static")))
