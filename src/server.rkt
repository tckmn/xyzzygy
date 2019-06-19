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
         xml
         "db.rkt"
         "auth.rkt"
         "util.rkt")

; utility functions

(define (binding req field) (bindings-assq field (request-bindings/raw req)))
(define (bindings req fields) (map (λ (field) (binding req field)) fields))

; main dispatch table

(define-values (xyzzygy-dispatch xyzzygy-url)
  (dispatch-rules
    [("") homepage/get]
    [("login") login-page/get]
    [("login") #:method "post" login-page/post]
    [("logout") #:method "post" logout-page/post]
    [("about") about-page/get]
    [("users") users-page/get]
    [("games") games-page/get]
    [("decks") decks-page/get]
    [("admin" "keys") keys-page/get]
    [("admin" "keys") #:method "post" keys-page/post]))

; specific page logic

(define (response/html html)
  (response/output
    (λ (out) (write-string html out))))

(define (homepage/get req)
  (let ([u (request->user req)])
    (response/html (include-template "../templates/home.html"))))

(define (login-page/get req)
  (let ([u (request->user req)]
        [failed (assq 'failed (url-query (request-uri req)))])
    (response/html (include-template "../templates/login.html"))))

(define (login-page/post req)
  (match
    (bindings req '(#"username" #"password" #"key"))
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

(define (logout-page/post req)
  (redirect-to "/" see-other
               #:headers
               (list (cookie->header (logout-id-cookie "auth")))))

(define (about-page/get req)
  (let ([u (request->user req)])
    (response/html (include-template "../templates/about.html"))))

(define (users-page/get req)
  (let ([u (request->user req)])
    (response/html (include-template "../templates/users.html"))))

(define (games-page/get req)
  (let ([u (request->user req)])
    (response/html (include-template "../templates/games.html"))))

(define (decks-page/get req)
  (let ([u (request->user req)])
    (response/html (include-template "../templates/decks.html"))))

(define (keys-page/get req)
  (let ([u (request->user req)])
    (response/html (include-template "../templates/admin/keys.html"))))

(define (make-key key)
  (match (bytes->string (binding:form-value key)) ["" (generate-key)] [s s]))
(define (keys-page/post req)
  (cond
    [(binding req #"addkey")
     => (λ (key)
           (query-exec
             db-conn
             (insert #:into keys
                     #:set [key ,(make-key key)] [created (julianday "now")])))]
    [(binding req #"delkey")
     => (λ (key)
           (query-exec
             db-conn
             (delete #:from keys
                     #:where (= key ,(make-key key)))))])
  (redirect-to "/admin/keys" see-other))

(define (404-page req)
  (let ([u (request->user req)])
    (response/html (include-template "../templates/404.html"))))

; main

(db-init)

(serve/servlet xyzzygy-dispatch
               #:command-line? #t
               #:listen-ip #f
               #:port 3001
               #:servlet-regexp #rx""
               #:server-root-path (current-directory)
               #:servlets-root (current-directory)
               #:file-not-found-responder 404-page
               #:extra-files-paths (list (build-path "static")))
