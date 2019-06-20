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
    [("")                             (pw homepage/get)]
    [("login")                        (pw login-page/get)]
    [("login")        #:method "post" (pw login-page/post)]
    [("logout")       #:method "post" (pw logout-page/post)]
    [("about")                        (pw about-page/get)]
    [("users")                        (pw users-page/get)]
    [("users" (integer-arg))          (pw users-page:id/get)]
    [("games")                        (pw games-page/get)]
    [("decks")                        (pw decks-page/get)]
    [("admin" "keys")                 (pw keys-page/get #:admin #t)]
    [("admin" "keys") #:method "post" (pw keys-page/post #:admin #t)]))

(define ((pw page #:admin [admin #f]) . args) ; page wrapper
  (let ([u (request->user (car args))])
    (apply (if (and admin (nand u (user-admin? u))) 403-page page) u args)))

; specific page logic

(define (response/html html #:code [code 200])
  (response/output #:code code
    (λ (out) (write-string html out))))

(define (homepage/get u req)
  (response/html (include-template "../templates/home.html")))

(define (login-page/get u req)
  (let ([failed (assq 'failed (url-query (request-uri req)))])
    (response/html (include-template "../templates/login.html"))))

(define (login-page/post u req)
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

(define (logout-page/post u req)
  (redirect-to "/" see-other
               #:headers
               (list (cookie->header (logout-id-cookie "auth")))))

(define (about-page/get u req)
  (response/html (include-template "../templates/about.html")))

(define (users-page/get u req)
  (response/html (include-template "../templates/users.html")))

(define (users-page:id/get u req userid)
  (let ([user (userid->user userid)])
    (if user
        (response/html (include-template "../templates/users_id.html"))
        (404-page req))))

(define (games-page/get u req)
  (response/html (include-template "../templates/games.html")))

(define (decks-page/get u req)
  (response/html (include-template "../templates/decks.html")))

(define (keys-page/get u req)
  (response/html (include-template "../templates/admin/keys.html")))

(define (make-key key)
  (match (bytes->string (binding:form-value key)) ["" (generate-key)] [s s]))
(define (keys-page/post u req)
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

(define (403-page u req . _)
  (response/html #:code 403 (include-template "../templates/403.html")))

(define (404-page u req . _)
  (response/html #:code 404 (include-template "../templates/404.html")))

; main

(db-init)

(serve/servlet xyzzygy-dispatch
               #:command-line? #t
               #:banner? #t
               #:listen-ip #f
               #:port 3371
               #:servlet-regexp #rx""
               #:server-root-path (current-directory)
               #:servlets-root (current-directory)
               #:file-not-found-responder (pw 404-page)
               #:log-file (current-output-port)
               #:extra-files-paths (list (build-path "static")))
