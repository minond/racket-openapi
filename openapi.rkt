#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     racket/list
                     racket/string
                     syntax/parse
                     threading
                     yaml))

(require racket/string
         racket/list
         racket/sequence

         net/http-easy
         monotonic
         json)

(provide openapi)

(define charset
  (map integer->char
       (append (inclusive-range 48 57)
               (inclusive-range 65 90)
               (inclusive-range 97 122))))

(define (random-item xs)
  (sequence-ref xs (random (sequence-length xs))))

(define (random-string [len 16])
  (list->string
    (map (lambda (x)
           (random-item charset))
         (make-list len 0))))

(begin-for-syntax
  (struct endpoint (id path method) #:transparent)

  (define (normalize-id str)
    (regexp-replace* #px"([A-Z])" str
                     (lambda (_ ch)
                       (string-append "-" (string-downcase ch))))))

(define-syntax (openapi stx)
  (syntax-parse stx
    [(openapi path:str (~optional (~seq #:headers headers)))
     (define definition (file->yaml (syntax->datum #'path)))

     (define servers (hash-ref definition "servers"))
     (define paths (hash-ref definition "paths"))
     (define urls (map (lambda~> (hash-ref "url")) servers))

     (define endpoints
       (flatten
         (for/list ([path (hash-keys paths)])
           (define by-method (hash-ref paths path))
           (for/list ([method (hash-keys by-method)])
             (define info (hash-ref by-method method))
             (define name (normalize-id (hash-ref info "operationId")))
             (define id (format-id #'openapi name))
             (endpoint id path (string->symbol method))))))

     #`(begin
         (define urls '#,urls)
         (define (gen-url p)
           (string-join (list (car (shuffle urls)) p) ""))

         (define gen-headers
           (~? headers
               (lambda (method path req)
                 null)))

         #,@(for/list ([endpoint endpoints])
              `(define (,(endpoint-id endpoint) req)
                 (define method ',(endpoint-method endpoint))
                 (define path ,(endpoint-path endpoint))
                 (define url (gen-url path))
                 (define headers (gen-headers method path req))
                 (define data (string->bytes/utf-8
                                (jsexpr->string req)))
                 (define id (random-string))

                 (log-info "~a ~a ~a ~aB"
                           id method path
                           (bytes-length data))

                 (define start-time (current-monotonic-nanoseconds))
                 (define res
                   (,(endpoint-method endpoint) url
                                                #:headers headers
                                                #:json req))
                 (define end-time (current-monotonic-nanoseconds))

                 (log-info "~a ~a ~a ~aB ~ams"
                           id method path
                           (bytes-length (response-body res))
                           (ceiling (/ (- end-time start-time) 1000000)))

                 (response-json res))))]))
