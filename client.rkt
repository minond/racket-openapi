#lang racket/base

(require racket/function
         request/param
         net/url-string
         json)

(provide openai-api-key
         make-request)

(define openai-api-key (make-parameter (getenv "OPENAI_API_KEY")))

(define (make-request #:uri uri #:data data #:normalizer [normalizer identity])
  (define url (string->url uri))
  (define body (string->bytes/utf-8 (jsexpr->string data)))
  (define headers (list "Content-Type: application/json"
                        (format "Authorization: Bearer ~a" (openai-api-key))))
  (define response (post url body #:headers headers))
  (define response-body (http-response-body response))
  (normalizer (string->jsexpr response-body)))
