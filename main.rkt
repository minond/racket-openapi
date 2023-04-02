#lang racket/base

(require "openapi.rkt")

(provide openai-api-key
         create-chat-completion)

(define openai-api-key
  (make-parameter (getenv "OPENAI_API_KEY")))

(openapi openai "openapi.yaml"
         #:headers (lambda (method path req)
                     (hash 'Content-Type "application/json"
                           'Authorization (format "Bearer ~a" (openai-api-key)))))
