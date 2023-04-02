#lang racket/base

(require "openapi.rkt")

(provide openai-api-key
         create-chat-completion)

(define openai-api-key (make-parameter (getenv "OPENAI_API_KEY")))

(openapi openai "openapi.yaml" #:bearer (openai-api-key))
