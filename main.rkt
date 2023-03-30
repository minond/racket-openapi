#lang racket/base

(require openai-api-client/spec)

(define completions
  (request-chat-completions
   #:model "gpt-3.5-turbo"
   #:messages (list (hash 'role "user"
                          'content "Can you write a summary of https://www.allthingsdistributed.com/2023/03/australia-the-new-epicenter-for-healthtech-startups.html"))))

(for ([choice (chat-completions-response-choices completions)])
  (define message (chat-completions-choices-response-message choice))
  (define content (chat-completions-choices-message-response-content message))
  (displayln content))
