#lang racket/base

(require "endpoint.rkt")

(provide openai-api-key
         request-chat-completions                               ; TODO export with -out helper once available
         chat-completions-response                              ; TODO export with -out helper once available
         chat-completions-response-id                           ; TODO export with -out helper once available
         chat-completions-response-object                       ; TODO export with -out helper once available
         chat-completions-response-created                      ; TODO export with -out helper once available
         chat-completions-response-model                        ; TODO export with -out helper once available
         chat-completions-response-usage                        ; TODO export with -out helper once available
         chat-completions-response-choices                      ; TODO export with -out helper once available
         chat-completions-usage-response                        ; TODO export with -out helper once available
         chat-completions-usage-response-prompt_tokens          ; TODO export with -out helper once available
         chat-completions-usage-response-completion_tokens      ; TODO export with -out helper once available
         chat-completions-usage-response-total_tokens           ; TODO export with -out helper once available
         chat-completions-choices-response                      ; TODO export with -out helper once available
         chat-completions-choices-response-message              ; TODO export with -out helper once available
         chat-completions-choices-response-finish_reason        ; TODO export with -out helper once available
         chat-completions-choices-response-index                ; TODO export with -out helper once available
         chat-completions-choices-message-response              ; TODO export with -out helper once available
         chat-completions-choices-message-response-role         ; TODO export with -out helper once available
         chat-completions-choices-message-response-content)     ; TODO export with -out helper once available

(define openai-api-key (make-parameter (getenv "OPENAI_API_KEY")))

(define-endpoint chat-completions
  #:uri "https://api.openai.com/v1/chat/completions"
  #:headers (list "Content-Type: application/json"
                  (format "Authorization: Bearer ~a" (openai-api-key))) ; TODO this should be list?/procedure?
  #:request (json [model string?]
                  [messages list?]
                  [temperature number? #:optional]
                  [top-p number? #:optional]
                  [n number? #:optional]
                  [stream boolean? #:optional]
                  [stop (or? string? list?) #:optional]
                  [max-tokens integer? #:optional]
                  [presence-penalty number? #:optional]
                  [frequency-penalty number? #:optional]
                  [logit-bias hash? #:optional]
                  [user string? #:optional])
  #:response (json [id string?]
                   [object string?]
                   [created integer?]
                   [model string?]
                   [usage (object [prompt_tokens integer?]              ; TODO dash to underscore
                                  [completion_tokens integer?]          ; TODO dash to underscore
                                  [total_tokens integer?])]             ; TODO dash to underscore
                   [choices (list [message (object [role string?]
                                                   [content string?])]
                                  [finish_reason string?]               ; TODO dash to underscore
                                  [index integer?])]))
