#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

(require openai-api-client/client)

(provide define-endpoint)

(begin-for-syntax
  (define syntax->keyword
    (compose string->keyword symbol->string syntax->datum))

  (define-syntax-class field
    (pattern (name:id type:expr (~alt (~optional (~and #:optional optional))) ...)
      #:with optional? (if (attribute optional) #'#t #'#f)
      #:with object? #'(and (list? (syntax->datum #'type))
                            (equal? (car (syntax->datum #'type)) 'object))
      #:with object-content #'(cdr (syntax->datum #'type))
      #:with kwd (syntax->keyword #'name)
      #:with val #'(kwd name)
      #:with arg (if (attribute optional)
                     #'(kwd [name #f])
                     #'(kwd  name)))))

(define-syntax (define-endpoint stx)
  (syntax-parse stx
    [(_ name:id
        uri:expr
        (request-format:id request-field:field ...)
        (response-format:id response-field:field ...))
     (with-syntax* ([request-id (format-id #'name "request-~a" #'name)]
                    [build-request-id (format-id #'name "build-~a-request" #'name)]
                    [parse-response-id (format-id #'name "parse-~a-response" #'name)]
                    [top-response-id (format-id #'name "~a" #'name)]
                    [((arg ...) ...) #'(request-field.arg ...)]
                    [((val ...) ...) #'(request-field.val ...)])
       #'(begin
           (struct top-response-id (response-field.name ...) #:transparent)

           (define (parse-response-id raw)
             (define-values (response-field.name ...)
               (values (hash-ref raw 'response-field.name) ...))

             (top-response-id response-field.name ...))

           (define (build-request-id arg ... ...)
             (define data (make-hash))

             (if request-field.optional?
                 (when request-field.name
                   (hash-set! data 'request-field.name request-field.name))
                 (hash-set! data 'request-field.name request-field.name)) ...

             data)

           (define (request-id  arg ... ...)
             (make-request #:uri uri
                           #:data (build-request-id val ... ...)
                           #:normalizer parse-response-id))))]))

(define-endpoint chat-completions
  "https://api.openai.com/v1/chat/completions"
  (json [model string?]
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
  (json [id string?]
        [object string?]
        [created integer?]
        [model string?]
        [usage (object [prompt-tokens integer?]
                       [completion-tokens integer?]
                       [total-tokens integer?])]
        [choices (object [message (object [role string?]
                                          [content string?])]
                         [finish-reason string?]
                         [index integer?])]))
