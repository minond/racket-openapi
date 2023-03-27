#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

(require request/param
         net/url-string
         json)

(provide define-endpoint)

(define openai-api-key (make-parameter (getenv "OPENAI_API_KEY")))

(begin-for-syntax
  (define syntax->keyword
    (compose string->keyword symbol->string syntax->datum))

  (define-syntax-class field-type
    (pattern (object -member:field ...)
      #:with object? #'#t
      #:with member #'(-member ...))
    (pattern object:expr
      #:with object? #'#f
      #:with member #'null))

  (define-syntax-class field
    (pattern (name:id (~var type field-type) (~alt (~optional (~and #:optional optional))) ...)
    ; (pattern (name:id type:expr (~alt (~optional (~and #:optional optional))) ...)
      #:with optional? (if (attribute optional) #'#t #'#f)
      ; #:with member #'(cdr (syntax->datum #'type))
      ; #:with object? #'(and (list? (syntax->datum #'type))
      ;                       (equal? (car (syntax->datum #'type)) 'object))
      #:with member #'type.member
      #:with object? #'type.object?
      #:with kwd (syntax->keyword #'name)
      #:with val #'(kwd name)
      #:with arg (if (attribute optional)
                     #'(kwd [name #f])
                     #'(kwd  name)))))

(define-syntax (define-endpoint stx)
  (syntax-parse stx
    [(_ name:id
        (~seq #:uri uri:expr)
        (~alt (~optional (~seq #:headers headers))) ...
        (~seq #:request (request-format:id request-field:field ...))
        (~seq #:response (response-format:id response-field:field ...)))
     (with-syntax* ([request-id (format-id #'name "request-~a" #'name)]
                    [build-request-id (format-id #'name "build-~a-request" #'name)]
                    [parse-response-id (format-id #'name "parse-~a-response" #'name)]
                    [url-id (format-id #'name "~a-url" #'name)]
                    [top-response-id (format-id #'name "~a" #'name)]
                    [((arg ...) ...) #'(request-field.arg ...)]
                    [((val ...) ...) #'(request-field.val ...)])
       #`(begin
           (struct top-response-id (response-field.name ...) #:transparent)
           (define url-id (if (url? uri)
                              uri
                              (string->url uri)))

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
             (define body (string->bytes/utf-8
                           (jsexpr->string
                            (build-request-id val ... ...))))
             (define response (post url-id body #:headers (~? headers null)))
             (parse-response-id
              (string->jsexpr
               (http-response-body response))))))]))

(define-endpoint chat-completions
  #:uri "https://api.openai.com/v1/chat/completions"
  #:headers (list "Content-Type: application/json"
                  (format "Authorization: Bearer ~a" (openai-api-key)))
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
                   [usage (object [prompt-tokens integer?]
                                  [completion-tokens integer?]
                                  [total-tokens integer?])]
                   [choices (object [message (object [role string?]
                                                     [content string?])]
                                    [finish-reason string?]
                                    [index integer?])]))
