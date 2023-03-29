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
    [(define-endpoint name:id
       (~seq #:uri uri:expr)
       (~alt (~optional (~seq #:headers headers))) ...
       (~seq #:request (request-format:id request-field:field ...))
       (~seq #:response (response-format:id response-field:field ...)))
     (with-syntax* ([request-id (format-id #'name "request-~a" #'name)]
                    [build-request-id (format-id #'name "build-~a-request" #'name)]
                    [parse-response-id (format-id #'name "parse-~a-response" #'name)]
                    [url-id (format-id #'name "*~a-url*" #'name)]
                    [((arg ...) ...) #'(request-field.arg ...)]
                    [((val ...) ...) #'(request-field.val ...)])
       (define (object-field? field)
         (list? (cadr field)))

       (define (find-complex-objects fields [prefix ""] [path null])
         (apply
          append
          (map (lambda (field)
                 (let* ([name (car field)]
                        [qualified-name (format "~a~a" prefix name)]
                        [subfields (cdadr field)])
                   (cons (hash 'id (format-id (datum->syntax #'define-endpoint field)
                                              "~a-response" qualified-name)
                               'fields (map car subfields)
                               'path path)
                         (find-complex-objects subfields
                                               (format "~a-" qualified-name)
                                               (append path (list name))))))
               (filter object-field? fields))))

       (define response-fields
         (syntax->datum #'(response-field ...)))

       (define response-objects
         (find-complex-objects
          (list (cons (syntax->datum #'name)
                      (list (cons 'object response-fields))))))

       (define top-response (car response-objects))
       (define top-response-id (hash-ref top-response 'id))
       (define top-response-fields (hash-ref top-response 'fields))

       #`(begin
           #,@(map (lambda (obj)
                     `(struct ,(hash-ref obj 'id) (,@(hash-ref obj 'fields)) #:transparent))
                   response-objects)

           (define url-id (make-parameter
                           (if (url? uri)
                               uri
                               (string->url uri))))

           (define (parse-response-id raw)
             (define-values (#,@top-response-fields)
               (values #,@(map (lambda (field)
                                 `(hash-ref raw ',field))
                               top-response-fields)))

             (#,top-response-id #,@top-response-fields))

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
             (define response (post (url-id) body #:headers (~? headers null)))
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
