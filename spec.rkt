#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

(require racket/function
         racket/list
         racket/string
         request/param
         net/url-string
         json)

(provide define-endpoint)

(define openai-api-key (make-parameter (getenv "OPENAI_API_KEY")))

(define (field-is? field ty)
  (and (list? (cadr field))
       (equal? ty (caadr field))))

(define (object-field? field)
  (field-is? field 'object))

(define (list-field? field)
  (field-is? field 'list))

(define (append-element lst elem)
  (append lst (list elem)))

(define (parse-data schema data path)
  (define values
    (map (lambda (field)
           (cond [(object-field? field)
                  (parse-data (cdadr field)
                              (hash-ref data (car field))
                              (append-element path (car field)))]
                 [(list-field? field)
                  (map (lambda (item-data)
                         (parse-data (cdadr field)
                                     item-data
                                     (append-element path (car field))))
                       (hash-ref data (car field)))]
                 [else
                  (hash-ref data (car field))]))
         schema))

  (define container-class
    (eval (string->symbol (string-join
                           (append-element (map symbol->string path) "response")
                           "-"))))
  (apply container-class values))

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
       (define (field-is? field ty)
         (and (list? (cadr field))
              (equal? ty (caadr field))))

       (define (object-field? field)
         (field-is? field 'object))

       (define (list-field? field)
         (field-is? field 'list))

       (define (complex-field? field)
         (or (object-field? field)
             (list-field? field)))

       (define (find-complex-objects fields [prefix ""] [path null])
         (apply
          append
          (map (lambda (field)
                 (let* ([name (car field)]
                        [qualified-name (format "~a~a" prefix name)]
                        [next-prefix (format "~a-" qualified-name)]
                        [stx (datum->syntax #'define-endpoint field)]
                        [subfields (cdadr field)]
                        [path (append path (list name))])
                   (cons (hash 'id (format-id stx "~a-response" qualified-name)
                               'instance-id (format-id stx "~a-object" qualified-name)
                               'fields (map car subfields)
                               'field-details (map (lambda (f)
                                                     (define is-object? (object-field? f))
                                                     (define is-list? (list-field? f))
                                                     (define is-complex? (complex-field? f))
                                                     (hash 'name (car f)
                                                           'object? is-object?
                                                           'list? is-list?
                                                           'container (when is-complex?
                                                                        (format-id stx "~a-~a-object" name (car f)))))
                                                   subfields)
                               'path path)
                         (find-complex-objects subfields next-prefix path))))
               (filter complex-field? fields))))

       (define response-fields
         (syntax->datum #'(response-field ...)))

       (define response-objects
         (find-complex-objects
          (list (cons (syntax->datum #'name)
                      (list (cons 'object response-fields))))))

       (define top-response (car response-objects))
       (define top-response-id (hash-ref top-response 'id))
       (define top-response-instance-id (hash-ref top-response 'instance-id))
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
             (parse-data '#,response-fields raw '(name)))

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
                   [usage (object [prompt_tokens integer?]
                                  [completion_tokens integer?]
                                  [total_tokens integer?])]
                   [choices (list [message (object [role string?]
                                                   [content string?])]
                                  [finish_reason string?]
                                  [index integer?])]
                   ))
