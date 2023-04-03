# OpenAPI + Racket

This is an [OpenAPI][openapi] client generator for Racket. Provide the
`openapi` macro with an OpenAPI specification file, and it will generate
functions for each endpoint.

For example, let's say we want to generate a client for [OpenAI's
API][openai_api_docs]. We'll first download their specification file from [this
repo][openai_api_spec] (and name it `openai-openapi.yaml`), and then run the
following code:

```racket
(openapi openai "openai-openapi.yaml" #:bearer (getenv "OPENAI_API_KEY"))
```

This will create a function for each endpoint found in the specification. These
functions have a keyword argument for each field in the request. Request fields
that have a default value or are nullable are optional arguments, while the
rest are required:

```racket
(define response
  (create-chat-completion
   #:model "gpt-3.5-turbo"
   #:user "sample"
   #:messages (list (hash 'role "user"
                          'content "Can you tell me a story?"))))
```

The returned value of every generated function is a hash of the JSON-parsed
body of the response. Note that this will change in the future and a component
schema specific struct will be returned instead.

## TODO

- [x] Generate request functions for all endpoints
- [x] Accept headers generator
- [x] Accept authorization shorthand
- [x] Generate request methods that accept keyword arguments instead of hashes.
- [x] Add avility to only generate specific actions.
- [ ] Dynamically provide all symbols created by the macro.
- [ ] Handle URL parameters
- [ ] Generate response structs and return that instead of hashes.
- [ ] Handle endpoints that don't have request bodies.
- [ ] Add documentation

---

[![Build](https://github.com/minond/racket-openapi/actions/workflows/ci.yml/badge.svg)](https://github.com/minond/racket-openapi/actions/workflows/ci.yml)

[openapi]: https://openapi-generator.tech
[openai_api_docs]: https://platform.openai.com/docs/introduction
[openai_api_spec]: https://github.com/openai/openai-openapi/tree/master
