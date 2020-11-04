#lang scribble/manual

@(require (for-label racket
                     racket/logging
                     vestige/explicit
                     vestige/logger)
          scribble/example)

@(define the-eval (make-base-eval))

@(module m racket/base
   (require (for-label racket/trace) scribble/manual)
   (define trace-id (racket trace))
   (define untrace-id (racket untrace))
   (define trace-define-id (racket trace-define))
   (define trace-lambda-id (racket trace-lambda))
   (define trace-let-id (racket trace-let))
   (provide (all-defined-out)))
@(require 'm)

@(define (tech/ref . pre-content)
   (apply tech #:doc '(lib "scribblings/reference/reference.scrbl") pre-content))

@title{Vestige}

This package mimics the tracing forms of @racketmodname[racket/trace]
and adds a @racket[trace-expression] form.

Although the technique used to capture calls and results is similar to
that used in @racketmodname[racket/trace], additional information is
captured and its disposition is different.

@itemlist[

 @item{Calls and results are emitted as @tech/ref{logger} events.}

 @item{More information is captured, including source location, and
 supplied in the value field of each logger event.

 @itemlist[

  @item{A tool such as @hyperlink["https://racket-mode.com"]{Racket
  Mode} can use this information to provide a better experience, such
  as jumping to the source of a trace, navigating the ``tree'' of
  traces, filtering traces by thread, and so on.}

  @item{Your own program may also use a @tech/ref{log receiver} to
  forward the information to logging systems that support a richer
  format than plain text, for example the JSON option of Amazon
  CloudWatch Logs.}]}]

In other words, this package can be a drop-in replacement for
@racketmodname[racket/trace] in the source files being traced, while
providing more options both for tools and your own logging needs.

@itemlist[

 @item{As a debugging technique, this style of tracing is sometimes
 the ``warm bowl of porridge'' between simple @racket[print] and a
 full step-debugger. (The latter can be ``heavy'' as a result of a
 special evaluator needing to rewrite your program into a
 ``step-debuggable'' program.)}

 @item{As a devops technique, logging certain function calls is
 something servers often want to do.}]

The premise is this one package can support both needs, and also
minimizing littering code with cross-cutting concerns.

Because ``trace'' is already somewhat overloaded in Racket --- see
``calltrace'' and ``errortrace'' as well as ``racket/trace'' --- this
package uses the name vestige which is a synonym for trace.


@section{Tracing all functions in a module}

@defmodule[vestige]

The @racketmodname[vestige] module provides the same forms as does
@racketmodname[vestige/explicit], but named without the ``trace-''
prefix. For example @racket[trace-define] is provided renamed as
@racket[define]. As a result, requiring this module shadows those
definitions from the @racketmodname[racket/base] language. In other
words, @racket[(require vestige)] is a convenient way to trace
everything in a module without otherwise needing to litter its source
with individual changes.


@section{Tracing specific functions or expressions}

@defmodule[vestige/explicit]

The @racketmodname[vestige/explicit] module provides distinctly named
forms. Use this when you want to trace only some functions or
expressions in a module, or do ad hoc tracing in a REPL.

@defform*[((trace-define id expr)
           (trace-define (head args) body ...+))]{
Like @|trace-define-id|.}

@defform[(trace-lambda [#:name id] args expr)]{Like @|trace-lambda-id|.}

@defform[(trace-let id ([arg expr] ...+) body ...+)]{Like @|trace-let-id|.}

@defform[(trace-expression e)]{Equivalent to @racket[(trace-let
gensym-id () e)] but for clarity the generated identifier is replaced
with @racket[e] in the trace logger events.}

@subsection{Ad hoc tracing in REPL}

@defform[(trace id ...)]{Like @|trace-id|.

This form is useful in a REPL to do ad hoc tracing.

In a source file it is usually preferable to use one of the other
tracing forms because they supply source location for the function or
expression being traced (whereas @racket[trace] can only record the
source location where it itself is used).}

@defform[(untrace id ...)]{Like @|untrace-id|.}


@section{Using a log receiver}

@defmodule[vestige/logger]

This module provides several values that are useful when you want to
make a log receiver --- either from scratch or by using
@racket[with-intercepted-logging] or @racket[with-logging-to-port].

@defthing[logger logger?]{The logger to which events are sent.}

@defthing[topic 'vestige-trace]{The topic for logger events.}

@defthing[level 'debug]{The level for logger events.}


@section{Examples}

Here we show using the values from @racketmodname[vestige/logger] to
make a log receiver (using the convenience
@racket[with-intercepted-logging]) that shows the logger events:

@examples[#:eval the-eval #:no-prompt #:label #f
(require vestige/explicit
         racket/logging
         racket/match
         racket/pretty
         vestige/logger)

(with-intercepted-logging
  (match-lambda [(vector _level message-str val _topic)
                 (pretty-print (list message-str val))])
  #:logger logger
  (lambda ()
    (trace-define (f x) (if (zero? x) 0 (add1 (f (sub1 x)))))
    (f 3)
    (trace-expression (+ 1 2)))
  level
  topic)
]

Keep in mind that the @racket['srcloc] mapping values such as
@racket['(eval 2 0 1 2)] are a result of how these examples are
evaluated to build this documentation. In real usage, when the source
is a file, the values would look something like
@racket['("/path/to/foo.rkt" 2 0 1 2)].
