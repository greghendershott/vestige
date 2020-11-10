#lang scribble/manual

@(require (for-label racket
                     racket/logging
                     vestige/explicit
                     vestige/logger)
          scribble/example)

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

 @item{More information is captured, including source location,
 @racket[current-thread], and @racket[current-inexact-milliseconds].
 The information is supplied in the ``value'' slot of each logger
 event vector as a @racket[hasheq]; rationale:

 @itemlist[

  @item{A @racket[hasheq] is easy to serialize to other formats such
  as JSON or association lists.}

  @item{A @racket[hasheq] may be extended over time by adding new
  mappings without breaking existing consumers.}]}]

As a result the logging information can be used in various ways:

@itemlist[

 @item{A tool such as @hyperlink["https://racket-mode.com"]{Racket
 Mode} can use this information to provide a better experience, such
 as jumping to the source of a trace, navigating the ``tree'' of
 traces, filtering traces by thread, and so on.}

 @item{Your own program may also use a @tech/ref{log receiver} to
 forward the information to logging systems that support a richer
 format than plain text, for example the JSON option of Amazon
 CloudWatch Logs.}]

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
package uses the name ``vestige'', a synonym for trace.


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

@defform[(trace-lambda [#:name id] args expr)]{
Like @|trace-lambda-id|.}

@defform*[((trace-let id ([arg expr] ...+) body ...+)
           (trace-let ([id expr] ...) body ...+))]{
The first form (``named let'') is like @|trace-let-id|.

The second form is simply @racket[let].}

@defform[(trace-expression expression)]{Equivalent to
@racket[(trace-let generated-identifier () expression)], although for
clarity the generated identifier is replaced with
@racket[(syntax->datum #'expression)] in trace logger events.

The rationale for expanding to @racket[trace-let] --- instead of
simply directly logging the @racket[expression] datum and the
resulting value --- is that the level (``call depth'') in relation to
other traced calls, as well as to nested uses of
@racket[trace-expression], will be available and correct for tools
that use levels for indent or other purposes.}

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

This module provides several constant values that are useful when you
want to make a @tech/ref{log receiver} receiver --- either from
scratch or by using @racket[with-intercepted-logging] or
@racket[with-logging-to-port].

@defthing[logger logger?]{The logger to which events are sent.}

@defthing[topic symbol?]{The topic for logger events.}

@defthing[level log-level/c]{The level for logger events.}


@section{Examples}

Here we show using the values from @racketmodname[vestige/logger] and
the convenience function @racket[with-intercepted-logging] to make a
@tech/ref{log receiver} that @racket[pretty-print]s the ``raw'' logger
event vectors:

@examples[#:eval (make-base-eval) #:no-prompt #:label #f
  (require racket/logging
           racket/format
           racket/match
           racket/pretty
           vestige/explicit
           vestige/logger)
  (define (example)
    (trace-define (f x) (+ 1 x))
    (f 42)
    (trace-expression (* 2 3)))
  (with-intercepted-logging pretty-print example #:logger logger level topic)
]

Note: The @racket['srcloc] mapping values in this example such as
@racketresult[(eval 2 0 2 1)] are a result of how these examples are
evaluated to build this documentation. In real usage, when the source
is a file, they would look something like @racketresult[("/path/to/file.rkt"
2 0 2 1)].

Tip: The @racket['thread] mapping values can be especially useful when
you keep in mind that the @racket[object-name] of a Racket
@tech/ref{thread descriptor} defaults to the name of its thunk
procedure. You can even use @racket[procedure-rename] to give every
thread a unique name related to a ``job'' or ``request'', as discussed
in
@hyperlink["https://www.greghendershott.com/2018/11/thread-names.html"]{this
blog post}.

Here is the example modified to convert the logger event value from a
@racket[hasheq] to JSON:

@examples[#:eval (make-base-eval) #:no-prompt #:label #f
  (require racket/logging
           racket/format
           racket/match
           vestige/explicit
           vestige/logger
           json)
  (define (example)
    (trace-define (f x) (+ 1 x))
    (f 42)
    (trace-expression (* 2 3)))
  (define (->jsexpr v)
    (cond [(hash? v)   (for/hasheq ([(k v) (in-hash v)])
                         (values k (->jsexpr v)))]
          [(list? v)   (map ->jsexpr v)]
          [(number? v) v]
          [else        (~a v)]))
  (define interceptor
    (match-lambda [(vector \_level _str value _topic)
                   (displayln (jsexpr->string (->jsexpr value)))]))
  (with-intercepted-logging interceptor example #:logger logger level topic)
]
