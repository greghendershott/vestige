#lang scribble/manual

@(require (for-label (except-in racket/base #%app)
                     racket/logging
                     vestige/explicit
                     vestige/logger
                     json)
          scribble/example)

@(module m racket/base
   (require (for-label racket/base
                       racket/trace)
            scribble/manual)
   (define trace-define-id (racket trace-define))
   (define trace-lambda-id (racket trace-lambda))
   (define trace-let-id (racket trace-let))
   (define #%app-id (racket #%app))
   (provide (all-defined-out)))
@(require 'm)

@(define (tech/ref . pre-content)
   (apply tech #:doc '(lib "scribblings/reference/reference.scrbl") pre-content))

@title{Vestige}

@section{Introduction}

@emph{TL;DR}: Structured, logger-based tracing is useful for both
``debugging'' and ``devops''.

This package aims to support both scenarios, while minimizing littering
code with cross-cutting concerns.

@subsection{What it does differently from @racketmodname[racket/trace]}

This package mimics some of the tracing forms of
@racketmodname[racket/trace] and adds a @racket[trace-expression]
form. Although the basic technique used to capture calls, results, and
levels is similar to that used in @racketmodname[racket/trace],
additional information is captured and its disposition is different:

@itemlist[

 @item{Calls and results are emitted as @tech/ref{logger} events.}

 @item{As well as the arguments, results, and level captured by
 @racketmodname[racket/trace], more information is captured:

 @itemlist[

  @item{The @racket[srcloc] for the definition of the function being
  traced.}

  @item{The @racket[srcloc] for the call site. This is only available
  when you use @racketmodname[vestige]'s @racket[#%app] to replace
  @racketmodname[racket/base]'s @|#%app-id| in modules that call a
  traced function, the call site location will be exact. This might be
  useful for editing and debugging tools, but imposes some overhead
  for all function calls in such a module.}

  @item{The @racket[srcloc] for the context surrounding the call site,
  obtained from @racket[continuation-mark-set->context].This avoids
  any penalty for non-traced functions, and is adequate for ``devops''
  purposes such as examining logs for a server (which ordinarily lack
  any automatic caller context).}

  @item{Useful information that must be captured eagerly, such as
  @racket[current-thread] and @racket[current-inexact-milliseconds],
  because logger events are received later on another thread.}]}

 @item{Although each logger event vector has a simple ``message''
 string similar to @racketmodname[racket/trace], much more information
 is supplied in the ``value'' slot. The data is structured as a
 @racket[hasheq] that also satisfies @racket[jsexpr?]. Justification:
 It is easy to serialize to other formats such as JSON or association
 lists, and, a @racket[hasheq] may be extended over time by adding new
 mappings without breaking existing consumers.}]

@subsection{Two main use cases: ``debugging'' and ``devops''}

The structured trace information can be used in two main ways:

@itemlist[

 @item{As a debugging technique, this style of tracing is sometimes
 the ``warm bowl of porridge'' between simple @racket[print]s and a
 full step-debugger.

 Manual @racket[print]s or @racket[log-debug]s tend to litter code,
 aren't so ``simple'' by the time you format various values and
 information, and the resulting output messages aren't always easy to
 correlate with their producing source.

 At the other extreme, full step-debugging can be ``heavy'', as a
 result of a special evaluator needing to rewrite your program into a
 ``step-debuggable'' program.

 The warm porridge: You can @racket[(require vestige)] to trace
 everything in a module. Or if even that is too ``heavy'', you can
 @racket[(require vestige/explicit)] and trace select items by
 replacing e.g. ``define'' with ``trace-define'' --- but not otherwise
 changing the source. As a rough analogy, this is like using Racket
 @racketmodname[racket/contract] or @racketmodname[syntax/parse]:
 Although you do change the source, the change is minimal and feels
 more @emph{**waves hands**} ``declarative''.

 Then, for example, a tool such as
 @hyperlink["https://racket-mode.com"]{Racket Mode} can use the
 structure trace logging to provide a better experience --- such as
 navigating the ``tree'' of traces, jumping to a call site or function
 definition site, filtering traces by thread, and so on.}

 @item{As a devops technique, logging certain function calls is
 something servers often want to do. But again, it can be tedious and
 distracting to litter code with such logging.

 You can minimize that with this package, as described in the previous
 bullet point. Furthermore, your program can use a @tech/ref{log
 receiver} to forward the information to logging systems that support
 a richer format than plain text, for example the JSON option of
 Amazon CloudWatch Logs.}]

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

@defform[(trace-expression expression)]{

Equivalent to @racket[(trace-let generated-identifier () expression)],
although for clarity the generated identifier is replaced with
@racket[(syntax->datum #'expression)] in trace logger events.

The rationale for expanding to @racket[trace-let] --- instead of
simply directly logging the @racket[expression] datum and the
resulting value --- is that the level (``call depth'') in relation to
other traced calls, as well as to nested uses of
@racket[trace-expression], will be available and correct for tools
that use levels for indent or other purposes.}

@defform[(#%app expr ...+)]{

Expands to @racket[(with-continuation-mark key srcloc (#%app expr
...))], where @racket[key] is a private value, @racket[srcloc] is that
of the call site, and @|#%app-id| is that of
@racketmodname[racket/base].

Using this is optional. It imposes some overhead but allows call site
information to be exact.}

@section{Using a log receiver}

@defmodule[vestige/logger]

This module provides several constant values that are useful when you
want to make a @tech/ref{log receiver} receiver --- either from
scratch or by using @racket[with-intercepted-logging] or
@racket[with-logging-to-port].

@defthing[logger logger?]{The logger to which events are sent.}

@defthing[topic symbol?]{The topic for logger events.}

@defthing[level log-level/c]{The level for logger events.}


@section{Hash table mappings}

Each @tech/ref{logger} event vector's ``value'' slot is a
@racket[hasheq] with at least the following mappings.

@racket['kind]: @racket[(or/c "call" "results")].

@racket['name]: A @racket[string?] with the ``name'' of the thing
being defined, such as the name of the function being traced, or the
datum of the expression being traced.

@racket['level]: An @racket[exact-nonnegative-integer?] for the call
depth.

@racket['show]: A simple @racket[string?] to show. Similar to the
logger event vector's ``message'' slot but without any level
information.

@racket['def-site]: A @racket[srcloc] as a @racket[list] --- with the
first, ``source'' slot always @racket[(or/c #f string?)] --- for where
the traced function is defined, or where the traced expression is.

@racket['call-site]: A @racket[srcloc] as a @racket[list] --- as for
@racket['def-site] -- describing the call site. This is only available
when the call is from a module using @racketmodname[vestige]'s
@racket[#%app]. Otherwise the value will be @racket[#f].

@racket['context]: A @racket[srcloc] as a @racket[list] --- as for
@racket['def-site] -- describing the context surrounding the call
site. This can be @racket[#f] when
@racket[continuation-mark-set->context] reports no surrounding context
that has source location.

@racket['thread]: @racket[(~a (object-name (current-thread)))] at the
time of the function call or result.

@racket['msec]: @racket[(current-inexact-milliseconds)] at the time of
the function call or result.


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

Note: The source location values for @racket['defined] and
@racket['called] in this example such as @racketresult[(eval 2 0 2 1)]
are a result of how these examples are evaluated to build this
documentation. In real usage, when the source is a file, they would
look something like @racketresult[("/path/to/file.rkt" 2 0 2 1)].

Tip: The @racket['thread] mapping values can be especially useful when
you keep in mind that the @racket[object-name] of a Racket
@tech/ref{thread descriptor} defaults to the name of its thunk
procedure. You can even use @racket[procedure-rename] to give each
thread thunk a unique name related to a ``job'' or ``request'', as
discussed in
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
  (define interceptor
    (match-lambda [(vector \_level _str value _topic)
                   (displayln (jsexpr->string value))]))
  (with-intercepted-logging interceptor example #:logger logger level topic)
]
