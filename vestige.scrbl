#lang scribble/manual

@(require (for-label json
                     (except-in racket/base #%app)
                     racket/format
                     racket/logging
                     syntax/define
                     vestige/explicit
                     vestige/logger)
          scribble/example)

@(module m racket/base
   (require (for-label racket/base
                       racket/trace)
            scribble/manual)
   (define trace-id (racket trace))
   (define untrace-id (racket untrace))
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

Structured, logger-based ``tracing'' is useful for both ``debugging''
and ``devops''.

This package aims to support both scenarios and minimize littering
code with cross-cutting concerns.

@subsection{What it does differently from @racketmodname[racket/trace]}

This package mimics some of the tracing forms of
@racketmodname[racket/trace] such as @racket[trace-define] and adds a
@racket[trace-expression] form.

Although the basic technique used to capture calls, results, and
levels is similar to that used in @racketmodname[racket/trace],
additional information is captured and its disposition is different:

@itemlist[

 @item{Calls and results are emitted as @tech/ref{logger} events.}

 @item{As well as the arguments, results, and level captured by
 @racketmodname[racket/trace], more information is captured:

 @itemlist[

  @item{The @racket[srcloc] for the definition of the function being
  traced.}

  @item{The @racket[srcloc] for the call site is available when you
  use @racketmodname[vestige]'s @racket[#%app] to replace
  @racketmodname[racket/base]'s @|#%app-id| in modules that call a
  traced function. This can be useful for editing and debugging tools,
  but imposes some overhead for all function calls in such a module.}

  @item{The @racket[srcloc] for the context surrounding the call site,
  obtained from @racket[continuation-mark-set->context]. This is
  adequate for ``devops'' purposes such as examining logs for a server
  (which ordinarily lack any automatic caller context).}

  @item{Useful information that should be captured eagerly, such as
  @racket[current-thread] and @racket[current-inexact-milliseconds],
  because logger events are received later on another thread.}]}

 @item{Although each logger event vector has a simple ``message''
 string similar to @racketmodname[racket/trace], much more information
 is supplied in the ``value'' slot. The data is structured as a
 @racket[hasheq] that also satisfies @racket[jsexpr?]. Justification:
 It is easy to serialize to other formats such as JSON or association
 lists, and, a @racket[hasheq] may be extended over time by adding new
 mappings without breaking existing consumers.}

 @item{Functions are traced when they are defined, without mutation;
 there is no @|trace-id| or @|untrace-id|.}]

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
 more @emph{*waves hands*} ``declarative''.

 Then, for example, a tool such as
 @hyperlink["https://racket-mode.com"]{Racket Mode} can use the
 structured trace logging to provide a better experience --- such as
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
expressions in a module.

@defform[(trace-lambda [#:name id] args expr)]{

Like @|trace-lambda-id|.

This is the core form into which others expand.}

@defform*[((trace-define id expr)
           (trace-define (head args) body ...+))]{

Like @|trace-define-id|.

The implementation uses @racket[normalize-definition], supplying
@racket[trace-lambda]. As a result, function definitions will be
traced, whereas definitions of other values will not.}

@defform*[((trace-let id ([arg expr] ...+) body ...+)
           (trace-let ([id expr] ...) body ...+))]{

The first form is like @|trace-let-id|: It traces the function
implicitly defined and called by a ``named let''.

Otherwise, as with the second form, this defers to @racket[let].}

@defform[(trace-expression expression)]{

Equivalent to @racket[((trace-lambda #:name id () expression))], where
@racket[id] is synthesized from [(syntax->datum #'expression)] and
gets a syntax property with the printed value of @racket[expression].

The rationale for expanding to @racket[trace-lambda] --- instead of
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
information to be logged.}


@section{Recording call sites}

@defmodule[vestige/app]

The @racketmodname[vestige/app] module provides only @racket[#%app],
which adds a continuation mark to record the source location of an
application.

Requiring this module shadows @racketmodname[racket/base]'s
@|#%app-id|.

In other words, @racket[(require vestige/app)] is a convenient way to
enable call-site information for @emph{calls from} that module, but
otherwise you don't want to trace anything @emph{defined} in that
module.


@section{Using a log receiver}

@defmodule[vestige/logger]

This module provides several constant values that are useful when you
want to make a @tech/ref{log receiver} --- either writing one from
scratch or by using @racket[with-intercepted-logging] or
@racket[with-logging-to-port].

@defthing[logger logger?]{The logger to which events are sent.}

@defthing[topic symbol?]{The topic for logger events.}

@defthing[level log-level/c]{The level for logger events.}


@section{Hash table mappings}

Each @tech/ref{logger} event vector's ``value'' slot is a
@racket[hasheq] that also satisfies @racket[jsexpr?]. The hash-table
has at least the following mappings.

@(define-syntax-rule (defmapping key type pre-contents ...)
   (defthing #:kind "mapping" #:link-target? #f key type pre-contents ...))

@defmapping['call boolean?]{True when the event represents the
evaluation of a function call or expression.}

@defmapping['tail boolean?]{True when @racket[call] is true and this
is a tail call.}

@defmapping['name string?]{The name of the function being traced, or,
the datum of the expression being traced.}

@defmapping['level exact-nonnegative-integer?]{The call depth.}

@defmapping['show string?]{The function with its arguments, the
expression, or the results. Similar to the logger event vector's
``message'' slot but not prefixed by any @litchar{>} or @litchar{<}
characters to show level.}

@defmapping['thread string?]{The name of the @tech/ref{thread
descriptor} for the currently executing thread: @racket[(~a
(object-name (current-thread)))]}

@defmapping['msec real?]{The time of the function call or result:
@racket[(current-inexact-milliseconds)].}

@defthing[#:link-target? #f srcloc-as-list? (list/c (or/c path-string? #f) line column position span)]{

For ease of serialization, the remaining mapping values use a
representation of a @racket[srcloc] struct as a list. The first,
``source'' value is either a @racket[path] converted with
@racket[path->string], or @racket[#f].}

@defmapping['definition srcloc-as-list?]{The location of the traced
function definition or expression.}

@defmapping['caller (or/c #f srcloc-as-list?)]{The location of the
caller. This is only available when the call is from a module using
@racketmodname[vestige]'s @racket[#%app]. Otherwise the value will be
@racket[#f].}

@defmapping['context (or/c #f srcloc-as-list?)]{The location of the
context surrounding the call site. This can be @racket[#f] when
@racket[continuation-mark-set->context] reports no surrounding context
with a @racket[complete-path?] source.}

@section{Examples}

Here we show using the values from @racketmodname[vestige/logger] and
the convenience function @racket[with-intercepted-logging] to make a
@tech/ref{log receiver} that @racket[pretty-print]s the ``raw'' logger
event vectors:

@margin-note{Note: The source location values in this example such as
 @racketresult[(eval 2 0 2 1)] are a result of how these examples are
 evaluated to build this documentation. In real usage, when the source
 is a file, they would look something like
 @racketresult[("/path/to/file.rkt" 2 0 2 1)].}

@examples[#:eval (make-base-eval) #:no-prompt #:label #f
  (require racket/logging
           racket/pretty
           vestige/explicit
           vestige/logger)
  (define (example)
    (trace-define (f x) (+ 1 x))
    (trace-define (g x) (+ 1 (f x)))
    (g 42)
    (trace-expression (* 2 3)))
  (with-intercepted-logging pretty-print example
    #:logger logger level topic)
]

Here is the example modified to convert the logger event value from a
@racket[hasheq] to JSON:

@examples[#:eval (make-base-eval) #:no-prompt #:label #f
  (require json
           racket/logging
           racket/match
           vestige/explicit
           vestige/logger)
  (define (example)
    (trace-define (f x) (+ 1 x))
    (trace-define (g x) (+ 1 (f x)))
    (g 42)
    (trace-expression (* 2 3)))
  (define interceptor
    (match-lambda [(vector __level __str value __topic)
                   (displayln (jsexpr->string value))]))
  (with-intercepted-logging interceptor example
    #:logger logger level topic)
]

@subsection{Tip: Naming threads}

The @racket['thread] mapping values can be especially useful when you
keep in mind that the @racket[object-name] of a Racket
@tech/ref{thread descriptor} defaults to the name of its thunk
procedure. You can even use @racket[procedure-rename] to give each
thread thunk a unique name related to a ``job'' or ``request'', as
discussed in
@hyperlink["https://www.greghendershott.com/2018/11/thread-names.html"]{this
blog post}.

