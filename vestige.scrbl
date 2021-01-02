#lang scribble/manual

@(require (for-label json
                     (except-in racket/base #%app)
                     racket/contract
                     racket/class
                     racket/format
                     racket/logging
                     syntax/define
                     syntax/name
                     vestige/tracing/explicit
                     vestige/tracing/class/explicit
                     vestige/logging
                     vestige/app
                     vestige/receiving)
          scribble/example)

@; A way to get links to things from racket/base and racket/trace that
@; are shadowed by vestige. IIRC I learned this from typed/racket.
@(module shadowed racket/base
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
@(require 'shadowed)

@(define (tech/ref . pre-content)
   (apply tech #:doc '(lib "scribblings/reference/reference.scrbl") pre-content))

@(define-syntax-rule (ex eval pre-content ...)
   (examples #:eval eval
             #:no-prompt
             #:label #f
             #:preserve-source-locations
             pre-content ...))

@(define-syntax-rule (ex/show pre-content ...)
   (let ((e (make-base-eval)))
     (examples #:eval e #:hidden (require (submod vestige/receiving private start)))
     (ex e pre-content ...)))

@(define-syntax-rule (ex/no-show pre-content ...)
   (ex (make-base-eval) pre-content ...))

@(define-syntax-rule (defmapping key type pre-contents ...)
   (defthing #:kind "mapping" #:link-target? #f key type pre-contents ...))

@title{Vestige}

@section{Introduction}

This package enhances logging generally, and, treats tracing as a
particular kind of logging.


@section{Tracing}

@margin-note{Because ``trace'' is already used by the
@hyperlink["https://pkgs.racket-lang.org/package/trace"]{trace} and
@hyperlink["https://pkgs.racket-lang.org/package/errortrace-lib"]{errortrace}
packages, as well as @racketmodname[racket/trace], this package is
named after a synonym, ``vestige''.}

The @racket[vestige/tracing] modules capture information about
function calls and results, as well as expression evaluations.

The information is supplied using structured logging.

Intended uses include both ``debugging'' and ``devops''.

Intended benefits include minimizing cross-cutting concerns.


@subsection{What it does differently from @racketmodname[racket/trace]}

The @racketmodname[vestige/tracing/explicit] module mimics some of the
forms of @racketmodname[racket/trace] such as @racket[trace-define]
and adds forms like tracing for @racket[racket/class] method
definitions and @racket[trace-expression] form.

Although the basic technique used to capture calls, results, and
levels is similar to that used in @racketmodname[racket/trace],
additional information is captured and its disposition is different:

@itemlist[

 @item{Instrumentation is enabled upon definition; there is no
 mutating @|trace-id| or @|untrace-id|.}

 @item{Information is captured in @tech/ref{continuation marks} and
 emitted as @tech/ref{logger} events.}

 @item{As well as the arguments, results, and call depth captured by
 @racketmodname[racket/trace], more information is captured:

 @itemlist[

  @item{Timing and thread information is captured eagerly using
  @racket[with-more-logging-info].}

  @item{@racket[srcloc] for various interesting things, when
  available:

   @itemlist[

    @item{The @emph{definition} of the function being called.}

    @item{The @emph{header} and @emph{formals} spans within the
    definition. Tools can use this to present logs in a UI resembling
    a step debugger.}

    @item{Other srcloc information captured by
    @racketmodname[vestige/logging] like the context and caller.}]}]}]

Also, @racketmodname[vestige/tracing/class/explicit] enables tracing
@racketmodname[racket/class] method definitions, such as
@racket[trace-define/override].

@subsection{Two main use cases: ``debugging'' and ``devops''}

Structured tracing logs can be used in two main ways:

@itemlist[

 @item{As a debugging technique, this approach is sometimes the ``warm
 bowl of porridge'' between simple @racket[print]s and a full
 step-debugger.

 Manual @racket[print]s or @racket[log-debug]s tend to litter code,
 aren't so ``simple'' by the time you format various values and
 information, and the resulting output messages aren't always easy to
 correlate with their producing source.

 At the other extreme, full step-debugging can be ``heavy'', as a
 result of a special evaluator needing to rewrite your program into a
 ``step-debuggable'' program.

 The warm porridge: You can @racket[(require
 vestige/tracing/implicit)] to instrument everything in a module. Or
 if even that is too ``heavy'', you can @racket[(require
 vestige/tracing/explicit)] and instrument select items by replacing
 e.g. ``define'' with ``trace-define'' --- but not otherwise changing
 the source. As a rough analogy, this is like using Racket
 @racketmodname[racket/contract] or @racketmodname[syntax/parse]:
 Although you do change the source, the change is minimal and feels
 more @italic{*waves hands*} ``declarative''.

 Then, for example, a tool such as
 @hyperlink["https://racket-mode.com"]{Racket Mode} can use the
 structured logs to provide a better experience --- such as navigating
 the ``tree'' of calls, jumping to a call site or function definition
 site, filtering items by thread, and so on. It can even show function
 call arguments @italic{in situ}, starting to resemble @italic{*waves
 hands*} a low-granularity ``time travel debugger''.}

 @item{As a devops technique, logging certain function calls is often
 something you want to do for a long-running server. But again, this
 is a cross-cutting concern; it can be tedious and distracting to
 litter code with such logging.

 You can minimize that with this package, as described in the previous
 bullet point. Furthermore, your program can use a @tech/ref{log
 receiver} to forward the information to logging systems that support
 a richer format than plain text, for example the JSON option of
 Amazon CloudWatch Logs.}]


@subsection{Instrumenting all functions defined in a module}

@defmodule[vestige/tracing/implicit]

Provides the same forms as does
@racketmodname[vestige/tracing/explicit], but named without the
``trace-'' prefix. For example @litchar{define} is actually
@racket[trace-define]. As a result, requiring this module shadows
those definitions from the @racketmodname[racket/base] language.

In other words, @racket[(require vestige/tracing/implicit)] is a
convenient way to trace everything in a module without otherwise
needing to litter its source with individual changes.

At the same time, this module also provides @racket[trace-expression]
so that you may use that on specific expressions of special interest.

@subsection{Instrumenting all @racketmodname[racket/class] methods defined in a module}

@defmodule[vestige/tracing/class/implicit]

Provides the exports of @racketmodname[racket/class], but instead of
providing method definition forms like @racket[define/private],
provides the same forms as does
@racketmodname[vestige/tracing/class/explicit] but named without the
``trace-'' prefix. For example @litchar{define/private} is actually
@racket[trace-define/private].

@subsection{Instrumenting specific functions or expressions}

@defmodule[vestige/tracing/explicit]

The @racketmodname[vestige/tracing/explicit] module provides
distinctly named forms. Use this when you want to instrument only some
functions or expressions in a module.

@defform[(trace-lambda [#:name name] kw-formals body ...+)]{

Like @|trace-lambda-id|.

This is the core form into which others expand.

The optional @racket[name] identifier is used for its symbol value as
well as a bearer of one or more special syntax properties. One such
property is the source location for the formals (the exact meaning of
which differs among the various forms that expand to
@racket[trace-lambda]) that appears as @racket['formals] in the
@racket[log-receiver-vector->hasheq] hash-table. When @racket[name]
is not supplied, the identifier is inferred using
@racket[syntax-local-infer-name] and the formals are the source
location for @racket[kw-formals].}

@defform[(trace-case-lambda [formals body ...+] ...+)]{

Like @racket[case-lambda] but expands to multiple
@racket[trace-lambda] forms, one for each clause, where each
@racket[#:name] comes from @racket[syntax-local-infer-name] and the
formals property is the location of each @racket[formals].}

@defform*[((trace-define id expr)
           (trace-define (head args) body ...+))]{

Like @|trace-define-id|.

The ``curried'' syntax --- e.g. @racket[(define ((f x) y) ____)] ---
expands to nested @racket[trace-lambda]s, each of which has a
@racket[#:name] identifier whose formals property covers that piece of
the syntax, and whose symbol is formed from the base name with the
formals appended; see @secref["curried-define-example"].}

@defform*[((trace-let proc-id ([id init-expr] ...) body ...+)
           (trace-let         ([id init-expr] ...) body ...+))]{

The first form is like @|trace-let-id| --- it instruments the function
implicitly defined and called by a ``named let''. It expands to a
@racket[trace-lambda] with a @racket[#:name] identifier whose formals
property covers @racket[id] and @racket[init-expr].

The second form defers to plain @racket[let].}

@defform[(trace-expression expression)]{

Equivalent to @racket[((trace-lambda #:name name () expression))],
where @racket[name] is synthesized from @racket[(syntax->datum
#'expression)] and gets a syntax property with the printed value of
@racket[expression], as well as a syntax property for the formals,
which is the entire @racket[expression].}

@subsection{Instrumenting specific @racketmodname[racket/class] method definitions}

@defmodule[vestige/tracing/class/explicit]

Provides the exports of @racketmodname[racket/class] plus tracing
variants of the method definitions forms:

@deftogether[(
  @defform*[((trace-define/private id expr)
             (trace-define/private (head args) body ...+))]

  @defform*[((trace-define/public id expr)
             (trace-define/public (head args) body ...+))]

  @defform*[((trace-define/pubment id expr)
             (trace-define/pubment (head args) body ...+))]

  @defform*[((trace-define/override id expr)
             (trace-define/override (head args) body ...+))]

  @defform*[((trace-define/overment id expr)
             (trace-define/overment (head args) body ...+))]

  @defform*[((trace-define/augride id expr)
             (trace-define/augride (head args) body ...+))]

  @defform*[((trace-define/augment id expr)
             (trace-define/augment (head args) body ...+))]

  @defform*[((trace-define/public-final id expr)
             (trace-define/public-final (head args) body ...+))]

  @defform*[((trace-define/override-final id expr)
             (trace-define/override-final (head args) body ...+))]

  @defform*[((trace-define/augment-final id expr)
             (trace-define/augment-final (head args) body ...+))] )]{}


@section{Logging}

@subsection{Depth and other information}

@defmodule[vestige/logging]

This module provides forms that use @tech/ref{continuation marks} to
associate extra information with the predefined
@racketkeywordfont{log-@italic{level}} forms like @racket[log-debug],
as well as the @racketkeywordfont{log-@italic{topic}-@italic{level}}
forms defined by @racket[define-logger]. (This also works with
@racket[log-message], provided its @racket[data] parameter is
@racket[current-continuation-marks], as is the case with the preceding
forms.)

A @tech/ref{log receiver} must know to look for this information.
Although the default log receiver created by Racket does not, it is
easy to create your own log receiver --- which you would do anyway if
you wanted to direct logging information to a destination like a local
or cloud logging database. See @racketmodname[vestige/receiving].

@defform[(with-more-logging-depth result-expr)]{Increases the depth
for all logging calls within the dynamic extent of
@racket[result-expr]. This allows for a grouping/indenting
presentation in a log receiver that knows how to retrieve the depth.

The default depth is zero. Each use of this form temporarily increases
the depth by one for the dynamic extent of the form.

When you use a @racket[vestige/tracing] module like
@racketmodname[vestige/tracing/explicit], the depth at any point is
the depth of the traced call(s). Other, ordinary logging is ``at that
depth'' automatically --- without using this form. For example a
@racket[log-info] in the body of a traced function is automatically at
the same depth as the tracing showing the function call. (You only
need use this form if you want to increase the depth even more.)

See also @racket[cms->logging-depth].}

@defform[(with-more-logging-info result-expr)]{Eagerly captures
information like @racket[current-inexact-milliseconds] and
@racket[current-thread] in a continuation mark. Capturing such
information eagerly matters because logger events are received later
and in a different thread; even if you were to use a custom log
receiver, it would be too late to add the information.

Also records the srcloc for the form, enabling a tool to show the
source of the logging.

See also @racket[cms->logging-info].}


@subsection{Application site}

@defmodule[vestige/app]

The @racketmodname[vestige/app] module provides an @racket[#%app] that
adds a continuation mark to record the source location of an
application.

Requiring this shadows the @racketidfont{#%app} provided by the module
language, for instance the @|#%app-id| of @racketmodname[racket/base].

In other words, @racket[(require vestige/app)] is a convenient way to
enable call-site information for @emph{calls from} that module, but by
itself does not instrument anything @emph{defined} in that module.

@defform[(#%app expr ...+)]{

Expands to @racket[(with-continuation-mark key loc (base-#%app expr
...))], where @racket[key] is a private value, @racket[loc] is
@racket[srcloc] of the application site, and @racket[base-#%app] is
the @|#%app-id| of @racketmodname[racket/base].

Using this is optional. Although it allows application site
information to be logged, it imposes some runtime overhead.

See also @racket[cms->caller-srcloc].}


@section{Log receiving}

@defmodule[vestige/receiving]

@subsection{Tracing logger values}

@deftogether[(
 @defthing[tracing-logger logger?]
 @defthing[tracing-topic symbol?]
 @defthing[tracing-level log-level/c]
)]{

The tracing logger, and the topic and level used for tracing logger
events.

You may need these values when making a log receiver and want to be
sure to receive logger events emitted by the @racket[vestige/tracing]
modules such as @racketmodname[vestige/tracing/explicit]. See
@secref["receiver-example"].}


@subsection{Event vectors}

@margin-note{Although most values are primitive strings and numbers,
some may be ``live'' Racket values such as thread descriptors, in case
your log receiver wants to do further processing. You may need to
apply @racket[serializable-hasheq] to this hash-table before giving it
hash-table to a function such as @racket[jsexpr->string].}

@subsubsection{High level}

@defproc[(log-receiver-vector->hasheq [v (vector/c log-level/c
                                                   string?
                                                   (or/c continuation-mark-set? any/c)
                                                   (or/c symbol? #f))])
         (and/c hash? hash-eq? immutable?)]{

Extracts information from a @tech/ref{log receiver} event vector.

When @racket[cms->tracing-data] produces a hasheq with a
@racket['message] mapping, substitutes that value for the original
message in the event vector.

Effectively this is a convenience function you could write yourself:

@racketblock[
(define (log-receiver-vector->hasheq v)
  (match v
    [(vector level message (? continuation-mark-set? cms) topic)
     (define tracing (cms->tracing-data cms))
     (hasheq 'message (or (and tracing (hash-ref tracing 'message #f))
                          message)
             'topic   topic
             'level   level
             'depth   (cms->logging-depth cms)
             'caller  (cms->caller-srcloc cms)
             'context (cms->context-srcloc cms)
             'info    (cms->logging-info cms)
             'tracing tracing)]
    [(vector level message _unknown-data topic)
     (hasheq 'message message
             'topic   topic
             'level   level
             'depth   0)]))
]}

@subsubsection{Low level}

Various forms from @racketmodname[vestige/logging] and
@racket[vestige/tracing] modules like
@racketmodname[vestige/tracing/explicit] add continuation marks. These
functions retrieve the mark values.

When values are hash-tables, keep in mind that it is possible that
additional mappings may be defined in the future; indeed this is one
reason for the choice of a @racket[hasheq] instead of a
@racket[struct]. For future compatibility, look for mappings you know
about and ignore others. For example use @racket[hash-ref] or use the
@racketmodname[racket/match] @racket[hash-table] match pattern.

Also with respect to source locations:

@nested[#:style 'inset
  @defthing[#:link-target? #f srcloc-as-list?
           (list/c (or/c path-string? #f)
                   line column position span)]{

  For ease of serialization, values that represent a @racket[srcloc]
  structure are instead represented as a list, where the first,
  ``source'' value is either a @racket[path] converted with
  @racket[path->string], or @racket[#f].}]

@defproc[(cms->logging-depth [cms continuation-mark-set?])
         (or/c #f exact-nonnegative-number?)]{

Returns the depth as set by @racket[with-more-logging-depth] and/or
the tracing forms.}

@defproc[(cms->caller-srcloc [cms continuation-mark-set?])
         (or/c #f srcloc-as-list?)]{

Returns the mark value set by @racket[#%app].}

@defproc[(cms->context-srcloc [cms continuation-mark-set?])
         (or/c #f srcloc-as-list?)]{

Returns the first non-false srcloc value, if any, from
@racket[continuation-mark-set->context] whose source is
@racket[complete-path?].}

@defproc[(cms->logging-info [cms continuation-mark-set?])
         (or/c #f (and/c hash? hash-eq? immutable?))]{

When a logger event is emitted in the dynamic extent of
@racket[with-more-logging-info] a mark can be retrieved by this
function. The value is a @racket[hasheq] with at least the following
mappings:

@nested[#:style 'inset

  @defmapping['srcloc srcloc-as-list?]{The source location of the
  @racket[with-more-logging-info] form.}

  @defmapping['msec real?]{The @racket[(current-inexact-milliseconds)]
  value at the time of logging.}

  @defmapping['thread thread?]{The @racket[(current-thread)] value at
  the time of logging.

  The @racket['thread] mapping values can be especially useful when
  you keep in mind that the @racket[object-name] of a Racket
  @tech/ref{thread descriptor} defaults to the name of its thunk
  procedure. You can even use @racket[procedure-rename] to give each
  thread thunk a unique name related to a ``job'' or ``request'', as
  discussed in
  @hyperlink["https://www.greghendershott.com/2018/11/thread-names.html"]{this
  blog post}.}

  @defmapping['performance-stats (vector/c vector? vector?)]{Vectors
  from @racket[vector-set-performance-stats!] for global stats and for
  @racket[current-thread]. For efficiency these are the ``raw''
  vectors. If you want a hash-table representation you can give these
  to @racket[performance-vectors->hasheq]. If you process the vectors
  yourself, be aware that the length of the vectors may increase in
  later versions of Racket.}]}

@defproc[(performance-vectors->hasheq [global vector?][thread vector?])
           (or/c #f (and/c hash? hash-eq? immutable?))]{

Given global and per-thread vectors from
@racket[vector-set-performance-stats!] or from
@racket[cms->logging-info], return a hasheq representation.}

@defproc[(cms->tracing-data [cms continuation-mark-set?])
         (or/c #f (and/c hash? hash-eq? immutable?))]{

When a logger event was emitted by @racket[vestige/tracing] modules
like @racketmodname[vestige/tracing/explicit], a mark can be retrieved
by this function. The value is a @racket[hasheq] with at least the
following mappings:

@nested[#:style 'inset

  @defmapping['call boolean?]{True when the event represents the
  evaluation of a function call or expression.

  False when the event represents a function returning results.}

  @defmapping['tail boolean?]{True when @racket[call] is true and this
  is a tail call.

  Note that results are not logged for tail calls.}

  @defmapping['name string?]{The name of the function or the datum of
  the expression.}

  @defmapping['identifier srcloc-as-list?]{The location of the
  identifier naming the function defintion or of the expression.}

  @defmapping['message string?]{The function name with the arguments
  in parentheses, the results, or the expression. Similar to the
  logger event vector's ``message'' slot string, but not prefixed by
  any @litchar{>} or @litchar{<} characters to show depth and call vs.
  return. Intended for a tool that will present this another way, such
  as using indentation for depth and other labels for call vs.
  return.}

  @defmapping['show-in-situ string?]{The arguments (only) to a call,
  or the results. Similar to the @racket[message] mapping, but what to
  show @italic{in situ} --- in place of source text at the
  @racket[formals] span or after the @racket[header] span. Intended
  for a tool that correlates tracing with source.}

  @defmapping['formals srcloc-as-list?]{The location of the formal
  parameters. What this means varies among the forms. The idea is that
  this is a good span to replace with the @racket[show-in-situ] value
  for a function call.}

  @defmapping['header srcloc-as-list?]{The location of the header.
  What this means varies among the forms. It is often a super-span of
  the @racket['formals] mapping. The idea is that this is a good span
  after which to show the @racket[show-in-situ] value for a function
  return.}]
}

@subsubsection{Serializing}

@defproc[(serializable-hasheq [h hash-eq?]) (and/c jsexpr? hash-eq? immutable?)]{

Given a hash-table --- such as one from
@racket[log-receiver-vector->hasheq], @racket[cms->logging-info], or
@racket[cms->tracing-data] --- returns one coerced to satisfy
@racket[jsexpr?].

For example, symbols are converted to strings, and thread descriptors
are converted to strings using @racket[object-name].

A hash-table satisfying @racket[jsexpr?] is obviously necessary to use
@racket[jsexpr->string], and more generally, is likely ready to
serialize/marshal using most other formats.}

@section{Examples}

@subsection{Example: Tracing}

@margin-note{Just for the sake of this documentation we've arranged
 for a logger receiver to pretty-print the logger data here.}

@margin-note{The source location values in these examples such as
 @racketresult[("eval" 2 0 2 1)] are a result of how these examples
 are evaluated to build this documentation. In real use, when the
 source is a file, @racketresult["eval"] would instead would be
 something like @racketresult["/path/to/file.rkt"].}

Here is a small example of what the structured logging data looks
like:

@ex/show[
  (require vestige/tracing/explicit
           vestige/logging)
  (define-logger example)

  (log-example-info "I am outside, my depth is 0")

  (trace-define (f x)
    (log-example-info "I am at the same depth as `f`: 2.")
    (with-more-logging-depth
      (log-example-info "I am one deeper: 3."))
    (+ 1 x))

  (trace-define (g x)
    (+ 1 (f x)))

  (g 42)

  (trace-expression (* 2 3))
]

The @racket[trace-define] forms cause log events for function calls
and results, with complete tracing information.

The first @racket[log-example-info] form has the same logging depth as
the call to @racket[f]. This happens automatically.

The second @racket[log-example-info] form is nested in a
@racket[with-more-logging-depth] form; as a result, its depth is one
greater. The use case here is for more detailed logging that a
receiver could show indented, folded, or hidden, depending on its user
interface options.


@subsection[#:tag "curried-define-example"]{Example: ``Curried'' define}

Here is how @racket[trace-define] handles so-called ``curried''
definitions, which expand into nested @racket[trace-lambda]s. Each
gets a name and a formalssource location that is distinct and
meaningful. (Otherwise the inner functions would get inferred names
like @racket[".../partial/path/to/foo.rkt:1:2"] and formals
locations.)

@margin-note{Although this package accommodates it, some people don't
like the ``curried'' style. Instead you can always replace it with
internal definitions of functions, in which case you control the
name.}

@ex/show[
  (require vestige/tracing/explicit)
  (trace-define ((f x0 x1) y0 y1)
    (+ x0 x1 y0 y1))
  ((f 1 2) 3 4)
]


@subsection[#:tag "receiver-example"]{Example: Making a log receiver thread}

The previous examples were printing some logger data because, in the
documentation environment, we arranged a simple @tech/ref{log
receiver} thread much like this:

@racketblock[
  (require vestige/receiving)
  (define receiver (make-log-receiver (current-logger)
                                      tracing-level tracing-topic
                                      'info 'example
                                      'fatal #f))
  (define (get-event)
    (pretty-print (log-receiver-vector->hasheq (sync receiver)))
    (get-event))
  (thread get-event)
]

@subsection{Using @racket[with-intercepted-logging] and JSON}

Another way to make a log receceiver is to use the
@racketmodname[racket/logging] convenience function
@racket[with-intercepted-logging], and supply it values from
@racketmodname[vestige/receiving]. The following example shows that.

Furthermore it shows using @racket[serializable-hasheq] and
@racket[jsexpr->string] to convert the hash-table to its JSON string
representation.

@ex/no-show[
  (require json
           racket/logging
           racket/match
           vestige/tracing/explicit
           vestige/receiving)
  (define (example)
    (trace-define (f x) (+ 1 x))
    (trace-define (g x) (+ 1 (f x)))
    (g 42))
  (define interceptor
    (compose displayln
             jsexpr->string
             serializable-hasheq
             log-receiver-vector->hasheq))
  (with-intercepted-logging interceptor example
    #:logger tracing-logger tracing-level tracing-topic)
]

Instead of @racket[displayln], this code could give the JSON string to
a function that sends it to AWS CloudWatch Logs or a similar
structured logging service.
