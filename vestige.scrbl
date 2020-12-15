#lang scribble/manual

@(require (for-label json
                     (except-in racket/base #%app)
                     racket/format
                     racket/logging
                     syntax/define
                     syntax/name
                     vestige/explicit
                     vestige/logger)
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

@title{Vestige}

@section{Introduction}

@margin-note{Because ``trace'' is already used by the
@hyperlink["https://pkgs.racket-lang.org/package/trace"]{trace} and
@hyperlink["https://pkgs.racket-lang.org/package/errortrace-lib"]{errortrace}
packages, as well as @racketmodname[racket/trace], this package is
named after a synonym, ``vestige''.}

This package captures information about function calls and expression
evaluations.

The information is supplied using structured logging.

You can minimize cross-cutting concerns.

Intended uses include both ``debugging'' and ``devops''.


@subsection{What it does differently from @racketmodname[racket/trace]}

This package mimics some of the forms of @racketmodname[racket/trace]
such as @racket[trace-define] and adds a @racket[trace-expression]
form.

Although the basic technique used to capture calls, results, and
levels is similar to that used in @racketmodname[racket/trace],
additional information is captured and its disposition is different:

@itemlist[

 @item{Instrumentation is enabled upon definition; there is no
 mutating @|trace-id| or @|untrace-id|.}

 @item{Information is emitted as @tech/ref{logger} events.}

 @item{Although each logger event vector has a simple ``message''
 string similar to @racketmodname[racket/trace], much more information
 is supplied in the logger event vector's ``value'' slot. The data is
 structured as a @racket[hasheq] that also satisfies @racket[jsexpr?].
 Justification: It is easy to serialize to other formats such as JSON
 or association lists, and, a @racket[hasheq] may be extended over
 time by adding new mappings without breaking existing consumers.}

 @item{As well as the arguments, results, and level captured by
 @racketmodname[racket/trace], more information is captured:

 @itemlist[

  @item{Useful information that should be captured eagerly, such as
  @racket[current-thread] and @racket[current-inexact-milliseconds],
  because logger events are received later and in another thread.}

  @item{@racket[srcloc] for various interesting things, when
  available.

   @itemlist[

    @item{The @emph{definition} of the function being called.}

    @item{The @emph{header} span within the definition. Tools can use
    this to present logs in a UI resembling a step debugger.}

    @item{The @emph{formals} span within the definition. Tools can use
    this to present logs in a UI resembling a step debugger.}

    @item{The @emph{caller} site. This information is only recorded
    for calls from modules where you use @racketmodname[vestige]'s
    @racket[#%app] to replace @racketmodname[racket/base]'s
    @|#%app-id|. Although useful for editing/debugging tools, this
    imposes some runtime overhead for all function calls in such a
    module.}

    @item{The @emph{context} surrounding the caller site, obtained
    from @racket[continuation-mark-set->context]. Although often
    imprecise, it imposes no overhead, and is sometimes adequate for
    ``devops'' purposes such as examining logs for a server (which
    traditionally lack any automatic caller context).}]}]}]

@subsection{Two main use cases: ``debugging'' and ``devops''}

The structured logs can be used in two main ways:

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

 The warm porridge: You can @racket[(require vestige)] to instrument
 everything in a module. Or if even that is too ``heavy'', you can
 @racket[(require vestige/explicit)] and instrument select items by
 replacing e.g. ``define'' with ``trace-define'' --- but not otherwise
 changing the source. As a rough analogy, this is like using Racket
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


@section{Instrumenting all functions in a module}

@defmodule[vestige]

The @racketmodname[vestige] module provides the same forms as does
@racketmodname[vestige/explicit], but named without the ``trace-''
prefix. For example @racket[trace-define] is provided renamed as
@racket[define]. As a result, requiring this module shadows those
definitions from the @racketmodname[racket/base] language.

In other words, @racket[(require vestige)] is a convenient way to
trace everything in a module without otherwise needing to litter its
source with individual changes.

At the same time, this module also provides @racket[trace-expression]
so that you may use that on specific expressions of special interest.


@section{Instrumenting specific functions or expressions}

@defmodule[vestige/explicit]

The @racketmodname[vestige/explicit] module provides distinctly named
forms. Use this when you want to instrument only some functions or
expressions in a module.

@defform[(trace-lambda [#:name name] kw-formals body ...+)]{

Like @|trace-lambda-id|.

This is the core form into which others expand.

The optional @racket[name] identifier is used for its symbol value as
well as a bearer of one or more special syntax properties. One such
property is the source location for the formals (the exact meaning of
which differs among the various forms that expand to
@racket[trace-lambda]) that appears as @racket['formals] in the
@secref["hash-table"]. When @racket[name] is not supplied, the
identifier is inferred using @racket[syntax-local-infer-name] and the
formals are the source location for @racket[kw-formals].}

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
formals appended; see the @secref["curried-define-example"] example.}

@defform*[((trace-let proc-id ([id init-expr] ...) body ...+)
           (trace-let         ([id init-expr] ...) body ...+))]{

The first form is like @|trace-let-id| -- it instruments the function
implicitly defined and called by a ``named let''. It expands to a
@racket[trace-lambda] with a @racket[#:name] identifier whose formals
property covers @racket[id] and @racket[init-expr].

The second form defers to plain @racket[let].}

@defform[(trace-expression expression)]{

Equivalent to @racket[((trace-lambda #:name name () expression))],
where @racket[name] is synthesized from @racket[(syntax->datum
#'expression)] and gets a syntax property with the printed value of
@racket[expression], as well as a syntax property for the formals,
which is the entire @racket[expression].

The rationale for expanding to @racket[trace-lambda] --- instead of
simply directly logging the @racket[expression] datum and the
resulting value --- is that the level (``call depth'') in relation to
other calls, as well as to nested uses of @racket[trace-expression],
will be available and correct for tools that use levels for indent or
other purposes.}

@defform[(#%app expr ...+)]{

Expands to @racket[(with-continuation-mark key loc (#%app expr ...))],
where @racket[key] is a private value, @racket[loc] is @racket[srcloc]
of the call site, and @|#%app-id| is that of
@racketmodname[racket/base].

Using this is optional. Although it allows call site information to be
logged, it imposes some runtime overhead.}


@section{Recording call sites}

@defmodule[vestige/app]

The @racketmodname[vestige/app] module provides only @racket[#%app],
which adds a continuation mark to record the source location of an
application.

Requiring this module shadows @racketmodname[racket/base]'s
@|#%app-id|.

In other words, @racket[(require vestige/app)] is a convenient way to
enable call-site information for @emph{calls from} that module, but
otherwise you don't want to instrument anything @emph{defined} in that
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


@section[#:tag "hash-table"]{Hash table mappings}

Each @tech/ref{logger} event vector's ``value'' slot is a
@racket[hasheq] that also satisfies @racket[jsexpr?]. The hash-table
has at least the following mappings.

@(define-syntax-rule (defmapping key type pre-contents ...)
   (defthing #:kind "mapping" #:link-target? #f key type pre-contents ...))

@defmapping['call boolean?]{True when the event represents the
evaluation of a function call or expression.}

@defmapping['tail boolean?]{True when @racket[call] is true and this
is a tail call.}

@defmapping['name string?]{The name of the function or the datum of
the expression.}

@defmapping['level exact-nonnegative-integer?]{The call depth.}

@defmapping['show string?]{The function with its arguments, the
expression, or the results. Similar to the logger event vector's
``message'' slot but not prefixed by any @litchar{>} or @litchar{<}
characters to show level.}

@defmapping['thread string?]{The name of the @tech/ref{thread
descriptor} for the currently executing thread: @racket[(~a
(object-name (current-thread)))]

The @racket['thread] mapping values can be especially useful when you
keep in mind that the @racket[object-name] of a Racket
@tech/ref{thread descriptor} defaults to the name of its thunk
procedure. You can even use @racket[procedure-rename] to give each
thread thunk a unique name related to a ``job'' or ``request'', as
discussed in
@hyperlink["https://www.greghendershott.com/2018/11/thread-names.html"]{this
blog post}.}

@defmapping['msec real?]{The time of the function call or result:
@racket[(current-inexact-milliseconds)].}

@defthing[#:link-target? #f srcloc-as-list? (list/c (or/c path-string? #f) line column position span)]{

For ease of serialization, the remaining mapping values use a
representation of a @racket[srcloc] struct as a list. The first,
``source'' value is either a @racket[path] converted with
@racket[path->string], or @racket[#f].}

@defmapping['definition srcloc-as-list?]{The location of the function
definition or expression.}

@defmapping['formals srcloc-as-list?]{The location of the formal
parameters. What this means varies among the forms. (The basic idea is
that a tool could show a function call or expression @italic{in situ}
at the location.)

For @racket[(trace-lambda (x y z) ____)] it covers the parameters
@litchar{x y z}.

For @racket[(trace-case-lambda [() ____][(x y z) ____] ____)] there is
a property for each of the two parameter lists, the first of which is
an empty span and the second of which is @litchar{x y z}.

For @racket[(trace-define (f x y z) ____)] it covers the parameters,
@litchar{x y z}. For ``curried'' define, each nested function header
has its own formals; see the @secref["curried-define-example"]
example.

For @racket[(let f ([x 1][y 2][z 3]) ____)], it covers the
parameter/initialization items, @litchar{[x 1][y 2][z 3]}.

For @racket[trace-expression] it is the entire expression.

In each case, the idea is that a tool could show something @italic{in
situ} in the style of a step debugger.}

@defmapping['header srcloc-as-list?]{The location of the header. What
this means varies among the forms. It is often a super-span of the
@racket['formals] mapping. The idea is that while @racket['formals] is
a good place to show actual arguments in place of formal parameters
when a function is called, the @racket['header] is a good place after
which to show the resulting value(s). The distinction matters for
things like @racket[trace-define], less so for things like
@racket[trace-lambda], and not at all for @racket[trace-expression]
(where both correspond to the entire expression.).}

@defmapping['caller (or/c #f srcloc-as-list?)]{The location of the
caller. This is only available when the call is from a module using
@racketmodname[vestige]'s @racket[#%app]. Otherwise the value will be
@racket[#f].}

@defmapping['context (or/c #f srcloc-as-list?)]{The location of the
context surrounding the call site. This can be @racket[#f] when
@racket[continuation-mark-set->context] reports no surrounding context
with a @racket[complete-path?] source.}


@section{Examples}

@(define-syntax-rule (ex eval pre-content ...)
   (examples #:eval eval
             #:no-prompt
             #:label #f
             #:preserve-source-locations
             pre-content ...))

@(define-syntax-rule (ex/show pre-content ...)
   (let ((e (make-base-eval)))
     (examples #:eval e #:hidden (require vestige/private/receiver))
     (ex e pre-content ...)))

@(define-syntax-rule (ex/no-show pre-content ...)
   (ex (make-base-eval) pre-content ...))

@subsection{Simple}

@margin-note{Just for the sake of this documentation we've arranged
 for a logger receiver to pretty-print the logger data here.}

@margin-note{The source location values in these examples such as
 @racketresult[("eval" 2 0 2 1)] are a result of how these examples
 are evaluated to build this documentation. In real use, when the
 source is a file, @racketresult["eval"] would instead would be
 something like @racketresult["/path/to/file.rkt"].}

Here is a small example of what the ``message'' and ``data'' slots of
logger event vectors look like:

@ex/show[
  (require vestige/explicit)
  (trace-define (f x) (+ 1 x))
  (trace-define (g x) (+ 1 (f x)))
  (g 42)
  (trace-expression (* 2 3))
]


@subsection[#:tag "curried-define-example"]{``Curried'' define}

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
  (require vestige/explicit)
  (trace-define ((f x0 x1) y0 y1)
    (+ x0 x1 y0 y1))
  ((f 1 2) 3 4)
]


@subsection{Converting logger events to JSON}

The previous examples were printing some logger data because, in the
documentation environment, we arranged a simple @tech/ref{log
receiver} thread much like this:

@racketblock[
  (require vestige/logger)
  (define receiver (make-log-receiver logger level))
  (define (receive)
    (pretty-print (match (sync receiver)
                    [(vector __level message data __topic)
                     (list message data)]))
    (receive))
  (void (thread receive))
]

Another way to make a log receceiver is to use the values from
@racketmodname[vestige/logger] and the @racketmodname[racket/logging]
convenience function @racket[with-intercepted-logging]. This example
extracts the @italic{data} member of the vector and converts that
@racket[hasheq] to JSON:

@ex/no-show[
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
    (match-lambda [(vector __level __message data __topic)
                   (displayln (jsexpr->string data))]))
  (with-intercepted-logging interceptor example
    #:logger logger level topic)
]

Instead of @racket[displayln], this code could give the JSON string to
a function that sends it to AWS CloudWatch Logs or a similar
structured logging service.
