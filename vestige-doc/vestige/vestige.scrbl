#lang scribble/manual

@(require (for-label json
                     (except-in racket/base #%app)
                     racket/contract
                     racket/class
                     racket/format
                     racket/logging
                     syntax/define
                     syntax/name
                     vestige/tracing
                     vestige/tracing/class
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

@(define-syntax-rule (defmapping key type pre-content ...)
   (defthing #:kind "mapping" #:link-target? #f key type pre-content ...))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@title{Vestige}

@margin-note{Because ``trace'' is already used so much ---
@hyperlink["https://pkgs.racket-lang.org/package/trace"]{trace},
@hyperlink["https://pkgs.racket-lang.org/package/errortrace-lib"]{errortrace},
@racketmodname[racket/trace] --- this package is named after a
synonym, ``vestige''.}

This package enhances logging generally, and, treats tracing as a
special case of logging.

@table-of-contents[]


@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@section{Tracing}

@subsection{Specific functions}

@defmodule[vestige/tracing]

@margin-note{See also @racket[log-expression].}

The @racketmodname[vestige/tracing] module provides distinctly named
forms. Use this when you want to instrument only some functions in a
module.

@defform[(trace-lambda [#:name name] kw-formals body ...+)]{

Like @|trace-lambda-id|.}

@defform[(trace-case-lambda [formals body ...+] ...+)]{

Like @racket[case-lambda] but expands to multiple
@racket[trace-lambda] forms, one for each clause, and each having
distinct source locations.}

@defform*[((trace-define id expr)
           (trace-define (head args) body ...+))]{

Like @|trace-define-id|.

The ``curried'' syntax --- e.g. @racket[(define ((f x) y) ____)] ---
expands to nested @racket[trace-lambda]s, each of which has distinct
source locations.}

@defform*[((trace-let proc-id ([id init-expr] ...) body ...+)
           (trace-let         ([id init-expr] ...) body ...+))]{

The first form is like @|trace-let-id| --- it instruments the function
implicitly defined and called by a ``named let''. The initial and
subsequnt calls have distinct source locations.

The second form defers to plain @racket[let].}


@subsection{All functions defined in a module}

@defmodule[vestige/tracing/implicit]

Provides the same forms as does @racketmodname[vestige/tracing], but
renamed without the @racketfont{trace-} prefix. For example
@racketfont{define} is actually @racket[trace-define]. As a result,
requiring this module shadows those definitions from the
@racketmodname[racket/base] language.

Also provides @racketmodname[vestige/app].

In other words, @racket[(require vestige/tracing/implicit)] is a
convenient way to trace everything in a module without otherwise
needing to litter its source with individual changes.


@subsection{Caller sites}

@defmodule[vestige/app]

Requiring @racket[(require vestige/app)] in a module records the
locations of @emph{calls from} that module. Then, when a module uses
@racketmodname[vestige/tracing] to trace @emph{calls to} a defined
function, tracing can check whether the direct caller was instrumented
and if so report its location.

@defform[(#%app proc-expr expr ...)]{

Adds a continuation mark with @racket[proc-expr] and the source
location of the entire application expression, then invokes the
@|#%app-id| of @racketmodname[racket/base].

Using this is optional. Although it enhances tracing by enabling
caller sites to be reported, it imposes some runtime overhead.}


@subsection{Specific @racketmodname[racket/class] method definitions}

@defmodule[vestige/tracing/class]

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


@subsection{All @racketmodname[racket/class] methods defined in a module}

@defmodule[vestige/tracing/class/implicit]

Provides the exports of @racketmodname[racket/class], except for the
method definition forms like @racket[define/private], for which it
substitutes the same forms as does
@racketmodname[vestige/tracing/class] but renamed without the
@racketfont{trace-} prefix. For example @racketfont{define/private} is
actually @racket[trace-define/private].


@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@section{Logging}

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

When you use a @racketmodname[vestige/tracing] module, the depth at
any point is the depth of the traced call(s). Other, ordinary logging
is ``under that depth'' automatically. For example a @racket[log-info]
in the body of a traced function is automatically at one more than the
depth as the tracing showing the function call. You only need use
@racket[with-more-logging-depth] if you want to increase the depth
even more.

See also @racket[cms->logging-depth].}

@defform[(with-more-logging-data result-expr)]{Eagerly captures
information like @racket[current-inexact-milliseconds] and
@racket[current-thread] in a continuation mark. Capturing such
information eagerly matters because logger events are received later
and in a different thread.

Also records the srcloc for the form, enabling a tool to show the
source of logging within the dynamic extent of this form..

See also @racket[cms->logging-data].}

@defform[(log-expression expr)]{Emits a logger event whose message
shows the quoted form of @racket[expr] and its result. The result of
@racket[expr] is the result of the @racket[log-expression] form.

Effectively a composition of @racket[with-more-logging-data] and a
@racket[log-message] using @racket[vestige-topic] and
@racket[vestige-level].}


@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@section{Receiving}

@defmodule[vestige/receiving]

@subsection{Logger topic and level}

@deftogether[(
 @defthing[vestige-topic symbol?]
 @defthing[vestige-level log-level/c]
)]{

The topic and level used for logger events emitted by
@racketmodname[vestige/tracing],
@racketmodname[vestige/tracing/class], and @racket[log-expression].

To receive those events you will need to supply these topic and level
values when making a log receiver. See @secref["receiver-example"].}


@subsubsection{High level}

@defproc[(log-receiver-vector->hasheq [v (vector/c log-level/c
                                                   string?
                                                   (or/c continuation-mark-set? any/c)
                                                   (or/c symbol? #f))])
         (and/c hash? hash-eq? immutable?)]{

Extracts information from a @tech/ref{log receiver} event vector.

When the vector's @racket[_data] member is a continuation mark set,
retrieves mark values set by various tracing and logging
instrumentation.

When @racket[cms->tracing-data] produces a hasheq with a
@racket['message] mapping, substitutes that value for the original
message from the event vector (which is formatted for plain old log
receivers).

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
             'context (cms->context-srcloc cms)
             'data    (cms->logging-data cms)
             'tracing tracing)]
    [(vector level message _unknown-data topic)
     (hasheq 'message message
             'topic   topic
             'level   level
             'depth   0)]))
]

Although most values in the main and child hash-tables are primitives
like strings and numbers, some may be ``live'' values such as thread
descriptors. This is intentional, so that your log receiver can use
them to obtain additional information if desired. On the other hand it
means you will need to apply @racket[serializable-hasheq] to this
hash-table before giving it to a function such as
@racket[jsexpr->string].}

@defproc[(add-presentation-sites [ht (and/c hash? hash-eq? immutable?)])
         (and/c hash? hash-eq? immutable?)]{

Given a hash-table produced by @racket[log-receiver-vector->hasheq],
returns one with @racket['primary-site] and @racket['secondary-site]
mappings. These can be used by an interactive tool to show the primary
and secondary sites, if any, associated with the logging event.

Although the original hash-table has all the necessary information,
the logic to translate that into a simple ``presentation action'' ---
what to show, where, and how --- is not necessarily immediate obvious.
This function is provided as a convenient, reasonable way to handle
this.

@nested[#:style 'inset

  @deftogether[(
    @defmapping['primary-site (or/c #f presentation)]
    @defmapping['secondary-site (or/c #f presentation)]
  )]{

  When the logging event originated from a
  @racketmodname[vestige/tracing] form: The primary site is a span
  within the formals or header of the traced, called function. The
  secondary site is the location of the caller (if available, else
  @racket[#f]).

  When the logging event originated in the dynamic extent of
  @racket[with-more-logging-data]: The primary site is the location of
  the @racket[with-more-logging-data] form. The secondary site is
  @racket[#f].

  Otherwise, both values will be @racket[#f].}

  @defthing[#:link-target? #f presentation
           (list* (or/c 'highlight 'replace 'after)
                  path-string?
                  exact-nonnegative-integer?
                  exact-nonnegative-integer?
                  (or/c (list)
                        (list string?)))]{

  For either site, the @racket[presentation] value is one of:

  @itemlist[

   @item{@racket[(list 'highlight _path _from _upto)]:

   Highlight the span [@racket[_from] @racket[_upto]) in
   @racket[_path].

   Used e.g. to show application with actual arguments at caller
   sites, or called functions with no formal parameters (thunks).}

   @item{@racket[(list 'replace _path _from _upto _str)]:

   Replace the span [@racket[_from] @racket[_upto]) in @racket[_path]
   with @racket[_str] and highlight that.

   Used e.g. for called functions, to replace their formal parameters
   with actual arguments.}

   @item{@racket[(list 'after _path _from _upto _str)]:

   Insert @racket[_str] at @racket[_upto] in @racket[_path] --- that
   is, insert after the span [@racket[_from] @racket[_upto]), leaving
   that intact. Highlight the inserted @racket[_str]. If you wish,
   also highlight the span [@racket[_from] @racket[_upto]).

   Used e.g. for function result values.}]

  In summary, when the user moves to a logging message in one buffer,
  you can make visible in one or more other buffers the primary and
  secondary sites, if any, using the suggested presentation action for
  each site.}]

}


@subsubsection{Low level}

Various forms from @racketmodname[vestige/logging] and
@racketmodname[vestige/tracing] add continuation marks. These
functions retrieve the mark values.

When values are hash-tables, keep in mind that it is possible that
additional mappings may be defined in the future; indeed this is one
reason for the choice of a @racket[hasheq] instead of a
@racket[struct]. For future compatibility, look for mappings you know
about and ignore others. For example use @racket[hash-ref] or use the
@racketmodname[racket/match] @racket[hash-table] match pattern.

@defthing[srcloc-as-list/c contract?
          #:value
          (list/c (or/c #f (and/c string? path-string?))
                  (or/c #f exact-positive-integer?)
                  (or/c #f exact-nonnegative-integer?)
                  (or/c #f exact-positive-integer?)
                  (or/c #f exact-nonnegative-integer?))]{

For ease of serialization, values that represent a @racket[srcloc]
structure are instead represented as a list, where the first,
``source'' value is either a @racket[path] converted with
@racket[path->string], or @racket[#f].}

@defproc[(cms->logging-depth [cms continuation-mark-set?])
         (or/c #f exact-nonnegative-number?)]{

Returns the depth as set by @racket[with-more-logging-depth] and/or
the tracing forms.}

@defproc[(cms->context-srcloc [cms continuation-mark-set?])
         (or/c #f srcloc-as-list/c)]{

Returns the first non-false srcloc value, if any, from
@racket[continuation-mark-set->context] whose source is
@racket[complete-path?].}

@defproc[(cms->logging-data [cms continuation-mark-set?])
         (or/c #f (and/c hash? hash-eq? immutable?))]{

When a logger event is emitted in the dynamic extent of
@racket[with-more-logging-data] a mark can be retrieved by this
function. The value is a @racket[hasheq] with at least the following
mappings:

@nested[#:style 'inset

  @defmapping['srcloc (or/c #f srcloc-as-list/c)]{The source location
  of the @racket[with-more-logging-data] form.

  Note: Internal uses of @racket[with-more-logging-data] by
  @racketmodname[vestige/tracing] forms set this value false, because
  the internal location is not relevant --- instead
  @racket[cms->tracing-data] supplies the interesting srclocs.

  See also the high level @racket[add-presentation-sites].}

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
@racket[cms->logging-data], return a hasheq representation.}

@defproc[(cms->tracing-data [cms continuation-mark-set?])
         (or/c #f (and/c hash? hash-eq? immutable?))]{

When a logger event was emitted by @racketmodname[vestige/tracing], a
mark can be retrieved by this function. The value is a @racket[hasheq]
with at least the following mappings:

@nested[#:style 'inset

  @defmapping['call boolean?]{True when the event represents the
  evaluation of a function call.

  False when the event represents a function returning results.}

  @defmapping['tail boolean?]{True when @racket['call] is true and
  this is a tail call.

  Note that function return results are not logged for tail calls.}

  @defmapping['name string?]{The name of the function.}

  @defmapping['identifier srcloc-as-list/c]{The location of the
  identifier naming the function defintion.}

  @defmapping['message string?]{When @racket['call] is true, the
  function name with the arguments in parentheses.

  When @racket['call] is false, the value(s) resulting from calling
  the function.

  Similar to the logger event vector's ``message'' slot string, but
  @italic{not} prefixed by any @litchar{>} or @litchar{<} characters
  to show depth and call vs. return. Intended for a tool that will
  present this another way, such as using indentation for depth and
  other labels for call vs. return.}

  @deftogether[(
    @defmapping['args-from (or/c #f exact-nonnegative-integer?)]
    @defmapping['args-upto (or/c #f exact-nonnegative-integer?)]
  )]{

  When @racket['call] is true, @racket[(substring message args-from
  args-upto)] is the string with the actual arguments to show
  @italic{in situ} at the @racket['formals] srcloc. Intended for a
  tool that correlates tracing with source. Note that these two values
  may be equal (indicating an empty string) when a function has no
  formal parameters (a ``thunk'').}

  @defmapping['formals srcloc-as-list/c]{The location of the formal
  parameters. What this means varies among the forms. The idea is that
  this is a good span to @italic{replace} with the actual arguments
  when @racket['call] is true. Note that this can be an empty span,
  when a function has no formal parameters (a ``thunk''); in that case
  it is probably best simply to highlight the text at the
  @racket['header] location to indicate the call.}

  @defmapping['header srcloc-as-list/c]{The location of the header.
  What this means varies among the forms. It is often a super-span of
  the @racket['formals] mapping. The idea is that the @racket['header]
  is a good span @italic{after which} to show the @racket['message]
  results when @racket['call] is false.}

  @defmapping['caller (or/c #f srcloc-as-list/c)]{The location of the
  directly calling function --- if this information is available from
  the use of @racketmodname[vestige/app] in module defining the
  calling function.}]

See also the high level @racket[add-presentation-sites].}

@subsubsection{Serializing}

@defproc[(serializable-hasheq [h hash-eq?]) (and/c jsexpr? hash-eq? immutable?)]{

Given a hash-table --- such as one from
@racket[log-receiver-vector->hasheq], @racket[cms->logging-data], or
@racket[cms->tracing-data] --- returns one coerced to satisfy
@racket[jsexpr?].

For example, symbols are converted to strings, and thread descriptors
are converted to strings using @racket[object-name].

The function is applied recursively (to values that are also
hash-tables, as well as to elements of lists and vectors).

A hash-table satisfying @racket[jsexpr?] is obviously necessary to use
@racket[jsexpr->string], and generally is more likely ready to
serialize/marshal using most other formats.}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@section{Examples}

@(define-syntax-rule (ex pre-content ...)
   (examples #:no-prompt
             #:label #f
             #:preserve-source-locations
             pre-content ...))

@subsection{Example: Tracing}

Here is a small example program:

@examples[#:no-result
(require vestige/app
         vestige/tracing
         vestige/logging)

(define-logger example)

(log-example-info "I am outside, my depth is 0")

(trace-define (f x)
  (log-example-info "I am automatically under the depth of `f`: 2.")
  (with-more-logging-depth
    (log-example-info "I am one deeper: 3."))
  (+ 1 x))

(trace-define (g x)
  (+ 1 (f x)))

(g 42)

(log-expression (* 2 3))
]

Here is the resulting structured logging data, as produced by
@racket[log-receiver-vector->hasheq] (but with some mappings removed
to make the output less overwhelming):

@(require racket/logging
          racket/pretty
          vestige/receiving
          vestige/app
          vestige/tracing
          vestige/logging)
@(verbatim
   (let ([out (open-output-string)])
     (with-intercepted-logging
       (λ (v)
         (newline out)
         (pretty-print (for/fold ([ht (log-receiver-vector->hasheq v)])
                                 ([k  (in-list '(info context))])
                         (hash-remove ht k))
                       out
                       1))
       (λ ()
         (define-logger example)

         (log-example-info "I am outside, my depth is 0")

         (trace-define (f x)
           (log-example-info "I am automatically under the depth of `f`: 2.")
           (with-more-logging-depth
             (log-example-info "I am one deeper: 3."))
           (+ 1 x))

         (trace-define (g x)
           (+ 1 (f x)))

         (g 42)

         (log-expression (* 2 3)))
       vestige-level vestige-topic
       'info         'example
       'fatal        #f)
     (get-output-string out)))

The @racket[trace-define] forms cause logger events for function calls
and results, with extra information under the @racket['tracing] key.

Also note the depths of the @racket[log-example-info] forms:

The first @racket[log-example-info] is not within any traced function
call or @racket[with-more-logging-depth] form, so its depth is the
default, 0.

The second @racket[log-example-info] is inside the call to @racket[f]
at depth 1, so automatically its depth is 2.

The third @racket[log-example-info] is within a
@racket[with-more-logging-depth] form; as a result, its depth is one
greater: 3. The use case here is for more detailed logging that a
receiver could show indented, folded, or hidden, depending on its user
interface options.


@subsection[#:tag "receiver-example"]{Example: Making a log receiver thread}

Although @racketmodname[racket/trace] prints output,
@racketmodname[vestige/tracing] does not --- instead it emits logger
events.

The previous example showed logger data because, here in the
documentation environment, we arranged a simple @tech/ref{log
receiver} thread somewhat like this:

@racketblock[
(require vestige/receiving)
(define receiver
  (make-log-receiver (current-logger)
                     (code:comment "A sequence of levels and topics...")
                     (code:comment "vestige")
                     vestige-level vestige-topic
                     (code:comment "(define-logger example) / log-example-info")
                     'info 'example
                     (code:comment "only fatal for all other topics")
                     'fatal #f))
(define (get-event)
  (pretty-print (log-receiver-vector->hasheq (sync receiver)))
  (get-event))
(thread get-event)
]

We ask @racket[make-log-receiver] to accept events from vestige's
level and topic, as well as from the @racket['info] level of the
@racket['example] topic (since our example used @racket[(define-logger
example)] and @racket[log-example-info]). Finally, we only want to see
@racket['fatal] level events for all other topics (which effectively
means we'll see almost nothing).


@subsection{Using @racket[with-intercepted-logging] and JSON}

Another way to make a log receceiver is to use the
@racketmodname[racket/logging] convenience function
@racket[with-intercepted-logging], and supplying it level and topic
values from @racketmodname[vestige/receiving]. The following example
shows that.

Furthermore it shows using @racket[serializable-hasheq] and
@racket[jsexpr->string] to convert the hash-table to its JSON string
representation.

@examples[#:no-prompt #:preserve-source-locations
(code:comment "The code being instrumented")
(module ex racket/base
  (require vestige/tracing)
  (define (example)
    (trace-define (f x) (+ 1 x))
    (trace-define (g x) (+ 1 (f x)))
    (g 42))
  (provide example))
(code:comment "Running the code and capturing logging")
(require 'ex
         json
         racket/logging
         vestige/receiving)
(with-intercepted-logging (compose displayln
                                   jsexpr->string
                                   serializable-hasheq
                                   log-receiver-vector->hasheq)
  example
  vestige-level vestige-topic)
]

Instead of @racket[displayln], this code could give the JSON string to
a function that sends it to AWS CloudWatch Logs or a similar
structured logging service.


@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@section{Discussion}


@subsection{Comparison with @racketmodname[racket/trace]}

The @racketmodname[vestige/tracing] module mimics some of the forms of
@racketmodname[racket/trace] such as @racket[trace-define], and adds a
@racket[trace-case-lambda].

Furthermore the @racketmodname[vestige/tracing/class] module adds
tracing flavors of @racketmodname[racket/class] method definitions.

More information is captured, and, its disposition is different:

@itemlist[

 @item{Instrumentation happens upon definition; there is no mutating
 @|trace-id| or @|untrace-id|.}

 @item{Information is emitted as @tech/ref{logger} events.}

 @item{Call depth is tracked through a continuation mark also used by
 @racket[with-more-logging-depth].}

 @item{As well as the arguments, results, and call depth reported by
 @racketmodname[racket/trace], more information is captured:

 @itemlist[

  @item{Timing and thread information is captured eagerly using
  @racket[with-more-logging-data].}

  @item{@racket[srcloc] for various interesting things, when
  available:

   @itemlist[

    @item{The @emph{definition} of the function being called.}

    @item{The @emph{header} and @emph{formals} spans within the
    definition. Tools can use this to present logs in a UI resembling
    a step debugger.}

    @item{Other srcloc information captured by
    @racketmodname[vestige/logging] like the context.}]}]}]



@subsection{Two main use cases: ``debugging'' and ``devops''}

Structured tracing logs can be used in two main ways:

@itemlist[

 @item{As a debugging technique, this approach is sometimes the ``warm
 bowl of porridge'' between simple @racket[print]s and a full
 step-debugger.

 @itemlist[

  @item{Manual @racket[print]s or @racket[log-debug]s tend to litter
  code, aren't so ``simple'' by the time you format various values and
  information, and the resulting output messages aren't always easy to
  correlate with their producing source.}

  @item{At the other extreme, full step-debugging can be ``heavy'', as
  a result of a special evaluator needing to rewrite your program into
  a ``step-debuggable'' program.}

  @item{The (possibly) warm porridge: You can @racket[(require
  vestige/tracing/implicit)] to instrument all functions defined in a
  module -- but not otherwise changing the source.

  Or if that is too ``heavy'', you can @racket[(require
  vestige/tracing)] and instrument select items by replacing e.g.
  ``define'' with ``trace-define'' --- but not otherwise changing the
  source.

  As a rough analogy, this is like using Racket
  @racketmodname[racket/contract] or @racketmodname[syntax/parse]:
  Although you do change the source, the change is minimal and feels
  more @italic{*waves hands*} ``declarative''.

  Then, for example, a tool such as
  @hyperlink["https://racket-mode.com"]{Racket Mode} can use the
  structured logs to provide a better experience --- such as
  navigating the ``tree'' of calls, jumping to a call site or function
  definition site, filtering items by thread, and so on. It can even
  show function call arguments @italic{in situ}, starting to resemble
  @italic{*waves hands*} a low-granularity ``time travel debugger''.}]}

 @item{As a devops technique, logging certain function calls is often
 something you want to do for a long-running server. But again, this
 is a cross-cutting concern; it can be tedious and distracting to
 litter code with such logging.

 You can minimize that with this package, as described in the previous
 bullet point. Furthermore, your program can use a @tech/ref{log
 receiver} to forward the information to logging systems that support
 a richer format than plain text, for example the JSON option of
 Amazon CloudWatch Logs.}]
