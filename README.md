[![CI](https://github.com/greghendershott/vestige/workflows/CI/badge.svg)](https://github.com/greghendershott/vestige/actions)

**This is not yet stable.**

This started as an alternative to `racket/trace`, with the idea of
emitting structured logger events that contain more data ---
especially source location information.

Eventually, I realized that some of the ideas could be generalized to
non-tracing logging.

So I shifted this from stuffing extra information in the `data` slot
of the logger event vector --- which normally is
`(current-continuation-marks)` --- to instead storing it in
continuation marks. That way plain old logging continues to work
normally. But a log receiver that knows about the continuation marks
can retrieve their values.

I've put a lot of work into the Scribble documentation, so feel free
to see that. But again, this is **not yet stable**. I have a `tracing`
branch of Racket Mode that `dynamic-require`s this and, if available,
uses it to greatly enhance the `racket-logger-mode`. I'm dogfooding
that indefinitely before even thinking of putting this on
https://pkgs.racket-lang.org.
