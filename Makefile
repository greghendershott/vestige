PACKAGE-NAMES=vestige-lib vestige-doc

DEPS-FLAGS=--check-pkg-deps --unused-pkg-deps

all: setup

# Primarily for use by CI.
# Installs dependencies as well as linking this as a package.
install:
	raco pkg install --auto --link $(PACKAGE-NAMES)

remove:
	raco pkg remove $(PACKAGE-NAMES)

# Primarily for day-to-day dev.
# Note: Also builds docs (if any) and checks deps.
setup:
	raco setup --tidy --avoid-main $(DEPS-FLAGS) --pkgs $(PACKAGE-NAMES)

# Note: Each collection's info.rkt can say what to clean, for example
# (define clean '("compiled" "doc" "doc/<collect>")) to clean
# generated docs, too.
clean:
	raco setup --fast-clean --pkgs $(PACKAGE-NAMES)

# Primarily for use by CI, after make install -- since that already
# does the equivalent of make setup, this tries to do as little as
# possible except checking deps.
check-deps:
	raco setup --no-docs $(DEPS-FLAGS) --pkgs $(PACKAGE-NAMES)

# Suitable for both day-to-day dev and CI
test:
	raco test -x -p $(PACKAGE-NAMES)
	raco test vestige-lib/vestige/example/example.rkt >/dev/null
# Note: example.rkt doesn't have rackunit tests. In fact it's
# included in `test-omit-paths` in vestige-lib/info.rkt because it has
# very noisy output. But it's a good smoke test to run it and make
# sure it doesn't raise any exceptions. So that's an extra step here.
