# clj-push3-play Project Summary

*State of `main`, 2026-09-28.*

## Overview

clj-push3-play is a Clojure implementation of the [Push3](https://faculty.hampshire.edu/lspector/push3-description.html) programming language. Push is a stack-based language designed as the target for program synthesis. The implementation reuses Clojush where that helps and is more data-oriented: instructions are entries in a table, and programs are plain edn.

On top of the interpreter, `main` has two bodies of work:

- **Propeller problem parity.** All 362 Propeller instructions have an equivalent (the instruction table grew from 253 to 525). 31 of the 34 Propeller/PSB problems are expressed as data and runnable. The missing three are `simple_classification`, `UBall5D` and `valiant`. See `docs/propeller-recook.md`.
- **Chaitin's metabiology on Push3.** Push programs are the software organisms. Fitness is a step-bounded busy beaver on an unbounded BIGNUM stack. Mutations are themselves Push programs (algorithmic mutation). The three regimes, exhaustive search, cumulative evolution and intelligent design, are measured against each other. See `docs/metabiology.md`.

Exploratory work (ARC solvers, HDC/VSA, relational CA, neurosymbolic, memetic algorithms) lives on other branches, listed in `Readme.org` under "Experimental work".

Downstream user: `~/repos/bjs-meme-constructor` (`plan-computing.md`) uses this repo as the machine for a constructor-theoretic treatment of computing and design.

## Key File Paths

### Interpreter (`src/benjamin_schwerdtner/clj_push3_play/`)
- `interpreter.clj`: `setup-state`, `execute`, `execute-1`. `*rng*` is bound once per `execute` call from `:push/random-seed`. `:push/num-executions` counts exec pops and is checked against `:max-executions`.
- `stack/`: push state and push values. `:push/integer` is clamped to ±1e10, BIGNUM is bounded by `:max-bignum-bits`.
- `instructions/`: the instruction table. `impl.clj` holds `register-instruction` and `instruction-table`, `interface.clj` requires every instruction namespace. Types: boolean, char, string, integer and float (`numbers.clj`), code, exec, name, vector, bignum, clj-object. Also:
  - `input_output.clj`: `:in1`..`:in8` push the inputs onto EXEC so they can be read repeatedly; the PRINT stack; `with-inputs`
  - `polymorphic.clj`: `_empty`, `_dup_times`, …
  - `mutation.clj`: the `:mut_*` instructions
  - `sandbox.clj`
- `generator.clj`: random code generation.
- `mutation/`:
  - `mut.clj`: mutation operators
  - `dual.clj`: dual evolution, where mutators are an evolved population
  - `parasites.clj`
- `configuration/`:
  - `config.clj`: `defaults`
  - `parser.clj`: reader for Push3 configuration files. Returns `{:parameters :types :instructions}` in the shape of `defaults`.
- `src/random_seed/`: seedable randomness.

### Propeller problems (`src/benjamin_schwerdtner/problems/propeller/`)
- `problems.clj`: the problems as data. Each one names its source in `propeller_problems/` under `:source`.
- `data.clj` (datasets), `alphabet.clj` (instruction alphabets per problem).
- `run.clj`: `evaluate`, `errors`, `total-error`, `n-solved`, `check-reference-solution`.
- `gp.clj`: a PushGP loop with UMAD, crossover and (epsilon-)lexicase and tournament selection (`run-gp`).
- `meta.clj`, `benchmark.clj`, `metabiology.clj`: metaevolution runs and Chaitin's regimes on the PSB suite.
- `propeller_problems/` (repo root): vendored Propeller source (EPL), used only as a reference. It is not on the classpath.

### Metabiology (`src/benjamin_schwerdtner/metabiology/`)
- `organism.clj`: organisms, fitness policies (busy beaver, frugal variants, parsimonious), `observe` with the `:resource-bounded` or `:halting-required` oracle.
- `mutation.clj`: algorithmic mutation (`mutate`), a two-stage size distribution that stands in for Chaitin's `2^-|M|`, and `mutation-distance`, an upper bound on `K(b|a)`.
- `evolution.clj`: `exhaustive-search`, `cumulative-evolution`, `intelligent-design`, `compare-regimes`, `growth-curve`, `cost-table`.
- `meta_regime.clj`: a meta-level regime.
- `src/benjamin_schwerdtner/problems/busy_beaver.clj`: Push3 busy beaver sketch.

### Docs, results, ideas
- `docs/propeller-recook.md`, `docs/metabiology.md`: the write-ups, with results.
- `results/`: the recorded runs the docs cite (`metabiology-*.edn`, `propeller-*.edn`).
- `ideas/`: `BIG_IDEAS.org`, `metaevolution.org`, `popperian-meta-evolution.org`, `autoconstructive.org`, `brain-development-is-software-synthesis.org`, `lit.org`.
- `Readme.org`: what is on `main` and where the experiments live. `Glossary.org`.
- `LLM_CODE_STYLE.md`, `CLAUDE.md`: code style.

### Tests (`test/`)
- `clj_push3_play/instructions_test.clj`: PSB-enabling instructions and arithmetic operand order ("second <op> top", fixed 2026-09-02).
- `clj_push3_play/configuration_parser_test.clj`: the configuration file reader.
- `problems/propeller_test.clj`: Propeller parity, including `alphabet-test` with the 362-instruction inventory.
- `metabiology/metabiology_test.clj`, `problems/propeller_metabiology_test.clj`.

## Dependencies

- Clojure CLI 1.12, JDK 27.
- `org.clojure/math.combinatorics`. The `:test` alias uses the cognitect test-runner, and the `:nrepl` alias is nREPL 1.3.
- No Python on `main`. `run.sh`, `activate.sh` and `dev.el` still assume a `.venv` from the experiment branches.

## Usage

```clojure
(require '[benjamin-schwerdtner.clj-push3-play.interpreter :as push])

(-> (push/execute (push/setup-state)
                  '(2 2 :exec_y (:integer_dup :integer_*))
                  {:max-executions 250})
    :stacks :push/integer)
;; => [2 10000000000]
```

Instructions are keywords such as `:integer_+` or `:code_car`, the Push3 name lower-cased with `.` replaced by `_`. Inputs go in with `io/with-inputs`, and the program reads them with `:in1`…

## Development Workflow

- REPL-driven: `clojure -M:nrepl`, then `(require '[ns] :reload)`.
- Tests: `clojure -J-Xmx<N>g -M:test`, one namespace with `-n <ns>`. Cap the heap explicitly, since the metabiology tests run evolution.
- Long runs write to `results/`, and the docs cite those files. Keep results and docs in sync. When shared code changes, re-derive the numbers. The PSB regime run did this and reproduced the committed 8-seed medians exactly.

## Implementation Patterns & Conventions

- Style from `LLM_CODE_STYLE.md`: small pure functions, `if` for single conditions, `cond` only for several branches, few `let` bindings, destructuring in parameters, `clojure.string` over interop.
- Comments use `;;`.
- Programs stay plain edn: there are no BIGNUM literals, and organisms lift values with `:bignum_frominteger`, so results files round-trip.
- Report saturation separately (`:saturated?`), so that hitting a substrate bound is never mistaken for open-ended growth.
- Comparisons between search regimes pay one currency (trials) and share one loop. Options such as `:observe`, `:alphabet` and `:saturation-terminal?` swap the landscape. Don't reimplement the loop.

## Known Leftovers

- The interpreter still sets up empty `:push/ca-rule` stacks, and `alphabet.clj` still mentions `:push/grid`, `:push/task` and `:push/ca-rule`. These come from the ARC/CA work on the experiment branches and are harmless on `main`.
