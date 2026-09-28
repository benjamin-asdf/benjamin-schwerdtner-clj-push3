# Propeller re-cook

The Propeller benchmark problems, re-expressed on this repository's own Push3
interpreter, and then attacked with the dual-evolution loop from
`ideas/metaevolution.org`.

`propeller_problems/` is a vendored copy of Lee Spector's Propeller (PushGP in
Clojure). It was sitting in the repository root unused. Two jobs:

1. re-cook the problems on our Push3, implementing whatever was missing;
2. try to solve them with metaevolution.

---

## Part 1 — What was missing

Our instruction table went from **253 to 525** instructions. All **362**
Propeller instructions now have an equivalent — the 343 of its instruction
set proper, plus the 19 the multiplier problem files define locally — pinned
by `propeller-test/alphabet-test`.

### The three real gaps

**Input instructions (`:in1` .. `:in8`)** —
`instructions/input_output.clj`. Our interpreter had no notion of a program's
input; the previous attempt at these problems
(`problems/propeller_adapter.clj`) pre-loaded the input onto a stack instead.
That is not a cosmetic difference. Pre-loaded, an input can be consumed
**once**: a program needing `x` twice must already have found `:integer_dup`,
and one that consumes it in the wrong order can never get it back. Most of the
PSB problems are unsolvable in that regime for reasons that have nothing to do
with the problem. `:inN` pushes onto EXEC, so one instruction serves every
input type and the value can be read as often as wanted.

**The PRINT stack** — `:push/print`, `<type>_print`, `:print_newline`. Push3
was specified for programs that leave their answer on a stack; PSB1/PSB2 are
specified the other way round — the answer is what the program *printed*.
`grade`, `smallest` and `number-io` read their answer from PRINT and could not
be expressed at all before. The accumulator starts at `""`, so "printed
nothing" is a scoreable value rather than a missing stack item.

**The vector stacks were never registered.** `instructions/vector.clj`
existed, complete, 253 lines — and no namespace required it, so all 185 of its
instructions were absent from the table. Adding one line to
`instructions/interface.clj` enabled every vector problem
(`count-odds`, `basement`, `fuel-cost`, `luhn`, `find-pair`).

### Smaller additions

- **Polymorphic operators** (`instructions/polymorphic.clj`): `_empty`,
  `_dup_times`, `_dup_items`, `_deep_dup`, across eleven types. `_empty` is
  the load-bearing one — without a way to ask whether a stack has anything on
  it, every consumer has to be written to survive being starved, and a loop
  over a vector has no termination condition.
- **Numeric**: `_>=`, `_<=`, `_inc`, `_dec`, `_quot`, `_fromchar`,
  `_fromstring` on both INTEGER and FLOAT.
- **Boolean**: `_xor`, `_invert_first_then_and`, `_invert_second_then_and`,
  and `_nand` / `_nor` / `_xnor` — NAND and NOR are each universal, so having
  them directly is the difference between a one-token gate and a two-token
  composition mutation has to keep adjacent.
- **String**: `_empty_string`, `_replace_first_char`.
- **Vector**: `_=`. Every other stack type had its own equality; the vector
  stacks did not, which left a program unable to ask whether two vectors it
  built were the same.

### Four bugs found on the way

These are behaviour changes to existing code, not additions.

**1. `integer_-`, `integer_mod`, `integer_div` had their operands reversed**
(`instructions/numbers.clj`). `(7 2 :integer_-)` returned **-5**. Push3
specifies "the second item minus the top item", and the functions' own
docstrings said the same, so the implementation contradicted its own stated
spec. `(7 2 :integer_mod)` returned 2 — i.e. `2 mod 7`. The float versions had
it too. The comparison generators were already correct, which is why this
survived: they name their arguments `[_state b a]` to make the reversal
visible, and these did not.

*This changes program semantics.* Anything searched or evolved under the old
behaviour — the ARC baseline synthesizer and the metabiology runs both have
`integer_mod` and `integer_-` in their alphabets — was found against a
different primitive. Recorded past results stand as records; re-runs will
differ.

**2. `handle-name` never put a name on the NAME stack**
(`interpreter.clj`). It read

```clojure
(get state :bindings identifier)
```

which looks up the key `:bindings` in the state *with the identifier as the
default value*. The bindings map is non-nil even when empty, so every
identifier took the "bound" branch and pushed the **bindings map itself** onto
EXEC. No name ever reached the NAME stack, which made every `*_define`
instruction unreachable. Now `(get (:bindings state) identifier)`, and
`(5 x :integer_define x x :integer_+)` evaluates to 10.

**3. `integer_max` `def`'d five vars on every call.** Leftover debug code
(`(def _state _state)` …) inside the instruction function, so every
`:integer_max` in every program in every generation interned vars.

**4. Random instructions leaked into problem alphabets.** Propeller's
`get-stack-instructions` excludes stochastic instructions unless `:random` is
requested; reproducing the stack sets without that rule handed
`integer-regression` — whose stack set is just `#{:exec :integer}` —
`:integer_rand`, and no run could fit a deterministic cubic. Worse, it
silently breaks the fitness memoisation in `mutation/dual.clj`, which keys on
the program and assumes the score is a function of it.

---

## Part 2 — The problems

31 problems in `problems/propeller/problems.clj`: PSB1 (4), PSB2 (19), the
self-contained ones Propeller defines without a dataset (6), and the two
boolean multipliers.

### The multipliers, and what they needed

`mul3` and `mul4` are the odd ones out and needed three things the rest did
not:

- **Named inputs.** They call their inputs `:a2 :a1 :a0 :b2 :b1 :b0` — the
  name says which bit it is. Any keyword present in the input map now
  resolves to its value (`interpreter/execute-1`), which is Propeller's
  `handle-input-instruction` design; it is checked after the instruction
  table so an input name can never shadow a real instruction.
- **Output registers.** `:set_c0`..`:set_c7` write the top BOOLEAN into a
  register, `:c0`..`:c7` read it back. A circuit has several independent
  outputs and the order a program computes them in is its own business;
  scoring the top six BOOLEAN items would constrain the *solution* rather
  than state the problem. Reading back matters too — the same partial
  products recur across the bits of a product, so `:cN` is how a program
  reuses one instead of rebuilding it. An unset register reads as `:unset`,
  never as `false`, because "never wrote it" and "deliberately zero" are
  exactly what the error function distinguishes.
- **Per-bit errors.** One error per output bit, so `mul3`'s 64 cases become
  **384 errors** and lexicase selects on individual bits. That is the point:
  a program that gets the low bit right on every input is worth keeping even
  though the rest of the product is wrong. There is no no-output penalty —
  Propeller's rule is "no answer same as wrong answer", one per missing bit.

Also added the gates they use: `boolean_nand`, `boolean_nor`, `boolean_xnor`
(Kalkreuth et al.'s recommended alphabet, which Propeller follows). `bufa`
and `nota` exist in `mul3.cljc` but are commented out of its instruction
list, so they are deliberately absent here too.

Their training set is **exhaustive** — every assignment of six or eight input
bits — and their test set is empty, matching Propeller. A circuit is correct
or it is not; there is nothing to hold out.

**Verified, and not yet solved.** The machinery is confirmed end-to-end: the
low bit of a product is always `a0 AND b0`, and the hand-written circuit
`(:a0 :b0 :boolean_and :set_c0)` scores exactly 64 of 384 bits — its bit,
every case. The empty program scores 0, confirming that an unset register is
charged as a wrong bit.

But search does **not** yet beat the trivial answer. Three seeds at
population 150 × 30 generations reach 251, 255, 255 of 384 bits — and a
circuit that just writes `false` to all six registers scores **273**, because
273 of the 384 expected bits are zero. So the GP is currently below the
all-zeros baseline. That is unsurprising at this budget for a problem
Propeller runs at population 1000 for 300 generations, but it is worth
stating plainly: `mul3` is wired up and measurable, not solved. The
all-zeros score is the number any future run has to beat first.

Instruction sets, ERCs, output stacks and error metrics are transcribed from
the Propeller sources and are exact. Reference solutions are written from the
published problem statements and checked against externally known values —
`fuel-cost` against the Advent of Code 2019 day 1 examples, `bowling` against
a perfect game (300), all-gutters (0), nine-and-a-miss (90) and a spare every
frame (150), `dice-game` against 15/36.

### The data is sampled, not the official dataset

Propeller calls `psb2/fetch-examples`, which downloads the datasets published
with the PSB2 paper. Those are not vendored here and fetching needs network
access, so inputs are **sampled from the published input specifications**
instead (`problems/propeller/data.clj`).

**Absolute success rates here are therefore not comparable to published PSB2
numbers.** The official training sets are curated — hand-chosen edge cases at
a frequency no i.i.d. sampler reproduces — and that curation is a large part of
what makes a PSB2 training set trainable. Each problem's known edge cases are
listed explicitly and always included in the training set, which is the same
trick, but it is not the same data. Comparisons *within* this repository, on
the same generator and seed, are unaffected.

### Two things found while transcribing

- **`small-or-large` is not a two-way split.** The PSB1 statement has a dead
  zone: `"small"` below 1000, `"large"` from 2000 up, `""` in between.
  `problems/propeller_adapter.clj` implemented it as small/large/equal about a
  single 1000 boundary — an easier, different problem.
- **`shopping-list` is unsolvable as Propeller configures it.** Its stack set
  is `#{:exec :integer :float :boolean :print}` but its inputs are two vectors
  of floats, so with no `:vector_float` in the alphabet a program cannot take
  them apart. Propeller's set is reproduced; `:stacks-corrected` carries the
  set that makes it solvable.

### What did not carry over

`close`, Propeller's genome marker, has no meaning here. A Propeller genome is
a flat `plushy` sequence and `close` is where a nesting level ends, so the
linear genome can encode a tree. Genomes here are already trees — where
Propeller tunes `close` frequency, this port tunes `:branch-probability`.

Instruction names differ (`:integer_+` not `:integer_add`). `alphabet/
propeller-name` maps them, for reading a published program into this
interpreter. Two instructions are named the same in both systems and mean
different things — `:string_contains` and `:string_replace` differ in operand
order — and are listed in `alphabet/semantics-differ`. Those were left alone:
nothing in Push3 makes either convention correct, and a search process only
needs the semantics to be consistent. The arithmetic operators were a
different case, and were fixed.

---

### Epsilon lexicase

Plain lexicase filters on **exact** equality with the best error on a case.
On integers and strings that is right — exact ties are the normal case. On
continuous errors it is not merely suboptimal, it is **degenerate**: two
programs essentially never have bit-identical floating point errors, so the
first case filters the pool to a single individual and every later case
exerts no pressure at all.

Measured on `nguyen-f1`, a population of 60 random programs:

| | survivors after case 1 | after cases 2-6 |
|---|---|---|
| plain lexicase | **3** | 3, 3, 3, 3, 3 |
| epsilon lexicase | **30** | 30, 30, 30, 30, 30 |

(3 rather than 1 because the programs producing no output at all share the
no-output penalty exactly and so tie with each other — plain lexicase on this
problem was selecting at random among the programs that output nothing.)

`epsilon-lexicase-select` keeps everything within `epsilon` of the best,
where epsilon is that case's **median absolute deviation** across the
population (La Cava et al. 2016) — MAD rather than a fixed tolerance because
the right tolerance depends on how spread out the population currently is,
which changes as a run converges. Epsilons are computed once per generation.

The six continuous-error problems — `bouncing-balls`, `dice-game`,
`shopping-list`, `snow-day`, `nguyen-f1`, `nguyen-f5` — now carry
`:selection :epsilon-lexicase` in their own maps, which beats `gp/defaults`
and loses to an explicit override so the control is still runnable. Propeller
sets `:parent-selection` per problem file for the same reason.

This does **not** affect the metaevolution results below: all six problems in
that experiment have integer, string or print output, where exact ties are
the normal case.

#### Does it help? Not measurably yet

`results/propeller-epsilon.edn` — 3 continuous-error problems × 3 seeds ×
{lexicase, epsilon-lexicase}, pop 150 × 40 generations, 50 cases. Total
training error (lower better):

| problem | lexicase | epsilon-lexicase |
|---|---|---|
| nguyen-f1 | 140.5 / **3.0e-12** / 612.2 | 500.9 / 34.8 / 99.2 |
| nguyen-f5 | 7.65 / 7.16 / 6.04 | 7.65 / 7.16 / 7.01 |
| dice-game | 11.48 / 13.29 / 13.70 | 11.48 / 13.13 / 13.70 |

**Epsilon-lexicase did not win.** On `nguyen-f1` plain lexicase found a
near-exact solution on seed 2 (3e-12, 44 of 50 cases exact) that epsilon
never matched; `nguyen-f5` and `dice-game` are effectively ties.

Read this as underpowered, not as evidence against. The seed-to-seed spread
on nguyen-f1 under plain lexicase is 140 / 3e-12 / 612 — orders of magnitude
larger than any difference between the two conditions. It is the same warning
`dual.clj` records about the busy-beaver landscape: run-to-run variance
exceeds the effect, so three seeds cannot answer this. Propeller runs these
at population 1000 for 300 generations.

Two things were checked so the null result is not just a broken
implementation:

- the epsilons are on a **sensible scale** — median 2.43, range 1.19–5.0,
  against a median real error of 7.9 on the same population. They are not
  being inflated by the 1e6 no-output penalty, which was the obvious failure
  mode (39% of that population's errors were at the penalty and the median
  still landed in real-error territory);
- the pool-collapse fix is real and measured, per the table above.

There is also a mechanism that plausibly explains the null: once a program is
*exactly* right, errors do hit exact zero, and then plain lexicase's
exact-tie filtering is precisely the right thing. Epsilon lexicase helps
while the population is still approximate and stops helping — or mildly
hurts, by keeping near-misses alive — once someone lands the answer. That
would predict exactly the `nguyen-f1` seed-2 result, and it is testable at a
real budget.

## Part 3 — Metaevolution

`mutation/dual.clj` ends by admitting its own landscape cannot test it:

> So: this landscape cannot answer whether meta evolution pays. It saturates,
> and it is stationary — and a STATIONARY landscape is precisely the case where
> evolvability has nothing to buy you. […] Testing this loop needs a landscape
> with that shape.

Program synthesis under **downsampling** has that shape, and gets it honestly.
Each generation scores the population against a fresh random subset of the
training cases (`problems/propeller/meta.clj`), so the landscape moves every
generation — and it moves the way an environment does, by changing which
competences are currently tested, not by perturbing a scalar. It does not
saturate: `fizz-buzz` has no ceiling short of a correct program, where "name
the largest integer" ran out at the INTEGER clamp. And it is the standard way
to run PushGP, so the moving landscape costs nothing in realism.

### Three conditions

| condition | what it is |
|---|---|
| `:meta` | M evolves (`:m-replace-frac 0.3`) |
| `:frozen` | M fixed, still competing for events (`:m-replace-frac 0`) |
| `:gp` | no M — lexicase + UMAD, `problems/propeller/gp.clj` |

`:frozen` is the one that matters. It separates "evolving the mutation
operators helped" from "having a bag of twenty hand-written operators helped",
and in the busy-beaver runs it was enough to erase the effect.

The GP's population is sized to `e-size + events × n-variations` so all three
conditions spend approximately the same number of program executions. Equal
generations is not equal compute.

### What to read

`dual.clj` selects M on the improvement rate it induces, and the standing
finding in this repository is that **improvement-event count is decoupled from
objective progress** — a change can lift M's credit metric while the held-out
benchmark stays flat, and one run reached a better benchmark with *half* the
improvement events. So:

- **`:benchmark`** — total error on the full training set, over the whole
  population, never used in selection. The only number that means progress.
- **`:train-solved` / `:test-solved`** — cases solved exactly, which is what
  the suite actually asks for.
- **not `:improvements`**, reported only to show whether M's credit signal had
  any density at all.

### Results

`results/propeller-meta.edn` — 6 problems × 3 seeds × 3 conditions, 40
generations, 39 minutes. Mean training cases solved exactly, of 100
(test, of 200, in brackets):

| problem | `:meta` | `:frozen` | `:gp` |
|---|---|---|---|
| small-or-large | 50.0 (94) | 50.0 (95) | **76.3 (163)** |
| fizz-buzz | 32.3 (63) | 28.0 (55) | **51.0 (120)** |
| count-odds | 12.7 (19) | 13.3 (19) | **27.3 (43)** |
| smallest | 16.0 (35) | 23.3 (37) | **43.0 (72)** |
| gcd | 51.0 (92) | 61.3 (125) | **62.0 (125)** |
| fuel-cost | 0.0 (0) | 0.0 (0) | 2.0 (0) |

Per-run, which is what matters given how large the seed-to-seed spread is:

- **`:gp` vs the better of the two dual conditions: 15 wins, 3 ties, 0
  losses.** Lexicase + UMAD never loses at matched evaluation budget, and
  usually wins by close to a factor of two.
- **`:meta` vs `:frozen`: 2 wins, 4 losses, 12 ties.** Evolving the mutation
  operators does not pay on this landscape. It is not a null effect in both
  directions either — the two clear separations went in opposite ways
  (fizz-buzz seed 2: 41 vs 28 for meta; smallest seed 1: 1 vs 14 against it).

Nothing solved a problem outright. `fuel-cost` was solved by nobody: the GP's
2/100 is memorisation and generalises to 0/200.

### Why dual loses, which is the useful part

The result is not "metaevolution does not work" — it is that **E's selection
method dominates everything M does.**

`dual.clj` selects E on a scalar: `fitness` is the summed error over the case
subset and survival is truncation on that sum. That discards exactly the
information lexicase selection runs on. A program that is uniquely correct on
the FizzBuzz-divisible-by-15 cases and hopeless elsewhere has a bad sum, so
truncation deletes it — and with it the only partial solution in the
population. Lexicase keeps it, because it is best on at least one case.

To test that rather than assert it, the baseline was re-run with **scalar
tournament selection** instead of lexicase and nothing else changed — same
budget, same landscape, same UMAD, same seeds
(`results/propeller-tournament.edn`). Mean training cases solved, of 100, over
the same 18 runs:

| | mean solved | head-to-head |
|---|---|---|
| GP, lexicase | **43.6** | 9 wins, 9 ties, 0 losses vs tournament |
| GP, scalar tournament | **32.5** | 8 wins, 9 ties, 1 loss vs dual |
| dual evolution (better of meta/frozen) | **30.2** | |

Swapping the selection method alone accounts for **11.1 of the 13.4-point gap,
about 83%**. And in **9 of the 18 runs the scalar GP and dual evolution return
identical numbers** — all three gcd seeds, all three count-odds seeds,
small-or-large seed 2, smallest seed 2. Once E's selection is scalar, two
quite different search algorithms land in exactly the same place. Whatever the
evolved mutation operators contribute is below the resolution of this
benchmark.

Two things follow:

1. The next experiment is **lexicase selection for E inside `dual.clj`** —
   pass the error vector through instead of the sum. Until then, `:meta` vs
   `:gp` is not a comparison about mutation at all; it is scalar-vs-lexicase
   with the mutation operators along for the ride.
2. The `:meta` vs `:frozen` result (2-4-12) is unaffected, because both share
   E's selection. On that axis the answer is clean and negative.

This extends the standing finding recorded in the `dual-evolution-loop`
notes — M's credit metric is decoupled from objective progress — with a
sharper version: on a landscape with a real gradient, M's composition is not
the binding constraint, and what binds is one level down, in how E is selected.

Still confounded, and worth knowing before running more of these: dual's E
population is 30 against the GP's 150 at equal evaluation count, because dual
spends most of its budget on mutation events rather than on population size.
Selection method and population size are not separated from each other by this
experiment — only from the mutation operators.

---

## Part 4 — Metabiology: Chaitin's three regimes on program synthesis

`metabiology/evolution.clj` compares three search regimes paying one currency,
trials:

    exhaustive search      enumerate programs shortest first
    cumulative evolution   random algorithmic mutation, keep if fitter
    intelligent design     inspect a pool of 32, take the best child

Its landscape is "name the largest integer", which saturates — once an organism
hits the bit budget nothing can be strictly fitter and the run is over. The
metabiology notes are explicit that this is a statement about the substrate,
not about evolution, and that testing the regimes properly needs a landscape
that does not run out.

The PSB benchmarks are the same shape with the ceiling made meaningful:

| busy beaver | program synthesis |
|---|---|
| bits named | training cases solved exactly |
| `saturated?` | every case solved — the benchmark is beaten |
| bit budget | the number of cases |
| program points | unchanged, still what frugality prices |

### The regimes are reused, not reimplemented

`evolution.clj` now takes `:observe`, `:alphabet` and `:saturation-terminal?`
as options, so `cumulative-evolution` here is the *same function*, the same
loop and the same trials accounting as the one that produced
`results/metabiology-regimes.edn`. That is the only way the two sets of numbers
mean the same thing. Verified: recomputing metabiology's own 8-seed medians
after the change reproduces the committed values exactly — CE median-bits
10799 / median-accepted 29/2 / saturated 2, ID 2555/2 / 15 / 1.

Algorithmic mutation carries over too, and it is the substantive import.
`mutation/mutate` applies an evolved **Push program** to the genome, so mutation
distance is program-size complexity rather than Hamming distance —
`:mut_duplicate` doubles a genome in one step, which point mutation reaches with
probability 2^-k. That is a genuinely different variation operator from the
UMAD `gp.clj` uses.

### Two bugs this exposed in the shared metabiology code

**`:mb_rand_atom` / `:mb_rand_code` ignored the alphabet.** Both closed over
`organism/atom-generators` directly rather than reading the state's
`:instructions`, which `mut/setup` sets from `:alphabet`. So every genome the
regimes grew was built from BIGNUM instructions whatever landscape they had
been pointed at. Invisible in the busy-beaver runs — there that *is* the right
alphabet — and total everywhere else: the inserted material could never do
anything, so nothing was ever accepted. The two probes were returning
byte-identical children for completely different alphabets, which is what gave
it away.

**The regimes and the GP were not drawing from the same alphabet.**
`problems/alphabet` mixes three kinds of thing: instruction keywords, literal
ERCs, and ERC *generators* — `random-int` is a function, not a value. The GP
never sees that difference, because `problems/atom-generators` calls a
generator to produce a fresh constant each time it draws one. The regimes take
the alphabet vector directly, so `:mb_rand_atom` spliced the **function object
itself** into genomes: inert when executed, and one fewer usable constant than
the GP had.

That is not cosmetic. It gave the population-based condition access to random
integer constants the three single-organism regimes could not reach, on
exactly the comparison being made — and the whole headline is that the GP
wins. Found because the results file stopped being readable: `pr-str` of a
program containing a raw function emits `#object[...]`, which `read-string`
refuses. `search-alphabet` now samples each generator into concrete values,
and the first benchmark run was discarded and redone.

**`intelligent-design` picked its winner with `max-key`**, which requires
numbers. The `:parsimonious` policies compare `[score -points]` vectors, so the
one regime most prone to bloat could not be run under the policies written to
control it — `ClassCastException`. Now a `reduce` with `organism/fitter?`,
tie-breaking deliberately matched to `max-key` (ties go to the later pool
member) so the recorded busy-beaver numbers still reproduce.

### Frugality does not transfer, and the reason is instructive

The metabiology finding was that ratio frugality (`named/points`) buys leanness
without freezing the climb, because the numerator is a tower of exponentials
and dominates any plausible program size — the cost term only separates
organisms naming the *same* number.

Here the numerator is a **bounded case count**. A 4-solver of 100 points scores
0.04; a 3-solver of 3 points scores 1.0. The denominator dominates, so
`:frugal-size` becomes a strong proportional penalty — precisely the shape the
metabiology notes identify as freezing a search. `:parsimonious` (lexicographic)
is the policy that keeps the intended meaning on a bounded score.

The general lesson: a fitness policy stated as a ratio encodes an assumption
about the *scale* of its numerator, and that assumption travels silently.

### `:solved` is too flat to be the default

`:solved` is the faithful analogue of busy-beaver fitness, and it is unusable
as a default: an exact solve is rare, so the landscape is flat almost
everywhere. Measured on `small-or-large`, `:solved` accepted **0** mutations in
20 000 trials, where `:neg-error` accepted 2 and reached 50/100 training cases
from a 5-point program. Kept as a condition rather than removed — the flatness
is a finding about the landscape, not a bug.

That is also why the benchmark's default budget is 20 000 trials: the
metabiology notes warn that this landscape has a takeoff between 4000 and
15 000 and that "at 4000 trials every frugal policy looks like total failure".

### The benchmark

`problems/propeller/benchmark.clj` runs all 31 problems under four conditions
at a matched budget — one trial is one program evaluated against the case set,
so the GP's population x generations equals the regimes' trials, and every
condition sees the same fixed 25-case set with no downsampling.

**What it cannot show**: the three regimes are single-organism hill-climbers,
i.e. the extreme of scalar selection, and the previous experiment here found
selection to be the dominant variable (~83% of the GP's advantage over dual
evolution came from lexicase vs scalar tournament). So a regime-vs-regime
difference is about search; a regime-vs-GP difference confounds *has a
population* with *selects per case*.

### Results

`results/propeller-benchmark.edn` — all **31 problems**, 3 seeds, 4 conditions,
20 000 trials each, 25 fixed cases, `:neg-error` fitness. 372 rows, 0 errors.

Head to head, counted **per run** rather than averaged, because the
seed-to-seed spread on these problems is larger than most differences between
conditions:

| comparison | wins / ties / losses |
|---|---|
| gp vs cumulative evolution | **72 / 21 / 0** |
| gp vs exhaustive search | **76 / 17 / 0** |
| cumulative evolution vs intelligent design | 1 / **91** / 1 |
| cumulative evolution vs exhaustive search | 22 / 58 / 13 |

Mean training cases solved (of 100; `mul3`/`mul4` are of 384 and 2048 output
bits), worst conditions omitted where all four are zero:

| problem | exhaustive | cum. evo | int. design | gp |
|---|---|---|---|---|
| small-or-large | 16.7 | 33.3 | 33.3 | **89.3** |
| smallest | 30.3 | 0.0 | 0.0 | **86.7** |
| string-classification | 80.0 | 80.0 | 80.0 | **81.7** |
| solve-boolean | 56.3 | 56.3 | 56.3 | **80.3** |
| basement | 46.0 | 46.0 | 46.0 | **76.0** |
| fizz-buzz | 18.3 | 18.3 | 18.3 | **74.7** |
| gcd | 35.3 | 35.3 | 35.3 | **63.0** |
| count-odds | 9.3 | 11.0 | 11.0 | **35.3** |
| number-io | 2.0 | 1.0 | 1.0 | **34.0** |
| nguyen-f1 | 0.0 | 0.7 | 0.3 | **30.0** |
| middle-character | 0.7 | 0.0 | 0.0 | **28.3** |
| twitter | 28.0 | 28.0 | 28.0 | 28.0 |
| grade | 21.3 | 21.0 | 21.0 | **26.0** |
| camel-case | 22.7 | 22.7 | 22.7 | 22.7 |
| spin-words | 22.3 | 22.3 | 22.3 | 22.3 |
| scrabble-score | 3.3 | 2.7 | 2.7 | **18.3** |
| find-pair | 0.3 | 0.3 | 0.3 | **12.3** |
| paired-digits | 1.7 | 4.0 | 4.0 | **11.7** |
| luhn | 1.7 | 3.3 | 3.3 | **8.0** |
| integer-regression | 0.0 | 3.0 | 3.0 | **7.7** |
| substitution-cipher | 3.0 | 3.0 | 3.0 | **4.7** |
| bouncing-balls | 1.0 | 0.3 | 0.3 | **4.3** |
| snow-day | 0.3 | 0.0 | 0.0 | **3.7** |
| bowling | 0.0 | 1.3 | 1.3 | **3.3** |
| square-digits | 1.0 | 1.0 | 1.0 | **2.7** |
| fuel-cost | 0.0 | 0.0 | 0.0 | **2.3** |
| nguyen-f5 | 0.0 | 0.0 | 0.0 | **1.0** |
| shopping-list | 0.0 | 0.0 | 0.0 | 0.7 |
| dice-game | 0.0 | 0.0 | 0.0 | 0.0 |
| mul3 (of 384 bits) | 38.0 | 219.7 | 221.7 | **255.7** |
| mul4 (of 2048 bits) | 160.0 | 893.3 | 893.3 | **1276.7** |

### What this says

**The population is doing the work, not the search regime.** The GP loses no
run to any regime — 72-0 against cumulative evolution, 76-0 against
enumeration — and the three regimes are barely distinguishable from each
other. Cumulative evolution and intelligent design returned the *same number
on 91 of 93 runs*, and cumulative evolution against brute-force enumeration is
22-58-13, i.e. a wash.

**Chaitin's rate separation does not reproduce here.** That is the headline.
The whole content of metabiology's comparison is that cumulative evolution
sits strictly between blind search and design and much closer to design —
exhaustive ~2^N, evolution ~N^2, designer ~N. On this landscape all three
collapse together, and `camel-case`, `spin-words` and `twitter` show why in
the cleanest possible form: **all four conditions score exactly the same**.
Everything finds one short, trivially-reachable program and nothing gets past
it. There is no rate to separate.

The busy-beaver landscape has a property these do not: every improvement is
reachable by a *small algorithmic step* from the last one, which is precisely
what makes cumulative evolution quadratic rather than exponential —
`:bignum_pow` applied to a good organism yields a better organism. On a PSB
problem the next improvement is usually structural — add a conditional, wrap a
loop — and a single-organism hill-climber cannot hold the intermediate,
temporarily-worse forms on the way there. A population can, and lexicase
selection is what keeps those intermediates alive because it rewards being
best on *one case* rather than best on average.

So: the regimes ported cleanly and reproduce metabiology's own numbers
exactly, and the mathematics they were built to demonstrate turns out to be a
property of that landscape rather than of the regimes. That is a more useful
result than a confirmation would have been.

Two smaller observations worth keeping:

- **Enumeration beats evolution where the answer is short and structured.**
  `smallest` is the sharp case: exhaustive gets 30.3, both mutation regimes get
  **0.0**, the GP 86.7. Shortest-first enumeration walks straight into the
  small program that prints one of its inputs; a hill-climber from `()` has no
  gradient toward it at all, because nothing shorter is any better.
- **The multipliers are the one place the regimes clearly beat enumeration**
  (mul3: 220 vs 38; mul4: 893 vs 160) — and all of them remain below the
  all-zeros baseline of 273/384. Per-bit errors give a dense gradient, which is
  exactly the condition under which hill-climbing has something to climb.

**The confound, restated**: regime-vs-GP mixes *has a population* with
*selects per case*. Regime-vs-regime does not, and that comparison is a
three-way tie.
---

## Running it

```clojure
(require '[benjamin-schwerdtner.problems.propeller.problems :as p]
         '[benjamin-schwerdtner.problems.propeller.run :as run]
         '[benjamin-schwerdtner.problems.propeller.gp :as gp]
         '[benjamin-schwerdtner.problems.propeller.meta :as meta])

;; What a problem looks like
(keys p/by-name)
(count (p/alphabet (p/by-name :fizz-buzz)))

;; A hand-written solution, to check the machinery
(let [problem (p/by-name :smallest)
      {:keys [train]} (p/train-and-test problem 30 5 7)]
  (run/n-solved problem
                '(:in1 :in2 :integer_min :in3 :integer_min
                  :in4 :integer_min :integer_print)
                train))
;; => 30

;; Baseline GP
(gp/run-gp (p/by-name :integer-regression)
           {:population-size 300 :max-generations 40 :n-train 21 :n-test 21})

;; Metaevolution, and the two controls
(meta/compare-conditions (p/by-name :fizz-buzz) {:generations 20 :seed 1})

;; The whole experiment (~40 min). Run it in its OWN JVM, not over nREPL:
;; the client times out and sends :interrupt, which kills the eval and loses
;; everything. `run-experiment!` writes after every problem-seed, so a partial
;; result survives a kill.
(meta/run-experiment! "results/propeller-meta.edn"
                      [:small-or-large :fizz-buzz :fuel-cost
                       :count-odds :smallest :gcd]
                      [1 2 3]
                      {:generations 40})

;; The selection control: same everything, scalar selection instead of
;; lexicase. This is what showed selection to be the dominant factor.
(meta/run-baseline (p/by-name :fizz-buzz)
                   {:generations 40 :seed 1 :selection :tournament})
```

Tests: `clojure -M:test -n benjamin-schwerdtner.problems.propeller-test -n
benjamin-schwerdtner.clj-push3-play.instructions-test`

## Files

| file | what |
|---|---|
| `clj_push3_play/instructions/input_output.clj` | `:inN`, PRINT stack |
| `clj_push3_play/instructions/polymorphic.clj` | `_empty`, `_dup_times`, `_dup_items`, `_deep_dup` |
| `problems/propeller/alphabet.clj` | `get-stack-instructions`, ERCs, name mapping |
| `problems/propeller/data.clj` | input sampling, case sets |
| `problems/propeller/problems.clj` | the 31 problem definitions |
| `problems/propeller/run.clj` | execute a program against cases |
| `problems/propeller/gp.clj` | lexicase + UMAD baseline |
| `problems/propeller/meta.clj` | the dual-evolution landscape |
