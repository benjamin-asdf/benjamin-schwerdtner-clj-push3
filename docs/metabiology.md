# Metabiology on Push3

Gregory Chaitin is one of the founders of algorithmic information theory
(program-size complexity, the halting probability Ω). *Metabiology* is his
attempt to give Darwin the same treatment (*Proving Darwin*, 2012): a theory of
evolution simple enough to prove theorems about.

The premise is that life is software. DNA is a program, so an organism is a
program, and a mutation is also a program: `A' = M(A)`, with `M` drawn with
probability decaying exponentially in its size. What evolves is not a string of
bits under point mutations but a piece of code under algorithmic mutations.

The question is about open-ended creativity: does random mutation plus
selection keep inventing new things forever, and how fast compared to the
alternatives? Chaitin answers with a comparison of three search regimes —
exhaustive search, cumulative evolution, intelligent design — and proves that
evolution sits much closer to design than to blind search.

To make that provable he needs a fitness that never tops out, and picks a toy
model for it: the *busy beaver* problem, "name the biggest integer you can".
It is a stand-in, not the substance; any fitness that rewards unbounded
mathematical creativity would do. We use it because it is Chaitin's, so the
results can be held against his bounds.

Here the software organisms are Push3 programs.

| namespace | what it is |
|---|---|
| `metabiology.organism` | an organism is a Push3 program; fitness is the number it names |
| `metabiology.mutation` | algorithmic mutation: the mutation operator is itself a program |
| `metabiology.evolution` | the three regimes, and their growth curves |
| `clj-push3-play.instructions.bignum` | BIGNUM stack: unbounded integers, see [the note at the end](#appendix-the-bignum-stack) |

## Organisms and the oracle we don't have

Chaitin's organisms are self-delimiting programs whose fitness is defined only
if they halt, and he helps himself to a **halting oracle** to discard the ones
that don't. We have no oracle, so we bound execution steps and pick a reading
policy (`:oracle` in `organism/observe`):

- `:resource-bounded` (default) — read whatever number the organism built when
  the step budget ran out. This makes fitness the *bounded* busy beaver:
  biggest number nameable in T steps.
- `:halting-required` — fitness 0 unless it halted in budget. Closer to
  Chaitin's letter, and brutal: an organism that squares in an `:exec_y` loop is
  exactly the good mathematician we want, and it never halts.

**What this costs us, stated plainly:** the bounded busy beaver is computable,
so these runs do *not* demonstrate the strict theorem (fitness outgrowing every
computable function). What they demonstrate is the *separation between search
regimes*, which is the part you can actually run. Raising the budget raises the
ceiling forever, and that is where the open-endedness lives.

`:saturated?` in a result means the organism hit the substrate's bit budget, not
the limit of its own cleverness. It is reported separately everywhere so it can
never be mistaken for open-ended growth.

## The frugal busy beaver

Plain busy beaver rewards bloat. Measured, under `:busy-beaver`: genomes double
9 → 17 → 33 → 65 points while fitness stays flat, because `:mut_duplicate` plus a
one-part-in-sixty fitness gain pays for any amount of junk. The frugal busy
beaver — already sketched in `problems/busy_beaver.clj` as `named / steps` — is
the obvious answer, so `organism/fitness-policies` implements the family:

| policy | selection value | shape |
|---|---|---|
| `:busy-beaver` | the number named | Rado / Chaitin |
| `:frugal-steps` | `named / steps` | ratio |
| `:frugal-size` | `named / points` | ratio |
| `:bits-per-step` | `log2(named) / steps` | log-space |
| `:bits-per-point` | `log2(named) / points` | log-space |
| `:parsimonious` | `[named, -points]` | lexicographic |
| `:parsimonious-steps` | `[named, -steps]` | lexicographic |

Every policy reports the *same* measured axes (`:bits`, `:points`, `:steps`), so
runs stay comparable: the policy decides what selection optimises, the axes are
what we observe.

### How weak is ratio frugality?

Easy to get wrong, and worth stating before running anything. When outputs are
towers, dividing by a resource barely registers:

```clojure
;; the same squaring organism, five verdicts
{:busy-beaver    "30103 digits"
 :frugal-steps   "30100 digits"   ;; divided by 1000 steps. Three digits.
 :frugal-size    "30102 digits"
 :bits-per-step  100.0
 :bits-per-point 14285.7}
```

An organism naming a 100,000-bit number in 200 steps has log2 fitness
`100000 - 7.6`. The penalty is 7.6 parts in 100,000. Frugality in **log space**
is proportional and therefore bites equally at every scale.

The arithmetic is right and the obvious conclusion drawn from it — that ratio
frugality is too weak to matter and log-space frugality is the one that will
control bloat — is **wrong in both halves**. Being weak is exactly why ratio
frugality works, and being proportional is exactly why log-space frugality
freezes. See the results below.

### Saturation stops being terminal

Under plain busy beaver, an organism that saturates the bit budget ends the run:
nothing can be strictly fitter. Under *any* frugal policy that is false, and this
is the most interesting consequence — a saturated organism can still get fitter
by naming the same number with less program or fewer steps. Selection silently
changes objective from "name a bigger number" to "name this number more
cheaply", which is a search for program-size complexity. `evolution/compression-phase`
extracts exactly that segment of a run: `:bits` flat, `:points` falling.

### Results: three mechanisms, not one

6 seeds, 60,000 trials, 200 execution steps, 1e5-bit budget, cumulative
evolution. Medians:

| policy | median bits | median points | median accepted | saturated | compressed |
|---|---|---|---|---|---|
| `:busy-beaver` | 9,638 | 267.5 | 17.5 | 1/6 | 0 (cannot) |
| `:frugal-steps` | 9,532 | 190.5 | 27 | 2/6 | 2 |
| `:frugal-size` | 52,733 | 158.5 | 32 | 3/6 | 2 |
| `:bits-per-step` | 4 | 3 | 6 | 1/6 | 1 |
| `:bits-per-point` | 4 | 3 | 6 | 1/6 | 1 |
| `:parsimonious` | 38 | 199.5 | 151.5 | 0/6 | 0 |
| `:parsimonious-steps` | 15 | 274 | 38.5 | 0/6 | 0 |

Cost pressure helps only when it is **weak**. Three distinct mechanisms:

**Ratio frugality (weak) — works.** `:frugal-steps` / `:frugal-size` climb like
plain busy beaver, because the `named` term dominates so completely that any
increase in the number is accepted whatever it costs. The cost term only
discriminates *among organisms naming the same number*, which is precisely where
bloat lives. Leaner genomes, and they compress.

The `:frugal-size` median of 52,733 bits against busy beaver's 9,638, and its
3/6 saturating runs against 1/6, are **noise** — see the 16-seed confirmation
below, where the rates come out equal. The leanness is not noise.

**Log-space frugality (proportional) — bimodal.** `:bits-per-point` per seed:
`[4 4 4 100000 4 5467]`. Four of six runs freeze at 3 points, one saturates
completely — and does so at **14 points**. A proportional penalty rejects every
intermediate step that does not immediately pay for itself, so the walk usually
sits at a lean local optimum forever. When it does escape, it produces the most
compact organisms of any policy.

**Lexicographic parsimony — churns.** `:parsimonious` was the predicted
best-of-both and is the worst behaved: 151.5 accepted mutations against plain
busy beaver's 17.5, while genomes stay at ~200 points and bits stay low. Making
cost a strict tie-breaker means every point-shedding lateral move gets accepted,
so the walk spends its budget shuffling cost around instead of climbing.

A note on the short-budget trap, which caught this thread twice: at 4,000 trials
*all* frugal policies look like total failures (3.5 bits, 3 points). Takeoff
happens between 4,000 and 15,000 trials. Any conclusion about this landscape
drawn from a few thousand trials is an artifact.

### 16-seed confirmation

`results/metabiology-frugal-16seeds.edn`. Restricted to the runs that reached the
100,000-bit ceiling, so it compares *programs that did the same mathematics*:

| policy | saturated | program points of the saturating runs | median | froze (<100 bits) |
|---|---|---|---|---|
| `:busy-beaver` | 9/16 | 57 63 81 105 136 252 277 312 353 | 136 | 3/16 |
| `:frugal-size` | 10/16 | 14 15 15 17 20 43 52 74 178 194 | 31.5 | 1/16 |
| `:bits-per-point` | 8/16 | 13 14 15 17 20 22 42 43 | 18.5 | 7/16 |

Three things, now on enough seeds to say them:

1. **Nobody climbs better.** 9, 10 and 8 of 16 reach the ceiling — the same rate.
   Frugality does not help you find big numbers, and does not hurt either.
2. **Frugality buys leanness, and the effect is large and clean.** Median program
   for the same output: 136 → 31.5 → 18.5 points. The `:busy-beaver` and
   `:bits-per-point` distributions do not overlap *at all* — busy beaver's
   leanest saturating program is 57 points, and every single one of
   `:bits-per-point`'s is smaller than that.
3. **Proportional pressure is a gamble, ratio pressure is free.**
   `:bits-per-point` froze 7 runs in 16 to buy its 20-point median.
   `:frugal-size` froze 1 in 16 — fewer than plain busy beaver's 3 — while still
   cutting the median program by a factor of 4. `:frugal-size` is the policy to
   reach for.

### What frugality actually buys: 20× smaller mathematicians

The clean result is not the climb rate, it is the *program*. The leanest
organism that reached the 100,000-bit ceiling, found under `:frugal-steps`:

```clojure
(9 9 9 9 9 (7 :bignum_frominteger)
 :bignum_pow :bignum_pow :bignum_pow :bignum_pow :bignum_pow)
```

`7^(9^5)` — Chaitin's `N^N^N...` ladder, discovered by a random walk, in **14
code points and 14 execution steps**. Plain busy beaver's saturating champion
needed **277 points and 200 steps** for the same output.

Points at the moment of saturation, per policy: `:busy-beaver` 277;
`:frugal-steps` 17 and 14; `:frugal-size` 17, 14, 178; `:bits-per-point` 14.

And the compression phase is real. After the number ceiling is hit, `:bits` stays
pinned at 100,000 while the genome is whittled down:

| policy | points | records |
|---|---|---|
| `:frugal-steps` | 53 → 17 | 26 |
| `:frugal-steps` | 37 → 14 | 22 |
| `:bits-per-point` | 35 → 14 | 15 |

So a saturated busy-beaver run does not have to be a dead run. Under frugal
fitness it becomes a program-compression run — an approximate search for the
shortest program naming a given number, which is Chaitin's program-size
complexity from the other direction.

One honest caveat: `:busy-beaver` shows 0 compression *by construction*, not as a
finding — saturation is terminal for it, so it cannot continue. The meaningful
comparison is 277 points versus 37 at the moment of saturation, before any
compression has happened.

## Algorithmic mutation

This is the move that makes metabiology different from a genetic algorithm. A
mutation `M` is a **program**; the child is `A' = M(A)`; and the probability of
drawing `M` decays exponentially in `|M|` — the size of the program that
*describes* the change, not the number of symbols it touches.

The consequence, which is the whole reason to bother:

```clojure
(mutate '(2 :bignum_frominteger) '(:mut_duplicate))
;; => (2 :bignum_frominteger 2 :bignum_frominteger)
```

Doubling the genome is **one** algorithmic step. Under point mutation it is
exponentially unlikely. Hamming distance maximal, algorithmic distance 1. Same
for reversal (`:mut_inverse`), rotation, interleaving two genomes (sex /
symbiosis). The reachable neighbourhood of an organism is not a ball of small
edits — it is every transformation with a short description. Gradualism is not
assumed.

`mutation/mutation-distance` searches for the smallest `M` with `M(a) = b`,
which is an upper bound on the relative information content of `b` given `a`
(Chaitin 1975). An upper bound is all anyone gets — the exact value isn't
computable.

### Deviation from Chaitin: the distribution

Chaitin's `2^-|M|` lives on self-delimiting binary programs, where Kraft's
inequality makes the weights sum to ≤ 1. Push programs over an `A`-symbol
alphabet are not self-delimiting: there are `A^k` programs of size `k`, so
weighting each by `2^-k` gives total mass `(A/2)^k`, which diverges.

So we sample in two stages: draw a size `k` with `P(k) ∝ 2^(-k/complexity-halving)`,
then draw uniformly among programs of that size. What is preserved is the
property the theory leans on — probability decaying exponentially in description
length. What is lost is the exact numerical weight.

### Two bugs that had to be fixed to make mutation work at all

Both were silently crippling variation:

1. **`execute-1` re-seeded the RNG on every execution step**
   (`interpreter.clj`), so `code_rand`, `integer_rand` and the `mut_*`
   instructions returned *the same draw at every step* — "random" instructions
   were effectively constants. `*rng*` is now bound once per `execute` call, so
   the stream advances across steps while a run stays reproducible from its
   seed. Nothing else in the repo called `execute-1` or set
   `:push/random-seed`, and the seed was always 0, so past results were
   deterministic before and remain deterministic now — but they may differ.

2. **`:code_rand` splices in ~80% junk.** It draws from
   `gen/default-generators` concatenated with the identifier list — 4 junk
   generators (booleans, random ints, gensyms, empty collections) against 1
   identifier generator. Organisms bloated with gensyms and nils while learning
   nothing. `metabiology.mutation` registers `:mb_rand_code` / `:mb_rand_atom`,
   which are the same idea drawing from the organism alphabet only. Measured
   effect: 1 accepted mutation per 3000 trials → 6, and then takeoff.

## The three regimes

Chaitin's central result is not that software evolves, it is a **comparison of
rates**. How long does each regime need to reach BB(N)?

| regime | Chaitin's bound |
|---|---|
| exhaustive search | ~2^N time |
| cumulative evolution (random walk, keep if fitter) | ~N² mutations |
| intelligent design (oracle picks the best mutation) | ~N mutations |

Cumulative evolution sits strictly between blind search and design, and much
closer to design. That is the mathematical content of "life is creative":
Darwinian evolution is a quantifiable search accelerator, neither mechanical nor
magic.

### Accounting

The regimes are only comparable if they pay the same currency, and there are two
that matter:

- **`:trials`** — one per mutation attempt or enumerated program, i.e. compute.
  Intelligent design pays `:pool-size` per step, because we have no oracle and
  must *buy* the best mutation by sampling and evaluating a pool.
- **`:accepted`** — accepted mutations only. This is Chaitin's unit, in which
  his designer gets the best mutation for free.

Reporting both is not fence-sitting; they disagree, and the disagreement is the
most interesting thing the runs show.

### Handicap left in place

Exhaustive search enumerates **flat** programs only, so it cannot express a
multi-instruction `:exec_y` loop body (which needs nesting) while the mutation
regimes can build one. It is handicapped on expressiveness on top of being
exponential. Enumerating nested programs in size order would fix it; the
exponential blowup is identical either way.

## Results

Budget 60,000 trials; organisms capped at 200 execution steps and a 1e5-bit
budget; organisms start from `'()`, as Chaitin's do.

Every number below was regenerated in a single batch run against one state of
the tree (`scratchpad/rerun.clj`), which matters more than it sounds. The
earlier regime numbers were produced interactively in a long-lived nREPL whose
loaded image changed underneath the experiment when `instructions/mutation.clj`
was reloaded from another session — so they were not reproducible from the
committed code. The frugal numbers, produced later in that same image, come back
bit-identical today. **Run experiments in a fresh JVM, or the image is a hidden
variable.**

### A single cumulative-evolution growth curve

`[trials bits]`, one entry per record-holder:

```
[0 0] [812 2] [860 4] [2577 4] [2586 4] [2775 4] [2960 5] [3005 5]
[3621 29] [3724 29] [3822 257] [3859 264] [3916 264] [3918 264]
[3920 392] [3981 36045]
```

Two things to read off it. The curve is **accelerating**, not linear: a long
crawl in the single-bit range, then 29 → 38 → 386 → saturation within 400
trials. And the crawl is real — a 3000-trial run stops just before takeoff and
concludes evolution barely works, which is what the first run here did.

### Cost to reach a given fitness (8 seeds)

`evolution/replicate-comparison`, seeds 1–8. Exhaustive search is deterministic
given the alphabet, so one run of it *is* the run.

`reached` is how many of the 8 runs ever got there; the cost columns are medians
**over the runs that did**, so they are survivor-biased and should be read
together with `reached`, not instead of it.

| target bits | exhaustive | CE reached | CE trials | CE accepted | ID reached | ID trials | ID accepted |
|---|---|---|---|---|---|---|---|
| 4 | 246 trials | 8/8 | 1,143 | 2 | 8/8 | 1,168 | 2 |
| 16 | never | 7/8 | 3,621 | 6 | 7/8 | 3,648 | 6 |
| 64 | never | 6/8 | 3,343 | 9 | 6/8 | 3,376 | 9 |
| 256 | never | 5/8 | 2,973 | 9 | 4/8 | 3,520 | 9.5 |
| 1,024 | never | 5/8 | 2,973 | 10 | 4/8 | 10,176 | 11 |
| 10,000 | never | 4/8 | 10,544 | 13.5 | 2/8 | 20,000 | 16 |
| 100,000 | never | 2/8 | 9,530 | 12 | 1/8 | 17,152 | 10 |

Aggregate, same runs:

| regime | median best bits | median trials | median accepted | saturated |
|---|---|---|---|---|
| exhaustive search | 7 | 60,000 | 60,000 | 0/1 |
| cumulative evolution | 10,799 | 60,000 | 14.5 | 2/8 |
| intelligent design | 1,277 | 60,000 | 15 | 1/8 |

### What reproduced and what didn't

**Reproduced, emphatically: the exhaustive-vs-evolution separation.** 60,000
enumerated programs reach 7 bits — and reach it after 246, then never improve
again. Cumulative evolution gets to 10,799 bits (median) and saturates the
100,000-bit substrate in 2 runs of 8, on ~14 accepted mutations. That is
Chaitin's `2^N`-vs-`N²` gap, and it is not subtle.

**Did not reproduce: intelligent design beating cumulative evolution.** In
accepted mutations ID tracks CE step for step and edges ahead or ties exactly
where theory says it should, but it never pulls away, and in trials it loses
badly — it reaches each threshold in fewer runs of 8 than CE does.

The reason is instructive rather than a bug. Best-of-pool **discards
improvements**: if 5 of the 32 pool members improve on the parent, ID banks one
and throws away the other 4. Cumulative evolution accepts the first improvement
it finds and immediately compounds from there. So per attempt CE found
improvements ~15× more readily. Chaitin's `~N` bound for design is a statement
about a *free* oracle; the moment the designer has to pay for its own
omniscience, greedy selection is worse than opportunistic selection under a
fixed compute budget.

## Open threads

- **Bloat on neutral plateaus** — *addressed*, see the frugal section above.
  Ratio frugality (`:frugal-size`) is the policy to reach for: same climb as
  plain busy beaver, median program ~4× leaner for the same output, and it keeps
  going after saturation.
- **Staged fitness.** The obvious next experiment, suggested directly by the
  results: run `:busy-beaver` until saturation, then switch to a frugal policy
  purely to compress. That would combine busy beaver's reliable climb with
  log-space frugality's 14-point champions, instead of gambling on a
  proportional penalty that freezes 4 runs in 6.
- **Why does log-space frugality freeze, and what lets a run escape?** One seed
  in six escaped and produced the most compact organism of the whole
  experiment. Whatever distinguishes that trajectory is worth knowing; it is a
  question about evolvability, not about busy beaver.
- **Lexicographic parsimony churns** (151 accepted mutations to plain busy
  beaver's 17). Worth testing whether capping lateral moves, or accepting cost
  reductions only after a stagnation window, recovers it.
- **Watson's objection**, recorded in `ideas/BIG_IDEAS.org`: natural selection
  may be divide-and-conquer rather than hill climbing. All three regimes here
  are Chaitin's, i.e. single-organism hill climbing. A symbiogenetic regime
  (`Compose(A,B) = A+B` over separately evolved genomes) is the obvious next
  one, and `:mut_interleave` is already in the mutator alphabet.
- **Meta-adaptation**, the thread `ideas/BIG_IDEAS.org` cares most about: make
  the mutation distribution itself evolve, i.e. a second genome for `Mut`. The
  substrate is ready for it — mutators are already Push programs, so they are
  already organisms.
- **Nested enumeration** for a fair exhaustive baseline.
- **Raise the ceiling.** Every saturating run is a run whose result is a
  statement about `:max-bignum-bits`, not about evolution.

## Appendix: the BIGNUM stack

The busy beaver toy model needs integers without a ceiling, since a fitness
with a ceiling has an optimum and an optimum ends the process. `:push/integer`
is clamped to ±1e10 (`stack/pushvalues.clj`), where repeated squaring
`(2 2 :exec_y (:integer_dup :integer_*))` stalls at 34 bits within 250 steps.
So `:push/bignum` carries `java.math.BigInteger`, bounded only by a bit budget
(`:max-bignum-bits`, default 1e5 ≈ 30k decimal digits) at which results
saturate instead of exhausting the heap; the same organism on BIGNUM,
`(2 :bignum_frominteger :exec_y (:bignum_dup :bignum_*))`, reaches the full
100,000 bits. There are deliberately no BIGNUM literals: organisms lift values
in with `:bignum_frominteger`, which keeps programs plain edn — a `BigInteger`
literal would read back as a `Long` when round-tripped through a results file.
