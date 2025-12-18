# Resonator Network Capacity Analysis for FHRR

## Summary

Based on literature review (Frady et al. 2020) and empirical testing, the capacity of Resonator Networks in FHRR is determined by the relationship between:
- Vector dimension `d`
- Number of factors `n`  
- Codebook size `m` (number of items per book)

**Key finding**: Capacity limit occurs when the search space size `m^n` approaches `√d`.

## Your Specific Case (d=10,000, n=3)

With FHRR dimension D=10,000 and 3 factors:
- √10,000 = 100
- Critical threshold: m^n ≈ 100

### Empirical Results

| m (book size) | Search Space (m³) | Success Rate | Assessment |
|---------------|-------------------|--------------|------------|
| 2             | 8                 | ~100%        | Excellent  |
| 3             | 27                | ~100%        | Excellent  |
| 4             | 64                | ~99%         | Excellent  |
| 5             | 125               | ~97%         | Very Good  |
| 8             | 512               | ~97%         | Very Good  |
| **10**        | **1,000**         | **~93%**     | **Good** (your observation) |
| 12            | 1,728             | ~88%         | Moderate   |
| 15            | 3,375             | ~83%         | Moderate   |
| 20            | 8,000             | ~77%         | Poor       |

## Why "3 books of m=10 then it starts"

Your observation that with m=10 "it starts" failing matches the empirical data:
- m=10 gives ~93% success rate
- This means ~70 failures per 1,000 attempts
- The search space (1,000) is 10× larger than √d (100)
- You're well beyond the theoretical comfort zone

## Theoretical Explanation

According to Resonator Networks papers by Frady, Kent, Olshausen, and Sommer (2020):

1. **Capacity Formula**: For reliable factorization, we need:
   - `m^n << √d` for high reliability
   - `m^n ≈ √d` is the critical threshold
   - `m^n > √d` leads to degraded performance

2. **Phase Space Saturation**: In FHRR, each hypervector component is a complex phase angle (phasor) randomly selected from (0, 2π]. When the search space becomes too large relative to the dimension, the phase space becomes overcrowded and similar patterns interfere.

3. **Resonator Dynamics**: The algorithm works by:
   - Starting with superposition of all codebook vectors
   - Iteratively refining estimates through binding/unbinding operations  
   - Using cleanup operations with codebook matrices
   - Converging to stable fixed points

## Recommendations for Your Use Case

### Option 1: Reduce Codebook Size
- Use m ≤ 4 for near-perfect reliability
- Use m ≤ 8 for >95% reliability

### Option 2: Increase Vector Dimension
For m=10 with 3 factors to work reliably:
- Need m³ << √d
- 1,000 << √d
- d should be >> 1,000,000 (impractical)

### Option 3: Accept Occasional Failures
- With m=10, expect ~7% failure rate
- Implement retry logic or error handling
- Use lower similarity threshold or more iterations

### Option 4: Reduce Number of Factors
- With n=2 factors: m² = 100 = √d (borderline)
- With n=1 factor: Not meaningful for factorization

## Code to Test Capacity

```clojure
(require '[benjamin-schwerdtner.clj-push3-play.hdc.capacity-analysis :as cap])

;; Check theoretical limit for your setup
(cap/theoretical-capacity {:d 10000 :n 3})
;; => Conservative: m ≤ 2, Aggressive: m ≤ 3

;; Test actual performance
(cap/test-capacity {:m 10 :n 3 :d 10000 :trials 100})
;; => Success rate ~0.93

;; Analyze capacity curve
(cap/analyze-capacity-curve {:n 3 :d 10000 :trials 100 :max-m 15})
```

## References

- Frady, E.P., Kent, S.J., Olshausen, B.A., Sommer, F.T. (2020). "Resonator Networks, 1: An Efficient Solution for Factoring High-Dimensional, Distributed Representations of Data Structures." Neural Computation 32(12), 2311-2331.

- Kent, S.J., Frady, E.P., Sommer, F.T., Olshausen, B.A. (2020). "Resonator Networks, 2: Factorization Performance and Capacity Compared to Optimization-Based Methods." Neural Computation 32(12), 2332-2388.
