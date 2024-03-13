# Fork

### Motivation

Fork is a collection of lightweight helper functions for multiverse analyses.

This software is released for educational purposes, not for active use.
It is possible that questions and bug reports may go unanswered.

Existing packages for multiverse analyses such as `multiverse` on CRAN
(Abhraneel Sarma, Matthew Kay) or the language-agnostic `Boba` (Yang Liu, Alex
Kale, Tim Althoff, Jeffrey Heer) rely on a domain-specific language to specify
branches, which allows multiverse branches to be specified without a bunch of
if/elses that mess up the code flow. The downside to this approach is that
they cannot be run as a plain R script or inside of an interactive session,
and force you into a certain way of doing things. They also assume that every
universe is independent of every other universe, so they miss opportunities to
cache shared paths before they diverge. (Boba in particular does provide
wonderful visualizations of the resulting multiverse, though!)

Fork, instead, is a sort of minimum viable approach to multiverse
specifications. It provides a `fork` function to easily specify partial cross
products of analytic choices, and an assortment of functions to help setting up
a cache, ranking universes from most to least favored, and so on. Scenarios
(universes) produced by repeated calls to `fork` are ordered to be maximally
cacheable with a low memory footprint.

This lightweight approach does not do away with branching, but it minimizes
their depth to one or two levels so that the resulting code still flows nicely
and is easy to reason about.

In principe, multiverse analyses can be executed on multiple cores, but this
requires a separate cache and a separate call to `plan_scenarios` for each core
-- just partition the scenarios into two, three, four, etc. and then call the
function on each part -- otherwise the cache hints provided by the plan will be
wrong.

### Installation



### Usage

```r
library('fork')

scenarios <- grid(add=1:3) |> 
  fork(add=1, multiply=5:10) |> 
  fork(add=2, multiply=10:15) |> 
  fork(add=3, multiply=15:20)

cache <- make_cache(scenarios)
planning <- plan_scenarios(scenarios)

analyze <- function(scenario) {
  invalidate_cache(cache, scenario$start_at)
  
  step <- step_through(scenario, cache)
  
  step('add', function(input) {
    10 + scenario$add
  })
  
  step('multiply', function(input) {
    input * scenario$multiply
  })
  
  return(cache$multiply)
}

multiverse <- planning |> 
  rowwise() |> 
  mutate(outcome = analyze(.data))
```
