---
title: FP Complete's Philosophy
---

Haskell has a strong tradition of blending a research and pragmatic
language. GHC, the flagship Haskell compiler, continues to explore
cutting edge language research. The flexible type system encourages
people to explore new ideas in libraries. At the same time, production
software is written and run around the world in Haskell.

FP Complete falls squarely on the pragmatic side of this
spectrum. We're here to make production-grade commercial software, and
help others do the same. Our philosophy is based in that world. Our
overall goal is to reap the huge benefits of Haskell while minimizing
risks.

As a company, when we analyze how to approach a project and which
tools to use, we want to maximize benefits (efficient runtime,
productivity, etc) while reducing costs (learning curve, project
risks, etc). Some general recommendations for this include:

* Respect the [novelty budget](https://www.shimweasel.com/2018/08/25/novelty-budgets)
* Make sure you understand the **business** requirements, not just technical requirements
* Follow the 80% rule: use things that deliver 80% of the value, and
  don't try to optimize for that last 20%.

In 2018, Michael Snoyman, Vice President of Engineering at FP
Complete, wrote up a document called the "Boring Haskell Manifesto,"
which captures a lot of these ideas.

## The problem

Haskell is in many ways a revolutionary language. Many languages in
widespread use today are incremental changes on previous
languages. However, Haskell doesn't fit this model. Concepts like
referential transparency, purely functional programming, laziness, and
immutability are a stark departure from common programming approaches
today.

Most any Haskeller would argue that these radical changes are well
justified, and deliver great benefit. At the same time, this inherent
culture of making revolutionary changes attracts a certain mindset to
the language. As a result, we end up in a situation where Haskell has
essentially two distinct (and overlapping) subcultures:

* Explore interesting and revolutionary concepts in computer science,
  software engineering, and mathematics
* Make better software

Again, these are overlapping subcultures. The entire history of
Haskell is cases of esoteric, academic, intellectual, and "useless"
concepts becoming an elegant solution to challenging problems. Monads
are probably the most prominent example of this, and we're seeing a
reality where many other languages are slowly adopting the concept to
solve problems in concurrency and error handling.

On the other hand, not every new concept turns into one of these
fundamental and useful techniques. And even for those that do: finding
the most practical way to leverage the technique is an arduous,
time-consuming process.

Exploring these concepts can be fun, rewarding, and—long term—a huge
benefit for productivity. Short and medium term, however, this
exploration can lead to slower and less reliable results. As a company
or project manager assessing Haskell for a project, this uncertainty
can thwart the possibility of adopting Haskell.

We're now faced with a situation where Haskell is often eliminated for
usage, representing a massive loss for two parties:

* Companies, projects, and managers could have realized great benefits
  in productivity, reliability, and performance from the less
  revolutionary pieces of Haskell. Instead, they're losing this
  competitive advantage.

* Engineers who would much prefer working in Haskell—even its less
  revolutionary subset—are unable to do so because of employer fears
  of choosing it.

We'd like to improve the situation.

## The Boring Haskell Manifesto

Our claim is simple: for many cases of software engineering, a simple,
well-defined subset of Haskell's language and ecosystem will deliver
large value for a project, while introducing little to no risk
compared to alternative options. We call this subset, somewhat
tongue-in-cheek, "boring Haskell." Our goal is to:

* Define such a subset
* Provide straightforward documentation, tutorials, cookbooks, and libraries that encourage this subset
* Continue to evolve the subset to optimize its best practices
* Help interested engineers to learn how to use boring Haskell, and get it adopted in their companies

The most concrete step in this direction is creating the [rio
library](https://github.com/commercialhaskell/rio#readme), which is
intended to capture these principles. If you want to embrace Boring
Haskell today, we recommend using that library. The rest of this
document discusses what we believe counts as "Boring Haskell," and
motivates these choices.

## Power-to-weight ratio

We want to analyze the features of Haskell that we recommend based on
its power-to-weight ratio, also known as a cost-benefit analysis. Put
more directly: we want to choose features which provide lots of
benefits while minimizing costs. Let's give some examples of these two
sides:

**Power/benefit**

* More maintainable code
* Less bugs in production
* Higher performance
* Higher productivity

**Weight/cost**

* Learning curve
* Ongoing cognitive overhead
* Ongoing tweaking
* Slower compile time
* Poor performance

A concrete example of something with a great power to weight ratio are
sum types. Sum types are a relatively simple concept to explain. Most
people can grok the concept almost immediately. Pattern matching feels
natural fairly quickly. And sum types solve large classes of problems
people regularly encounter in programming tasks.

## Reducing risk

* Support is available
* We can hire people
* What’s the cost of hiring people full time/what’s the cost of training
* We can do rapid discovery in Haskell, then rewrite in something “less risky”
* Learning Haskell makes your engineers better engineers
* Escape hatch from Haskell if you hit a wall
    * Microservice
    * FFI to C/Rust
    * Feel free to drop down to imperative Haskell
