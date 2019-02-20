---
title: Promote Haskell
---

<p class="lead">Help your team adopt Haskell</p>

This page is targeted at developers looking to get their teams or
companies to consider adopting Haskell.

At FP Complete, we like to use the **attractors** and **obstacles**
approach, contrasting the draw of Haskell with concerns surrounding
deploying Haskell. This page is designed to help you:

* Find the attractors
* Address the obstacles

## Attractor: The three pillars

At FP Complete, we talk about Haskell having the three pillars of software development:

* **Performance** Haskell performs on par with other higher-level languages
* **Productivity** Haskell is expressive, allowing developers to get work done quickly
* **Robustness** Strong/static typing, purity, and immutability lead to solid, easy to maintain code with reduced bug counts

## Attractor: Powerful concurrency

Haskell's concurrency story is best-in-class. Immutable-by-default
prevents a large number of race conditions. Software Transaction
Memory simplifies many others. Purity simplifies the interaction of
code.

## Attractor: Rich library ecosystem

There are over 10,000 open source Haskell libraries on
[Hackage](https://hackage.haskell.org). [Stackage](https://www.stackage.org),
the curated subset of Hackage, contains thousands of commonly used
libraries which are vetted to build and work together.

## Attractor: Commercial success

Haskell has a long history of commercial success, especially in the FinTech and Life Sciences spaces, and more recently in the Blockchain space. It is also used regularly by companies large and small for tasks varying from web development to replacing shell scripts.

FP Complete has a [collection of case
studies](https://www.fpcomplete.com/case-study) demonstrating large
Haskell success stories. In addition, we have a crowd funded list of
Haskell success blog posts and articles below. If you'd like to add
something to the list, please use the edit button at the top of the
page. (Also, check out the original [call to action blog
post](http://www.snoyman.com/blog/2017/04/haskell-success-stories)).

+ [Technical Underpinnings](http://blog.newbusinessmonitor.co.uk/posts/2017-04-25-technical-underpinnings.html) - description of a working haskell stack
+ [My “Haskell In Production” Story](https://medium.com/@djoyner/my-haskell-in-production-story-e48897ed54c)
+ [Shipping Haskell via Homebrew](http://chrispenner.ca/posts/homebrew-haskell)
+ [Use Haskell to attach binaries to GitHub releases](http://taylor.fausak.me/2016/05/09/add-files-to-github-releases/)
+ [How we secretly introduced Haskell and got away with it](https://tech.channable.com/posts/2017-02-24-how-we-secretly-introduced-haskell-and-got-away-with-it.html)
+ [Parse Ruby Objects in Haskell](https://filib.io/posts/2017-04-24-parse-ruby-objects-in-haskell.html)
+ [Jekyll in Haskell](https://github.com/2016rshah/heckle)
+ [REST in Haskell](https://mcksp.com/rest-api-in-haskell)
+ [The Water Jug Problem](http://clrnd.com.ar/posts/2017-04-21-the-water-jug-problem-in-hedgehog.html)
+ [Building Gathering: A Website For Announcing Group Events](https://gilmi.me/post/2017/04/25/building-gathering)
+ [Submitting a pull request to GHC](https://chris-martin.org/2017/phabricator-ghc-pull-request)

## Obstacle: Commercial support

There is a persistent belief that, due to Haskell's [roots in
academia](/philosophy), there is no commercial support available for
the language. This is not the case. To try and make this as easy to
knock down as possible, FP Complete has put together a [Haskell
Success Program](/success), which makes it easy and affordable for
companies to work with our team of Haskellers to get commercial
assistance and training.

## Obstacle: Ability to hire

There is some truth to this concern. Haskell does not have the
marketshare of, say, Java. If your requirements at work is to find
highly experienced Haskell developers locally on a regular basis, you
may have difficulty unless you're in a large tech hub. However, we've
seen companies succeed greatly using a few techniques:

* Be open to remote work. The marketshare issue comes down to lack of
  density of jobs and developers in the same locale. Globally, there's
  a huge market of Haskellers.
* Hire non-Haskellers and train them. With the growing popularity of
  functional programming, Haskell's concepts are no longer foreign to
  developers. We've provided lots of [learning material](/learn) on
  this site to help with that, and are always available to [help your
  team succeed](/success).
* Supplement your team with consultants. For more information, check
  out the [FP Complete consulting
  page](https://www.fpcomplete.com/consulting).

Remember that the marketshare issue cuts both ways. While companies
may have trouble finding engineers, overall the Haskell job market is
a buyer's market. There's a huge pool of highly talented people out
there looking to spend their days writing Haskell code.

## Obstacle: Learning curve

Haskell _is_ significantly different from standard languages most
developers learn. The delta between Python and Ruby is significantly
smaller than Python and Haskell, for example. You should anticipate a
learning curve when introducing your team to Haskell. We believe this
cost is worth it, because:

* The benefits of Haskell pay off in the medium term, and you reap
  huge benefits in the long term
* The act of learning pure functional programming can help your
  developers find new approaches to problems in _any_ language

That said, a few things can help reduce the learning curve.

* [Use our learning material](/learn), which is designed to focus on
  commercial Haskell programming and bypass a lot of guesswork by the
  learner
* [Follow our recommended best
  practices](/tutorial/best-practices). The biggest time sink we see
  for new Haskellers is reinventing the wheel. It's not necessary! Get
  started quickly with established recommendations.
* Consider our [Haskell Success Program](/success) which includes
  significant training and mentoring.

## Obstacle: Not pragmatic

There is a persistent myth that Haskell isn't pragmatic. "It takes 50
lines to write Hello World in Haskell," or other such nonsense. This
isn't true. While you _can_ make Haskell non-pragmatic, you can do the
same in any language. A lot of the non-pragmatic code you'll see
people point to in online discussions is specifically exploring new
ideas, which is a [healthy part of language development](/philosophy).

If you follow our [recommended best
practices](/tutorial/best-practices), you can focus in on the most
pragmatic and beneficial parts of the language and ecosystem.

## Convince my boss

Alright, so you're ready to convince your boss to try Haskell. Or you
_are_ the boss, and you're intrigued by what you've read. Firstly, in
communicating with the rest of your team, **do not dismiss their
concerns**. We've included some of those concerns here. These are not
non-issues. In our estimation, the benefits outweigh the costs. But be
honest about measuring the costs, especially in your specific case.

Use Haskell _at the right time_. If you have a 3 week deadline and no
one on your team knows Haskell, _don't start using Haskell now_. Our
recommendation is:

* Schedule some time to learn Haskell
* Participate in some [commercial training](/success) to speed up the process
* Select a pilot project to test the waters with. Small web services
  or command line utilities are great for this, since they have well
  defined interaction points with the rest of your code.
* Plan to have a retrospective after the pilot project to assess how
  it went and decide how you'd like to continue
    * While we at FP Complete hope you'll love Haskell and continue
      using it, keep in mind that even if you go back to writing code
      in your previous programming language, learning Haskell can help
      you better take advantage of functional features in other
      languages

We're always happy to have a conversation about Haskell
adoption. [Contact us](https://www.fpcomplete.com/contact-us) to set
up a conversation with one of our Haskell developers.
