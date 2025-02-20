PLDI 2021 Paper #66 Reviews and Comments
===========================================================================
Paper #66 How to Evaluate Blame for Gradual Types


Review #66A
===========================================================================

Overall merit
-------------
A. I will champion accepting this paper.

Reviewer expertise
------------------
Z. **Some familiarity** = "I have a passing knowledge of the topic(s) but
   do not follow the relevant literature"

Paper summary
-------------
This paper presents a methodology for evaluating blame assignment strategies
based on the notion of "the rational programmer". The paper develops the
methodology in detail and then then uses it to evaluate three different blame
strategies: "Transient", "Natural" and "Erasure". 

In gradual typing, a blame assignment is used to show where the program went
wrong. Importantly, at least on the theory side, a well-typed component cannot
be blamed. The authors argue that such blame assignment has received a lot of
academic attention, yet industrial implementations of programming languages with
gradual typing completely forego blame. Why is this? Is it because blame is not
useful in practice? The paper aims to answer this question by applying the
developed methodology to three strategies: The "Natural" blame strategy relies
on proxies to perform additional type checks at run-time. The "Transient" blame
strategy inlines these checks and does not perform "deep" type checks. The
"Erasure" blame "strategy" performs no checks: it only fails if execution of the
program fails. 

The contribution of the paper is the methodology for evaluating blame (and
secondary its application to the three strategies.) The idea is to design an
algorithm that behaves as a programmer would debug a gradually typed program
that crashes at run-time with a type error: by incrementally adding types to the
untyped parts of the program until the source of the type error is revealed. In
this way, the question is: Does the blame assignment (or lack thereof in the
case of "Erasure") help guide the programmer? How many "steps" must he take to
find the defect and can he find it at all? 

Trying to summarize the methodology:

- Take a collection of fully typed programs.
- Consider mix-typed versions of those programs.
- Develop mutators (like in mutation testing) that inject type errors.
- Emulate the rational programmer who follows the "trail" in the spirit of
  Figure 5.
  - At each step the programmer decides on an untyped component and then types
    it, reruns the program, and either sees a type error or decides to type
    another untyped component.
- Take into account how the "next untyped component" is selected based on the
  specific system at hand. 
- Experimentally evaluate whether the above method is able to find the fault and
  through how many steps.

Comments for author
-------------------
This paper is exceptionally well-written and was a joy to read. Every time I had
a concern the paper seemed to anticipate it and provide useful commentary. 

The paper does a very good job at describing the challenges of how to evaluate
blame assignment strategies and then sets out to develop an automated evaluation
methodology based on the notion of "the rational programmer". The paper
carefully lays out the foundation for the generation of buggy mix-type programs,
the actions and variants ("modes") of the rational programmer which can depend
on the specific blame system, and finally how to sample the "experimental
space". The paper also includes a clear section on threats to validity.

I think it is important to have a methodology to evaluate the usefulness of
blame, and in lieu of large-scale and costly user studies, I think this paper
provides a very well thought out automatic approach (with the natural
limitations that such an approach must inevitably have.)

Points for:

- Addresses a problem that has not been addressed before.
- Clear, concise, and meaningful methodology.
- Clear description of the threats to validity.
- Very well-written paper.

Points against:
- none

# Questions

- Could you comment a bit more on what makes a mutator "good"? (other than
  experimentally seeing that it is good.) Perhaps, what would be a really bad
  mutator for evaluating blame assignment?

- I don't understand where the 16,800 number comes from. Is it an exhaustive
  enumeration of all possible mutations? (Or the number of mutants constructed
  in X amount of time?)

- How large are the programs in the benchmark suite?

# Minor Comments

- L38: Should there have been a colon? Is the text "This contrast... " and
  challenge quote from [3]? Who is saying what here...

- L104: I was confused by the word "Next". Maybe instead: We now ..."

- L268: Grammar issue.

- L314: Floating period.

- L441: Add line numbers (or other metric of size).

- L956: 30,000 hours of compute time is 1,250 days. I assume that the CPU has
  many cores. Could you put that number?

- L980: Figure 5.4??

- L991: Figure 6: It took me some time to understand the figure. I wonder if
  there is a better way to represent the same data?



Review #66B
===========================================================================

Overall merit
-------------
B. I support accepting this paper but will not champion it.

Reviewer expertise
------------------
Y. **Knowledgeable** = "I follow the literature on this paper's topic(s)
   but may have missed some relevant developments"

Paper summary
-------------
The paper proposes an evaluation method for blame assignment
strategies and evaluates three different strategies with the
method. While many theoretical gradual typing systems have adopted
blame, blame has been neither shown to be practically useful nor
applied to real-world languages because the systematic evaluation of blame
is difficult. The authors introduce a notion of rational programmers
and simulate their behaviors to evaluate blame. A rational programmer
is assumed to iteratively add type annotations to an untyped portion
of his or her program according to the feedback given by a run-time
system. This behavior can be simulated even without real human
programmers. If a run-time system leads a programmer to the correct
locations of a bug, its blame strategy can be considered more useful
than a strategy that cannot do so. The paper compares the Natural,
Transient, and Erasure strategies. The result shows that both Natural
and Transient outperform Erasure, which does not assign any blame,
and, therefore, implies usefulness of blame assignments.

Comments for author
-------------------
This paper tackles a long-standing difficult problem, which is the
problem of evaluating blame. Many existing gradual typing papers
provide blame systems, but because the papers do not evaluate blame
systems, it is not obvious whether blame itself is indeed helpful for
debugging. Among many theoretical gradual type systems adopting blame
assignments, no paper discussed the evaluation of blame systems. It is an
important problem because, without any evaluation, theoretical aspects
like gradual guarantee and the blame theorem may not be applicable to
practical systems combining static and dynamic typing.

This paper shows that blame assignments do help programmers, and it
may serve as a first step to close the gap between the theory and
the practice of gradual typing.

The experiment results show that there are debugging scenarios where
Transient is more useful than Natural. Does it happen simply because
of luck? Or, does it imply that Transient is more suitable than
Natural for a particular kind of bugs for some reason? Also, can there
be a new blame strategy that outperforms both Transient and Natural
all the time?

Theoretical gradual type systems usually allow programmers to control
the precision of type annotations in a fine-grained way with the Dyn
type rather than distinguishing typed and untyped modules. Would the
same evaluation method be able to directly appliable to blame in such systems?

Please explain what impedance mismatches are in the paper. Is it
simply type mismatches?

# Strengths

* The paper is well-organized.
* It proposes a novel solution to a long-standing difficult problem.
* Threats to validity are discussed in detail.

# Minor comments

The paper states that the effort distribution of the random mode
follows a normal distribution, as expected. Please explain why a
normal distribution is a reasonable expectation. When there are $n$
untyped components, the random mode randomly selects one of them, and
only one of them is a buggy component. Therefore, in the first
iteration, the random mode reaches the end of the trail with the
probability of $1/n$. In the second iteration, there are $(n-1)$
untyped components and the probability of a failure at the first
iteration is $(n-1)/n$, so the probability of effort $2$ is $1/n$
again. In this way, it seems to be that the probability of effort $i$
is $1/n$ where $1<=i<=n$ and $n$ is the number of initially untyped
components. Each trail has a different number of untyped components,
so the resulting distribution would be the average of such
distributions. Based on this reasoning, it is unclear why the
distribution is a normal distribution. This point does not affect the
contribution of the paper, but it is not clear from the paper.

* page 9 line 980: Figure 5.4 -> Figure 6
* page 10 Figure 7: Null -> Random
* page 11 line 1103: ( 770 -> (770



Review #66C
===========================================================================

Overall merit
-------------
C. I would not accept this paper but will not argue strongly against
   accepting it.

Reviewer expertise
------------------
Z. **Some familiarity** = "I have a passing knowledge of the topic(s) but
   do not follow the relevant literature"

Paper summary
-------------
This paper develops an approach for empirically evaluating the effectiveness of blame assignment in gradually typed languages. Roughly speaking, the idea is to model the steps taken by a "rational programmer" who systematically debugs errors in a gradually typed programming. The question is whether different blame assignment strategies are able to help this rational programmer find the "true" source of errors faster, requiring fewer steps.

To evaluate this approach, the paper also presents empirical results. Starting with a small corpus of programs, the authors use mutations to systematically (but synthetically) introduce type errors -- e.g., one mutation replaces a constants with another of a different type, and another mutation swaps method identifiers. They then conduct experiments to quantify the number of steps required by a rational programmer for several blame assignment schemes: Natural (used in Typed Racket, which tracks at most one location for blame), Transient (used in Reticulated Python, which tracks a set of locations), and Erasure (which does not actually assign blame, and is widely used in industry).

Unsurprisingly, the evaluation shows that blame is effective, when used by a rational programmer. The results are inconclusive, but some trends are clear. Most significant, all of the schemes that assign blame are more useful than Erasure. And Natural blame more often more useful than Transient blame.

Comments for author
-------------------
This is a neat paper that develops an approach for evaluating gradual typing and blame assignment. Thanks for submitting it to PLDI '21! The results are presented in an extremely clear way that draws out the key insights without compromising on technical detail. The notion of a rational programmer -- loosely modeled on rational agents from economic theory -- is a neat conceptual device. And the empirical approach does seem like the right way to address the question of the usefulness of blame. 

But while there is a lot to like about the paper, it also has some shortcomings. The suite of programs used for the evaluation is fairly small (c.f., the SIGPLAN checklist). The faults are introduced using synthetic mutations that may or may not correspond to the kinds of errors that arise in practice. This can be seen from the results in Figure 7 -- the lengths of the "trails" is quite small. While the study is mostly well done, it only considers two blame assignment strategies. For example, it would be interesting to consider the "omniscient programmer" to get a bound on the value of blame. But this is not done. And unfortunately the results are inconclusive -- Section 8 says "our results call for a deeper understanding of the two models for blame". Also, the published Transient scheme seems to have a minor bug, which might affect the outcomes when fixed (Section 8.2). Most if not all of these shortcomings are discussed in the paper, which is much appreciated. But they do risk undermine the value of the contributions to some degree.

To explain one comment above: I was surprised not to see a comparison against an "omniscient programmer" to establish a baseline (c.f., the SIGPLAN checklist). That is, model a blame assignment scheme in which blame could be placed on any component. And always pick the component that minimizes the length of the trail. Of course, this would be infeasible to implement in practice, but it would place an upper bound on the utility of blame. 

Similarly, I wondered why the modes of the transient programmer only consider picking the first/last blame, and not other choices. Section 5.2 just says "our answer" is that there are at least "two reasonable options." Why are other choices not reasonable?



Review #66D
===========================================================================

Overall merit
-------------
B. I support accepting this paper but will not champion it.

Reviewer expertise
------------------
Y. **Knowledgeable** = "I follow the literature on this paper's topic(s)
   but may have missed some relevant developments"

Paper summary
-------------
Several contract and gradual type systems provide mechanisms for
assigning blame: When encountering a run-time error signifying the
violation of a contract or type signature, which pieces of code should
be scrutinized by the programmer in seeking a fix?

In a recent POPL 2020 paper, Lazarek et al. provide the first attempt
to evaluate if and how blame assignment may help in actual debugging.
In that paper, bugs are systematically injected (via syntactic
mutation) into a suite of Racket programs, and the corresponding blame
trails are analyzed to see whether they, by writing more precise
contracts to "shift" blame from the assigned component, eventually
lead to the actual bug.

The current paper extends the POPL 2020 methodology from contracts in
Racket to gradual (or migratory) typing in Typed Racket, in several
steps:

First, to complement the "Natural" higher-order contract system of
(Typed) Racket, this work implements a "Transient" blame assignment
strategy, which unlike Natural inserts only shallow run-time checks.
(To date, Transient has appeared only in the separate design and
implementation of Reticulated Python, which prevents a head-to-head
comparison as desired in this work.)

Second, compared to POPL 2020, new mutators are defined to inject the
kinds of type-mismatch bugs that may be caught by the gradual type
system and corresponding run-time checks and blame assignment.

Third, the paper runs a large experiment on 10 benchmark programs,
inserting mutations and analyzing whether the blame trails produced by
each debugging mode --- following (a) the component blamed by Natural,
(b/c) the first or last component, respectively, blamed by Transient,
or (d) an exception raised by the underyling language --- conclude
with the actual bug. Results show that Natural, Transient-First, and
Transient-Last all produce short successful blame trails (typically of
length one or two) on most debugging tasks, and that they are all
similarly effective with no clear winner for all tasks.

Comments for author
-------------------
This work advances a compelling approach to evaluating a usability
question through a systematic exploration of synthetic bugs and fixes.
The large-scale experiment is impressive, and I find both of the main
results surprising:

1. That Natural is not much more effective than Transient. I thought
the shallow checks of Transient would certainly be less effective than
the full checks of Natural. It will be interesting to see if and how
this holds up in subsequent usability studies, both with synthetic and
real users and tasks.

2. That Transient --- as implemented in Typed Racket --- is much
slower than Natural. This also goes against intuition, as well as
previous results reported about Reticulated Python (but apparently
there were flaws in those experiments, as noted in this paper). Given
that the experiments are developed and run in the context of the
large, real-world Racket ecosystem, I wonder whether about the chance
of incidental implementation choices or bugs affecting this result.

Overall, I think this is a useful reference for further usability work
on contracts and gradual type systems, and am generally in favor of
acceptance.

At the same time, however, I believe the paper overstates the novelty
and challenges to "bring [the POPL 2020 approach] to the world of
gradual typing" (L1252). Comments about each of the three main
components of the methodology in turn below.


*Natural and Transient Under the Same Roof (Section 3)*

This seems to be "only" an engineering challenge.

And the presentation in this section is rather informal. As someone
with passable knowledge of Natural and Transient, I would have liked
to see a concise formalism with clear "knobs" for choosing between
blame assignment strategies. Trying to imagine readers with less
familiarity, I wonder how well this discussion serves as a primer.


*Custom Mutators (Section 4)*

The paper suggests that the mutators required here are significantly
different and more subtle, but this does not seem to be the case when
comparing Figure 3 and Table 3 of the POPL 2020 paper.

Of the 12 (of 16) gradual-typing related mutators listed in Figure 3:

* `constant`: Similar to previous, but now with a type error
* `deletion`: Similar to previous
* `public`: Similar to previous "hide-method"
* Four others involving swapping identifiers: Similar in spirit to
"swap argument" in previous

Of the 4 (of 16) Typed Racket related mutators:

* `arithmetic`: Simpler version of previous
* `boolean`: Same as in previous
* `negate-cond`: Same as in previous

So there is quite a bit of similarity. And on the flip side, why are
not all (8) mutators from the POPL 2020 experiment included here?

These questions are not so important in an absolute sense; _some_
specific mutators need to be chosen to generate some bugs, and these
seem to work fine. But they are salient given how much the paper
emphasizes the new mutators as a contribution.


*Debugging Strategies (Section 5)*

These also seem to be overemphasized. The main design choice seems to
be in how to use Transient blame, which reports multiple components,
and picking the first and last are pretty intuitive.

I'm also not sure why the exception mode is needed in addition to
Erasure (I was confused in Section 2 L204-215 and Section 5.1 L732).
Indeed, Section 5.3 defines the Erasure mode the follow the Natural
exceptional mode.


*Additional Comments and Typos*

L69: I wish here, and elsewhere, the comparison to [13] had been made
more explicit. As discussed above, there seem to be more similarities
than suggested.

L92: "use of _a_ higher-order contract system"

L155: "authors extensive"

L253: "exports _it_ as"

L280: Missing `untyped-`

L314: " . "

L382: "Dyn,"

Fig 2: Presumably authors were included to indicate the variety of
sources, but I'm not sure that was necessary.

L597: "make us_e_ of"

L664: "described in _Section_ 4"

L980: "Figure 5.4" ==> "Figure 6"

L986: "2.8%" ==> "2.18%" ?

L1017: "33%" ==> "26%" ?

L1304: "Problem is ...."



Review #66E
===========================================================================

Overall merit
-------------
C. I would not accept this paper but will not argue strongly against
   accepting it.

Reviewer expertise
------------------
Z. **Some familiarity** = "I have a passing knowledge of the topic(s) but
   do not follow the relevant literature"

Paper summary
-------------
This paper seeks to establish (or refute) the benefit of blame tracking in gradually typed programming environments. It takes several Typed Racket benchmarks and considers multiple approaches to blame tracking. The central idea is to use a fault injection method to create variant programs along two dimensions: type errors inserted according to some crafted heuristics, and static typing selectively removed on a component-by-component granularity. This creates a large number of variant programs which may produce run-time type errors with blame; this is then iterated to simulate a "rational programmer", i.e. such that blame is used to select where more static checking will be added next; at any given iteration this may or may not catch the injected error, and the number of iterations is counted as a proxy for effort. A further contribution is implementing the different forms of blame-tracking in the same framework so that they can be compared. The finding is that blame is found to offer some positive advantage in effort (over simply being guided by the stack trace of a run-time exception) in 9--13% of the initial sample of variants, with some minor differences between the forms of blame tracking tried.

Comments for author
-------------------
I commend the authors for asking the question. Their method is clearly the product of much careful thought. I understand the difficulty in doing experiments about errors that tend not to hit 'real' source code repositories, and I appreciate the effort taken to re-create the different approaches to blame in a comparable setting.

However, I don't find the results to be as conclusive as the authors state. Aside from the Erasure case, whose relevance I found unclear (see below), conclusions rest on some small-valued "more useful than" percentages. These are pretty hard to interpret, because the "usefulness" metrics necessarily build in a lot of simplifying assumptions. E.g. in 5.4, the "percentage of scenarios where ... is more effective" doesn't account for *extent* of difference, and the trail-length "programmer effort" metric in 5.5 is also unlikely to be a great proxy. These limitations are understandable, method-wise, but overall the results are fairly null... they let us continue believing "what we'd expect" but not with any great sense of added confidence. That doesn't entirely diminish the work, of course, but I'd say the current write-up overplays its hand a little. I appreciate the discussion of threats to validity.

The presentation of the results is very much around aggregates and summaries. Indeed the whole method is about having run a huge compute job over a large number of variants. Perhaps it's paranoia but I'd be interested to see some specific examples walked through by hand (in the paper) and some smaller sample manually classified (as results). That would add an extra sanity check that the metrics do correspond to some meaningful reality.

About Erasure: if I'm reading this correctly, the key thing here is that in Typed Racket, the set of static types is more expressive than the set of dynamic types. For example, there is a static notion of non-negative integer that is distinct from plain integer. This sort of design isn't universal -- in some languages/systems static types closely mirror the classification of objects in the language's dynamic semantics. Writing these stricter contracts into a program brings its own benefits, separate from blame or indeed from static checking. It feels like the paper doesn't take enough care to distinguish the two effects: the effect of systematically applying more refined contracts over program values, and the effect of gradually enabling static checking of those contracts (iteratively, guided by blame). The authors do mention this around line 204, and the comparisons in Figure 6 between "_ exceptions" and "Erasure" seem to be measuring this -- the gain from checking these extra annotations, with the more precise and/or more timely checks that they imply, relative to the erased case where only the language baseline contracts are checked. Indeed that's the point of 'exception' experiments. Since the biggest effect sizes on display are these ones -- between Erasure and anything else -- this seems at best distracting. I'd be glad to hear from the authors if I'm misunderstanding anything here. In the detailed comments below, I have noted some places in the text where it would be useful to remind the reader that this design property of Typed Racket is at play.

It's interesting to note that there seems to be no convincing analogous experimental or modelling-based justification for plain old static typing, i.e. showing that it somehow presents a net gain to the programmer. Rather, this has simply been posited/assumed by a very long line of work. I am not defending that state of affairs, but it points to the difficulty of showing conclusively that something truly helps programmers. I can see value in this sort of simulation-style approach, and it is no less convincing than user studies. So I'd be interested to hear from the author(s) if they have any more arguments that (1) I've underestimated the results' conclusiveness, or (2) this is a novel/interesting family of methods that might be pursued more widely, or (3) that deeper experiments building on these ideas might yield more compelling insights.

Detailed comments:

In the title, "evaluating blame" reads oddly. It is ambiguous, because "evaluate" sometimes means to compute a result. A fuller phrase, like "evaluating the usefulness of blame tracking...", would probably be worth the words.

The abstract doesn't say much about the work. It would be better written for experts to quickly gather an overview what the paper contributes.

Conversely, the main body of the paper skimps a bit on background. It never explicitly covers what "blame" means and how it works in practice. Similarly, it repeatedly talks about "impedance mismatches" without defining them. Perhaps this phrase is now standard in the gradual typing literature, which I haven't kept up with (hence my Z expertise). But I did read the Wadler/Findler paper carefully at the time. It did not talk about impedance mismatches. In any case, it is a fuzzy metaphor... please say exactly what it means here.

line 34: "then their compilers remove types and rely on the built-in safety checks of the underlying language to catch any problems". This reads oddly in context. Didn't they just do a bunch of static checking? So they are *not* relying just on dynamic checks to catch problems? Maybe there is something more accurate to say here... e.g. no dynamic check is removed, or something like that.

line 38: "explicit statement and challenge" -- what is it?

line 49: I agree they got the word wrong, but you should explain this

line 60: around this paragraph the writing started to grate. There's no need to generalise about what people do or think, and the "As a matter of fact... simply..." style is somewhere between laboured and patronising. It's better to more plainly state the gap in the literature that you're addressing. Throughout the paper, much space could be saved by writing in a more direct style.

line 69: be explicit that Lazarek et al were (as I later gathered) doing something about blame to do with higher-order contracts but not gradual types

line 70: what does it mean to "follow" the slogan? Was unclear to me.

line 90: at first I wondered: what is a case? Maybe say "program variant" to foreshadow the idea of generating mutants etc?

line 91-ish: "Transient, "Natural", "Erasure" -- be explicit that these are names that *you* are introducing

line 94: "forego" => "forgo"

line 119: "homo economicus" needs glossing or a reference

line 125: was wondering whether "impedance mismatch" just means "feasible run-time type error".

line 148: last sentence appears to contradict the preceding paragraph, and is left hanging oddly. Instead, make it the start of a new paragraph.

line 160: "disparity" is a strange word here. "diversity"?

line 170: "Otherwise, results from..." -- the point was already clear

line 174: ... instead of just saying what you built on, first say what you did! It's really not clear at this point.

line 187: what is a "component"? Should be easy to define.

line 199: "blame set as another form of a stack trace" -- yes. I was hoping to see a clearer example of debugging with and without blame, i.e. something making explicit the similarities and differences between having blame info and having only a stack trace at the error site.

line 211: "languages exceptions" typo

line 268: this explanation of the workings of proxies seemed overwrought

line 277: what is "responsibility" of a "party", exactly? Are "party", "module" and "component" all the same things?

line 283: "rewrites typed modules to inline checks" -- so a module gets turned into a check? Clearly not, but that's how it reads

line 293: I was wondering what constitutes a boundary crossing. Clearly, passing by function call or return crosses from the caller's module to the callee's. What about values exchanged through reads/writes to shared state?

line 299: "crosses" => "crossings" (probably)

line 313: it threw me that a program might not fail but "produce a wrong result". If it could be caught by the gradual type system, why can't it be caught at run time? My best get at explaining this is by what I wrote above, i.e. it's a consequence of Typed Racket's more refined static notion of type. In certain other systems this wouldn't be possible, because the static checker would only catch (albeit earlier) errors that would be caught at run time, so there would be no basis to call the result "wrong".

lien 352: "three interpretations" -- what are they? I don't see them in the figure.

line 380: from this I inferred that "migratory typing" means "gradual typing applied at modulewise granularity". Assuming that's correct, it's worth saying directly.

line 421: "type mistake" -- does this mean "feasible run-time type error"?

line 424: "fully typed correct programs" -- presumably your method could also work with not-yet-fully-typed correct programs, just not ranging over the entire lattice in those cases. I was wondering whether that might give different/interesting results.

line 433: "without loss of diversity" -- this is glib. Clearly diversity is lost; just claim that what remains is still diverse enough.

line 497: "truthiness" needs explaining

line 517: the difference between #1 and #2 here again relies on the surprising property (to the unfamiliar) that Typed Racket has a notion of "type-level mistake" that doesn't surface under erasure (as an exception or whatever) but also is not a false positive (i.e. you're not talking about conservativeness of static checking). Line 538's "unavoidable" claim is probably also true only in such a context.

line 519: "at least three" -- better to claim this is only two here, then explain later that the driver doesn't count. Mentioning "three" up-front just raises an unnecessary question in the reader's mind.

line 634: earlier I had inferred that the distinction between "migratory" and "gradual" was more than just preferring one word; this seems to contradict that

line 640: what makes it "concise"?

line 673: "programmers runs" typo

line 727: "dubbed" -- reads oddly. Maybe italicise the "location", but it seems overkill. The phrase is pretty self-suggesting as it is.

line 756: "checked the value's type.." -- and the check passed!?

line 752: "added to the blame set first" -- should it be a blame list, then?

line 936: no need for hyphen after the adverb

line 966: "more useful C" -- missing "than"

line 982: don't think these percentages deserve 3 significant figures

line 1304: "Problem is" -- missing "The". Also, no need to italicise the next sentence... it is really not that deep or surprising.
