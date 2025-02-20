Change log
==========

Log format
----------
Review #: or in reference to Review #:
> comment ...+
*Changes in response to comment.*


Stressed revisions
------------------

Review Summary
> In Section 7 or 8: incorporate examples that illustrate the difference
> in trails between the three mechanisms. As it is, the reader does not
> learn much about these blame methods, as numbers do not teach the whole
> story.

in reference to Review C:
> Sections 8 and 9 report on the finding of the paper. These include statements
> that blame is useful, that Natural often produces shorter trails than
> Transient, and others. My only issue at this point is that the reader is left
> with numnbers only, while I think a reader would like to see selected
> examples, for example about which blame labels have been provided by Natural
> and which by Transient, so to see that one approach took a longer path, as
> well as similar examples the reader can learn from. Numbers do not seem to
> teach the whole story in this part of the paper.
*Add new prose + figure at beginning of discussion section (9, pages 22-23) walking through an example trail for every mode.*


Review Summary
> Revise the way you address the experiments of Vitousek et al. [38]
> according to the feedback that you have received.


in reference to Review D:
> First, the performance figures cited from [38] (worst case 5.4x
> slowdown) are used incorrectly: those numbers are about transient
> /without/ blame (see Section 6.2 of [38]), and given that "with the
> ...
> clear if the point is that performance of this benchmark in blame-free
> Reticulated is seriously slow compared to regular Python, or if the
> program itself is just slow in Python (in which case, what's the
> relevance to this paper?).
*Fix descriptions of Reticulated type inference and performance in the discussion section (9.6, pages 25-26).*
Specifically, we 
1. Fix the mistaken slowdown multipliers (bottom of page 25),
2. add an explicit acknowledgment that Reticulated Python supports type
   inference for local variables (top of page 26), and
3. revise all of the bullets summarizing the factors that may explain the observed slowdown (top of page 26).


in reference Review D:
> The characterization of the Monotonic semantics for references [22] as a
> variant of natural isn't correct--its exception-raising and blame
> behavior for references (though not for functions) is quite different
> from that of Natural. For example, a monotonic reference imported from
> untyped code into two incompatible typed contexts will error
> immediately, before being used.
*Fix discussion of Monotonic in related work section (10, page 26).*




Other revisions based on reviews
--------------------------------
Review A:
> When the rational programmer "fixes" a module and reruns, the implicit
> assumption seems to be that he runs the program in the exact same way. That
> would make the most sense to me, but you do not seem to come out and say so.
*Note in section 3 (page 6) that the rational programmer runs the program in the same way every time.*


Review B:
>  - Why does the paper conclude that "the existing theory does not predict
>    practice properly"?  Figure 8 shows that the theory (Natural) works well.
>  - What does the paper mean by "The existing practice may need additional
>    experiments"?  I have not found any evidence that confirms this claim in the
>    paper.
>  - What does "practice" means here?
*Rephrase the end of the intro section (1, page 2) to make clear the meaning of "practice".*


Review B:
> - L1154 "in the "fully typed" benchmarks":  Are these benchmarks on Python?
> - L1161 "the simplest benchmark":  Is this on Racket?  Which benchmark
>   does it intend?
*Adjust Transient threats discussion to make clear when we talk about Python benchmarks vs Racket ones (section 9.6, pages 25-26).*


Review B:
> - Page 7: How is the question (5) answered?
*Answer question 5 explicitly in section 3.1 (page 7).*


Review B:
> - It seems that the experiment implicitly supposes a few assumptions on
>   benchmarks.  First, the original benchmarks must be fully typed.  Thus more
>   experiments on mix-typed programs where some components cannot be typed may be
>   needed.  Second, the benchmarks are supposed to be deterministic, which I find
>   from the definitions of trails.  If it is the case, while I do not think these
>   restrictions have to be lifted in the paper, it would be nice to expose them.
*Note assumptions that benchmark modules are typeable and deterministic (in 6.1, pages 13-14).*


Review B:
>  - At first glance, the conclusion in lines 55-56 (starting with "Second, ...")
>    seems to contradict that in line 99-100 (starting with "neither is ...").  It would
>    be nicer to address them in a clearer manner.
and
>  - Which blame assignment strategy is "good" and which is not "good"?
*Reword conclusions of the intro section (1, page 2).*


Review B:
> - L424 "Let a configuration s of P":  Are configurations the same as scenarios?
*Use the term configuration consistently to clarify that a debugging scenario is a kind of configuration (section 5, especially 5.1 on page 9).*


Review B:
> - L685 "e.g., changing '*' to '/'":  Is this an example of the mutator that
>   "does not reliably lead to type errors"?
*Remove footnote about mutating `*` and `/`.*


All reviews mentioning typos.
*Fix typos, including in particular the 756/752 typo.*


Review B:
> - L1198 "But just because...":  It is difficult for me to find what this
>   sentence wants to say.  Please consider rephrasing.
*Rephrase start of conclusion (11, page 27).*


Review B:
> - L332 "soundness mechanisms":  What they mean is somewhat unclear.  Please
>   consider clarifying.
*Rephrase to eliminate use of "soundness mechanisms" (2).*


Review A:
> It would help if you could explain how a mutator like negate-cond actually
> leads to a program that has more type-level mistakes than before. It does not
> seem to me this part of the code itself will now fail, but that changing the
> conditions makes certain previously excluded paths in the code feasible.
and B:
> - It is not fully explained how the given mutators change programs.
*Add an example and explanation of the kinds of programs occurrence-typing mutators target (6.2, page 15).*


Review B:
> - In principle, Natural blame should always point out the faulty components
>   by the Wadler-Findler slogan.  However, the experiment shows it is not the
>   case.  Why?  Is it the same reason as in Lazarek et al.?  (Perhaps it is due
>   to a gap between theory and practice, but exposing a reason is crucial.)
and
> - Are failing Natural blame trails produced even for programs with impedance
>   mismatch?  (This question is related to the above issue with Natural blame.)
*Extend definition of blame modes to clarify that we use stack in absence of blame, and add paragraph explaining why it's necessary (5.2, 5.3; pages 10-11).*
*This now connects more clearly with the description on page 17, next to figure 10, describing trail failures we observed in the data.*


Review B:
> The table in section 2 looks rather ugly
*Simplify the table layout at the end of section 2 (page 4).*


Review B:
> - The paper often says that a blame system is (un)sound, but it is difficult for
>   me to identify what it precisely means.
*Ensure that sound and complete blame is adequately referenced throughout.*


Review B:
> - L1099 "as behavioral economics has shown more recently":  Is there a reference
>   to be cited?
*Add references for homo economicus and its problems (3, 9.3; pages 5, 24).*


Review B:
>   - The benchmarks are selected (line 647), but why?  The GPT benchmark suite of
>     Racket provides more examples.
*Add a paragraph to section 6.1 (page 14) clarifying how the benchmarks from GTP were filtered.*


Review B:
> - L356 "despite advertisements for the opposite":  I cannot find what this means.
*Clarify on page 8.*


Review B:
> - L370 "the latter must represent":  What does the "latter" specify?
*Rephrase on page 8.*


Review B:
> - It is not fully explained how the given mutators change programs.  For
>   example, I cannot completely predict changes by the mutators deletion and
>   class:super.
*Add extra example and explanation of occurrence typing mutators, which along with the examples in the mutator table (pages 14-15) should address this.*


Review B:
> - The paragraph starting at line 1122 is quite difficult to understand for me.
>   [...]
>   - What does the paper mean by "incomplete population of the blame map"?
>   - To understand the idea on the improvements of Transit, more explanations
>     on the usage of blame maps in Transient would be needed.
*Revise section 9.5 (page 25).*


Review B:
>   - What "sophisticated typing features" are considered (line 589)?  It would be
>     crucial to confirm whether the proposed mutators are enough.
*Done on page 13.*


Review B:
> - Perhaps it is valuable for followers to share the experience on developing
>   mutators that are not interesting, .e.g., in the supplementary material.
and
> - L785 "the interesting standard guided countless iterations":  I fail to find
>   what this intends.
*Expand prose at end of section 6.3 (page 16) to clarify how we used the criteria of interestingness to develop mutators.*


Review B:
> - L431 "type-level mistake":  Is it the same as a impedance mismatch?
*Revise the terminology in section 2 (pages 2-4) around impedance mismatches and boundaries to clarify this.*


Review B:
> - L556: It would be helpful to describe how to extend trails and how to
>   determine if there is no scenario to be added.  (This comment is also related
>   to the issue with Natural blame).
*Add new example at the beginning of discussion section (9, pages 22-23) which should address this.*


Review B:
> I am wondering whether the approach that the paper calls Transient First could
> be regarded as Transient Early, since it points to an earlier part of the
> code. Similarly Transient Last could be Transient Late, but you would have to
> see whether this terminology really fits.
*After a lengthy discussion and extremely careful consideration, we decided against these names because we like the stronger "first" and "last" contrast.*



Other improvements
------------------
*Update data in results section (8) with fixed issue that made some modes look worse than they should.*
Fixing this issue does not change our analysis or conclusions.


*Clean up tables in figures 4 and 5 (pages 13, 14).*


*Add a new threat subsection (9.2, page 24) to make generalization caveats very clear.*


*Clarify notions of boundary and blame, and how they differ between Natural and Transient, in section 2.*


*Add a new threat subsection (9.4, page 25) to clarify the threat of erasure bias.*


*Minor prose revisions, touch-ups, and rewording throughout.*
