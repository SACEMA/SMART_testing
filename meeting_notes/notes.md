SMART-testing June 04 2020
=================

## Agenda:

* Finalise research question

* Updates from junior team:

	* Testing process:
		* random sampling for ascertainment (multivariate hypergeometric distribution)
		* separate steps for disease process and testing ("testing in the evening")
		* reducing eligible pool in various compartments (weighting of group sizes)

		Questions:
		* demand driven eligibility/weights?
		* choosing and/or interpreting eligibility/weights for ascertainment

		Issue: how do we sensibly incorporate lags in testing?
		* Possible answer: don't move people to ascertained compartments, just count the numbers of positives; moving people to ascertained compartments becomes more complex when lag in test results becomes long.

	* Demand-driven testing:

		* The number of tests conducted is determined by:
			* A baseline daily number of tests
			* A simple linear feedback with lag, with some imposed maxima of daily tests

		* Question: should we make the lag stochastic (which distribution to use)? or spread the effect of demand?

## Points discussed

#### Research Question
Right now people are estimating incidence from positive tests. Can we improve those estimates, or our understanding of those estimates, by adding information about testing practices. In addition to test protocols and total tests performed, can we use information about who was tested, and via which protocols?
Additional question/goal: Can we use this project to drive better data collection (and sharing) practices?

#### General sentiments:
We should explore the model we currently have (focussing on differential testing and test quantities) without adding additional complexity (e.g. lags and/or backlogs in test processing) for the time being.

#### Plotting results:
* positives vs incidence; proportion positive vs incidence; proportion of active cases which are ascertained
* make plots prettier (use ggplot)

#### Questions and comments for moving forward:
* deterministic version should be deterministic, stochastic version should be stochastic.
* Should we make the testing deterministic for now, and then later move to a fully stochastic model, or should we make the switch to a stochastic version right now? Answer: Start with a fully deterministic model: Junior team to spend approximately 1 hour trying to figure out how a deterministic analog to the hypergeometric distribution, otherwise we will ping Jonathan.
* Why are we getting feedback in proportions positive via the "demand-driven" testing mechanism we have implemented?
* Request that we move towards doing things in the tidyverse.

#### Current situation in SA:
* testing is currently limited by reagent supplies, rather than machines (or staff?). There are global shortages of reagents.
* there was a a big push to get wider testing in communities, via community health care workers etc
* now that there's a big backlog of tests they are talking about discarding backlog (samples degraded?)
* pending change in testing protocol. under new protocol only healthcare workers at high risk and people admitted to hospital will be tested.
* How active is/was community screening? How many of those samples were processed?



## Meeting 2020-06-18
And subsequent discussion with Juliet

### Agenda:
Present progress:
* deterministic distribution of tests (distributes all tests)
* cyclic behavior seems to be improved

### Discussion:
Questions for South Africa
* how many COVID deaths go undetected?
* someone dies of covid without being tested - how are they classified?
* what percentage of critical cases are dying?
* tracking of throwing away of backlogs (how is backlog being processed, how are they choosing which backlogs to throw away)
* what is happening with backlogs?
* what is positivity rate in SA?

### Next steps:
* look through parameters again and make sure the fatality rates are reasonable
* explore some simple scenarios. e.g. initially bias towards testing covid-positive people (travellers), then shift towards more-random testing, then shift back to only-hospitalised people (including non-covid hospitalisations) **
* start thinking about lags in testing and how they might be incorporated.
* characterise the current method of demand-driven testing and think about how it could be improved. (suggestion: testing = product of demand and supply / sum of demand and supply. There may be a second order refinement of this.). Describe the "slider for how much testing is determined by demand vs supply".
* plot prevalence of non-ascertained people among elibible population (as a basic test) **
* what effect is the removal of ascertained people from the testing pool having on the proportion positive over time (we expect it to decrease) ** proportion positive vs prevalence?

### After next steps:
* include mechanism for post-mortem classification of deaths?
* full demographic stochasticity
* consider making this a shiny app; at least create better pipeline for outputs.

Random idea:
what about (deterministically) smushing delay from collection to result

Agenda: 2020_06_24

1. Fatality rates corrected (outbreak plot and/or cumulative plot)
2. Demand feedback (Tests conducted plot)
3. Discussion about scenarios:
- what we did; how do we actually choose values for these?
4. How will we implement lag?
- separate test processing and test collection?
- implementing a lag:
	- do we want to separate test collection and processing ? In order to create a backlog.
	- we want the lag to be smushed out in time ? at least when there's a big backlog.
	- it feels as if the "Ascertained" compartments are getting in the way of including a backlog...
- do we really want ascertained compartments? it seem like hospitalisation is a somewhat separate process, and in many places quarantine will depend on symptoms rather than testing (so quarantining people based on testing will make the process more realistic for pre-symptomatic and asymptomatic people [?] but we can probably get away with clunky things instead of all these compartments). clunky things = fixed reduction of infectiousness of critical cases (and/or mild), or a reduction in overall transmission related to the extent of ascertainment.

not done: pipeline, sensible separation of distinct parts of code, saving output plots

td
0. clean and pipe and engineer - Jeremy to begin
1. clearly write down the definitions of each scenario
2. fixing scenarios (fixing weights). same weights over time.
3. ask "if trying to infer incidence from tests, how could you do that?
3. just do a simple comparison like we did in the first one  
4. how does little r differ between positives and reality; can we control for that.
5. what factors offset the peak of one from the peak of the other
-next level. to what extent can or can't you learn more or faster from reports than just from hospitalisations
- is there evidence that using negative test results can be an improvement over using just positives, positives + hospital, just hospital. if so, what are the improvements we can use.
6. generate "true" and "visible" outputs.
7. add compartments of people waiting for test results - Abi
*** imagine a pipeline which takes observed data and tries to infer true data
- Elisha to look at literature - Elisha
- current 1/5 test positive in SA.
- lag complicated; solution: compartments of people waiting for test results.

- another issue related to backlog is degradation of samples over time. (2-4 days after collection most samples will test negative)
- highly sensitive; specificity?
- weird lines. why are they squiggly?
comment from Juliet:
- interesting to see that positivity goes down as soon as we hit threshold.
- in a situation such as SA, is there an indicator of when testing is reliable? or positivity?
- data curation team?
- how are covid hospitalisations classified? are they reporting suspected covid hospitalisations?


Other things:
- Basic question: having critical going directly to death/recovery is an issue? how to we balance amount of people who die vs recover with the amount of time it takes them to die/recover?

- what do we call "ascertained" to be consistent
- prevalence among eligible pop vs proportion positive
- how should we organise code?

Junior team meeting:
each of us think of a pitch direction, then make pitch to group.
what

# Meeting 2020-07-01
### Agenda
* This is a short update meeting
* Jeremy's very minor updates
* Elisha's literature update
* Abi's update on waiting compartments and discussion to accompany


General consideration
* people being tested positive - reported in "tests done" statistics?
* this work could eventually be presented to NICD to request better data.

### Elisha's literature update
* we should all read this paper (Junior team can read and write summary?)
* one place with space is that they focussed on CFR and IFR (testing bias in those measures). Could be good for us to be using our model to explore bias in Rt or r or peak timing, final size, etc. (Only paper that looks at these measures are those by Pitzer et al.)
* Didn't look at any data (purely theoretical model)
* use simpler version of test assignment
* use fewer disease compartment distinctions, but include age and ??
* normal surveillance with PCR (random only) and sero survey
* testing more of diseased/symptomatic classes will bias IFR.
* even if sampling randomly can still have biased estimate of case fatality because there is a lag - cases are moving
* people who got infected today, take some time before you know their outcome.