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