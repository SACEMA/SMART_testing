
### Helper function:
### Where _would_ the tests go, assuming you know the sizes and hazards?
placeTests <- function(state, haz){
	return(state*(1-exp(-haz)))
}

## Is the base hazard right?
checkTests <-  function(base, numTests, state, haz){
	return(sum(placeTests(state, base*haz)) - numTests)
}

### Where _do_ the tests go if we calibrate the base hazard 
### to get the right number number of tests
assignTests <- function(numTests, state, relHaz, mult=1.1){
	hpop <- sum(state[relHaz>0])
	stopifnot(numTests<hpop)
	popHaz <- min(relHaz[relHaz>0])
	prop <- numTests/hpop
	upr <- -mult*log(1-prop)/popHaz
	lwr <- 0
	uni <- uniroot(checkTests, lower=lwr, upper=upr
		, numTests=numTests, state=state, haz=relHaz
	)
	baseHaz <- uni$root
	place <- placeTests(state, baseHaz*relHaz)
	return(numTests*place/sum(place))
}

assignTests(
	numTests <- 180
	, state <- c(100, 100)
	, relHaz <- c(1, 3)
)
