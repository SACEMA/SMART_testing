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
	stopifnot(numTests<=hpop)
	popHaz <- min(relHaz[relHaz>0])
	prop <- numTests/hpop
	upr <- -mult*log(1-prop)/popHaz
	lwr <- 0
	uni <- uniroot(checkTests, lower=lwr, upper=upr
		, numTests=numTests, state=state, haz=relHaz
	)
	baseHaz <- uni$root
	place <- placeTests(state, baseHaz*relHaz)
	# print(numTests/sum(place))
	return(numTests*place/sum(place))
}

# numTests = tests_conducted[t_index]
# state = SEIR[t_index, names(relHaz)]
# relHaz = relHaz[t_index,]
# 
# assignTests(numTests = tests_conducted[t_index], state = SEIR[t_index, names(relHaz)], relHaz = relHaz[t_index,])


assignTests(
	numTests <- 180
	, state <- c("a" = 100,"b" = 100)
	, relHaz <- c("a" = 1,"b" = 1)
)

