test_that("## simple unconfounded X -> Y", {

b <- readRDS(test_path("test-graphs", "simple-unconfounded.RData"))
# igraph::upgrade_graph(b)
# saveRDS(object = b, file = test_path("test-graphs", "simple-unconfounded.RData"))
V(b)$nvals <- c(2,2)
eff <- "p{Y(X = 1) = 1} - p{Y(X = 0) = 1}"
obj <- analyze_graph(b, constraints = NULL, effectt = eff)
bound <- optimize_effect(obj)

expect_true(all(bound$bounds == c("\nMAX {\np0_0 - p0_1\n}\n\n", "\nMIN {\np0_0 - p0_1\n}\n\n")))

# with new version of optimize_effect:
new_version_bound <- optimize_effect_2(obj)
expect_equal(new_version_bound$bounds, c(lower = "\nMAX {\n  p0_0 - p0_1\n}\n", 
                                         upper = "\nMIN {\n  p0_0 - p0_1\n}\n"))
# visual comparison:
# cat(bound$bounds) # old version output string
# cat(new_version_bound$bounds) # new version output string

})

test_that("## simple confounded X -> Y", {

b <- readRDS(test_path("test-graphs", "simple-confounded.RData"))
# igraph::upgrade_graph(b)
# saveRDS(object = b, file = test_path("test-graphs", "simple-confounded.RData"))
V(b)$nvals <- c(2,2,2)
eff <- "p{Y(X = 1) = 1} - p{Y(X = 0) = 1}"
obj <- analyze_graph(b, constraints = NULL, effectt = eff)
bound <- optimize_effect(obj)

expect_true(all(bound$bounds == c("\nMAX {\n- p10_ - p01_\n}\n\n", "\nMIN {\n- p10_ - p01_ + 1\n}\n\n")))

# with new version of optimize_effect:
new_version_bound <- optimize_effect_2(obj)
expect_equal(new_version_bound$bounds, c(lower = "\nMAX {\n  -p10_ - p01_\n}\n", 
                                         upper = "\nMIN {\n  1 - p10_ - p01_\n}\n"))
# visual comparison:
# cat(bound$bounds) # old version output string
# cat(new_version_bound$bounds) # new version output string

## with categorical outcome (3 levels)

V(b)$nvals <- c(3,2,2)
eff <- "p{Y(X = 2) = 1} - p{Y(X = 0) = 1}"
obj <- analyze_graph(b, constraints = NULL, effectt = eff)
bound <- optimize_effect_2(obj)

expect_equal(bound$bounds, c(lower = "\nMAX {\n  -p10_ - p20_ - p01_ - p11_\n}\n", 
                      upper = "\nMIN {\n  1 - p20_ - p01_\n}\n"))
})



test_that("instrumental variable", {
    
    ## instrument Z -> X -> Y
    
    b <- readRDS(test_path("test-graphs", "instrument.RData"))
    # igraph::upgrade_graph(b)
    # saveRDS(object = b, file = test_path("test-graphs", "instrument.RData"))
    V(b)$nvals <- c(2,2,2,2)
    eff <- "p{Y(X = 1) = 1} - p{Y(X = 0) = 1}"
    
    obj <- analyze_graph(b, constraints = NULL, effectt = eff)
    bound <- optimize_effect(obj)
    
    expect_true(all(bound$bounds == c("\nMAX {\np00_0 - p00_1 + p10_0 - 2 p10_1 - 2 p01_1\n- p00_0 + p00_1 - p10_0 - p01_0\n- p00_0 + p00_1 - 2 p10_0 + p10_1 - 2 p01_0\np00_0 - p00_1 - p10_1 - p01_1\n- p10_0 - p01_0\n- p10_1 - p01_1\np00_0 - p00_1 - p10_0 - p10_1 - p01_0\n- p00_0 + p00_1 - p10_0 - p10_1 - p01_1\n}\n\n", "\nMIN {\n- p00_0 - p10_0 - p10_1 - 2 p01_1 + 2\n- p00_1 - p10_0 - p10_1 - 2 p01_0 + 2\n- p10_0 - p01_1 + 1\np00_1 - 2 p10_0 + p10_1 - p01_0 + 1\n- p10_0 - p01_0 + 1\n- p10_1 - p01_0 + 1\n- p10_1 - p01_1 + 1\np00_0 + p10_0 - 2 p10_1 - p01_1 + 1\n}\n\n" )))
    
    # with new version of optimize_effect:
    new_version_bound <- optimize_effect_2(obj)
    expect_equal(new_version_bound$bounds, c(lower = "\nMAX {\n  p00_0 - p00_1 - p10_1 - p01_1,\n  p00_0 - p00_1 - p10_0 - p10_1 - p01_0,\n  p00_0 - p00_1 + p10_0 - 2p10_1 - 2p01_1,\n  -p10_1 - p01_1,\n  -p10_0 - p01_0,\n  -p00_0 + p00_1 - 2p10_0 + p10_1 - 2p01_0,\n  -p00_0 + p00_1 - p10_0 - p10_1 - p01_1,\n  -p00_0 + p00_1 - p10_0 - p01_0\n}\n",
                                             upper = "\nMIN {\n  1 - p10_1 - p01_0,\n  1 + p00_0 + p10_0 - 2p10_1 - p01_1,\n  2 - p00_1 - p10_0 - p10_1 - 2p01_0,\n  1 - p10_1 - p01_1,\n  1 - p10_0 - p01_0,\n  1 + p00_1 - 2p10_0 + p10_1 - p01_0,\n  2 - p00_0 - p10_0 - p10_1 - 2p01_1,\n  1 - p10_0 - p01_1\n}\n"))
    # visual comparison:
    #cat(bound$bounds) # old version output string
    #cat(new_version_bound$bounds) # new version output string
    
    ## functional version
    
    f1 <- interpret_bounds(new_version_bound$bounds, obj$parameters)
    f1.new <- interpret_bounds(bound$bounds, obj$parameters)
    
    set.seed(12354)
    pr1 <- runif(length(obj$parameters))
    pr1[seq(1, 7, by = 2)] <- pr1[seq(1, 7, by = 2)] / sum(pr1[seq(1, 7, by = 2)])
    pr1[seq(2, 8, by = 2)] <- pr1[seq(2, 8, by = 2)] / sum(pr1[seq(2, 8, by = 2)])
    
    pr1 <- as.list(pr1)
    names(pr1) <- obj$parameters
    expect_equal(do.call(f1, pr1), do.call(f1.new, pr1))
    
    ## with monotonocity
    
    mono <- list("X(Z = 1) >= X(Z = 0)")
    
    obj <- analyze_graph(b, constraints = mono, effectt = eff)
    bound <- optimize_effect(obj)
    
    expect_true(all(bound$bounds == c("\nMAX {\np00_0 - p00_1 - p10_1 - p01_1\n}\n\n", "\nMIN {\n- p10_1 - p01_0 + 1\n}\n\n")))
    
    # with new version of optimize_effect:
    new_version_bound <- optimize_effect_2(obj)
    expect_equal(new_version_bound$bounds, c(lower = "\nMAX {\n  p00_0 - p00_1 - p10_1 - p01_1\n}\n", 
                                             upper = "\nMIN {\n  1 - p10_1 - p01_0\n}\n"))
    # visual comparison:
    #cat(bound$bounds) # old version output string
    #cat(new_version_bound$bounds) # new version output string
    
    ## compliers minus defiers effect
    
    eff <- "p{X(Z = 1) = 1; X(Z = 0) = 0} - p{X(Z = 1) = 0; X(Z = 0) = 1}"
    obj <- analyze_graph(b, constraints = NULL, effectt = eff)
    bound <- optimize_effect(obj)
    
    expect_true(all(bound$bounds == c("\nMAX {\np00_0 - p00_1 + p01_0 - p01_1\n}\n\n", "\nMIN {\np00_0 - p00_1 + p01_0 - p01_1\n}\n\n")))
    
    # with new version of optimize_effect:
    new_version_bound <- optimize_effect_2(obj)
    expect_equal(new_version_bound$bounds, c(lower = "\nMAX {\n  p00_0 - p00_1 + p01_0 - p01_1\n}\n", 
                                             upper = "\nMIN {\n  p00_0 - p00_1 + p01_0 - p01_1\n}\n"))
    # visual comparison:
    #cat(bound$bounds) # old version output string
    #cat(new_version_bound$bounds) # new version output string
    
    ## treatment effect among the treated? This one is not actually liner under the dag
    
    eff <- "p{Y(X = 1) = 1; X = 1} - p{Y(X = 0) = 1; X = 1}"
    obj <- analyze_graph(b, constraints = NULL, effectt = eff)
    bound <- optimize_effect(obj)
    
    # with new version of optimize_effect:
    new_version_bound <- optimize_effect_2(obj)
    #all(new_version_bound$bounds == c("\nMAX {\n  p00_0 - p00_1 - p10_0 - p10_1,\n  p00_0 - p00_1 - p10_1 - p01_1,\n  p00_0 - p00_1 + p10_0 - 2p10_1 - 2p01_1,\n  -p10_0 - p01_0,\n  -p10_1 - p01_1,\n  -p00_0 + p00_1 - 2p10_0 + p10_1 - 2p01_0,\n  -p00_0 + p00_1 - p10_0 - p10_1,\n  -p00_0 + p00_1 - p10_0 - p01_0\n}\n", "\nMIN {\n  1 - p10_1 - p01_0,\n  2 - p00_0 - p00_1 - p10_0 - p10_1 - 2p01_0,\n  1 + p00_0 + p10_0 - 2p10_1 - p01_1,\n  1 - p10_1 - p01_1,\n  1 - p10_0 - p01_0,\n  1 + p00_1 - 2p10_0 + p10_1 - p01_0,\n  1 - p10_0 - p01_1,\n  2 - p00_0 - p00_1 - p10_0 - p10_1 - 2p01_1\n}\n"))
    # visual comparison:
    #cat(bound$bounds) # old version output string
    #cat(new_version_bound$bounds) # new version output string
    
})


test_that("Mediator", {
    
    
    ## mediator X -> Z -> Y
    ## bounds should match https://onlinelibrary.wiley.com/doi/full/10.1111/j.1541-0420.2007.00949.x
    
    
    b <- readRDS(test_path("test-graphs", "mediator.RData"))
    # igraph::upgrade_graph(b)
    # saveRDS(object = b, file = test_path("test-graphs", "mediator.RData"))
    V(b)$nvals <- c(2,2,2,2)
    
    ## total effect: identifiable
    
    eff <- "p{Y(X = 1) = 1}"
    obj <- analyze_graph(b, constraints = NULL, effectt = eff)
    #bound1 <- optimize_effect(obj) # with old version
    # with new version of optimize_effect:
    new_version_bound1 <- optimize_effect_2(obj)
    
    expect_equal(new_version_bound1$bounds, 
                 c(lower = "\nMAX {\n  1 - p00_1 - p10_1\n}\n", upper= "\nMIN {\n  1 - p00_1 - p10_1\n}\n"))
    # visual comparison:
    #cat(bound1$bounds) # old version output string
    #cat(new_version_bound1$bounds) # new version output string
    
    eff <- "p{Y(X = 1, Z(X = 1)) = 1}"
    obj <- analyze_graph(b, constraints = NULL, effectt = eff)
    #bound2 <- optimize_effect(obj) # with old version
    # with new version of optimize_effect:
    new_version_bound2 <- optimize_effect_2(obj)
    # visual comparison:
    #cat(bound2$bounds) # old version output string
    #cat(new_version_bound2$bounds) # new version output string
    
    #all(bound1$bounds == bound2$bounds)
    expect_true(all(new_version_bound1$bounds == new_version_bound2$bounds)) # with new version
    
    ## controlled direct effect
    
    eff <- "p{Y(X = 1, Z = 0) = 1} - p{Y(X = 0, Z = 0) = 1}"
    obj <- analyze_graph(b, constraints = NULL, effectt = eff)
    #bound <- optimize_effect(obj) # with old version
    
    
    ## check against equation (3)
    #all(bound$bounds == c("\nMAX {\np00_0 + p01_1 - 1\n}\n\n", "\nMIN {\n- p00_1 - p01_0 + 1\n}\n\n"))
    
    # with new version of optimize_effect:
    new_version_bound <- optimize_effect_2(obj)
    expect_true(all(new_version_bound$bounds == c("\nMAX {\n  -1 + p00_0 + p01_1\n}\n", "\nMIN {\n  1 - p00_1 - p01_0\n}\n")))
    # visual comparison:
    #cat(bound$bounds) # old version output string
    #cat("\nMAX {\np00_0 + p01_1 - 1\n}\n\n", "\nMIN {\n- p00_1 - p01_0 + 1\n}\n\n")#
    #cat(new_version_bound$bounds) # new version output string
    
    ## with monotoncity
    
    
    mono2 <- list("Z(X = 1) >= Z(X = 0)",
                  "Y(X = 1, Z = 0) >= Y(X = 0, Z = 0)",
                  "Y(X = 1, Z = 1) >= Y(X = 0, Z = 1)",
                  "Y(X = 0, Z = 1) >= Y(X = 0, Z = 0)",
                  "Y(X = 1, Z = 1) >= Y(X = 1, Z = 0)")
    
    obj <- analyze_graph(b, constraints = mono2, effectt = eff)
    #bound <- optimize_effect(obj) # with old version
    
    ## check againts equation (5)
    #all(bound$bounds == c("\nMAX {\n- p01_0 + p01_1\n0\n}\n\n", "\nMIN {\n- p00_1 - p10_1 - p01_0 + 1\n}\n\n"))
    
    # with new version of optimize_effect:
    new_version_bound <- optimize_effect_2(obj)
    expect_true(all(new_version_bound$bounds == c("\nMAX {\n  -p01_0 + p01_1,\n  0\n}\n", "\nMIN {\n  1 - p00_1 - p10_1 - p01_0\n}\n")))
    # visual comparison:
    #cat(bound$bounds) # old version output string
    #cat("\nMAX {\n- p01_0 + p01_1\n0\n}\n\n", "\nMIN {\n- p00_1 - p10_1 - p01_0 + 1\n}\n\n")#
    #cat(new_version_bound$bounds) # new version output string
    
    ## natural direct effect
    
    eff <- "p{Y(X = 1, Z(X = 0)) = 1} - p{Y(X = 0, Z(X = 0)) = 1}"
    
    obj <- analyze_graph(b, constraints = NULL, effectt = eff)
    #bound <- optimize_effect(obj) # with old version
    # with new version of optimize_effect:
    new_version_bound <- optimize_effect_2(obj)
    #all(new_version_bound$bounds == c("min-bound: MAX {\n  -p00_1 + p10_0 - p10_1 - p01_0 - p01_1,\n  -2 + 2p00_0 + p10_0 + p01_0 + p01_1,\n  -1 + p00_0 + p10_0\n}\n", "max-bound: MIN {\n  2p00_0 + p10_0 - p10_1 + p01_0,\n  p00_0 + p10_0,\n  1 - p00_1 + p10_0 - p01_0\n}\n"))
    # visual comparison:
    #cat(bound$bounds) # old version output string
    #cat(new_version_bound$bounds) # new version output string
    
    obj <- analyze_graph(b, constraints = mono2, effectt = eff)
    #bound <- optimize_effect(obj) # with old version
    # with new version of optimize_effect:
    new_version_bound <- optimize_effect_2(obj)
    #all(new_version_bound$bounds == c("min-bound: MAX {\n  p10_0 - p10_1 - p01_0 + p01_1,\n  p10_0 - p10_1,\n  -p01_0 + p01_1,\n  0\n}\n", "max-bound: MIN {\n  p00_0 - p00_1 + p10_0 - p10_1\n}\n"))
    # visual comparison:
    #cat(bound$bounds) # old version output string
    #cat(new_version_bound$bounds) # new version output string
    
    ## compare to Arvids paper 2009 
    
    
    obj <- analyze_graph(b, constraints = list("Z(X=0)<=Z(X=1)"), effectt = eff)
    #bound <- optimize_effect(obj) # with old version
    # with new version of optimize_effect:
    new_version_bound <- optimize_effect_2(obj)
    #all(new_version_bound$bounds == c("min-bound: MAX {\n  p10_0 - p10_1 - p01_0 + p01_1,\n  -1 + p00_0 + p10_0 + p01_1\n}\n", "max-bound: MIN {\n  p00_0 - p00_1 + p10_0,\n  2p00_0 - 2p00_1 + p10_0 - p10_1 + p01_0 - p01_1\n}\n"))
    # visual comparison:
    #cat(bound$bounds) # old version output string
    #cat(new_version_bound$bounds) # new version output string
    
})


test_that("Multiple IV numeric comparison", {
    
    
    ## comparison of multiple IV bounds results
    b <- graph_from_literal(Z1 -+ X, Z2 -+ X, Z2 -+ Z1, Ul -+ Z1, Ul -+ Z2, X -+ Y, Ur -+ X, Ur -+ Y)
    V(b)$leftside <- c(1, 0, 1, 1, 0, 0)
    V(b)$latent <- c(0, 0, 0, 1, 0, 1)
    V(b)$nvals <- c(2, 2, 2, 2, 2, 2)
    E(b)$rlconnect <- c(0, 0, 0, 0, 0, 0, 0, 0)
    E(b)$edge.monotone <- c(0, 0, 0, 0, 0, 0, 0, 0)
    obj <- analyze_graph(b, constraints = NULL, effectt = "p{Y(X = 1) = 1} - p{Y(X = 0) = 1}")
    
    mivold <- readRDS(test_path("test-graphs", "MIV-bounds-result.RData"))
    mivnew <- optimize_effect_2(obj)
    
    f2new <- interpret_bounds(mivnew$bounds, obj$parameters)
    f2old <- interpret_bounds(mivold$bounds, obj$parameters)
    
    sim.qs <- rbeta(length(obj$variables), .05, 1)
    sim.qs <- sim.qs / sum(sim.qs)
    
    names(sim.qs) <- obj$variables
    
    inenv <- new.env()
    for(j in 1:length(sim.qs)) {
        
        assign(names(sim.qs)[j], sim.qs[j], inenv)
        
    }
    res <- lapply(as.list(obj$constraints[-1]), function(x){
        x1 <- strsplit(x, " = ")[[1]]
        x0 <- paste(x1[1], " = ", x1[2])
        eval(parse(text = x0), envir = inenv)
    })
    
    params <- lapply(obj$parameters, function(x) get(x, envir = inenv))
    names(params) <- obj$parameters
    
    
    expect_equal(do.call(f2new, params),  do.call(f2old, params))
    
})


test_that("Missing paths filling in works properly", {
    
    
    b2 <- graph_from_literal(X -+ Y, Ul -+ X, X -+ M1, X -+ M2, M1 -+ Y, M2 -+ Y,
                             Ur -+ M1, Ur -+ M2, Ur -+ Y, M1 -+ M2)
    V(b2)$leftside <- c(1, 0, 1, 0, 0, 0)
    V(b2)$latent <- c(0, 0, 1, 0, 0, 1)
    V(b2)$nvals <- c(2, 2, 2, 2, 2, 2)
    E(b2)$rlconnect <- rep(0, 10)
    E(b2)$edge.monotone <- rep(0, 10)
    
    nofill <- "p{Y(X = 1, M1 = 1, M2(X = 1, M1(X = 1))) = 1}"
    nofill <- "p{Y(X = 1, M1 = 1, M2(X = 1, M1 = 1)) = 1}"
    eff2 <- parse_effect(nofill)$vars[[1]][[1]]
    
    thisintervene <- unlist(causaloptim:::list_to_path(eff2, "Y"))
    basevars <- sapply(strsplit(names(thisintervene), " -> "), "[", 1)
    ## check for missing paths from intervention sets to outcome
    outcome <- V(b2)[2]
    parents <- adjacent_vertices(b2, v = outcome, mode = "in")[[1]]
    expect_true(length(setdiff(names(parents[which(names(parents) != "Ur")]), 
                               names(eff2))) == 0)
    if(length(setdiff(names(parents[which(names(parents) != "Ur")]), 
                      names(eff2))) > 0) {
        
        isets <- unique(causaloptim:::btm_var(eff2))
        missingpaths <- lapply(isets, function(cc) {
            allpaths <- igraph::all_simple_paths(b2, from = cc, to = "Y", mode = "out")
            paths2 <- unlist(lapply(allpaths, function(x) paste(names(x), collapse = " -> ")))
            setdiff(paths2, names(thisintervene))
        })
        
    }
    
    b1 <- graph_from_literal(X -+ M1, M1 -+ Y, X -+ Y, Ul -+ X, Ur -+ M1, Ur -+ Y)
    V(b1)$leftside <- c(1, 0, 0, 1, 0)
    V(b1)$latent <- c(0, 0, 0, 1, 1)
    V(b1)$nvals <- c(2, 2, 2, 2, 2)
    E(b1)$rlconnect <- rep(0, 6)
    E(b1)$edge.monotone <- rep(0, 6)
    
    fill <- "p{Y(X = 1) = 1}"
    eff1 <- parse_effect(fill)$vars[[1]][[1]]
    outcome <- V(b1)[3]
    thisintervene <- unlist(causaloptim:::list_to_path(eff1, "Y"))
    basevars <- sapply(strsplit(names(thisintervene), " -> "), "[", 1)
    ## check for missing paths from intervention sets to outcome
    ## only do this if any of the top level intervention sets doesn't contain all
    ## parents of the outcome
    ## the logic being that if the user wrote that as an effect, then 
    ## the intention was to propogate that intervention set forward through
    ## all paths in the graph to the outcome
    
    parents <- adjacent_vertices(b1, v = outcome, mode = "in")[[1]]
    if(length(setdiff(names(parents[which(names(parents) != "Ur")]), 
                      names(eff1))) > 0) {
        isets <- unique(causaloptim:::btm_var(eff1))
        missingpaths <- lapply(isets, function(cc) {
            allpaths <- igraph::all_simple_paths(b1, from = cc, to = "Y", mode = "out")
            paths2 <- unlist(lapply(allpaths, function(x) paste(names(x), collapse = " -> ")))
            setdiff(paths2, names(thisintervene))
        })
        expect_equal(missingpaths[[1]], "X -> M1 -> Y")
    }
    
    
})

