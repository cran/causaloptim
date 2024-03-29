## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(causaloptim)

## -----------------------------------------------------------------------------
b <- graph_from_literal(X -+ Y, Ur -+ X, Ur -+ Y)
V(b)$leftside <- c(0,0,0)
V(b)$latent <- c(0,0,1)
V(b)$nvals <- c(3,2,2)
E(b)$rlconnect <- E(b)$edge.monotone <- c(0, 0, 0)

obj <- analyze_graph(b, constraints = NULL, effectt = "p{Y(X = 1) = 1} - p{Y(X = 0) = 1}")
optimize_effect_2(obj)


obj2 <- analyze_graph(b, constraints = NULL, effectt = "p{Y(X = 2) = 1} - p{Y(X = 0) = 1}")
optimize_effect_2(obj2)


obj3 <- analyze_graph(b, constraints = NULL, effectt = "p{Y(X = 2) = 1} - p{Y(X = 1) = 1}")
optimize_effect_2(obj3)

## ----eval = FALSE-------------------------------------------------------------
#  b <- graph_from_literal(Z1 -+ X, Z2 -+ X, Z2 -+ Z1, Ul -+ Z1, Ul -+ Z2,
#                          X -+ Y, Ur -+ X, Ur -+ Y)
#  V(b)$leftside <- c(1, 0, 1, 1, 0, 0)
#  V(b)$latent <- c(0, 0, 0, 1, 0, 1)
#  V(b)$nvals <- c(2, 2, 2, 2, 2, 2)
#  E(b)$rlconnect <- c(0, 0, 0, 0, 0, 0, 0, 0)
#  E(b)$edge.monotone <- c(0, 0, 0, 0, 0, 0, 0, 0)
#  
#  obj <- analyze_graph(b, constraints = NULL, effectt = "p{Y(X = 1) = 1} - p{Y(X = 0) = 1}")
#  
#  bounds.multi <- optimize_effect_2(obj)
#  
#  b2 <- graph_from_literal(Z1 -+ X, Ul -+ Z1,
#                           X -+ Y, Ur -+ X, Ur -+ Y)
#  V(b2)$leftside <- c(1, 0, 1, 0, 0)
#  V(b2)$latent <- c(0, 0, 1, 0, 1)
#  V(b2)$nvals <- c(2, 2, 2, 2, 2)
#  E(b2)$rlconnect <- c(0, 0,  0, 0, 0)
#  E(b2)$edge.monotone <- c(0, 0, 0, 0, 0)
#  
#  
#  ## single instrument
#  obj2 <- analyze_graph(b2, constraints = NULL, effectt = "p{Y(X = 1) = 1} - p{Y(X = 0) = 1}")
#  bounds.sing <- optimize_effect_2(obj2)
#  
#  
#  b3 <- graph_from_literal(Z3 -+ X, Ul -+ Z3,
#                           X -+ Y, Ur -+ X, Ur -+ Y)
#  V(b3)$leftside <- c(1, 0, 1, 0, 0)
#  V(b3)$latent <- c(0, 0, 1, 0, 1)
#  V(b3)$nvals <- c(4, 2, 2, 2, 2)
#  E(b3)$rlconnect <- c(0, 0,  0, 0, 0)
#  E(b3)$edge.monotone <- c(0, 0, 0, 0, 0)
#  
#  
#  ## single instrument
#  obj3 <- analyze_graph(b3, constraints = NULL, effectt = "p{Y(X = 1) = 1} - p{Y(X = 0) = 1}")
#  bounds.quad <- optimize_effect_2(obj3)
#  
#  
#  joint <- function(df, alpha, pUr, pUl) {
#  
#    Z1 <- df$Z1
#    Z2 <- df$Z2
#    X <- df$X
#    Y <- df$Y
#  
#    pUr * pUl * (((pnorm(alpha[1] + alpha[2] * 1)) ^ Z1 * (1 - pnorm(alpha[1] + alpha[2] * 1)) ^ (1 - Z1)) *
#                   ((pnorm(alpha[3] + alpha[4] * 1 + alpha[5] * Z1)) ^ Z2 *
#                      (1 - pnorm(alpha[3] + alpha[4] * 1 + alpha[5] * Z1)) ^ (1 - Z2)) *
#                   ((pnorm(alpha[6] + alpha[7] * Z1 + alpha[8] * Z2 + alpha[9] * 1)) ^ X *
#                      (1 - pnorm(alpha[6] + alpha[7] * Z1 + alpha[8] * Z2 + alpha[9] * 1)) ^ (1 - X)) *
#                   (pnorm(alpha[10] + alpha[11] * X + alpha[12] * 1)) ^ Y *
#                   (1 - pnorm(alpha[10] + alpha[11] * X + alpha[12] * 1)) ^ (1 - Y)) +
#      pUr * (1 - pUl) * (((pnorm(alpha[1] + alpha[2] * 0)) ^ Z1 * (1 - pnorm(alpha[1] + alpha[2] * 0)) ^ (1 - Z1)) *
#                           ((pnorm(alpha[3] + alpha[4] * 0 + alpha[5] * Z1)) ^ Z2 *
#                              (1 - pnorm(alpha[3] + alpha[4] * 0 + alpha[5] * Z1)) ^ (1 - Z2)) *
#                           ((pnorm(alpha[6] + alpha[7] * Z1 + alpha[8] * Z2 + alpha[9] * 1)) ^ X *
#                              (1 - pnorm(alpha[6] + alpha[7] * Z1 + alpha[8] * Z2 + alpha[9] * 1)) ^ (1 - X)) *
#                           (pnorm(alpha[10] + alpha[11] * X + alpha[12] * 1)) ^ Y *
#                           (1 - pnorm(alpha[10] + alpha[11] * X + alpha[12] * 1)) ^ (1 - Y)) +
#      (1 - pUr) * pUl * (((pnorm(alpha[1] + alpha[2] * 1)) ^ Z1 * (1 - pnorm(alpha[1] + alpha[2] * 1)) ^ (1 - Z1)) *
#                           ((pnorm(alpha[3] + alpha[4] * 1 + alpha[5] * Z1)) ^ Z2 *
#                              (1 - pnorm(alpha[3] + alpha[4] * 1 + alpha[5] * Z1)) ^ (1 - Z2)) *
#                           ((pnorm(alpha[6] + alpha[7] * Z1 + alpha[8] * Z2 + alpha[9] * 0)) ^ X *
#                              (1 - pnorm(alpha[6] + alpha[7] * Z1 + alpha[8] * Z2 + alpha[9] * 0)) ^ (1 - X)) *
#                           (pnorm(alpha[10] + alpha[11] * X + alpha[12] * 0)) ^ Y *
#                           (1 - pnorm(alpha[10] + alpha[11] * X + alpha[12] * 0)) ^ (1 - Y)) +
#      (1 - pUr) * (1 - pUl) * (((pnorm(alpha[1] + alpha[2] * 0)) ^ Z1 * (1 - pnorm(alpha[1] + alpha[2] * 0)) ^ (1 - Z1)) *
#                                 ((pnorm(alpha[3] + alpha[4] * 0 + alpha[5] * Z1)) ^ Z2 *
#                                    (1 - pnorm(alpha[3] + alpha[4] * 0 + alpha[5] * Z1)) ^ (1 - Z2)) *
#                                 ((pnorm(alpha[6] + alpha[7] * Z1 + alpha[8] * Z2 + alpha[9] * 0)) ^ X *
#                                    (1 - pnorm(alpha[6] + alpha[7] * Z1 + alpha[8] * Z2 + alpha[9] * 0)) ^ (1 - X)) *
#                                 (pnorm(alpha[10] + alpha[11] * X + alpha[12] * 0)) ^ Y *
#                                 (1 - pnorm(alpha[10] + alpha[11] * X + alpha[12] * 0)) ^ (1 - Y))
#  
#  
#  }
#  
#  
#  ## get conditional probabilities
#  ## key = XY_Z1Z2
#  
#  get_cond_probs <- function(p.vals) {
#  
#    z1z2.joint <- unique(p.vals[, c("Z1", "Z2")])
#    for(j in 1:nrow(z1z2.joint)) {
#      z1z2.joint$Prob.condz1z2[j] <- sum(subset(p.vals, Z1 == z1z2.joint[j, "Z1"] & Z2 == z1z2.joint[j, "Z2"])$Prob)
#  
#    }
#  
#    p.vals.2 <- merge(p.vals, z1z2.joint, by = c("Z1", "Z2"), sort = FALSE)
#  
#    p.vals.2$Prob.cond.fin <- ifelse(p.vals.2$Prob ==0, 0.0, p.vals.2$Prob / p.vals.2$Prob.condz1z2)
#    res <- as.list(p.vals.2$Prob.cond.fin)
#    names(res) <- with(p.vals.2, paste0("p", X, Y, "_", Z1, Z2))
#  
#    ## conditional on Z1 only
#  
#    xyz1.joint <- unique(p.vals[, c("Z1", "X", "Y")])
#    for(j in 1:nrow(xyz1.joint)) {
#  
#      xyz1.joint$Prob.xyz1[j] <- sum(subset(p.vals, Z1 == xyz1.joint$Z1[j] &
#                                              X == xyz1.joint$X[j] & Y == xyz1.joint$Y[j])$Prob)
#  
#    }
#  
#    z1.marg0 <- sum(subset(xyz1.joint, Z1 == 0)$Prob.xyz1)
#    z1.marg1 <-   sum(subset(xyz1.joint, Z1 == 1)$Prob.xyz1)
#  
#    xyz1.joint$Prob.z1[xyz1.joint$Z1 == 0] <- z1.marg0
#    xyz1.joint$Prob.z1[xyz1.joint$Z1 == 1] <- z1.marg1
#  
#    xyz1.joint$Prob.cond <- with(xyz1.joint, Prob.xyz1 / Prob.z1)
#    res2 <- as.list(xyz1.joint$Prob.cond)
#    names(res2) <- with(xyz1.joint, paste0("p", X, Y, "_", Z1))
#  
#  
#    ## conditioning on Z3
#  
#    z3.joint <- unique(p.vals[, c("Z3"), drop = FALSE])
#    for(j in 1:nrow(z3.joint)) {
#      z3.joint$Prob.condz3[j] <- sum(subset(p.vals, Z3 == z3.joint[j, "Z3"])$Prob)
#    }
#  
#    p.vals.3 <- merge(p.vals, z3.joint, by = c("Z3"), sort = FALSE)
#  
#    p.vals.3$Prob.cond.fin <- ifelse(p.vals.3$Prob ==0, 0.0, p.vals.3$Prob / p.vals.3$Prob.condz3)
#    res3 <- as.list(p.vals.3$Prob.cond.fin)
#    names(res3) <- with(p.vals.3, paste0("p", X, Y, "_", Z3))
#  
#  
#    list(multi = res,
#         sing = res2,
#         quad = res3)
#  
#  }
#  
#  
#  
#  ## simulate and compare the two
#  nsim <- 50000
#  f.multi <- interpret_bounds(bounds.multi$bounds, obj$parameters)
#  f.single <- interpret_bounds(bounds.sing$bounds, obj2$parameters)
#  f.quad <- interpret_bounds(bounds.quad$bounds, obj3$parameters)
#  
#  result <- matrix(NA, ncol = 9, nrow = nsim)
#  
#  set.seed(211129)
#  for (i in 1:nsim) {
#  
#    alpha <- rnorm(12, sd = 2)
#    pUr <- runif(1)
#    pUl <- runif(1)
#  
#    p.vals.joint <- obj$p.vals
#    p.vals.joint$Prob <- joint(p.vals.joint, alpha, pUr, pUl)
#  
#    p.vals.joint$Z3 <- with(p.vals.joint, ifelse(Z1 == 0 & Z2 == 0, 0,
#                                                 ifelse(Z1 == 0 & Z2 == 1, 1,
#                                                        ifelse(Z1 == 1 & Z2 == 0, 2,
#                                                               3))))
#  
#    if(any(p.vals.joint$Prob == 0)) next
#  
#    condprobs <- get_cond_probs(p.vals.joint)
#  
#    bees <- do.call(f.multi, condprobs$multi)
#    bees.sing <- do.call(f.single, condprobs$sing)
#    bees.quad <- do.call(f.quad, condprobs$quad)
#  
#    result[i, ] <- unlist(c(sort(unlist(bees)), abs(bees[2] - bees[1]),
#                            sort(unlist(bees.sing)), abs(bees.sing[2]- bees.sing[1]),
#                            sort(unlist(bees.quad)), abs(bees.quad[2]- bees.quad[1])))
#  
#  }
#  colnames(result) <- c("bound.lower",
#                        "bound.upper", "width.multi",
#                        "bound.lower.single", "bound.upper.single", "width.single",
#                        "bound.lower.quad", "bound.upper.quad", "width.quad")
#  bounds.comparison <- as.data.frame(result)
#  
#  #pdf("figsim.pdf", width = 8, height = 4.25, family = "serif")
#  par(mfrow = c(1,2))
#  plot(width.multi ~ width.single, data = bounds.comparison, pch = 20, cex = .3,
#       xlim = c(0, 1), ylim = c(0, 1), xlab= "Single IV", ylab = "Two binary IV/Single 4-level IV",
#       main = "Width of bounds intervals")
#  abline(0, 1, lty = 3)
#  
#  plot(bound.lower.quad ~ bound.lower, data = bounds.comparison[1:100,], pch = 20, cex = 1,
#       xlim = c(-1, 1), ylim = c(-1, 1), xlab = "Two binary IV", ylab = "Single 4-level IV",
#       main = "Bounds values")
#  points(bound.upper.quad ~ bound.upper, data = bounds.comparison[1:100,], pch = 1, cex = 1)
#  
#  legend("bottomright", pch = c(1, 20), legend = c("upper", "lower"))
#  #dev.off()
#  
#  summary(bounds.comparison) # contains 467 NA's to avoid division by 0
#  # Verify that a single quad-level instrument yield the same bounds as two linked binary ones.
#  all(round(x = bounds.comparison$bound.lower, digits = 12) ==
#          round(x = bounds.comparison$bound.lower.quad, digits = 12) &&
#          round(x = bounds.comparison$bound.upper, digits = 12) ==
#          round(x = bounds.comparison$bound.upper.quad, digits = 12),
#      na.rm = TRUE)
#  

## -----------------------------------------------------------------------------
b <- graph_from_literal(Ul -+ X -+ Y -+ Y2, Ur -+ Y, Ur -+ Y2)
V(b)$leftside <- c(1, 1, 0, 0, 0)
V(b)$latent <- c(1, 0, 1, 0, 1)
V(b)$nvals <- c(2, 2, 2, 2, 2)
E(b)$rlconnect <- c(0, 0, 0, 0, 0)
E(b)$edge.monotone <- c(0, 0, 0, 0, 0)

obj <- analyze_graph(b, constraints = "Y2(Y = 1) >= Y2(Y = 0)",
                     effectt = "p{Y(X = 1) = 1} - p{Y(X = 0) = 1}")

optimize_effect_2(obj)

