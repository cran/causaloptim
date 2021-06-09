## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(causaloptim)

## ----Multiple_Instruments_Image, echo = FALSE, fig.align = "center", out.width = "40%", fig.cap = "Benchmark Example. Multiple Instruments."----
knitr::include_graphics("TwoIVs.png")

## ----Multiple_Instruments_Code, eval = TRUE-----------------------------------
library(causaloptim)
library(igraph)
b <- graph_from_literal(Z1 -+ X, Z2 -+ X, Z2 -+ Z1, Ul -+ Z1, Ul -+ Z2, X -+ Y, Ur -+ X, Ur -+ Y)
V(b)$leftside <- c(1, 0, 1, 1, 0, 0)
V(b)$latent <- c(0, 0, 0, 1, 0, 1)
E(b)$rlconnect <- c(0, 0, 0, 0, 0, 0, 0, 0)
E(b)$edge.monotone <- c(0, 0, 0, 0, 0, 0, 0, 0)
obj <- analyze_graph(b, constraints = NULL, effectt = "p{Y(X = 1) = 1} - p{Y(X = 0) = 1}")

## ----Running_Time_Pre, eval = FALSE-------------------------------------------
#  system.time(oldbnds <- optimize_effect(obj))
#  #>     user  system  elapsed
#  #> 31093.57   47.02 61368.22

## ----Running_Time_Post, eval = FALSE------------------------------------------
#  system.time(newbnds <- optimize_effect_2(obj))
#  #>  user  system  elapsed
#  #> 0.139   0.001    0.140

## ---- include = FALSE---------------------------------------------------------

oldbnds <- readRDS("MIV-bounds-result.RData")
newbnds <- optimize_effect_2(obj)


## ---- eval = TRUE-------------------------------------------------------------
eval_newbnds <- interpret_bounds(newbnds$bounds, obj$parameters)
eval_oldbnds <- interpret_bounds(oldbnds$bounds, obj$parameters)

## ---- eval = TRUE-------------------------------------------------------------
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

## ---- eval = TRUE-------------------------------------------------------------
do.call(eval_newbnds, params) 
do.call(eval_oldbnds, params)

## ----User_Input_DAG_D, echo = FALSE, fig.align = "center", out.width = "40%", fig.cap = "Original Example: User Input DAG $D$."----
knitr::include_graphics("User_Input_DAG.png")

## ----Causal_Graph_G, echo = FALSE, fig.align = "center", out.width = "40%", fig.cap = "Original Example: Causal Graph $G$."----
knitr::include_graphics("Causal_Graph.png")

## ----Response_Function_Variables, echo = FALSE, fig.align = "center", out.width = "30%", fig.cap = "Original Example: Response Function Variables."----
knitr::include_graphics("Canonical_Partitions.png")

## ----Dual_from_Primal, eval = FALSE-------------------------------------------
#  a1 <- rbind(cbind(t(A_l), t(A_e)),
#              cbind(diag(x = 1, nrow = m_l, ncol = m_l), matrix(data = 0, nrow = m_l, ncol = m_e)))
#  b1 <- rbind(c0,
#              matrix(data = 0, nrow = m_l, ncol = 1))
#  if (opt == "max") {
#    a1 <- -a1
#    b1 <- -b1
#  }

## ----Vertex_Enumeration, eval = FALSE-----------------------------------------
#  library(rcdd)
#  hrep <- makeH(a1 = a1, b1 = b1)
#  vrep <- scdd(input = hrep)
#  matrix_of_vrep <- vrep$output
#  indices_of_vertices <- matrix_of_vrep[ , 1] == 0 & matrix_of_vrep[ , 2] == 1
#  vertices <- matrix_of_vrep[indices_of_vertices, -c(1, 2), drop = FALSE]

## ----Potential_Optima, eval = FALSE-------------------------------------------
#  expressions <- apply(vertices, 1, function(y) evaluate_objective(c1_num = c1_num, p = p, y = y))

