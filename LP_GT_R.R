
################ Code for Question 2 #####################
library(lpSolveAPI)

clothfactory <- make.lp(9, 9) # 9 variables, 9 constraints
lp.control(clothfactory, sense= "maximize")
set.objfn(clothfactory, c(25,21,25,10,6,10,5,1,5)) # set objective function

#Constraints
set.row(clothfactory, 1, c(1,1,1), indices = c(1,4,7))
set.row(clothfactory, 2, c(1,1,1), indices = c(2,5,8))
set.row(clothfactory, 3, c(1,1,1), indices = c(3,6,9))
set.row(clothfactory, 4, c(0.45,-0.55,-0.55), indices = c(1,4,7))
set.row(clothfactory, 5, c(0.55,-0.45,-0.45), indices = c(2,5,8))
set.row(clothfactory, 6, c(0.7,-0.3,-0.3), indices = c(3,6,9))
set.row(clothfactory, 7, c(-0.3,0.7,-0.3), indices = c(1,4,7))
set.row(clothfactory, 8, c(-0.4,0.6,-0.4), indices = c(2,5,8))
set.row(clothfactory, 9, c(-0.5,0.5,-0.5), indices = c(3,6,9))

set.rhs(clothfactory, c(4800,3000,3500,0,0,0,0,0,0))
set.constr.type(clothfactory, c("<=",	"<=","<=",">=",">=",">=",">=",">=",">="))

#Bounds
set.type(clothfactory, c(1:9),"real")
set.bounds(clothfactory, lower = rep(0, 9), upper = rep(Inf, 9))

#Solve the model
solve(clothfactory) # http://lpsolve.sourceforge.net/5.5/solve.htm
clothfactory
objvalue<-get.objective(clothfactory)
objvalue #Solution/Maximum profit

#Material in tons to be used for the production
decisionVariables<-get.variables(clothfactory)
decisionVariables

# Verify the quantities 
sum(decisionVariables[c(1,4,7)])     # Quantity Equals maximum demand of Spring
sum(decisionVariables[c(2,5,8)])     # Quantity Equals maximum demand of Autumn
sum(decisionVariables[c(3,6,9)])     # Quantity Equals maximum demand of Winter

#################### Code for Question 3 ##############################

############ Part 1: Helen's game ##############

library(lpSolveAPI)

chips <- make.lp(0, 8)

lp.control(chips, sense= "maximize")  #Player 1, hence sense is maximize

set.objfn(chips, c(0, 0, 0, 0, 0, 0, 0, 1))

add.constraint(chips, c(-1, -2, -1, 0, -1, -2, -1, 1), "<=", 0) #David: Strategy 1
add.constraint(chips, c(0, -1, -2, -2, -2, -1, 0, 1), "<=", 0) #David: Strategy 2
add.constraint(chips, c(0, 0, -2, -4, -2, 0, 0, 1), "<=", 0) #David: Strategy 3
add.constraint(chips, c(0, -1, -2, -2, -2, -1, 0, 1), "<=", 0) #David: Strategy 4
add.constraint(chips, c(-1, -2, -1, 0, -1, -2, -1, 1), "<=", 0) #David: Strategy 5

add.constraint(chips, c(1,1,1,1,1,1,1,0), "=", 1)    #Sum of probability

set.bounds(chips, lower = c(0, 0, 0, 0, 0, 0, 0, -Inf))

RowNames <- c("Row1", "Row2", "Row3","Row4", "Row5", "Row6")

ColNames <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "v")

dimnames(chips) <- list(RowNames, ColNames)

solve(chips) # http://lpsolve.sourceforge.net/5.5/solve.htm

get.objective(chips) #Value of the game
get.variables(chips) #Probability distribution for Helen's strategies
get.constraints(chips) 
chips

############ Part 2: David's game ##############

library(lpSolveAPI)

chips <- make.lp(0, 6)
lp.control(chips, sense= "minimize")  #Player 2, hence sense is minimize
set.objfn(chips, c(0, 0, 0, 0, 0, 1)) 

add.constraint(chips, c(-1, 0, 0, 0, -1, 1), ">=", 0) #Helen uses strategy 1
add.constraint(chips, c(-2, -1, 0, -1, -2, 1), ">=", 0) #Helen uses strategy 2
add.constraint(chips, c(-1, -2, -2, -2, -1, 1), ">=", 0) #Helen uses strategy 3
add.constraint(chips, c(0, -2, -4, -2, 0, 1), ">=", 0) #Helen uses strategy 4
add.constraint(chips, c(-1, -2, -2, -2, -1, 1), ">=", 0) #Helen uses strategy 5
add.constraint(chips, c(-2, -1, 0, -1, -2, 1), ">=", 0) #Helen uses strategy 6
add.constraint(chips, c(-1, 0, 0, 0, -1, 1), ">=", 0) #Helen uses strategy 7

add.constraint(chips, c(1,1,1,1,1,0), "=", 1) #Sum of probability

set.bounds(chips, lower = c(0, 0, 0, 0, 0, -Inf))

RowNames <- c("Row1", "Row2", "Row3","Row4", "Row5", "Row6", "Row7","Row8" )
ColNames <- c("y1", "y2", "y3", "y4", "y5", "v")
dimnames(chips) <- list(RowNames, ColNames)

solve(chips) # http://lpsolve.sourceforge.net/5.5/solve.htm
get.objective(chips) #Value of the game
get.variables(chips) #Probability distribution
get.constraints(chips)
chips