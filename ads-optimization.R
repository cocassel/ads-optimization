library(gurobi)
library(igraph)

####################################### DATA PREPARATION #########################################

set.seed(0017)

# Generate number of queries
m = sample(20:25, 1)
m

# Generate number of times each query is asked
numTimesQueryAsked = vector("numeric", m)
for(i in 1:m) {
  numTimesQueryAsked[i] = sample(5:20, 1)
}
numTimesQueryAsked

# Generate number of advertisers
n = sample(15:20, 1)
n

# Generate quality scores
qualityScores = matrix(nrow = m, ncol = n)
for(i in 1:m) {
  for(j in 1:n) {
    qualityScores[i,j] = sample(1:10, 1)
  }
}
qualityScores

# Generate bids
bids = matrix(nrow = m, ncol = n)
for(i in 1:m) {
  for(j in 1:n) {
    bids[i,j] = sample(0:25, 1)
  }
}
bids

# Generate budgets
budgets = vector("numeric", n)
for(j in 1:n) {
  budgets[j] = sample(20:100, 1)
}
budgets

# Save generated data to CSV 
write.csv(numTimesQueryAsked, file="/Users/Celeste/Documents/2020/4B/MSCI 433/Assignment 4/numTimesQueryAsked.csv", row.names=FALSE)
write.csv(qualityScores, file="/Users/Celeste/Documents/2020/4B/MSCI 433/Assignment 4/qualityScores.csv", row.names=FALSE)
write.csv(bids, file="/Users/Celeste/Documents/2020/4B/MSCI 433/Assignment 4/bids.csv", row.names=FALSE)
write.csv(budgets, file="/Users/Celeste/Documents/2020/4B/MSCI 433/Assignment 4/budgets.csv", row.names=FALSE)

# Read in data from CSV
numTimesQueryAsked=as.matrix(read.csv("/Users/Celeste/Documents/2020/4B/MSCI 433/Assignment 4/numTimesQueryAsked.csv"))
qualityScores=as.matrix(read.csv("/Users/Celeste/Documents/2020/4B/MSCI 433/Assignment 4/qualityScores.csv"))
bids=as.matrix(read.csv("/Users/Celeste/Documents/2020/4B/MSCI 433/Assignment 4/bids.csv"))
budgets=as.matrix(read.csv("/Users/Celeste/Documents/2020/4B/MSCI 433/Assignment 4/budgets.csv"))

m = length(numTimesQueryAsked)
n = length(budgets)

############################################ PART 1 ##################################################

# ----------------------------------------- PART 1.1 ------------------------------------------------

# Number of slates for each keyword for slates of size 2
numSlates = vector("numeric", m)
for(i in 1:m) {
  # Number of non-zero bids is the number of advertisers in our bidding landscape
  nonZeroBids = sum(bids[i,] != 0)
  # If we only show one ad on our slate, we can show any one of the bidders
  numOf1AdSlates = nonZeroBids
  # If we show two ads on our slate, they have to be in the order of the bidding landscape
  # choose 2 from bidding landscape
  numOf2AdSlates = choose(nonZeroBids, 2)
  numSlates[i] = numOf1AdSlates + numOf2AdSlates
}
# Number of slates possible for each keyword using a max of 2 ads
numSlates
# Total number of slates possible
sum(numSlates)

# Number of slates for each keyword for slates of size 3
numSlates3 = vector("numeric", m)
for(i in 1:m) {
  # Number of non-zero bids is the number of advertisers in our bidding landscape
  nonZeroBids = sum(bids[i,] != 0)
  # If we only show one ad on our slate, we can show any one of the bidders
  numOf1AdSlates = nonZeroBids
  # If we show two ads on our slate, they have to be in the order of the bidding landscape
  # choose 2 from bidding landscape
  numOf2AdSlates = choose(nonZeroBids, 2)
  # If we show three ads on our slate, they have to be in the order of the bidding landscape
  # choose 3 from bidding landscape
  numOf3AdSlates = choose(nonZeroBids, 3)
  numSlates3[i] = numOf1AdSlates + numOf2AdSlates + numOf3AdSlates
}
# Number of slates possible for each keyword using a max of 3 ads
numSlates3
# Total number of slates possible
sum(numSlates3)

# ----------------------------------------- PART 1.2 ------------------------------------------------

# Clickthrough rates for 1st and 2nd positions
ctr1 = 0.05
ctr2 = 0.025

# Calculate bids*quality scores
scores = matrix(nrow = m, ncol = n)
for(i in 1:m) {
  for(j in 1:n) {
    scores[i,j] = bids[i,j]*qualityScores[i,j]
  }
}

# Get the bidding landscapes for each query
# Rows are queries and columns are spots in the bidding landscape
# Elements are advertisers (or zeroes if spots are unused)
biddingLandscapes = matrix(nrow = 0, ncol = n)
for(i in 1:m) {
  advertisers = c(1:n)
  query = rbind(advertisers,scores[i,])
  query = t(query)
  
  # Order scores from highest to lowest
  # ASSUMPTION: Tied scores can be ordered arbitrarily
  query = t(query[order(query[,2], decreasing = TRUE),])
  # Advertisers with scores of 0 (i.e. those who do not bid) are not in the bidding landscape
  # We represent these advertisers with 0s in the bidding landscape matrix
  for (j in 1:n) {
    if(query[2,j] == 0) {
      query[1,j] = 0
    }
  }
  # Add the advertisers to the bidding landscape matrix
  biddingLandscapes = rbind(biddingLandscapes, query[1,])
}


# Create slates 
# Rows are slates and columns are advertisers
# All slates are added for query 1, then all slates are added for query 2, etc. 
# Slates for a query are added in the order of the bidding landscape. Slates with 1 ad are added first
# Then slates with 2 ads are added. 
# E.g. if bidding landscape is 1,2,3,4, slates would be: 1, 2, 3, 4, 1&2, 1&3, 1&4, 2&3, 2&4, 3&4
# Decision variables will follow this order as well 
slates = matrix(nrow = 0, ncol = n)
# For each query
for(i in 1:m) {
  # For each spot in bidding landscape, add a slate with a single advertiser (as long as they bid)
  for(j in 1:n) {
    advertiser = biddingLandscapes[i,j]
    # Only want to consider bidders
    if(advertiser != 0) {
      rowVector = vector("numeric", n)
      rowVector[advertiser] = 1
      slates = rbind(slates, rowVector)
    }
  }
  # Add all slates with 2 ads
  # Ads must be in order of the bidding landscape
  for(j in 1:(n-1)) {
    advertiser1 = biddingLandscapes[i,j]
    # Only want to consider bidders
    if(advertiser1 != 0) {
      for(k in (j+1):n) {
        advertiser2 = biddingLandscapes[i,k]
        if(advertiser2 != 0) {
          rowVector = vector("numeric", n)
          rowVector[advertiser1] = 1
          rowVector[advertiser2] = 1
          slates = rbind(slates, rowVector)
        }
      }
    }
  }
}
write.csv(slates, file="slates.csv")


# Calculate PPC
# Rows are queries and columns are advertisers
PPC = matrix(nrow = m, ncol = n)
# For each query
for(i in 1:m) {
  # For each spot in the bidding landscape
  for(j in 1:n) {
    # Get the advertiser in that spot of the bidding landscape
    advertiser = biddingLandscapes[i,j]
    # A 0 means that spot in the bidding landscape isn't occupied
    # If spot is occupied by advertiser, we need to calculate their PPC
    if(advertiser != 0) {
      # Last ad in the bidding landscape pays 0.1
      if(j == n || biddingLandscapes[i,j+1] == 0) {
        PPC[i,advertiser] = 0.01
      } 
      # How high does their bid need to be so that their score is still higher than the next advertiser in the landscape?
      else {
        # Get the score (QS*bid) from the next ad in the bidding landscape
        nextAdvertiser = biddingLandscapes[i, j+1]
        ownQualityScore = qualityScores[i,advertiser]
        nextAdvertiserScore = scores[i,nextAdvertiser]
        minBid = nextAdvertiserScore/ownQualityScore + 0.01
        PPC[i,advertiser] = minBid
      }
    } 
    # Any advertisers who did not bid on the query will have a NA in the PPC matrix
  }
}

# Set the operators vector
operators = matrix('<=', nrow = m + n, ncol = 1)


# Set the A matrix 
A = matrix(nrow = 0, ncol = sum(numSlates))

# Add budget constraints for each advertiser
for(j in 1:n) {
  # Gives us all the slates the advertiser is part of
  rowVector = as.vector(slates[,j])
  # Need to multiply by PPC 
  counter = 0
  # For each query
  for(i in 1:m) {
    # Get number of slates/rows for this query in the slates matrix
    numSlatesForQuery = numSlates[i]
    # Get PPC for the advertiser and the query
    ppc = PPC[i,j]
    if(!is.na(ppc)) { 
      for(s in 1:numSlatesForQuery) {
        slateNum = counter + s
        if(rowVector[slateNum] != 0 ) {
          # Multiply by ppc
          rowVector[slateNum] = ppc*(rowVector[slateNum])
          # Also multiply by CTR
          advertisersInSlate = which(slates[slateNum,] == 1)
          # If advertiser is the only one in slate, it takes first position
          if(length(advertisersInSlate) == 1) {
            #Use CTR for position 1 in the results page
            rowVector[slateNum] = rowVector[slateNum]*ctr1
          }
          # If another ad is in the slate, need to determine if advertiser is first or second (based on bidding landscape)
          else if(length(advertisersInSlate) == 2) {
            if(advertisersInSlate[1] == j) {
              otherAdvertiser = advertisersInSlate[2]
            } else {
              otherAdvertiser = advertisersInSlate[1]
            }
            landscapeForQuery = biddingLandscapes[i,]
            spotOfAd = match(j, landscapeForQuery)
            spotOfOtherAd = match(otherAdvertiser, landscapeForQuery)
            # If the advertiser of focus (j) comes before the other advertiser in the bidding landscape, it gets first position
            if(spotOfAd < spotOfOtherAd) {
              rowVector[slateNum] = rowVector[slateNum]*ctr1
            } else {
              rowVector[slateNum] = rowVector[slateNum]*ctr2
            }
          }
        }
      }
    }
    counter = counter + numSlatesForQuery
  }
  A = rbind(A, rowVector)
}


# Add query constraints
counter = 0
for(i in 1:m) {
  rowVector = vector("numeric", sum(numSlates))
  
  # numSlates has the number of decision variables for each query
  numVars = numSlates[i]
  for(s in 1:numVars) {
    rowVector[counter + s] = 1
  }
  counter = counter + numVars
  A = rbind(A, rowVector)
}
write.csv(A, file="A-part1.2.csv")

# Set the B vector
b = rbind(budgets, numTimesQueryAsked)

# Ojective function is the sum of the LHS of each of the budget constraints
budgetConstraints = A[1:n,]
obj = colSums(budgetConstraints)


# ----------------------------------------- PART 1.3 ------------------------------------------------

# Set the variable types. 
vtype = matrix('C', nrow = 1, ncol = sum(numSlates))

# Solve
model = list()
model$A = A
model$obj = obj
model$modelsense = "max"
model$rhs = b
model$sense = operators
model$vtype = vtype
result = gurobi(model)

# Compare objective function value (revenue) to total budget of all advertisers
sum(budgets)

# Compare actual money used by advertisers to their budgets
budgetUsed = vector("numeric", 0)
for(j in 1:n) {
  budgetUsed = c(budgetUsed, sum(A[j,]*(result$x)))
}
budgetUsedPercentage = budgetUsed/budgets
budgetComparison = as.data.frame(cbind(budgets, budgetUsed, budgetUsedPercentage))
colnames(budgetComparison) = c("budgets", "budgets_used", "budgets_used_fraction")
write.csv(budgetComparison, "budgetComparison-part1.3.csv")

# Get results and append to slates matrix for convenient viewing
resultSlates = cbind(slates, result$x)
queryIndices = vector("numeric", 0)
for(i in 1:m) {
  vec = rep(i, numSlates[i])
  queryIndices = c(queryIndices, vec)
}
resultSlates = cbind(resultSlates, as.matrix(queryIndices))

# Filter out slates that were never shown
resultSlates = resultSlates[resultSlates[,n+1] > 0,]
write.csv(resultSlates, file="results-part1.3.csv")

# Get total number of slates shown
sum(result$x)
# Compare to number of total queries
sum(numTimesQueryAsked)


# ----------------------------------------- PART 1.4 ------------------------------------------------

# Sensitivity analysis
shadowPrices = result$pi
reducedCosts = result$rc
write.csv(shadowPrices, file="shadow_prices.csv")
write.csv(reducedCosts, file="reduced_costs.csv")

# ----------------------------------------- PART 1.5 ------------------------------------------------

# Set the variable types. 
vtype = matrix('I', nrow = 1, ncol = sum(numSlates))

# Solve
model = list()
model$A = A
model$obj = obj
model$modelsense = "max"
model$rhs = b
model$sense = operators
model$vtype = vtype
result = gurobi(model)

# Compare objective function value (revenue) to total budget of all advertisers
sum(budgets)

# Compare actual money used by advertisers to their budgets
budgetUsed = vector("numeric", 0)
for(j in 1:n) {
  budgetUsed = c(budgetUsed, sum(A[j,]*(result$x)))
}
budgetUsedPercentage = budgetUsed/budgets
budgetComparison = as.data.frame(cbind(budgets, budgetUsed, budgetUsedPercentage))
colnames(budgetComparison) = c("budgets", "budgets_used", "budgets_used_fraction")
write.csv(budgetComparison, "budgetComparison-part1.5.csv")

# Get results and append to slates matrix for convenient viewing
resultSlates = cbind(slates, result$x)
queryIndices = vector("numeric", 0)
for(i in 1:m) {
  vec = rep(i, numSlates[i])
  queryIndices = c(queryIndices, vec)
}
resultSlates = cbind(resultSlates, as.matrix(queryIndices))

# Filter out slates that were never shown
resultSlates = resultSlates[resultSlates[,n+1] > 0,]
write.csv(resultSlates, file="results-part1.5.csv")

# Get total number of slates shown
sum(result$x)
# Compare to number of total queries
sum(numTimesQueryAsked)

############################################ PART 2 ##################################################

# ----------------------------------------- PART 2.1 ------------------------------------------------

# Please see word document for explanation. Advertiser 7 was involved in none of the winning slates so
# I will use them for the remainder of part 2

# ----------------------------------------- PART 2.2 ------------------------------------------------

# Assume other bids and quality scores are as before
# Assume slates have n spots (1 for each advertiser), so slates = bidding landscape

unsuccessfulAdvertiser = 7

# CTR percentages provided in assignment
ctrPercentages = c(10, 7.3, 5.329, 3.89, 2.840, 2.073, 1.513, 1.105, 0.806, 0.589, 0.43, 0.314, 0.229, 
        0.167, 0.122, 0.089, 0.065, 0.047, 0.032, 0.023)
ctr = ctrPercentages/100

# Take the last n click through rates
ctr = ctr[(20-n+1):20]

# Get rid of the advertiser we are trying to solve for
otherScores = scores[,-unsuccessfulAdvertiser]

# Need decision variables for the CTR of each query (m variables)
# Need decision variables for the cost for each query (m variables)
# Need decision variables for each CTR interval for each query 
# The order of the binary variables will be as follows: All binary variables for query 1, all binary variables for query 2, etc.
# For all binary variables for a query, the first binary variable will be for bid=0, the second variable will be for the bid interval which
# gets the ad the LAST spot. The third variable will be for the bid interval that gets the ad the 2nd-last spot. The last variable
# will be for the bid interval taht gets the ad the 1st spot. The "last" spot is the last amongst BIDDERS ONLY. E.g. If only
# 13 of the other advertisers bid, occupying the "last" spot would be taking spot 14. 
binaryVarNums =  vector("numeric", m)
# The number of intervals is two more than the number of non-zero scores/bids for that query (not considering our advertiser)
# The first "interval" is for bid=0
for(i in 1:m) {
  query = otherScores[i,]
  countNonZero = length(which(query != 0))
  binaryVarNums[i] = countNonZero + 2
}
numVars = m + m + sum(binaryVarNums)


# Set the variable types
vtype = matrix('B', nrow = 1, ncol = numVars)
# CTR and cost variables are continous. The rest are binary
for(v in 1:(2*m)) {
  vtype[v] = 'C'
}


# Set the objective function vector
obj = matrix(0, nrow = 1, ncol = numVars)
# First m decision variables are CTR for each query
for(i in 1:m) {
  obj[i] = numTimesQueryAsked[i]
}


# Set the A matrix 
# First constraint is the budget constraint
# The following m constraints are for CTR
# The following m constraints are for cost
# The following m constraints are making sure you select only one bid interval
A = matrix(nrow = 0, ncol = numVars)

# Add the cost/budget constraint
# Second m decision variables are cost for each query
rowVector = vector("numeric", numVars)
for(i in 1:m) {
  rowVector[m+i] = numTimesQueryAsked[i]
}
A = rbind(A, rowVector)

# Set the CTR constraints and the cost constraints
# For each query, calculate CTR, PPC, and cost for each interval
ctrConstraints = matrix(nrow = 0, ncol = numVars)
costConstraints = matrix(nrow = 0, ncol = numVars)
# Keep track of interval breakpoints so we can use them after solving
intervalBreakpointsList = vector("list", m)
# Binary variables start at 2*m + 1
counter = 2*m + 1
for(i in 1:m) {
  rowVectorCTR = vector("numeric", numVars)
  rowVectorCost = vector("numeric", numVars)
  
  advertiserQualityScore = qualityScores[i, unsuccessfulAdvertiser]
  # Since other advertisers bids and quality scorse are as before, we can reuse the scores matrix
  otherAdQueryScores = as.vector(otherScores[i,])
  # Sort scores from lowest to highest
  otherAdQueryScores = sort(otherAdQueryScores)
  # Get rid of zeroes
  otherAdQueryScores = otherAdQueryScores[ otherAdQueryScores != 0 ]
  numOtherAdvertisers = length(otherAdQueryScores)
  # ASSUMPTION: Intervals with the same upper and lower bound (due to ties between scores) are permitted.
  # If this interval wins, the bid will be exactly equal to the lower and upper bound 
  intervalBreakpoints = otherAdQueryScores/advertiserQualityScore
  intervalBreakpointsList[[i]] = intervalBreakpoints
  # PPC is each of the interval breakpoints plus 0.01
  PPCs = intervalBreakpoints + 0.01
  PPCs = c(0, 0.01, PPCs)
  print(PPCs)

  # Get the CTRs for the number of other bidders we have plus 1 
  # E.g. if there are three other bidders, we can get the 1st, 2nd, 3rd, or 4th spot on the results page. We an also not
  # be on the results page if we don't bid (then CTR is 0)
  CTRs = rev(ctr[1:(numOtherAdvertisers+1)]) 
  # Add zero at the beginning
  CTRs = c(0, CTRs)
  print(CTRs)
  
  # Multiply PPC by CTR to get cost
  costs = CTRs*PPCs
  print(costs)

  # Add CTRs to our CTR constraint for this query
  numBinaryVarsForQuery = binaryVarNums[i]
  rowVectorCTR[counter:(counter + numBinaryVarsForQuery - 1)] = CTRs
  
  # Add costs to our cost contraint for this query
  rowVectorCost[counter:(counter + numBinaryVarsForQuery - 1)] = costs

  # The first m variables are CTR variables
  rowVectorCTR[i] = -1
  # The second m variables are cost variables
  rowVectorCost[i+m] = -1
  ctrConstraints = rbind(ctrConstraints, rowVectorCTR)
  costConstraints = rbind(costConstraints, rowVectorCost)
  # Update counter
  counter = counter + numBinaryVarsForQuery
}
# Add all CTR constraints
A = rbind(A, ctrConstraints)
# Add all cost constraints
A = rbind(A, costConstraints)

# Set the interval constraints (only can pick one bid interval per query)
# Binary variables start at 2*m + 1
counter = 2*m
# For each query
for(i in 1:m) {
  rowVector = vector("numeric", numVars)
  numBinaryVarsForQuery = binaryVarNums[i]
  for(v in 1:numBinaryVarsForQuery) {
    rowVector[counter + v] = 1
  }
  counter = counter + numBinaryVarsForQuery
  A = rbind(A, rowVector)
}
write.csv(A, file="A-part2.2.csv")


# Set the B vector
b = matrix(nrow = 1, ncol = 1 )
# Add RHS for budget constraint
b[1] = budgets[unsuccessfulAdvertiser]
# Add RHS for CTR constraints
rhsCTR = matrix(0, nrow = m, ncol = 1 )
b = rbind(b, rhsCTR)
# Add RHS for cost constraints
rhsCost = matrix(0, nrow = m, ncol = 1 )
b = rbind(b, rhsCost)
# Add RHS for bid interval constraints
rhsIntervals = matrix(1, nrow = m, ncol = 1 )
b = rbind(b, rhsIntervals)


# Set the operators vector
operators = matrix('=', nrow = 3*m + 1, ncol = 1)
operators[1] = '<='


# Solve
model = list()
model$A = A
model$obj = obj
model$modelsense = "max"
model$rhs = b
model$sense = operators
model$vtype = vtype
result = gurobi(model)

# ----------------------------------------- PART 2.3 ------------------------------------------------

resultVars = result$x
resultCTRs = resultVars[1:m]
resultCosts = resultVars[(m+1):(2*m)]
write.csv(cbind(resultCTRs, resultCosts), "results-part2.3.csv")

# Total money spent
sum(resultCosts*numTimesQueryAsked)
# Compare to budget
budgets[unsuccessfulAdvertiser]
resultBinaryVars = resultVars[(2*m + 1):numVars]

# Need to decide on bids for queries
# For each query
counter = 1
newBids = vector("numeric", m)
for(i in 1:m) {
  numBinaryVarsForQuery = binaryVarNums[i]
  intervalBreakpoints = intervalBreakpointsList[[i]]
  intervalBreakpoints = c(0, intervalBreakpoints)
  resultBinaryVarsForQuery = resultBinaryVars[counter:(counter+numBinaryVarsForQuery-1)]
  print(resultBinaryVarsForQuery)

  # The first variable for each query reprsents a bid of 0
  if(resultBinaryVarsForQuery[1] == 1) {
    newBids[i] = 0
  }
  # If the advertiser gets the first spot
  else if(resultBinaryVarsForQuery[numBinaryVarsForQuery] == 1) {
    # Since this is the first spot, the interval only has a lower bound. 
    # We will just add 0.01 to remain above the bound (we could use any bid above the bound - it's just a matter of choice)
    newBids[i] = intervalBreakpoints[numBinaryVarsForQuery-1] + 0.01
  }
  # In this case, we have both an upper and lower bound to work with
  else {
    winningVariable = which(resultBinaryVarsForQuery == 1)
    # Lower bound of interval
    breakPoint1 = intervalBreakpoints[winningVariable - 1]
    # Upper bound of interval
    breakPoint2 = intervalBreakpoints[winningVariable]
    # Use midpoint for our bid (again, this is just a matter of choice)
    midpoint = (breakPoint1 + breakPoint2)/2
    newBids[i] = midpoint
  }
  counter = counter + numBinaryVarsForQuery
}

bidsComparison = cbind(bids[,unsuccessfulAdvertiser], newBids)
colnames(bidsComparison) = c("old_bids", "new_bids")
write.csv(bidsComparison, file="bidsComparison.csv")

# ----------------------------------------- PART 2.4 ------------------------------------------------

# Read in data from CSV
numTimesQueryAsked=as.matrix(read.csv("/Users/Celeste/Documents/2020/4B/MSCI 433/Assignment 4/numTimesQueryAsked.csv"))
qualityScores=as.matrix(read.csv("/Users/Celeste/Documents/2020/4B/MSCI 433/Assignment 4/qualityScores.csv"))
budgets=as.matrix(read.csv("/Users/Celeste/Documents/2020/4B/MSCI 433/Assignment 4/budgets.csv"))
bids=as.matrix(read.csv("/Users/Celeste/Documents/2020/4B/MSCI 433/Assignment 4/bids.csv"))
bids[,unsuccessfulAdvertiser] = newBids

# RUN PART 1.1 TO 1.3 AGAIN TO OBTAIN NEW RESULTS
