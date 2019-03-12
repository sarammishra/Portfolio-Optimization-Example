# Clear the global environment
rm(list=ls())

# install.packages('lpSolve) if you don't have it
library(lpSolve)

#-------------------------------------------
# SECTION 1: PREPARE DATA
#-------------------------------------------

# Create a table with securities information (real world has tons more)
tableOfSecurities = matrix(0, nrow = 41, ncol = 3)
colnames(tableOfSecurities) = c("Class", "Yield", "Duration")
rownames(tableOfSecurities) = c(
  'Agency 1y',
  'Agency 2y',
  'Agency 3y',
  'Agency 4y',
  'Agency 5y',
  'Agency 7y',
  'Agency 10y',
  'Corp A 1y',
  'Corp A 2y',
  'Corp A 3y',
  'Corp A 4y',
  'Corp A 5y',
  'Corp A 10y',
  'Corp A 15y',
  'Corp A 20y',
  'Corp AA 1y',
  'Corp AA 2y',
  'Corp AA 3y',
  'Corp AA 4y',
  'Corp AA 5y',
  'Corp AA 10y',
  'Corp AA 15y',
  'Corp AA 20y',
  'MBS 10y 2.5% M',
  'MBS 15y 3.0% S',
  'MBS 20y 3.0% M',
  'MBS 20y 3.5% M',
  'Muni Taxable 3y',
  'Muni Taxable 5y',
  'Muni Taxable 8y',
  'Muni Taxable 10y',
  'Muni Taxable 12y',
  'FedFunds',
  'UST 10y',
  'UST 1y',
  'UST 2y',
  'UST 30y',
  'UST 3y',
  'UST 4y',
  'UST 5y',
  'UST 7y'
)

# Convert the table to a data frame so that numeric and character classes can be mixed in table
tableOfSecurities = as.data.frame(tableOfSecurities)

# Populate classes 
tableOfSecurities[,"Class"] = c(
  'Agency',
  'Agency',
  'Agency',
  'Agency',
  'Agency',
  'Agency',
  'Agency',
  'Corporate',
  'Corporate',
  'Corporate',
  'Corporate',
  'Corporate',
  'Corporate',
  'Corporate',
  'Corporate',
  'Corporate',
  'Corporate',
  'Corporate',
  'Corporate',
  'Corporate',
  'Corporate',
  'Corporate',
  'Corporate',
  'MBS',
  'MBS',
  'MBS',
  'MBS',
  'Muni',
  'Muni',
  'Muni',
  'Muni',
  'Muni',
  'UST',
  'UST',
  'UST',
  'UST',
  'UST',
  'UST',
  'UST',
  'UST',
  'UST'
)

# Populate returns 
tableOfSecurities[,"Yield"] = c(
  1.94,
  2.45,
  2.47,
  2.6,
  2.81,
  2.86,
  2.81,
  2.18,
  2.8,
  3.33,
  3.6,
  3.73,
  4.21,
  4.53,
  4.08,
  2.05,
  2.8,
  2.91,
  3.1,
  3.37,
  3.98,
  4.06,
  3.85,
  2.33,
  2.62,
  2.72,
  2.79,
  2.81,
  3.18,
  3.36,
  3.58,
  3.52,
  1.4,
  2.68,
  1.87,
  2.36,
  2.75,
  2.51,
  2.66,
  2.86,
  2.9
)

# Populate duration 
tableOfSecurities[,"Duration"] = c(
  1,
  2,
  3,
  4,
  5,
  7,
  10,
  1,
  2,
  3,
  4,
  5,
  10,
  15,
  20,
  1,
  2,
  3,
  4,
  5,
  10,
  15,
  20,
  10,
  15,
  20,
  20,
  3,
  5,
  8,
  10,
  12,
  0,
  10,
  1,
  2,
  30,
  3,
  4,
  5,
  7
)

# Check out the table of securities 
# View(tableOfSecurities)



#-------------------------------------------
# SECTION 2: SET CONSTRAINTS
#-------------------------------------------

# Set concentration limit for individual securities
limitAsset = 0.15

# Set concentration limits for asset classes 
limitAgency = 0.20
limitCorporate = 0.20
limitMBS = 0.25 
limitMuni = 0.25
limitUST = 0.15

# Can you determine how to constrain duration?



#-------------------------------------------
# SECTION 3: SETUP THE PROBLEM
#-------------------------------------------

# --------------------------
# 1. Constraints matrix 
# --------------------------
constraints = matrix(0, nrow = nrow(tableOfSecurities) + 1 + 5, ncol = nrow(tableOfSecurities))
colnames(constraints) = tableOfSecurities[,"Class"]
rownames(constraints) = c(
  rownames(tableOfSecurities),
  "Total",
  "limitAgency",
  "limitCorporate",
  "limitMBS",
  "limitMuni",
  "limitUST"
)

# Identity matrix for individual sector securities 
for (i in (1:nrow(tableOfSecurities)))
{
  constraints[i,i] = 1
}

# This will populate values for sector groups and totals
constraints["Total",] = 1
constraints["limitAgency", colnames(constraints)=='Agency'] = 1
constraints["limitCorporate", colnames(constraints)=='Corporate'] = 1
constraints["limitMBS", colnames(constraints)=='MBS'] = 1
constraints["limitMuni", colnames(constraints)=='Muni'] = 1
constraints["limitUST", colnames(constraints)=='UST'] = 1

# See the result
# View(constraints)

# --------------------------
# 2. Limits vector 
# --------------------------
limits = c(
  rep(limitAsset, nrow(tableOfSecurities)),
  1, 
  limitAgency,
  limitCorporate,
  limitMBS, 
  limitMuni,
  limitUST
)

# See the result
# View(limits)

# --------------------------
# 3. Directional vector 
# --------------------------
directional = c(
  rep("<=", nrow(tableOfSecurities)), 
  "=", 
  rep("<=", 5)
)

# See the result
# View(directional)

# --------------------------
# 4. Objective function 
# --------------------------                
objectiveFun = tableOfSecurities[,"Yield"]

# See the result
# View(objectiveFun)




#-------------------------------------------
# SECTION 4: SOLVE THE PROBLEM 
#-------------------------------------------
theSolution = lp("max", objectiveFun, constraints, directional, limits, compute.sens=TRUE)
theWeights = round(theSolution$solution, digits = 2) * 100

# Add the weights into the original table 
tableOfSecurities$Optimal = theWeights

# See the result
View(tableOfSecurities)

# Plot the result 
library(ggplot2)
library(scales)

# Limit data to only what is in the portfolio 
plotData = tableOfSecurities[tableOfSecurities[,"Optimal"] > 0,]

# Pie of asset class allocations
pie = ggplot(plotData, aes(x="", y=Optimal, fill=Class))+
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + 
  scale_fill_brewer("Blues") + 
  theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  ) + 
  theme(axis.text.x=element_blank()) + 
  ggtitle("Concentration Limits by Asset Class")

# Show plot
pie
