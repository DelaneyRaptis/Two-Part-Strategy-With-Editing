# Load the AlphaSimR package which contains functions for simulating plant and animal breeding
library(AlphaSimR)

# Set the random number generator seed so that the results are reproducible each time you run the script
set.seed(123)

# Step 1: Create a founder population using quickHaplo()
# - nInd = 100: create 100 individuals (plants)
# - nChr = 1: with 1 chromosome
# - segSites = 1000: and 1000 segregating (variable) genetic sites per chromosome
founders <- quickHaplo(nInd = 100, nChr = 1, segSites = 1000)

# Set up simulation parameters using SimParam
# This object holds the rules and genetic architecture for the simulation
SP <- SimParam$new(founders)

# Add a quantitative trait controlled by additive effects only
# - nQtlPerChr = 100: 100 QTL (quantitative trait loci) per chromosome
SP$addTraitA(nQtlPerChr = 100)

# Set environmental variance to simulate heritability
# - h2 = 0.4: set narrow-sense heritability of the trait to 0.4 (moderate)
SP$setVarE(h2 = 0.4)

# STEP 2: Start Population Improvement Component
# This part simulates fast-cycle recurrent genomic selection

# Create an initial population from the founder individuals
popImprovement <- newPop(founders)

# Loop through 3 cycles of population improvement (recurrent selection)
for (cycle in 1:3) {
  # Select the top 20 individuals based on their genomic breeding values (GEBVs)
  # - use = "bv": select using breeding value (bv = best guess of genetic potential)
  selected <- selectInd(popImprovement, nInd = 20, use = "bv")
  
  # Randomly cross the selected individuals to make the next generation
  # - nCrosses = 10: 10 mating pairs
  # - nProgeny = 10: each cross produces 10 offspring
  popImprovement <- randCross(selected, nCrosses = 10, nProgeny = 10)
}

# STEP 3: Send seed to Product Development Component
# This mimics taking improved material and evaluating it in later trials

# Select the top 50 individuals from the final improved population
# - These are treated as inbred lines to evaluate and potentially release
candidateLines <- selectInd(popImprovement, nInd = 50, use = "bv")

# STEP 4: Product Development Simulation
# Select the 5 best lines for release based on observed phenotype (simulated trial performance)
# - use = "pheno": select based on observed phenotypic value (genetics + environment)
selectedForRelease <- selectInd(candidateLines, nInd = 5, use = "pheno")

# Output section
# Print a message to indicate what’s being shown
cat("Phenotypic values of final selected lines (release candidates):\n")

# Print the actual phenotypic values of the 5 selected individuals
print(selectedForRelease@pheno)
