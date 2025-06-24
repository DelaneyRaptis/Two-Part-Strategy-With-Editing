# Load the AlphaSimR package for simulation of plant or animal breeding
library(AlphaSimR)

# Set a random number generator seed for reproducibility
set.seed(123)

# STEP 1: Create founder population with 100 individuals, 1 chromosome, and 1000 segregating sites
founders <- quickHaplo(nInd = 100, nChr = 1, segSites = 1000)

# Create simulation parameters object (SP) based on the founder population
SP <- SimParam$new(founders)

# Add a quantitative trait controlled by additive genetic effects (100 QTL per chromosome)
SP$addTraitA(nQtlPerChr = 100)

# Set environmental variance such that heritability of the trait is 0.4
SP$setVarE(h2 = 0.4)

# STEP 2: Generate the initial population from the founders
popImprovement <- newPop(founders)

# STEP 3: Define function to simulate PAGE (promotion of alleles by genome editing)
# This function returns adjusted breeding values by adding a "boost" for favorable homozygous QTL genotypes
getPageBoostedValues <- function(pop, nEdit = 5, boost = 1.5) {
  # Extract additive QTL effects from the simulation parameters
  qtlEffects <- SP$traits[[1]]@addEff
  
  # Identify the top nEdit QTLs with the largest absolute effect
  topQTL <- order(abs(qtlEffects), decreasing = TRUE)[1:nEdit]
  
  # Get the genotypes at the selected top QTLs
  geno <- pullQtlGeno(pop, simParam = SP)[, topQTL]
  
  # Count the number of homozygous favorable alleles (value == 2) for each individual
  favorable <- rowSums(geno == 2)
  
  # Retrieve the original breeding values for the population
  originalBV <- bv(pop)
  
  # Simulate the PAGE effect by adding a proportional boost to the breeding value
  boostedBV <- originalBV + boost * (favorable / nEdit)
  
  # Return the adjusted breeding values
  return(boostedBV)
}

# STEP 4: Perform 3 cycles of population improvement using PAGE
for (cycle in 1:3) {
  # Compute PAGE-boosted breeding values for the current population
  boostedBV <- getPageBoostedValues(popImprovement, nEdit = 5, boost = 1.5)
  
  # Select the top 20 individuals based on the PAGE-boosted breeding values
  selected <- popImprovement[order(boostedBV, decreasing = TRUE)[1:20]]
  
  # Randomly cross the selected individuals to generate the next generation (10 crosses × 10 progeny = 100)
  popImprovement <- randCross(selected, nCrosses = 10, nProgeny = 10)
}

# STEP 5: Product development component
# Select top 50 individuals based on actual (unboosted) breeding values
candidateLines <- selectInd(popImprovement, nInd = 50, use = "bv")

# From those 50, select top 5 based on observed phenotypic values (mimicking trial evaluation)
selectedForRelease <- selectInd(candidateLines, nInd = 5, use = "pheno")

# STEP 6: Output results
# Display a message about what is being printed
cat("Phenotypic values of final selected lines (release candidates):\n")

# Print the phenotypic values of the selected lines
print(pheno(selectedForRelease))
