# Load AlphaSimR package
library(AlphaSimR)

# Set random seed for reproducibility
set.seed(123)

# STEP 1: Make the founder population
cat("\nSTEP 1: Make the founder population\n")
founders <- quickHaplo(nInd = 100, nChr = 1, segSites = 1000)

SP <- SimParam$new(founders)
SP$addTraitA(nQtlPerChr = 100)
SP$setVarE(h2 = 0.4)

cat("Created a founder population with 100 individuals.\n")

# STEP 2: Population improvement component
cat("\nSTEP 2: Population improvement component\n")

popImprovement <- newPop(founders)

# Loop through 5 cycles of improvement
for (cycle in 1:5) {
  cat(sprintf("\n-- Cycle %d --\n", cycle))
  
  # Select top 20 by breeding value
  parents <- selectInd(popImprovement, nInd = 20, use = "bv")
  cat("Selected 20 individuals based on breeding value.\n")
  cat("Mean breeding value of selected parents:\n")
  print(mean(bv(parents)))
  
  # Cross to make 100 offspring (10 crosses × 10 progeny)
  popImprovement <- randCross(parents, nCrosses = 10, nProgeny = 10)
  cat("Created new population from crosses (100 individuals).\n")
  
  # Save seed to product development every 2nd cycle
  if (cycle %% 2 == 0) {
    cat("Sending seed from this cycle to product development.\n")
    if (!exists("productPool")) {
      productPool <- popImprovement
    } else {
      productPool <- c(productPool, popImprovement)
    }
  }
}

# STEP 3: Product development component
cat("\nSTEP 3: Product development component\n")

# Select top 100 individuals from product pool by breeding value
productLines <- selectInd(productPool, nInd = 100, use = "bv")
cat("Selected 100 candidate lines from product pool based on breeding value.\n")
cat("Mean breeding value of selected product lines:\n")
print(mean(bv(productLines)))

# Further select top 10 based on phenotype (simulated field performance)
bestLines <- selectInd(productLines, nInd = 10, use = "pheno")
cat("\nSelected top 10 lines based on phenotype (trial performance).\n")
cat("Phenotypic values of top 10:\n")
print(pheno(bestLines))

# Final selection: choose top 5 to release
released <- selectInd(bestLines, nInd = 5, use = "pheno")
cat("\nSTEP 4: Final released lines\n")
cat("Phenotypic values of released lines:\n")
print(pheno(released))
