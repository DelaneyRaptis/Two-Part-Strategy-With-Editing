# Load the AlphaSimR package
library(AlphaSimR)

# Set a random seed for reproducibility
set.seed(123)

# Step 1: Create a founder population with 10 individuals and 1 chromosome
founderPop <- runMacs(nInd = 10, nChr = 1, segSites = 100)

# Step 2: Set up simulation parameters (traits, heritability, etc.)
SP <- SimParam$new(founderPop)
SP$addTraitA(nQtlPerChr = 50)     # 50 quantitative trait loci
SP$setVarE(h2 = 0.4)              # Set heritability to 0.4

# Step 3: Create the F1 generation (make 5 crosses, each with 10 offspring)
parents <- newPop(founderPop)
crosses <- randCross(parents, nCrosses = 5, nProgeny = 10)

# Step 4: Select the top 5 individuals based on phenotype
selected <- selectInd(crosses, nInd = 5, use = "pheno")

# Step 5: Output their phenotypic values
cat("Top 5 selected phenotypic values:\n")
print(selected@pheno)

crosses <- randCross(selected, nCrosses = 5, nProgeny = 10)
selected <- selectInd(crosses, nInd = 5, use = "pheno")
cat("Top 5 selected phenotypic values:\n")
print(selected@pheno)