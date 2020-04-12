
# Read in sample item/tote data -------------------------------------------
items <- read.csv("data/sample_items.csv")
 

# Add columns to hold TotePacking output ----------------------------------
items$FitQty <- 0
items$Itrs <- 0


# Run everything in functions.R and helpers.R before continuing -----------
source("01_helpers.R")
source("02_functions.R")


# Test run of TotePacking -------------------------------------------------
for (i in 1:nrow(items)){
  tempy <- TotePacking(as.vector(c(items$item_length[i], items$item_width[i], items$item_height[i])), 
                       as.vector(c(items$tote_length[i], items$tote_width[i], items$tote_height[i])))
  items$FitQty[i] <- tempy[1]
  items$Itrs[i] <- tempy[2]
}
rm(i)
rm(tempy)

View(items)
