
# Trying to find a more elegant way to call TotePacking -------------------

# Maybe combine the dimensions into column-lists? 
# Shorten call to TotePacking?
# Allow vectorized calls to TotePacking??
items$item_dims <- split(items[2:4], seq(nrow(items)))
items$tote_dims <- split(items[5:7], seq(nrow(items)))


for (i in 1:nrow(items)){
  # This doesn't work...
  eleg <- TotePacking(items$item_dims[i], items$tote_dims[i])
  items$CvnQty[i] <- eleg[1]
  items$Itrs[i] <- eleg[2]
  
}

items$item_dims[1]

typeof(as.vector(items$item_dims[1]))
typeof(as.vector(c(items$item_length[1], items$item_width[1], items$item_height[1])))

rm(i)
rm(eleg)