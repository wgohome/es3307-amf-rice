# Create dataset

rm(list=ls())
AMF <- c("Ctrl", "AMF1", "AMF2")
NIT <- c(0, 1)
PHOS <- c(0, 1)
norms <- c(
  65, 70, 75, 90,
  75, 85, 85, 105, 
  95, 105, 110, 130
)

row <- 1
norm_i <- 1
for (a in AMF)
{
  for (b in NIT)
  {
    for (c in PHOS)
    {
      for (i in 1:10) {
        if (row == 1)
        {
          df <- data.frame(POT=row, MASS=rnorm(1, mean=norms[norm_i], sd=5), AMF=a, NIT=b, PHOS=c)
        }
        else
        {
          df[row,] <- list(POT=row, MASS=rnorm(1, mean=norms[norm_i], sd=5), AMF=a, NIT=b, PHOS=c)
        }
        row <- row + 1
      }
      norm_i <- norm_i + 1
    }
  }
}

# Save to csv
# write.csv(df, "rice.csv", row.names=FALSE)

