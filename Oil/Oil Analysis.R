# How to convert a data frame column to numeric type?: https://stackoverflow.com/questions/2288485/how-to-convert-a-data-frame-column-to-numeric-type
# How to create barplot from dataframe: https://stackoverflow.com/questions/38586554/how-to-create-barplot-from-dataframe
# Data structures: http://adv-r.had.co.nz/Data-structures.html
getRowNo <- function(dataFrame, value) {
  rowNo <- which(dataFrame$Thousand.barrels.daily == value)
  return(rowNo)
}

worldOilDir = "C:\\Users\\jacqu\\Documents\\Proj\\Investment-Research-Analysis\\Oil\\"
worldOil = read.csv(file = paste(worldOilDir, "Oil Production - Barrels 1965 to 2019.csv", sep=""), skip=2, header=TRUE)

# Get the Total World row, ignoring the last 3 columns
dataFrameRow = worldOil[getRowNo(worldOil, "Total World"), 2:(ncol(worldOil) - 5)]
dataFrameRow

cols = colnames(worldOil)[2:(ncol(worldOil) - 5)]
cols

# vectorRow = factor(unname(unlist(dataFrameRow[1,])))

# range(worldOil["Total World", 1:(ncol(worldOil)-3)], na.rm=TRUE)
# colnames(worldOil)[1:(ncol(worldOil) - 3)]


#-------------------------------------------------------------------------

# x <- 1:length(dataFrameRow)[1]

# unname(unlist(dataFrameRow[1,]))

annualTotalWorldOil = as.numeric(t(dataFrameRow))


# Plot the data
barplot(annualTotalWorldOil, 
        beside=TRUE,
        names.arg = cols,
        legend.text = TRUE,
        args.legend = list(x = "topleft", bty = "n", inset=c(-0.05, 0)))
