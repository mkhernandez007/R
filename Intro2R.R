# 
## Simple math
#
12 + 8
12* 64
12/64
12-64
12^2
(543+6473)*7
#
## Variables and assignments
#
x = 32
x + 8
y <- 8
y + 10
bucky <- tuna <- 20
bucky              # In R to print the variable name on the line
tuna
assign ("ham", 71) # R function that assigns the data to a value that is seperated by a ","
ham + tuna
rm(ham)            # R function that frees up memory and removes variable and data
name <- "Bucky Roberts"
nchar(name)        # R function that counts all characters in the variable name
nchar("name")      # R function will return the counts of all characters in ""
#
## Different data types
#
a <- 32
b <- "Bucky"
class(a)           # R function that describes the data type
class(b)
a <- 423
b <- -8
c<- 32.5345
d <- "rainbows"
is.numeric(a)      # R function that gives a boolean response to the variable in the parens
is.numeric(b)
is.numeric(c)
is.numeric(d)
#
## Working with dates
#
chicken <- as.Date("2014-06-28") # R function needed to describe a data an not a string
eggs <- ("2014-06-28")
class(chicken)
class(eggs)
#
## Logic Operators
#
5 == 7             # R function that asks "if 5 is equal to 7?"
12 != 14           # R function that asks "if 12 is not equal to 14?"
45 < 83            # R function that asks "if 45 < 83?"
7 > 7              # R function that asks "if 7 > 7?", which will return false
7 >= 7             # R function that asks "if 7 >= 7?", which will return true
#
## Vectors
#
b1 <- c(1,2,3,4,5) # R fuction that combines a set of numbers
b2 <- c("bucky", "hoss","emma")
b1
b2
b1 * 2             # Scalar multiplication of a vector
slammer <- 1:5     # Make a vector of numbers of 1-5
slammer
bacon <- -3:-12
bacon
va <- 1:3
vb <- 7:9
va+vb
length (vb)        # R function that tells us the number of items in the list
slammer + va       # When mismatched vectors va will repeat its sequence so you will see  <2, 4, 6, 5, 7>
va < 2             # R function asking "is the elements of the vectors < 2?"
any (va < 2)       # R function that says if at least one element meets the condition then return as true
all (va < 2)       # R function that says if all elements meets the condition then return as true
hosser <- 50:60
hosser[3]          # R function that plucks the 3rd element in the vector list
hosser[1:5]        # R function that helps pluck a range of elements in the list
#
## Data Frames: spreadsheet like creation of a set of vectors
#
id = 1:15
age = c(18, 13, 66, 32, 3, 43, 54, 656, 87, 323, 7, 2, 9, 34, 65)
named = c("bucky", "tom", "bobby", "henry", "emily", "baby", "hannah", "joe", "cathy", "sandy", "lesley", "emma", "ann", "old dan","eric")
xsheet <- data.frame(id,age,name) # R fuction that creates a spreadsheet with rows and columns
xsheet
nrow(xsheet)                      # R function that returns the number of rows in the data frame
ncol(xsheet)                      # R function that returns the number of columns in the data frame
dim(xsheet)                       # R function that returns the number of both rows and columns in a data frame
names(xsheet)[2]                  # R function that returns the title of the column requested
head(xsheet)                      # R function that shows the first few rows of the data frame
tail(xsheet)                      # R function that shows the last few rows of the data frame
xsheet$age                        # R function that returns the contents in the column per name
xsheet[2]                         
xsheet[2,2]                       # R function that returns the contents in the row number first and column second
xsheet[3,1:3]
xsheet[3, ]                       # R function that returns all row contents
xsheet[,3]                        # R function that returns all column contents
class(xsheet["age"])              # This will look for the class of the entire data frame, hence will return data.frame
class(xsheet[,"age"])             # Here we are looking for the part of the data frame
#
## Lists: storing data of different types of data
#
buckylist = list(71,"bacon", c(1:5), "tundra", xsheet)  # List can store unstructured data
buckylist                                               # Printing out your list
names(buckylist) = c("favNumber", "hatedFood","vecky", "coldDoesntBotherMe", "favData") # Naming your elements in the list
buckylist[["hatedFood"]]                                # Pulling out an element from the list
buckylist[["favData"]]$age                              # pulling data of data from within the list
length(buckylist)
buckylist[["sisName"]] = "Hannah"                       # append a new item
bucky
length(buckylist)
#
## Matrices: the entire thing must be the same types of data
#
one = matrix(1:100, nrow =10)
one
two = matrix(51:60, nrow = 2)
three = matrix(61:70, nrow = 2)
two 
three
dim(two)
dim(three)
two * three
#
## How to read data from a CSV file
#
# file download location: http://samplecsvs.s3.amazonaws.com/Sacramentorealestatetransactions.csv
getwd()                                     # Get the working directory path, place above file in that wd
tunacan = read.csv("C:/Users/fj998d/Downloads/Sacramentorealestatetransactions.csv",TRUE,",") # True if your first column has title names, in column delimited
class(tunacan)
tunacans = read.csv("http://samplecsvs.s3.amazonaws.com/Sacramentorealestatetransactions.csv", TRUE,",") #pulls from the web without downloading it to your comp

#
## Charts and graphics
#
bData = read.csv("C:/Users/fj998d/Downloads/Sacramentorealestatetransactions.csv",TRUE,",") # True if your first column has title names, in column delimited
head(bData)
hist(bData$beds, main = "histrogram of beds", xlab="beds", ylab = "frequency") #Histogram with title/main, and x & y lables
scatter.smooth(bData$beds,bData$baths, main = "scatter plot of beds", xlab="beds", ylab = "baths")
boxplot(bData$sq__ft, main = "box plot of beds", ylab = "square feet")
#
## Creating scatterplots with ggplot 2
## The images that come from this data set of diamonds and the code came from the help of http://ggplot2.org/book/qplot.pdf
#
install.packages("ggplot2")
require("ggplot2")
head(diamonds)
qplot(carat, price, data = diamonds, color=clarity) # scatter plots with color based on a third variable
qplot(carat, x * y * z, data = diamonds)
qplot(carat, price, data = dsmall, colour = color)
qplot(carat, price, data = dsmall, shape = cut)
qplot(carat, price, data = diamonds, alpha = I(1/10))
qplot(carat, price, data = diamonds, alpha = I(1/100))
qplot(carat, price, data = diamonds, alpha = I(1/200))            # you can see where a bulk of the data lies
qplot(carat, price, data = dsmall, geom = c("point", "smooth"))
qplot(carat, price, data = diamonds, geom = c("point", "smooth")) # adding a smoother to a plot
qplot(carat, price, data = dsmall, geom = c("point", "smooth"), span = 0.2)
qplot(carat, price, data = dsmall, geom = c("point", "smooth"), span = 1)
qplot(color, price / carat, data = diamonds, geom = "jitter", alpha = I(1 / 5))
qplot(color, price / carat, data = diamonds, geom = "jitter", alpha = I(1 / 50))
qplot(color, price / carat, data = diamonds, geom = "jitter", alpha = I(1 / 200))
qplot(carat, data = diamonds, geom = "histogram")
qplot(carat, data = diamonds, geom = "density")
qplot(carat, data = diamonds, geom = "histogram", binwidth = 1, xlim = c(0,3))
qplot(carat, data = diamonds, geom = "histogram", binwidth = 0.1, xlim = c(0,3))
qplot(carat, data = diamonds, geom = "histogram", binwidth = 0.01, xlim = c(0,3))
qplot(carat, data = diamonds, geom = "density", colour = color)
qplot(carat, data = diamonds, geom = "histogram", fill = color)
qplot(color, data = diamonds, geom = "bar")
qplot(color, data = diamonds, geom = "bar", weight = carat) +  scale_y_continuous("carat")
qplot(carat, data = diamonds, facets = color ~ ., geom = "histogram", binwidth = 0.1, xlim = c(0, 3))
qplot(carat, ..density.., data = diamonds, facets = color ~ ., geom = "histogram", binwidth = 0.1, xlim = c(0, 3))