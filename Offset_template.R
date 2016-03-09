require(manipulate)

a <- c(1,2,3,4,5)
b <- c(2,4,6,8,10)
df <- data.frame(a,b)

a2 <- c(2,3,4,5,6)
b2 <- c(2,4,6,8,10)
df2 <- data.frame(a2,b2)

f1 <- function(x, y) {
  x+y
}

applyOffset <- function(x, y, offset_value) {
  x + offset_value
}


createPlot <- function(x, y, xmin, xmax, offset_value){
  temp.x <- applyOffset(x, y, offset_value)
  plot(temp.x,y, xlim = c(xmin, xmax), type = "l")
  lines(df2$a2,df2$b2, xlim = c(xmin, xmax))
}


manipulate(
  createPlot(df$a, df$b, x.min, x.max, offset_value),
  x.min = slider(0,15, initial = 0),
  x.max = slider(15,30, initial = 25),
  offset_value = slider(-5,50,initial=0)
  
)

