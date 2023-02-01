9 < (5 %% 3) + 7 | (5 > 9) | FALSE
TRUE + TRUE

if (FALSE) TRUE else FALSE

if (1) {
  print(1) 
} else if (0) {
  print(2)
} else {
  print(3)
}

if (1) print (1) else if (0) print(2) else print(3)

if (1) { print(1); print(2) } else if (0) print(2) else print(3)

if (1) { 
  print(1)
  print(2) 
} else if (0) print(2) else print(3)

# this will break
if (1) print(1); print(2) else if (0) print(2) else print(3)
if ("1") print(1)

x <- 0
while(x >= 0) {
  print(x)
  x <- x + 1
  if(x == 100000) break
}

for(i in c(T,F,F,T)) print(i)

while(TRUE) print(i)

my_func <- function(x,y) {
  return(x + y)
}

string_vec <- c("yes","sURE","YUP")
lapply(string_vec, casefold, upper=T)
sapply(string_vec, casefold, upper=T)

data(mtcars)
lapply(mtcars, function(x) return(list(mean(x),sd(x))))
sapply(mtcars, function(x) return(list(mean(x),sd(x))))
mssds_df <- as.data.frame(t(sapply(mtcars, function(x) return(
  list(
    mean=round(mean(x),2),
    sd=round(sd(x),2)
    )
  ))))

sapply(mtcars, \(x) return(list(mean(x),sd(x))))