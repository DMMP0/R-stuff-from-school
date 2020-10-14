set.seed(203818)

dado <- 1:6

sample(dado, 1)
#6
sample(dado, 2,TRUE)
#4 5
x <- sample(dado, 20, TRUE)
x
#2 2 3 4 3 4 2 6 5 4 4 1 1 5 6 4 2 2 5 4
y <- matrix(x, ncol=2)
y
#     [,1] [,2]
# [1,]    2    4
# [2,]    2    1
# [3,]    3    1
# [4,]    4    5
# [5,]    3    6
# [6,]    4    4
# [7,]    2    2
# [8,]    6    2
# [9,]    5    5
#[10,]    4    4

z <- apply(y, 1, sum)

table(z)
#z
# 3  4  6  8  9 10 
# 1  2  1  3  2  1 
prop.table(table(z))
#z
#  3   4   6   8   9  10 
#0.1 0.2 0.1 0.3 0.2 0.1 


#end
