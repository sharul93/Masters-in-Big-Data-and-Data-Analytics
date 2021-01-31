linear.congruence <- function(a=22695477, b=1, m=2**32, seed=1234)
{
  random.number <- (a * seed + b) %% m
  print(random.number)
  if (random.number <= 2**31)
  {
    return(c(0, random.number))
  } else
    {
    return(c(1, random.number))
    }
}


#max.number.moves <- number.Moves
#initial.seed <- 1234
#current.seed <- initial.seed

#This loop implements all the games rounds.
#for(i in 1:max.number.moves){
#  x <- linear.congruence(seed=current.seed)
#  computer.move <- x[1]
#  new.seed <- x[2]
#  print(paste("computer.move=",computer.move,"new.seed=",new.seed))
#  current.seed <- new.seed
#}