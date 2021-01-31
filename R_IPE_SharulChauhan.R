#R- Individual Programming Exercise (IPE)
setwd("C:/Users/sharu/OneDrive/Desktop/MBD/R/R-Projects/assignment/") #sets the working directory
getwd() #prints the current working directory

source("auxfunc.R") #sourcing an external file into the code
#current.seed = 1234 #setting current seed at 1234

#Game Starts
print("Welcome to Player Behavior Prediction by Sharul Chauhan")

repeat{
  
  #Asking the player to choose between Easy, Difficult, or Quit the game
  value.typed <- readline(
    prompt="Choose the type of game (1: Easy; 2: Difficult) OR Enter 3 to Quit  ")
  type.of.game <- as.integer(value.typed)
  
  if (type.of.game == 3) {
    print("It was a pleasure to have you, hope you enjoyed!!")
    break
  }
  
  #Asking the player to enter a value as current seed
  current.seed <- as.integer(readline(prompt="Choose a random number as current seed: "))
  
  #Asking the player to select the number of rounds to play
  value.typed <- readline(
    prompt="Enter the number of moves: ")
  number.of.moves <- as.integer(value.typed)
  
  #Easy game
  if(type.of.game == 1) {
  computer.numb.victories <- 0
  player.numb.victories <- 0
  player.move.history <- c()
  
    #If the player gives the number of moves as 0 or less, the game will end in a tie with 0-0 score
    for (j in if (number.of.moves >= 1) 1:number.of.moves else c()){
    print("---") 
    
    #Using the linear congruence function to generate computer move
    x <- linear.congruence(seed=current.seed)
    computer.move <- x[1]
    new.seed <- x[2]
    print(paste("computer.move=",computer.move,"new.seed=",new.seed))
    current.seed <- new.seed
    
    #Asking the player to enter the move
    #The computer will not allow any other number except 0 or 1
    #If the move is a decimal number then the computer will only pick the number before the decimal as an integer
    repeat{
      value.typed <- readline(
      prompt="Choose your move number 1 (0 or 1): ")
      
      if(value.typed == 0 | value.typed == 1){
      player.move <- as.integer(value.typed)  
      break
      } else{
        print("Enter either 0 or 1")
      }
    }
    
    #Comparing the result; if same move as computer then computer wins, else player wins
    #Computer wins scenario
    if (player.move == computer.move){ 
      print(paste("Player =",player.move," Computer =",computer.move,"- Computer wins!"))
      computer.numb.victories <- computer.numb.victories +1
    #Player wins scenario
    } else {
      print(paste("Player =",player.move," Computer =",computer.move,"- Player wins!"))
      player.numb.victories <- player.numb.victories +1
    }
    
    #Player gets a star per win
    message2print <- "PLAYER: "
    if(player.numb.victories>0) {
      for (i in 1:player.numb.victories) {
        message2print <- paste(message2print,"*",sep="")
      }
    }
    print(message2print)
    
    #Computer gets a star per win
    message2print <- "COMPUTER: "
    if(computer.numb.victories>0) {
      for (i in 1:computer.numb.victories) {
        message2print <- paste(message2print,"*",sep="")
      }
    }
    print(message2print)
  }
  
  #Final result of the Easy game
  if(player.numb.victories < computer.numb.victories) {
    print(paste("GAME OVER!!! Final score: Player",
                player.numb.victories, 
                "-", 
                computer.numb.victories,"Computer  ",
                "THE COMPUTER WON!!"))
  } else if (player.numb.victories > computer.numb.victories){
    print(paste("GAME OVER!!! Final score: Player",
                player.numb.victories, 
                "-", 
                computer.numb.victories,"Computer  ",
                "THE PLAYER WON!!"))
  } else{
    print(paste("GAME OVER!!! Final score: Player",
                player.numb.victories,
                "-",
                computer.numb.victories,"Computer  ",
                "It's a TIE!!"))
  }
  
#Difficult Game
} else if(type.of.game == 2) {
  computer.numb.victories <- 0
  player.numb.victories <- 0
  prev.player.throw <- 0
  
  throw00 <- 0 #Number of times player's move is 0 given the last move was 0
  throw01 <- 0 #Number of times player's move is 0 given the last move was 1
  throw10 <- 0 #Number of times player's move is 1 given the last move was 0
  throw11 <- 0 #Number of times player's move is 1 given the last move was 1
  
  #If the player gives the number of moves as 0 or less, the game will end in a tie with 0-0 score
  for (j in if (number.of.moves >= 1) 1:number.of.moves else c()){
    print("###")
    
    if(j==1){
      #Using the linear congruence function to generate computer move
      x <- linear.congruence(seed=current.seed)
      computer.move <- x[1]
      new.seed <- x[2]
      print(paste("computer.move=",computer.move,"new.seed=",new.seed))
      current.seed <- new.seed
    } else {
        #Computer move based on player's behavior/last move; last throw by player was 0
        if(prev.player.throw == 0){
          if(throw10 > throw00) {
          computer.move <- 1
          } else if(throw10 < throw00){
           computer.move <- 0
          } else if(throw10 == throw00){
          x <- linear.congruence(seed=current.seed)
          computer.move <- x[1]
          new.seed <- x[2]
          print(paste("computer.move=",computer.move,"new.seed=",new.seed))
          current.seed <- new.seed
          }
        }
    
        #Computer move based on player's behavior/last move; last throw by player was 1     
        else if(prev.player.throw == 1){
          if(throw11 > throw01) {
          computer.move <- 1
          } else if(throw11 < throw01){
          computer.move <- 0
          } else if(throw11 == throw01){
          x <- linear.congruence(seed=current.seed)
          computer.move <- x[1]
          new.seed <- x[2]
          print(paste("computer.move=",computer.move,"new.seed=",new.seed))
          current.seed <- new.seed
          }
        }
      }
    
    #Asking the player to enter the move
    #The computer will not allow any other number except 0 or 1
    #If the move is a decimal number then the computer will only pick the number before the decimal as an integer
    repeat{
      value.typed <- readline(
        prompt="Choose your move number 1 (0 or 1): ")
      
      if(value.typed == 0 | value.typed == 1){
        player.move <- as.integer(value.typed)  
        break
      } else{
        print("Enter either 0 or 1")
      }
    }
    
    #Record player's moves
      if (player.move == 0 && prev.player.throw == 0){
        throw00 <- throw00 + 1
      } else if (player.move == 0 && prev.player.throw == 1){
        throw01 <- throw01 + 1
      } else if(player.move == 1 && prev.player.throw == 0){
        throw10 <- throw10 + 1
      } else{
        throw11 <- throw11 + 1
      }
      
      #Comparing the result; if same move as computer then computer wins, else player wins
      #Computer wins scenario
      if (player.move == computer.move){ #Computer correctly guessed!
      print(paste("Player =",player.move," Computer =",computer.move,"- Computer wins!"))
      print(paste("Current seed=",current.seed)) 
      computer.numb.victories <- computer.numb.victories +1
      #Player wins scenario
      } else {
      print(paste("Player =",player.move," Computer =",computer.move,"- Player wins!"))
      print(paste("Current seed=",current.seed))
      player.numb.victories <- player.numb.victories +1
      }
    
    #Player gets a star per win
    message2print <- "PLAYER: "
    if(player.numb.victories>0) {
      for (i in 1:player.numb.victories) {
        message2print <- paste(message2print,"*",sep="")
      }
    }
    print(message2print)
    
    #Computer gets a star per win
    message2print <- "COMPUTER: "
    if(computer.numb.victories>0) {
      for (i in 1:computer.numb.victories) {
        message2print <- paste(message2print,"*",sep="")
      }
    }
    print(message2print)
  }
    
    #Final result of the Difficult game
    if(player.numb.victories < computer.numb.victories) {
      print(paste("GAME OVER!!! Final score: Player",
                  player.numb.victories, 
                  "-", 
                  computer.numb.victories,"Computer  ",
                  "THE COMPUTER WON!!"))
    } else if (player.numb.victories > computer.numb.victories){
      print(paste("GAME OVER!!! Final score: Player",
                  player.numb.victories, 
                  "-", 
                  computer.numb.victories,"Computer  ",
                  "THE PLAYER WON!!"))
    } else{
      print(paste("GAME OVER!!! Final score: Player",
                  player.numb.victories,
                  "-",
                  computer.numb.victories,"Computer  ",
                  "It's a TIE!!"))
    }
  } 
    #Computer will allow the player to choose 1-Easy; 2-Difficult; 3-Quit 
    #If the input is anything different, then the player will have to enter a value again
    else{
    print("Please enter a valid number to choose the game type or enter 3 to Quit")
  }
}
