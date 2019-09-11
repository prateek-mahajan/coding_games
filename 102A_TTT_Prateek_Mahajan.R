#Prateek Mahajan STATS 102A
#display function
state <- as.character(1:9)
display <- function(x){
  cat("",x[1],"|",x[2],"|",x[3],"\n")
  cat("---+---+---","\n")
  cat("",x[4],"|",x[5],"|",x[6],"\n")
  cat("---+---+---","\n")
  cat("",x[7],"|",x[8],"|",x[9],"\n")
  cat("---+---+---","\n")
}
display(state)

#illegal function
illegal_check <- function(x,y){
  if(y!="1"&y!="2"&y!="3"&y!="4"&y!="5"&y!="6"&y!="7"&y!="8"&y!="9"){return(FALSE)}
  else if(x[as.double(y)]=="x"|x[as.double(y)]=="o"){return(FALSE)}
  return(TRUE)
}

#valid input check function
valid_input_check <- function(y){
  if(y!= "1" & y!= "2")
  {return(FALSE)
  } else{return(TRUE)}
}

#update function
update_board <- function(who,state,pos){
  if(who==1){(state[pos]<- "x")}
  else if(who==2){(state[pos]<- "o")}
  return(state)
}

#winner function
winning <- list(c(1,2,3),c(4,5,6),c(7,8,9),c(1,4,7),c(2,5,8),c(3,6,9),c(1,5,9),c(3,5,7))
winner_check <- function(state){
  if(all(state[c(1,2,3)]=="x")|all(state[c(4,5,6)]=="x")|all(state[c(7,8,9)]=="x")|all(state[c(1,4,7)]=="x")|
     all(state[c(2,5,8)]=="x")|all(state[c(3,6,9)]=="x")|all(state[c(1,5,9)]=="x")|all(state[c(3,5,7)]=="x")){return(TRUE)}
  else if(all(state[c(1,2,3)]=="o")|all(state[c(4,5,6)]=="o")|all(state[c(7,8,9)]=="o")|all(state[c(1,4,7)]=="o")|
          all(state[c(2,5,8)]=="o")|all(state[c(3,6,9)]=="o")|all(state[c(1,5,9)]=="o")|all(state[c(3,5,7)]=="o"))
  {return(TRUE)}
  else if(sum(state==as.character(1:9))==0){return(TRUE)}
  
  return(FALSE)
}

#AI
computer_game <- function(state){
  i <- 1
  xpos <- which(state=="x")
  opos <- which(state=="o")
  while(i<9){
    
    thismatch<-na.omit(match(xpos,winning[[i]]))
    opmatch  <-na.omit(match(opos,winning[[i]]))
    if((length(thismatch)==2) && (length(opmatch)==0)){
      pos<-(winning[[i]][-thismatch])
      return(pos)
    }
    else if((length(opmatch)==2) && (length(thismatch)==0)){
      pos <- (winning[[i]][-opmatch])
      return(pos)
    }
    i <- i+1
  }
  return(sample(which(state!="x" & state!="o"),1,replace = TRUE))
}

#human vs human
two_game <- function(x){
  cat("Welcome to human vs human game. x plays first.","\n","\n")
  who<-1
  display(x)
  cat("\n")
  while(winner_check(state)==FALSE){       
    if(who==1){
      pos <- (readline(prompt="Where should x play?: "))
      illegal_check(state,pos)
      while(illegal_check(state,pos)==FALSE){
        pos <- (readline(prompt="Error! Please choose a position between 1 & 9 that has not been taken: "))
      }
      state<-update_board(who,state,as.double(pos))
      cat("\n")
      display(state)
      cat("\n")
      who <- 2
    }
    winner_check(state)
    if(winner_check(state)==TRUE){break}
    pos <- (readline(prompt="Where should o play? : "))
    illegal_check(state,pos)
    while(illegal_check(state,pos)==FALSE){
      pos <- (readline(prompt="Error! Please choose a position between 1 & 9 that has not been taken: "))
    }
    state <- update_board(who,state,as.double(pos))
    winner_check(state)
    if(winner_check(state)==TRUE){break}
    cat("\n")
    display(state)
    cat("\n")
    who <-1
    
  }
  
  if(all(state[c(1,2,3)]=="x")|all(state[c(4,5,6)]=="x")|all(state[c(7,8,9)]=="x")|all(state[c(1,4,7)]=="x")|
     all(state[c(2,5,8)]=="x")|all(state[c(3,6,9)]=="x")|all(state[c(1,5,9)]=="x")|all(state[c(3,5,7)]=="x"))
  {cat("Game over! x has won.","\n"); display(state)}
  else if(all(state[c(1,2,3)]=="o")|all(state[c(4,5,6)]=="o")|all(state[c(7,8,9)]=="o")|all(state[c(1,4,7)]=="o")|
          all(state[c(2,5,8)]=="o")|all(state[c(3,6,9)]=="o")|all(state[c(1,5,9)]=="o")|all(state[c(3,5,7)]=="o"))
  {cat("Game over! o has won","\n");display(state)}
  else{cat("Game over! it is a tie. Type play() to play again","\n");display(state)}
}

#computer vs human
one_game <- function(x){
  cat("Welcome to Computer vs Human game.","\n")
  answer <- readline(prompt ="Should computer play as player 1 or player 2? ")
  valid_input_check(answer)
  while(valid_input_check(answer)==FALSE){
    answer <- readline(prompt = "Error! Type 1 or 2: ")
  }
  if(as.integer(answer)==1){
    who<-1
    display(x)
    cat("Computer is X. You are O","\n")
    while(winner_check(state)==FALSE){       
      
      if(who==1){
        pos <- computer_game(state)
        state <- update_board(who,state,pos)
        cat("\n","computer plays", pos, "\n","\n")
        display(state)
        cat("\n")
        who <- 2
      }
      winner_check(state)
      if(winner_check(state)==TRUE){break}
      pos <- (readline(prompt="Where should o play? : "))
      illegal_check(state,pos)
      while(illegal_check(state,pos)==FALSE){
        pos <- (readline(prompt="Error! Please choose a position between 1 & 9 that has not been taken: "))
      }
      state <- update_board(who,state,as.double(pos))
      winner_check(state)
      if(winner_check(state)==TRUE){break}
      cat("\n")
      display(state)
      cat("\n")
      who <-1  
    }
    
  }
  else if(as.integer(answer)==2){
    who<-1
    display(x)
    cat("Computer is O. You are X","\n")
    while(winner_check(state)==FALSE){
      if(who==1){
        pos <- (readline(prompt="Where should x play? : "))
        illegal_check(state,pos)
        while(illegal_check(state,pos)==FALSE){
          pos <- (readline(prompt="Error! Please choose a position between 1 & 9 that has not been taken: "))
        }
        state <- update_board(who,state,as.double(pos))
        
        display(state)
        who <-2  
      }
      winner_check(state)
      if(winner_check(state)==TRUE){break}
      pos <- computer_game(state)
      state <- update_board(who,state,pos)
      cat("\n","computer plays", pos, "\n","\n")
      display(state)
      winner_check(state)
      if(winner_check(state)==TRUE){break}
      who <- 1
    }
  }
  
  if(all(state[c(1,2,3)]=="x")|all(state[c(4,5,6)]=="x")|all(state[c(7,8,9)]=="x")|all(state[c(1,4,7)]=="x")|
     all(state[c(2,5,8)]=="x")|all(state[c(3,6,9)]=="x")|all(state[c(1,5,9)]=="x")|all(state[c(3,5,7)]=="x"))
  {cat("Game over! x has won.","\n"); display(state)}
  else if(all(state[c(1,2,3)]=="o")|all(state[c(4,5,6)]=="o")|all(state[c(7,8,9)]=="o")|all(state[c(1,4,7)]=="o")|
          all(state[c(2,5,8)]=="o")|all(state[c(3,6,9)]=="o")|all(state[c(1,5,9)]=="o")|all(state[c(3,5,7)]=="o"))
  {cat("Game over! o has won","\n");display(state)}
  else{cat("Game over! it is a tie. Type play() to play again","\n");display(state)}
  }

#play function
play <- function(){
  question <- readline(prompt = "How many players? 1 or 2? : ")
  valid_input_check(question)
    while(valid_input_check(question)==FALSE){
      question <- readline(prompt = "Error! Type 1 or 2: ")
    }
  if(as.numeric(question)==1){
    one_game(state)
  }
  else if(as.numeric(question)==2){
    two_game(state)
  }
  
}
# write play() to start the game

