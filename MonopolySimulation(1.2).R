#creates 40 int vector to represent board and sets every player to not be in jail
#how much money they have and if they are eliminated
playerCount <- 4
mboard <-  c()
isJail <- c()
jailTimer <- c()
playerPosition <- c()
dbles <- "False"
move <-0
turnCount <- 1
cards <- c(1:16)
ccCards <- c(sample(cards,16))
drawCCnum <- 0
i <- 1
while(i <= playerCount) { 
  add <- c("False")
  tempPosition <- c(1)
  tempTimer <- c(0)
  isJail <- c(isJail, add)
  jailTimer <- c(jailTimer, tempTimer)
  playerPosition <- c(playerPosition,tempPosition)
 
  
  i <- i + 1
}
i <- 1
scoreboard <- data.frame(isJail,playerPosition,jailTimer)
scoreboard
View(scoreboard)

#sends them to jail and sets their position to 11
sendJail <- function(playerNum) { 
  scoreboard[playerNum, 2] <<- 11
  scoreboard[playerNum, 1] <<- "True"
  print(paste0("Player ",playerNum))
  print("sent to jail")
  
}
escapeJail <- function(playerNum) { 
  scoreboard[playerNum, 1] <<- "False"
  scoreboard[playerNum, 3] <<- 0
  print(paste0("Player", playerNum))
  print("escaped jail")
}
communityCard <- function(playerNum) { 
  drawCCnum <<- drawCCnum + 1
  if (drawCCnum >= 17) { 
    ResetCardSample()
  }
  #print(paste0("Player ",playerNum, " drew a Community Chest Card"))
  
  if (ccCards[drawCCnum] == 1) {        # 1 will represent getting a go to jail card
    sendJail(playerNum)
  }
  else if (ccCards[drawCCnum] == 2) {   # 2 will represent getting a Advance to Go card
    scoreboard[playerNum, 2] <<- 1
    #print(paste0("Player ",playerNum, " Advanced to Go"))
  }
  #if it is anything else, the card won't do anything in this simulation because position does not change
  }

ResetCardSample <- function() { 
  ccCards <<- c(sample(cards,16))
  drawCCnum <<- 1
  }
Roll_die <- function() { 
  die <- c(1:6)
  roll_1 <- sample(die, 1,replace = TRUE)
  print((roll_1))
  roll_2 <- sample(die, 1,replace = TRUE)
  print(roll_2)
  sum <- roll_2 + roll_1
  if (roll_1 == roll_2) { 
    dbles <<- "True"
    return(sum)
  }
  else { 
    return(sum) 
  }
  }

while(turnCount <= 50) { #50 turns
  
for (i in 1:playerCount) { 
  if (scoreboard[i,1] == "True") { 
    scoreboard[i,3] <- scoreboard[i,3] + 1  #jailTimer increases by 1 so they can escape after 3 turns
    move <- Roll_die()
    if (dbles == "True") { 
      escapeJail(i)
      currentPos <- scoreboard[i,2]
      scoreboard[i,2] <- currentPos + move
      if (scoreboard[i,2] > 40) { 
        scoreboard[i,2] <- scoreboard[i,2] - 40
      } 
      if ((scoreboard[i,2] == 3) | (scoreboard[i,2] == 18) | (scoreboard[i,2] == 34)){ 
        communityCard(i) 
      }
      else if ((scoreboard[i,2] == 8) | (scoreboard[i,2] == 23) | (scoreboard[i,2] == 37)) { 
        chanceCard() #chance card functions need to be added
      }
      
      dbles <- "False"
      }
    if (scoreboard[i,3] == 3) { 
      escapeJail(i)
    }
  }
  else {
  print(paste0("Player ",i, " is moving"))               
  
  move <- Roll_die()
  currentPos <- scoreboard[i,2]
  scoreboard[i,2] <- currentPos + move
  if (scoreboard[i,2] > 40) { 
    scoreboard[i,2] <- scoreboard[i,2] - 40
  }
  mboard <- c(mboard, scoreboard[i,2])
  if ((scoreboard[i,2] == 3) | (scoreboard[i,2] == 18) | (scoreboard[i,2] == 34)){ 
    communityCard(i) 
  }
  else if ((scoreboard[i,2] == 8) | (scoreboard[i,2] == 23) | (scoreboard[i,2] == 37)) { 
    #chanceCard() #chance card functions need to be added
  }
  else if ((scoreboard[i,2] == 31)) { 
    sendJail(i)
  }
  
  #if rolled double twice
  if (dbles == "True") { 
    print(paste0("Player ",i, " is moving again"))           
    dbles <- "False"
    move <- Roll_die()
    currentPos <- scoreboard[i,2]
    scoreboard[i,2] <- currentPos + move
    if (scoreboard[i,2] > 40) { 
      scoreboard[i,2] <- scoreboard[i,2] - 40
      } 
    if ((scoreboard[i,2] == 3) | (scoreboard[i,2] == 18) | (scoreboard[i,2] == 34)){ 
      communityCard(i) #community card functions need to be added
    }
    else if ((scoreboard[i,2] == 8) | (scoreboard[i,2] == 23) | (scoreboard[i,2] == 37)) { 
      #chanceCard() #chance card functions need to be added
    }
    else if ((scoreboard[i,2] == 31)) { 
      sendJail(i)
    }
    
  }
  
  #third roll when not double on third 
  if (dbles == "True") { 
    print(paste0("Player ",i, " is moving again"))           
    dbles <- "False"
    move <- Roll_die()
    currentPos <- scoreboard[i,2]
    scoreboard[i,2] <- currentPos + move
    if (scoreboard[i,2] > 40) { 
      scoreboard[i,2] <- scoreboard[i,2] - 40
    } 
    if ((scoreboard[i,2] == 3) | (scoreboard[i,2] == 18) | (scoreboard[i,2] == 34)){ 
      communityCard(i) 
    }
    else if ((scoreboard[i,2] == 8) | (scoreboard[i,2] == 23) | (scoreboard[i,2] == 37)) { 
      #chanceCard() #chance card functions need to be added
    }
    else if ((scoreboard[i,2] == 31)) { 
      sendJail(i)
    }
    
  }
  
  #if rolled double 3 times --> sends that player to jail
  if (dbles == "True") { 
    dbles <- "False"
    sendJail(i)
    }
  
  
  
  
  
  i <- i + 1
  }
}
print(paste("End of ", turnCount, "turn"))
turnCount <- turnCount + 1
}
View(scoreboard)
#print(mboard)
#for loop has ended if it reached here 
hist(mboard, ylim = c(0,12), breaks = seq(from=0, to=40, by=1), main = "Histomgram of the number of times a player landed on a place on the board", xlab = "Place on board", las=1)
hist(mboard, freq=F, breaks = seq(from=0, to=40, by=1), main = "Histomgram of the number of times a player landed on a place on the board", xlab = "Place on board", las=1)


#ignore this
#scoreboard[3,2] <- 50
#scoreboard


