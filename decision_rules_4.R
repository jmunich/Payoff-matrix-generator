my_max<-function(x){
  return(which(x == max(x)))
}

generate_game<-function(p1,p2,y){
  payoff<-sample(y,2*p1*p2,replace = TRUE)
  game<-matrix(payoff,p1,2*p2)
  return(game)
}

game_own<-function(game){
  payoff_own<-game[,c(TRUE,FALSE)]
  return(payoff_own)
}

game_other<-function(game){
  payoff_other<-game[,c(FALSE,TRUE)]
  return(payoff_other)
}

ch<-1
while(length(ch)==1){
  spiel<-generate_game(3,3,values)
  ch<-choice_naive(spiel)
}

choice_optimist<-function(game){
  choice<-my_max(apply(game_own(game),1,max))
  return(choice)
}

choice_pessimist<-function(game){
  choice<-my_max(apply(game_own(game),1,min))
  return(choice)
}

choice_altruist<-function(game){
  sum_mat<-rowSums(game)
  choice<-my_max(sum_mat)
  return(choice)
}

choice_naive<-function(game){
  sum_mat<-rowSums(game_own(game))
  choice<-my_max(sum_mat)
  return(choice)
}

game<-generate_game(3,3,values)

choice_k.level<-function(game,k){
  own<-game_own(game)
  other<-game_other(game)
  own_1<-choice_naive(game)
  
  if(length(own_1)==1){
    choice_other_1<-my_max(other[own_1,])
  }
  
  if(length(other[choice_naive(game),1])>1){
    choice_other_1<-my_max(colSums(other[own_1,]))
  }
  
  if(k==1){
    if(length(choice_other_1)==1){
      choice<-my_max(own[,choice_other_1])
    }
    if(length(choice_other_1)>1){
      choice<-my_max(rowSums(own[,choice_other_1]))
    }
    
  }
  
  if(k>1){
    
    if(length(choice_other_1)==1){
      choice<-my_max(own[,choice_other_1])
    }
    if(length(choice_other_1)>1){
      choice<-my_max(rowSums(own[,choice_other_1]))
    }
    
    for(i in 2:k){
      if(length(choice)==1){
        choice_other_k<-my_max(other[choice,])
      }
      if(length(choice)>1){
        choice_other_k<-my_max(colSums(other[choice,]))
      }
      if(length(choice_other_k)==1){
        choice<-my_max(own[,choice_other_k])
      }
      if(length(choice_other_k)>1){
        choice_other_k<-my_max(rowSums(own[,choice_other_k]))
      }
    }
  }
  return(choice)
}

game<-generate_game(3,3,values)

choice_dominance<-function(game,k){
  own<-game_own(game)
  other<-game_other(game)
  xind<-c(1:length(own[,1]))
  yind<-c(1:length(own[1,]))
  
  
  for(z in 1:k){
    if(length(xind)>1&length(yind)>1){
      
      dom_other<-c()
      for(i in 1:length(other[1,])){
        doms<-c()
        for(j in 1:length(other[1,])){
          tvec<-other[,i]<other[,j]
          doms[j]<-ifelse(sum(tvec)==length(other[,1]),1,0)
        }
        dom_other[i]<-sum(doms)
      }
      
      exclude_other<-which(dom_other>0)
      
      if(length(exclude_other)>0){
        xind<-xind[-exclude_other]
        own<-own[,-exclude_other]
        other<-other[,-exclude_other]
      }
      
    }
    
    if(length(xind)>1&length(yind)>1){
      
      dom_own<-c()
      for(i in 1:length(own[,1])){
        doms<-c()
        for(j in 1:length(own[,1])){
          tvec<-own[i,]<own[j,]
          doms[j]<-ifelse(sum(tvec)==length(own[1,]),1,0)
        }
        dom_own[i]<-sum(doms)
      }
      exclude_own<-which(dom_own>0)
      
      if(length(exclude_own)>0){
        yind<-yind[-exclude_own]
        own<-own[-exclude_own,]
        other<-other[-exclude_own,]
      }
      
    }
    
    if(length(yind)==1){
      choice<-yind
      return(choice)
      #break
    }
    
    if(length(xind)==1){
      choice<-my_max(own)
      return(choice)
      #break
    }
    
    if(length(xind)>1&length(yind)>1){
      choice<-my_max(rowSums(own))
      return(choice)
      #break
    }
  }
  
}

choice_nash<-function(game){
  p1<-length(game[,1])
  p2<-length(game[1,])/2
  own<-game_own(game)
  other<-game_other(game)
  
  own_mat<-own
  for(i in 1:length(own_mat[1,])){
    vec<-own_mat[,i]
    crit<-max(vec)
    own_mat[,i]<-ifelse(vec==crit,1,0)
  }
  
  ot_mat<-other
  for(i in 1:length(ot_mat[,1])){
    vec<-ot_mat[i,]
    crit<-max(vec)
    ot_mat[i,]<-ifelse(vec==crit,1,0)
  }
  
  eq_mat<-own_mat*ot_mat
  
  choice<-which(rowSums(eq_mat)>0)
  return(choice)
}

########################################################################################
# values: what values can the payoff have
# player1: Choices available to player 1
# player2: Choices available to player 2
# sel1: index of decision rule that should have solutions distinct from the rest
# decision rules are (respectively): non-strategic(optimist, pessimist, altruist, naive)
#                                  ; strategic(nash, k1, k2, domination1, domination2)
ngames<-20
values<-c(1:9)
player1<-3
player2<-3
transpose<-TRUE

sel1<-c(1:4)


glist_own<-list()
glist_other<-list()
slist<-list()
for(i in 1:ngames){
  mark<-TRUE
  mark2<-1
  while(mark|(mark2!=0)){
    game<-generate_game(player1,player2,values)
    
    opt<-choice_optimist(game)
    pes<-choice_pessimist(game)
    alt<-choice_altruist(game)
    nai<-choice_naive(game)
    nash<-choice_nash(game)
    k1<-choice_k.level(game,1)
    k2<-choice_k.level(game,2)
    d1<-choice_dominance(game,1)
    d2<-choice_dominance(game,2)
    
    solutions<-list(opt, pes, alt, nai, nash, k1, k2, d1, d2)
    mark<-(length(solutions)!=sum(lengths(solutions)))|(length(solutions[[5]])==0)
    choices<-unlist(solutions)
    mark2<-sum(solutions[sel1]%in%solutions[-sel1],solutions[-sel1]%in%solutions[sel1])
  }
  
  solnames<-c("Optimist","Pessimist","Altruist","Naive","Nash", "k1","k2","d1","d2")
  solvec<-unlist(solutions)
  own<-game_own(game)
  other<-game_other(game)
  if(transpose){
    own<-t(own)
    other<-t(other)
  }
  
  gvec_own<-as.vector(t(own))
  gvec_other<-as.vector(t(other))
  names(solvec)<-solnames
  glist_own[[i]]<-gvec_own
  glist_other[[i]]<-gvec_other
  slist[[i]]<-solvec
}
usegame_own<-do.call(rbind,glist_own)
usegame_other<-do.call(rbind,glist_other)
usesol<-do.call(rbind,slist)

write.csv(usegame_own, "games_own.csv", row.names=FALSE)
write.csv(usegame_other, "games_other.csv", row.names=FALSE)
write.csv(usesol, "games_solutions.csv", row.names=FALSE)