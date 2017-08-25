library(gtools)
#do porównywania
compare <- function(table,vector){
  (wynik <- t(t(table)==vector))
  (wynik <- rowSums(wynik))
  wynik <- wynik>(length(vector)-1)
  return(wynik)
}
#przykład
set.seed(1)
example <- sample(c(1,2,4,5),300,replace=T)
example_1 <- read_csv("~/Desktop/Psychologia kwantowa/Survey/los_chem_zera_3_3_10_57_34.csv")
example <- (example_1$key)



#kwestia dwóch maksimów do rozwiązania jeszcze została bo na razie wybiera pierwszy, a powininen losować.
Markov_new <- function(example){
  #historia, niestety nie jest ją tak łatwo zmienić. Trzeba dodać kolejny if w for.
  history <- 5
  #do porównywania
  compare <- function(table,vector){
    (wynik <- t(t(table)==vector))
    (wynik <- rowSums(wynik))
    wynik <- wynik>(length(vector)-1)
    return(wynik)
  }
  #na razie taką historię ustawiam
  #stworzenie tranisiton matrix
  transition_matrix <- permutations(n=length(unique(example)),r=history,v=unique(example),repeats.allowed = T)
  #nazwanie kolumn
  colnames(transition_matrix) <- paste0("h",c(1:history))
  temp_table <- data.frame(t(rep(0,length(unique(example)))))
  colnames(temp_table) <- sort(unique(example))
  #połączenie dwóch tabel
  transition_matrix <- cbind(transition_matrix,temp_table)
  #tutaj będzie zapisywana historia z której będą brane przewidywania
  which_history <- c(0,0)
  #zapisywanie ciągu
  ciag <- sample(unique(example),1,replace=F)
  ciag[2] <- example[1]
  #główna pętla, która wydaje się w końcu działać tak jak powinna
  for (i in 3:(length(example))){
    #historia 1
      transition_matrix[compare(transition_matrix[,1],example[i-2]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,1],example[i-2]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
      temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,1],example[i-1]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
      probability_matrix <- temp_table/sum(temp_table)
      if (sum(temp_table)<1){
        probability_matrix[1,] <- 1/length(unique(example))
      }
      next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
      next_element_probability <- max(probability_matrix)
      if (sum(probability_matrix[1,]==next_element_probability)>1){
        next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
      }
      comparison_data_frame <- data.frame(next_element,next_element_probability)
    #historia 2
    if (i > 3){
      transition_matrix[compare(transition_matrix[,c(1:2)],example[c((i-3),(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:2)],example[c((i-3),(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
      temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:2)],example[c((i-2):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
      probability_matrix <- temp_table/sum(temp_table)
      if (sum(temp_table)<1){
        probability_matrix[1,] <- 1/length(unique(example))
      }
      next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
      next_element_probability <- max(probability_matrix)
      if (sum(probability_matrix[1,]==next_element_probability)>1){
        next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
      }
      comparison_data_frame <- rbind(comparison_data_frame,data.frame(next_element,next_element_probability))
    }
    #historia 3
    if (i > 4){
      transition_matrix[compare(transition_matrix[,c(1:3)],example[c((i-4):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:3)],example[c((i-4):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
      temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:3)],example[c((i-3):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
      probability_matrix <- temp_table/sum(temp_table)
      if (sum(temp_table)<1){
        probability_matrix[1,] <- 1/length(unique(example))
      }
      next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
      next_element_probability <- max(probability_matrix)
      if (sum(probability_matrix[1,]==next_element_probability)>1){
        next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
      }
      comparison_data_frame <- rbind(comparison_data_frame,data.frame(next_element,next_element_probability))
    }
    #historia 4
    if (i > 5){
      transition_matrix[compare(transition_matrix[,c(1:4)],example[c((i-5):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:4)],example[c((i-5):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
      temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:4)],example[c((i-4):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
      probability_matrix <- temp_table/sum(temp_table)
      if (sum(temp_table)<1){
        probability_matrix[1,] <- 1/length(unique(example))
      }
      next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
      next_element_probability <- max(probability_matrix)
      if (sum(probability_matrix[1,]==next_element_probability)>1){
        next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
      }
      comparison_data_frame <- rbind(comparison_data_frame,data.frame(next_element,next_element_probability))
    }
    #historia 5
    if (i > 6){
      transition_matrix[compare(transition_matrix[,c(1:5)],example[c((i-6):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:5)],example[c((i-6):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
      temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:5)],example[c((i-5):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
      probability_matrix <- temp_table/sum(temp_table)
      if (sum(temp_table)<1){
        probability_matrix[1,] <- 1/length(unique(example))
      }
      next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
      next_element_probability <- max(probability_matrix)
      if (sum(probability_matrix[1,]==next_element_probability)>1){
        next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
      }
      comparison_data_frame <- rbind(comparison_data_frame,data.frame(next_element,next_element_probability))
    }
    which_history[i] <- which.max(comparison_data_frame$next_element_probability)
    ciag[i] <- as.numeric(as.character(comparison_data_frame$next_element[which_history[i]]))
  }
  return(list(which_history,ciag))
}

ciag <- Markov_new(without_heteregonity_chart$gap_bin)

sum(ciag[[2]]==without_heteregonity_chart$gap_bin)/80


unique(without_heteregonity_chart$gap_bin)
13/80
64/140



ciag <- 0
for (i in 6:length(example)){
  ciag[i-5] <- Markov_new(example[(i-5):i])[[2]][i]
  
}




ciag==example

Markov_new(example[5:10])

Markov_new(example)
debug(Markov_new)

ciag <- Markov_new(example)[[2]]

sum(ciag==example)/298

ciag <- Markov_new(without_heteregonity_chart$gap_bin)[[2]]


sum(without_heteregonity_chart$gap_bin==ciag)/80

ciag <- Markov_new(example)[[1]]

debug(Markov_new)

sum(ciag==example)/298


ciag <- Markov_new(without_heteregonity_chart$gap_bin)[[2]]

ciag
example

sum(ciag[2:80]==without_heteregonity_chart$gap_bin[1:79])/79
#51.89

#kwestia dwóch maksów? Problem jest taki, że jak szuka w poziomie to wtedy nie znajduje tylko pierwszy.
temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:2)],example[c(1:2)]),c((history+1):(2*history))],2,sum)))
probability_matrix <- temp_table/sum(temp_table)
next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
next_element_probability <- max(probability_matrix)
comparison_data_frame <- data.frame(next_element,next_element_probability)


which.max(test)


test <- (matrix(c(1,5,4,5),ncol=4))

colnames(test) <- c(1,4,6,7)

max(test)
agrregate(transition_matrix)

transition_matrix[apply(transition_matrix[,c(6:10)],1,sum)>93,]

max(apply(transition_matrix[,c(6:10)],1,sum))


transition_matrix[(transition_matrix[,1]==example[1]),c((history+1):(dim(transition_matrix)[2]))] <- transition_matrix[(transition_matrix[,1]==example[1]),c(6:10)] + 1

transition_matrix[(transition_matrix[,c(1,2)]==example[c(1,2)]),c((history+1):(dim(transition_matrix)[2]))] <- transition_matrix[(transition_matrix[,c(1,2)]==example[c(1,2)]),c((history+1):(dim(transition_matrix)[2]))] + 1





for (i in 1:length(unique(example))) transition_matrix$paste()

for (i in 1:length(unique(example))) transition_matrix$paste(sort(unique(example))[i]) == 0

cbind(transition_matrix,0)
