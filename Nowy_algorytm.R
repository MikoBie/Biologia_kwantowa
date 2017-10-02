library(gtools)
library(plyr)
library(knitr)
library(readr)
library(kableExtra)

#przykłady z ludźmi

example_1 <- read_csv("~/Desktop/Psychologia kwantowa/Survey/los_chem_zera_3_3_10_57_34.csv")
example_2 <- read_csv("~/Desktop/Psychologia kwantowa/Survey/los_chem_zera_3_1_14_17_18.csv")

#przygotowanie dla nowego algorytmu
example_1_new <- example_1$key
example_2_new <- example_2$key

#przygotowanie dla starego algorytmu
example_1_old <- example_1
colnames(example_1_old)[2] <- "gap_bin"
example_2_old <- example_2
colnames(example_2_old)[2] <- "gap_bin"


#symulacje Michała

setwd("~/Desktop/Biologia_kwantowa")
vector_of_files<-list.files("~/Desktop/Biologia_kwantowa",pattern = ".dat$")

#with heteregonity
with_heteregonity <- read.delim(paste0(vector_of_files[1]),header = F, sep = '\t')
#without heteregonity
without_heteregonity <- read.delim(paste0(vector_of_files[2]),header = F, sep = '\t')

# Mój pierwszy ruch to policzenie odstępów.

with_heteregonity$gap <- c(NA,c(with_heteregonity$V1[2:length(with_heteregonity$V1)])-c(with_heteregonity$V1[1:(length(with_heteregonity$V1)-1)]))
without_heteregonity$gap <- c(NA,c(without_heteregonity$V1[2:length(without_heteregonity$V1)])-c(without_heteregonity$V1[1:(length(without_heteregonity$V1)-1)]))

# Pierwszy czas to NA bo czas pierwszego spike'a jest od włączenia nagrywania, dlatego bez żalu go usuwam

with_heteregonity_chart <- with_heteregonity[-1,]
without_heteregonity_chart <- without_heteregonity[-1,]

#zbinowanie wyników

with_heteregonity_chart$gap_bin <- as.numeric(as.character(cut(with_heteregonity_chart$gap,c(0, seq(10, 120, by = 10)),c(1:12))))
without_heteregonity_chart$gap_bin <- as.numeric(as.character(cut(without_heteregonity_chart$gap,c(0, seq(10, 200, by = 10)),c(1:20))))

#przygotowanie dla nowego algorytmu
with_heteregonity_new <- with_heteregonity_chart$gap_bin
without_heteregonity_new <- without_heteregonity_chart$gap_bin

quantile_with_1 <- as.numeric(as.character(cut(with_heteregonity_chart$gap,quantile(with_heteregonity_chart$gap, prob=seq(0,1,0.1),type=1),labels=F)))
quantile_with_1[61] <- 1
quantile_with_2 <- as.numeric(as.character(cut(with_heteregonity_chart$gap,quantile(with_heteregonity_chart$gap, prob=seq(0,1,0.1),type=2),labels=F)))
quantile_with_2[61] <- 1
quantile_with_3 <- as.numeric(as.character(cut(with_heteregonity_chart$gap,quantile(with_heteregonity_chart$gap, prob=seq(0,1,0.1),type=3),labels=F)))
quantile_with_3[61] <- 1

sum(quantile_with_2==quantile_with_1)

quantile_without_1 <- as.numeric(as.character(cut(without_heteregonity_chart$gap,quantile(without_heteregonity_chart$gap, prob=seq(0,1,0.1),type=1),labels=F)))
quantile_without_1[51] <- 1
quantile_without_2 <- as.numeric(as.character(cut(without_heteregonity_chart$gap,quantile(without_heteregonity_chart$gap, prob=seq(0,1,0.1),type=2),labels=F)))
quantile_without_2[51] <- 1
quantile_without_3 <- as.numeric(as.character(cut(without_heteregonity_chart$gap,quantile(without_heteregonity_chart$gap, prob=seq(0,1,0.1),type=3),labels=F)))
quantile_without_3[51] <- 1

sum(quantile_without_2==quantile_without_3)

with_heteregonity_new <- quantile_with_1
without_heteregonity_new <- quantile_without_1

#rozkład podział na 10

range(with_heteregonity_chart$gap)
(113.7-12.9)/10
range(without_heteregonity_chart$gap)
(194.2-17.0)/10

with_heteregonity_new <- (cut(with_heteregonity_chart$gap,seq(12.9,113.7,10.08),labels=F))

without_heteregonity_new <- cut(without_heteregonity_chart$gap,seq(17.0,194.2,17.72),labels=F)
without_heteregonity_new[51] <- 1


#odchyelnie standardowe
mean_with <- mean(with_heteregonity_chart$gap)
sd_with <- sd(with_heteregonity_chart$gap)

with_heteregonity_new <- (cut(with_heteregonity_chart$gap,c(mean_with-3*sd_with,mean_with-2*sd_with,mean_with-1*sd_with,mean_with,mean_with+1*sd_with,mean_with+2*sd_with,mean_with+3*sd_with,mean_with+4*sd_with,mean_with+5*sd_with,mean_with+6*sd_with),labels=F))

mean_without <- mean(without_heteregonity_chart$gap)
sd_without <- sd(without_heteregonity_chart$gap)

without_heteregonity_new <- (cut(without_heteregonity_chart$gap,c(mean_without-3*sd_without,mean_without-2*sd_without,mean_without-1*sd_without,mean_without,mean_without+1*sd_without,mean_without+2*sd_without,mean_without+3*sd_without,mean_without+4*sd_without,mean_without+5*sd_without,mean_without+6*sd_without),labels=F))

unique(with_heteregonity_new)
unique(without_heteregonity_new)



#Ciekawe bo historia dwa wydaje się być najlepsza, daje koło 
#kwestia dwóch maksimów do rozwiązania jeszcze została bo na razie wybiera pierwszy, a powininen losować.
#oryginalny nowy algorytm
Markov_new <- function(example){
  #niestety historię w tej funkcji trzeba regulować ręcznie poprzez zmianę liczby poniżej oraz dodanie linijek kodu w pętli for. Na razie ustawiona jest historia 5 ale zmiana tego to jakieś 30 sekund roboty.
  history <- 5
  #ta sama krótka funkcja co w poprzednim algorytmie służąca do porównywania wierszy macierzy z wektorem.
  compare <- function(table,vector){
    (wynik <- t(t(table)==vector))
    (wynik <- rowSums(wynik))
    wynik <- wynik>(length(vector)-1)
    return(wynik)
  }
  #stworzenie macierzy wszystkich stanów dla wszystkich historii. To jest zasadnicza różnica pomiędzy tym algorytmem i tym poprzednim. Ta macierz tworzona jest przez połączenie tak naprawdę dwóch macierzy. Z jednej strony macierzy kombinacji wszystkich stanów dla historii 5 (tylko dlatego, że tak ustawiona jest historia), a z drugiej strony kolumn z liczebnością przewidywanego wyrazu (Tabela 2 pokazuję tę macierz).
  transition_matrix <- permutations(n=length(unique(example)),r=history,v=unique(example),repeats.allowed = T)
  #nazwanie kolumn
  colnames(transition_matrix) <- paste0("h",c(1:history))
  temp_table <- data.frame(t(rep(0,length(unique(example)))))
  colnames(temp_table) <- sort(unique(example))
  #połączenie dwóch tabel
  transition_matrix <- cbind(transition_matrix,temp_table)
  #Zmienna, w której będzie zapisywana historia, za pomocą, której robione są przewidywania. Jej pierwsze dwa elementy to zera ponieważ pierwsze dwa elementy są przewidywane na podstawie zwykłego prawdopodobieństwa.
  which_history <- c(0,0)
  #zmienna, w której zapisywane są elementy ciągu. Pierwszy element jest po prostu losowany z równym prawdopodobieństwem. Dla dwóch elementów jest to 0.5. Drugi element jest po prostu przepisaniem pierwszego wyrazu oryginalnego ciągu bo na podstawie jednego wyrazu tylko takie przewidywanie można zrobić (w tym sensie trochę oszukuję z historią)
  ciag <- sample(unique(example),1,replace=F)
  ciag[2] <- example[1]
  #to jest serce tej funkcji, tego algorytmu. Podzielone jest na odpowiednie historie. Dla każdej historii. Wydaje mi się, że w miarę jasno opisane jest działanie tego algorytmu powyżej.
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
    #to jest ciągle aktualizująca się tabela, w której zapisywany jest przewidywany następny wyraz, jego prawdopodobieństwo oraz długość historii na podstawie, której został przewidziany. Po uaktualnieniu każdej historii jest 
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
    #zapisanie historii, która zostanie wybrana do przewidzenia  
    which_history[i] <- which.max(comparison_data_frame$next_element_probability)
    #zapisanie kolejnego przewidzianego elementu.
    ciag[i] <- as.numeric(as.character(comparison_data_frame$next_element[which_history[i]]))
  }
  return(list(which_history,ciag,transition_matrix))
}

list_with_heteregonity_full <- Markov_new(with_heteregonity_new)
list_without_heteregonity_full <- Markov_new(without_heteregonity_new)

sum(list_with_heteregonity_full[[2]][1:80]==with_heteregonity_new[1:80])/80
sum(list_without_heteregonity_full[[2]]==without_heteregonity_new)/80

#tylko historia 1.
Markov_new <- function(example){
  #niestety historię w tej funkcji trzeba regulować ręcznie poprzez zmianę liczby poniżej oraz dodanie linijek kodu w pętli for. Na razie ustawiona jest historia 5 ale zmiana tego to jakieś 30 sekund roboty.
  history <- 5
  #ta sama krótka funkcja co w poprzednim algorytmie służąca do porównywania wierszy macierzy z wektorem.
  compare <- function(table,vector){
    (wynik <- t(t(table)==vector))
    (wynik <- rowSums(wynik))
    wynik <- wynik>(length(vector)-1)
    return(wynik)
  }
  #stworzenie macierzy wszystkich stanów dla wszystkich historii. To jest zasadnicza różnica pomiędzy tym algorytmem i tym poprzednim. Ta macierz tworzona jest przez połączenie tak naprawdę dwóch macierzy. Z jednej strony macierzy kombinacji wszystkich stanów dla historii 5 (tylko dlatego, że tak ustawiona jest historia), a z drugiej strony kolumn z liczebnością przewidywanego wyrazu (Tabela 2 pokazuję tę macierz).
  transition_matrix <- permutations(n=length(unique(example)),r=history,v=unique(example),repeats.allowed = T)
  #nazwanie kolumn
  colnames(transition_matrix) <- paste0("h",c(1:history))
  temp_table <- data.frame(t(rep(0,length(unique(example)))))
  colnames(temp_table) <- sort(unique(example))
  #połączenie dwóch tabel
  transition_matrix <- cbind(transition_matrix,temp_table)
  #Zmienna, w której będzie zapisywana historia, za pomocą, której robione są przewidywania. Jej pierwsze dwa elementy to zera ponieważ pierwsze dwa elementy są przewidywane na podstawie zwykłego prawdopodobieństwa.
  which_history <- c(0,0)
  #zmienna, w której zapisywane są elementy ciągu. Pierwszy element jest po prostu losowany z równym prawdopodobieństwem. Dla dwóch elementów jest to 0.5. Drugi element jest po prostu przepisaniem pierwszego wyrazu oryginalnego ciągu bo na podstawie jednego wyrazu tylko takie przewidywanie można zrobić (w tym sensie trochę oszukuję z historią)
  ciag <- sample(unique(example),1,replace=F)
  ciag[2] <- example[1]
  #to jest serce tej funkcji, tego algorytmu. Podzielone jest na odpowiednie historie. Dla każdej historii. Wydaje mi się, że w miarę jasno opisane jest działanie tego algorytmu powyżej.
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
    #to jest ciągle aktualizująca się tabela, w której zapisywany jest przewidywany następny wyraz, jego prawdopodobieństwo oraz długość historii na podstawie, której został przewidziany. Po uaktualnieniu każdej historii jest 
    comparison_data_frame <- data.frame(next_element,next_element_probability)
    # #historia 2
    # if (i > 3){
    #   transition_matrix[compare(transition_matrix[,c(1:2)],example[c((i-3),(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:2)],example[c((i-3),(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    #   temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:2)],example[c((i-2):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    #   probability_matrix <- temp_table/sum(temp_table)
    #   if (sum(temp_table)<1){
    #     probability_matrix[1,] <- 1/length(unique(example))
    #   }
    #   next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    #   next_element_probability <- max(probability_matrix)
    #   if (sum(probability_matrix[1,]==next_element_probability)>1){
    #     next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    #   }
    #   comparison_data_frame <- rbind(comparison_data_frame,data.frame(next_element,next_element_probability))
    # }
    # #historia 3
    # if (i > 4){
    #   transition_matrix[compare(transition_matrix[,c(1:3)],example[c((i-4):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:3)],example[c((i-4):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    #   temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:3)],example[c((i-3):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    #   probability_matrix <- temp_table/sum(temp_table)
    #   if (sum(temp_table)<1){
    #     probability_matrix[1,] <- 1/length(unique(example))
    #   }
    #   next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    #   next_element_probability <- max(probability_matrix)
    #   if (sum(probability_matrix[1,]==next_element_probability)>1){
    #     next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    #   }
    #   comparison_data_frame <- rbind(comparison_data_frame,data.frame(next_element,next_element_probability))
    # }
    # #historia 4
    # if (i > 5){
    #   transition_matrix[compare(transition_matrix[,c(1:4)],example[c((i-5):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:4)],example[c((i-5):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    #   temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:4)],example[c((i-4):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    #   probability_matrix <- temp_table/sum(temp_table)
    #   if (sum(temp_table)<1){
    #     probability_matrix[1,] <- 1/length(unique(example))
    #   }
    #   next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    #   next_element_probability <- max(probability_matrix)
    #   if (sum(probability_matrix[1,]==next_element_probability)>1){
    #     next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    #   }
    #   comparison_data_frame <- rbind(comparison_data_frame,data.frame(next_element,next_element_probability))
    # }
    # #historia 5
    # if (i > 6){
    #   transition_matrix[compare(transition_matrix[,c(1:5)],example[c((i-6):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:5)],example[c((i-6):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    #   temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:5)],example[c((i-5):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    #   probability_matrix <- temp_table/sum(temp_table)
    #   if (sum(temp_table)<1){
    #     probability_matrix[1,] <- 1/length(unique(example))
    #   }
    #   next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    #   next_element_probability <- max(probability_matrix)
    #   if (sum(probability_matrix[1,]==next_element_probability)>1){
    #     next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    #   }
    #   comparison_data_frame <- rbind(comparison_data_frame,data.frame(next_element,next_element_probability))
    # }
    #zapisanie historii, która zostanie wybrana do przewidzenia  
    which_history[i] <- which.max(comparison_data_frame$next_element_probability)
    #zapisanie kolejnego przewidzianego elementu.
    ciag[i] <- as.numeric(as.character(comparison_data_frame$next_element[which_history[i]]))
  }
  return(list(which_history,ciag,transition_matrix))
}

list_with_heteregonity_only_h1 <- Markov_new(with_heteregonity_new)
list_without_heteregonity_only_h1 <- Markov_new(without_heteregonity_new)

sum(list_with_heteregonity_only_h1[[2]]==with_heteregonity_new)/140
sum(list_without_heteregonity_only_h1[[2]]==without_heteregonity_new)/80



#tylko historia 2.
Markov_new <- function(example){
  #niestety historię w tej funkcji trzeba regulować ręcznie poprzez zmianę liczby poniżej oraz dodanie linijek kodu w pętli for. Na razie ustawiona jest historia 5 ale zmiana tego to jakieś 30 sekund roboty.
  history <- 5
  #ta sama krótka funkcja co w poprzednim algorytmie służąca do porównywania wierszy macierzy z wektorem.
  compare <- function(table,vector){
    (wynik <- t(t(table)==vector))
    (wynik <- rowSums(wynik))
    wynik <- wynik>(length(vector)-1)
    return(wynik)
  }
  #stworzenie macierzy wszystkich stanów dla wszystkich historii. To jest zasadnicza różnica pomiędzy tym algorytmem i tym poprzednim. Ta macierz tworzona jest przez połączenie tak naprawdę dwóch macierzy. Z jednej strony macierzy kombinacji wszystkich stanów dla historii 5 (tylko dlatego, że tak ustawiona jest historia), a z drugiej strony kolumn z liczebnością przewidywanego wyrazu (Tabela 2 pokazuję tę macierz).
  transition_matrix <- permutations(n=length(unique(example)),r=history,v=unique(example),repeats.allowed = T)
  #nazwanie kolumn
  colnames(transition_matrix) <- paste0("h",c(1:history))
  temp_table <- data.frame(t(rep(0,length(unique(example)))))
  colnames(temp_table) <- sort(unique(example))
  #połączenie dwóch tabel
  transition_matrix <- cbind(transition_matrix,temp_table)
  #Zmienna, w której będzie zapisywana historia, za pomocą, której robione są przewidywania. Jej pierwsze dwa elementy to zera ponieważ pierwsze dwa elementy są przewidywane na podstawie zwykłego prawdopodobieństwa.
  which_history <- c(0,0,0)
  #zmienna, w której zapisywane są elementy ciągu. Pierwszy element jest po prostu losowany z równym prawdopodobieństwem. Dla dwóch elementów jest to 0.5. Drugi element jest po prostu przepisaniem pierwszego wyrazu oryginalnego ciągu bo na podstawie jednego wyrazu tylko takie przewidywanie można zrobić (w tym sensie trochę oszukuję z historią)
  ciag <- sample(unique(example),1,replace=F)
  ciag[2] <- example[1]
  ciag[3] <- sample(ciag,1,replace=F)
  #to jest serce tej funkcji, tego algorytmu. Podzielone jest na odpowiednie historie. Dla każdej historii. Wydaje mi się, że w miarę jasno opisane jest działanie tego algorytmu powyżej.
  for (i in 4:(length(example))){
    #historia 1
    # transition_matrix[compare(transition_matrix[,1],example[i-2]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,1],example[i-2]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    # temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,1],example[i-1]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    # probability_matrix <- temp_table/sum(temp_table)
    # if (sum(temp_table)<1){
    #   probability_matrix[1,] <- 1/length(unique(example))
    # }
    # next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    # next_element_probability <- max(probability_matrix)
    # if (sum(probability_matrix[1,]==next_element_probability)>1){
    #   next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    # }
    # #to jest ciągle aktualizująca się tabela, w której zapisywany jest przewidywany następny wyraz, jego prawdopodobieństwo oraz długość historii na podstawie, której został przewidziany. Po uaktualnieniu każdej historii jest 
    # comparison_data_frame <- data.frame(next_element,next_element_probability)
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
      comparison_data_frame <- data.frame(next_element,next_element_probability)
    }
    # #historia 3
    # if (i > 4){
    #   transition_matrix[compare(transition_matrix[,c(1:3)],example[c((i-4):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:3)],example[c((i-4):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    #   temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:3)],example[c((i-3):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    #   probability_matrix <- temp_table/sum(temp_table)
    #   if (sum(temp_table)<1){
    #     probability_matrix[1,] <- 1/length(unique(example))
    #   }
    #   next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    #   next_element_probability <- max(probability_matrix)
    #   if (sum(probability_matrix[1,]==next_element_probability)>1){
    #     next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    #   }
    #   comparison_data_frame <- rbind(comparison_data_frame,data.frame(next_element,next_element_probability))
    # }
    # #historia 4
    # if (i > 5){
    #   transition_matrix[compare(transition_matrix[,c(1:4)],example[c((i-5):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:4)],example[c((i-5):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    #   temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:4)],example[c((i-4):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    #   probability_matrix <- temp_table/sum(temp_table)
    #   if (sum(temp_table)<1){
    #     probability_matrix[1,] <- 1/length(unique(example))
    #   }
    #   next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    #   next_element_probability <- max(probability_matrix)
    #   if (sum(probability_matrix[1,]==next_element_probability)>1){
    #     next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    #   }
    #   comparison_data_frame <- rbind(comparison_data_frame,data.frame(next_element,next_element_probability))
    # }
    # #historia 5
    # if (i > 6){
    #   transition_matrix[compare(transition_matrix[,c(1:5)],example[c((i-6):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:5)],example[c((i-6):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    #   temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:5)],example[c((i-5):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    #   probability_matrix <- temp_table/sum(temp_table)
    #   if (sum(temp_table)<1){
    #     probability_matrix[1,] <- 1/length(unique(example))
    #   }
    #   next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    #   next_element_probability <- max(probability_matrix)
    #   if (sum(probability_matrix[1,]==next_element_probability)>1){
    #     next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    #   }
    #   comparison_data_frame <- rbind(comparison_data_frame,data.frame(next_element,next_element_probability))
    # }
    #zapisanie historii, która zostanie wybrana do przewidzenia  
    which_history[i] <- which.max(comparison_data_frame$next_element_probability)
    #zapisanie kolejnego przewidzianego elementu.
    ciag[i] <- as.numeric(as.character(comparison_data_frame$next_element[which_history[i]]))
  }
  return(list(which_history,ciag,transition_matrix))
}

list_with_heteregonity_only_h2 <- Markov_new(with_heteregonity_new)
list_without_heteregonity_only_h2 <- Markov_new(without_heteregonity_new)

sum(list_with_heteregonity_only_h2[[2]]==with_heteregonity_new)/140
sum(list_without_heteregonity_only_h2[[2]]==without_heteregonity_new)/80

#tylko historia 3.
Markov_new <- function(example){
  #niestety historię w tej funkcji trzeba regulować ręcznie poprzez zmianę liczby poniżej oraz dodanie linijek kodu w pętli for. Na razie ustawiona jest historia 5 ale zmiana tego to jakieś 30 sekund roboty.
  history <- 5
  #ta sama krótka funkcja co w poprzednim algorytmie służąca do porównywania wierszy macierzy z wektorem.
  compare <- function(table,vector){
    (wynik <- t(t(table)==vector))
    (wynik <- rowSums(wynik))
    wynik <- wynik>(length(vector)-1)
    return(wynik)
  }
  #stworzenie macierzy wszystkich stanów dla wszystkich historii. To jest zasadnicza różnica pomiędzy tym algorytmem i tym poprzednim. Ta macierz tworzona jest przez połączenie tak naprawdę dwóch macierzy. Z jednej strony macierzy kombinacji wszystkich stanów dla historii 5 (tylko dlatego, że tak ustawiona jest historia), a z drugiej strony kolumn z liczebnością przewidywanego wyrazu (Tabela 2 pokazuję tę macierz).
  transition_matrix <- permutations(n=length(unique(example)),r=history,v=unique(example),repeats.allowed = T)
  #nazwanie kolumn
  colnames(transition_matrix) <- paste0("h",c(1:history))
  temp_table <- data.frame(t(rep(0,length(unique(example)))))
  colnames(temp_table) <- sort(unique(example))
  #połączenie dwóch tabel
  transition_matrix <- cbind(transition_matrix,temp_table)
  #Zmienna, w której będzie zapisywana historia, za pomocą, której robione są przewidywania. Jej pierwsze dwa elementy to zera ponieważ pierwsze dwa elementy są przewidywane na podstawie zwykłego prawdopodobieństwa.
  which_history <- c(0,0,0,0)
  #zmienna, w której zapisywane są elementy ciągu. Pierwszy element jest po prostu losowany z równym prawdopodobieństwem. Dla dwóch elementów jest to 0.5. Drugi element jest po prostu przepisaniem pierwszego wyrazu oryginalnego ciągu bo na podstawie jednego wyrazu tylko takie przewidywanie można zrobić (w tym sensie trochę oszukuję z historią)
  ciag <- sample(unique(example),1,replace=F)
  ciag[2] <- example[1]
  ciag[3] <- sample(ciag,1,replace=F)
  ciag[4] <- sample(ciag,1,replace=F)
  #to jest serce tej funkcji, tego algorytmu. Podzielone jest na odpowiednie historie. Dla każdej historii. Wydaje mi się, że w miarę jasno opisane jest działanie tego algorytmu powyżej.
  for (i in 5:(length(example))){
    #historia 1
    # transition_matrix[compare(transition_matrix[,1],example[i-2]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,1],example[i-2]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    # temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,1],example[i-1]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    # probability_matrix <- temp_table/sum(temp_table)
    # if (sum(temp_table)<1){
    #   probability_matrix[1,] <- 1/length(unique(example))
    # }
    # next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    # next_element_probability <- max(probability_matrix)
    # if (sum(probability_matrix[1,]==next_element_probability)>1){
    #   next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    # }
    # #to jest ciągle aktualizująca się tabela, w której zapisywany jest przewidywany następny wyraz, jego prawdopodobieństwo oraz długość historii na podstawie, której został przewidziany. Po uaktualnieniu każdej historii jest 
    # comparison_data_frame <- data.frame(next_element,next_element_probability)
    #historia 2
    # if (i > 3){
    #   transition_matrix[compare(transition_matrix[,c(1:2)],example[c((i-3),(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:2)],example[c((i-3),(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    #   temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:2)],example[c((i-2):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    #   probability_matrix <- temp_table/sum(temp_table)
    #   if (sum(temp_table)<1){
    #     probability_matrix[1,] <- 1/length(unique(example))
    #   }
    #   next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    #   next_element_probability <- max(probability_matrix)
    #   if (sum(probability_matrix[1,]==next_element_probability)>1){
    #     next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    #   }
    #   comparison_data_frame <- data.frame(next_element,next_element_probability)
    # }
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
      comparison_data_frame <- data.frame(next_element,next_element_probability)
    }
    # #historia 4
    # if (i > 5){
    #   transition_matrix[compare(transition_matrix[,c(1:4)],example[c((i-5):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:4)],example[c((i-5):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    #   temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:4)],example[c((i-4):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    #   probability_matrix <- temp_table/sum(temp_table)
    #   if (sum(temp_table)<1){
    #     probability_matrix[1,] <- 1/length(unique(example))
    #   }
    #   next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    #   next_element_probability <- max(probability_matrix)
    #   if (sum(probability_matrix[1,]==next_element_probability)>1){
    #     next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    #   }
    #   comparison_data_frame <- rbind(comparison_data_frame,data.frame(next_element,next_element_probability))
    # }
    # #historia 5
    # if (i > 6){
    #   transition_matrix[compare(transition_matrix[,c(1:5)],example[c((i-6):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:5)],example[c((i-6):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    #   temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:5)],example[c((i-5):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    #   probability_matrix <- temp_table/sum(temp_table)
    #   if (sum(temp_table)<1){
    #     probability_matrix[1,] <- 1/length(unique(example))
    #   }
    #   next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    #   next_element_probability <- max(probability_matrix)
    #   if (sum(probability_matrix[1,]==next_element_probability)>1){
    #     next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    #   }
    #   comparison_data_frame <- rbind(comparison_data_frame,data.frame(next_element,next_element_probability))
    # }
    #zapisanie historii, która zostanie wybrana do przewidzenia  
    which_history[i] <- which.max(comparison_data_frame$next_element_probability)
    #zapisanie kolejnego przewidzianego elementu.
    ciag[i] <- as.numeric(as.character(comparison_data_frame$next_element[which_history[i]]))
  }
  return(list(which_history,ciag,transition_matrix))
}

list_with_heteregonity_only_h3 <- Markov_new(with_heteregonity_new)
list_without_heteregonity_only_h3 <- Markov_new(without_heteregonity_new)

sum(list_with_heteregonity_only_h3[[2]]==with_heteregonity_new)/140
sum(list_without_heteregonity_only_h3[[2]]==without_heteregonity_new)/80

#tylko historia 4.
Markov_new <- function(example){
  #niestety historię w tej funkcji trzeba regulować ręcznie poprzez zmianę liczby poniżej oraz dodanie linijek kodu w pętli for. Na razie ustawiona jest historia 5 ale zmiana tego to jakieś 30 sekund roboty.
  history <- 5
  #ta sama krótka funkcja co w poprzednim algorytmie służąca do porównywania wierszy macierzy z wektorem.
  compare <- function(table,vector){
    (wynik <- t(t(table)==vector))
    (wynik <- rowSums(wynik))
    wynik <- wynik>(length(vector)-1)
    return(wynik)
  }
  #stworzenie macierzy wszystkich stanów dla wszystkich historii. To jest zasadnicza różnica pomiędzy tym algorytmem i tym poprzednim. Ta macierz tworzona jest przez połączenie tak naprawdę dwóch macierzy. Z jednej strony macierzy kombinacji wszystkich stanów dla historii 5 (tylko dlatego, że tak ustawiona jest historia), a z drugiej strony kolumn z liczebnością przewidywanego wyrazu (Tabela 2 pokazuję tę macierz).
  transition_matrix <- permutations(n=length(unique(example)),r=history,v=unique(example),repeats.allowed = T)
  #nazwanie kolumn
  colnames(transition_matrix) <- paste0("h",c(1:history))
  temp_table <- data.frame(t(rep(0,length(unique(example)))))
  colnames(temp_table) <- sort(unique(example))
  #połączenie dwóch tabel
  transition_matrix <- cbind(transition_matrix,temp_table)
  #Zmienna, w której będzie zapisywana historia, za pomocą, której robione są przewidywania. Jej pierwsze dwa elementy to zera ponieważ pierwsze dwa elementy są przewidywane na podstawie zwykłego prawdopodobieństwa.
  which_history <- c(0,0,0,0,0)
  #zmienna, w której zapisywane są elementy ciągu. Pierwszy element jest po prostu losowany z równym prawdopodobieństwem. Dla dwóch elementów jest to 0.5. Drugi element jest po prostu przepisaniem pierwszego wyrazu oryginalnego ciągu bo na podstawie jednego wyrazu tylko takie przewidywanie można zrobić (w tym sensie trochę oszukuję z historią)
  ciag <- sample(unique(example),1,replace=F)
  ciag[2] <- example[1]
  ciag[3] <- sample(ciag,1,replace=F)
  ciag[4] <- sample(ciag,1,replace=F)
  ciag[5] <- sample(ciag,1,replace=F)
  #to jest serce tej funkcji, tego algorytmu. Podzielone jest na odpowiednie historie. Dla każdej historii. Wydaje mi się, że w miarę jasno opisane jest działanie tego algorytmu powyżej.
  for (i in 6:(length(example))){
    #historia 1
    # transition_matrix[compare(transition_matrix[,1],example[i-2]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,1],example[i-2]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    # temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,1],example[i-1]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    # probability_matrix <- temp_table/sum(temp_table)
    # if (sum(temp_table)<1){
    #   probability_matrix[1,] <- 1/length(unique(example))
    # }
    # next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    # next_element_probability <- max(probability_matrix)
    # if (sum(probability_matrix[1,]==next_element_probability)>1){
    #   next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    # }
    # #to jest ciągle aktualizująca się tabela, w której zapisywany jest przewidywany następny wyraz, jego prawdopodobieństwo oraz długość historii na podstawie, której został przewidziany. Po uaktualnieniu każdej historii jest 
    # comparison_data_frame <- data.frame(next_element,next_element_probability)
    #historia 2
    # if (i > 3){
    #   transition_matrix[compare(transition_matrix[,c(1:2)],example[c((i-3),(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:2)],example[c((i-3),(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    #   temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:2)],example[c((i-2):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    #   probability_matrix <- temp_table/sum(temp_table)
    #   if (sum(temp_table)<1){
    #     probability_matrix[1,] <- 1/length(unique(example))
    #   }
    #   next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    #   next_element_probability <- max(probability_matrix)
    #   if (sum(probability_matrix[1,]==next_element_probability)>1){
    #     next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    #   }
    #   comparison_data_frame <- data.frame(next_element,next_element_probability)
    # }
    #historia 3
    # if (i > 4){
    #   transition_matrix[compare(transition_matrix[,c(1:3)],example[c((i-4):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:3)],example[c((i-4):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    #   temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:3)],example[c((i-3):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    #   probability_matrix <- temp_table/sum(temp_table)
    #   if (sum(temp_table)<1){
    #     probability_matrix[1,] <- 1/length(unique(example))
    #   }
    #   next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    #   next_element_probability <- max(probability_matrix)
    #   if (sum(probability_matrix[1,]==next_element_probability)>1){
    #     next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    #   }
    #   comparison_data_frame <- data.frame(next_element,next_element_probability)
    # }
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
      comparison_data_frame <- data.frame(next_element,next_element_probability)
    }
    # #historia 5
    # if (i > 6){
    #   transition_matrix[compare(transition_matrix[,c(1:5)],example[c((i-6):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:5)],example[c((i-6):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    #   temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:5)],example[c((i-5):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    #   probability_matrix <- temp_table/sum(temp_table)
    #   if (sum(temp_table)<1){
    #     probability_matrix[1,] <- 1/length(unique(example))
    #   }
    #   next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    #   next_element_probability <- max(probability_matrix)
    #   if (sum(probability_matrix[1,]==next_element_probability)>1){
    #     next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    #   }
    #   comparison_data_frame <- rbind(comparison_data_frame,data.frame(next_element,next_element_probability))
    # }
    #zapisanie historii, która zostanie wybrana do przewidzenia  
    which_history[i] <- which.max(comparison_data_frame$next_element_probability)
    #zapisanie kolejnego przewidzianego elementu.
    ciag[i] <- as.numeric(as.character(comparison_data_frame$next_element[which_history[i]]))
  }
  return(list(which_history,ciag,transition_matrix))
}

list_with_heteregonity_only_h4 <- Markov_new(with_heteregonity_new)
list_without_heteregonity_only_h4 <- Markov_new(without_heteregonity_new)

sum(list_with_heteregonity_only_h4[[2]]==with_heteregonity_new)/140
sum(list_without_heteregonity_only_h4[[2]]==without_heteregonity_new)/80

#tylko historia 5.
Markov_new <- function(example){
  #niestety historię w tej funkcji trzeba regulować ręcznie poprzez zmianę liczby poniżej oraz dodanie linijek kodu w pętli for. Na razie ustawiona jest historia 5 ale zmiana tego to jakieś 30 sekund roboty.
  history <- 5
  #ta sama krótka funkcja co w poprzednim algorytmie służąca do porównywania wierszy macierzy z wektorem.
  compare <- function(table,vector){
    (wynik <- t(t(table)==vector))
    (wynik <- rowSums(wynik))
    wynik <- wynik>(length(vector)-1)
    return(wynik)
  }
  #stworzenie macierzy wszystkich stanów dla wszystkich historii. To jest zasadnicza różnica pomiędzy tym algorytmem i tym poprzednim. Ta macierz tworzona jest przez połączenie tak naprawdę dwóch macierzy. Z jednej strony macierzy kombinacji wszystkich stanów dla historii 5 (tylko dlatego, że tak ustawiona jest historia), a z drugiej strony kolumn z liczebnością przewidywanego wyrazu (Tabela 2 pokazuję tę macierz).
  transition_matrix <- permutations(n=length(unique(example)),r=history,v=unique(example),repeats.allowed = T)
  #nazwanie kolumn
  colnames(transition_matrix) <- paste0("h",c(1:history))
  temp_table <- data.frame(t(rep(0,length(unique(example)))))
  colnames(temp_table) <- sort(unique(example))
  #połączenie dwóch tabel
  transition_matrix <- cbind(transition_matrix,temp_table)
  #Zmienna, w której będzie zapisywana historia, za pomocą, której robione są przewidywania. Jej pierwsze dwa elementy to zera ponieważ pierwsze dwa elementy są przewidywane na podstawie zwykłego prawdopodobieństwa.
  which_history <- c(0,0,0,0,0,0)
  #zmienna, w której zapisywane są elementy ciągu. Pierwszy element jest po prostu losowany z równym prawdopodobieństwem. Dla dwóch elementów jest to 0.5. Drugi element jest po prostu przepisaniem pierwszego wyrazu oryginalnego ciągu bo na podstawie jednego wyrazu tylko takie przewidywanie można zrobić (w tym sensie trochę oszukuję z historią)
  ciag <- sample(unique(example),1,replace=F)
  ciag[2] <- example[1]
  ciag[3] <- sample(ciag,1,replace=F)
  ciag[4] <- sample(ciag,1,replace=F)
  ciag[5] <- sample(ciag,1,replace=F)
  ciag[6] <- sample(ciag,1,replace=F)
  #to jest serce tej funkcji, tego algorytmu. Podzielone jest na odpowiednie historie. Dla każdej historii. Wydaje mi się, że w miarę jasno opisane jest działanie tego algorytmu powyżej.
  for (i in 7:(length(example))){
    #historia 1
    # transition_matrix[compare(transition_matrix[,1],example[i-2]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,1],example[i-2]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    # temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,1],example[i-1]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    # probability_matrix <- temp_table/sum(temp_table)
    # if (sum(temp_table)<1){
    #   probability_matrix[1,] <- 1/length(unique(example))
    # }
    # next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    # next_element_probability <- max(probability_matrix)
    # if (sum(probability_matrix[1,]==next_element_probability)>1){
    #   next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    # }
    # #to jest ciągle aktualizująca się tabela, w której zapisywany jest przewidywany następny wyraz, jego prawdopodobieństwo oraz długość historii na podstawie, której został przewidziany. Po uaktualnieniu każdej historii jest 
    # comparison_data_frame <- data.frame(next_element,next_element_probability)
    #historia 2
    # if (i > 3){
    #   transition_matrix[compare(transition_matrix[,c(1:2)],example[c((i-3),(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:2)],example[c((i-3),(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    #   temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:2)],example[c((i-2):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    #   probability_matrix <- temp_table/sum(temp_table)
    #   if (sum(temp_table)<1){
    #     probability_matrix[1,] <- 1/length(unique(example))
    #   }
    #   next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    #   next_element_probability <- max(probability_matrix)
    #   if (sum(probability_matrix[1,]==next_element_probability)>1){
    #     next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    #   }
    #   comparison_data_frame <- data.frame(next_element,next_element_probability)
    # }
    #historia 3
    # if (i > 4){
    #   transition_matrix[compare(transition_matrix[,c(1:3)],example[c((i-4):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:3)],example[c((i-4):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    #   temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:3)],example[c((i-3):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    #   probability_matrix <- temp_table/sum(temp_table)
    #   if (sum(temp_table)<1){
    #     probability_matrix[1,] <- 1/length(unique(example))
    #   }
    #   next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    #   next_element_probability <- max(probability_matrix)
    #   if (sum(probability_matrix[1,]==next_element_probability)>1){
    #     next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    #   }
    #   comparison_data_frame <- data.frame(next_element,next_element_probability)
    # }
    #historia 4
    # if (i > 5){
    #   transition_matrix[compare(transition_matrix[,c(1:4)],example[c((i-5):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:4)],example[c((i-5):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    #   temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:4)],example[c((i-4):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    #   probability_matrix <- temp_table/sum(temp_table)
    #   if (sum(temp_table)<1){
    #     probability_matrix[1,] <- 1/length(unique(example))
    #   }
    #   next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    #   next_element_probability <- max(probability_matrix)
    #   if (sum(probability_matrix[1,]==next_element_probability)>1){
    #     next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    #   }
    #   comparison_data_frame <- data.frame(next_element,next_element_probability)
    # }
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
      comparison_data_frame <- data.frame(next_element,next_element_probability)
    }
    #zapisanie historii, która zostanie wybrana do przewidzenia  
    which_history[i] <- which.max(comparison_data_frame$next_element_probability)
    #zapisanie kolejnego przewidzianego elementu.
    ciag[i] <- as.numeric(as.character(comparison_data_frame$next_element[which_history[i]]))
  }
  return(list(which_history,ciag,transition_matrix))
}

list_with_heteregonity_only_h5 <- Markov_new(with_heteregonity_new)
list_without_heteregonity_only_h5 <- Markov_new(without_heteregonity_new)

sum(list_with_heteregonity_only_h5[[2]]==with_heteregonity_new)/140
sum(list_without_heteregonity_only_h5[[2]]==without_heteregonity_new)/80


#tylko historia 1 i 2
Markov_new <- function(example){
  #niestety historię w tej funkcji trzeba regulować ręcznie poprzez zmianę liczby poniżej oraz dodanie linijek kodu w pętli for. Na razie ustawiona jest historia 5 ale zmiana tego to jakieś 30 sekund roboty.
  history <- 5
  #ta sama krótka funkcja co w poprzednim algorytmie służąca do porównywania wierszy macierzy z wektorem.
  compare <- function(table,vector){
    (wynik <- t(t(table)==vector))
    (wynik <- rowSums(wynik))
    wynik <- wynik>(length(vector)-1)
    return(wynik)
  }
  #stworzenie macierzy wszystkich stanów dla wszystkich historii. To jest zasadnicza różnica pomiędzy tym algorytmem i tym poprzednim. Ta macierz tworzona jest przez połączenie tak naprawdę dwóch macierzy. Z jednej strony macierzy kombinacji wszystkich stanów dla historii 5 (tylko dlatego, że tak ustawiona jest historia), a z drugiej strony kolumn z liczebnością przewidywanego wyrazu (Tabela 2 pokazuję tę macierz).
  transition_matrix <- permutations(n=length(unique(example)),r=history,v=unique(example),repeats.allowed = T)
  #nazwanie kolumn
  colnames(transition_matrix) <- paste0("h",c(1:history))
  temp_table <- data.frame(t(rep(0,length(unique(example)))))
  colnames(temp_table) <- sort(unique(example))
  #połączenie dwóch tabel
  transition_matrix <- cbind(transition_matrix,temp_table)
  #Zmienna, w której będzie zapisywana historia, za pomocą, której robione są przewidywania. Jej pierwsze dwa elementy to zera ponieważ pierwsze dwa elementy są przewidywane na podstawie zwykłego prawdopodobieństwa.
  which_history <- c(0,0)
  #zmienna, w której zapisywane są elementy ciągu. Pierwszy element jest po prostu losowany z równym prawdopodobieństwem. Dla dwóch elementów jest to 0.5. Drugi element jest po prostu przepisaniem pierwszego wyrazu oryginalnego ciągu bo na podstawie jednego wyrazu tylko takie przewidywanie można zrobić (w tym sensie trochę oszukuję z historią)
  ciag <- sample(unique(example),1,replace=F)
  ciag[2] <- example[1]
  #to jest serce tej funkcji, tego algorytmu. Podzielone jest na odpowiednie historie. Dla każdej historii. Wydaje mi się, że w miarę jasno opisane jest działanie tego algorytmu powyżej.
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
    #to jest ciągle aktualizująca się tabela, w której zapisywany jest przewidywany następny wyraz, jego prawdopodobieństwo oraz długość historii na podstawie, której został przewidziany. Po uaktualnieniu każdej historii jest 
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
    # #historia 3
    # if (i > 4){
    #   transition_matrix[compare(transition_matrix[,c(1:3)],example[c((i-4):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:3)],example[c((i-4):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    #   temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:3)],example[c((i-3):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    #   probability_matrix <- temp_table/sum(temp_table)
    #   if (sum(temp_table)<1){
    #     probability_matrix[1,] <- 1/length(unique(example))
    #   }
    #   next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    #   next_element_probability <- max(probability_matrix)
    #   if (sum(probability_matrix[1,]==next_element_probability)>1){
    #     next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    #   }
    #   comparison_data_frame <- rbind(comparison_data_frame,data.frame(next_element,next_element_probability))
    # }
    # #historia 4
    # if (i > 5){
    #   transition_matrix[compare(transition_matrix[,c(1:4)],example[c((i-5):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:4)],example[c((i-5):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    #   temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:4)],example[c((i-4):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    #   probability_matrix <- temp_table/sum(temp_table)
    #   if (sum(temp_table)<1){
    #     probability_matrix[1,] <- 1/length(unique(example))
    #   }
    #   next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    #   next_element_probability <- max(probability_matrix)
    #   if (sum(probability_matrix[1,]==next_element_probability)>1){
    #     next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    #   }
    #   comparison_data_frame <- rbind(comparison_data_frame,data.frame(next_element,next_element_probability))
    # }
    # #historia 5
    # if (i > 6){
    #   transition_matrix[compare(transition_matrix[,c(1:5)],example[c((i-6):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:5)],example[c((i-6):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    #   temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:5)],example[c((i-5):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    #   probability_matrix <- temp_table/sum(temp_table)
    #   if (sum(temp_table)<1){
    #     probability_matrix[1,] <- 1/length(unique(example))
    #   }
    #   next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    #   next_element_probability <- max(probability_matrix)
    #   if (sum(probability_matrix[1,]==next_element_probability)>1){
    #     next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    #   }
    #   comparison_data_frame <- rbind(comparison_data_frame,data.frame(next_element,next_element_probability))
    # }
    #zapisanie historii, która zostanie wybrana do przewidzenia  
    which_history[i] <- which.max(comparison_data_frame$next_element_probability)
    #zapisanie kolejnego przewidzianego elementu.
    ciag[i] <- as.numeric(as.character(comparison_data_frame$next_element[which_history[i]]))
  }
  return(list(which_history,ciag,transition_matrix))
}

list_with_heteregonity_only_h12 <- Markov_new(with_heteregonity_new)
list_without_heteregonity_only_h12 <- Markov_new(without_heteregonity_new)

sum(list_with_heteregonity_only_h12[[2]]==with_heteregonity_new)/140
sum(list_without_heteregonity_only_h12[[2]]==without_heteregonity_new)/80

#tylko historia 1 i 3
Markov_new <- function(example){
  #niestety historię w tej funkcji trzeba regulować ręcznie poprzez zmianę liczby poniżej oraz dodanie linijek kodu w pętli for. Na razie ustawiona jest historia 5 ale zmiana tego to jakieś 30 sekund roboty.
  history <- 5
  #ta sama krótka funkcja co w poprzednim algorytmie służąca do porównywania wierszy macierzy z wektorem.
  compare <- function(table,vector){
    (wynik <- t(t(table)==vector))
    (wynik <- rowSums(wynik))
    wynik <- wynik>(length(vector)-1)
    return(wynik)
  }
  #stworzenie macierzy wszystkich stanów dla wszystkich historii. To jest zasadnicza różnica pomiędzy tym algorytmem i tym poprzednim. Ta macierz tworzona jest przez połączenie tak naprawdę dwóch macierzy. Z jednej strony macierzy kombinacji wszystkich stanów dla historii 5 (tylko dlatego, że tak ustawiona jest historia), a z drugiej strony kolumn z liczebnością przewidywanego wyrazu (Tabela 2 pokazuję tę macierz).
  transition_matrix <- permutations(n=length(unique(example)),r=history,v=unique(example),repeats.allowed = T)
  #nazwanie kolumn
  colnames(transition_matrix) <- paste0("h",c(1:history))
  temp_table <- data.frame(t(rep(0,length(unique(example)))))
  colnames(temp_table) <- sort(unique(example))
  #połączenie dwóch tabel
  transition_matrix <- cbind(transition_matrix,temp_table)
  #Zmienna, w której będzie zapisywana historia, za pomocą, której robione są przewidywania. Jej pierwsze dwa elementy to zera ponieważ pierwsze dwa elementy są przewidywane na podstawie zwykłego prawdopodobieństwa.
  which_history <- c(0,0)
  #zmienna, w której zapisywane są elementy ciągu. Pierwszy element jest po prostu losowany z równym prawdopodobieństwem. Dla dwóch elementów jest to 0.5. Drugi element jest po prostu przepisaniem pierwszego wyrazu oryginalnego ciągu bo na podstawie jednego wyrazu tylko takie przewidywanie można zrobić (w tym sensie trochę oszukuję z historią)
  ciag <- sample(unique(example),1,replace=F)
  ciag[2] <- example[1]
  #to jest serce tej funkcji, tego algorytmu. Podzielone jest na odpowiednie historie. Dla każdej historii. Wydaje mi się, że w miarę jasno opisane jest działanie tego algorytmu powyżej.
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
    #to jest ciągle aktualizująca się tabela, w której zapisywany jest przewidywany następny wyraz, jego prawdopodobieństwo oraz długość historii na podstawie, której został przewidziany. Po uaktualnieniu każdej historii jest 
    comparison_data_frame <- data.frame(next_element,next_element_probability)
    # #historia 2
    # if (i > 3){
    #   transition_matrix[compare(transition_matrix[,c(1:2)],example[c((i-3),(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:2)],example[c((i-3),(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    #   temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:2)],example[c((i-2):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    #   probability_matrix <- temp_table/sum(temp_table)
    #   if (sum(temp_table)<1){
    #     probability_matrix[1,] <- 1/length(unique(example))
    #   }
    #   next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    #   next_element_probability <- max(probability_matrix)
    #   if (sum(probability_matrix[1,]==next_element_probability)>1){
    #     next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    #   }
    #   comparison_data_frame <- rbind(comparison_data_frame,data.frame(next_element,next_element_probability))
    # }
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
    # #historia 4
    # if (i > 5){
    #   transition_matrix[compare(transition_matrix[,c(1:4)],example[c((i-5):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:4)],example[c((i-5):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    #   temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:4)],example[c((i-4):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    #   probability_matrix <- temp_table/sum(temp_table)
    #   if (sum(temp_table)<1){
    #     probability_matrix[1,] <- 1/length(unique(example))
    #   }
    #   next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    #   next_element_probability <- max(probability_matrix)
    #   if (sum(probability_matrix[1,]==next_element_probability)>1){
    #     next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    #   }
    #   comparison_data_frame <- rbind(comparison_data_frame,data.frame(next_element,next_element_probability))
    # }
    # #historia 5
    # if (i > 6){
    #   transition_matrix[compare(transition_matrix[,c(1:5)],example[c((i-6):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:5)],example[c((i-6):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    #   temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:5)],example[c((i-5):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    #   probability_matrix <- temp_table/sum(temp_table)
    #   if (sum(temp_table)<1){
    #     probability_matrix[1,] <- 1/length(unique(example))
    #   }
    #   next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    #   next_element_probability <- max(probability_matrix)
    #   if (sum(probability_matrix[1,]==next_element_probability)>1){
    #     next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    #   }
    #   comparison_data_frame <- rbind(comparison_data_frame,data.frame(next_element,next_element_probability))
    # }
    #zapisanie historii, która zostanie wybrana do przewidzenia  
    which_history[i] <- which.max(comparison_data_frame$next_element_probability)
    #zapisanie kolejnego przewidzianego elementu.
    ciag[i] <- as.numeric(as.character(comparison_data_frame$next_element[which_history[i]]))
  }
  return(list(which_history,ciag,transition_matrix))
}

list_with_heteregonity_only_h13 <- Markov_new(with_heteregonity_new)
list_without_heteregonity_only_h13 <- Markov_new(without_heteregonity_new)

sum(list_with_heteregonity_only_h13[[2]]==with_heteregonity_new)/140
sum(list_without_heteregonity_only_h13[[2]]==without_heteregonity_new)/80

#historia 1,4
Markov_new <- function(example){
  #niestety historię w tej funkcji trzeba regulować ręcznie poprzez zmianę liczby poniżej oraz dodanie linijek kodu w pętli for. Na razie ustawiona jest historia 5 ale zmiana tego to jakieś 30 sekund roboty.
  history <- 5
  #ta sama krótka funkcja co w poprzednim algorytmie służąca do porównywania wierszy macierzy z wektorem.
  compare <- function(table,vector){
    (wynik <- t(t(table)==vector))
    (wynik <- rowSums(wynik))
    wynik <- wynik>(length(vector)-1)
    return(wynik)
  }
  #stworzenie macierzy wszystkich stanów dla wszystkich historii. To jest zasadnicza różnica pomiędzy tym algorytmem i tym poprzednim. Ta macierz tworzona jest przez połączenie tak naprawdę dwóch macierzy. Z jednej strony macierzy kombinacji wszystkich stanów dla historii 5 (tylko dlatego, że tak ustawiona jest historia), a z drugiej strony kolumn z liczebnością przewidywanego wyrazu (Tabela 2 pokazuję tę macierz).
  transition_matrix <- permutations(n=length(unique(example)),r=history,v=unique(example),repeats.allowed = T)
  #nazwanie kolumn
  colnames(transition_matrix) <- paste0("h",c(1:history))
  temp_table <- data.frame(t(rep(0,length(unique(example)))))
  colnames(temp_table) <- sort(unique(example))
  #połączenie dwóch tabel
  transition_matrix <- cbind(transition_matrix,temp_table)
  #Zmienna, w której będzie zapisywana historia, za pomocą, której robione są przewidywania. Jej pierwsze dwa elementy to zera ponieważ pierwsze dwa elementy są przewidywane na podstawie zwykłego prawdopodobieństwa.
  which_history <- c(0,0)
  #zmienna, w której zapisywane są elementy ciągu. Pierwszy element jest po prostu losowany z równym prawdopodobieństwem. Dla dwóch elementów jest to 0.5. Drugi element jest po prostu przepisaniem pierwszego wyrazu oryginalnego ciągu bo na podstawie jednego wyrazu tylko takie przewidywanie można zrobić (w tym sensie trochę oszukuję z historią)
  ciag <- sample(unique(example),1,replace=F)
  ciag[2] <- example[1]
  #to jest serce tej funkcji, tego algorytmu. Podzielone jest na odpowiednie historie. Dla każdej historii. Wydaje mi się, że w miarę jasno opisane jest działanie tego algorytmu powyżej.
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
    #to jest ciągle aktualizująca się tabela, w której zapisywany jest przewidywany następny wyraz, jego prawdopodobieństwo oraz długość historii na podstawie, której został przewidziany. Po uaktualnieniu każdej historii jest 
    comparison_data_frame <- data.frame(next_element,next_element_probability)
    # #historia 2
    # if (i > 3){
    #   transition_matrix[compare(transition_matrix[,c(1:2)],example[c((i-3),(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:2)],example[c((i-3),(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    #   temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:2)],example[c((i-2):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    #   probability_matrix <- temp_table/sum(temp_table)
    #   if (sum(temp_table)<1){
    #     probability_matrix[1,] <- 1/length(unique(example))
    #   }
    #   next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    #   next_element_probability <- max(probability_matrix)
    #   if (sum(probability_matrix[1,]==next_element_probability)>1){
    #     next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    #   }
    #   comparison_data_frame <- rbind(comparison_data_frame,data.frame(next_element,next_element_probability))
    # }
    #historia 3
    # if (i > 4){
    #   transition_matrix[compare(transition_matrix[,c(1:3)],example[c((i-4):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:3)],example[c((i-4):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    #   temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:3)],example[c((i-3):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    #   probability_matrix <- temp_table/sum(temp_table)
    #   if (sum(temp_table)<1){
    #     probability_matrix[1,] <- 1/length(unique(example))
    #   }
    #   next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    #   next_element_probability <- max(probability_matrix)
    #   if (sum(probability_matrix[1,]==next_element_probability)>1){
    #     next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    #   }
    #   comparison_data_frame <- rbind(comparison_data_frame,data.frame(next_element,next_element_probability))
    # }
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
    # #historia 5
    # if (i > 6){
    #   transition_matrix[compare(transition_matrix[,c(1:5)],example[c((i-6):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:5)],example[c((i-6):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    #   temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:5)],example[c((i-5):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    #   probability_matrix <- temp_table/sum(temp_table)
    #   if (sum(temp_table)<1){
    #     probability_matrix[1,] <- 1/length(unique(example))
    #   }
    #   next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    #   next_element_probability <- max(probability_matrix)
    #   if (sum(probability_matrix[1,]==next_element_probability)>1){
    #     next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    #   }
    #   comparison_data_frame <- rbind(comparison_data_frame,data.frame(next_element,next_element_probability))
    # }
    #zapisanie historii, która zostanie wybrana do przewidzenia  
    which_history[i] <- which.max(comparison_data_frame$next_element_probability)
    #zapisanie kolejnego przewidzianego elementu.
    ciag[i] <- as.numeric(as.character(comparison_data_frame$next_element[which_history[i]]))
  }
  return(list(which_history,ciag,transition_matrix))
}

list_with_heteregonity_only_h14 <- Markov_new(with_heteregonity_new)
list_without_heteregonity_only_h14 <- Markov_new(without_heteregonity_new)

sum(list_with_heteregonity_only_h14[[2]]==with_heteregonity_new)/140
sum(list_without_heteregonity_only_h14[[2]]==without_heteregonity_new)/80

#historia 1,5
Markov_new <- function(example){
  #niestety historię w tej funkcji trzeba regulować ręcznie poprzez zmianę liczby poniżej oraz dodanie linijek kodu w pętli for. Na razie ustawiona jest historia 5 ale zmiana tego to jakieś 30 sekund roboty.
  history <- 5
  #ta sama krótka funkcja co w poprzednim algorytmie służąca do porównywania wierszy macierzy z wektorem.
  compare <- function(table,vector){
    (wynik <- t(t(table)==vector))
    (wynik <- rowSums(wynik))
    wynik <- wynik>(length(vector)-1)
    return(wynik)
  }
  #stworzenie macierzy wszystkich stanów dla wszystkich historii. To jest zasadnicza różnica pomiędzy tym algorytmem i tym poprzednim. Ta macierz tworzona jest przez połączenie tak naprawdę dwóch macierzy. Z jednej strony macierzy kombinacji wszystkich stanów dla historii 5 (tylko dlatego, że tak ustawiona jest historia), a z drugiej strony kolumn z liczebnością przewidywanego wyrazu (Tabela 2 pokazuję tę macierz).
  transition_matrix <- permutations(n=length(unique(example)),r=history,v=unique(example),repeats.allowed = T)
  #nazwanie kolumn
  colnames(transition_matrix) <- paste0("h",c(1:history))
  temp_table <- data.frame(t(rep(0,length(unique(example)))))
  colnames(temp_table) <- sort(unique(example))
  #połączenie dwóch tabel
  transition_matrix <- cbind(transition_matrix,temp_table)
  #Zmienna, w której będzie zapisywana historia, za pomocą, której robione są przewidywania. Jej pierwsze dwa elementy to zera ponieważ pierwsze dwa elementy są przewidywane na podstawie zwykłego prawdopodobieństwa.
  which_history <- c(0,0)
  #zmienna, w której zapisywane są elementy ciągu. Pierwszy element jest po prostu losowany z równym prawdopodobieństwem. Dla dwóch elementów jest to 0.5. Drugi element jest po prostu przepisaniem pierwszego wyrazu oryginalnego ciągu bo na podstawie jednego wyrazu tylko takie przewidywanie można zrobić (w tym sensie trochę oszukuję z historią)
  ciag <- sample(unique(example),1,replace=F)
  ciag[2] <- example[1]
  #to jest serce tej funkcji, tego algorytmu. Podzielone jest na odpowiednie historie. Dla każdej historii. Wydaje mi się, że w miarę jasno opisane jest działanie tego algorytmu powyżej.
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
    #to jest ciągle aktualizująca się tabela, w której zapisywany jest przewidywany następny wyraz, jego prawdopodobieństwo oraz długość historii na podstawie, której został przewidziany. Po uaktualnieniu każdej historii jest 
    comparison_data_frame <- data.frame(next_element,next_element_probability)
    # #historia 2
    # if (i > 3){
    #   transition_matrix[compare(transition_matrix[,c(1:2)],example[c((i-3),(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:2)],example[c((i-3),(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    #   temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:2)],example[c((i-2):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    #   probability_matrix <- temp_table/sum(temp_table)
    #   if (sum(temp_table)<1){
    #     probability_matrix[1,] <- 1/length(unique(example))
    #   }
    #   next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    #   next_element_probability <- max(probability_matrix)
    #   if (sum(probability_matrix[1,]==next_element_probability)>1){
    #     next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    #   }
    #   comparison_data_frame <- rbind(comparison_data_frame,data.frame(next_element,next_element_probability))
    # }
    #historia 3
    # if (i > 4){
    #   transition_matrix[compare(transition_matrix[,c(1:3)],example[c((i-4):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:3)],example[c((i-4):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    #   temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:3)],example[c((i-3):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    #   probability_matrix <- temp_table/sum(temp_table)
    #   if (sum(temp_table)<1){
    #     probability_matrix[1,] <- 1/length(unique(example))
    #   }
    #   next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    #   next_element_probability <- max(probability_matrix)
    #   if (sum(probability_matrix[1,]==next_element_probability)>1){
    #     next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    #   }
    #   comparison_data_frame <- rbind(comparison_data_frame,data.frame(next_element,next_element_probability))
    # }
    #historia 4
    # if (i > 5){
    #   transition_matrix[compare(transition_matrix[,c(1:4)],example[c((i-5):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:4)],example[c((i-5):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    #   temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:4)],example[c((i-4):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    #   probability_matrix <- temp_table/sum(temp_table)
    #   if (sum(temp_table)<1){
    #     probability_matrix[1,] <- 1/length(unique(example))
    #   }
    #   next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    #   next_element_probability <- max(probability_matrix)
    #   if (sum(probability_matrix[1,]==next_element_probability)>1){
    #     next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    #   }
    #   comparison_data_frame <- rbind(comparison_data_frame,data.frame(next_element,next_element_probability))
    # }
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
    #zapisanie historii, która zostanie wybrana do przewidzenia  
    which_history[i] <- which.max(comparison_data_frame$next_element_probability)
    #zapisanie kolejnego przewidzianego elementu.
    ciag[i] <- as.numeric(as.character(comparison_data_frame$next_element[which_history[i]]))
  }
  return(list(which_history,ciag,transition_matrix))
}

list_with_heteregonity_only_h15 <- Markov_new(with_heteregonity_new)
list_without_heteregonity_only_h15 <- Markov_new(without_heteregonity_new)

sum(list_with_heteregonity_only_h15[[2]]==with_heteregonity_new)/140
sum(list_without_heteregonity_only_h15[[2]]==without_heteregonity_new)/80

#historia 1,2,3
Markov_new <- function(example){
  #niestety historię w tej funkcji trzeba regulować ręcznie poprzez zmianę liczby poniżej oraz dodanie linijek kodu w pętli for. Na razie ustawiona jest historia 5 ale zmiana tego to jakieś 30 sekund roboty.
  history <- 5
  #ta sama krótka funkcja co w poprzednim algorytmie służąca do porównywania wierszy macierzy z wektorem.
  compare <- function(table,vector){
    (wynik <- t(t(table)==vector))
    (wynik <- rowSums(wynik))
    wynik <- wynik>(length(vector)-1)
    return(wynik)
  }
  #stworzenie macierzy wszystkich stanów dla wszystkich historii. To jest zasadnicza różnica pomiędzy tym algorytmem i tym poprzednim. Ta macierz tworzona jest przez połączenie tak naprawdę dwóch macierzy. Z jednej strony macierzy kombinacji wszystkich stanów dla historii 5 (tylko dlatego, że tak ustawiona jest historia), a z drugiej strony kolumn z liczebnością przewidywanego wyrazu (Tabela 2 pokazuję tę macierz).
  transition_matrix <- permutations(n=length(unique(example)),r=history,v=unique(example),repeats.allowed = T)
  #nazwanie kolumn
  colnames(transition_matrix) <- paste0("h",c(1:history))
  temp_table <- data.frame(t(rep(0,length(unique(example)))))
  colnames(temp_table) <- sort(unique(example))
  #połączenie dwóch tabel
  transition_matrix <- cbind(transition_matrix,temp_table)
  #Zmienna, w której będzie zapisywana historia, za pomocą, której robione są przewidywania. Jej pierwsze dwa elementy to zera ponieważ pierwsze dwa elementy są przewidywane na podstawie zwykłego prawdopodobieństwa.
  which_history <- c(0,0)
  #zmienna, w której zapisywane są elementy ciągu. Pierwszy element jest po prostu losowany z równym prawdopodobieństwem. Dla dwóch elementów jest to 0.5. Drugi element jest po prostu przepisaniem pierwszego wyrazu oryginalnego ciągu bo na podstawie jednego wyrazu tylko takie przewidywanie można zrobić (w tym sensie trochę oszukuję z historią)
  ciag <- sample(unique(example),1,replace=F)
  ciag[2] <- example[1]
  #to jest serce tej funkcji, tego algorytmu. Podzielone jest na odpowiednie historie. Dla każdej historii. Wydaje mi się, że w miarę jasno opisane jest działanie tego algorytmu powyżej.
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
    #to jest ciągle aktualizująca się tabela, w której zapisywany jest przewidywany następny wyraz, jego prawdopodobieństwo oraz długość historii na podstawie, której został przewidziany. Po uaktualnieniu każdej historii jest 
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
    # historia 3
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
    # if (i > 5){
    #   transition_matrix[compare(transition_matrix[,c(1:4)],example[c((i-5):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:4)],example[c((i-5):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    #   temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:4)],example[c((i-4):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    #   probability_matrix <- temp_table/sum(temp_table)
    #   if (sum(temp_table)<1){
    #     probability_matrix[1,] <- 1/length(unique(example))
    #   }
    #   next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    #   next_element_probability <- max(probability_matrix)
    #   if (sum(probability_matrix[1,]==next_element_probability)>1){
    #     next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    #   }
    #   comparison_data_frame <- rbind(comparison_data_frame,data.frame(next_element,next_element_probability))
    # }
    #historia 5
    # if (i > 6){
    #   transition_matrix[compare(transition_matrix[,c(1:5)],example[c((i-6):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:5)],example[c((i-6):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    #   temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:5)],example[c((i-5):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    #   probability_matrix <- temp_table/sum(temp_table)
    #   if (sum(temp_table)<1){
    #     probability_matrix[1,] <- 1/length(unique(example))
    #   }
    #   next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    #   next_element_probability <- max(probability_matrix)
    #   if (sum(probability_matrix[1,]==next_element_probability)>1){
    #     next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    #   }
    #   comparison_data_frame <- rbind(comparison_data_frame,data.frame(next_element,next_element_probability))
    # }
    #zapisanie historii, która zostanie wybrana do przewidzenia  
    which_history[i] <- which.max(comparison_data_frame$next_element_probability)
    #zapisanie kolejnego przewidzianego elementu.
    ciag[i] <- as.numeric(as.character(comparison_data_frame$next_element[which_history[i]]))
  }
  return(list(which_history,ciag,transition_matrix))
}

list_with_heteregonity_only_h123 <- Markov_new(with_heteregonity_new)
list_without_heteregonity_only_h123 <- Markov_new(without_heteregonity_new)

sum(list_with_heteregonity_only_h123[[2]]==with_heteregonity_new)/140
sum(list_without_heteregonity_only_h123[[2]]==without_heteregonity_new)/80

#historia 1,2,4
Markov_new <- function(example){
  #niestety historię w tej funkcji trzeba regulować ręcznie poprzez zmianę liczby poniżej oraz dodanie linijek kodu w pętli for. Na razie ustawiona jest historia 5 ale zmiana tego to jakieś 30 sekund roboty.
  history <- 5
  #ta sama krótka funkcja co w poprzednim algorytmie służąca do porównywania wierszy macierzy z wektorem.
  compare <- function(table,vector){
    (wynik <- t(t(table)==vector))
    (wynik <- rowSums(wynik))
    wynik <- wynik>(length(vector)-1)
    return(wynik)
  }
  #stworzenie macierzy wszystkich stanów dla wszystkich historii. To jest zasadnicza różnica pomiędzy tym algorytmem i tym poprzednim. Ta macierz tworzona jest przez połączenie tak naprawdę dwóch macierzy. Z jednej strony macierzy kombinacji wszystkich stanów dla historii 5 (tylko dlatego, że tak ustawiona jest historia), a z drugiej strony kolumn z liczebnością przewidywanego wyrazu (Tabela 2 pokazuję tę macierz).
  transition_matrix <- permutations(n=length(unique(example)),r=history,v=unique(example),repeats.allowed = T)
  #nazwanie kolumn
  colnames(transition_matrix) <- paste0("h",c(1:history))
  temp_table <- data.frame(t(rep(0,length(unique(example)))))
  colnames(temp_table) <- sort(unique(example))
  #połączenie dwóch tabel
  transition_matrix <- cbind(transition_matrix,temp_table)
  #Zmienna, w której będzie zapisywana historia, za pomocą, której robione są przewidywania. Jej pierwsze dwa elementy to zera ponieważ pierwsze dwa elementy są przewidywane na podstawie zwykłego prawdopodobieństwa.
  which_history <- c(0,0)
  #zmienna, w której zapisywane są elementy ciągu. Pierwszy element jest po prostu losowany z równym prawdopodobieństwem. Dla dwóch elementów jest to 0.5. Drugi element jest po prostu przepisaniem pierwszego wyrazu oryginalnego ciągu bo na podstawie jednego wyrazu tylko takie przewidywanie można zrobić (w tym sensie trochę oszukuję z historią)
  ciag <- sample(unique(example),1,replace=F)
  ciag[2] <- example[1]
  #to jest serce tej funkcji, tego algorytmu. Podzielone jest na odpowiednie historie. Dla każdej historii. Wydaje mi się, że w miarę jasno opisane jest działanie tego algorytmu powyżej.
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
    #to jest ciągle aktualizująca się tabela, w której zapisywany jest przewidywany następny wyraz, jego prawdopodobieństwo oraz długość historii na podstawie, której został przewidziany. Po uaktualnieniu każdej historii jest 
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
    # # historia 3
    # if (i > 4){
    #   transition_matrix[compare(transition_matrix[,c(1:3)],example[c((i-4):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:3)],example[c((i-4):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    #   temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:3)],example[c((i-3):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    #   probability_matrix <- temp_table/sum(temp_table)
    #   if (sum(temp_table)<1){
    #     probability_matrix[1,] <- 1/length(unique(example))
    #   }
    #   next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    #   next_element_probability <- max(probability_matrix)
    #   if (sum(probability_matrix[1,]==next_element_probability)>1){
    #     next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    #   }
    #   comparison_data_frame <- rbind(comparison_data_frame,data.frame(next_element,next_element_probability))
    # }
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
    # if (i > 6){
    #   transition_matrix[compare(transition_matrix[,c(1:5)],example[c((i-6):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:5)],example[c((i-6):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    #   temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:5)],example[c((i-5):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    #   probability_matrix <- temp_table/sum(temp_table)
    #   if (sum(temp_table)<1){
    #     probability_matrix[1,] <- 1/length(unique(example))
    #   }
    #   next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    #   next_element_probability <- max(probability_matrix)
    #   if (sum(probability_matrix[1,]==next_element_probability)>1){
    #     next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    #   }
    #   comparison_data_frame <- rbind(comparison_data_frame,data.frame(next_element,next_element_probability))
    # }
    #zapisanie historii, która zostanie wybrana do przewidzenia  
    which_history[i] <- which.max(comparison_data_frame$next_element_probability)
    #zapisanie kolejnego przewidzianego elementu.
    ciag[i] <- as.numeric(as.character(comparison_data_frame$next_element[which_history[i]]))
  }
  return(list(which_history,ciag,transition_matrix))
}

list_with_heteregonity_only_h124 <- Markov_new(with_heteregonity_new)
list_without_heteregonity_only_h124 <- Markov_new(without_heteregonity_new)

sum(list_with_heteregonity_only_h124[[2]]==with_heteregonity_new)/140
sum(list_without_heteregonity_only_h124[[2]]==without_heteregonity_new)/80


#historia 1,2,5
Markov_new <- function(example){
  #niestety historię w tej funkcji trzeba regulować ręcznie poprzez zmianę liczby poniżej oraz dodanie linijek kodu w pętli for. Na razie ustawiona jest historia 5 ale zmiana tego to jakieś 30 sekund roboty.
  history <- 5
  #ta sama krótka funkcja co w poprzednim algorytmie służąca do porównywania wierszy macierzy z wektorem.
  compare <- function(table,vector){
    (wynik <- t(t(table)==vector))
    (wynik <- rowSums(wynik))
    wynik <- wynik>(length(vector)-1)
    return(wynik)
  }
  #stworzenie macierzy wszystkich stanów dla wszystkich historii. To jest zasadnicza różnica pomiędzy tym algorytmem i tym poprzednim. Ta macierz tworzona jest przez połączenie tak naprawdę dwóch macierzy. Z jednej strony macierzy kombinacji wszystkich stanów dla historii 5 (tylko dlatego, że tak ustawiona jest historia), a z drugiej strony kolumn z liczebnością przewidywanego wyrazu (Tabela 2 pokazuję tę macierz).
  transition_matrix <- permutations(n=length(unique(example)),r=history,v=unique(example),repeats.allowed = T)
  #nazwanie kolumn
  colnames(transition_matrix) <- paste0("h",c(1:history))
  temp_table <- data.frame(t(rep(0,length(unique(example)))))
  colnames(temp_table) <- sort(unique(example))
  #połączenie dwóch tabel
  transition_matrix <- cbind(transition_matrix,temp_table)
  #Zmienna, w której będzie zapisywana historia, za pomocą, której robione są przewidywania. Jej pierwsze dwa elementy to zera ponieważ pierwsze dwa elementy są przewidywane na podstawie zwykłego prawdopodobieństwa.
  which_history <- c(0,0)
  #zmienna, w której zapisywane są elementy ciągu. Pierwszy element jest po prostu losowany z równym prawdopodobieństwem. Dla dwóch elementów jest to 0.5. Drugi element jest po prostu przepisaniem pierwszego wyrazu oryginalnego ciągu bo na podstawie jednego wyrazu tylko takie przewidywanie można zrobić (w tym sensie trochę oszukuję z historią)
  ciag <- sample(unique(example),1,replace=F)
  ciag[2] <- example[1]
  #to jest serce tej funkcji, tego algorytmu. Podzielone jest na odpowiednie historie. Dla każdej historii. Wydaje mi się, że w miarę jasno opisane jest działanie tego algorytmu powyżej.
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
    #to jest ciągle aktualizująca się tabela, w której zapisywany jest przewidywany następny wyraz, jego prawdopodobieństwo oraz długość historii na podstawie, której został przewidziany. Po uaktualnieniu każdej historii jest 
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
    # # historia 3
    # if (i > 4){
    #   transition_matrix[compare(transition_matrix[,c(1:3)],example[c((i-4):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:3)],example[c((i-4):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    #   temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:3)],example[c((i-3):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    #   probability_matrix <- temp_table/sum(temp_table)
    #   if (sum(temp_table)<1){
    #     probability_matrix[1,] <- 1/length(unique(example))
    #   }
    #   next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    #   next_element_probability <- max(probability_matrix)
    #   if (sum(probability_matrix[1,]==next_element_probability)>1){
    #     next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    #   }
    #   comparison_data_frame <- rbind(comparison_data_frame,data.frame(next_element,next_element_probability))
    # }
    # #historia 4
    # if (i > 5){
    #   transition_matrix[compare(transition_matrix[,c(1:4)],example[c((i-5):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:4)],example[c((i-5):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    #   temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:4)],example[c((i-4):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    #   probability_matrix <- temp_table/sum(temp_table)
    #   if (sum(temp_table)<1){
    #     probability_matrix[1,] <- 1/length(unique(example))
    #   }
    #   next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    #   next_element_probability <- max(probability_matrix)
    #   if (sum(probability_matrix[1,]==next_element_probability)>1){
    #     next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    #   }
    #   comparison_data_frame <- rbind(comparison_data_frame,data.frame(next_element,next_element_probability))
    # }
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
    #zapisanie historii, która zostanie wybrana do przewidzenia  
    which_history[i] <- which.max(comparison_data_frame$next_element_probability)
    #zapisanie kolejnego przewidzianego elementu.
    ciag[i] <- as.numeric(as.character(comparison_data_frame$next_element[which_history[i]]))
  }
  return(list(which_history,ciag,transition_matrix))
}

list_with_heteregonity_only_h125 <- Markov_new(with_heteregonity_new)
list_without_heteregonity_only_h125 <- Markov_new(without_heteregonity_new)

sum(list_with_heteregonity_only_h125[[2]]==with_heteregonity_new)/140
sum(list_without_heteregonity_only_h125[[2]]==without_heteregonity_new)/80

#historia 1,3,4
Markov_new <- function(example){
  #niestety historię w tej funkcji trzeba regulować ręcznie poprzez zmianę liczby poniżej oraz dodanie linijek kodu w pętli for. Na razie ustawiona jest historia 5 ale zmiana tego to jakieś 30 sekund roboty.
  history <- 5
  #ta sama krótka funkcja co w poprzednim algorytmie służąca do porównywania wierszy macierzy z wektorem.
  compare <- function(table,vector){
    (wynik <- t(t(table)==vector))
    (wynik <- rowSums(wynik))
    wynik <- wynik>(length(vector)-1)
    return(wynik)
  }
  #stworzenie macierzy wszystkich stanów dla wszystkich historii. To jest zasadnicza różnica pomiędzy tym algorytmem i tym poprzednim. Ta macierz tworzona jest przez połączenie tak naprawdę dwóch macierzy. Z jednej strony macierzy kombinacji wszystkich stanów dla historii 5 (tylko dlatego, że tak ustawiona jest historia), a z drugiej strony kolumn z liczebnością przewidywanego wyrazu (Tabela 2 pokazuję tę macierz).
  transition_matrix <- permutations(n=length(unique(example)),r=history,v=unique(example),repeats.allowed = T)
  #nazwanie kolumn
  colnames(transition_matrix) <- paste0("h",c(1:history))
  temp_table <- data.frame(t(rep(0,length(unique(example)))))
  colnames(temp_table) <- sort(unique(example))
  #połączenie dwóch tabel
  transition_matrix <- cbind(transition_matrix,temp_table)
  #Zmienna, w której będzie zapisywana historia, za pomocą, której robione są przewidywania. Jej pierwsze dwa elementy to zera ponieważ pierwsze dwa elementy są przewidywane na podstawie zwykłego prawdopodobieństwa.
  which_history <- c(0,0)
  #zmienna, w której zapisywane są elementy ciągu. Pierwszy element jest po prostu losowany z równym prawdopodobieństwem. Dla dwóch elementów jest to 0.5. Drugi element jest po prostu przepisaniem pierwszego wyrazu oryginalnego ciągu bo na podstawie jednego wyrazu tylko takie przewidywanie można zrobić (w tym sensie trochę oszukuję z historią)
  ciag <- sample(unique(example),1,replace=F)
  ciag[2] <- example[1]
  #to jest serce tej funkcji, tego algorytmu. Podzielone jest na odpowiednie historie. Dla każdej historii. Wydaje mi się, że w miarę jasno opisane jest działanie tego algorytmu powyżej.
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
    #to jest ciągle aktualizująca się tabela, w której zapisywany jest przewidywany następny wyraz, jego prawdopodobieństwo oraz długość historii na podstawie, której został przewidziany. Po uaktualnieniu każdej historii jest 
    comparison_data_frame <- data.frame(next_element,next_element_probability)
    # #historia 2
    # if (i > 3){
    #   transition_matrix[compare(transition_matrix[,c(1:2)],example[c((i-3),(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:2)],example[c((i-3),(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    #   temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:2)],example[c((i-2):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    #   probability_matrix <- temp_table/sum(temp_table)
    #   if (sum(temp_table)<1){
    #     probability_matrix[1,] <- 1/length(unique(example))
    #   }
    #   next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    #   next_element_probability <- max(probability_matrix)
    #   if (sum(probability_matrix[1,]==next_element_probability)>1){
    #     next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    #   }
    #   comparison_data_frame <- rbind(comparison_data_frame,data.frame(next_element,next_element_probability))
    # }
    # historia 3
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
    # #historia 5
    # if (i > 6){
    #   transition_matrix[compare(transition_matrix[,c(1:5)],example[c((i-6):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:5)],example[c((i-6):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    #   temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:5)],example[c((i-5):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    #   probability_matrix <- temp_table/sum(temp_table)
    #   if (sum(temp_table)<1){
    #     probability_matrix[1,] <- 1/length(unique(example))
    #   }
    #   next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    #   next_element_probability <- max(probability_matrix)
    #   if (sum(probability_matrix[1,]==next_element_probability)>1){
    #     next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    #   }
    #   comparison_data_frame <- rbind(comparison_data_frame,data.frame(next_element,next_element_probability))
    # }
    #zapisanie historii, która zostanie wybrana do przewidzenia  
    which_history[i] <- which.max(comparison_data_frame$next_element_probability)
    #zapisanie kolejnego przewidzianego elementu.
    ciag[i] <- as.numeric(as.character(comparison_data_frame$next_element[which_history[i]]))
  }
  return(list(which_history,ciag,transition_matrix))
}

list_with_heteregonity_only_h134 <- Markov_new(with_heteregonity_new)
list_without_heteregonity_only_h134 <- Markov_new(without_heteregonity_new)

sum(list_with_heteregonity_only_h134[[2]]==with_heteregonity_new)/140
sum(list_without_heteregonity_only_h134[[2]]==without_heteregonity_new)/80

#historia 1,3,5
Markov_new <- function(example){
  #niestety historię w tej funkcji trzeba regulować ręcznie poprzez zmianę liczby poniżej oraz dodanie linijek kodu w pętli for. Na razie ustawiona jest historia 5 ale zmiana tego to jakieś 30 sekund roboty.
  history <- 5
  #ta sama krótka funkcja co w poprzednim algorytmie służąca do porównywania wierszy macierzy z wektorem.
  compare <- function(table,vector){
    (wynik <- t(t(table)==vector))
    (wynik <- rowSums(wynik))
    wynik <- wynik>(length(vector)-1)
    return(wynik)
  }
  #stworzenie macierzy wszystkich stanów dla wszystkich historii. To jest zasadnicza różnica pomiędzy tym algorytmem i tym poprzednim. Ta macierz tworzona jest przez połączenie tak naprawdę dwóch macierzy. Z jednej strony macierzy kombinacji wszystkich stanów dla historii 5 (tylko dlatego, że tak ustawiona jest historia), a z drugiej strony kolumn z liczebnością przewidywanego wyrazu (Tabela 2 pokazuję tę macierz).
  transition_matrix <- permutations(n=length(unique(example)),r=history,v=unique(example),repeats.allowed = T)
  #nazwanie kolumn
  colnames(transition_matrix) <- paste0("h",c(1:history))
  temp_table <- data.frame(t(rep(0,length(unique(example)))))
  colnames(temp_table) <- sort(unique(example))
  #połączenie dwóch tabel
  transition_matrix <- cbind(transition_matrix,temp_table)
  #Zmienna, w której będzie zapisywana historia, za pomocą, której robione są przewidywania. Jej pierwsze dwa elementy to zera ponieważ pierwsze dwa elementy są przewidywane na podstawie zwykłego prawdopodobieństwa.
  which_history <- c(0,0)
  #zmienna, w której zapisywane są elementy ciągu. Pierwszy element jest po prostu losowany z równym prawdopodobieństwem. Dla dwóch elementów jest to 0.5. Drugi element jest po prostu przepisaniem pierwszego wyrazu oryginalnego ciągu bo na podstawie jednego wyrazu tylko takie przewidywanie można zrobić (w tym sensie trochę oszukuję z historią)
  ciag <- sample(unique(example),1,replace=F)
  ciag[2] <- example[1]
  #to jest serce tej funkcji, tego algorytmu. Podzielone jest na odpowiednie historie. Dla każdej historii. Wydaje mi się, że w miarę jasno opisane jest działanie tego algorytmu powyżej.
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
    #to jest ciągle aktualizująca się tabela, w której zapisywany jest przewidywany następny wyraz, jego prawdopodobieństwo oraz długość historii na podstawie, której został przewidziany. Po uaktualnieniu każdej historii jest 
    comparison_data_frame <- data.frame(next_element,next_element_probability)
    # #historia 2
    # if (i > 3){
    #   transition_matrix[compare(transition_matrix[,c(1:2)],example[c((i-3),(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:2)],example[c((i-3),(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    #   temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:2)],example[c((i-2):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    #   probability_matrix <- temp_table/sum(temp_table)
    #   if (sum(temp_table)<1){
    #     probability_matrix[1,] <- 1/length(unique(example))
    #   }
    #   next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    #   next_element_probability <- max(probability_matrix)
    #   if (sum(probability_matrix[1,]==next_element_probability)>1){
    #     next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    #   }
    #   comparison_data_frame <- rbind(comparison_data_frame,data.frame(next_element,next_element_probability))
    # }
    # historia 3
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
    # #historia 4
    # if (i > 5){
    #   transition_matrix[compare(transition_matrix[,c(1:4)],example[c((i-5):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:4)],example[c((i-5):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    #   temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:4)],example[c((i-4):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    #   probability_matrix <- temp_table/sum(temp_table)
    #   if (sum(temp_table)<1){
    #     probability_matrix[1,] <- 1/length(unique(example))
    #   }
    #   next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    #   next_element_probability <- max(probability_matrix)
    #   if (sum(probability_matrix[1,]==next_element_probability)>1){
    #     next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    #   }
    #   comparison_data_frame <- rbind(comparison_data_frame,data.frame(next_element,next_element_probability))
    # }
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
    #zapisanie historii, która zostanie wybrana do przewidzenia  
    which_history[i] <- which.max(comparison_data_frame$next_element_probability)
    #zapisanie kolejnego przewidzianego elementu.
    ciag[i] <- as.numeric(as.character(comparison_data_frame$next_element[which_history[i]]))
  }
  return(list(which_history,ciag,transition_matrix))
}

list_with_heteregonity_only_h135 <- Markov_new(with_heteregonity_new)
list_without_heteregonity_only_h135 <- Markov_new(without_heteregonity_new)

sum(list_with_heteregonity_only_h135[[2]]==with_heteregonity_new)/140
sum(list_without_heteregonity_only_h135[[2]]==without_heteregonity_new)/80

#historia 1,4,5
Markov_new <- function(example){
  #niestety historię w tej funkcji trzeba regulować ręcznie poprzez zmianę liczby poniżej oraz dodanie linijek kodu w pętli for. Na razie ustawiona jest historia 5 ale zmiana tego to jakieś 30 sekund roboty.
  history <- 5
  #ta sama krótka funkcja co w poprzednim algorytmie służąca do porównywania wierszy macierzy z wektorem.
  compare <- function(table,vector){
    (wynik <- t(t(table)==vector))
    (wynik <- rowSums(wynik))
    wynik <- wynik>(length(vector)-1)
    return(wynik)
  }
  #stworzenie macierzy wszystkich stanów dla wszystkich historii. To jest zasadnicza różnica pomiędzy tym algorytmem i tym poprzednim. Ta macierz tworzona jest przez połączenie tak naprawdę dwóch macierzy. Z jednej strony macierzy kombinacji wszystkich stanów dla historii 5 (tylko dlatego, że tak ustawiona jest historia), a z drugiej strony kolumn z liczebnością przewidywanego wyrazu (Tabela 2 pokazuję tę macierz).
  transition_matrix <- permutations(n=length(unique(example)),r=history,v=unique(example),repeats.allowed = T)
  #nazwanie kolumn
  colnames(transition_matrix) <- paste0("h",c(1:history))
  temp_table <- data.frame(t(rep(0,length(unique(example)))))
  colnames(temp_table) <- sort(unique(example))
  #połączenie dwóch tabel
  transition_matrix <- cbind(transition_matrix,temp_table)
  #Zmienna, w której będzie zapisywana historia, za pomocą, której robione są przewidywania. Jej pierwsze dwa elementy to zera ponieważ pierwsze dwa elementy są przewidywane na podstawie zwykłego prawdopodobieństwa.
  which_history <- c(0,0)
  #zmienna, w której zapisywane są elementy ciągu. Pierwszy element jest po prostu losowany z równym prawdopodobieństwem. Dla dwóch elementów jest to 0.5. Drugi element jest po prostu przepisaniem pierwszego wyrazu oryginalnego ciągu bo na podstawie jednego wyrazu tylko takie przewidywanie można zrobić (w tym sensie trochę oszukuję z historią)
  ciag <- sample(unique(example),1,replace=F)
  ciag[2] <- example[1]
  #to jest serce tej funkcji, tego algorytmu. Podzielone jest na odpowiednie historie. Dla każdej historii. Wydaje mi się, że w miarę jasno opisane jest działanie tego algorytmu powyżej.
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
    #to jest ciągle aktualizująca się tabela, w której zapisywany jest przewidywany następny wyraz, jego prawdopodobieństwo oraz długość historii na podstawie, której został przewidziany. Po uaktualnieniu każdej historii jest 
    comparison_data_frame <- data.frame(next_element,next_element_probability)
    # #historia 2
    # if (i > 3){
    #   transition_matrix[compare(transition_matrix[,c(1:2)],example[c((i-3),(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:2)],example[c((i-3),(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    #   temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:2)],example[c((i-2):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    #   probability_matrix <- temp_table/sum(temp_table)
    #   if (sum(temp_table)<1){
    #     probability_matrix[1,] <- 1/length(unique(example))
    #   }
    #   next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    #   next_element_probability <- max(probability_matrix)
    #   if (sum(probability_matrix[1,]==next_element_probability)>1){
    #     next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    #   }
    #   comparison_data_frame <- rbind(comparison_data_frame,data.frame(next_element,next_element_probability))
    # }
    # # historia 3
    # if (i > 4){
    #   transition_matrix[compare(transition_matrix[,c(1:3)],example[c((i-4):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:3)],example[c((i-4):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    #   temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:3)],example[c((i-3):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    #   probability_matrix <- temp_table/sum(temp_table)
    #   if (sum(temp_table)<1){
    #     probability_matrix[1,] <- 1/length(unique(example))
    #   }
    #   next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    #   next_element_probability <- max(probability_matrix)
    #   if (sum(probability_matrix[1,]==next_element_probability)>1){
    #     next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    #   }
    #   comparison_data_frame <- rbind(comparison_data_frame,data.frame(next_element,next_element_probability))
    # }
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
    #zapisanie historii, która zostanie wybrana do przewidzenia  
    which_history[i] <- which.max(comparison_data_frame$next_element_probability)
    #zapisanie kolejnego przewidzianego elementu.
    ciag[i] <- as.numeric(as.character(comparison_data_frame$next_element[which_history[i]]))
  }
  return(list(which_history,ciag,transition_matrix))
}

list_with_heteregonity_only_h145 <- Markov_new(with_heteregonity_new)
list_without_heteregonity_only_h145 <- Markov_new(without_heteregonity_new)

sum(list_with_heteregonity_only_h145[[2]]==with_heteregonity_new)/140
sum(list_without_heteregonity_only_h145[[2]]==without_heteregonity_new)/80

#historia 1,2,3,4
Markov_new <- function(example){
  #niestety historię w tej funkcji trzeba regulować ręcznie poprzez zmianę liczby poniżej oraz dodanie linijek kodu w pętli for. Na razie ustawiona jest historia 5 ale zmiana tego to jakieś 30 sekund roboty.
  history <- 5
  #ta sama krótka funkcja co w poprzednim algorytmie służąca do porównywania wierszy macierzy z wektorem.
  compare <- function(table,vector){
    (wynik <- t(t(table)==vector))
    (wynik <- rowSums(wynik))
    wynik <- wynik>(length(vector)-1)
    return(wynik)
  }
  #stworzenie macierzy wszystkich stanów dla wszystkich historii. To jest zasadnicza różnica pomiędzy tym algorytmem i tym poprzednim. Ta macierz tworzona jest przez połączenie tak naprawdę dwóch macierzy. Z jednej strony macierzy kombinacji wszystkich stanów dla historii 5 (tylko dlatego, że tak ustawiona jest historia), a z drugiej strony kolumn z liczebnością przewidywanego wyrazu (Tabela 2 pokazuję tę macierz).
  transition_matrix <- permutations(n=length(unique(example)),r=history,v=unique(example),repeats.allowed = T)
  #nazwanie kolumn
  colnames(transition_matrix) <- paste0("h",c(1:history))
  temp_table <- data.frame(t(rep(0,length(unique(example)))))
  colnames(temp_table) <- sort(unique(example))
  #połączenie dwóch tabel
  transition_matrix <- cbind(transition_matrix,temp_table)
  #Zmienna, w której będzie zapisywana historia, za pomocą, której robione są przewidywania. Jej pierwsze dwa elementy to zera ponieważ pierwsze dwa elementy są przewidywane na podstawie zwykłego prawdopodobieństwa.
  which_history <- c(0,0)
  #zmienna, w której zapisywane są elementy ciągu. Pierwszy element jest po prostu losowany z równym prawdopodobieństwem. Dla dwóch elementów jest to 0.5. Drugi element jest po prostu przepisaniem pierwszego wyrazu oryginalnego ciągu bo na podstawie jednego wyrazu tylko takie przewidywanie można zrobić (w tym sensie trochę oszukuję z historią)
  ciag <- sample(unique(example),1,replace=F)
  ciag[2] <- example[1]
  #to jest serce tej funkcji, tego algorytmu. Podzielone jest na odpowiednie historie. Dla każdej historii. Wydaje mi się, że w miarę jasno opisane jest działanie tego algorytmu powyżej.
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
    #to jest ciągle aktualizująca się tabela, w której zapisywany jest przewidywany następny wyraz, jego prawdopodobieństwo oraz długość historii na podstawie, której został przewidziany. Po uaktualnieniu każdej historii jest 
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
    # historia 3
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
    # #historia 5
    # if (i > 6){
    #   transition_matrix[compare(transition_matrix[,c(1:5)],example[c((i-6):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:5)],example[c((i-6):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    #   temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:5)],example[c((i-5):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    #   probability_matrix <- temp_table/sum(temp_table)
    #   if (sum(temp_table)<1){
    #     probability_matrix[1,] <- 1/length(unique(example))
    #   }
    #   next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    #   next_element_probability <- max(probability_matrix)
    #   if (sum(probability_matrix[1,]==next_element_probability)>1){
    #     next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    #   }
    #   comparison_data_frame <- rbind(comparison_data_frame,data.frame(next_element,next_element_probability))
    # }
    #zapisanie historii, która zostanie wybrana do przewidzenia  
    which_history[i] <- which.max(comparison_data_frame$next_element_probability)
    #zapisanie kolejnego przewidzianego elementu.
    ciag[i] <- as.numeric(as.character(comparison_data_frame$next_element[which_history[i]]))
  }
  return(list(which_history,ciag,transition_matrix))
}

list_with_heteregonity_only_h11234 <- Markov_new(with_heteregonity_new)

list_without_heteregonity_only_h1234 <- Markov_new(without_heteregonity_new)

sum(list_with_heteregonity_only_h11234[[2]]==with_heteregonity_new)/140
sum(list_without_heteregonity_only_h1234[[2]]==without_heteregonity_new)/80

#historia 1,3,4,5
Markov_new <- function(example){
  #niestety historię w tej funkcji trzeba regulować ręcznie poprzez zmianę liczby poniżej oraz dodanie linijek kodu w pętli for. Na razie ustawiona jest historia 5 ale zmiana tego to jakieś 30 sekund roboty.
  history <- 5
  #ta sama krótka funkcja co w poprzednim algorytmie służąca do porównywania wierszy macierzy z wektorem.
  compare <- function(table,vector){
    (wynik <- t(t(table)==vector))
    (wynik <- rowSums(wynik))
    wynik <- wynik>(length(vector)-1)
    return(wynik)
  }
  #stworzenie macierzy wszystkich stanów dla wszystkich historii. To jest zasadnicza różnica pomiędzy tym algorytmem i tym poprzednim. Ta macierz tworzona jest przez połączenie tak naprawdę dwóch macierzy. Z jednej strony macierzy kombinacji wszystkich stanów dla historii 5 (tylko dlatego, że tak ustawiona jest historia), a z drugiej strony kolumn z liczebnością przewidywanego wyrazu (Tabela 2 pokazuję tę macierz).
  transition_matrix <- permutations(n=length(unique(example)),r=history,v=unique(example),repeats.allowed = T)
  #nazwanie kolumn
  colnames(transition_matrix) <- paste0("h",c(1:history))
  temp_table <- data.frame(t(rep(0,length(unique(example)))))
  colnames(temp_table) <- sort(unique(example))
  #połączenie dwóch tabel
  transition_matrix <- cbind(transition_matrix,temp_table)
  #Zmienna, w której będzie zapisywana historia, za pomocą, której robione są przewidywania. Jej pierwsze dwa elementy to zera ponieważ pierwsze dwa elementy są przewidywane na podstawie zwykłego prawdopodobieństwa.
  which_history <- c(0,0)
  #zmienna, w której zapisywane są elementy ciągu. Pierwszy element jest po prostu losowany z równym prawdopodobieństwem. Dla dwóch elementów jest to 0.5. Drugi element jest po prostu przepisaniem pierwszego wyrazu oryginalnego ciągu bo na podstawie jednego wyrazu tylko takie przewidywanie można zrobić (w tym sensie trochę oszukuję z historią)
  ciag <- sample(unique(example),1,replace=F)
  ciag[2] <- example[1]
  #to jest serce tej funkcji, tego algorytmu. Podzielone jest na odpowiednie historie. Dla każdej historii. Wydaje mi się, że w miarę jasno opisane jest działanie tego algorytmu powyżej.
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
    #to jest ciągle aktualizująca się tabela, w której zapisywany jest przewidywany następny wyraz, jego prawdopodobieństwo oraz długość historii na podstawie, której został przewidziany. Po uaktualnieniu każdej historii jest 
    comparison_data_frame <- data.frame(next_element,next_element_probability)
    # #historia 2
    # if (i > 3){
    #   transition_matrix[compare(transition_matrix[,c(1:2)],example[c((i-3),(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:2)],example[c((i-3),(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    #   temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:2)],example[c((i-2):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    #   probability_matrix <- temp_table/sum(temp_table)
    #   if (sum(temp_table)<1){
    #     probability_matrix[1,] <- 1/length(unique(example))
    #   }
    #   next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    #   next_element_probability <- max(probability_matrix)
    #   if (sum(probability_matrix[1,]==next_element_probability)>1){
    #     next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    #   }
    #   comparison_data_frame <- rbind(comparison_data_frame,data.frame(next_element,next_element_probability))
    # }
    # historia 3
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
    #zapisanie historii, która zostanie wybrana do przewidzenia  
    which_history[i] <- which.max(comparison_data_frame$next_element_probability)
    #zapisanie kolejnego przewidzianego elementu.
    ciag[i] <- as.numeric(as.character(comparison_data_frame$next_element[which_history[i]]))
  }
  return(list(which_history,ciag,transition_matrix))
}

list_with_heteregonity_only_h1345 <- Markov_new(with_heteregonity_new)
list_without_heteregonity_only_h1345 <- Markov_new(without_heteregonity_new)

sum(list_with_heteregonity_only_h1345[[2]]==with_heteregonity_new)/140
sum(list_without_heteregonity_only_h1345[[2]]==without_heteregonity_new)/80

#historia 1,2,3,5
Markov_new <- function(example){
  #niestety historię w tej funkcji trzeba regulować ręcznie poprzez zmianę liczby poniżej oraz dodanie linijek kodu w pętli for. Na razie ustawiona jest historia 5 ale zmiana tego to jakieś 30 sekund roboty.
  history <- 5
  #ta sama krótka funkcja co w poprzednim algorytmie służąca do porównywania wierszy macierzy z wektorem.
  compare <- function(table,vector){
    (wynik <- t(t(table)==vector))
    (wynik <- rowSums(wynik))
    wynik <- wynik>(length(vector)-1)
    return(wynik)
  }
  #stworzenie macierzy wszystkich stanów dla wszystkich historii. To jest zasadnicza różnica pomiędzy tym algorytmem i tym poprzednim. Ta macierz tworzona jest przez połączenie tak naprawdę dwóch macierzy. Z jednej strony macierzy kombinacji wszystkich stanów dla historii 5 (tylko dlatego, że tak ustawiona jest historia), a z drugiej strony kolumn z liczebnością przewidywanego wyrazu (Tabela 2 pokazuję tę macierz).
  transition_matrix <- permutations(n=length(unique(example)),r=history,v=unique(example),repeats.allowed = T)
  #nazwanie kolumn
  colnames(transition_matrix) <- paste0("h",c(1:history))
  temp_table <- data.frame(t(rep(0,length(unique(example)))))
  colnames(temp_table) <- sort(unique(example))
  #połączenie dwóch tabel
  transition_matrix <- cbind(transition_matrix,temp_table)
  #Zmienna, w której będzie zapisywana historia, za pomocą, której robione są przewidywania. Jej pierwsze dwa elementy to zera ponieważ pierwsze dwa elementy są przewidywane na podstawie zwykłego prawdopodobieństwa.
  which_history <- c(0,0)
  #zmienna, w której zapisywane są elementy ciągu. Pierwszy element jest po prostu losowany z równym prawdopodobieństwem. Dla dwóch elementów jest to 0.5. Drugi element jest po prostu przepisaniem pierwszego wyrazu oryginalnego ciągu bo na podstawie jednego wyrazu tylko takie przewidywanie można zrobić (w tym sensie trochę oszukuję z historią)
  ciag <- sample(unique(example),1,replace=F)
  ciag[2] <- example[1]
  #to jest serce tej funkcji, tego algorytmu. Podzielone jest na odpowiednie historie. Dla każdej historii. Wydaje mi się, że w miarę jasno opisane jest działanie tego algorytmu powyżej.
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
    #to jest ciągle aktualizująca się tabela, w której zapisywany jest przewidywany następny wyraz, jego prawdopodobieństwo oraz długość historii na podstawie, której został przewidziany. Po uaktualnieniu każdej historii jest 
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
    # historia 3
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
    # #historia 4
    # if (i > 5){
    #   transition_matrix[compare(transition_matrix[,c(1:4)],example[c((i-5):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:4)],example[c((i-5):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    #   temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:4)],example[c((i-4):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    #   probability_matrix <- temp_table/sum(temp_table)
    #   if (sum(temp_table)<1){
    #     probability_matrix[1,] <- 1/length(unique(example))
    #   }
    #   next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    #   next_element_probability <- max(probability_matrix)
    #   if (sum(probability_matrix[1,]==next_element_probability)>1){
    #     next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    #   }
    #   comparison_data_frame <- rbind(comparison_data_frame,data.frame(next_element,next_element_probability))
    # }
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
    #zapisanie historii, która zostanie wybrana do przewidzenia  
    which_history[i] <- which.max(comparison_data_frame$next_element_probability)
    #zapisanie kolejnego przewidzianego elementu.
    ciag[i] <- as.numeric(as.character(comparison_data_frame$next_element[which_history[i]]))
  }
  return(list(which_history,ciag,transition_matrix))
}

list_with_heteregonity_only_h1235 <- Markov_new(with_heteregonity_new)
list_without_heteregonity_only_h1235 <- Markov_new(without_heteregonity_new)

sum(list_with_heteregonity_only_h1235[[2]]==with_heteregonity_new)/140
sum(list_without_heteregonity_only_h1235[[2]]==without_heteregonity_new)/80

#historia 1,2,4,5
Markov_new <- function(example){
  #niestety historię w tej funkcji trzeba regulować ręcznie poprzez zmianę liczby poniżej oraz dodanie linijek kodu w pętli for. Na razie ustawiona jest historia 5 ale zmiana tego to jakieś 30 sekund roboty.
  history <- 5
  #ta sama krótka funkcja co w poprzednim algorytmie służąca do porównywania wierszy macierzy z wektorem.
  compare <- function(table,vector){
    (wynik <- t(t(table)==vector))
    (wynik <- rowSums(wynik))
    wynik <- wynik>(length(vector)-1)
    return(wynik)
  }
  #stworzenie macierzy wszystkich stanów dla wszystkich historii. To jest zasadnicza różnica pomiędzy tym algorytmem i tym poprzednim. Ta macierz tworzona jest przez połączenie tak naprawdę dwóch macierzy. Z jednej strony macierzy kombinacji wszystkich stanów dla historii 5 (tylko dlatego, że tak ustawiona jest historia), a z drugiej strony kolumn z liczebnością przewidywanego wyrazu (Tabela 2 pokazuję tę macierz).
  transition_matrix <- permutations(n=length(unique(example)),r=history,v=unique(example),repeats.allowed = T)
  #nazwanie kolumn
  colnames(transition_matrix) <- paste0("h",c(1:history))
  temp_table <- data.frame(t(rep(0,length(unique(example)))))
  colnames(temp_table) <- sort(unique(example))
  #połączenie dwóch tabel
  transition_matrix <- cbind(transition_matrix,temp_table)
  #Zmienna, w której będzie zapisywana historia, za pomocą, której robione są przewidywania. Jej pierwsze dwa elementy to zera ponieważ pierwsze dwa elementy są przewidywane na podstawie zwykłego prawdopodobieństwa.
  which_history <- c(0,0)
  #zmienna, w której zapisywane są elementy ciągu. Pierwszy element jest po prostu losowany z równym prawdopodobieństwem. Dla dwóch elementów jest to 0.5. Drugi element jest po prostu przepisaniem pierwszego wyrazu oryginalnego ciągu bo na podstawie jednego wyrazu tylko takie przewidywanie można zrobić (w tym sensie trochę oszukuję z historią)
  ciag <- sample(unique(example),1,replace=F)
  ciag[2] <- example[1]
  #to jest serce tej funkcji, tego algorytmu. Podzielone jest na odpowiednie historie. Dla każdej historii. Wydaje mi się, że w miarę jasno opisane jest działanie tego algorytmu powyżej.
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
    #to jest ciągle aktualizująca się tabela, w której zapisywany jest przewidywany następny wyraz, jego prawdopodobieństwo oraz długość historii na podstawie, której został przewidziany. Po uaktualnieniu każdej historii jest 
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
    # # historia 3
    # if (i > 4){
    #   transition_matrix[compare(transition_matrix[,c(1:3)],example[c((i-4):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]] <- transition_matrix[compare(transition_matrix[,c(1:3)],example[c((i-4):(i-2))]),colnames(transition_matrix)[(colnames(transition_matrix)==example[i-1])]]+1
    #   temp_table <- (t(apply(transition_matrix[compare(transition_matrix[,c(1:3)],example[c((i-3):(i-1))]),c((history+1):(dim(transition_matrix)[2]))],2,sum)))
    #   probability_matrix <- temp_table/sum(temp_table)
    #   if (sum(temp_table)<1){
    #     probability_matrix[1,] <- 1/length(unique(example))
    #   }
    #   next_element <- colnames(probability_matrix)[which.max(probability_matrix)]
    #   next_element_probability <- max(probability_matrix)
    #   if (sum(probability_matrix[1,]==next_element_probability)>1){
    #     next_element <- sample(colnames(probability_matrix)[probability_matrix[1,]==next_element_probability],1,replace = F)
    #   }
    #   comparison_data_frame <- rbind(comparison_data_frame,data.frame(next_element,next_element_probability))
    # }
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
    #zapisanie historii, która zostanie wybrana do przewidzenia  
    which_history[i] <- which.max(comparison_data_frame$next_element_probability)
    #zapisanie kolejnego przewidzianego elementu.
    ciag[i] <- as.numeric(as.character(comparison_data_frame$next_element[which_history[i]]))
  }
  return(list(which_history,ciag,transition_matrix))
}

list_with_heteregonity_only_h1245 <- Markov_new(with_heteregonity_new)
list_without_heteregonity_only_h1245 <- Markov_new(without_heteregonity_new)

sum(list_with_heteregonity_only_h1245[[2]]==with_heteregonity_new)/140
sum(list_without_heteregonity_only_h1245[[2]]==without_heteregonity_new)/80



