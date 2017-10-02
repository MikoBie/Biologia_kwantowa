library(plyr)
library(ggplot2)
library(gtools)

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

# Zobaczenie jak wyglądają time series
par(mfrow=c(3,1))

plot(with_heteregonity_chart,type="n",xlab="",ylab="")
lines(with_heteregonity_chart$V1,with_heteregonity_chart$gap)
abline(h=median(with_heteregonity_chart$gap),col="red",lty="dashed")
abline(h=mean(with_heteregonity_chart$gap)-sd(with_heteregonity_chart$gap),col="blue",lty="dashed")
abline(h=mean(with_heteregonity_chart$gap)+sd(with_heteregonity_chart$gap),col="blue",lty="dashed")

plot(without_heteregonity_chart,type="n",xlab="",ylab="")
lines(without_heteregonity_chart$V1,without_heteregonity_chart$gap)
abline(h=median(without_heteregonity_chart$gap),col="red",lty="dashed")
abline(h=mean(without_heteregonity_chart$gap)-sd(without_heteregonity_chart$gap),col="blue",lty="dashed")
abline(h=mean(without_heteregonity_chart$gap)+sd(without_heteregonity_chart$gap),col="blue",lty="dashed")

ccf(with_heteregonity_chart$gap,without_heteregonity_chart$gap,type = "correlation",main="CrossCorelation")


# Jedna rzecz, której trochę nie przewidziałem na którą w sumie mnie uczulałeś jest na pewno taka, że trzeba będzie zbinować te odstępy.
# W takim sensie, że różnice w czasach rzadko się powtarzają. Bez tego robienie tych ciągów Markova nie ma sensu.
# Nie wiem na ile to jest ciekawe na ile nie w tym przypadku z heteregonity się powtarzają, a w tym drugim nie.
# Tzn. na pewno jest to związane jest to z tym, że w tym pierwszym jest więcej tych strzałów ogólnie.
# Poza tym może to być kwestia pomiaru

sum(table(with_heteregonity_chart$gap)>1)
sum(table(without_heteregonity_chart$gap)>1)

# Na sam początek zbinuje je po 10. Patrząc na rozkłady już chyba trochę widać, że są podobnie prawo skośne ale example 2 jest przesunięty w względem example 2 w prawo.
# Czyli jednym słowem w example 2 średnie są dłuższe. Dość banalne to jest jak na razie.
# Poglądowy wykres tego jak wyglądają zbinowane po 10
ggplot()+geom_histogram(data=with_heteregonity_chart,aes(gap),binwidth = 10,fill="blue",alpha=0.5)+geom_histogram(data=without_heteregonity_chart,aes(gap),binwidth = 10,fill="red",alpha=0.5)+theme_classic()+scale_x_continuous("Gap")+scale_y_continuous("Frequency")

par(mfrow=c(1,2))
boxplot(with_heteregonity_chart$gap,main="With heteregonity")
boxplot(without_heteregonity_chart$gap,main="Without heteregonity")

dev.off()

#zbinowanie wyników

with_heteregonity_chart$gap_bin <- as.numeric(as.character(cut(with_heteregonity_chart$gap,c(0, seq(10, 120, by = 10)),c(1:12))))
without_heteregonity_chart$gap_bin <- as.numeric(as.character(cut(without_heteregonity_chart$gap,c(0, seq(10, 200, by = 10)),c(1:20))))

table(with_heteregonity_chart$gap_bin)
table(without_heteregonity_chart$gap_bin)

#funkcja, która przygtowuje wektor w taki sposób, że w zależności od długości historii
# zamienia np. przy historii 1 wektor c(1,2,3) na c(1,2,2,3)
#możliwe ulepszenie to oczywiście usunięcie for.
index_prep<- function(breaks_example_chart,history){
  help_data_frame <- data.frame(1:length(breaks_example_chart$gap_bin))
  for (i in 1:history){
    length <- c(1:length(breaks_example_chart$gap_bin))
    temp_data_frame <- (length+1) 
    help_data_frame <- cbind(help_data_frame,temp_data_frame)
    temp_vector <- as.vector(t(help_data_frame))
    temp_markov_series <- breaks_example_chart$gap_bin[temp_vector]
    temp_markov_series <- as.vector(na.omit(temp_markov_series))
    return(temp_markov_series)
  }
}

#funkcja do liczenia Markova. Pobiera ciąg i historię.
#Ciąg musi być w postaci np. dla historii 1 c(1,2,2,3).
#Funkcja przeskakuje po prostu co ileś tam wyrazów
#możliwe ulepszenie to na pewno pozbycie się sapply. Niestety for nie da się pozbyć.
#Do połamania sobie głowy to jeszcze trzeba sie zastanowić na długość ciągu przy porównaniu jest dobra. W sumie jest dużo rzeczy nad którymi warto się zastanowić.

markov <- function(original_markov_series=breaks_example_1_chart,temp_markov_series=index_prep(breaks_example_1_chart,1),history=1){
  compare <- function(table,vector){
    (wynik <- t(t(table)==vector))
    (wynik <- rowSums(wynik))
    wynik <- wynik>(length(vector)-1)
    return(wynik)
  }
  (number_of_states <- unique(original_markov_series$gap_bin))
  (transition_matrix <- as.data.frame(cbind(permutations(n=length(number_of_states),r=history+1,v=number_of_states,repeats.allowed=T),prob=0,n=1)))
  ciag <- 0
  #nie wszystkie ale przynajmniej 2
  ciag[c(1:history)] <- sample(sort(number_of_states),history,replace=T,as.vector(table(original_markov_series$gap_bin)/length(original_markov_series$gap_bin)))
  for (i in (history+1):length(original_markov_series$gap_bin)){
    (test_for_equality <- transition_matrix[compare(transition_matrix[,c(1:history)],temp_markov_series[(i-history):(i-1)]),])
    max_row <- test_for_equality$n==max(test_for_equality$n)
    if ((sum(max_row)>1) && (i!=length(temp_markov_series))){
      max_row <- test_for_equality$n==max(test_for_equality$n)
      ciag[i] <- sample(test_for_equality[max_row,history+1],1)
      add_row <- compare(transition_matrix[,c(1:(history+1))],original_markov_series$gap_bin[(i-history):i])
      transition_matrix$n[add_row] <- transition_matrix[add_row,history+3]+1
    } 
    if (sum(max_row)<2){
      max_row <- which.max(test_for_equality$n)
      ciag[i] <- test_for_equality[max_row,history+1]
      if(i!=length(temp_markov_series)){
        add_row <- compare(transition_matrix[,c(1:(history+1))],original_markov_series$gap_bin[(i-history):i])
        transition_matrix$n[add_row] <- transition_matrix$n[add_row]+1
      }
    }
    i <- i+history
  }
  print(transition_matrix)
  wynik <- ciag
  #wynik <- sum(ciag[1:(length(ciag))]==original_markov_series$gap_bin[1:length(original_markov_series$gap_bin)])/(length(ciag))
  return(wynik)
}


colnames(example_1)[2] <- "gap_bin" 
markov(example_1,index_prep(example_1,8),8)

#historia=1
(markov(with_heteregonity_chart,index_prep(with_heteregonity_chart,1),1))
(markov(without_heteregonity_chart,index_prep(without_heteregonity_chart,1),1))

(markov(with_heteregonity_chart,index_prep(with_heteregonity_chart,2),2))
(markov(without_heteregonity_chart,index_prep(without_heteregonity_chart,2),2))

(markov(with_heteregonity_chart,index_prep(with_heteregonity_chart,3),3))
(markov(without_heteregonity_chart,index_prep(without_heteregonity_chart,3),3))

(markov(with_heteregonity_chart,index_prep(with_heteregonity_chart,4),4))
(markov(without_heteregonity_chart,index_prep(without_heteregonity_chart,4),4))

(markov(with_heteregonity_chart,index_prep(with_heteregonity_chart,5),5))
(markov(without_heteregonity_chart,index_prep(without_heteregonity_chart,5),5))

markov_2 <- function(original_markov_series=breaks_example_1_chart,temp_markov_series=index_prep(breaks_example_1_chart,1),history=1){
  compare <- function(table,vector){
    (wynik <- t(t(table)==vector))
    (wynik <- rowSums(wynik))
    wynik <- wynik>(length(vector)-1)
    return(wynik)
  }
  (number_of_states <- unique(original_markov_series$gap_bin))
  (transition_matrix <- as.data.frame(cbind(permutations(n=length(number_of_states),r=history+1,v=number_of_states,repeats.allowed=T),prob=0,n=1)))
  transition_matrix$id <- (as.numeric(rownames(transition_matrix))-1)%/%length(number_of_states)
  ciag <- 0
  ciag[c(1:history)] <- sample(paste0(sort(number_of_states)),history,replace=T,as.vector(table(original_markov_series$gap_bin)/length(original_markov_series$gap_bin)))
  for (i in (history+1):length(original_markov_series$gap_bin)){
    (test_for_equality <- transition_matrix[compare(transition_matrix[,c(1:history)],temp_markov_series[(i-history):(i-1)]),])
    max_row <- test_for_equality$n==max(test_for_equality$n)
    if (sum(max_row)>1){
      max_row <- test_for_equality$n==max(test_for_equality$n)
      ciag[i] <- sample(test_for_equality[max_row,history+1],1)
      test_for_equality$prob <- test_for_equality$n/sum(test_for_equality$n)
      add_row <- compare(transition_matrix[,c(1:(history+1))],original_markov_series$gap_bin[(i-history):i])
      transition_matrix$n[add_row] <- transition_matrix[add_row,history+3]+1
      wynik_2 <- test_for_equality[test_for_equality[,history+1]==ciag[i],c((history+1):(history+2))]
      if(i==length(temp_markov_series)){
        add_row <- compare(transition_matrix[,c(1:(history+1))],original_markov_series$gap_bin[(i-history):i])
        transition_matrix$n[add_row] <- transition_matrix$n[add_row]+1
      }
    }
    if (sum(max_row)<2){
      test_for_equality$prob <- test_for_equality$n/sum(test_for_equality$n)
      max_row <- which.max(test_for_equality$n)
      ciag[i] <- test_for_equality[max_row,history+1]
      wynik_2 <- test_for_equality[max_row,c((history+1):(history+2))]
      if(i==length(temp_markov_series)){
        add_row <- compare(transition_matrix[,c(1:(history+1))],original_markov_series$gap_bin[(i-history):i])
        transition_matrix$n[add_row] <- transition_matrix$n[add_row]+1
      }
    }
    i <- i+history
  }
  wynik <- sum(ciag[1:(length(ciag))]==original_markov_series$gap_bin[1:length(original_markov_series$gap_bin)])/(length(ciag))
  return(wynik_2)
}

which_history <- 0
predicted_ciag <- 0
for (i in 1:(length(with_heteregonity_chart$gap_bin)-1)){
  if ((i==1) | (i==2)){
    opt_table <- data.frame(id=c(1:1),history=c(1:1))
    length_of_history <- ddply(opt_table,.(id),function(table){
      temp_table <- markov_2(with_heteregonity_chart[1:i,],index_prep(with_heteregonity_chart[1:i,],table$history),table$history)
      return(c(temp_table[,1],temp_table[,2]))
    })
  }
  if (i==3){
    opt_table <- data.frame(id=c(1:2),history=c(1:2))
    length_of_history <- ddply(opt_table,.(id),function(table){
      temp_table <- markov_2(with_heteregonity_chart[1:i,],index_prep(with_heteregonity_chart[1:i,],table$history),table$history)
      return(c(temp_table[,1],temp_table[,2]))
    })
  }
  if (i==4){
    opt_table <- data.frame(id=c(1:3),history=c(1:3))
    length_of_history <- ddply(opt_table,.(id),function(table){
      temp_table <- markov_2(with_heteregonity_chart[1:i,],index_prep(with_heteregonity_chart[1:i,],table$history),table$history)
      return(c(temp_table[,1],temp_table[,2]))
    })
  }
  if (i==5){
    opt_table <- data.frame(id=c(1:4),history=c(1:4))
    length_of_history <- ddply(opt_table,.(id),function(table){
      temp_table <- markov_2(with_heteregonity_chart[1:i,],index_prep(with_heteregonity_chart[1:i,],table$history),table$history)
      return(c(temp_table[,1],temp_table[,2]))
    })
  }
  if (i>5){
    opt_table <- data.frame(id=c(1:5),history=c(1:5))
    length_of_history <- ddply(opt_table,.(id),function(table){
      temp_table <- markov_2(with_heteregonity_chart[1:i,],index_prep(with_heteregonity_chart[1:i,],table$history),table$history)
      return(c(temp_table[,1],temp_table[,2]))
    })
  }
  print(length_of_history)
  maxi_row <- length_of_history[,3]==max(length_of_history[,3])
  if (sum(maxi_row)>1)
  {
    which_history[i] <- sample(length_of_history[maxi_row,1],1)
  }else{
    which_history[i] <- length_of_history[maxi_row,1]
  }
  predicted_ciag[i] <- length_of_history[which_history[i],2]
  print(i)
}

predicted_ciag_with <- predicted_ciag
which_history_with <- which_history
sum(predicted_ciag_with==with_heteregonity_chart$gap_bin[1:length(predicted_ciag_with)])/length(predicted_ciag_with)

unique(with_heteregonity_chart$gap_bin)

100/9

which_history <- 0
predicted_ciag <- 0
for (i in 1:(length(without_heteregonity_chart$gap_bin)-1)){
  if ((i==1) | (i==2)){
    opt_table <- data.frame(id=c(1:1),history=c(1:1))
    length_of_history <- ddply(opt_table,.(id),function(table){
      temp_table <- markov_2(without_heteregonity_chart[1:i,],index_prep(without_heteregonity_chart[1:i,],table$history),table$history)
      return(c(temp_table[,1],temp_table[,2]))
    })
  }
  if (i==3){
    opt_table <- data.frame(id=c(1:2),history=c(1:2))
    length_of_history <- ddply(opt_table,.(id),function(table){
      temp_table <- markov_2(without_heteregonity_chart[1:i,],index_prep(without_heteregonity_chart[1:i,],table$history),table$history)
      return(c(temp_table[,1],temp_table[,2]))
    })
  }
  if (i==4){
    opt_table <- data.frame(id=c(1:3),history=c(1:3))
    length_of_history <- ddply(opt_table,.(id),function(table){
      temp_table <- markov_2(without_heteregonity_chart[1:i,],index_prep(without_heteregonity_chart[1:i,],table$history),table$history)
      return(c(temp_table[,1],temp_table[,2]))
    })
  }
  if (i==5){
    opt_table <- data.frame(id=c(1:4),history=c(1:4))
    length_of_history <- ddply(opt_table,.(id),function(table){
      temp_table <- markov_2(without_heteregonity_chart[1:i,],index_prep(without_heteregonity_chart[1:i,],table$history),table$history)
      return(c(temp_table[,1],temp_table[,2]))
    })
  }
  if (i>5){
    opt_table <- data.frame(id=c(1:5),history=c(1:5))
    length_of_history <- ddply(opt_table,.(id),function(table){
      temp_table <- markov_2(without_heteregonity_chart[1:i,],index_prep(without_heteregonity_chart[1:i,],table$history),table$history)
      return(c(temp_table[,1],temp_table[,2]))
    })
  }
  print(length_of_history)
  maxi_row <- length_of_history[,3]==max(length_of_history[,3])
  if (sum(maxi_row)>1)
  {
    which_history[i] <- sample(length_of_history[maxi_row,1],1)
  }else{
    which_history[i] <- length_of_history[maxi_row,1]
  }
  predicted_ciag[i] <- length_of_history[which_history[i],2]
  print(i)
}

predicted_ciag_without <- predicted_ciag
which_history_without <- which_history
