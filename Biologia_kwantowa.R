kupa
library(readr)
library(plyr)
library(markovchain)
library(gtools)
library(ggplot2)
#70.1342
example_1 <- read_csv("~/Desktop/Psychologia kwantowa/Survey/los_chem_zera_3_3_10_57_34.csv")
#61.4094
example_2 <- read_csv("~/Desktop/Psychologia kwantowa/Survey/los_chem_zera_3_1_14_17_18.csv")

#ważne między np. 2 i 3 jest przerwa 0.
#tworznie ciągu odstępu między jedynkami
example_1_rows_with_1 <- with(example_1,key==1)
example_2_rows_with_1 <- with(example_2,key==1)
breaks_example_1 <- example_1[example_1_rows_with_1,]
breaks_example_2 <- example_2[example_2_rows_with_1,]
breaks_example_1$gap <- c(NA,(breaks_example_1[2:nrow(breaks_example_1),1]-breaks_example_1[1:nrow(breaks_example_1)-1,1])$id)-1
breaks_example_2$gap <- c(NA,(breaks_example_2[2:nrow(breaks_example_2),1]-breaks_example_2[1:nrow(breaks_example_2)-1,1])$id)-1
#przydatne do wykresów bo bez pierwszego wiersza, w którym w normalnym zbiorze jest NA bo nie wiadomo kiedy było ostatnia 1.
breaks_example_1_chart <- breaks_example_1[-1,]
breaks_example_2_chart <- breaks_example_2[-1,]

par(mfrow=c(3,1),las=1,cex=0.75,cex.lab=4/3)
#ustalenie długości osi x, żeby miały tę samą długość
x1 <- c(1:dim(breaks_example_1_chart)[1])
x2 <- c(1:dim(breaks_example_2_chart)[1])

if(length(x1)>length(x2)){
  x <- x1
}else{
  x <- x2
}
#mnożę razy 20 tylko dlatego, ze inaczej jest nieczytelne przy tak długiej osi x.
#czerwona linia to mediana, a niebieskie to +/-odchylenie standardowe
#nie ma legendy bo ciężko zrobic taką, która nie zasłoni całego wyrkesu
y1 <- c(0:max(breaks_example_1_chart$gap))*20
plot(x,type="n",xlab="",ylab="",axes=F)
title(ylab=strwrap("Distance between spikes example 1",30),line=1)
axis(2,pos=0,y1,labels=y1/20)
axis(1,pos=-5,x,labels=F)
lines(breaks_example_1_chart$gap*20)
abline(h=median(breaks_example_1_chart$gap)*20,col="red",lty="dashed")
abline(h=mean(breaks_example_1_chart$gap)*20-sd(breaks_example_1_chart$gap)*20,col="blue",lty="dashed")
abline(h=mean(breaks_example_1_chart$gap)*20+sd(breaks_example_1_chart$gap)*20,col="blue",lty="dashed")
#legend("topright", legend=c("Median", "Mean +/- SD"),lty="dashed", col=c("blue", "red"),cex=1,pt.lwd=0.25)

#Drugi wykres, drugiego ciągu
y2 <- c(0:max(breaks_example_2_chart$gap))*20
plot(x,type="n",xlab="",ylab="",axes=F)
title(ylab=strwrap("Distance between spikes example 2",30),line=1)
axis(2,pos=0,y2,2,labels=y2/20)
axis(1,pos=-5,x,labels=F)
lines(breaks_example_2_chart$gap*20)
abline(h=median(breaks_example_2_chart$gap)*20,col="red",lty="dashed")
abline(h=mean(breaks_example_2_chart$gap)*20-sd(breaks_example_2_chart$gap)*20,col="blue",lty="dashed")
abline(h=mean(breaks_example_2_chart$gap)*20+sd(breaks_example_2_chart$gap)*20,col="blue",lty="dashed")
#wykres kross korelacji pomędzy example 1, a example 2. Zasadniczo lag odnosi się do drugiego ciągu.
ccf(breaks_example_1_chart$gap,breaks_example_2_chart$gap,type = "correlation",ylab="",main="CrossCorrelation example 1 and example 2")

#rozkłady prawdopodobieństwa poszczególnych
probability_distribution_example_1 <- as.vector(table(breaks_example_1_chart$gap)/length(breaks_example_1_chart$gap))
probability_distribution_example_2 <- as.vector(table(breaks_example_2_chart$gap)/length(breaks_example_2_chart$gap))

#funkcja, która za pomocą klasycznego prawdopodobieństwa liczy 1000 prawdopodobieństw trafienia w nasz zwykły ciąg
#potrzebuje, tablei z gap oraz prawdopodobieństwo poszczególnych gapów.
classic_approach <- function(breaks_example,probability_distribution){
  wynik <- 0
  for(j in c(1:1000)){
    temp_wynik <- 0
    temp_wynik <- sample(sort(unique(breaks_example$gap)),length(breaks_example$gap),replace=T,probability_distribution[probability_distribution>0])
    wynik[j] <- (sum(breaks_example$gap==temp_wynik)/length(breaks_example))/100
  }
  return(wynik)
}

classic_approach_example_1 <- classic_approach(breaks_example_1_chart,probability_distribution_example_1)
classic_approach_example_2 <- classic_approach(breaks_example_2_chart,probability_distribution_example_2)


#czerwona linia to średnia, a czarna to oczywiście mediana. Dlatego lepiej brać medianę.
par(mfrow=c(1,2),las=1)
boxplot(classic_approach_example_1,xlab="",ylab="")
title(xlab = "Example 1",ylab="Predictability using classic approach")
abline(h=mean(classic_approach_example_1),col="red")
boxplot(classic_approach_example_2,xlab="",ylab="")
title(xlab = "Example 2",ylab="Predictability using classic approach")
abline(h=mean(classic_approach_example_2),col="red")


if(length(probability_distribution_example_1)>length(probability_distribution_example_2)){
  difference <- length(probability_distribution_example_1)-length(probability_distribution_example_2)
  probability_distribution_example_1 <- c(probability_distribution_example_1,rep(0,difference))
}else{
  difference <- length(probability_distribution_example_2)-length(probability_distribution_example_1)
  probability_distribution_example_1 <- c(probability_distribution_example_1,rep(0,difference))
}

par(mfrow=c(2,1),las=1)
plot(probability_distribution_example_1,type="n",xlab="",ylab="")
title(ylab="Example 1. Probability of distances",xlab = "Distances between spikes")
lines(probability_distribution_example_1)

plot(probability_distribution_example_2,type="n",xlab="",ylab="")
title(ylab="Example 2. Probability of distances",xlab = "Distances between spikes")
lines(probability_distribution_example_2)


#sprawdzić dlaczego to nie działa. Dlaczego chi square wywala błąd.
#chisq.test(probability_distribution_example_1,p=rep(1/length(probability_distribution_example_1),length(probability_distribution_example_1)))
#chisq.test(probability_distribution_example_2,p=rep(1/length(probability_distribution_example_2),length(probability_distribution_example_2)))

#funkcja, która przygtowuje wektor w taki sposób, że w zależności od długości historii
# zamienia np. przy historii 1 wektor c(1,2,3) na c(1,2,2,3)
#możliwe ulepszenie to oczywiście usunięcie for.
index_prep<- function(breaks_example_chart,history){
  help_data_frame <- data.frame(1:length(breaks_example_chart$gap))
  for (i in 1:history){
    length <- c(1:length(breaks_example_chart$gap))
    temp_data_frame <- (length+1) 
    help_data_frame <- cbind(help_data_frame,temp_data_frame)
    temp_vector <- as.vector(t(help_data_frame))
    temp_markov_series <- breaks_example_chart$gap[temp_vector]
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
  (number_of_states <- unique(original_markov_series$gap))
  (transition_matrix <- as.data.frame(cbind(permutations(n=length(number_of_states),r=history+1,v=number_of_states,repeats.allowed=T),prob=0,n=1)))
  ciag <- 0
  #nie wszystkie ale przynajmniej 2
  ciag[c(1:history)] <- sample(sort(number_of_states),history,replace=T,as.vector(table(original_markov_series$gap)/length(original_markov_series$gap)))
  for (i in (history+1):length(original_markov_series$gap)){
    (test_for_equality <- transition_matrix[compare(transition_matrix[,c(1:history)],temp_markov_series[(i-history):(i-1)]),])
    max_row <- test_for_equality$n==max(test_for_equality$n)
    if ((sum(max_row)>1) && (i!=length(temp_markov_series))){
      max_row <- test_for_equality$n==max(test_for_equality$n)
      ciag[i] <- sample(test_for_equality[max_row,history+1],1)
      add_row <- compare(transition_matrix[,c(1:(history+1))],original_markov_series$gap[(i-history):i])
      transition_matrix$n[add_row] <- transition_matrix[add_row,history+3]+1
    } 
    if (sum(max_row)<2){
      max_row <- which.max(test_for_equality$n)
      ciag[i] <- test_for_equality[max_row,history+1]
      if(i!=length(temp_markov_series)){
        add_row <- compare(transition_matrix[,c(1:(history+1))],original_markov_series$gap[(i-history):i])
        transition_matrix$n[add_row] <- transition_matrix$n[add_row]+1
      }
    }
    i <- i+history
  }
  wynik <- sum(ciag[1:(length(ciag))]==original_markov_series$gap[1:length(original_markov_series$gap)])/(length(ciag))
  return(wynik)
}

#historia=1
(markov(breaks_example_1_chart,index_prep(breaks_example_1_chart,1),1))
(markov(breaks_example_2_chart,index_prep(breaks_example_2_chart,1),1))

#estimation_markov_1_example_1<- 0
#system.time(for (i in 1:1000){estimation_markov_1_example_1[i] <- (markov(breaks_example_1_chart,index_prep(breaks_example_1_chart,1),1))} )
#estimation_markov_1_example_2<- 0
#system.time(for (i in 1:1000){estimation_markov_1_example_2[i] <- (markov(breaks_example_2_chart,index_prep(breaks_example_2_chart,1),1))} )

#estimation_matrix <- data.frame(Example_1=estimation_markov_1_example_1,Example_2=estimation_markov_1_example_2,Markov=1)
#estimation_matrix

#czerwona linia to średnia, a czarna to oczywiście mediana. Dlatego lepiej brać medianę.
# par(mfrow=c(1,2),las=1)
# boxplot(estimation_markov_1_example_1)
# title(xlab = "Example 1",ylab="Predictability using History 1",line=4)
# abline(h=mean(estimation_markov_1_example_1),col="red")
# boxplot(estimation_markov_1_example_2)
# title(xlab = "Example 2",ylab="Predictability using History 1")
# abline(h=mean(estimation_markov_1_example_2),col="red")

#historia=2
(markov(breaks_example_1_chart,index_prep(breaks_example_1_chart,1),2))
(markov(breaks_example_2_chart,index_prep(breaks_example_2_chart,1),2))

# estimation_markov_2_example_1<- 0
# system.time(for (i in 1:1000){estimation_markov_2_example_1[i] <- (markov(breaks_example_1_chart,index_prep(breaks_example_1_chart,2),2))} )
# estimation_markov_2_example_2<- 0
# system.time(for (i in 1:1000){estimation_markov_2_example_2[i] <- (markov(breaks_example_2_chart,index_prep(breaks_example_2_chart,2),2))} )
# 
# estimation_matrix <- rbind(estimation_matrix,data.frame(Example_1=estimation_markov_2_example_1,Example_2=estimation_markov_2_example_2,Markov=2))
#  estimation_matrix
# 
# #czerwona linia to średnia, a czarna to oczywiście mediana. Dlatego lepiej brać medianę.
# par(mfrow=c(1,2),las=1)
# boxplot(estimation_markov_2_example_1)
# title(xlab = "Example 1",ylab="Predictability using History 1")
# abline(h=mean(estimation_markov_2_example_1),col="red")
# boxplot(estimation_markov_2_example_2)
# title(xlab = "Example 2",ylab="Predictability using History 1")
# abline(h=mean(estimation_markov_2_example_2),col="red")


#historia=3
(markov(breaks_example_1_chart,index_prep(breaks_example_1_chart,1),3))
(markov(breaks_example_2_chart,index_prep(breaks_example_2_chart,1),3))


# estimation_markov_3_example_1<- 0
# system.time(for (i in 1:1000){estimation_markov_3_example_1[i] <- (markov(breaks_example_1_chart,index_prep(breaks_example_1_chart,2),3))} )
# estimation_markov_3_example_2<- 0
# system.time(for (i in 1:1000){estimation_markov_3_example_2[i] <- (markov(breaks_example_2_chart,index_prep(breaks_example_2_chart,2),3))} )
#
# estimation_matrix <- rbind(estimation_matrix,data.frame(Example_1=estimation_markov_3_example_1,Example_2=estimation_markov_3_example_2,Markov=3))
# estimation_matrix

# #czerwona linia to średnia, a czarna to oczywiście mediana. Dlatego lepiej brać medianę.
# par(mfrow=c(1,2),las=1)
# boxplot(estimation_markov_3_example_1)
# title(xlab = "Example 1",ylab="Predictability using History 1")
# abline(h=mean(estimation_markov_3_example_1),col="red")
# boxplot(estimation_markov_3_example_2)
# title(xlab = "Example 2",ylab="Predictability using History 1")
# abline(h=mean(estimation_markov_3_example_2),col="red")

#historia=4
(markov(breaks_example_1_chart,index_prep(breaks_example_1_chart,1),4))
(markov(breaks_example_2_chart,index_prep(breaks_example_2_chart,1),4))

# estimation_markov_4_example_1<- 0
# system.time(for (i in 1:1000){estimation_markov_4_example_1[i] <- (markov(breaks_example_1_chart,index_prep(breaks_example_1_chart,4),4))} )
# estimation_markov_2_example_2<- 0
# system.time(for (i in 1:1000){estimation_markov_2_example_2[i] <- (markov(breaks_example_2_chart,index_prep(breaks_example_2_chart,4),4))} )

#estimation_matrix <- rbind(estimation_matrix,data.frame(Example_1=estimation_markov_4_example_1,Example_2=estimation_markov_4_example_2,Markov=4))
# estimation_matrix
# #czerwona linia to średnia, a czarna to oczywiście mediana. Dlatego lepiej brać medianę.
# par(mfrow=c(1,2),las=1)
# boxplot(estimation_markov_4_example_1)
# title(xlab = "Example 1",ylab="Predictability using History 1")
# abline(h=mean(estimation_markov_4_example_1),col="red")
# boxplot(estimation_markov_4_example_2)
# title(xlab = "Example 2",ylab="Predictability using History 1")
# abline(h=mean(estimation_markov_4_example_2),col="red")

#historia=5
(markov(breaks_example_1_chart,index_prep(breaks_example_1_chart,1),5))
(markov(breaks_example_2_chart,index_prep(breaks_example_2_chart,1),5))

# estimation_markov_5_example_1<- 0
# system.time(for (i in 1:1000){estimation_markov_5_example_1[i] <- (markov(breaks_example_1_chart,index_prep(breaks_example_1_chart,5),5))} )
# estimation_markov_5_example_2<- 0
# system.time(for (i in 1:1000){estimation_markov_5_example_2[i] <- (markov(breaks_example_2_chart,index_prep(breaks_example_2_chart,5),5))} )
# 
# estimation_matrix <- rbind(estimation_matrix,data.frame(Example_1=estimation_markov_5_example_1,Example_2=estimation_markov_5_example_2,Markov=5))
# estimation_matrix
# #czerwona linia to średnia, a czarna to oczywiście mediana. Dlatego lepiej brać medianę.
# par(mfrow=c(1,2),las=1)
# boxplot(estimation_markov_5_example_1)
# title(xlab = "Example 1",ylab="Predictability using History 1")
# abline(h=mean(estimation_markov_5_example_1),col="red")
# boxplot(estimation_markov_5_example_2)
# title(xlab = "Example 2",ylab="Predictability using History 1")
# abline(h=mean(estimation_markov_5_example_2),col="red")

#historia=6
#(markov(breaks_example_1_chart,index_prep(breaks_example_1_chart,1),6))
#(markov(breaks_example_2_chart,index_prep(breaks_example_2_chart,1),6))

# estimation_markov_6_example_1<- 0
# system.time(for (i in 1:1000){estimation_markov_6_example_1[i] <- (markov(breaks_example_1_chart,index_prep(breaks_example_1_chart,6),6))} )
# estimation_markov_6_example_2<- 0
# system.time(for (i in 1:1000){estimation_markov_6_example_2[i] <- (markov(breaks_example_2_chart,index_prep(breaks_example_2_chart,6),6))} )

# estimation_matrix <- rbind(estimation_matrix,data.frame(Example_1=estimation_markov_6_example_1,Example_2=estimation_markov_6_example_2,Markov=6))
# estimation_matrix
#czerwona linia to średnia, a czarna to oczywiście mediana. Dlatego lepiej brać medianę.
# par(mfrow=c(1,2),las=1)
# boxplot(estimation_markov_6_example_1)
# title(xlab = "Example 1",ylab="Predictability using History 1")
# abline(h=mean(estimation_markov_6_example_1),col="red")
# boxplot(estimation_markov_6_example_2)
# title(xlab = "Example 2",ylab="Predictability using History 1")
# abline(h=mean(estimation_markov_6_example_2),col="red")



#super ekstra giga funkcja oraz kroczące prawdopodobieństwo

table_mean_example_1 <- with(estimation_matrix,aggregate(Example_1,by=list(Markov),mean))
ggplot(data=estimation_matrix,aes(y=Example_1,x=as.factor(Markov)))+geom_boxplot()+geom_line(data=table_mean_example_1,aes(x=(Group.1),y=x),col="red")+geom_point(data=table_mean_example_1,aes(x=(Group.1),y=x),col="red")+theme_classic()+scale_y_continuous("Example 1")+scale_x_discrete("Length of history on which prediction are made")
table_mean_example_2 <- with(estimation_matrix,aggregate(Example_2,by=list(Markov),mean))
ggplot(data=estimation_matrix,aes(y=Example_2,x=as.factor(Markov-1)))+geom_boxplot()+geom_line(data=table_mean_example_2,aes(x=(Group.1-1),y=x),col="red")+geom_point(data=table_mean_example_2,aes(x=(Group.1-1),y=x),col="red")+theme_classic()+scale_y_continuous("Example 2")+scale_x_discrete("Length of history on which prediction are made")

#Markov, który zwraca prawdopodobieństwo następnego wyrazu. Zasadniczo ta funkcja służy do tego, żeby móc porównywać najlepsze prawdopodobieństwa.

markov_2 <- function(original_markov_series=breaks_example_1_chart,temp_markov_series=index_prep(breaks_example_1_chart,1),history=1){
  compare <- function(table,vector){
    (wynik <- t(t(table)==vector))
    (wynik <- rowSums(wynik))
    wynik <- wynik>(length(vector)-1)
    return(wynik)
  }
  (number_of_states <- unique(original_markov_series$gap))
  (transition_matrix <- as.data.frame(cbind(permutations(n=length(number_of_states),r=history+1,v=number_of_states,repeats.allowed=T),prob=0,n=1)))
  transition_matrix$id <- (as.numeric(rownames(transition_matrix))-1)%/%length(number_of_states)
  ciag <- 0
  ciag[c(1:history)] <- sample(sort(number_of_states),history,replace=T,as.vector(table(original_markov_series$gap)/length(original_markov_series$gap)))
  for (i in (history+1):length(original_markov_series$gap)){
    (test_for_equality <- transition_matrix[compare(transition_matrix[,c(1:history)],temp_markov_series[(i-history):(i-1)]),])
    max_row <- test_for_equality$n==max(test_for_equality$n)
    if (sum(max_row)>1){
      max_row <- test_for_equality$n==max(test_for_equality$n)
      ciag[i] <- sample(test_for_equality[max_row,history+1],1)
      test_for_equality$prob <- test_for_equality$n/sum(test_for_equality$n)
      add_row <- compare(transition_matrix[,c(1:(history+1))],original_markov_series$gap[(i-history):i])
      transition_matrix$n[add_row] <- transition_matrix[add_row,history+3]+1
      wynik_2 <- test_for_equality[test_for_equality[,history+1]==ciag[i],c((history+1):(history+2))]
      if(i==length(temp_markov_series)){
        add_row <- compare(transition_matrix[,c(1:(history+1))],original_markov_series$gap[(i-history):i])
        transition_matrix$n[add_row] <- transition_matrix$n[add_row]+1
      }
    }
    if (sum(max_row)<2){
      test_for_equality$prob <- test_for_equality$n/sum(test_for_equality$n)
      max_row <- which.max(test_for_equality$n)
      ciag[i] <- test_for_equality[max_row,history+1]
      wynik_2 <- test_for_equality[max_row,c((history+1):(history+2))]
      if(i==length(temp_markov_series)){
        add_row <- compare(transition_matrix[,c(1:(history+1))],original_markov_series$gap[(i-history):i])
        transition_matrix$n[add_row] <- transition_matrix$n[add_row]+1
      }
    }
    i <- i+history
  }
  wynik <- sum(ciag[1:(length(ciag))]==original_markov_series$gap[1:length(original_markov_series$gap)])/(length(ciag))
  return(wynik_2)
}

#Wyszukuje najwyższe prawdopodobieństwo pomiędzy historią od 1 do 5. Nadal jest to statyczna funkcja.
#Żeby zwiększyć historię to trzeba dodać jeszcze jedno if np. (i==6), a potem w w opt_tabeli dać odpowiednie indeksy, tzn. dla 6 zwiększyć z 1:5 na 1:6.
#Może trzeba by to na funkcję przerobić, ale na razie mi się nie chce.

debug(markov_2)

which_history <- 0
predicted_ciag <- 0
for (i in 1:(length(breaks_example_1$gap)-1)){
  if ((i==1) | (i==2)){
    opt_table <- data.frame(id=c(1:1),history=c(1:1))
    length_of_history <- ddply(opt_table,.(id),function(table){
      temp_table <- markov_2(breaks_example_1_chart[1:i,],index_prep(breaks_example_1_chart[1:i,],table$history),table$history)
      return(c(temp_table[,1],temp_table[,2]))
    })
  }
  if (i==3){
    opt_table <- data.frame(id=c(1:2),history=c(1:2))
    length_of_history <- ddply(opt_table,.(id),function(table){
      temp_table <- markov_2(breaks_example_1_chart[1:i,],index_prep(breaks_example_1_chart[1:i,],table$history),table$history)
      return(c(temp_table[,1],temp_table[,2]))
    })
  }
  if (i==4){
    opt_table <- data.frame(id=c(1:3),history=c(1:3))
    length_of_history <- ddply(opt_table,.(id),function(table){
      temp_table <- markov_2(breaks_example_1_chart[1:i,],index_prep(breaks_example_1_chart[1:i,],table$history),table$history)
      return(c(temp_table[,1],temp_table[,2]))
    })
  }
  if (i==5){
    opt_table <- data.frame(id=c(1:4),history=c(1:4))
    length_of_history <- ddply(opt_table,.(id),function(table){
      temp_table <- markov_2(breaks_example_1_chart[1:i,],index_prep(breaks_example_1_chart[1:i,],table$history),table$history)
      return(c(temp_table[,1],temp_table[,2]))
    })
  }
  if (i>5){
    opt_table <- data.frame(id=c(1:5),history=c(1:5))
    length_of_history <- ddply(opt_table,.(id),function(table){
      temp_table <- markov_2(breaks_example_1_chart[1:i,],index_prep(breaks_example_1_chart[1:i,],table$history),table$history)
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

#Na jakiej historii przewidywaliśmy kolejne elementy.
which_history

#prawdopodobieństwo ciągu.
sum(breaks_example_1_chart$gap==predicted_ciag)/length(predicted_ciag)




#kroczące. Problem się tu pojawia. Nie mówiąc już o tym, że w poprzednim jest dramat.

result_2 <- 0
which_history <- 0
predicted_ciag <- 0
  
for (i in 1:(length(breaks_example_1$gap)-1)){
    if ((i==1) | (i==2)){
      opt_table <- data.frame(id=c(1:1),history=c(1:1))
      length_of_history <- ddply(opt_table,.(id),function(table){
        temp_table <- markov_2(breaks_example_1_chart[1:i,],index_prep(breaks_example_1_chart[1:i,],table$history),table$history)
        return(c(temp_table[,1],temp_table[,2]))
      })
    }
    if (i==3){
      opt_table <- data.frame(id=c(1:2),history=c(1:2))
      length_of_history <- ddply(opt_table,.(id),function(table){
        temp_table <- markov_2(breaks_example_1_chart[1:i,],index_prep(breaks_example_1_chart[1:i,],table$history),table$history)
        print(temp_table)
        return(c(temp_table[,1],temp_table[,2]))
      })
    }
    if (i==4){
      opt_table <- data.frame(id=c(1:3),history=c(1:3))
      length_of_history <- ddply(opt_table,.(id),function(table){
        temp_table <- markov_2(breaks_example_1_chart[1:i,],index_prep(breaks_example_1_chart[1:i,],table$history),table$history)
        return(c(temp_table[,1],temp_table[,2]))
      })
    }
    if (i==5){
      opt_table <- data.frame(id=c(1:4),history=c(1:4))
      length_of_history <- ddply(opt_table,.(id),function(table){
        temp_table <- markov_2(breaks_example_1_chart[1:i,],index_prep(breaks_example_1_chart[1:i,],table$history),table$history)
        return(c(temp_table[,1],temp_table[,2]))
      })
    }
    if (i>5){
      opt_table <- data.frame(id=c(1:5),history=c(1:5))
      length_of_history <- ddply(opt_table,.(id),function(table){
        temp_table <- markov_2(breaks_example_1_chart[c((i-5):i),],index_prep(breaks_example_1_chart[c((i-5):i),],table$history),table$history)
        return(c(temp_table[,1],temp_table[,2]))
      })
    }
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

which_history  
result_2 <- sum(breaks_example_1_chart$gap==predicted_ciag)/length(predicted_ciag)
result_2


example_1_gap <- example_1
colnames(example_1_gap)[2] <- "gap"
wynik <- 0
# for (i in 1:100){
  #wynik[i] <- 
    markov(example_1_gap,index_prep(example_1_gap,1),1)
    merkov(example_1_gap,index_prep((example_1_gap)))
  #print(i)
# }
# plot(wynik)

which_history <- 0
predicted_ciag <- 0

for (i in 1:(length(example_1_gap$gap))){
  if ((i==1) | (i==2)){
    opt_table <- data.frame(id=c(1:1),history=c(1:1))
    length_of_history <- ddply(opt_table,.(id),function(table){
      temp_table <- markov_2(example_1_gap[1:i,],index_prep(example_1_gap[1:i,],table$history),table$history)
      return(c(temp_table[,1],temp_table[,2]))
    })
  }
  if (i==3){
    opt_table <- data.frame(id=c(1:2),history=c(1:2))
    length_of_history <- ddply(opt_table,.(id),function(table){
      temp_table <- markov_2(example_1_gap[1:i,],index_prep(example_1_gap[1:i,],table$history),table$history)
      return(c(temp_table[,1],temp_table[,2]))
    })
  }
  if (i==4){
    opt_table <- data.frame(id=c(1:3),history=c(1:3))
    length_of_history <- ddply(opt_table,.(id),function(table){
      temp_table <- markov_2(example_1_gap[1:i,],index_prep(example_1_gap[1:i,],table$history),table$history)
      return(c(temp_table[,1],temp_table[,2]))
    })
  }
  if (i==5){
    opt_table <- data.frame(id=c(1:4),history=c(1:4))
    length_of_history <- ddply(opt_table,.(id),function(table){
      temp_table <- markov_2(example_1_gap[1:i,],index_prep(example_1_gap[1:i,],table$history),table$history)
      return(c(temp_table[,1],temp_table[,2]))
    })
  }
  if (i>5){
    opt_table <- data.frame(id=c(1:5),history=c(1:5))
    length_of_history <- ddply(opt_table,.(id),function(table){
      temp_table <- markov_2(example_1_gap[1:i,],index_prep(example_1_gap[1:i,],table$history),table$history)
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
ciag_statyczne <- predicted_ciag
sum(predicted_ciag==example_1_gap$gap)/length(predicted_ciag)

which_history <- 0
predicted_ciag <- 0


example_1_gap$gap

for (i in 1:(length(example_1_gap$gap))){
  if ((i==1) | (i==2) | (i==3) | (i==4)){
    opt_table <- data.frame(id=c(1:1),history=c(1:1))
    length_of_history <- ddply(opt_table,.(id),function(table){
      temp_table <- markov_2(example_1_gap[1:i,],index_prep(example_1_gap[1:i,],table$history),table$history)
      return(c(temp_table[,1],temp_table[,2]))
    })
  }
  if ((i==-3) | (i==-4)){
    opt_table <- data.frame(id=c(1:2),history=c(1:4))
    length_of_history <- ddply(opt_table,.(id),function(table){
      temp_table <- markov_2(example_1_gap[1:i,],index_prep(example_1_gap[1:i,],table$history),table$history)
      print(temp_table)
      return(c(temp_table[,1],temp_table[,2]))
    })
  }
  if (i==-4){
    opt_table <- data.frame(id=c(1:2),history=c(1,4))
    length_of_history <- ddply(opt_table,.(id),function(table){
      temp_table <- markov_2(example_1_gap[1:i,],index_prep(example_1_gap[1:i,],table$history),table$history)
      return(c(temp_table[,1],temp_table[,2]))
    })
  }
  if (i==5){
    opt_table <- data.frame(id=c(1:2),history=c(1,4))
    length_of_history <- ddply(opt_table,.(id),function(table){
      temp_table <- markov_2(example_1_gap[1:i,],index_prep(example_1_gap[1:i,],table$history),table$history)
      return(c(temp_table[,1],temp_table[,2]))
    })
  }
  if (i>5){
    opt_table <- data.frame(id=c(1:3),history=c(1,4,5))
    length_of_history <- ddply(opt_table,.(id),function(table){
      temp_table <- markov_2(example_1_gap[c((i-5):i),],index_prep(example_1_gap[c((i-5):i),],table$history),table$history)
      return(c(temp_table[,1],temp_table[,2]))
    })
  }
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

ciag_kraoczace <- predicted_ciag
sum(ciag_kraoczace==ciag_statyczne)
sum(ciag_kraoczace==example_1_gap$gap)
sum(ciag_statyczne==example_1_gap$gap)

sum(predicted_ciag==example_1_gap$gap[1:13])/length(predicted_ciag)

which_history





