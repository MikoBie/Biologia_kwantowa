
library(VLMC)
data(bnrf1)
bnrf1EB [1:50]


lapply(wyniki,function(table){
  ciag <- 0
  length <- length(table)
  for (i in 2:length){
    wynik <- vlmc(table[1:(i)],cutoff=100)
    ciag[i] <- simulate.vlmc(wynik,1)
    # (predict_matrix <- predict(wynik,c(t(table[1:i]))))
    # if (predict_matrix[i,1]>predict_matrix[i:2]){
    #   ciag[i] <- 0
    # }else{
    #   ciag[i] <- 1
    # }
  }
print(sum(table[2:length]==ciag[1:length])/length)
})

plot <- 0

for(j in 1:100){
wynik <- vlmc(wyniki[[2]],cutoff=1)
l <- 0
for(i in 1:1000){
  l[i] <- sum(simulate(wynik,(length(wyniki[[2]])-1))==wyniki[[2]][2:length(wyniki[2])])/(length(wyniki[[2]])-1)
}

plot[j] <- sum(l)/length(l)
}


draw(wynik)

lines(plot)
permutation(6)

wyniki <- wyniki_new[,c(9:1024)]

wyniki <- as.list(as.data.frame(t(wyniki)))

wyniki <- lapply(wyniki,function(table){
  table <- table[!is.na(table)]
  return(table)
})


