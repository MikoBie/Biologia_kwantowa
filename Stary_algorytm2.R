## Ładowanie potrzebnych pakietów
library(tidyverse)
library(magrittr)
library(gtools)

## Ta funkcja służy do przewidywania ciągu na podstawie macierzy przekształcenia z ukrytych ciągów Markova. Innymi słowy zakłada się w niej, że ludzie nie traktują losowości jako zdarzeń niezależnych. Przynajmniej nie zawsze tak ją traktują.

Markov <- function(ciag=ciag,historia=5){
  ## funkcja do porównywania wektora z tabelą
  compare <- function(table,vector){
    (wynik <- t(t(table)==vector))
    (wynik <- rowSums(wynik))
    wynik <- wynik>(length(vector)-1)
    return(wynik)
  }
  ## losowanie pierwszego elementu ciągu
  predicted_ciag <- data_frame(item=sample(unique(ciag),1),
                               history=-1,
                               p=.5)
  ## stworzenie listy tabel, w której zapisane są wszystkie kombinacje elementów dla danej historii. Oznacza to, że dla historii 1 i ciągu składającego się tylko z dwóch elementów tabela ma dwa wiersze.
  historia_lista <- list()
  for (i in (1:historia)) {
    historia_lista[[i]] <- permutations(n=ciag %>% unique %>% length,
                                        r=i,
                                        v=ciag %>% unique,
                                        repeats.allowed = TRUE) %>%
      cbind(n=0) %>%
      as_tibble()
  }
  ## element po elemencie przechodzenie przez ciąg
  for (i in (2:(ciag %>% length))){
    ## samoaktualizująca się lista tabel dla wszysktkich historii
    historia_lista <- lapply(X=historia_lista,
                             FUN=function(table){
                               if (i-(dim(table)[2]-1)>0){
                                 comaprison <- compare(table=table %>% select(-n),
                                                       vector=ciag[(i-(dim(table)[2]-1)):(i-1)])
                                 table$n[comaprison] <- table$n[comaprison]+1
                               }
                               return(table)
                             })
    ## lista tabel z przewidywaniami dotyczącymi następnego elementu
    transition_matrix <- lapply(X=historia_lista,
                                FUN=function(table){
                                  if((i-(dim(table)[2]-1)>0) & (dim(table)[2]!=2)){
                                    comparison_prediction <- compare(table=table %>% select(1:(ncol(.)-2)),
                                                                     vector=ciag[(i-(dim(table)[2]-2)):(i-1)])
                                    
                                    suma <- table %>%
                                      filter(comparison_prediction) %$%
                                      sum(n)
                                    prediction <- table %>%
                                      filter(comparison_prediction) %>%
                                      mutate(p=n/suma) %>%
                                      select(-n) %>%
                                      mutate(p=if_else(is.nan(p),.5,p)) %>%
                                      filter(p>=.5) %>%
                                      select((ncol(.)-1):ncol(.)) 
                                    
                                    if (dim(prediction)[1]>1){
                                      prediction <- prediction %>%
                                        slice(sample(c(1:length(prediction)),1))
                                    }
                                    return(prediction)
                                  }
                                  return(NULL)
                                  
                                })
    ## uzupełnienie listy tabel z przewidywaniami o pierwszą tabelę, bo ona jest inna niż wszyskie inne. Jest to po prostu klasyczne prawdopodobieństwo przy założeniu niezależności zdarzeń
    transition_matrix[[1]] <- data_frame(V0=c(0,1),p=c(1-(sum(ciag[1:i])/i),sum(ciag[1:i]/i))) 
    ## tabela z przewidywanym następnym elementem na podstawie historii 0
    predicted_item <- data_frame(item=0,history=0,p=0) %>%
      mutate(item=sample(x=unique(ciag) %>% sort,size=1,prob=transition_matrix[[1]]$p),
             history=0,
             p=transition_matrix[[1]]$p %>% max)
    ## wybór historii o najwyższym prawdopodobieństwie
    for (j in c(2:historia)){
      if (!is.null(transition_matrix[[j]])){
        if (transition_matrix[[j]]$p>predicted_item$p){
          predicted_item$p <- transition_matrix[[j]]$p
          predicted_item$history <- j
          predicted_item$item <- transition_matrix[[j]][1,1]
        }
      } 
    }
    ## zbindowanie wyników
    predicted_ciag <-   rbind(predicted_ciag,predicted_item)
  }
  ## zwraca tabele z przewidywanym elementem, historią na podstawie, której dokonywane jest przeiwydanie oraz prawdopodobieństwem tej hsitorii (przy historii 0 to prawdopodobieństwo wcale nie musi być poprawne, bo tam jest element losowy)
  return(predicted_ciag)  
}


Markov(example)