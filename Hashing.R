
library(ape)
library(seqinr)
library(tidyverse)

data("woodmouse")
woodmouse

woodmouse_DNA <- str_to_upper(c2s(woodmouse[1,]))
woodmouse_DNA

install.packages('stringr')
library(stringr)

kmer_to_index <- function(kmer) {
  
  n <- str_length(kmer)
  letter_value <- c("A" = 0, "C" = 1, "G" = 2, "T" = 3)
  base <- 1
  index <- 1
  for (i in n:1) {
    
    nucleotide <- str_sub(kmer, start = i, end = i)
    index <- index + base * letter_value[nucleotide]
    base <- base * 4
    
    }
  
    return(as.numeric(index))
}

print("Dogs are not allowed here no metter much big size and breed dog have")
print("Just one sentance abandoned puppies if any bitch don't have self respect what can i do")


kmer_to_index("AA")

kmer_to_index("AC")

kmer_to_index("TG")



k <- 2

kmers <- numeric(4^k)
kmers

N <- str_length(woodmouse_DNA)
for (i in 1:(N - k + 1)) {
  
  kmer <- str_sub(woodmouse_DNA, i, i + k - 1)
  index <- kmer_to_index(kmer)
  kmers[index] <- kmers[index] + 1
  
}

kmers

seqinr::count(woodmouse[1,], 2)

get_DNA_sample <- function(DNA, n){
  N <- str_length(DNA)
  start <- sample(1:(N - n + 1), size = 1)
  return(str_sub(DNA, start, start + n - 1))
}

set.seed(2017)










