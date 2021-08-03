# Algoritmo prelimitar para motivar o desenvolvimento de um algoritmo
#  genético completo
# Este algoritmo avalia uma única solução candidata com base em 
#  seu número de bits
# Aplica apenas mutações aleatórias
# Entrada:
# - tamanho do cromossomo (tc)
# - número de gerações (ng)
# Saída:
# - geração onde a melhor solução foi encontrada
# - número de bits da melhor solução
alg1 <- function(tc, ng) {
  
  # Gera uma solução candidata aleatória
  cromossomo = round(runif(tc))
  
  # Avalia o cromossomo
  nbits = sum(cromossomo)
  
  # Neste exemplo, a aptidão (fitness) é igual ao valor de nbits
  melhor_fitness = nbits
  
  # Vetor auxiliar, para armazenar o histórico do fitness
  vet_fitness = c()
  
  # Armazena o fitness do nosso primeiro indivíduo
  vet_fitness = c(vet_fitness, nbits)
  
  # Armazena o valor do melhor indivíduo encotrado
  #  (no momento, é o primeiro)
  melhor_crom = cromossomo
  
  # Imprime informações sobre o primeiro indivíduo (geração 0)
  cat(0, "\t", nbits, "\t", cromossomo, "\n")
  
  # Início do processo evolutivo
  cat("Geracao\t Fitness\t Cromossomo\n")
  gen = 0
  for (i in 1:ng) {
    
    # Seleciona um gene para sofre mutação
    pontomut <- sample(tc, 1, replace=TRUE)
    
    # Inverte o valor do respectivo bit
    cromossomo[pontomut] = !cromossomo[pontomut]
    
    # Avalia a nova solução candidata
    nbits = sum(cromossomo)
    
    # Armazena o fitness do i-ésimo individuo
    vet_fitness = c(vet_fitness, nbits)
    
    # Armazena os dados do melhor cromossomo
    # (geração onde foi encontrado, seu fitness e seu genótipo)
    if (nbits > melhor_fitness) {
      melhor_fitness = nbits
      gen = i
      melhor_crom = cromossomo
    }
    
    cat(i, "\t", nbits, "\t", cromossomo, "\n")
  }
  
  cat("Melhor cromossomo: ", melhor_crom, "\n")
  
  plot(vet_fitness, type = "l")
  
  return(c(gen, melhor_fitness))
}
