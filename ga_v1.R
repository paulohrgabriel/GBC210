# Algoritmo genético simples, sem muitos detalhes
# Avalia um indivíduo com base em seu número de bits
# Aplica mutação e recombinação
# Entrada:
# - tamanho da população  (tp)
# - tamanho do cromossomo (tc)
# - número de gerações    (ng)
# Saída:
# - média e desvio do fitness da população final
# A seleção é realizada de maneira puramente aleatória
# Todos os indivíduos selecionados são cruzados e
#  seus filhos sofrem mutação
ga_v1 <- function(tp, tc, ng) {
  
  # Cria uma população de indivíduos
  # Cada cromossomo é uma linha da matriz
  pop <- matrix(round(runif(tp * tc)), nrow = tp)
  print(pop)
  
  # Cria um array para armazenar as aptidões
  vet_fitness <- c()
  
  # Avalia cada indivíduo
  # Neste exemplo, a aptidão é exatamente o valor da soma dos bits
  for (i in 1:nrow(pop)) {
    nbits = sum(pop[i,])
    vet_fitness = c(vet_fitness, nbits)
  }
  
  # Imprime a aptidão do melhor indivíduo, do pior e a média
  cat(0, "\t", max(vet_fitness), "\t", mean(vet_fitness), "\t", min(vet_fitness), "\n")
  
  # Início do processo evolutivo
  for (i in 1:ng) {
    # Seleciona dois indivíduos para reprodução
    reprodutor <- sample(nrow(pop), 2)
    

    #if (runif(1) > taxacross) { #0,1 - 0.7
      
    # Seleciona um ponto para ocorrer o crossover (recombinação)
    pontocross <- sample(tc-1, 1)
    
    # Aplica o crossover de 1-ponto, gerando dois filhos
    f1 <- c(pop[reprodutor[1], 1:pontocross],
            pop[reprodutor[2], (pontocross+1):tc])
    
    f2 <- c(pop[reprodutor[2], 1:pontocross],
            pop[reprodutor[1], (pontocross+1):tc])
    #}
    
    #if (runif(1) > taxamut) { #0,1 - 0.2
    # Seleciona um gene do filho 1 para sofre mutação
    pontomut <- sample(tc, 1, replace=TRUE)
    f1[pontomut] <- !f1[pontomut]
    
    # Seleciona um gene do filho 2 para sofre mutação
    pontomut <- sample(tc, 1, replace=TRUE)
    f2[pontomut] <- !f2[pontomut]
    #}
    
    # Calcula o fitness de cada filho
    fitness_f1 <- sum(f1)
    fitness_f2 <- sum(f2)
    
    # Encontra os dois indivíduos de menor fitness na população
    pos <- order(vet_fitness)[1:2]
    
    # Se os filhos têm melhor fitness que os dois piores indivíduos
    #  então realiza uma substituição
    if (fitness_f1 > vet_fitness[pos[1]]) {
      pop[pos[1],] <- f1
      vet_fitness[pos[1]] = fitness_f1
    }
    
    if (fitness_f2 > vet_fitness[pos[2]]) {
      pop[pos[2],] <- f2
      vet_fitness[pos[2]] = fitness_f2
    }
    
    # Imprime a aptidão do melhor indivíduo, do pior e a média
    cat(i, "\t", max(vet_fitness), "\t", mean(vet_fitness), "\t", min(vet_fitness), "\n")
  }
  
  print(pop)
  # Retorna o valor da média é do desvio-padrão
  return(c(mean(vet_fitness), sd(vet_fitness)))
}
