# Algoritmo genético simples (SGA) para maximização da soma de bits.
# Algoritmo geracional: a cada geração, subsitui toda a população.

# Exercício: fazer versão steady-state (substitui apenas o pior a cada geração).

fobj <- function(x) {
  # [0,1,1,0,0] = 2
  return(sum(x))
}

fitness <- function(pop) {
  f = rep(0, tampop)
  for (i in 1:tampop) {
    f[i] = fobj(pop[i,])
  }
  
  return(f)
}

selecao <- function(fit) {

  #fit = c(2,0,0,6)
  fit = sort(fit)
  
  prob = rep(0, tampop)
  prob = fit / sum(fit)
  
  rand = runif(1, 0, max(prob))
  #print(rand)
  for (i in 1:length(fit)) {
    if (rand < prob[i]) {
      return(i)
    }
  }
  
  #return(sample(x = tampop, size = 1))
}


crossover <- function(pop, p1, p2, pc) {
  c = sample(x = tamcrom-2, size = 1)+1
  
  if (runif(1) < pc) {
    temp = pop[p1, 1:c]
    pop[p1, 1:c] = pop[p2, 1:c]
    pop[p2, 1:c] = temp
  }
  
  return(pop)
}

mutacao <- function(pop, pm) {
  #for (i in 1:tampop) {
  if (runif(1) < pm) {
    c = sample(x = tamcrom, size = 1)
    if (pop[i,c] == 0) pop[i,c] = 1
    else pop[i,c] = 0
  }
  #}
  
  return(pop)
}

### main

tampop = 5
tamcrom = 8
ngen = 30
pc = 0.7
pm = 1/tampop
  
genes = sample(c(0,1), size = tampop*tamcrom, replace = TRUE)
pop = matrix(genes, nrow = tampop, ncol = tamcrom)
print(pop)
fit = fitness(pop)
fit
  
gen = 0
  
cat("gen \t max \t avg\n")
cat(gen, "\t", max(fit), "\t", mean(fit), "\n")
while (gen < ngen) {
  pool = c()
  for (i in 1:tampop) {
    pool = c(pool, selecao(fit))
  }
  #print(pool)
  
  oldpop = pop
    
  for (i in 1:tampop) {
    pop[i,] = oldpop[pool[i],]
  }
    
  i = 1
  while (i < tampop) {
    pop = crossover(pop, pool[i], pool[i+1], pc)
    i = i+2
  }
    
  for (i in 1:tampop) {
    pop = mutacao(pop, pm)
  }
  
  fit = fitness(pop)
  #print(fit)
  gen = gen + 1
  cat(gen, "\t", max(fit), "\t", mean(fit), "\n")
}

### GGA - Algoritmo Genético Geracional

#for (i in 1:4) print(selecao(fit))
