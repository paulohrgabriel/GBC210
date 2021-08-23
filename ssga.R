# Algoritmo genético steady-state (SSGA).
# A cada geração, subsitui apenas o pior indivíduo da geração anterior.

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
  
  fit = c(2,1,0,7,3)
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
pm = 0.5

genes = sample(c(0,1), size = tampop*tamcrom, replace = TRUE)
pop = matrix(genes, nrow = tampop, ncol = tamcrom)
print(pop)


gen = 0

cat("gen \t max \t avg\n")
while (gen < ngen) {
  
  fit = fitness(pop)
  #fit
  cat(gen, "\t", max(fit), "\t", mean(fit), "\n")
  
  pool = c()
  for (i in 1:tampop) {
    pool = c(pool, selecao(fit))
  }
  #print(pool)
  
  oldpop = pop
  oldfit = fit
  
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
  melhor = which.max(fit)
  pior = which.min(oldfit)
  
  if (fit[melhor] > oldfit[pior]) {
    oldpop[pior,] = pop[melhor,]
  }
  
  pop = oldpop
  #fit = fitness(oldpop)
  
  #print(fit)
  gen = gen + 1
  
  cat(gen, "\t", max(fit), "\t", mean(fit), "\n")
}
