# Função objetivo a ser minimizada
f <- function(x) {
  x^4 + 2*x^3 - 12*x^2 - 2*x + 6
}

# Estratégia Evolutiva do tipo (1,1)-EE
# - Um indivíduo (pai) gera um novo indivíduo (filho)
# - O filho substitui o pai
# - Reprodução assexuada
# Primeira versão: apenas um indivíduo
ee1 <- function() {
  
  # x é um conjunto de soluções
  # x é uma "População"
  # Cada elemento de x é um "Indivíduo"
  
  # Gera 1 valor aleatório inicial entre -10 e 10
  x = runif(n = 1, min = -10, max = 10)
  
  # Avalia o valor de x
  y = f(x)
  
  print(c(x, y))
  
  # Cada passo (iteração) do algoritmo
  #  é uma "Geração"
  for (i in 1:50) {
    
    # "perturba aleatoriamente o valor de x
    # Aplica uma "Mutação" sobre x
    # Cria, assim, uma nova solução
    x1 = x + runif(n = 1, min = -1, max = 1)
    y1 = f(x1)
  
    # "Seleção" da melhor solução
    # Se a nova solução é melhor, substitui a original
    if (y1 < y) {
      x = x1
      y = y1
    }
    
    print(c(x, y))
  }
}

### Estratégia Evolutiva do tipo (1,1)-EE
# Cada indivíduo gera um novo indivíduo
#  e esse novo substitui o anterior
#  (se for melhor)
# Versão 2 com parâmetros e população > 1
# Parâmetros:
# - tamanho da população: tam
# - número de gerações: gen
# - limite inferior para geração dos indivíduos: lb
# - limite superior para geração dos indivíduos: ub
ee2 <- function(tam = 1, gen = 10, lb = -10, ub = 10) {
  
  # x é um conjunto de soluções
  # x é uma "População"
  # Cada elemento de x é um "Indivíduo"
  
  # Gera uma população aleatória inicial entre -10 e 10
  x = runif(n = tam, min = lb, max = ub)
  
  # Avalia o valor de x
  y = f(x)
  
  #print(cat("x0 = ", x))
  #print(cat("y0 = ", y))
  print(min(y))
  
  # Cada passo (iteração) do algoritmo
  #  é uma "Geração"
  for (i in 1:gen) {
    
    # "perturba aleatoriamente o valor de x
    # Aplica uma "Mutação" sobre x
    # Cria, assim, uma nova solução
    # A perturbação varia entre -1 e 1
    x1 = x + runif(n = tam, min = -1, max = 1)
    y1 = f(x1)
    
    # "Seleção" da melhor solução
    # Se a menor solução da nova população é melhor,
    #  substitui a população original
    if (min(y1) < min(y)) {
      x = x1
      y = y1
    }
    
    #print(cat("x = ", x))
    #print(cat("y = ", y))
    print(min(y))
  }
}

### Testes:
ee2()
ee2(tam = 10, gen = 20)

### Estratégia Evolutiva do tipo (1+1)-EE
# Cada indivíduo gera um novo indivíduo
#  e esse novo substitui o anterior
#  (se for melhor)
# Versão 2 com parâmetros e população > 1
# Parâmetros:
# - tamanho da população: tam
# - número de gerações: gen
# - limite inferior para geração dos indivíduos: lb
# - limite superior para geração dos indivíduos: ub
ee2 <- function(tam = 1, gen = 10, lb = -10, ub = 10) {  
  # x é um conjunto de soluções
  # x é uma "População"
  # Cada elemento de x é um "Indivíduo"
  
  # Gera uma população aleatória inicial entre -10 e 10
  x = runif(n = tam, min = lb, max = ub)
  
  # Avalia o valor de x
  y = f(x)
  
  #print(cat("x0 = ", x))
  #print(cat("y0 = ", y))
  print(min(y))
  
  # Cada passo (iteração) do algoritmo
  #  é uma "Geração"
  for (i in 1:gen) {
    
    # "perturba aleatoriamente o valor de x
    # Aplica uma "Mutação" sobre x
    # Cria, assim, uma nova solução
    x1 = x + runif(n = tam, min = -1, max = 1)
    y1 = f(x1)
    
    # "Seleção" da melhor solução
    # Se a menor solução da nova população é melhor,
    #  substitui a população original
    if (min(y1) < min(y)) {
      for (j in 1:length(x)) {
        if (y1[j] < y[j]) {
          x[j] = x1[j]
          y[j] = y1[j]
        }
      }
    }
    
    #print(cat("x = ", x))
    #print(cat("y = ", y))
    
    print(min(y))
  }
}

ee2()
ee2(tam = 10, gen = 20)
