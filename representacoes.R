### Representação (ou Codificação) de indivíduos

# Indivíduos originais
x1 = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
y1 = c(0.3, 0.2, 0.3, 0.2, 0.3, 0.2, 0.3, 0.2, 0.3)

n = length(x1)

### Abordagem 1 - simulando a recombinação binária

# Seleciona um ponto de corte
# Executa uma troca de posições a partir do corte

x2 = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.2, 0.3, 0.2, 0.3)
y2 = c(0.3, 0.2, 0.3, 0.2, 0.3, 0.6, 0.7, 0.8, 0.9)


### Abordagem 2 - Recombinação Aritmética Simples de 1 Ponto

# Seleciona um ponto de corte
# Escolhe um valor de alpha
# Recombina o gene selecionado

x2 = x1
y2 = y1

# seleciona um valor aleatório entre 1 e n
p = sample(x = n, size = 1)

# alpha pode ser qualquer valor entre 0 e 1
alpha = 0.5

# cruza os genes no ponto de corte p
x2[p] = (alpha*y1[p]) + (1 - alpha)*x1[p]
y2[p] = (alpha*x1[p]) + (1 - alpha)*y1[p]

### Abordagem 3 - Recombinação Aritmética Simples de N Pontos

# Seleciona um ponto de corte p
# Escolhe um valor de alpha
# Recombina todos os gene do ponto p até o final da sequência

x2 = x1
y2 = y1

p = sample(x = seq(2,n), size = 1)
alpha = 0.5

for (i in 1:n) {
  x2[i] = (alpha*y1[i]) + (1 - alpha)*x1[i]
  y2[i] = (alpha*x1[i]) + (1 - alpha)*y1[i]
}


### Abordagem 4 - Recombinação completa (whole crossover)

# Escolhe um valor de alpha
# Recombina todos os genes

x2 = x1
y2 = y1

alpha = 0.5

x2 = alpha*y1 + (1-alpha)*x1
y2 = alpha*x1 + (1-alpha)*y1


### Mutação simples

# Seleciona um ponto de mutação p
# Escolhe o valor de beta [0,1]
# "Perturbe" o gene em p usando beta

p = sample(x = n, size = 1)
beta = 0.5

x2[p] = beta*x2[p]





