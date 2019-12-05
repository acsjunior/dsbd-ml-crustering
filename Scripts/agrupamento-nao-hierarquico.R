#-----------------------------------------------------------------------
# Pacotes.

library(corrplot)   # Diagramas de correlação.
ls("package:corrplot")

library(factoextra) # Funções para análise e visualização.
ls("package:factoextra")

library(NbClust)    # Número ótimo de grupos em agrupamento.
ls("package:NbClust")

library(cluster)    # Medidas de distância e algoritmos.
ls("package:cluster")

library(tidyverse)  # Manipulação e visualização de dados.

#-----------------------------------------------------------------------
# Importação e preparação dos dados.

# Baixa arquivo com os dados.
rm(list = objects())
url <- "http://leg.ufpr.br/~walmes/data/inmet-insolacao-total.txt"
tb <- read.table(url,
                 header = TRUE,
                 comment = "#",
                 sep = "\t",
                 dec = ",",
                 na.strings = "-",
                 stringsAsFactors = FALSE)
str(tb)

# Encurta os nomes.
names(tb) <- substr(names(tb), 1, 3)
dput(names(tb))

# Conta as localidades por estado.
sort(table(tb$UF), decreasing = TRUE)

# Cria a matriz X com valores mensais.
v <- c("Jan", "Fev", "Mar", "Abr",
       "Mai", "Jun", "Jul", "Ago",
       "Set", "Out", "Nov", "Dez")
X <- as.matrix(tb[, v])

# Mantém apenas casos completos.
u <- complete.cases(X)
X <- X[u, ]

# Acerta o nome das linhas.
rownames(X) <- sprintf("%s (%s)", tb$Nom[u], tb$UF[u])
head(X)

# Pares de diagramas de dispersão.
lattice::splom(X, as.matrix = TRUE)

# Diagrama de correlação.
layout(1)
cx <- cor(X)
corrplot(cx)

corrplot(cor(X),
         method = "number",
         type = "upper",
         tl.pos = "d",
         col = "black",
         cl.pos = "n")
corrplot(cor(X),
         add = TRUE,
         type = "lower",
         diag = FALSE,
         tl.pos = "n",
         cl.pos = "n")

# Desvio-padrão por mês (são muito próximos).
apply(X, MARGIN = 2, FUN = sd)

# Distribuição marginal de cada variável.
ggplot(data = stack(as.data.frame(X)),
       mapping = aes(x = values)) +
  # facet_wrap(facets = ~ind, scale = "free") +
  facet_wrap(facets = ~ind) +
  geom_density() +
  geom_rug()

# Distribuição marginal de cada variável.
ggplot(data = stack(as.data.frame(X)),
       mapping = aes(x = ind, y = values)) +
  geom_boxplot()

# Nova observação para ser alocada após ajuste.
X_new <- c(Jan = 207.5, Fev = 193.3, Mar = 203.9, Abr = 201.1,
           Mai = 217.0, Jun = 223.9, Jul = 247.6, Ago = 251.2,
           Set = 213.5, Out = 215.1, Nov = 213.6, Dez = 200.0)

#-----------------------------------------------------------------------
# Agrupamento não hierárquico com K-means usando K = 3.

# Número de grupos.
k <- 3

# Ajuste dos centróides
set.seed(123)
km <- kmeans(X, centers = k)
names(km)
# Agrupamento do K-means.
table(km$cluster)

# Pares de diagramas de dispersão.
lattice::splom(X,
               groups = km$cluster,
               as.matrix = TRUE)


# Gráfico do agrupamento na projeção dos componentes principais.
fviz_cluster(km,
             data = X,
             stand = FALSE,
             show.clust.cent = TRUE,
             geom = "point")


fviz_cluster(km,
             data = X,
             stand = FALSE,
             show.clust.cent = FALSE,
             repel = TRUE)

# Gráfico da silhueta.
layout(1)
D <- daisy(X, metric = "euclidean")
sil <- silhouette(km$cluster, dist = D)
str(sil)
plot(sil)

#-----------------------------------------------------------------------
# Sobre o número ótimo de grupos.

# Número ótimo de grupos para dois critérios.
fviz_nbclust(X, kmeans, method = "wss")
fviz_nbclust(X, kmeans, method = "silhouette")

# Vários critérios para a avaliação do número ótimo de grupos.
n_clus <- NbClust(data = X,
                  min.nc = 2,
                  max.nc = 10,
                  method = "kmeans")
layout(1)

names(n_clus)
t(n_clus$Best.nc)

#-----------------------------------------------------------------------

# Respostas
library(tidyverse)

# A maior proximidade entre centróides é entre o grupo 2 e 3
fviz_cluster(km,
             data = X,
             stand = FALSE,
             show.clust.cent = TRUE,
             geom = "point")

# Distância entre os centroides 1 e 2:
sqrt(sum((km$centers[1,] - km$centers[2,])^2))
dist(rbind(km$centers[1,],km$centers[2,]))

# Distância entre os centroides 1 e 3:
sqrt(sum((km$centers[1,] - km$centers[3,])^2))
dist(rbind(km$centers[1,],km$centers[3,]))

# Distância entre os centroides 2 e 3:
sqrt(sum((km$centers[2,] - km$centers[3,])^2))
dist(rbind(km$centers[2,],km$centers[3,]))


# Resposta: Verdadeira
# Os clusters 2 e 3 apresentam a menor distância entre centróides




# b. A observação com vetor de características representado no objeto X_new é alocada ao grupo 3.
X2 <- rbind(X, X_new)

set.seed(123)
k <- 3
km2 <- kmeans(X2, centers = k)
tail(km2$cluster, 1)

# Resposta: Falso
# X_new foi alocada ao grupo 1



# c. O tamanho do grupo onde está alocada Campo Grande (MS) é maior que o grupo onde está alocada Brasília (DF).

# Grupo Campo Grande (MS) = 1
# Size = 99
clusters <- as.data.frame(km$cluster)
clusters$city <- rownames(clusters)
clusters[clusters$city == "Campo Grande (MS)", ]
km$size
nrow(clusters[clusters$`km$cluster` == 1, ])

# Grupo Brasília (DF) = 2
# Size = 65
clusters[clusters$city == "Brasília (DF)", ]
km$size
nrow(clusters[clusters$`km$cluster` == 2, ])

# Resposta: Verdadeiro
# O cluster de Campo Grande tem tamanho 99, enquanto o cluster de Brasília tem tamanho 65



# d. O grupo com menor soma de quadrados dentro do grupo é o 1.

# Soma dos quadrados dentro do cluster:
km$withinss
min(km$withinss)

# Reposta: Falso
# O grupo 3 possui a menor soma dos quadrados


# e. O quociente entre a soma de quadrados total dentro de grupos (within SS) e a soma de quadrados total (total SS) é inferior a 40%.
km$tot.withinss / km$totss

# Resposta: Falso
# O quociente é superior a 50%




# f. Todas as localidades do Mato Grosso do Sul (MS) foram alocadas no mesmo grupo.

# MS
clusters_MS <- clusters %>%
  filter(substr(city, start = nchar(city)-3, stop = nchar(city)) == "(MS)")
table(clusters_MS$`km$cluster`)

# Reposta: Verdadeiro
# Todos os municípios de MS estão alocados no grupo 1



# g. As localidades Curitiba (PR) e Maringá (PR) foram alocadas no mesmo grupo.
clusters %>%
  filter(city == "Curitiba (PR)" | city == "Maringá (PR)")

# Reposta: Falso
# Curitiba foi alocada no grupo 3, enquanto Maringá foi alocada no grupo 1



# h. As localidades Dourados (MS) e Maringá (PR) foram alocadas no mesmo grupo.
clusters %>%
  filter(city == "Dourados (MS)" | city == "Maringá (PR)")

# Reposta: Verdadeiro
# Dourados e Maringá estão alocadas no grupo 1



