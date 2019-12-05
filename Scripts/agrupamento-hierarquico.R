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
# Importação e preparo dos dados.

# Limpa espaço de trabalho.
rm(list = objects())

# Dados do censo.
url <- "http://leg.ufpr.br/~walmes/data/Censo-Sinopse-PR.csv"
censo <- read.csv(url,
                  header = TRUE,
                  comment.char = "#",
                  stringsAsFactors = FALSE,
                  quote = "\"",
                  na.strings = "-")
str(censo)

# Filtra apenas para a população residente.
censo <- censo %>%
  filter(Nome == "População residente") %>%
  select(Localidade, X2010) %>%
  rename("População residente" = "X2010")
str(censo)

# Cadastro de empresas.
url <- "http://leg.ufpr.br/~walmes/data/Cadastro-Central-de-Empresas-PR.csv"
cce <- read.csv(url,
                header = TRUE,
                comment.char = "#",
                stringsAsFactors = FALSE,
                quote = "\"",
                na.strings = "-")
str(cce)

# Filtra para as variáveis de interesse.
u <- xtabs(~Nome, cce)
v <- names(u)[u == 399]

# Prepara a tabela com municípios nas linhas e morbidades nas colunas.
cce_2010 <- cce %>%
  filter(Nome %in% v) %>%
  mutate(Y = rowMeans(select(., X2008:X2010), na.rm = TRUE)) %>%
  select(Nome, Localidade, Y) %>%
  spread(key = "Nome", value = "Y")

# Junta com a população.
cce_2010 <- inner_join(cce_2010, censo)

# Dividir cada variável pela população residente.
cce_2010 <- cce_2010 %>%
  mutate_at(v[-4], ~ ./`População residente`)

# Transformar para escala log base 10 os valores padronizados pelo
# quociente em relação ao mínimo.
curve(log10((x + 10)/10), from = 0, to = 1000)

# Aplica a transformação.
tb <- cce_2010 %>%
  mutate_if(is.numeric,
            function(x) {
              m <- min(x[x > 0])
              log10((x + m)/m)
            })

# Cria a matriz.
X <- as.matrix(tb[, v])
rownames(X) <- tb$Localidade

# Pares de diagramas de dispersão.
lattice::splom(X, as.matrix = TRUE)

# Distribuição marginal de cada variável.
ggplot(data = stack(as.data.frame(X)),
       mapping = aes(x = values)) +
  facet_wrap(facets = ~ind, scale = "free") +
  geom_density() +
  geom_rug()

# Diagrama de correlação.
cx <- cor(X, use = "pairwise")
rownames(cx) <- colnames(cx) <- abbreviate(colnames(cx))
corrplot(cx)

# Desvio-padrão.
round(apply(X, MARGIN = 2, sd, na.rm = TRUE), digits = 4)

# Padroniza para mitigar efeito de escala.
X <- scale(X)

# Novos valores para terem a classe predita.
X_new <- c(`Número de empresas atuantes` = 1.27,
           `Pessoal ocupado` = 1.276,
           `Pessoal ocupado assalariado` = 1.308,
           `Salário médio mensal` = 1.143,
           `Salários e outras remunerações` = 1.373,
           `Unidades locais` = 1.267)

#-----------------------------------------------------------------------
# Agrupamento hierárquico.

# Distâncias entre cada par de observações.
dis <- dist(X, method = "euclidian")

# Número de grupos.
k <- 3

# Agrupamento hierárquico.
hcl <- hclust(dis, method = "complete")
# Classe, métodos e conteúdo.
class(hcl)
methods(class = "hclust")
names(hcl)

# Algumas variáveis que podem ser úteis.
head(hcl$height)
head(hcl$merge)

# Dendograma.
layout(1)
plot(hcl, hang = -1)

# Classificação das observações.
clus <- cutree(tree = hcl, k = k)
table(clus)

# Visualização com identificação dos grupos no dendrograma.
fviz_dend(hcl, k = k)

# Gráfico do agrupamento na projeção dos componentes principais.
fviz_cluster(list(data = X, cluster = clus))

# Gráfico da silhueta.
D <- daisy(X)
plot(silhouette(clus, D))

#-----------------------------------------------------------------------
# Classificando novas observações via k-NN.

# Usando k vizinhos mais próximos para a classificação.
class::knn(train = X,
           test = matrix(X_new, nrow = 1),
           cl = cutree(tree = hcl, k = k),
           k = 3,
           prob = TRUE)

#-----------------------------------------------------------------------
# Sobre o número ótimo de grupos.

# Número ótimo de grupos para dois critérios.
fviz_nbclust(X, hcut, method = "wss")
fviz_nbclust(X, hcut, method = "silhouette")

# Vários critérios para a avaliação do número ótimo de grupos.
n_clus <- NbClust(data = X,
                  min.nc = 2,
                  max.nc = 8,
                  method = "complete")
layout(1)

#-----------------------------------------------------------------------