# Instalação e carregamento de Pacotes utilizados

pacotes <- c("tidytext","ggplot2","dplyr","tibble","wordcloud", "gutenbergr", "stopwords", "patchwork", "forcats", "RColorBrewer")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

library("dplyr")
library("gutenbergr")
library("tibble")
library("tidytext")
library("ggplot2")
library("patchwork")
library("wordcloud")
library("stopwords")
library("forcats")
library("RColorBrewer")

# Visualizando as informações disponíveis no Projeto Gutemberg (PG)
# Fonte: https://docs.ropensci.org/gutenbergr/reference/gutenberg_metadata.html
lista_geral <- gutenberg_metadata
lista_geral

# Filtrando livros em português (PT), por autor
lista_pt <- lista_geral %>% count(language, author, sort = T) %>% filter(language == "pt")
lista_pt

# Filtrando livros textuais em português, por autor  
lista_pt_txt <- lista_geral %>% count(language, author, has_text, sort = T) %>% filter(language == "pt" & has_text == T)
lista_pt_txt

# Criando uma tabela de dados filtrados pelo autor Machado de Assis (MA), em 
# português (pt), com texto
# Fonte: https://docs.ropensci.org/gutenbergr/reference/gutenberg_works.html

lista_MA <- gutenberg_works(
  author == "Machado de Assis",
  languages = "pt",
  only_text = TRUE)

lista_MA

# Para o TCC, vamos usar Dom casmurro, Helena e Braz Cubas; assim, criamos a 
# lista de livros do TCC fltrando os livros da "lista_MA" por título

lista_TCC <- lista_MA %>% filter((title == "Helena")|(title == "Dom Casmurro")|(grepl("Memorias", title)))
lista_TCC

# Aqui, baixamos os três livros numa única tabela
ID_Livros_TCC <- c(lista_TCC$gutenberg_id[1:nrow(lista_TCC)])

Livros_TCC <- gutenberg_download(ID_Livros_TCC, meta_fields = "title")
view (Livros_TCC)

# Identificando o ID de cada obra
for (i in 1:nrow(lista_TCC)) {
  if (grepl("Memorias",lista_TCC$title[i])){ 
    id_memorias <- (lista_TCC$gutenberg_id[i])     }
  if (grepl("Dom",lista_TCC$title[i])){ 
    id_dcasmurro <- (lista_TCC$gutenberg_id[i])     } 
  if (grepl("Helena",lista_TCC$title[i])){ 
    id_helena <- (lista_TCC$gutenberg_id[i])  }
}

# Separando os livros
lv_dcasm <- Livros_TCC %>% filter(gutenberg_id == id_dcasmurro)
lv_helena <- Livros_TCC %>% filter(gutenberg_id == id_helena)
lv_memorias <- Livros_TCC %>% filter(gutenberg_id == id_memorias)


# Alterando encoding para ASCII/Translit para remover acentos, facilitando 
# a manipulação dos dados 

lv_dcasm$text <- iconv (lv_dcasm$text, from = "ISO-8859-1", to = "ASCII//TRANSLIT")
lv_helena$text <- iconv(lv_helena$text, from = "", to = "ASCII//TRANSLIT")
lv_memorias$text <- iconv (lv_memorias$text, from = "ISO-8859-1", to = "ASCII//TRANSLIT")

# Removemos as primeiras linhas dos livros, onde constam informações sobre a 
# edição, livros do autor e índice

lv_dcasm <- lv_dcasm[-(2:21),]
lv_helena <- lv_helena[-(1:74),]
lv_memorias <- lv_memorias[-(6:39),]

# Transformando em tibble, com número da linha e texto
tb_dcasm <- tibble(line = 1:nrow(lv_dcasm), text = lv_dcasm$text)
tb_helena <- tibble(line = 1:nrow(lv_helena), text = lv_helena$text)
tb_memorias <- tibble(line = 1:nrow(lv_memorias), text = lv_memorias$text)

# tokenização dos livros, já convertidos em letras minúsculas, retirando a 
# pontuação (pelo próprio unnest) e, em seguida, números
df_dcasm <- tb_dcasm %>% unnest_tokens(word, text) %>% filter(!grepl('[0-9]', word)) 
view(df_dcasm)

df_helena <- tb_helena %>% unnest_tokens(word, text) %>% filter(!grepl('[0-9]', word)) 
view(df_helena)

df_memorias <- tb_memorias %>% unnest_tokens(word, text) %>% filter(!grepl('[0-9]', word)) 
view(df_memorias)

# Busca palavras mais comuns por meio da contagem simples, e visualização
conta_dcasm <- df_dcasm %>% count(word, sort = TRUE) 
conta_helena <- df_helena %>% count(word, sort = TRUE) 
conta_memorias <- df_memorias %>% count(word, sort = TRUE) 

g1<-conta_dcasm [1:20,] %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col(fill = "salmon") + 
  labs(subtitle = "Dom Casmurro", x = NULL, y = "Palavras")  

g2<-conta_helena [1:20,] %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col(fill = "seagreen3") + 
  labs(subtitle = "Helena", x = "Ocorrências", y = NULL)

g3<-conta_memorias [1:20,] %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col(fill = "skyblue3") + 
  labs(subtitle = "Memórias Póstumas de Brás Cubas", x = NULL, y = NULL)

g1+g2+g3

# Calculando o TF-IDF de cada palavra 
lv_join <- lv_dcasm %>% bind_rows(lv_helena) %>% bind_rows(lv_memorias)

book_words <- lv_join %>%  unnest_tokens(word, text) %>% 
  filter(!grepl('[0-9]', word)) %>%
  group_by(title) %>%
  count(title, word, sort = TRUE)

total_words <- book_words %>% 
  group_by(title) %>% 
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)

ggplot(book_words, aes(n/total, fill = title)) +
  geom_histogram(show.legend = FALSE, bins = 25) +
  xlim(NA, 0.0009) +
  facet_wrap(~title, ncol = 3, scales = "free_y") +
  theme_light()

# Os gráficos acima mostram a distribuição de frequência das palavras em cada obra
# ou seja, o número de vezes que uma palavra aparece, dividido pelo total de
# palavras. Confirma Lei de Zipf.

books_tf_idf <- book_words %>% bind_tf_idf(word, title, n)

books_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))

books_tf_idf %>%
  group_by(title) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = title)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~title, ncol = 3, scales = "free") +
  labs(x = "tf-idf", y = NULL) +
  theme_light()

# Nesta segunda parte, vamos excluir as stopwords e repetir as análises acima #
# Pesquisando dataframes de stopwords em Português 
# Fonte: (https://www.rdocumentation.org/packages/stopwords/versions/2.3)

my_stopwords <- stopwords::stopwords("pt", source = "stopwords-iso", simplify = FALSE)

my_stopwords <- iconv(my_stopwords, from = "UTF-8", to = "ASCII//TRANSLIT")
length(my_stopwords)

# Removendo duplicadas após remoção de acento (ex: e, é) e transforma em tibble
my_stopwords <- unique(my_stopwords)
my_stopwords <- tibble(i = 1:length((my_stopwords)), word = my_stopwords)

# Removendo stopwords dos df tokenizados, contando palavras e visualizando
df_dcasm_stw <- df_dcasm %>% anti_join(my_stopwords)
conta_dcasm_stw <- df_dcasm_stw %>%  count(word, sort = TRUE) 

df_helena_stw <- df_helena %>% anti_join(my_stopwords)
conta_helena_stw <- df_helena_stw %>%  count(word, sort = TRUE) 

df_memorias_stw <- df_memorias %>% anti_join(my_stopwords)
conta_memorias_stw <- df_memorias_stw %>%  count(word, sort = TRUE) 

g1_stw<-conta_dcasm_stw [1:20,] %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col(fill = "salmon") + 
  labs(subtitle = "Dom Casmurro", x = NULL, y = "Palavras") 

g2_stw<-conta_helena_stw [1:20,] %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col(fill = "seagreen3") + 
  labs(subtitle = "Helena", x = "Ocorrências", y = NULL)

g3_stw<-conta_memorias_stw [1:20,] %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col(fill = "skyblue3") + 
  labs(subtitle = "Memórias Póstumas de Brás Cubas", x = NULL, y = NULL)

g1_stw + g2_stw + g3_stw

# Calculando TF-IDF pós remoção das stopwords
lv_join_stw <- lv_dcasm %>% bind_rows(lv_helena) %>% bind_rows(lv_memorias)

book_words_stw <- lv_join_stw %>%  unnest_tokens(word, text) %>% 
  filter(!grepl('[0-9]', word)) %>%
  anti_join(my_stopwords) %>%
  group_by(title) %>%
  count(title, word, sort = TRUE)

total_words_stw <- book_words_stw %>% 
  group_by(title) %>% 
  summarize(total = sum(n))

book_words_stw <- left_join(book_words_stw, total_words_stw)

ggplot(book_words_stw, aes(n/total, fill = title)) +
  geom_histogram(bins = 25, show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~title, ncol = 3, scales = "free_y") +
  theme_light()

# Os gráficos acima mostram a distribuição de frequência das palavras em cada 
# obra pós remoção de stopwords. Confirma lei de Zipf.

books_tf_idf_stw <- book_words_stw %>% bind_tf_idf(word, title, n)

books_tf_idf_stw %>%
  select(-total) %>%
  arrange(desc(tf_idf))

books_tf_idf_stw %>%
  group_by(title) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = title)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~title, ncol = 3, scales = "free") +
  labs(x = "tf-idf", y = NULL) +
  theme_light()

# Os gráficos mostram as mesmas palavras antes e depois da remoção das stopwords

# Nuvens de palavras
# Definindo a paleta de cores
pal <- brewer.pal(8, "Set1")

# Configurando a área de plotagem para 3 colunas
par(mfrow = c(1, 3))

# Gerando as nuvens de palavras
conta_dcasm_stw %>% with(wordcloud(word, n, scale=c(4,.5), min.freq=10, rot.per=.25, use.r.layout=FALSE, random.order = FALSE, max.words = 50, colors = pal))
title("Dom Casmurro")

conta_helena_stw %>% with(wordcloud(word, n, scale=c(4,.5), min.freq=10, rot.per=.25, use.r.layout=FALSE, random.order = FALSE, max.words = 50, colors = pal))
title("Helena")

conta_memorias_stw %>% with(wordcloud(word, n, scale=c(4,.5), min.freq=10, rot.per=.25, use.r.layout=FALSE, random.order = FALSE, max.words = 50, colors = pal))
title("Memórias Póstumas de Brás Cubas")




