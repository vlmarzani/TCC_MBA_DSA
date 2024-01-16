##################################################################################
#                  INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS             #
##################################################################################
# Pacotes utilizados
# Fonte: Text Mining.Rproj - aula do prof Jeronymo, trocando pacote janeausten pelo projeto Gutemberg
# Fonte: https://docs.ropensci.org/gutenbergr/

pacotes <- c("tidytext","ggplot2","dplyr","tibble","wordcloud","stringr","SnowballC","widyr","gutenbergr", "readr")

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
library("tidytext")
library("ggplot2")
library("tibble")
library("readr")

# Visualizando as informações disponíveis no Projeto Gutemberg (PG)
#Fonte: https://docs.ropensci.org/gutenbergr/reference/gutenberg_metadata.html
listageral <- gutenberg_metadata
listageral

#quantidade de livros em português (PT)
livros_pt <- listageral %>% count(language, sort = T)
livros_pt[livros_pt$language == "pt",]

#quantidade de livros textuais
livros_txt <- listageral %>% count(has_text, sort = T)
livros_txt[livros_txt$has_text == T,]

# Buscando uma tabela de dados filtrados pelo autor Machado de Assis (MA) em 
# português (pt) com texto
# Fonte: https://docs.ropensci.org/gutenbergr/reference/gutenberg_works.html

lista_tcc <- gutenberg_works(
  author == "Machado de Assis",
  languages = "pt",
  only_text = TRUE)

view(lista_tcc)
lista_tcc

# Encontramos 9 livros do MA em Português no PG. Vamos fazer o download de 
# todos, trazendo o título deles
# Fonte: https://docs.ropensci.org/gutenbergr/

#Livros_MA_todos <- gutenberg_works(
#  author == "Machado de Assis",
#  languages = "pt",
#  only_text = TRUE) %>%
#  gutenberg_download(meta_fields = "title")


#para o TCC, vamos usar dom casmurro e helena ou Braz cubas; assim, criamos a 
# lista de livros do TCC fltrando os livros da lista tcc por título

Livros_TCC <- lista_tcc %>% filter((title == "Helena")|(title == "Dom Casmurro")|(grepl("Memorias", title)))
Livros_TCC

#aqui, baixamos os dois livros numa única tabela - só pra ver como fica

#Livros_MA_TCC <- gutenberg_works(
#  author == "Machado de Assis",
#  title == "Helena"|title == "Dom Casmurro",
#  languages = "pt",
#  only_text = TRUE) %>%
#  gutenberg_download(meta_fields = "title")

#view(Livros_MA_TCC)

#busca o id gutemberg dos dois livros escolhidos
id_dcasmurro <- lista_tcc$gutenberg_id[lista_tcc$title == "Dom Casmurro"]
id_helena <- lista_tcc$gutenberg_id[lista_tcc$title == "Helena"]

#tentando incluir memorias postumas no TCC
#id_memorias <- lista_tcc$gutenberg_id[lista_tcc$title == "Memorias Posthumas de Braz Cubas"]

#### TESTE DE BUSCAS DO ID POR PARTE DO TÍTULO> 
for (i in 1:nrow(lista_tcc)) {
     if (grepl("Memorias",lista_tcc$title[i])){ 
      id_memorias <- (lista_tcc$gutenberg_id[i])     }
     if (grepl("Dom",lista_tcc$title[i])){ 
      id_dcasmurro <- (lista_tcc$gutenberg_id[i])     } 
     if (grepl("Helena",lista_tcc$title[i])){ 
      id_helena <- (lista_tcc$gutenberg_id[i])  }
}


#baixa d casmurro com a função gutemberg_download
lv_dcasm <- gutenberg_download(id_dcasmurro,
                               mirror = NULL,
                               strip = TRUE,
                               meta_fields = NULL,
                               verbose = TRUE,
                               files = NULL)

#alterando o encode de Dom Casmurro, que veio com símbolos :
lv_dcasm$text <- iconv (lv_dcasm$text, from = "ISO-8859-1", to = "UTF-8")

lv_helena <- gutenberg_download(id_helena,
                               mirror = NULL,
                               strip = TRUE,
                               meta_fields = NULL,
                               verbose = TRUE,
                               files = NULL)

lv_memorias <- gutenberg_download(id_memorias,
                                  mirror = NULL,
                                  strip = T,
                                  meta_fields = NULL,
                                  verbose = TRUE,
                                  files = NULL)

#alterando o encode de Memórias Póstumas, que veio com símbolos :
lv_memorias$text <- iconv (lv_memorias$text, from = "ISO-8859-1", to = "UTF-8")

#contando quantas linhas tem d casmurro
linhas_dcasm <- nrow(lv_dcasm)
tb_dcasm <- tibble(line = 1:linhas_dcasm, text = lv_dcasm$text)

df_dcasm <-  tb_dcasm %>% unnest_tokens(word, text)
