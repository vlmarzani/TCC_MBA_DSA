##################################################################################
#                  INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS             #
##################################################################################
# Pacotes utilizados
# Fonte: Text Mining.Rproj - aula do prof Jeronymo, trocando pacote janeausten pelo projeto Gutemberg
# Fonte: https://docs.ropensci.org/gutenbergr/

pacotes <- c("tidytext","ggplot2","dplyr","tibble","wordcloud","stringr","SnowballC","widyr","gutenbergr", "readr", "stopwords", "quanteda")

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
library("wordcloud")

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

#view(lista_tcc)
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
#id_dcasmurro <- lista_tcc$gutenberg_id[lista_tcc$title == "Dom Casmurro"]
#id_helena <- lista_tcc$gutenberg_id[lista_tcc$title == "Helena"]

#tentando incluir memorias postumas no TCC
#id_memorias <- lista_tcc$gutenberg_id[lista_tcc$title == "Memorias Posthumas de Braz Cubas"]

#### TESTE DE BUSCAS DO ID POR PARTE DO TÍTULO> ficou bem melhor 
for (i in 1:nrow(lista_tcc)) {
     if (grepl("Memorias",lista_tcc$title[i])){ 
      id_memorias <- (lista_tcc$gutenberg_id[i])     }
     if (grepl("Dom",lista_tcc$title[i])){ 
      id_dcasmurro <- (lista_tcc$gutenberg_id[i])     } 
     if (grepl("Helena",lista_tcc$title[i])){ 
      id_helena <- (lista_tcc$gutenberg_id[i])  }
}


#baixa d casmurro e demais com a função gutemberg_download
lv_dcasm <- gutenberg_download(id_dcasmurro,
                               mirror = NULL,
                               strip = TRUE,
                               meta_fields = NULL,
                               verbose = TRUE,
                               files = NULL)

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

### mudar encoding para ASCII conforme recomendação prof. Jeronymo em aula. Essa
# mudança já faz a retirada dos acentos, facilitando a manipulação dos dados 
# (dá erro em Helena - não fazer)

Encoding(lv_dcasm$text) <- "ASCII"
lv_dcasm$text <- iconv (lv_dcasm$text, from = "ISO-8859-1", to = "UTF-8")
for (i in 1:nrow(lv_dcasm)) {
  lv_dcasm$text[i] <- iconv(lv_dcasm$text[i], to = "ASCII//TRANSLIT")
}

Encoding(lv_helena$text) <- "ASCII"
#lv_helena$text <- iconv(lv_helena$text, from = "ISO-8859-1", to = "UTF-8")
for (i in 1:nrow(lv_helena)) {
  lv_helena$text[i] <- iconv(lv_helena$text[i], to = "ASCII//TRANSLIT")
}

Encoding(lv_memorias$text) <- "ASCII"
lv_memorias$text <- iconv (lv_memorias$text, from = "ISO-8859-1", to = "UTF-8")
for (i in 1:nrow(lv_memorias)) {
  lv_memorias$text[i] <- iconv(lv_memorias$text[i], to = "ASCII//TRANSLIT")
}

#alterando o encode de Dom Casmurro, que veio com símbolos (conforme pesquisa no bing)
#funciona tb, mas preferi deixar em ASCII ao invés do UTF-8, conforme recomendado
#em aula de tex mining:
#lv_dcasm$text <- iconv (lv_dcasm$text, from = "ISO-8859-1", to = "UTF-8")
#lv_memorias$text <- iconv (lv_memorias$text, from = "ISO-8859-1", to = "UTF-8")

#contando quantas linhas tem d casmurro e transformando em tible
linhas_dcasm <- nrow(lv_dcasm)
tb_dcasm <- tibble(line = 1:linhas_dcasm, text = lv_dcasm$text)

#contando quantas linhas tem helena e transformando em tible
linhas_helena <- nrow(lv_helena)
tb_helena <- tibble(line = 1:linhas_helena, text = lv_helena$text)

#contando quantas linhas tem memórias e transformando o objeto em tible
linhas_memorias <- nrow(lv_memorias)
tb_memorias <- tibble(line = 1:linhas_memorias, text = lv_memorias$text)

# tokenização de d casmurro, já convertido em letras minúsculas e retirando a 
# pontuação (pelo próprio unnest). Em seguida, números e eventuais NA
df_dcasm <- tb_dcasm %>% unnest_tokens(word, text) %>% filter(!grepl('[0-9]', word)) 
#df_dcasm <- df_dcasm[complete.cases(df_dcasm$word), ] 
view(df_dcasm)

#tokenização de helena, já convertido em letras minúsculas e retirando a 
# pontuação (pelo próprio unnest). Em seguida, números e eventuais NA
df_helena <- tb_helena %>% unnest_tokens(word, text) %>% filter(!grepl('[0-9]', word)) 
#df_helena <- df_helena[complete.cases(df_helena$word), ]
view(df_helena)

#tokenização de memorias, já convertido em letras minúsculas e retirando a 
# pontuação (pelo próprio unnest). Em seguida, números e eventuais NA
df_memorias <- tb_memorias %>% unnest_tokens(word, text) %>% filter(!grepl('[0-9]', word)) 
#df_memorias <- df_memorias[complete.cases(df_memorias$word), ]
view(df_memorias)

# Busca palavras mais comuns
conta_dcasm <- df_dcasm %>% count(word, sort = TRUE) 
conta_helena <- df_helena %>% count(word, sort = TRUE) 
conta_memorias <- df_memorias %>% count(word, sort = TRUE) 

#grafico
#Para facilitar a visualização (FICOU HORRÍVEL!!!)
#df_dcasm %>%
#  count(word, sort = TRUE) %>%
#  mutate(word = reorder(word, n)) %>%
#  ggplot(aes(n, word)) +
#  geom_col() 

#Vamos retirar números JÁ RETIREI EM CIMA
#df_dcasm_s_num <- tb_dcasm %>%  unnest_tokens(word, text) %>% filter(!grepl('[0-9]', word))
#conta_dcasm_s_num <- df_dcasm_s_num %>%  count(word, sort = TRUE) 
#df_helena_s_num <- tb_helena %>%  unnest_tokens(word, text) %>% filter(!grepl('[0-9]', word))
#conta_helena_s_num <- df_helena_s_num %>%  count(word, sort = TRUE)
#df_memorias_s_num <- tb_memorias %>%  unnest_tokens(word, text) %>% filter(!grepl('[0-9]', word))
#conta_memorias_s_num <- df_memorias_s_num %>%  count(word, sort = TRUE)

#pesquisando dataframes de stopwords em Português 
#Fonte: (https://www.rdocumentation.org/packages/stopwords/versions/2.3)
#install.packages("stopwords")

#tirando as stopwords padrão dos livros
#stopwords::stopwords("pt", source = "stopwords-iso")
#stopwords::stopwords(language = "pt")
#get_stopwords(language = 'pt')

my_stopwords <- stopwords::stopwords(language = "pt", source = "stopwords-iso")

#Encoding(my_stopwords) <- "ASCII"
my_stopwords <- iconv(my_stopwords, from = "UTF-8", to = "ASCII//TRANSLIT")
length(my_stopwords)
#trazendo mais stop words prontas do git lopes  https://gist.github.com/alopes/5358189.js
lopes <- c('de',  'a',  'o',  'que',  'e',  'do',  'da',  'em',  'um',  'para',  'é',  'com',  'não',  'uma',  'os',  'no',  'se',  'na',  'por',  'mais',  'as',  'dos',  'como',  'mas',  'foi',  'ao',  'ele',  'das',  'tem',  'à',  'seu',  'sua',  'ou',  'ser',  'quando',  'muito',  'há',  'nos',  'já',  'está',  'eu',  'também',  'só',  'pelo',  'pela',  'até',  'isso',  'ela',  'entre',  'era',  'depois',  'sem',  'mesmo',  'aos',  'ter',  'seus',  'quem',  'nas',  'me',  'esse',  'eles',  'estão',  'você',  'tinha',  'foram',  'essa',  'num',  'nem',  'suas',  'meu',  'às',  'minha',  'têm',  'numa',  'pelos',  'elas',  'havia',  'seja',  'qual',  'será',  'nós',  'tenho',  'lhe',  'deles',  'essas',  'esses',  'pelas',  'este',  'fosse',  'dele',  'tu',  'te',  'vocês',  'vos',  'lhes',  'meus',  'minhas',  'teu',  'tua',  'teus',  'tuas',  'nosso',  'nossa',  'nossos',  'nossas',  'dela',  'delas',  'esta',  'estes',  'estas',  'aquele',  'aquela',  'aqueles',  'aquelas',  'isto',  'aquilo',  'estou',  'está',  'estamos',  'estão',  'estive',  'esteve',  'estivemos',  'estiveram',  'estava',  'estávamos',  'estavam',  'estivera',  'estivéramos',  'esteja',  'estejamos',  'estejam',  'estivesse',  'estivéssemos',  'estivessem',  'estiver',  'estivermos',  'estiverem',  'hei',  'há',  'havemos',  'hão',  'houve',  'houvemos',  'houveram',  'houvera',  'houvéramos',  'haja',  'hajamos',  'hajam',  'houvesse',  'houvéssemos',  'houvessem',  'houver',  'houvermos',  'houverem',  'houverei',  'houverá',  'houveremos',  'houverão',  'houveria',  'houveríamos',  'houveriam',  'sou',  'somos',  'são',  'era',  'éramos',  'eram',  'fui',  'foi',  'fomos',  'foram',  'fora',  'fôramos',  'seja',  'sejamos',  'sejam',  'fosse',  'fôssemos',  'fossem',  'for',  'formos',  'forem',  'serei',  'será',  'seremos',  'serão',  'seria',  'seríamos',  'seriam',  'tenho',  'tem',  'temos',  'tém',  'tinha',  'tínhamos',  'tinham',  'tive',  'teve',  'tivemos',  'tiveram',  'tivera',  'tivéramos',  'tenha',  'tenhamos',  'tenham',  'tivesse',  'tivéssemos',  'tivessem',  'tiver',  'tivermos',  'tiverem',  'terei',  'terá',  'teremos',  'terão',  'teria',  'teríamos',  'teriam')
Encoding(lopes) <- "ASCII"
length(lopes)
lopes <- iconv(lopes, from = "UTF-8", to = "ASCII//TRANSLIT")
lopes <- unique(lopes)
my_stw_lopes <- union(my_stopwords, lopes) #lopes está dentro de iso, pois o resultado
# da união tem o mesmo tamanho de my stop words

my_stopwords <- unique(my_stopwords)
my_stw_lopes <- unique(my_stw_lopes)

my_stw_lopes <- tibble(i = 1:length(my_stw_lopes), word = my_stw_lopes)
my_stopwords <- tibble(i = 1:length((my_stopwords)), word = my_stopwords)

df_dcasm_stw <- df_dcasm %>% anti_join(my_stopwords)
conta_dcasm_stw <- df_dcasm_stw %>%  count(word, sort = TRUE) 

df_helena_stw <- df_helena %>% anti_join(get_stopwords (language = 'pt'))
conta_helena_stw <- df_helena_stw %>%  count(word, sort = TRUE) 

df_memorias_stw <- df_memorias %>% anti_join(get_stopwords (language = 'pt'))
conta_memorias_stw <- df_memorias_stw %>%  count(word, sort = TRUE) 

#após o data-cleaning inicial (removendo números, espaços e símbolos/pontução)
#e o primeiro pré-processamento de retirada de stop words, vemos que algumas
#palavras possuem grafia arcaica (como ella aou invés de ela) e também precisam
#ser retiradas. Vamos fazer isso manualmente, mas antes vamos fazer nuvem de palavras

# define a paleta de cores
pal <- brewer.pal(8,"Dark2")

# word cloud
conta_dcasm_stw %>% with(wordcloud(word, n, random.order = FALSE, max.words = 50, colors=pal))

conta_helena_stw %>% with(wordcloud(word, n, random.order = FALSE, max.words = 50, colors=pal))

conta_memorias_stw %>% with(wordcloud(word, n, random.order = FALSE, max.words = 50, colors=pal))



#vamos retirar elle ella si della ia nao
my_stopwords <- 

##################################

###quanteda::char_edit(stopwords("en", source = "stopwords-iso"))

###my_stopwords <- quanteda::list_edit(stopwords("en", source = "stopwords-iso"))


###### retomar a leitura de https://www.rdocumentation.org/packages/stopwords/versions/2.3
##### para aprender a fazer minhas p´roprias stopwords. Mas antes é melhor dar 
#uma olhada em como ficaram os dfs, fazendo estatísticas das palavras que eles contém.


# Aula de Text Mining
# Vamos fazer o Bag of Words

# Data Cleaning

# Stop words





