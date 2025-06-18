# Cargar paquetes
library(tidyverse)
library(readr)
library(tidytext)
library(stringr)
library(reactable)
library(ggplot2)
library(plotly)
library(textdata)
library(DT)
library(widyr)
library(stringi)
library(knitr)
library(stopwords)

# Cargar el índice con rutas y metadatos
docs_index <- read_csv("Data/Data/db/Originales/index.csv")

# Leer el contenido de cada archivo
docs_index <- docs_index %>%
  mutate(texto = map_chr(ubicacion, read_file))

# Vista previa
reactable(docs_index)
###############################
stopwords_es <- stopwords("es")

# Tokenización con stopwords en español
tokens <- docs_index %>%
  unnest_tokens(word, texto) %>%
  filter(!word %in% stopwords_es) %>%
  filter(!str_detect(word, "\\d+")) %>%
  filter(str_length(word) > 2)

# Ver frecuencia general
tokens %>%
  count(word, sort = TRUE) %>%
  head(20) %>%
  reactable()

##################
# Palabras clave
keywords <- c("religión", "iglesia", "católica", "orden", "amenaza", "enemigo", "rebelión")

tokens_frec <- tokens %>%
  filter(word %in% keywords) %>%
  count(anio, word, sort = TRUE)

# Gráfico con ggplot2
gg <- ggplot(tokens_frec, aes(x = word, y = n, fill = factor(anio))) +
  geom_col(position = "dodge") +
  labs(title = "Frecuencia de palabras clave por año",
       x = "Término", y = "Frecuencia") +
  theme_minimal()

ggplotly(gg)
####################

# Palabras clave relevantes para el análisis
keywords <- c("religión", "religioso", "iglesia", "católica", "culto",
              "orden", "amenaza", "enemigo", "rebelión", "pecado")

# Tokenizar bigramas
bigrams <- docs_index %>%
  unnest_tokens(bigram, texto, token = "ngrams", n = 2)

# Separar en dos palabras y filtrar stopwords y números
bigrams_separados <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(
    !word1 %in% stopwords_es,
    !word2 %in% stopwords_es,
    !str_detect(word1, "\\d+"),
    !str_detect(word2, "\\d+")
  )

# Filtrar bigramas que contengan al menos una palabra clave
bigrams_filtrados <- bigrams_separados %>%
  filter(word1 %in% keywords | word2 %in% keywords)

# Reunir bigramas y contar frecuencia
bigrams_frec <- bigrams_filtrados %>%
  unite(bigram, word1, word2, sep = " ") %>%
  count(bigram, sort = TRUE)

# Visualizar los bigramas relevantes
bigrams_frec %>% reactable::reactable()

################################
# Cargar lexicón NRC
# Diccionario NRC español simplificado
nrc_es <- tribble(
  ~word,           ~sentimiento,
  # Alegría
  "alegría",       "alegría",
  "felicidad",     "alegría",
  "gozo",          "alegría",
  "bendición",     "alegría",
  "salvación",     "alegría",
  
  # Tristeza
  "tristeza",      "tristeza",
  "pena",          "tristeza",
  "lamento",       "tristeza",
  "llanto",        "tristeza",
  "dolor",         "tristeza",
  
  # Ira
  "ira",           "ira",
  "odio",          "ira",
  "rabia",         "ira",
  "furia",         "ira",
  "rebelión",      "ira",
  
  # Miedo
  "miedo",         "miedo",
  "terror",        "miedo",
  "amenaza",       "miedo",
  "temor",         "miedo",
  "peligro",       "miedo",
  
  # Confianza
  "confianza",     "confianza",
  "fe",            "confianza",
  "orden",         "confianza",
  "obediencia",    "confianza",
  "autoridad",     "confianza",
  
  # Sorpresa
  "sorpresa",      "sorpresa",
  "asombro",       "sorpresa",
  "maravilla",     "sorpresa",
  
  # Anticipación
  "esperanza",     "anticipación",
  "promesa",       "anticipación",
  "anuncio",       "anticipación",
  
  # Positivo
  "paz",           "positivo",
  "virtud",        "positivo",
  "gloria",        "positivo",
  "resurrección",  "positivo",
  "milagro",       "positivo",
  
  # Negativo
  "enemigo",       "negativo",
  "castigo",       "negativo",
  "pecado",        "negativo",
  "condena",       "negativo",
  "maldad",        "negativo"
)


# Unir tokens con sentimientos
sentimiento_anio <- tokens %>%
  inner_join(nrc_es, by = c("word")) %>%
  count(anio, sentimiento) %>%
  group_by(anio) %>%
  mutate(porcentaje = n / sum(n))

# Visualizar emociones por año
gg_emocion <- ggplot(sentimiento_anio, aes(x = sentimiento, y = porcentaje, fill = factor(anio))) +
  geom_col(position = "dodge") +
  labs(title = "Tono emocional en los discursos (1926–1928)",
       x = "Emoción", y = "Porcentaje") +
  theme_minimal()

# Interactivo
plotly::ggplotly(gg_emocion)

