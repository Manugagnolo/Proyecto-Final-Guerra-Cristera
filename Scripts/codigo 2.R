# =============================================================================
# ANÁLISIS ROBUSTO DE TEXTO - DISCURSOS HISTÓRICOS (1926-1928)
# Análisis avanzado con limpieza exhaustiva y visualizaciones interpretativas
# =============================================================================

# Cargar paquetes necesarios
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
library(RColorBrewer)
library(wordcloud)
library(scales)
library(corrplot)
library(gridExtra)
library(lubridate)
library(tm)
library(SnowballC)

# =============================================================================
# 1. CARGA Y LIMPIEZA EXHAUSTIVA DE DATOS
# =============================================================================

# Cargar el índice con rutas y metadatos

docs_index <- read_csv("Data/Data/db/Originales/index1.csv")

# Función avanzada de limpieza de texto
limpiar_texto <- function(texto) {
  texto %>%
    # Convertir a minúsculas
    str_to_lower() %>%
    # Remover caracteres especiales pero conservar espacios y puntuación básica
    str_replace_all("[^\\p{L}\\s\\.]", " ") %>%
    # Normalizar espacios múltiples
    str_replace_all("\\s+", " ") %>%
    # Remover espacios al inicio y final
    str_trim() %>%
    # Remover líneas muy cortas (menos de 10 caracteres)
    ifelse(str_length(.) < 10, NA_character_, .) %>%
    # Remover texto que parece ser metadata o headers
    str_replace_all("^(página|pág|cap|capítulo).*", "") %>%
    # Normalizar acentos y caracteres especiales
    stri_trans_general("Latin-ASCII")
}

# Leer y limpiar contenido de archivos
# Crear función segura
safe_read_file <- safely(read_file)

# Usar dentro de map para capturar resultado y errores
docs_index <- docs_index %>%
  mutate(
    # Leer texto y manejar errores
    lectura = map(ubicacion, safe_read_file),
    texto_original = map_chr(lectura, ~ ifelse(is.null(.x$result), NA_character_, .x$result)),
    texto = map_chr(texto_original, limpiar_texto),
    longitud_original = str_length(texto_original),
    longitud_limpia = str_length(texto),
    num_palabras = str_count(texto, "\\S+"),
    texto_valido = !is.na(texto) & str_length(texto) > 50
  ) %>%
  filter(texto_valido) %>%
  select(-lectura)  # opcional: quitar la columna intermedia


# Estadísticas de limpieza
cat("=== ESTADÍSTICAS DE LIMPIEZA DE DATOS ===\n")
cat("Documentos cargados:", nrow(docs_index), "\n")
cat("Documentos válidos:", sum(docs_index$texto_valido), "\n")
cat("Promedio de palabras por documento:", round(mean(docs_index$num_palabras, na.rm = TRUE)), "\n")
cat("Reducción promedio de caracteres:", 
    round(100 * (1 - mean(docs_index$longitud_limpia / docs_index$longitud_original, na.rm = TRUE)), 1), "%\n\n")

# Calcular estadísticas
stats_limpieza <- tibble(
  Métrica = c(
    "Documentos cargados",
    "Documentos válidos",
    "Promedio de palabras por documento",
    "Reducción promedio de caracteres"
  ),
  Valor = c(
    nrow(docs_index),
    sum(docs_index$texto_valido),
    round(mean(docs_index$num_palabras, na.rm = TRUE)),
    paste0(round(100 * (1 - mean(docs_index$longitud_limpia / docs_index$longitud_original, na.rm = TRUE)), 1), "%")
  )
)

# Mostrar en reactable
reactable(stats_limpieza, striped = TRUE, highlight = TRUE, bordered = TRUE)


# =============================================================================
# 2. TOKENIZACIÓN AVANZADA CON MÚLTIPLES FILTROS
# =============================================================================

# Crear lista expandida de stopwords
stopwords_es_expandida <- c(
  stopwords("es"),
  # Stopwords específicas del contexto histórico
  c("don", "doña", "señor", "señora", "año", "años", "dia", "dias", 
    "mes", "meses", "vez", "veces", "asi", "aqui", "alli", "entonces",
    "ahora", "antes", "despues", "siempre", "nunca", "muy", "mas", "menos",
    "bien", "mal", "mejor", "peor", "grande", "pequeño", "nuevo", "viejo",
    "primer", "segundo", "tercero", "ultimo", "solo", "toda", "todo", "todos",
    "algunas", "algunos", "muchas", "muchos", "pocas", "pocos", "otra", "otro")
)

# Función de tokenización robusta
tokenizar_texto <- function(df, columna_texto) {
  df %>%
    unnest_tokens(word, {{columna_texto}}) %>%
    # Filtrar stopwords
    filter(!word %in% stopwords_es_expandida) %>%
    # Filtrar números puros
    filter(!str_detect(word, "^\\d+$")) %>%
    # Filtrar palabras muy cortas o muy largas
    filter(str_length(word) >= 3 & str_length(word) <= 15) %>%
    # Filtrar palabras que son solo caracteres repetidos
    filter(!str_detect(word, "^(.)\\1{2,}$")) %>%
    # Stemming básico para español
    mutate(word = wordStem(word, language = "spanish"))
}

# Tokenizar documentos
tokens <- tokenizar_texto(docs_index, texto) 
# =============================================================================
# 3. ANÁLISIS DE FRECUENCIAS Y DISTRIBUCIONES
# =============================================================================

# Frecuencias generales con estadísticas
freq_general <- tokens %>%
  count(word, sort = TRUE) %>%
  mutate(
    porcentaje = n / sum(n) * 100,
    acumulado = cumsum(porcentaje),
    rango = row_number()
  )

# Análisis de Zipf (ley de potencias)
freq_general %>%
  filter(rango <= 100) %>%
  ggplot(aes(x = log10(rango), y = log10(n))) +
  geom_point(alpha = 0.7, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Distribución de Zipf - Frecuencia de Palabras",
    subtitle = "Análisis de la ley de potencias en el corpus",
    x = "Log10(Rango)", y = "Log10(Frecuencia)"
  ) +
  theme_minimal()

# =============================================================================
# 4. ANÁLISIS TEMPORAL DETALLADO
# =============================================================================

# Palabras clave expandidas y categorizadas
keywords_categorias <- tribble(
  ~palabra,      ~categoria,
  # Religión y espiritualidad
  "religion",    "Religioso",
  "religioso",   "Religioso",
  "religios",    "Religioso",
  "iglesia",     "Religioso",
  "catolica",    "Religioso",
  "catolico",    "Religioso",
  "culto",       "Religioso",
  "sagrado",     "Religioso",
  "divino",      "Religioso",
  "cristiano",   "Religioso",
  "cristiana",   "Religioso",
  "fe",          "Religioso",
  
  # Autoridad y orden
  "orden",       "Autoridad",
  "autoridad",   "Autoridad",
  "obediencia",  "Autoridad",
  "disciplina",  "Autoridad",
  "jerarquia",   "Autoridad",
  "poder",       "Autoridad",
  
  # Conflicto y amenaza
  "amenaza",     "Conflicto",
  "enemigo",     "Conflicto",
  "rebelion",    "Conflicto",
  "revolucion",  "Conflicto",
  "guerra",      "Conflicto",
  "conflicto",   "Conflicto",
  
  # Moral y valores
  "pecado",      "Moral",
  "virtud",      "Moral",
  "moral",       "Moral",
  "honor",       "Moral",
  "deber",       "Moral",
  "justicia",    "Moral"
)%>%
  mutate(categoria = case_when(
    palabra %in% c("religion", "religios", "iglesia", "catolica", "catolico", 
                   "culto", "sagrado", "divino", "cristiano", "cristiana", "fe") ~ "Religioso",
    palabra %in% c("orden", "autoridad", "obediencia", "disciplina", "jerarquia", "poder") ~ "Autoridad",
    palabra %in% c("amenaza", "enemigo", "rebelion", "revolucion", "guerra", "conflicto") ~ "Conflicto",
    palabra %in% c("pecado", "virtud", "moral", "honor", "deber", "justicia") ~ "Moral",
    TRUE ~ "Otros"
  ))

# Análisis temporal por categorías
evolucion_temporal <- tokens %>%
  inner_join(keywords_categorias, by = c("word" = "palabra")) %>%
  count(anio, categoria, name = "frecuencia") %>%
  group_by(anio) %>%
  mutate(
    total_anio = sum(frecuencia),
    porcentaje = frecuencia / total_anio * 100
  ) %>%
  ungroup()

# Visualización de evolución temporal
p_temporal <- ggplot(evolucion_temporal, aes(x = anio, y = porcentaje, color = categoria)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_point(size = 3, alpha = 0.9) +
  facet_wrap(~categoria, scales = "free_y") +
  labs(
    title = "Evolución Temporal de Categorías Temáticas (1926-1928)",
    subtitle = "Porcentaje de menciones por año",
    x = "Año", y = "Porcentaje (%)",
    color = "Categoría"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_brewer(type = "qual", palette = "Set2")

print(p_temporal)

# =============================================================================
# 5. ANÁLISIS DE N-GRAMAS AVANZADO
# =============================================================================

# Función para análisis de n-gramas
analizar_ngramas <- function(df, n = 2, min_freq = 3) {
  df %>%
    unnest_tokens(ngram, texto, token = "ngrams", n = n) %>%
    separate(ngram, paste0("word", 1:n), sep = " ") %>%
    filter_at(vars(starts_with("word")), all_vars(!. %in% stopwords_es_expandida)) %>%
    filter_at(vars(starts_with("word")), all_vars(!str_detect(., "\\d+"))) %>%
    filter_at(vars(starts_with("word")), all_vars(str_length(.) > 2)) %>%
    unite(ngram, starts_with("word"), sep = " ") %>%
    count(ngram, sort = TRUE) %>%
    filter(n >= min_freq)
}

# Bigramas y trigramas
bigramas <- analizar_ngramas(docs_index, n = 2, min_freq = 2)
trigramas <- analizar_ngramas(docs_index, n = 3, min_freq = 2)

# Visualización de bigramas relevantes
bigramas %>%
  head(15) %>%
  ggplot(aes(x = reorder(ngram, n), y = n)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  coord_flip() +
  labs(
    title = "Bigramas Más Frecuentes",
    subtitle = "Combinaciones de dos palabras más comunes",
    x = "Bigrama", y = "Frecuencia"
  ) +
  theme_minimal()

# =============================================================================
# 6. ANÁLISIS DE SENTIMIENTOS ROBUSTO
# =============================================================================

# Diccionario de sentimientos expandido y contextualizado
nrc_es_expandido <- tribble(
  ~word, ~sentimiento, ~intensidad,
  # Emociones positivas
  "alegria", "positivo", 3, "felicidad", "positivo", 3, "gozo", "positivo", 3,
  "bendicion", "positivo", 2, "salvacion", "positivo", 3, "gloria", "positivo", 3,
  "paz", "positivo", 2, "virtud", "positivo", 2, "honor", "positivo", 2,
  "esperanza", "positivo", 2, "fe", "positivo", 2, "amor", "positivo", 3,
  
  # Emociones negativas
  "tristeza", "negativo", 2, "dolor", "negativo", 2, "pena", "negativo", 2,
  "ira", "negativo", 3, "odio", "negativo", 3, "rabia", "negativo", 3,
  "miedo", "negativo", 2, "terror", "negativo", 3, "amenaza", "negativo", 2,
  "enemigo", "negativo", 2, "pecado", "negativo", 2, "castigo", "negativo", 2,
  "rebelion", "negativo", 2, "guerra", "negativo", 3, "conflicto", "negativo", 2,
  
  # Autoridad y orden
  "orden", "autoridad", 2, "obediencia", "autoridad", 2, "disciplina", "autoridad", 2,
  "poder", "autoridad", 2, "jerarquia", "autoridad", 1, "autoridad", "autoridad", 3
)

# Análisis de sentimientos por año con ponderación
sentimiento_evolucion <- tokens %>%
  inner_join(nrc_es_expandido, by = "word") %>%
  group_by(anio, sentimiento) %>%
  summarise(
    frecuencia = n(),
    intensidad_promedio = mean(intensidad, na.rm = TRUE),
    score_ponderado = sum(intensidad),
    .groups = "drop"
  ) %>%
  group_by(anio) %>%
  mutate(
    porcentaje = frecuencia / sum(frecuencia) * 100,
    score_normalizado = score_ponderado / sum(score_ponderado) * 100
  )

# Visualización de evolución sentimental
p_sentimientos <- ggplot(sentimiento_evolucion, aes(x = anio, y = score_normalizado, fill = sentimiento)) +
  geom_area(alpha = 0.7) +
  labs(
    title = "Evolución del Tono Emocional en los Discursos (1926-1928)",
    subtitle = "Análisis de sentimientos ponderado por intensidad",
    x = "Año", y = "Score Normalizado (%)",
    fill = "Sentimiento"
  ) +
  theme_minimal() +
  scale_fill_brewer(type = "qual", palette = "Set3")

plotly::ggplotly(p_sentimientos)

# =============================================================================
# 7. ANÁLISIS DE COOCURRENCIA Y REDES
# =============================================================================

# Análisis de coocurrencia de palabras clave
coocurrencia <- tokens %>%
  filter(word %in% keywords_categorias$palabra) %>%
  pairwise_count(word, ubicacion, sort = TRUE) %>%
  filter(n >= 2)

# Correlación entre términos
correlacion_terminos <- tokens %>%
  filter(word %in% keywords_categorias$palabra) %>%
  pairwise_cor(word, ubicacion, sort = TRUE) %>%
  filter(abs(correlation) > 0.1)

# =============================================================================
# 8. ANÁLISIS ESTADÍSTICO AVANZADO
# =============================================================================

# Análisis de diversidad léxica por año
diversidad_lexica <- tokens %>%
  group_by(anio) %>%
  summarise(
    total_palabras = n(),
    palabras_unicas = n_distinct(word),
    ratio_diversidad = palabras_unicas / total_palabras,
    .groups = "drop"
  )

# Test de significancia para cambios temporales
if(length(unique(docs_index$anio)) > 1) {
  # Prueba chi-cuadrado para distribución de categorías por año
  tabla_contingencia <- evolucion_temporal %>%
    select(anio, categoria, frecuencia) %>%
    pivot_wider(names_from = categoria, values_from = frecuencia, values_fill = 0)
  
  if(nrow(tabla_contingencia) > 1) {
    chi_test <- chisq.test(tabla_contingencia[-1])
    cat("Test Chi-cuadrado para independencia temporal:\n")
    cat("p-value:", chi_test$p.value, "\n")
    cat("Estadístico:", chi_test$statistic, "\n\n")
  }
}

# =============================================================================
# 9. VISUALIZACIONES INTERPRETATIVAS AVANZADAS
# =============================================================================

# Mapa de calor de intensidad emocional
heatmap_data <- sentimiento_evolucion %>%
  select(anio, sentimiento, score_normalizado) %>%
  pivot_wider(names_from = sentimiento, values_from = score_normalizado, values_fill = 0)

if(ncol(heatmap_data) > 2) {
  # Crear matriz para heatmap
  matriz_sentimientos <- as.matrix(heatmap_data[-1])
  rownames(matriz_sentimientos) <- heatmap_data$anio
  
  # Heatmap con corrplot
  corrplot(cor(matriz_sentimientos), method = "color", type = "upper",
           title = "Correlación entre Dimensiones Emocionales por Año",
           mar = c(0,0,2,0))
}

# Nube de palabras por categoría
if(require(wordcloud, quietly = TRUE)) {
  # Crear nubes de palabras por categoría
  for(cat in unique(keywords_categorias$categoria)) {
    palabras_cat <- tokens %>%
      inner_join(keywords_categorias, by = c("word" = "palabra")) %>%
      filter(categoria == cat) %>%
      count(word, sort = TRUE)
    
    if(nrow(palabras_cat) > 0) {
      wordcloud(words = palabras_cat$word, freq = palabras_cat$n, 
                max.words = 50, random.order = FALSE, 
                colors = brewer.pal(8, "Dark2"),
                main = paste("Categoría:", cat))
    }
  }
}

# =============================================================================
# 10. RESUMEN EJECUTIVO E INTERPRETACIONES
# =============================================================================

cat("=== RESUMEN EJECUTIVO DEL ANÁLISIS ===\n\n")

cat("1. CALIDAD DE DATOS:\n")
cat("   - Documentos procesados:", nrow(docs_index), "\n")
cat("   - Tokens únicos:", length(unique(tokens$word)), "\n")
cat("   - Diversidad léxica promedio:", round(mean(diversidad_lexica$ratio_diversidad), 3), "\n\n")

cat("2. HALLAZGOS TEMPORALES:\n")
top_evolucion <- evolucion_temporal %>%
  group_by(categoria) %>%
  arrange(desc(porcentaje)) %>%
  slice_head(n = 1)

for(i in 1:nrow(top_evolucion)) {
  cat("   -", top_evolucion$categoria[i], "pico en", top_evolucion$anio[i], 
      "(", round(top_evolucion$porcentaje[i], 1), "%)\n")
}

cat("\n3. ANÁLISIS SENTIMENTAL:\n")
sentimiento_promedio <- sentimiento_evolucion %>%
  group_by(sentimiento) %>%
  summarise(promedio = mean(porcentaje), .groups = "drop") %>%
  arrange(desc(promedio))

cat("   - Tono predominante:", sentimiento_promedio$sentimiento[1], 
    "(", round(sentimiento_promedio$promedio[1], 1), "%)\n")

cat("\n4. BIGRAMAS MÁS RELEVANTES:\n")
top_bigramas <- head(bigramas, 5)
for(i in 1:nrow(top_bigramas)) {
  cat("   -", top_bigramas$ngram[i], "(", top_bigramas$n[i], "menciones)\n")
}

cat("\n=== FIN DEL ANÁLISIS ===\n")

# Preparar los datos del resumen ejecutivo
resumen <- tibble(
  Sección = c(
    "Calidad de datos",
    "Tokens únicos",
    "Diversidad léxica promedio",
    "Hallazgos temporales",
    "Tono predominante",
    "Bigramas más relevantes"
  ),
  Resumen = c(
    paste("Documentos procesados:", nrow(docs_index)),
    paste("Tokens únicos:", length(unique(tokens$word))),
    paste("Promedio:", round(mean(diversidad_lexica$ratio_diversidad), 3)),
    paste0(
      paste0(
        "- ", top_evolucion$categoria, " pico en ", top_evolucion$anio, 
        " (", round(top_evolucion$porcentaje, 1), "%)"
      ), 
      collapse = "<br>"
    ),
    paste(
      sentimiento_promedio$sentimiento[1], 
      "(", round(sentimiento_promedio$promedio[1], 1), "%)"
    ),
    paste0(
      paste0("- ", top_bigramas$ngram, " (", top_bigramas$n, " menciones)"), 
      collapse = "<br>"
    )
  )
)

# Mostrar con reactable
reactable(
  resumen,
  striped = TRUE,
  bordered = TRUE,
  highlight = TRUE,
  columns = list(
    Resumen = colDef(html = TRUE)  # Para soportar saltos de línea con <br>
  )
)


# Guardar resultados en archivo
resultados <- list(
  metadatos = docs_index,
  tokens = tokens,
  frecuencias = freq_general,
  evolucion_temporal = evolucion_temporal,
  sentimientos = sentimiento_evolucion,
  bigramas = bigramas,
  diversidad_lexica = diversidad_lexica
)

# save(resultados, file = "analisis_discursos_completo.RData")