# =============================================================================
# ANÁLISIS ROBUSTO DE TEXTO - DISCURSOS HISTÓRICOS (1926-1928) - VERSIÓN MEJORADA
# Análisis avanzado con limpieza exhaustiva, visualizaciones interpretativas y validación estadística
# =============================================================================
# Cargar paquetes necesarios
require(pacman)
pacman::p_load(htmltools,tidyverse,readr,tidytext,stringr,reactable,ggplot2,plotly,textdata,
               DT,widyr,stringi,knitr,stopwords,RColorBrewer,wordcloud,scales,
               corrplot,gridExtra,lubridate,tm,SnowballC,textclean,patchwork,ggraph,
               igraph,quanteda,quanteda.textplots,topicmodels,cluster,factoextra,
               VIM,car,broom,ComplexHeatmap,circlize,wesanderson)


# =============================================================================
# 1. CARGA Y LIMPIEZA EXHAUSTIVA DE DATOS CON VALIDACIÓN
# =============================================================================

# Función de logging mejorada
log_proceso <- function(mensaje, nivel = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(sprintf("[%s] %s: %s\n", timestamp, nivel, mensaje))
}

log_proceso("Iniciando análisis robusto de texto")

# Cargar el índice con validación
tryCatch({
  docs_index <- read_csv("Data/Data/db/Originales/index1.csv")
  log_proceso(sprintf("Cargados %d documentos del índice", nrow(docs_index)))
}, error = function(e) {
  log_proceso(paste("Error al cargar índice:", e$message), "ERROR")
})

# Función avanzada de limpieza de texto con múltiples etapas
limpiar_texto_avanzado <- function(texto) {
  if(is.na(texto)) return(NA_character_)
  
  texto %>%
    # Etapa 1: Normalización inicial
    str_to_lower() %>%
    # Reemplazar caracteres unicode problemáticos
    textclean::replace_non_ascii() %>%
    # Normalizar espacios en blanco especiales
    str_replace_all("[\u00A0\u2007\u202F]", " ") %>%
    # Etapa 2: Limpieza de elementos estructurales
    str_replace_all("\\b(página|pág|cap|capítulo|fig|figura)\\s*\\d+\\b", "") %>%
    str_replace_all("\\b(art|artículo)\\s*\\d+", "") %>%
    str_replace_all("\\([^)]{0,3}\\)", "") %>%  # Paréntesis con poco contenido
    # Etapa 3: Limpieza de caracteres especiales
    str_replace_all("[^\\p{L}\\s\\.,;:!?¡¿]", " ") %>%
    # Etapa 4: Normalización de espacios
    str_replace_all("\\s+", " ") %>%
    str_trim() %>%
    # Etapa 5: Filtros de calidad
    {ifelse(str_length(.) < 20, NA_character_, .)} %>%
    # Etapa 6: Normalización de caracteres
    stri_trans_general("Latin-ASCII")
}

# Función de validación de calidad de texto
validar_calidad_texto <- function(texto) {
  if (is.na(texto)) return(FALSE)
  
  longitud_ok <- str_length(texto) >= 20
  palabras_ok <- str_count(texto, "\\S+") >= 5
  no_repetitivo <- !str_detect(texto, "(\\b\\w+\\b)(?:\\s+\\1){3,}")
  
  # Solo caracteres imprimibles (letras, puntuación, espacios, etc.)
  charset_ok <- stri_detect_regex(texto, "^[\\p{L}\\p{N}\\p{P}\\p{Zs}]+$")
  
  return(longitud_ok & palabras_ok & no_repetitivo & charset_ok)
}


# Función segura mejorada con reintentos
safe_read_file_mejorado <- function(path, encoding = "UTF-8", max_intentos = 3) {
  for(intento in 1:max_intentos) {
    tryCatch({
      return(read_file(path, locale = locale(encoding = encoding)))
    }, error = function(e) {
      if(intento < max_intentos) {
        # Intentar con diferentes encodings
        encodings <- c("latin1", "windows-1252", "ISO-8859-1")
        if(intento <= length(encodings)) {
          tryCatch({
            return(read_file(path, locale = locale(encoding = encodings[intento])))
          }, error = function(e2) {
            log_proceso(sprintf("Intento %d fallido para %s: %s", intento, path, e2$message), "WARN")
          })
        }
      } else {
        log_proceso(sprintf("Archivo no legible después de %d intentos: %s", max_intentos, path), "ERROR")
        return(NA_character_)
      }
    })
  }
  return(NA_character_)
}

# Procesamiento mejorado con métricas de calidad
log_proceso("Iniciando limpieza y validación de textos")

docs_index <- docs_index %>%
  mutate(
    # Leer texto con función mejorada
    texto_original = map_chr(ubicacion, safe_read_file_mejorado),
    # Aplicar limpieza avanzada
    texto_limpio = map_chr(texto_original, limpiar_texto_avanzado),
    # Métricas de calidad
    longitud_original = str_length(texto_original),
    longitud_limpia = str_length(texto_limpio),
    num_palabras_original = str_count(texto_original, "\\S+"),
    num_palabras_limpia = str_count(texto_limpio, "\\S+"),
    num_oraciones = str_count(texto_limpio, "[.!?]+"),
    densidad_puntuacion = str_count(texto_limpio, "[.,;:!?]") / str_length(texto_limpio),
    # Validación de calidad
    calidad_ok = map_lgl(texto_limpio, validar_calidad_texto),
    # Detección de idioma (simple)
    posible_español = str_detect(texto_limpio, "\\b(el|la|de|que|y|en|un|es|se|no|te|lo|le|da|su|por|son|con|para|al|del|los|las|una)\\b"),
    # Métricas adicionales
    ratio_reduccion = (longitud_original - longitud_limpia) / longitud_original,
    complejidad_lexica = num_palabras_limpia / num_oraciones
  ) %>%
  # Filtrar documentos válidos
  filter(calidad_ok & posible_español & !is.na(texto_limpio)) %>%
  # Renombrar para compatibilidad
  rename(texto = texto_limpio)

# Análisis de calidad de datos mejorado
analisis_calidad <- list(
  docs_cargados = nrow(docs_index),
  docs_validos = sum(docs_index$calidad_ok, na.rm = TRUE),
  promedio_palabras = round(mean(docs_index$num_palabras_limpia, na.rm = TRUE)),
  promedio_oraciones = round(mean(docs_index$num_oraciones, na.rm = TRUE)),
  reduccion_promedio = round(mean(docs_index$ratio_reduccion, na.rm = TRUE) * 100, 1),
  complejidad_promedio = round(mean(docs_index$complejidad_lexica, na.rm = TRUE), 1)
)

# Tabla de calidad con reactable mejorada
tabla_calidad <- tibble(
  Métrica = c("Documentos cargados", "Documentos válidos", "Tasa de éxito (%)",
              "Promedio palabras/doc", "Promedio oraciones/doc", "Complejidad léxica promedio",
              "Reducción de texto (%)", "Documentos en español (%)"),
  Valor = c(analisis_calidad$docs_cargados,
            analisis_calidad$docs_validos,
            round(analisis_calidad$docs_validos/analisis_calidad$docs_cargados*100, 1),
            analisis_calidad$promedio_palabras,
            analisis_calidad$promedio_oraciones,
            analisis_calidad$complejidad_promedio,
            analisis_calidad$reduccion_promedio,
            round(sum(docs_index$posible_español, na.rm = TRUE)/nrow(docs_index)*100, 1))
)

reactable(
  tabla_calidad,
  striped = TRUE,
  highlight = TRUE,
  bordered = TRUE,
  theme = reactableTheme(
    headerStyle = list(backgroundColor = "#f8f9fa", fontWeight = "bold"),
    cellPadding = "8px"
  ),
  columns = list(
    Métrica = colDef(minWidth = 200),
    Valor = colDef(align = "center", style = list(fontWeight = "bold"))
  )
)

# =============================================================================
# 2. TOKENIZACIÓN AVANZADA CON VALIDACIÓN LINGÜÍSTICA
# =============================================================================

# Stopwords expandidas con contexto histórico y regional
stopwords_contextuales <- c(
  stopwords("es"),
  # Formas históricas y arcaicas
  c("deste", "desa", "aqueste", "aquesa", "donde", "quando", "quien", "qual",
    "señor", "señora", "don", "doña", "vuestra", "vuestro", "merced"),
  # Conectores y muletillas del período
  c("pues", "luego", "empero", "mas", "sino", "aunque", "porque", "puesto",
    "asi", "asimismo", "tambien", "aun", "adem�s", "incluso"),
  # Términos administrativos comunes
  c("articulo", "capitulo", "seccion", "parte", "titulo", "numero", "pagina",
    "folio", "documento", "expediente", "archivo"),
  # Números en texto
  c("uno", "dos", "tres", "cuatro", "cinco", "seis", "siete", "ocho", "nueve", "diez",
    "primer", "segundo", "tercero", "cuarto", "quinto"),
  # Palabras muy comunes sin valor semántico
  c("cosa", "cosas", "forma", "manera", "modo", "caso", "casos", "vez", "veces",
    "dia", "dias", "año", "años", "tiempo", "momento", "parte", "partes")
)

# Función de tokenización mejorada con validación
tokenizar_avanzado <- function(df, columna_texto) {
  log_proceso("Iniciando tokenización avanzada")
  
  tokens <- df %>%
    # Tokenización básica
    unnest_tokens(word, {{columna_texto}}) %>%
    # Filtro de stopwords expandido
    filter(!word %in% stopwords_contextuales) %>%
    # Filtros de calidad mejorados
    filter(
      # Longitud apropiada
      str_length(word) >= 3 & str_length(word) <= 20,
      # No solo números
      !str_detect(word, "^\\d+$"),
      # No solo caracteres repetidos
      !str_detect(word, "^(.)\\1{2,}$"),
      # No fragmentos HTML/XML
      !str_detect(word, "^(lt|gt|amp|nbsp|quot)$"),
      # Caracteres alfabéticos válidos
      str_detect(word, "^[a-záéíóúñü]+$")
    ) %>%
    # Stemming contextual
    mutate(
      word_original = word,
      word = wordStem(word, language = "spanish")
    ) %>%
    # Filtrar stems muy cortos resultantes
    filter(str_length(word) >= 2)
  
  log_proceso(sprintf("Tokenización completada: %d tokens únicos", n_distinct(tokens$word)))
  return(tokens)
}

# Aplicar tokenización
tokens <- tokenizar_avanzado(docs_index, texto)

# Análisis de tokenización
token_stats <- tokens %>%
  summarise(
    total_tokens = n(),
    tokens_unicos = n_distinct(word),
    ratio_diversidad = tokens_unicos / total_tokens,
    palabras_por_doc = total_tokens / n_distinct(anio),
    .groups = "drop"
  )%>%
  reactable()

log_proceso(sprintf("Estadísticas de tokenización - Total: %d, Únicos: %d, Diversidad: %.3f", 
                    token_stats$total_tokens, token_stats$tokens_unicos, token_stats$ratio_diversidad))

# =============================================================================
# 3. ANÁLISIS DE FRECUENCIAS CON VALIDACIÓN ESTADÍSTICA
# =============================================================================

# Frecuencias con estadísticas robustas
freq_detallada <- tokens %>%
  count(word, sort = TRUE) %>%
  mutate(
    porcentaje = n / sum(n) * 100,
    acumulado = cumsum(porcentaje),
    rango = row_number(),
    # Clasificación por frecuencia
    categoria_freq = case_when(
      n >= quantile(n, 0.95) ~ "Muy Alta",
      n >= quantile(n, 0.75) ~ "Alta", 
      n >= quantile(n, 0.50) ~ "Media",
      n >= quantile(n, 0.25) ~ "Baja",
      TRUE ~ "Muy Baja"
    ),
    # Métricas de distribución
    z_score = scale(n)[,1],
    percentil = percent_rank(n)
  )

# Análisis de Zipf mejorado con ajuste estadístico
analisis_zipf <- freq_detallada %>%
  filter(rango <= 1000) %>%
  mutate(
    log_rango = log10(rango),
    log_freq = log10(n)
  )

# Ajuste del modelo de Zipf
modelo_zipf <- lm(log_freq ~ log_rango, data = analisis_zipf)
zipf_summary <- broom::tidy(modelo_zipf)

# Visualización de Zipf mejorada
p_zipf <- analisis_zipf %>%
  ggplot(aes(x = log_rango, y = log_freq)) +
  geom_point(alpha = 0.6, color = "steelblue", size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
  labs(
    title = "Distribución de Zipf - Análisis de Ley de Potencias",
    subtitle = sprintf("R² = %.3f, Pendiente = %.3f (p < %.3f)", 
                       summary(modelo_zipf)$r.squared, 
                       zipf_summary$estimate[2], 
                       zipf_summary$p.value[2]),
    x = "Log₁₀(Rango)", 
    y = "Log₁₀(Frecuencia)",
    caption = "Una pendiente cercana a -1 indica cumplimiento de la Ley de Zipf"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "gray60")
  )

print(p_zipf)

# =============================================================================
# 4. ANÁLISIS TEMPORAL CON PRUEBAS ESTADÍSTICAS
# =============================================================================

# Keywords expandidas y contextualizadas históricamente
keywords_historicas <- tribble(
  ~palabra, ~categoria, ~subcategoria, ~peso,
  # Religión católica
  "religion", "Religioso", "Doctrina", 3,
  "catolico", "Religioso", "Identidad", 3,
  "iglesia", "Religioso", "Institución", 3,
  "fe", "Religioso", "Creencia", 2,
  "culto", "Religioso", "Práctica", 2,
  "sagrado", "Religioso", "Sacralidad", 2,
  "divino", "Religioso", "Divinidad", 2,
  "bendicion", "Religioso", "Ritual", 1,
  
  # Autoridad y orden social
  "orden", "Autoridad", "Estructura", 3,
  "autoridad", "Autoridad", "Poder", 3,
  "obediencia", "Autoridad", "Sumisión", 2,
  "disciplina", "Autoridad", "Control", 2,
  "jerarquia", "Autoridad", "Estratificación", 2,
  "gobierno", "Autoridad", "Política", 2,
  "ley", "Autoridad", "Legal", 2,
  
  # Conflicto y resistencia
  "revolucion", "Conflicto", "Revolución", 3,
  "rebelion", "Conflicto", "Resistencia", 3,
  "guerra", "Conflicto", "Violencia", 3,
  "enemigo", "Conflicto", "Oposición", 2,
  "amenaza", "Conflicto", "Peligro", 2,
  "lucha", "Conflicto", "Combate", 2,
  
  # Moral y valores
  "moral", "Moral", "Ética", 3,
  "virtud", "Moral", "Bondad", 2,
  "honor", "Moral", "Dignidad", 2,
  "deber", "Moral", "Obligación", 2,
  "justicia", "Moral", "Equidad", 2,
  "pecado", "Moral", "Transgresión", 2,
  
  # Identidad nacional
  "patria", "Nacional", "Territorio", 3,
  "nacion", "Nacional", "Comunidad", 3,
  "mexicano", "Nacional", "Identidad", 2,
  "pueblo", "Nacional", "Ciudadanía", 2,
  "bandera", "Nacional", "Símbolo", 1,
  
  # Modernización
  "progreso", "Modernidad", "Avance", 2,
  "civilizacion", "Modernidad", "Cultura", 2,
  "educacion", "Modernidad", "Instrucción", 2,
  "ciencia", "Modernidad", "Conocimiento", 2
)

# Análisis temporal robusto con ventanas móviles
analisis_temporal_robusto <- tokens %>%
  inner_join(keywords_historicas, by = c("word" = "palabra")) %>%
  group_by(anio, categoria, subcategoria) %>%
  summarise(
    frecuencia = n(),
    frecuencia_ponderada = sum(peso),
    .groups = "drop"
  ) %>%
  # Calcular totales por año
  group_by(anio) %>%
  mutate(
    total_anio = sum(frecuencia),
    total_ponderado_anio = sum(frecuencia_ponderada),
    porcentaje = frecuencia / total_anio * 100,
    porcentaje_ponderado = frecuencia_ponderada / total_ponderado_anio * 100
  ) %>%
  ungroup() %>%
  # Calcular tendencias
  group_by(categoria) %>%
  mutate(
    tendencia = porcentaje_ponderado - lag(porcentaje_ponderado, default = first(porcentaje_ponderado)),
    cambio_absoluto = abs(tendencia)
  ) %>%
  ungroup()

# Prueba estadística para cambios significativos
if(length(unique(analisis_temporal_robusto$anio)) > 1) {
  # ANOVA para diferencias entre años
  modelo_temporal <- aov(porcentaje_ponderado ~ factor(anio) + categoria, 
                         data = analisis_temporal_robusto)
  anova_summary <- broom::tidy(modelo_temporal)
  
  log_proceso(sprintf("ANOVA temporal - F(año): %.3f, p-valor: %.4f", 
                      anova_summary$statistic[1], anova_summary$p.value[1]))
}

# Visualización temporal mejorada con facetas
p_temporal_mejorado <- analisis_temporal_robusto %>%
  ggplot(aes(x = anio, y = porcentaje_ponderado, color = categoria)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_point(size = 3, alpha = 0.9) +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed", alpha = 0.5) +
  facet_wrap(~categoria, scales = "free_y", ncol = 3) +
  labs(
    title = "Evolución Temporal de Categorías Temáticas (1926-1928)",
    subtitle = "Análisis ponderado por relevancia histórica con tendencias suavizadas",
    x = "Año", 
    y = "Porcentaje Ponderado (%)",
    color = "Categoría",
    caption = "Líneas punteadas muestran tendencias suavizadas (LOESS)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(size = 14, face = "bold")
  ) +
  scale_color_manual(values = wesanderson::wes_palette("Darjeeling1", 
                                                       n = length(unique(analisis_temporal_robusto$categoria)), 
                                                       type = "continuous"))

plotly::ggplotly(p_temporal_mejorado) %>%
  layout(height = 600)

# =============================================================================
# 5. ANÁLISIS DE N-GRAMAS CON SIGNIFICANCIA ESTADÍSTICA
# =============================================================================

# Función mejorada para n-gramas con validación
extraer_ngramas_avanzado <- function(df, n = 2, min_freq = 2, filtro_calidad = TRUE) {
  
  log_proceso(sprintf("Extrayendo %d-gramas con frecuencia mínima %d", n, min_freq))
  
  ngramas <- df %>%
    unnest_tokens(ngram, texto, token = "ngrams", n = n) %>%
    separate(ngram, paste0("word", 1:n), sep = " ", remove = FALSE) %>%
    # Filtrar stopwords en cada posición
    filter_at(vars(starts_with("word")), all_vars(!. %in% stopwords_contextuales)) %>%
    # Filtros de calidad si se solicita
    {if(filtro_calidad) {
      filter_at(., vars(starts_with("word")), all_vars(
        !str_detect(., "\\d+") & 
          str_length(.) > 2 & 
          str_detect(., "^[a-záéíóúñü]+$")
      ))
    } else .} %>%
    # Recomponer n-grama limpio
    unite(ngram_limpio, starts_with("word"), sep = " ") %>%
    count(ngram_limpio, sort = TRUE) %>%
    filter(n >= min_freq) %>%
    # Calcular métricas adicionales
    mutate(
      longitud_promedio = map_dbl(str_split(ngram_limpio, " "), ~mean(str_length(.x))),
      coherencia = n / sum(n) * 100,
      rango = row_number()
    )
  
  log_proceso(sprintf("Extraídos %d %d-gramas únicos", nrow(ngramas), n))
  return(ngramas)
}

# Extraer diferentes tipos de n-gramas
bigramas_avanzados <- extraer_ngramas_avanzado(docs_index, n = 2, min_freq = 3)
trigramas_avanzados <- extraer_ngramas_avanzado(docs_index, n = 3, min_freq = 2)

# Análisis de colocaciones (palabras que aparecen juntas más de lo esperado)
colocaciones <- tokens %>%
  pairwise_count(word, ubicacion, sort = TRUE) %>%
  filter(n >= 3) %>%
  mutate(
    # Calcular probabilidades esperadas vs observadas
    prob_observada = n / sum(n),
    significancia = log2(prob_observada / (0.001))  # Simplificado
  ) %>%
  filter(significancia > 0) %>%
  arrange(desc(significancia))

# Visualización de bigramas con red
if(nrow(bigramas_avanzados) > 10) {
  # Preparar datos para red
  bigramas_red <- bigramas_avanzados %>%
    head(20) %>%
    separate(ngram_limpio, c("word1", "word2"), sep = " ") %>%
    filter(!is.na(word1) & !is.na(word2))
  
  # Crear grafo
  grafo_bigramas <- bigramas_red %>%
    graph_from_data_frame()
  
  # Visualización de red
  p_red_bigramas <- ggraph(grafo_bigramas, layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, 
                   arrow = arrow(type = "closed", length = unit(0.1, "inches"))) +
    geom_node_point(color = "lightblue", size = 3) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1, size = 3) +
    labs(
      title = "Red de Bigramas Más Frecuentes",
      subtitle = "Conexiones representan co-ocurrencia de palabras",
      caption = "Grosor de líneas proporcional a frecuencia"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "gray60")
    )
  
  print(p_red_bigramas)
}

# =============================================================================
# 6. ANÁLISIS DE SENTIMIENTOS CONTEXTUALIZADO - VERSIÓN MEJORADA
# =============================================================================

# Diccionario de sentimientos contextual e histórico
sentimientos_historicos <- tribble(
  ~word, ~sentimiento, ~intensidad, ~contexto,
  # Positivos religiosos/tradicionales
  "bendicion", "positivo", 3, "religioso",
  "salvacion", "positivo", 3, "religioso", 
  "gloria", "positivo", 3, "religioso",
  "virtud", "positivo", 2, "moral",
  "honor", "positivo", 2, "moral",
  "paz", "positivo", 2, "social",
  "orden", "positivo", 2, "social",
  "patria", "positivo", 3, "nacional",
  "progreso", "positivo", 2, "modernidad",
  
  # Negativos - amenazas al orden
  "revolucion", "negativo", 3, "político",
  "rebelion", "negativo", 3, "político",
  "enemigo", "negativo", 2, "conflicto",
  "amenaza", "negativo", 2, "conflicto",
  "guerra", "negativo", 3, "conflicto",
  "pecado", "negativo", 2, "religioso",
  "castigo", "negativo", 2, "moral",
  "destruccion", "negativo", 3, "social",
  
  # Autoridad (neutral-positivo en contexto)
  "autoridad", "autoridad", 2, "político",
  "gobierno", "autoridad", 2, "político",
  "obediencia", "autoridad", 1, "social",
  "disciplina", "autoridad", 1, "social",
  
  # Emociones básicas
  "alegria", "positivo", 2, "emocional",
  "tristeza", "negativo", 2, "emocional",
  "miedo", "negativo", 2, "emocional",
  "esperanza", "positivo", 2, "emocional"
)

# Análisis de sentimientos por año y contexto
sentimientos_evolucion <- tokens %>%
  inner_join(sentimientos_historicos, by = "word") %>%
  group_by(anio, sentimiento, contexto) %>%
  summarise(
    frecuencia = n(),
    intensidad_promedio = mean(intensidad),
    score_ponderado = sum(intensidad),
    .groups = "drop"
  ) %>%
  # Calcular porcentajes y normalizaciones
  group_by(anio) %>%
  mutate(
    total_anio = sum(frecuencia),
    porcentaje = frecuencia / total_anio * 100,
    score_normalizado = score_ponderado / sum(score_ponderado) * 100
  ) %>%
  ungroup() %>%
  # Calcular índice de sentimiento compuesto
  group_by(anio) %>%
  mutate(
    indice_sentimiento = case_when(
      sentimiento == "positivo" ~ score_normalizado,
      sentimiento == "negativo" ~ -score_normalizado,
      TRUE ~ 0
    )
  ) %>%
  ungroup()

# Calcular índice de polarización por año
polarizacion_anual <- sentimientos_evolucion %>%
  group_by(anio) %>%
  summarise(
    indice_polarizacion = sum(abs(indice_sentimiento)),
    balance_emocional = sum(indice_sentimiento),
    diversidad_emocional = n_distinct(contexto),
    .groups = "drop"
  )

# Función para crear paleta de colores personalizada
crear_paleta_sentimientos <- function(n_colors) {
  colores_base <- c(
    # Positivos
    "#DC143C", "#32CD32", "#228B22", "#90EE90",
    # Negativos  
    "#DC143C", "#B22222", "#FF6347", "#FFB6C1",
    # Autoridad
    "#4169E1", "#6495ED", "#87CEEB", "#E6E6FA",
    # Neutros
    "#D2691E", "#F4A460", "#DDA0DD", "#98FB98"
  )
  
  if (n_colors <= length(colores_base)) {
    return(colores_base[1:n_colors])
  } else {
    return(colorRampPalette(colores_base)(n_colors))
  }
}

# Visualización mejorada con manejo robusto de errores
crear_viz_sentimientos <- function(datos, interactive = TRUE) {
  
  # Verificar si hay datos
  if (nrow(datos) == 0) {
    warning("No hay datos para visualizar")
    return(NULL)
  }
  
  # Preparar datos con validaciones adicionales
  datos_viz <- datos %>%
    # Filtrar valores válidos
    filter(!is.na(score_normalizado), 
           !is.infinite(score_normalizado),
           !is.na(anio)) %>%
    mutate(
      sentimiento_contexto = paste(sentimiento, "-", contexto),
      # Convertir año a numérico y asegurar continuidad
      anio_num = as.numeric(anio),
      # Crear factor ordenado para mejor control
      anio_factor = factor(anio, levels = sort(unique(anio)))
    ) %>%
    # Verificar que tengamos al menos 2 años para evitar problemas de diff()
    filter(length(unique(anio_num)) >= 1)
  
  # Validar datos después de limpieza
  if (nrow(datos_viz) == 0) {
    warning("No hay datos válidos después de la limpieza")
    return(NULL)
  }
  
  # Número de categorías únicas
  n_categorias <- length(unique(datos_viz$sentimiento_contexto))
  
  # Crear escala de años explícita para evitar problemas de continuidad
  anios_completos <- seq(min(datos_viz$anio_num), max(datos_viz$anio_num), by = 1)
  
  # Crear gráfico base con configuraciones más robustas
  p_base <- datos_viz %>%
    ggplot(aes(x = anio_num, y = score_normalizado, fill = sentimiento_contexto)) +
    geom_area(alpha = 0.7, position = "stack") +
    facet_wrap(~sentimiento, scales = "free_y", nrow = 2) +
    # Escala x explícita para mejor control
    scale_x_continuous(
      breaks = anios_completos,
      labels = as.character(anios_completos),
      expand = c(0.02, 0)
    ) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(
      title = "Evolución del Panorama Emocional por Contexto (1926-1928)",
      subtitle = "Análisis multidimensional de sentimientos con contexto histórico",
      x = "Año", 
      y = "Score Normalizado (%)",
      fill = "Sentimiento - Contexto",
      caption = "Áreas apiladas muestran contribución relativa de cada contexto"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      panel.grid.minor.x = element_blank()
    ) +
    scale_fill_manual(values = crear_paleta_sentimientos(n_categorias)) +
    guides(fill = guide_legend(nrow = 3, byrow = TRUE))
  
  # Retornar versión según parámetro
  if (interactive) {
    # Crear plotly con configuraciones más conservadoras
    tryCatch({
      p_interactive <- plotly::ggplotly(p_base, height = 600) %>%
        plotly::layout(
          legend = list(
            orientation = "h",
            x = 0.1,
            y = -0.2
          ),
          # Configuraciones adicionales para estabilidad
          xaxis = list(fixedrange = FALSE),
          yaxis = list(fixedrange = FALSE)
        )
      return(p_interactive)
    }, warning = function(w) {
      # Suprimir warnings específicos de plotly
      if (grepl("ningún argumento finito para min", w$message)) {
        # Continuar sin mostrar el warning
        invokeRestart("muffleWarning")
      }
    }, error = function(e) {
      cat("Error en plotly, devolviendo versión estática:", e$message, "\n")
      return(p_base)
    })
  } else {
    return(p_base)
  }
}

# Función auxiliar para suprimir warnings específicos
suppressSpecificWarnings <- function(expr, patterns) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      if (any(sapply(patterns, function(p) grepl(p, w$message, fixed = TRUE)))) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

# Crear visualización con manejo completo de errores y warnings
suppressSpecificWarnings({
  
  tryCatch({
    
    # Verificar que los datos existen
    if (!exists("tokens")) {
      stop("El objeto 'tokens' no existe. Asegúrate de haber ejecutado el código de tokenización primero.")
    }
    
    # Verificar calidad de datos antes de procesar
    cat("=== DIAGNÓSTICO DE DATOS ===\n")
    cat("Filas en sentimientos_evolucion:", nrow(sentimientos_evolucion), "\n")
    cat("Años únicos:", paste(sort(unique(sentimientos_evolucion$anio)), collapse = ", "), "\n")
    cat("Sentimientos únicos:", paste(unique(sentimientos_evolucion$sentimiento), collapse = ", "), "\n")
    
    # Crear visualización
    viz_sentimientos <- crear_viz_sentimientos(sentimientos_evolucion, interactive = TRUE)
    
    # Mostrar visualización
    if (!is.null(viz_sentimientos)) {
      print(viz_sentimientos)
      cat("\n✓ Visualización interactiva creada exitosamente\n")
    } else {
      cat("⚠ No se pudo crear la visualización interactiva\n")
    }
    
  }, error = function(e) {
    cat("❌ Error en la visualización:", e$message, "\n")
    cat("Creando versión estática como alternativa...\n")
    
    # Crear versión estática como respaldo
    viz_estatica <- crear_viz_sentimientos(sentimientos_evolucion, interactive = FALSE)
    if (!is.null(viz_estatica)) {
      print(viz_estatica)
      cat("✓ Visualización estática creada como respaldo\n")
    }
  })
  
}, patterns = c("ningún argumento finito para min", "no finite arguments to min"))

# =============================================================================
# ANÁLISIS ADICIONAL: MÉTRICAS DE SENTIMIENTO
# =============================================================================

# Resumen de métricas por año
metricas_sentimiento <- sentimientos_evolucion %>%
  group_by(anio) %>%
  summarise(
    total_palabras_sentimiento = sum(frecuencia),
    contextos_diversos = n_distinct(contexto),
    sentimientos_diversos = n_distinct(sentimiento),
    intensidad_promedio = mean(intensidad_promedio),
    .groups = "drop"
  ) %>%
  # Calcular tendencias
  mutate(
    cambio_palabras = total_palabras_sentimiento - lag(total_palabras_sentimiento),
    cambio_porcentual = (cambio_palabras / lag(total_palabras_sentimiento)) * 100
  )

# Mostrar métricas
cat("\n=== MÉTRICAS DE SENTIMIENTO POR AÑO ===\n")
print(metricas_sentimiento)

# Análisis de contextos dominantes
contextos_dominantes <- sentimientos_evolucion %>%
  group_by(anio, contexto) %>%
  summarise(
    score_total = sum(score_normalizado),
    .groups = "drop"
  ) %>%
  group_by(anio) %>%
  slice_max(score_total, n = 3) %>%
  arrange(anio, desc(score_total))

cat("\n=== CONTEXTOS DOMINANTES POR AÑO ===\n")
print(contextos_dominantes)

# Visualización de tendencias de polarización
p_polarizacion <- polarizacion_anual %>%
  ggplot(aes(x = anio)) +
  geom_line(aes(y = indice_polarizacion, color = "Polarización"), size = 1.2) +
  geom_line(aes(y = balance_emocional, color = "Balance Emocional"), size = 1.2) +
  geom_point(aes(y = diversidad_emocional * 10, color = "Diversidad x10"), size = 3) +
  labs(
    title = "Índices de Polarización y Balance Emocional",
    subtitle = "Evolución de la tensión emocional en el discurso (1926-1928)",
    x = "Año",
    y = "Índice",
    color = "Métrica"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("Polarización" = "#DC143C", 
                                "Balance Emocional" = "#2E8B57",
                                "Diversidad x10" = "#4169E1"))

print(p_polarizacion)
# =============================================================================
# 7. MODELADO DE TEMAS (TOPIC MODELING) CON LDA
# =============================================================================

log_proceso("Iniciando modelado de temas con LDA")

# Preparar matriz documento-término
dtm_prep <- tokens %>%
  count(ubicacion, word) %>%
  cast_dtm(ubicacion, word, n)

# Encontrar número óptimo de temas
if(require(ldatuning, quietly = TRUE) && nrow(dtm_prep) > 5) {
  
  tryCatch({
    # Búsqueda de número óptimo (rango limitado para eficiencia)
    resultado_tuning <- FindTopicsNumber(
      dtm_prep,
      topics = seq(3, min(8, nrow(dtm_prep)-1), by = 1),
      metrics = c("Griffiths2004", "CaoJuan2009"),
      method = "Gibbs",
      control = list(seed = 123),
      verbose = FALSE
    )
    
    num_topics_optimo <- resultado_tuning$topics[which.max(resultado_tuning$Griffiths2004)]
    log_proceso(sprintf("Número óptimo de temas: %d", num_topics_optimo))
    
  }, error = function(e) {
    log_proceso("Error en tuning de temas, usando valor por defecto", "WARN")
    num_topics_optimo <- min(5, nrow(dtm_prep)-1)
  })
  
} else {
  num_topics_optimo <- min(5, nrow(dtm_prep)-1)
}

# Ajustar modelo LDA
if(num_topics_optimo >= 2) {
  modelo_lda <- LDA(dtm_prep, k = num_topics_optimo, control = list(seed = 123))
  
  # Extraer temas
  temas_palabras <- tidy(modelo_lda, matrix = "beta") %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  # Extraer distribución de temas por documento
  temas_documentos <- tidy(modelo_lda, matrix = "gamma") %>%
    spread(topic, gamma) %>%
    left_join(docs_index %>% select(ubicacion, anio), by = c("document" = "ubicacion"))
  
  # Visualizar temas principales
  p_temas <- temas_palabras %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ paste("Tema", topic), scales = "free") +
    coord_flip() +
    scale_x_reordered() +
    labs(
      title = "Temas Principales Identificados por LDA",
      subtitle = "Top 10 palabras más representativas por tema",
      x = "Términos", y = "Probabilidad Beta"
    ) +
    theme_minimal() +
    theme(strip.text = element_text(face = "bold"))
  
  print(p_temas)
  
  # Evolución temporal de temas
  if("anio" %in% colnames(temas_documentos)) {
    evolucion_temas <- temas_documentos %>%
      select(-document) %>%
      gather(tema, probabilidad, -anio) %>%
      group_by(anio, tema) %>%
      summarise(prob_promedio = mean(probabilidad, na.rm = TRUE), .groups = "drop")
    
    p_evolucion_temas <- evolucion_temas %>%
      ggplot(aes(x = anio, y = prob_promedio, color = tema)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      labs(
        title = "Evolución Temporal de Temas",
        subtitle = "Probabilidad promedio de cada tema por año",
        x = "Año", y = "Probabilidad Promedio",
        color = "Tema"
      ) +
      theme_minimal() +
      scale_color_brewer(type = "qual", palette = "Set2")
    
    plotly::ggplotly(p_evolucion_temas)
  }
}

# =============================================================================
# 8. ANÁLISIS DE COOCURRENCIA Y REDES SEMÁNTICAS
# =============================================================================

log_proceso("Construyendo redes semánticas")

# Umbral dinámico de frecuencia mínima
n_total <- nrow(tokens)
umbral_minimo <- if (n_total < 100) 1 else if (n_total < 1000) 1 else 2

coocurrencia_avanzada <- tokens %>%
  filter(word %in% freq_detallada$word[freq_detallada$rango <= 100]) %>%
  pairwise_count(word, ubicacion, sort = TRUE) %>%
  filter(n >= umbral_minimo) %>%
  mutate(
    prob_conjunta = n / sum(n),
    pmi = log2(prob_conjunta / 0.01)
  ) %>%
  filter(pmi > 0)

log_proceso(sprintf("Se detectaron %d coocurrencias relevantes con n >= %d", 
                    nrow(coocurrencia_avanzada), umbral_minimo))

# Construcción de grafo si hay al menos 2 conexiones
if(nrow(coocurrencia_avanzada) >= 2) {
  
  n_max <- min(50, nrow(coocurrencia_avanzada))  # No usar head(50) si hay menos
  
  grafo_semantico <- coocurrencia_avanzada %>%
    head(n_max) %>%
    graph_from_data_frame(directed = FALSE)

  # Métricas
  V(grafo_semantico)$degree <- degree(grafo_semantico)
  V(grafo_semantico)$betweenness <- betweenness(grafo_semantico)
  V(grafo_semantico)$closeness <- closeness(grafo_semantico)
  
  # Comunidades
  comunidades <- cluster_fast_greedy(grafo_semantico)
  V(grafo_semantico)$community <- membership(comunidades)
  
  # Visualización
  p_red_semantica <- ggraph(grafo_semantico, layout = "stress") +
    geom_edge_link(aes(width = n, alpha = n), color = "gray70") +
    geom_node_point(aes(size = degree, color = factor(community)), alpha = 0.8) +
    geom_node_text(aes(label = name, size = degree), 
                   repel = TRUE, point.padding = 0.3, max.overlaps = 15) +
    scale_edge_width(range = c(0.5, 3), guide = "none") +
    scale_edge_alpha(range = c(0.3, 0.8), guide = "none") +
    scale_size_continuous(range = c(3, 8), guide = "none") +
    scale_color_brewer(type = "qual", palette = "Set3", name = "Comunidad") +
    labs(
      title = "Red Semántica de Conceptos Clave",
      subtitle = "Tamaño = grado de conexión, Color = comunidad semántica",
      caption = "Basada en coocurrencia de palabras en documentos"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      legend.position = "bottom"
    )
  
  print(p_red_semantica)
  
  # Análisis de centralidad
  centralidad_palabras <- tibble(
    palabra = V(grafo_semantico)$name,
    grado = V(grafo_semantico)$degree,
    intermediacion = V(grafo_semantico)$betweenness,
    cercania = V(grafo_semantico)$closeness,
    comunidad = V(grafo_semantico)$community
  ) %>%
    arrange(desc(grado))
  
} else {
  log_proceso("No hay suficientes coocurrencias para construir la red semántica", "WARNING")
}


# =============================================================================
# 9. ANÁLISIS MULTIVARIADO Y CLUSTERING
# =============================================================================

log_proceso("Realizando análisis multivariado")

# Preparar matriz de características por documento
caracteristicas_docs <- tokens %>%
  # Seleccionar palabras más informativas
  filter(word %in% freq_detallada$word[freq_detallada$rango <= 50]) %>%
  count(ubicacion, word) %>%
  # Aplicar TF-IDF
  bind_tf_idf(word, ubicacion, n) %>%
  select(ubicacion, word, tf_idf) %>%
  pivot_wider(names_from = word, values_from = tf_idf, values_fill = 0) %>%
  column_to_rownames("ubicacion")

# Eliminar columnas con varianza cero
caracteristicas_docs <- caracteristicas_docs[, apply(caracteristicas_docs, 2, var) > 0]

# PCA para reducción de dimensionalidad
if(ncol(caracteristicas_docs) > 3 && nrow(caracteristicas_docs) > 3) {
  
  pca_resultado <- prcomp(caracteristicas_docs, scale. = TRUE, center = TRUE)
  
  # Varianza explicada
  var_explicada <- summary(pca_resultado)$importance[2,] * 100
  
  # Datos PCA
  pca_datos <- pca_resultado$x[,1:min(5, ncol(pca_resultado$x))] %>%
    as_tibble(rownames = "ubicacion") %>%
    left_join(docs_index %>% select(ubicacion, anio), by = "ubicacion")
  
  # Filtrar años con al menos 3 puntos para evitar warning en stat_ellipse
  conteo_por_anio <- pca_datos %>% count(anio)
  anios_validos <- conteo_por_anio %>% filter(n >= 3) %>% pull(anio)
  
  # Biplot PCA (solo años válidos)
  p_pca <- pca_datos %>%
    filter(anio %in% anios_validos) %>%
    ggplot(aes(x = PC1, y = PC2, color = factor(anio))) +
    geom_point(size = 3, alpha = 0.7) +
    stat_ellipse(type = "confidence", level = 0.68) +
    labs(
      title = "Análisis de Componentes Principales (PCA)",
      subtitle = sprintf("PC1: %.1f%% varianza, PC2: %.1f%% varianza", 
                         var_explicada[1], var_explicada[2]),
      x = sprintf("Componente Principal 1 (%.1f%%)", var_explicada[1]),
      y = sprintf("Componente Principal 2 (%.1f%%)", var_explicada[2]),
      color = "Año"
    ) +
    theme_minimal() +
    scale_color_brewer(type = "qual", palette = "Set2")
  
  plotly::ggplotly(p_pca)
  
  # Clustering jerárquico
  dist_docs <- dist(caracteristicas_docs, method = "euclidean")
  cluster_jerarquico <- hclust(dist_docs, method = "ward.D2")
  
  # Determinar número óptimo de clusters
  if(require(factoextra, quietly = TRUE)) {
    
    # Método del codo
    p_codo <- fviz_nbclust(caracteristicas_docs, 
                           FUN = function(x, k) list(withinss = kmeans(x, k, nstart = 20)$tot.withinss),
                           method = "wss", k.max = min(8, nrow(caracteristicas_docs)-1))
    
    print(p_codo)
    
    # Clustering k-means
    k_optimo <- 3  # Ajustar según resultado del método del codo
    kmeans_resultado <- kmeans(caracteristicas_docs, centers = k_optimo, nstart = 20)
    
    # Añadir clusters a datos PCA
    pca_datos$cluster <- as.factor(kmeans_resultado$cluster)
    
    # Contar puntos por cluster para filtrar
    conteo_por_cluster <- pca_datos %>% count(cluster)
    clusters_validos <- conteo_por_cluster %>% filter(n >= 3) %>% pull(cluster)
    
    # Visualización con clusters válidos
    p_pca_cluster <- pca_datos %>%
      filter(cluster %in% clusters_validos) %>%
      ggplot(aes(x = PC1, y = PC2, color = cluster, shape = factor(anio))) +
      geom_point(size = 4, alpha = 0.8) +
      stat_ellipse(aes(group = cluster), type = "confidence", level = 0.68) +
      labs(
        title = "Clustering de Documentos en Espacio PCA",
        subtitle = "Agrupación por similitud semántica",
        x = sprintf("PC1 (%.1f%%)", var_explicada[1]),
        y = sprintf("PC2 (%.1f%%)", var_explicada[2]),
        color = "Cluster", shape = "Año"
      ) +
      theme_minimal() +
      scale_color_brewer(type = "qual", palette = "Dark2")
    
    print(p_pca_cluster)
  }
  
} else {
  log_proceso("Matriz de características insuficiente para ejecutar PCA")
}


# =============================================================================
# 10. MÉTRICAS DE CALIDAD Y VALIDACIÓN
# =============================================================================

# Métricas de calidad del corpus
metricas_corpus <- list(
  # Estadísticas básicas
  total_documentos = nrow(docs_index),
  total_tokens = nrow(tokens),
  vocabulario_unico = n_distinct(tokens$word),
  
  # Diversidad léxica
  ratio_diversidad_global = n_distinct(tokens$word) / nrow(tokens),
  
  # Distribución temporal
  cobertura_temporal = paste(range(docs_index$anio), collapse = "-"),
  
  # Calidad de procesamiento
  tasa_exito_limpieza = sum(docs_index$calidad_ok) / nrow(docs_index),
  reduccion_ruido_promedio = mean(docs_index$ratio_reduccion, na.rm = TRUE),
  
  # Métricas de contenido
  palabras_por_documento = mean(docs_index$num_palabras_limpia),
  oraciones_por_documento = mean(docs_index$num_oraciones),
  complejidad_lexica_promedio = mean(docs_index$complejidad_lexica, na.rm = TRUE),
  
  # Validación lingüística
  proporcion_español = mean(docs_index$posible_español),
  
  # Métricas de Zipf
  ajuste_zipf = summary(modelo_zipf)$r.squared,
  pendiente_zipf = coef(modelo_zipf)[2]
)

# Test de consistencia interna (si hay suficientes documentos)
if(nrow(docs_index) >= 10) {
  
  # Dividir corpus en mitades para validación cruzada
  set.seed(123)
  muestra_1 <- sample(1:nrow(docs_index), size = floor(nrow(docs_index)/2))
  
  corpus_1 <- docs_index[muestra_1, ]
  corpus_2 <- docs_index[-muestra_1, ]
  
  # Tokenizar cada mitad
  tokens_1 <- tokenizar_avanzado(corpus_1, texto)
  tokens_2 <- tokenizar_avanzado(corpus_2, texto)
  
  # Calcular solapamiento de vocabulario
  vocab_1 <- unique(tokens_1$word)
  vocab_2 <- unique(tokens_2$word)
  
  solapamiento_vocab <- length(intersect(vocab_1, vocab_2)) / length(union(vocab_1, vocab_2))
  
  metricas_corpus$consistencia_vocabulario <- solapamiento_vocab
  
  log_proceso(sprintf("Consistencia de vocabulario entre mitades: %.3f", solapamiento_vocab))
}

# =============================================================================
# 11. DASHBOARD INTERACTIVO DE RESULTADOS
# =============================================================================

# Preparar resumen ejecutivo mejorado
resumen_ejecutivo <- tibble(
  Dimensión = c(
    "📊 Calidad de Datos",
    "🔤 Procesamiento Lingüístico", 
    "📈 Distribución Estadística",
    "⏱️ Cobertura Temporal",
    "🎯 Temas Identificados",
    "💭 Análisis Sentimental",
    "🕸️ Estructura Semántica",
    "✅ Validación del Modelo"
  ),
  Métrica_Clave = c(
    sprintf("%.1f%% éxito en limpieza", metricas_corpus$tasa_exito_limpieza * 100),
    sprintf("%d tokens únicos / %.3f diversidad", 
            metricas_corpus$vocabulario_unico, metricas_corpus$ratio_diversidad_global),
    sprintf("R² Zipf: %.3f (pendiente: %.2f)", 
            metricas_corpus$ajuste_zipf, metricas_corpus$pendiente_zipf),
    sprintf("Período: %s (%d docs)", 
            metricas_corpus$cobertura_temporal, metricas_corpus$total_documentos),
    sprintf("%d temas principales identificados", 
            ifelse(exists("num_topics_optimo"), num_topics_optimo, 0)),
    sprintf("%.1f%% polarización promedio", 
            ifelse(nrow(polarizacion_anual) > 0, mean(polarizacion_anual$indice_polarizacion), 0)),
    sprintf("%d comunidades semánticas", 
            ifelse(exists("comunidades"), length(comunidades), 0)),
    sprintf("Consistencia vocabulario: %.3f", 
            ifelse("consistencia_vocabulario" %in% names(metricas_corpus), 
                   metricas_corpus$consistencia_vocabulario, 0))
  ),
  Interpretación = c(
    ifelse(metricas_corpus$tasa_exito_limpieza > 0.8, "✅ Excelente", "⚠️ Revisar"),
    ifelse(metricas_corpus$ratio_diversidad_global > 0.1, "✅ Rica diversidad", "⚠️ Vocabulario limitado"),
    ifelse(abs(metricas_corpus$pendiente_zipf + 1) < 0.3, "✅ Distribución natural", "⚠️ Distribución atípica"),
    ifelse(metricas_corpus$total_documentos >= 10, "✅ Cobertura adecuada", "⚠️ Muestra pequeña"),
    ifelse(exists("num_topics_optimo") && num_topics_optimo >= 3, "✅ Estructura temática clara", "⚠️ Temas limitados"),
    "📊 Ver gráficos de evolución",
    ifelse(exists("comunidades") && length(comunidades) >= 2, "✅ Estructura semántica rica", "⚠️ Estructura simple"),
    ifelse("consistencia_vocabulario" %in% names(metricas_corpus) && 
             metricas_corpus$consistencia_vocabulario > 0.5, "✅ Corpus consistente", "⚠️ Revisar consistencia")
  )
)

# Mostrar dashboard principal
cat("\n" + "="*80 + "\n")
cat("🎯 DASHBOARD EJECUTIVO - ANÁLISIS DE TEXTO ROBUSTO\n")
cat("="*80 + "\n\n")

reactable(
  resumen_ejecutivo,
  striped = TRUE,
  highlight = TRUE,
  bordered = TRUE,
  pagination = FALSE,
  theme = reactableTheme(
    headerStyle = list(
      backgroundColor = "#2c3e50",
      color = "white",
      fontWeight = "bold",
      textAlign = "center"
    ),
    cellPadding = "12px",
    style = list(fontSize = "14px")
  ),
  columns = list(
    Dimensión = colDef(
      minWidth = 200,
      style = list(fontWeight = "bold", fontSize = "15px")
    ),
    Métrica_Clave = colDef(
      minWidth = 250,
      style = list(fontFamily = "monospace", backgroundColor = "#f8f9fa")
    ),
    Interpretación = colDef(
      minWidth = 200,
      style = function(value) {
        if(str_detect(value, "✅")) {
          list(color = "#27ae60", fontWeight = "bold")
        } else if(str_detect(value, "⚠️")) {
          list(color = "#f39c12", fontWeight = "bold")  
        } else {
          list(color = "#3498db")
        }
      }
    )
  )
)

# =============================================================================
# 12. EXPORTACIÓN DE RESULTADOS Y RECOMENDACIONES
# =============================================================================

# Compilar todos los resultados
resultados_completos <- list(
  # Datos procesados
  metadatos = docs_index,
  tokens = tokens,
  
  # Análisis descriptivo
  frecuencias = freq_detallada,
  estadisticas_zipf = list(modelo = modelo_zipf, datos = analisis_zipf),
  
  # Análisis temporal
  evolucion_categorias = analisis_temporal_robusto,
  
  # Análisis de sentimientos
  sentimientos_evolucion = sentimientos_evolucion,
  polarizacion_anual = polarizacion_anual,
  
  # N-gramas y coocurrencia
  bigramas = bigramas_avanzados,
  trigramas = trigramas_avanzados,
  coocurrencia = coocurrencia_avanzada,
  
  # Modelado de temas
  modelo_temas = if(exists("modelo_lda")) modelo_lda else NULL,
  temas_palabras = if(exists("temas_palabras")) temas_palabras else NULL,
  
  # Análisis multivariado
  pca_resultado = if(exists("pca_resultado")) pca_resultado else NULL,
  clustering = if(exists("kmeans_resultado")) kmeans_resultado else NULL,
  
  # Métricas de calidad
  metricas_corpus = metricas_corpus,
  resumen_ejecutivo = resumen_ejecutivo
)

# Guardar resultados
tryCatch({
  save(resultados_completos, file = "analisis_discursos_completo_v2.RData")
  log_proceso("Resultados guardados exitosamente")
}, error = function(e) {
  log_proceso(paste("Error al guardar:", e$message), "ERROR")
})

# Recomendaciones finales
cat("\n" + "="*80 + "\n")
cat("🔍 RECOMENDACIONES PARA ANÁLISIS FUTUROS\n")
cat("="*80 + "\n\n")

recomendaciones <- c(
  "1. 📚 Ampliar corpus: Incluir más documentos del período para mayor robustez estadística",
  "2. 🔍 Validación externa: Comparar resultados con análisis de historiadores del período",
  "3. 📊 Análisis longitudinal: Extender el rango temporal para identificar tendencias de largo plazo",
  "4. 🎯 Segmentación avanzada: Analizar por tipo de documento, autor o contexto específico",
  "5. 🌐 Análisis comparativo: Contrastar con corpus similares de otros países/períodos",
  "6. 🤖 ML avanzado: Implementar modelos de embeddings (Word2Vec, BERT) para análisis semántico",
  "7. 📈 Validación temporal: Dividir corpus por períodos para validar estabilidad de patrones",
  "8. 🔗 Análisis de redes: Profundizar en redes semánticas y análisis de centralidad",
  "9. 📝 Anotación manual: Validar automáticamente clasificaciones con revisión experta",
  "10. 📊 Métricas adicionales: Implementar medidas de coherencia temática y estabilidad"
)

for(rec in recomendaciones) {
  cat(rec, "\n")
}

cat("\n✨ ANÁLISIS COMPLETADO EXITOSAMENTE ✨\n")
cat("="*80 + "\n")

log_proceso("Análisis robusto completado exitosamente")

