# Proyecto: La amenaza religiosa y el orden estatal en el discurso oficial mexicano (1926–1928)

Este proyecto explora el discurso político oficial en México durante el periodo de 1926 a 1928, en el contexto de la **Guerra Cristera**, mediante técnicas de **minería de texto**. El análisis se centra en cómo el Estado delineó su autoridad frente a la Iglesia católica a través de documentos clave del presidente Plutarco Elías Calles y otras fuentes oficiales.

## Objetivo

Analizar la manera en que el discurso oficial del Estado mexicano construyó una narrativa de amenaza religiosa y legitimó la separación Iglesia-Estado entre 1926 y 1928.

---

## Estructura del proyecto

```
proyecto Guerra Cristera/
├── 📁Data/ # Conjunto de datos originales y procesados
│ └── 📁db/
│ ├── 📁Originales/ # Textos históricos y CSV original
│ │ ├── index.csv # Metadatos de los documentos
│ │ └── 📁textos/ # Textos completos en formato .txt
│ └── 📁Procesados/ # Espacio para almacenar versiones limpias/tokenizadas
│
├── 📁Presentacion/
│ ├── 📁Informes/ # Reportes exportables (vacío actualmente)
│ ├── 📁pdf/ # Documentos en PDF
│ └── 📁web/ # Versión HTML del informe (Quarto)
│ ├── *.html # Documento principal generado con Quarto
│ ├── *.qmd # Documento fuente Quarto
│ ├── referencias.bib # Bibliografía en formato BibTeX
│ └── 📁libs/ # Librerías JS y CSS necesarias para widgets
│
├── 📁Resultados/
│ ├── 📁img/ # Gráficos e imágenes generadas
│ └── 📁pdf/ # Exportables en PDF (gráficas, tablas)
│
├── 📁Scripts/
│ └── codigo.R # Código de análisis principal en R
│
├── .gitignore # Exclusiones para Git
├── proyecto Guerra Cristera.Rproj # Proyecto de RStudio
├── README.md # Este archivo
```


## Tecnologías y herramientas

- **Lenguaje**: R
- **Entorno**: RStudio + Quarto
- **Librerías principales**:
  - `tidyverse`, `tidytext`, `stringr`: Procesamiento de texto
  - `reactable`: Tablas interactivas
  - `knitr`, `kableExtra`: Tablas en Quarto
  - `stopwords`, `tokenizers`: Limpieza lingüística
  - `Quarto`: Generación de reportes dinámicos
  - `biblatex`, `apa.csl`: Formato bibliográfico

---

## Documento principal

El informe se encuentra en:

- `Presentacion/web/La amenaza religiosa y el orden estatal en el discurso oficial mexicano (1926–1928): un análisis exploratorio con técnicas de minería de texto.qmd`

Este archivo Quarto (`.qmd`) contiene todo el análisis, visualizaciones y referencias.

Para compilarlo:

```bash
quarto render "Presentacion/web/La amenaza religiosa...texto.qmd"
```

## Propósito de uso del Corpus

El objetivo es analizar cómo el Estado mexicano representó a la religión como una amenaza durante la Ley Calles (1926–1928), a través de un corpus de documentos oficiales. Se examinarán términos clave, estrategias discursivas y el tono emocional de los textos, para comprender cómo el lenguaje estatal operó como herramienta de control y legitimación política..
