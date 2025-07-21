---
title: "Text Data Preprocessing for Topic Modeling"
subtitle: "Optimized for Polish Language Texts"
author: "Your Name"
date: "`r Sys.Date()`"
output: 
  html_document:
    toc: true
    toc_float: true
    theme: flatly
    highlight: tango
    code_folding: show
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
```

# Introduction

This R Markdown document provides a comprehensive workflow for preprocessing Polish text data for topic modeling. The script includes multiple approaches using different R packages and provides quality checks to ensure the data is ready for topic modeling algorithms.

# Load Required Libraries

```{r load-libraries}
# Load required libraries
library(tidyverse)     # Data manipulation
library(tm)           # Text mining
library(SnowballC)    # Stemming
library(quanteda)     # Alternative text processing
library(stringr)      # String operations
library(VCorpus)      # Corpus handling
library(topicmodels)  # Topic modeling
library(tidytext)     # Tidy text processing
library(DT)          # For interactive tables
library(kableExtra)   # For nice tables
```

# Data Loading

Load your text data from a CSV file. Make sure your dataframe has at least 'id' and 'text' columns.

```{r load-data}
# Load data from CSV file
# Adjust file path, encoding and column names as needed
df <- read.csv("your_data.csv", encoding = "UTF-8", stringsAsFactors = FALSE)

# Expected structure: dataframe with at least 'id' and 'text' columns
# If your columns have different names, rename them:
# df <- df %>% rename(text = your_text_column_name)

# Display basic info about the dataset
cat("Dataset dimensions:", nrow(df), "rows,", ncol(df), "columns\n")
cat("Column names:", paste(names(df), collapse = ", "), "\n")

# Show first few rows
head(df) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped", "hover"))
```

# Text Cleaning Functions

Define comprehensive text cleaning functions optimized for Polish texts.

```{r text-cleaning-functions}
# Basic text cleaning function
clean_text <- function(text) {
  # Convert to lowercase
  text <- tolower(text)
  
  # Remove HTML tags (if present)
  text <- gsub("<.*?>", " ", text)
  
  # Remove URLs
  text <- gsub("http[s]?://\\S+", " ", text)
  text <- gsub("www\\.\\S+", " ", text)
  
  # Remove email addresses
  text <- gsub("\\S+@\\S+", " ", text)
  
  # Remove numbers (optional)
  text <- gsub("\\d+", " ", text)
  
  # Remove special characters, keep only Polish letters and spaces
  text <- gsub("[^ąćęłńóśźża-z\\s]", " ", text)
  
  # Replace multiple spaces with single space
  text <- gsub("\\s+", " ", text)
  
  # Trim leading and trailing spaces
  text <- str_trim(text)
  
  return(text)
}

# Polish stemming function
polish_stemmer <- function(text) {
  # Basic Polish suffixes and inflections to remove
  endings <- c(
    # Noun endings
    "ami", "ach", "em", "ie", "ów", "y", "e", "a", "ę", "ą",
    # Adjective endings  
    "owy", "owa", "owe", "ich", "ych", "ej", "ym", "ymi",
    # Verb endings
    "ować", "ić", "ąć", "nąć", "em", "esz", "emy", "ecie", "ą",
    "ł", "ła", "ło", "ły", "łem", "łeś", "łam", "łaś",
    # Common suffixes
    "ność", "ość", "enie", "anie", "acja", "tion", "ment",
    "owy", "owa", "owe", "ny", "na", "ne", "ty", "ta", "te"
  )
  
  words <- unlist(strsplit(text, " "))
  stemmed_words <- sapply(words, function(word) {
    for(ending in endings) {
      if(str_ends(word, ending) && nchar(word) > nchar(ending) + 2) {
        return(str_remove(word, paste0(ending, "$")))
      }
    }
    return(word)
  })
  
  return(paste(stemmed_words, collapse = " "))
}

# Polish stopwords (comprehensive list)
polish_stopwords <- c(
  # Articles and particles
  "a", "aby", "ale", "am", "ani", "aż", "bardzo", "bez", "bo", "być", "ci", "co", 
  "czy", "dla", "do", "gdy", "go", "i", "ich", "ile", "im", "ja", "jak", "jako", 
  "je", "jego", "jej", "jest", "jestem", "już", "lub", "ma", "mają", "może", 
  "na", "nad", "nam", "nami", "nas", "nasz", "nasze", "nie", "nią", "nim", "nimi", 
  "o", "od", "po", "pod", "podczas", "oraz", "również", "się", "są", "tak", "także", 
  "tam", "te", "tej", "tez", "to", "tu", "tym", "tylko", "w", "we", "więc", 
  "wszystko", "z", "za", "ze", "że",
  # Pronouns
  "mnie", "mi", "mną", "cię", "ci", "ciebie", "tobą", "on", "ona", "ono", "oni", 
  "one", "mu", "ją", "niego", "niej", "nich", "nimi", "sobie", "siebie",
  # Common verbs
  "będzie", "będą", "był", "była", "było", "były", "mieć", "może", "można", 
  "powinien", "powinna", "powinno", "chce", "chcę", "chcesz",
  # Prepositions and conjunctions
  "między", "przed", "przez", "przeciw", "przy", "według", "wobec", "około", 
  "wśród", "obok", "dzięki", "oprócz", "poza", "zamiast", "mimo", "gdyby", 
  "jeśli", "ponieważ", "dlatego", "jednak", "natomiast", "gdyż"
)

cat("Defined", length(polish_stopwords), "Polish stopwords\n")
```

# Apply Text Cleaning

Apply the cleaning functions to your text data.

```{r apply-cleaning}
# Apply text cleaning
df$clean_text <- sapply(df$text, clean_text)

# Show before/after comparison for first few texts
comparison <- df[1:min(3, nrow(df)), c("text", "clean_text")]
comparison %>% 
  kable(col.names = c("Original Text", "Cleaned Text")) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"))
```

# Method 1: TM Package Approach

Create and process corpus using the tm package.

```{r tm-approach}
# Create corpus
corpus <- VCorpus(VectorSource(df$clean_text))

# Clean corpus using tm package functions
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)

# Remove stopwords
corpus <- tm_map(corpus, removeWords, polish_stopwords)
corpus <- tm_map(corpus, removeWords, stopwords("polish")) # if available

# Apply stemming
corpus <- tm_map(corpus, content_transformer(polish_stemmer))

# Create Document-Term Matrix
dtm <- DocumentTermMatrix(corpus)

# Remove sparse terms (words that appear in less than 5% of documents)
dtm_filtered <- removeSparseTerms(dtm, 0.95)

# Remove empty documents (if any)
row_sums <- apply(dtm_filtered, 1, sum)
dtm_filtered <- dtm_filtered[row_sums > 0, ]

cat("TM Package Results:\n")
cat("Documents:", nrow(dtm_filtered), "\n")
cat("Terms:", ncol(dtm_filtered), "\n")
cat("Non-zero entries:", sum(dtm_filtered > 0), "\n")
```

# Method 2: Quanteda Package Approach

Alternative approach using the quanteda package.

```{r quanteda-approach}
# Create quanteda corpus
quanteda_corpus <- corpus(df$clean_text)

# Tokenization and cleaning
tokens <- tokens(quanteda_corpus,
                remove_punct = TRUE,
                remove_numbers = TRUE,
                remove_symbols = TRUE)

# Remove stopwords
tokens <- tokens_remove(tokens, polish_stopwords)

# Create Document-Feature Matrix (DFM)
dfm <- dfm(tokens)

# Filter rare terms
dfm_trimmed <- dfm_trim(dfm, 
                       min_docfreq = 2,    # appear in minimum 2 documents
                       max_docfreq = 0.8,  # appear in maximum 80% of documents
                       docfreq_type = "prop")

cat("Quanteda Package Results:\n")
cat("Documents:", nrow(dfm_trimmed), "\n")
cat("Features:", ncol(dfm_trimmed), "\n")
cat("Non-zero entries:", sum(dfm_trimmed > 0), "\n")
```

# Method 3: Tidy Text Approach

Using the tidytext package for a tidy workflow.

```{r tidytext-approach}
# Convert to tidy format
tidy_text <- df %>%
  unnest_tokens(word, clean_text) %>%
  anti_join(data.frame(word = polish_stopwords), by = "word") %>%
  count(id, word, sort = TRUE)

# Create document-term matrix from tidy format
dtm_tidy <- tidy_text %>%
  cast_dtm(id, word, n)

cat("Tidy Text Package Results:\n")
cat("Documents:", nrow(dtm_tidy), "\n")
cat("Terms:", ncol(dtm_tidy), "\n")
cat("Non-zero entries:", sum(dtm_tidy > 0), "\n")

# Show most common words
top_words <- tidy_text %>%
  group_by(word) %>%
  summarise(total = sum(n), .groups = 'drop') %>%
  arrange(desc(total)) %>%
  head(10)

top_words %>%
  kable(col.names = c("Word", "Frequency")) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
```

# Quality Checks and Statistics

Comprehensive quality assessment of the processed data.

```{r quality-checks}
cat("=== CORPUS STATISTICS ===\n")
cat("Number of documents:", nrow(dtm_filtered), "\n")
cat("Number of terms:", ncol(dtm_filtered), "\n")
cat("Non-zero elements:", sum(dtm_filtered > 0), "\n")
cat("Sparsity ratio:", round(1 - sum(dtm_filtered > 0)/(nrow(dtm_filtered)*ncol(dtm_filtered)), 3), "\n\n")

# Most frequent words
word_freq <- sort(colSums(as.matrix(dtm_filtered)), decreasing = TRUE)
cat("=== TOP 10 MOST FREQUENT WORDS ===\n")
print(head(word_freq, 10))

# Document lengths
doc_lengths <- rowSums(as.matrix(dtm_filtered))
cat("\n=== DOCUMENT LENGTH STATISTICS ===\n")
print(summary(doc_lengths))

# Create visualization of document lengths
doc_length_df <- data.frame(
  doc_id = 1:length(doc_lengths),
  length = doc_lengths
)

ggplot(doc_length_df, aes(x = length)) +
  geom_histogram(bins = 20, fill = "steelblue", alpha = 0.7) +
  labs(title = "Distribution of Document Lengths",
       x = "Number of Terms per Document",
       y = "Frequency") +
  theme_minimal()
```

# Final Validation

Check if the data is ready for topic modeling.

```{r final-validation}
# Check: are there any empty documents?
empty_docs <- which(rowSums(as.matrix(dtm_filtered)) == 0)
if(length(empty_docs) > 0) {
  cat("⚠️ Warning! Empty documents:", length(empty_docs), "\n")
} else {
  cat("✅ No empty documents\n")
}

# Check: minimum word count per document
min_words_per_doc <- 5
short_docs <- which(rowSums(as.matrix(dtm_filtered)) < min_words_per_doc)
if(length(short_docs) > 0) {
  cat("⚠️ Warning! Short documents (<", min_words_per_doc, "words):", length(short_docs), "\n")
} else {
  cat("✅ All documents contain at least", min_words_per_doc, "words\n")
}

cat("\n=== PREPROCESSING COMPLETED ===\n")
cat("DTM is ready for topic modeling!\n")
cat("Available variables:\n")
cat("- dtm_filtered: tm package DTM\n")
cat("- dfm_trimmed: quanteda DFM\n") 
cat("- dtm_tidy: tidytext DTM\n")
```

# Save Results

Export the processed data for further analysis.

```{r save-results}
# Save DTM in RDS format (for topicmodels package)
saveRDS(dtm_filtered, "dtm_for_topic_modeling_polish.rds")

# Save cleaned texts
write.csv(df, "cleaned_texts_polish.csv", row.names = FALSE, fileEncoding = "UTF-8")

# Save quanteda DFM
saveRDS(dfm_trimmed, "dfm_for_topic_modeling_polish.rds")

cat("Files saved:\n")
cat("- dtm_for_topic_modeling_polish.rds: Document-Term Matrix (tm package)\n")
cat("- dfm_for_topic_modeling_polish.rds: Document-Feature Matrix (quanteda)\n")
cat("- cleaned_texts_polish.csv: Original and cleaned texts\n")
```

# Session Information

```{r session-info}
sessionInfo()
```

---

**Notes:**
- Adjust the CSV file path in the data loading section
- Customize Polish stopwords list if needed based on your domain
- Modify filtering parameters based on your data characteristics
- The script provides three different approaches - choose the one that best fits your workflow
- Polish morphology is complex - consider using more advanced stemming libraries like `RPolishStemmer` for better results