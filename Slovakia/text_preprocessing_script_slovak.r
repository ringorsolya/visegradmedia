---
title: "Text Data Preprocessing for Topic Modeling"
subtitle: "Optimized for Slovak Language Texts"
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

This R Markdown document provides a comprehensive workflow for preprocessing Slovak text data for topic modeling. The script includes multiple approaches using different R packages and provides quality checks to ensure the data is ready for topic modeling algorithms.

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

Define comprehensive text cleaning functions optimized for Slovak texts.

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
  
  # Remove special characters, keep only Slovak letters and spaces
  text <- gsub("[^áäčďéíĺľňóôŕšťúýža-z\\s]", " ", text)
  
  # Replace multiple spaces with single space
  text <- gsub("\\s+", " ", text)
  
  # Trim leading and trailing spaces
  text <- str_trim(text)
  
  return(text)
}

# Slovak stemming function
slovak_stemmer <- function(text) {
  # Basic Slovak suffixes and inflections to remove
  endings <- c(
    # Noun endings (cases - 6 cases in Slovak)
    "ovia", "ovi", "ov", "om", "och", "ami", "mi", "ím", "ách", "ách", "ou", 
    "u", "y", "e", "a", "í", "ej", "ého", "ému", "ým", "ých",
    # Adjective endings
    "ého", "ému", "ými", "ých", "ová", "ové", "ový", "ná", "né", "ný", 
    "ou", "ej", "om", "ami", "ích", "ími",
    # Verb endings
    "ovať", "ávať", "ať", "ť", "ieť", "núť", "sť", "cť",
    "em", "eš", "e", "eme", "ete", "ú", "u", "í", "íme", "íte", "ia",
    "l", "la", "lo", "li", "ly", "al", "ala", "alo", "ali", "aly",
    # Past participles and adjectives
    "ný", "ná", "né", "ní", "tý", "tá", "té", "tí", "ený", "ená", "ené",
    # Diminutives and augmentatives
    "ček", "čok", "ik", "ík", "ko", "ka", "isko", "isko",
    # Common suffixes
    "nosť", "osť", "anie", "enie", "stvo", "ár", "ár", "áč", "ačka"
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

# Slovak stopwords (comprehensive list)
slovak_stopwords <- c(
  # Articles, particles, and basic words
  "a", "aby", "aj", "ak", "ako", "ale", "alebo", "and", "ani", "áno", "asi", 
  "až", "bez", "bude", "budeme", "budem", "budete", "budeš", "budú", "by", "byť",
  # Pronouns and demonstratives  
  "čí", "čím", "čo", "či", "ďalší", "dnes", "do", "ho", "hra", "hrá", "i", "ich", 
  "idem", "ideme", "idete", "ideš", "idú", "ide", "je", "jedna", "jedno", "jeho", 
  "jej", "jej", "jeden", "jedného", "jednej", "jednom", "jednou", "jednu", 
  "jeho", "jejích", "jej", "jej", "jej",
  # Common verbs and auxiliaries
  "som", "si", "je", "sme", "ste", "sú", "bol", "bola", "bolo", "boli", "boly",
  "mať", "má", "máš", "máme", "máte", "majú", "mal", "mala", "malo", "mali", "maly",
  "môcť", "môžem", "môžeš", "môže", "môžeme", "môžete", "môžu",
  # Prepositions and conjunctions
  "k", "ku", "kam", "kde", "kedy", "keď", "ktorá", "ktoré", "ktorý", "ktorých", 
  "ktorým", "ktorými", "lebo", "len", "má", "mať", "medzi", "mi", "mna", "mne", 
  "mnou", "my", "na", "nad", "nám", "náš", "naše", "našej", "našich", "našimi", 
  "našou", "nás", "ne", "nech", "než", "ní", "nich", "ním", "nimi", "nič",
  # More common words
  "o", "od", "odo", "on", "ona", "ono", "oni", "ony", "po", "pod", "podľa", 
  "pokiaľ", "pre", "pred", "pri", "s", "sa", "se", "so", "svojej", "svojho", 
  "svojich", "svojimi", "svojou", "svoj", "svoje", "tak", "také", "takí", "takú", 
  "tam", "to", "toho", "tomu", "tom", "tou", "tu", "ty", "tým", "tými", "tých",
  # Additional common words
  "už", "v", "vo", "vám", "váš", "vaše", "vašej", "vašich", "vašimi", "vašou", 
  "vás", "veľa", "veľmi", "však", "všetci", "všetko", "vy", "z", "za", "zo", 
  "že", "žeby",
  # Numbers and time
  "jeden", "dva", "tri", "štyri", "päť", "dnes", "včera", "zajtra", "rok", 
  "roky", "rokov", "hodina", "hodiny", "hodín", "deň", "dni", "dní", "čas", 
  "času", "času", "doba", "doby"
)

cat("Defined", length(slovak_stopwords), "Slovak stopwords\n")
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
corpus <- tm_map(corpus, removeWords, slovak_stopwords)
corpus <- tm_map(corpus, removeWords, stopwords("slovak")) # if available

# Apply stemming
corpus <- tm_map(corpus, content_transformer(slovak_stemmer))

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
tokens <- tokens_remove(tokens, slovak_stopwords)

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
  anti_join(data.frame(word = slovak_stopwords), by = "word") %>%
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
saveRDS(dtm_filtered, "dtm_for_topic_modeling_slovak.rds")

# Save cleaned texts
write.csv(df, "cleaned_texts_slovak.csv", row.names = FALSE, fileEncoding = "UTF-8")

# Save quanteda DFM
saveRDS(dfm_trimmed, "dfm_for_topic_modeling_slovak.rds")

cat("Files saved:\n")
cat("- dtm_for_topic_modeling_slovak.rds: Document-Term Matrix (tm package)\n")
cat("- dfm_for_topic_modeling_slovak.rds: Document-Feature Matrix (quanteda)\n")
cat("- cleaned_texts_slovak.csv: Original and cleaned texts\n")
```

# Session Information

```{r session-info}
sessionInfo()
```

---

**Notes:**
- Adjust the CSV file path in the data loading section
- Customize Slovak stopwords list if needed based on your domain
- Modify filtering parameters based on your data characteristics  
- The script provides three different approaches - choose the one that best fits your workflow
- Slovak has 6 grammatical cases and complex verb conjugations - consider using specialized Slovak NLP tools like `UDPipe` for more accurate morphological analysis