# visegradmedia

# Identifying News Slant in Crisis Communication Using Artificial Intelligence

![Language](https://img.shields.io/badge/languages-R%20%7C%20Python-blue.svg)
[![Hugging Face Model](https://img.shields.io/badge/HF-model-visegradmedia--emotion-orange)](https://huggingface.co/visegradmedia-emotion)
[![Dataset (OSF)](https://img.shields.io/badge/dataset-OSF-green)](https://osf.io/45et2/?view_only=1842a8c679284c909556cb496228f476)
![License](https://img.shields.io/badge/license-research--only-lightgrey)

This repository contains the analysis scripts for the Visegrad project *“Identifying News Slant in Crisis Communication Using Artificial Intelligence.”* It includes code for all four participating countries — **Czechia, Hungary, Poland, and Slovakia** — using both **R** and **Python**.

## Project Overview

Our project applies various text mining and natural language processing (NLP) methods — including novel approaches based on language modelling and deep learning — to identify **slants in online news coverage**. The goal is to promote resilience against biased reporting by developing a robust, cross-country slant detection model.

The project is co-financed by the Governments of Czechia, Hungary, Poland, and Slovakia through **Visegrad Grants** from the **International Visegrad Fund**. The mission of the fund is to advance ideas for sustainable regional cooperation in Central Europe.

## Repository Contents

The repository includes the following components:

- `scripts/lda/`: LDA topic modelling scripts per country (`.R` and `.py`)
- `scripts/preprocessing/`: Text cleaning and preprocessing scripts
- `scripts/emotion_analysis/`: Emotion classification scripts using XLM-RoBERTa
- `results/`: Output figures and topic models

## Emotion Model

The emotion analysis is based on a **fine-tuned XLM-RoBERTa model**, specifically trained for the project’s purposes.  
It is available via HuggingFace:  
🔗 [https://huggingface.co/visegradmedia-emotion](https://huggingface.co/visegradmedia-emotion)

If you use this model in your work, please cite the following publication:

> **István ÜVEGES, Orsolya RING** (2025). *Evaluating the Impact of Synthetic Data on Emotion Classification: A Linguistic and Structural Analysis*. **Information**, 16(4), 330.  
> DOI: [https://doi.org/10.3390/info16040330](https://doi.org/10.3390/info16040330)

## Dataset

The data used in this project is **not included in this repository**. It is hosted on the Open Science Framework (OSF) and accessible upon request:

🔗 **[Visegradmedia – Central European News Corpus (OSF link)](https://osf.io/45et2/?view_only=1842a8c679284c909556cb496228f476)**

**Title**: *Visegradmedia – Central European News Corpus*  
**Version**: 1.0  
**Year**: 2025  

This corpus contains textual content derived from publicly available news media sources in Hungary, Slovakia, Czechia, and Poland. The copyright for all original articles remains with the respective publishers.

**Permitted Use**:
- Academic research  
- Non-commercial educational use  

**Prohibited Use**:
- Redistribution or republishing of the corpus in whole or in part  
- Commercial use of the corpus or its contents  
- Use of the corpus to train commercial language models  

**Attribution**: Any publications, presentations, or derivative research using this corpus must cite the corpus creators and acknowledge the original media content appropriately.

**Access**: Access may be granted upon request. Users must comply with all applicable copyright and data protection laws.

**Contact for access or inquiries**: [ring.orsolyal@tk.hu](mailto:ring.orsolyal@tk.hu)

## Requirements

- **Python 3.10+**
- **R 4.2+**
- Key packages:
  - Python: `transformers`, `pandas`, `scikit-learn`, `matplotlib`
  - R: `textmineR`, `tm`, `topicmodels`, `tidyverse`

You can install Python dependencies via:

```bash
pip install -r requirements.txt

## Funding Acknowledgement

This project is co-financed by the Governments of Czechia, Hungary, Poland, and Slovakia through Visegrad Grants from the International Visegrad Fund. The mission of the fund is to advance ideas for sustainable regional cooperation in Central Europe.

<p align="center">
  <img src="https://www.visegradfund.org/media/logo/visegrad_fund_logo_black.svg" alt="Visegrad Fund Logo" width="250"/>
</p>
