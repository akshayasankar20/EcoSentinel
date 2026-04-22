# EcoSentinel: Ecological Tipping Point Detection System

## Project Overview

EcoSentinel is a data science system that predicts **ecosystem collapse before it happens** and explains the reasons behind it using machine learning and explainable AI.

The system simulates ecological data and detects early warning signals for critical transitions (tipping points).

---

## Key Features

* Simulates 30 years of ecological data
* Detects early warning signals (EWS)
* Predicts tipping points using Random Forest
* Explainable AI (Global + Local explanations)
* Changepoint detection using PELT algorithm
* Interactive dashboard using Shiny

---

##  Technologies Used

* R Programming
* Random Forest (Machine Learning)
* Changepoint Detection (PELT)
* Explainable AI (Permutation Importance, LIME)
* Shiny (Web Dashboard)
* ggplot2 (Visualization)

---

##  Project Structure

```
EcoSentinel/
│
├── 00_run_all.R              # Main script to run entire project
├── 01_data_simulation.R      # Data generation & preprocessing
├── 02_model_and_detection.R  # ML model + changepoint detection
├── 03_explainability.R       # Explainable AI
├── 04_visualization.R        # Graphs and plots
├── app.R                     # Shiny dashboard
├── README.md                 # Project documentation
```
<img width="1600" height="817" alt="WhatsApp Image 2026-04-07 at 12 10 03 AM" src="https://github.com/user-attachments/assets/4f07c312-5607-43bb-a149-df29d9439a83" />
<img width="1600" height="768" alt="WhatsApp Image 2026-04-07 at 12 10 32 AM" src="https://github.com/user-attachments/assets/58311b57-e8b9-4008-bda3-2fdbd7512bd4" />
<img width="1600" height="757" alt="WhatsApp Image 2026-04-07 at 12 11 05 AM" src="https://github.com/user-attachments/assets/c010b8ac-221b-472f-8eba-b8a26049f8c1" />
<img width="1600" height="762" alt="WhatsApp Image 2026-04-07 at 12 11 49 AM" src="https://github.com/user-attachments/assets/c3a8c7cf-4eae-46fc-90ad-885ca64cd913" />
<img width="1600" height="746" alt="WhatsApp Image 2026-04-07 at 12 12 16 AM" src="https://github.com/user-attachments/assets/9975f7eb-3b58-40ca-be80-c4cc60d3fbad" />





---

##  How to Run the Project

### Set working directory

```r
setwd("path_to_project_folder")
```

### Run main script

```r
source("00_run_all.R")
```

### Launch dashboard (if not auto)

```r
shiny::runApp("app.R")
```

---

## Output

* Time series plots
* Early warning signals
* Prediction probabilities
* Feature importance graphs
* Interactive dashboard

---

## Objective

To detect ecological tipping points **before they occur** and provide interpretable insights for decision-making.





This project is for academic and research purposes.
