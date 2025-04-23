# Effect of Nipple Water Troughs on Cross-Sucking in Dairy Calves  

![Example Visualization]()  
*(Example figure from the manuscript: Frequency of cross-sucking by body part.)*

## 📜 Study Overview  
This project evaluated how nipple-based water troughs influence cross-sucking behavior and growth in group-housed dairy calves raised on pasture. Key findings include:  
- **42% reduction in cross-sucking** in calves with nipple access vs. open troughs (*p* = 0.003).  
- **No negative impact on growth** (weaning weights: *p* = 0.788).  
- Behavioral shifts: Longer water trough visits and nighttime water intake in nipple-equipped calves.  

**Manuscript Status**: Under review at *Applied Animal Behaviour Science*.  

---

## 🗂️ Repository Structure 
nipple-troughs-calf-welfare/
├── data/ # Raw/processed data (anonymized)
├── scripts/ # Analysis code (R/Python)
│ ├── analysis.R # GLMMs and statistical tests
│ └── visualization.R # ggplot2 scripts for figures
├── figures/ # Manuscript figures (PDF/PNG)
├── docs/ 
├── LICENSE
└── README.md # This file


---

## 🔍 Key Findings  
| Metric               | Nipple-WT Calves | Open-WT Calves | *p*-value |  
|-----------------------|------------------|----------------|-----------|  
| Cross-sucking events/day | 5.04 ± 3.55     | 8.63 ± 3.56    | 0.003     |  
| Event duration (sec)  | 38.0 ± 43.5      | 64.8 ± 57.8    | 0.028     |  
| Weaning weight (kg)   | 62.1 ± 11.2      | 62.7 ± 9.6     | 0.788     |  

*(Full results available in the manuscript.)*

---

## 🛠️ Skills & Tools Demonstrated  
- **Statistical Analysis**: GLMMs (`lme4`), Poisson/Gamma distributions, AIC validation.  
- **Data Visualization**: Heatmaps (temporal behavior), bar plots, growth curves (`ggplot2`).  
- **Reproducibility**: R scripts, raw/processed data (where shareable), pre-registered methods.  
- **Domain Expertise**: Dairy calf welfare, pasture-based systems, practical trade-off analysis.  

---

## 🚀 How to Use This Repository  
1. **Reproduce analysis**:  
   ```bash
   Rscript scripts/analysis.R  # Runs statistical models  
   Rscript scripts/visualization.R  # Generates figures
   ```
   
