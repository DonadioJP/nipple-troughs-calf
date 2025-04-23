# Nipple Water Troughs Reduces Cross-Sucking in Dairy Calves  

![Example Visualization](figures/Calves_water.png)  

## 📜 Study Overview  
This project evaluated how nipple-based water troughs influence cross-sucking behavior and growth in group-housed dairy calves raised on pasture. Key findings include:  
- **42% reduction in cross-sucking** in calves with nipple access vs. open troughs (*p* = 0.003).  
- **No negative impact on growth** (weaning weights: *p* = 0.788).  
- Behavioral shifts: Longer water trough visits and nighttime water intake in nipple-equipped calves.  

**Manuscript Status**: Under review at *Applied Animal Behaviour Science*.  

---

## 🗂️ Repository Structure 
nipple-troughs-calf-welfare/
├── data/                 # Raw/processed data
│   ├── raw/              # Original behavioral scans
│   └── processed/        # Cleaned datasets
├── scripts/
│   ├── analysis.R        # GLMMs
│   └── visualization.R   # ggplot2 figures
├── figures/              # Manuscript figures
├── docs/                 # Quarto report
├── LICENSE               # MIT License
└── README.md


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
   
