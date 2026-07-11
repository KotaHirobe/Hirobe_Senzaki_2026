# Hirobe & Senzaki., 2026
R code and data for statistical analysis for the following paper:

"Multimodal human and pet cues intensify wildlife fear responses"

We investigated the effects of human and dog visual and acoustic cues on fear responses in wild sika deer. Dog barking increased alert distance, whereas human voices and a dog decoy increased flight initiation distance. These findings demonstrate that both human multimodal cues and pet cues intensify wildlife fear responses. We also suggest that considering the combined effects of human and pet cues could support more targeted management of recreation and dog walking than blanket restrictions.

We fitted all models in R version 4.4.1 using the ‘lme4’ package version 1.1-35.5 (Bates et al. 2014; R Core Team, 2024). We assessed multicollinearity using the ‘car’ package version 3.1-3 (Fox & Weisberg, 2019), calculated 95% CIs using the ‘MASS’ package version 7.3-60.2 (Venables & Ripley, 2002), calculated the R² values using the ‘performance’ package version 0.15.0 (Lüdecke et al., 2021), and obtained estimated marginal means of AD and FID using the ‘emmeans’ package version 1.10.5 (Lenth, 2024).

Contents:

1. Analysis_Code.R: R script

2. Multimodal_human_pet_fear.Rproj: R project

3. Deer_FID_AD.csv: Dataset 1

4. AcousticAttenuation.csv: Dataset 2

Variable Name Correspondence Table
This table summarizes the correspondence between terms used in the manuscript and variable names used in the dataset or R code.

| Manuscript Term                               | Data/Code Variable Name   |
| --------------------------------------------- | ------------------------- |
| Human voice (presence)                        | `human_acoustic`          |
| Dog decoy (presence)                          | `dog_visual`              |
| Dog barking (presence)                        | `dog_acoustic`            |
| Covered dog decoy (presence)                  | `blinddog_visual`         |
| White noise (presence)                        | `noise_acoustic`          |
| Ambient light level                           | `light`                   |
| SD                                            | `SD`                      |
| Group size                                    | `flock`                   |
| Mean wind speed                               | `AvgWind`                 |
| Season (non-rutting)                          | `season`                  |

