# Bargaining Drift in CEO Pay: An Empirical Study of Compensation Dynamics

### Sonia Di Giannatale, Itza Tlaloc Quetzalcoatl Curiel-Cabral, Genaro Basulto.


This repository contains the code for the numerical and econometric analysis in the correspinding paper. This study empirically examines the evolution of CEOs' bargaining power using compensation data from the ExecuComp and Annual Snapshot databases (1999–2022). Findings indicate that CEO age stabilizes bargaining power fluctuations, while firm size shapes its primary drivers: future compensation components, such as option grants, dominate in large-capitalization firms; both current and future pay influence bargaining power in mid-capitalization firms; and salary and stock grants drive shifts in small-capitalization firms. These results enhance our understanding of the persistence of bargaining power in executive compensation, offering insights for policymakers and corporate governance stakeholders on the evolving nature of pay negotiations.

## Features

- Explain exclusion criteria from the data in Compustat Snapshot and ExecuComp databases.
- Code for replicating the regression results in the paper.
- Code for simulating compensaiton paths according to the model.

## Repository Contents

- **data_cleaing:** The program for merging and cleaing the datasets downloaded from Compustat.  
- **regressions:** The program for running the regressions given by equation (3) in the paper using the dataset obtained after the exclusion criteria. 
- **salary_simulations** Code to generate simulations of compensation path following the regression model results.
## How to Use

0. Download the Compustar Annual Snapshot database for all companies available from 1999 to 2022 and the ExecuComp database for all CEOs using the same sample period. 
1. Clone this repository:  
   ```bash
   git clone https://github.com/genarobasulto/Bargaining_Drift
   cd Bargaining_Drift
   ```

2. Install dependencies (if applicable).  

3. Follow the instructions in the documentation to set up input parameters and run the regressions.

4. We prepared an example of usage in all files. 

## Citation

If you use this code, please cite the repository:  
[Bargaining Drift in CEO Pay](https://github.com/genarobasulto/Bargaining_Drift)  
