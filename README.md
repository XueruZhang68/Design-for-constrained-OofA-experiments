This repository hosts the R code that implements Constructions 1-3, S1 and S2 and reproduces numerical simulation results from our paper as "Chen, J.B., Lin, D.J.K, Nicholas, R., and Zhang, X.R. (2026). Design and analysis for constrained order-of-addition
experiments".

## Folders
- `Algorithms`: R code of Algorithms 1 and S1-S3
  - Algorithm 1: Threshold accepting algorithm for constructing efficient designs under Model(6) when $\mathcal{C}=\tilde{\mathcal{C}}$
  - Algorithm S1: Randomized Kahn's algorithm for topological sorting
  - Algorithm S2: Transitive reduction algorithm
  - Algorithm S3: A threshold accepting algorithm for constructing efficient designs under Model (6) when $\mathcal C\not=\tilde{\mathcal C}$
  
- `Subroutines`: all subroutines used in Algorithms 1 and S1-S3

- `Case study`:   
  - Survey Questions.docx: details of survey questions

  - OofASurveyCodev2.R: R code for
    - Figure 2
    - Table 5

  - clean_survey_data_4162024.csv: colleted dataset for 
    - Figure 2
    - Table 5

- `Constructions`: R code of Constructions 1-3 and S1-S2 
  - Construction 1: systematic construction of COofA-OAs under Model (2)
  - Construction 2: systematic construction of COofA-OAs under Model (3)
  - Construction 3: systematic construction of COofA-OAs under Model (4) with a single group and a single pairwise constraint
  - Construction S1: systematic construction of COofA-OAs under Model (4)
  - Construction S2: systematic construction of COofA-OAs under Model (5)

- `Numerical results`:   
  - OAs used in Tables: orthogonal arrays used in Constructions to yield the COofA-OAs in Tables 1-2 and S1-S2

  - Figures-code:  R code of Figures 1 and S1
    - Figures 1: R code of Figure 1
    - Figures S1(a): R code of Figure S1(a)
    - Figures S1(b): R code of Figure S1(b)
    
  - Tables-code:  code of Tables 1, 3, S1 and S2
    - Table 1: R code of Table 1
    - Table 3: R code of Table 3
    - Table S1: R code of Table S1
    - Table S2: R code of Table S2
  
  - Figures-data: RData of Figure 1
    - Figure 1.RData
    
  - Tables-data: RData of Table 1 and 3
    - Table 1 (Cases 1-13).RData: RData of Cases 1-13 in Table 1
    - Table 1 (Cases 14-26).RData: RData of Cases 14-26 in Table 1
    - Table 3 (Cases 1-18).RData: RData of Cases 1-18 in Table 3
    - Table 3 (Cases 19-27).RData: RData of Cases 19-27 in Table 3

  - ta_kahn_random_unfixed: a simplified version of Algorithm S3 is used for the cases with free group orders in Tables 1 and 3.


