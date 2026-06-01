This repository hosts the R code that implements Constructions 1-3, S1 and S2 and reproduces numerical simulation results from our paper as "Chen, J.B., Lin, D.J.K, Nicholas, R., and Zhang, X.R. (2026). Design and analysis for constrained order-of-addition
experiments".

## Folders
- `Algorithms`: R code of Algorithms 1 and S1-S3
  - Algorithms 1: Threshold accepting algorithm for constructing efficient designs under Model(6) when $\mathcal{C}=\tilde{\mathcal{C}}$
  - Algorithms S1: Randomized Kahn's algorithm for topological sorting
  - Algorithms S2: Transitive reduction algorithm
  - Algorithms S3: A threshold accepting algorithm for constructing efficient designs under Model (6) when $\mathcal C\not=\tilde{\mathcal C}$
    
- `LACE`: R code of Algorithms 0<sub>L</sub>-3<sub>L</sub>
  - A0<sub>L</sub>: The best collection of $m$ columns from $\mathcal{L}{(D_0)}$ by  simulated annealing
  - A1<sub>L</sub>: The $D_0$-based algorithm （LACE version）
  - A2<sub>L</sub>: The $D_1$-based algorithm （LACE version）
  - A3<sub>L</sub>: The $(D_1,D_1')$-based algorithm （LACE version）
  
- `Subroutines`: all subroutines used in ACE/LACE


