This folder contains all code necessary to reproduce all statistical results, tables, and figures of the thesis 
and the thesis "An Analysis of Model-Agnostic Interpretability Machine Learning Techniques" itself. 
This text-file briefly explains the folder structure of this USB stick:

- "Data Set": contains the data set which was downloaded from the URL: https://archive.ics.uci.edu/ml/datasets/Abalone
  and the readme-file which explains the variables and original data acquisition/preprocessing procedures in more detail.
- "Latex": contains the latex code which was used to compile the PDF-version of the thesis, the bib.tex-file "ref" 
  which contains the citations referenced in the Latex file, and the "figures"-folder which contains all figures of the thesis.
  Compiling the latex-file "An Analysis of Model-Agnostic Interpretability ML Techniques" yields the PDF-version thereof.
- "PDF": contains the PDF version of the thesis, which was obtained by compiling the Latex-Code in the "Latex" folder.
- "R Code": contains all eight R-Code-files necessary to reproduce all statistical results, tables, and figures of the thesis.
  All statistical calculations of a chapter are structured into their own respective R-Code file in order to clearly arrange all code.
  "0. Descriptive Statistics and Correlation Matrix" contains all code for chapter 4.
  "1. Linear Regression" contains all code for chapter 5.1.
  "2. Decision Trees" contains all code for chapter 5.2.
  "3. Partial Dependence Plots" contains all code for chapter 6.1.
  "4. Individual Conditional Expectation Plots" contains all code for chapter 6.2.
  "5. Accumulated Local Effects Plots" contains all code for chapter 6.3.
  "6. Global Surrogate Models" contains all code for chapter 6.4.
  "7. Local Interpretable Model-Agnostic Explanations" contains all code for chapter 6.5.
  "Effect calculation for observation 28 for figure 3" contains the calculations for the red stars indicating the effect of 
  observation 28 in figure 3.

If there are any further questions please let me know: knoepflephilipp@gmail.com.

-- The end of this document. --