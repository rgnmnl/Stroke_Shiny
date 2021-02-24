# Stroke_Shiny

<!-- ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
``` -->
### Exploring results from an association analysis of the UKBiobank data with ischemic stroke, intracerebral haemorrhage, and subarachnoid haemorrhage.  
##### Paul Auer, Andrea Rau, Regina Manansala  

<br>
<img src="UWMZSPH.png" alt="drawing" width="200px"/>
<img src="UKBB.png" alt="drawing" width="200px"/>
</br>
<!-- ![](UWMZSPH.png) ![](UKBB.png) --> 

<!--```{r, echo=FALSE, out.width='25%', fig.align='center',fig.show='hold'}
knitr::include_graphics(c('UWMZSPH.png','UKBB.png'))
``` -->

This App is intended to provide easy access to summary statistics from a genome-wide association study of the UKBiobank data with ischemic stroke, intracerebral haemorrhage, and subarachnoid haemorrhage. In our ischemic stroke analyses, we included <b>4,474 cases</b> (as algorithmically defined in <b>Field 42008</b> in the UKBiobank) and 24,000 stroke free controls that were also free of myocardial infarction. In our intracerebral haemorrhage analyses, we included <b>959 cases</b> (as algorithmically defined in <b>Field 42010</b> in the UKBiobank) and 4,800 stroke free controls that were also free of myocardial infarction. Our subarachnoid haemorrhage analyses included <b>1,194 cases</b> (as algorithmically defined in <b>Field 42012</b> in the UKBiobank) and 5,970 stroke free controls that were also free of myocardial infarction. All analyses were conducted in PLINK2 utilizing the dosages from the imputed UKBiobank data.

<!--  4,474 ischemic stroke cases | DO FOR HEM STROKES  & FIX LINKS -->

We ran three different models to adjust for different sets of covariates. All models were run within a logistic regression framework that utilized the Firth penalized likelihood to account for rare variants. Model 1 was adjusted for age, sex, and principal components 1-10 (as computed by the UKBiobank). Model 2 was adjusted for age, sex, and principal components 1-10, and hypertension. Model 3 was adjusted for age, sex, and principal components 1-10, hypertension, BMI, and warfarin status. 

Access to the UKBiobank data was granted as part of Project ID 19746, “Development and application of methods for analyzing whole-genome imputed data for hematological traits.”

Please contact Dr. Paul Auer (pauer@uwm.edu) if you have any questions or experience any technical difficulties with the app. 



<br></br>
