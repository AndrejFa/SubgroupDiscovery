# Subgroup discovery R analysis

Subgroup discovery - SD analysis is a descriptive data mining technique. I had been using SD during Erasmus internship at University of Murcia, Spain and for drug prescription analysis among diabetes patients. Aim of SD application in both projects were discovery of risk factors in relationship to target variable. To confirm already known knowledge or discover some new pattern that had been evolved. Analysis were conducted in `R` and `RStudio` IDE in combination with `LaTeX`, a document preparation system, to build fast reproducible reports of findings. 

**MODEST2** folder shows that kind of reproducible research analysis. MODEL FOR EARLY DETECTION OF DIABETES TYPE 2 - [MODEST](http://www.ri.fzv.um.si/modest2/page1.html) has been a research project on Faculty of health science at University of Maribor, Slovenia. Project was conducted in collaboration with Nova Vizija d.d.. Report in that folder shows midterm report of SD analysis of drug prescriptions to diabetes patients. 

Given that there are 2 packages: `rsubgroup` and `SDEFSR` for SD analysis in `R`, I prepared unified package organization `R` code for SD. In this way a was able to document my code and build some feature on top of this packages that help me to produce fast and easy SD analysis and report of findings/results. 

I had unified: 
- table output that evaluate rules or subgroups based on confusion matrix,
- true positive rate vs false positve rate ploting,
- presentation the results with `LaTeX` and `R` code,
- unified function calls for SD analysis from both packages.

First two feature were build on top of `rsubgroup` package and the last two on top of both packages.

## Notes

This is a representational repository of my work with `R`, `RStudio` and `LaTeX`.

## Acnowledgements
- Izr. prof. dr. Gregor Štiglic, Faculty of health science, University of Maribor, Slovenia,
- dr. Manuel Campos Martinez, Faculty of Informatics, University of Murcia, Spain,
- Doc. dr. Petra Povalej Bržan, Faculty of health science, University of Maribor, Slovenia.