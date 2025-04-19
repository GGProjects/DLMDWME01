```{r}
#| eval: false
#| include: false

source_file <- "dict_csv"
target <- "dlmdwme01_uc2_csv.pdf"
target_dir <- paste0(wd, "/00_docs/03_artifacts/02_data_reports/00_dictionaries/")
paper_target <- paste0(wd, "/00_docs/03_artifacts/01_paper/041-Annexes.tex")
rel_paper_target <- "../02_data_reports/00_dictionaries/"
quarto::quarto_render(input = paste0(source_file, ".qmd"),
                      output_file = target)
system(paste0("mv ",target, " ", target_dir))

system(paste0("mv ",source_file, ".tex ", target_dir))

readr::write_lines(paste0("\\input{", rel_paper_target, source_file, "}"), 
                   file = paper_target, 
                   append = TRUE)
```
