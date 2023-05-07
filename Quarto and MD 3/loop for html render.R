library(quarto)

for (i in 1:8){
  quarto:: quarto_render(
    input="Arshad_assignment_27_04.qmd",
    output_file =paste0("Arshad_GOT Season", i,".html", sep=""),
    execute_params = list(
      season=i
    )
  )
}
  



