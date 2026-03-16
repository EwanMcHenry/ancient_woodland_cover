#render quorto doc for sharepoint
# D:/Users/Ewan McHenry/OneDrive - the Woodland Trust/Analysis/ancient_woodland_cover/AW extent exploration.qmd
library(quarto)


quarto_render("AW extent exploration.qmd",output_file =  
         "AW extent exploration.html")
quarto_render("AW extent exploration.qmd",output_file =  
         "AW extent exploration.aspx")
