# Code to extract databases from darwin.edu.ar
############ Client: Cienciambiental (Environmental consultancy) ##################

# Author: Pasquinell Urbani
# Date: 2016-03-03

# Databases extraction from www.darwin.edu.ar
# The final objective is to enter an xls. file with the binomials of the species and that the output
# An xls. contains the names and all the characteristics of the species within the website

# Data is obtained with rvest package
# Information of rvest package: http://www.r-bloggers.com/rvest-easy-web-scraping-with-r/
# A good example with rvest file:///C:/Users/pasquinell/Documents/R/win-library/3.2/rvest/doc/selectorgadget.html
# http://www.darwin.edu.ar/Proyectos/FloraArgentina/Especies.asp

# To learn how to extract the data from the page, check in:
# https://cran.r-project.org/web/packages/rvest/vignettes/selectorgadget.html


# Info temp
# Follow link at rvest http://htmlasks.com/following_ldquo_nextrdquo_link_with_relative_paths_using_rvest


install.packages("rvest")
install.packeges("xml2")
library(xml2)
library(rvest)
setwd("C:/Users/pasquinell/Desktop")

###################### 1 Data extraction from darwin.edu.ar #######

# Load website

letters = c("A", "B", "C","D","E","F", "G", "H", "I", "J", "K","L", "M", "N", "O",
            "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z")
#letters = c("A", "B")
species_clean = c()
species_clean = c()
species_dir = c()
for (let in letters){
  #darwin = read_html("http://www.darwin.edu.ar/Proyectos/FloraArgentina/Especies.asp")
  string = sprintf( "http://www.darwin.edu.ar/Proyectos/FloraArgentina/Especies.asp?Letra=%s", let)
  print(string)
  #darwin = read_html("http://www.darwin.edu.ar/Proyectos/FloraArgentina/Especies.asp?Letra=A")
  print("Exploring web direction")
  print(string)
  darwin = read_html(string)
  print(c("darwin_string", darwin))
  # Choose the specific node (it can be found using the software: SelecgorGadget)
  ##########species_raw = html_nodes(darwin, "#content td a")
  species_raw = html_nodes(darwin, "td a")
  # With the tool html_text clean all the code html(?) to obtain the species list
  species_clean_i = html_text(species_raw)
  # through the function html_attr obtain the attribute "href", that corresponds to the species??? website
  species_dir_i = html_attr(species_raw, "href")
  # Append species_raw
  species_clean = c(species_clean, species_clean_i)
  print(species_clean)
  # Append species dir
  species_dir = c(species_dir, species_dir_i)
}



#########  IF THE CODE IS RAN ONCE, START HERE   ################

##################### 2 File upload and list conversion #######

# Upload file
species_string <- read.table("lista_entrada.csv",
                             header = FALSE,
                             sep="/",
                             strip.white = TRUE,
                             na.strings = "EMPTY")
#typeof(species_string)
# list must be converted into a vector in order to use it in the next loop 
species_string = unlist(species_string)
#species_string = c("Azorella diversifolia","Axonopus obtusifolius")


######################## 3    Species matching and characteristics saving ################

df_raw = c()
df_error = c()
col_scientif = c()
col_status = c()
col_dir = c()
species_string = toupper(species_string)
species_clean = toupper(species_clean)
print(species_clean)
# Run one code loop for each species of the uploaded file 
for (j in species_string){
  print(c('j is', j))
  print(c('species_clean is', species_clean))
  aa = grep(j, species_clean, value=TRUE)
  if (length(aa)==0){
    df_error = c(df_error,j)
    print(paste("la especie", j, "esta mal escrita"))}
  # Grep can find two different subspecies within one species. As a result, run the next loop to consider all the subspecies in aa 
  for (i in aa){
    sp_number = match(i, species_clean)
    a = species_dir[sp_number] 
    # Add the base website direction
    print(a)
    new_dir = paste("http://www.darwin.edu.ar", a, sep="")
    # Obtain the html of the selected species
    #specie_particular = read_html(new_dir)
    specie_particular = read_html(iconv(new_dir, to = "UTF-8"), encoding = "utf8")
    # Extract the characteristics of each species from the html
    info_raw = html_nodes(specie_particular, "table:nth-child(4) td")
    dato_extra_raw = html_nodes(specie_particular, "tr:nth-child(1) font")
    # Refine the relevant information that is not part of the html code 
    info_refine = html_text(info_raw)
    dato_extra = html_text(dato_extra_raw)
    col_scientif = c(col_scientif, dato_extra[1])
    col_status = c(col_status, dato_extra[2])
    col_dir = c(col_dir, new_dir)
    # Create vector ???vec??? to fill in with the characteristics of each species 
    vec <- vector(mode="numeric", length=21)
    # Name the first element of the vector with the species name of interest
    vec[1]=i
    k= 1 
    l=2
    # Since the relevant information is every two frames,  the next loop is made for pasting it in the ???vec??? slots 
    # rescatar la informaci?n relevante y pegarla en los slots de vec
    while(k<21){vec[k+1] = info_refine[l] ;k = k+1;l=l+2}
    # Vectors ???vec??? are accumulated in df_raw
    df_raw = rbind(df_raw, vec)
  }
}

# Remove columns from df_raw
rownames(df_raw) <- NULL

# Allocate names to the columns of df_raw
colnames(df_raw) = c("Especies","Familia","Genero", "Especie", "Sigla sp.", 
                     "Subespecie", "Sigla ssp", "Variedad", "Sigla Var", "Forma",
                     "Sigla f.", "Publicado en", "Volumen", "Paginas", "Anho", 
                     "Habito", "Status", "Elevacion (m s.m.)",
                     "Distribucion Argentina", "Paises limitrofes", "Notas")

new_cols = cbind(col_scientif, col_status, col_dir )
colnames(new_cols) = c("Nombre cientifico","Condicion","Direccion")

df_raw = cbind(df_raw,new_cols)
df = as.data.frame(df_raw)
df$Especie = NULL

df =  df[c("Especies","Familia","Genero", "Nombre cientifico", "Status","Habito","Sigla sp.", 
           "Subespecie", "Sigla ssp", "Variedad", "Sigla Var", "Forma",
           "Sigla f.", "Publicado en", "Volumen", "Paginas", "A?o", 
           "Elevacion (m s.m.)",
           "Distribucion Argentina", "Paises limitrofes", "Notas", "Condicion",
           "Direccion")]

write.table(df, file = "coincidencias.csv",row.names=FALSE, na="",col.names=TRUE, sep=",")
write.table(df_error, file = "species_error.csv",row.names=FALSE, na="",col.names=FALSE, sep=",")

print('############## The End ########')