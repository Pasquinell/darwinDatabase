
# Codigo para extraer bases de datos de darwin.edu.ar
############ Contexto de consultor?a a Cienciambiental ##################

# Autor: Pasquinell Urbani
# Inicio del codigo: 2016-03-03

# Extraccion de bases de dator de la pagina www.darwin.edu.ar
# El objetivo final es ingresar un xls con los binomios de las especies y que la salida
# sea un xls con los nombres de las especies y todas las caracteristicas que presenten cada
# una dentro de la p?gina


# Se obtienen los datos con el paquete rvest
# Informaci?n sobre el paquete rvest: http://www.r-bloggers.com/rvest-easy-web-scraping-with-r/
# Un buen ejemplo con rvest file:///C:/Users/pasquinell/Documents/R/win-library/3.2/rvest/doc/selectorgadget.html
# http://www.darwin.edu.ar/Proyectos/FloraArgentina/Especies.asp

# para saber como extraer los datos de la p?gina, consultar en:
# https://cran.r-project.org/web/packages/rvest/vignettes/selectorgadget.html



# Info temp
# Seguir link en rvest http://htmlasks.com/following_ldquo_nextrdquo_link_with_relative_paths_using_rvest




install.packages("rvest")
install.packeges("xml2")
library(xml2)
library(rvest)
setwd("C:/Users/pasquinell/Desktop")
#setwd("/Users/Alex/Dropbox/AO/Darwinion_CODIGO")

###################### 1  Extraccion de datos de darwin.edu.ar #######

# Cargo la pagina

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
  print("Explorando direccion web")
  print(string)
  darwin = read_html(string)
  print(c("darwin_string", darwin))
  # Escojo el nodo especifico (para encontrarlo se us? el programa SelecgorGadget)
  ##########species_raw = html_nodes(darwin, "#content td a")
  species_raw = html_nodes(darwin, "td a")
  # Con la funcion html_text limpio todo el codigo html(?) para obtener la lista de especies limpia
  species_clean_i = html_text(species_raw)
  # Con la funci?n html_attr obtengo el atributo "href", que corresponde al la direccion web de las especies
  species_dir_i = html_attr(species_raw, "href")
  # acumulo species_raw
  species_clean = c(species_clean, species_clean_i)
  print(species_clean)
  # acumulo species dir
  species_dir = c(species_dir, species_dir_i)
  }







#########  SI LO CORRI UNA VEZ, PARTIR DESDE ACA   ################

##################### 2  Carga de archivo y conversi?n en lista #######

# Carga del archivo
species_string <- read.table("lista_entrada.csv",
                 header = FALSE,
                 sep="/",
                 strip.white = TRUE,
                 na.strings = "EMPTY")
#typeof(species_string)
# a la lista la convierto en vector para poder usarla en el for que viene
species_string = unlist(species_string)
#species_string = c("Azorella diversifolia","Axonopus obtusifolius")


######################## 3    Matching de especies y guardado de caracteristicas ################

df_raw = c()
df_error = c()
col_scientif = c()
col_status = c()
col_dir = c()
species_string = toupper(species_string)
species_clean = toupper(species_clean)
print(species_clean)
# Corremos un for para cada una de las especies del archivo que cargamos
for (j in species_string){
  print(c('j is', j))
  print(c('species_clean is', species_clean))
  aa = grep(j, species_clean, value=TRUE)
  if (length(aa)==0){
    df_error = c(df_error,j)
    print(paste("la especie", j, "esta mal escrita"))}
  # Grep puede encontrar dos subespecies distintas dentro de una especie, por eso se
  # hace el siguiente for, para pasar por todas las subespecies en aa
  for (i in aa){
    sp_number = match(i, species_clean)
    a = species_dir[sp_number] 
    # Le adhiero la direccion base de la pagina
    print(a)
    new_dir = paste("http://www.darwin.edu.ar", a, sep="")
    # Obtengo el html de la especie que llam?
    #specie_particular = read_html(new_dir)
    specie_particular = read_html(iconv(new_dir, to = "UTF-8"), encoding = "utf8")
    # Extraigo las caracteristicas de cada especie del html
    info_raw = html_nodes(specie_particular, "table:nth-child(4) td")
    dato_extra_raw = html_nodes(specie_particular, "tr:nth-child(1) font")
    # Refino de lo anterior la informaci?n relevante que no es codigo html
    info_refine = html_text(info_raw)
    dato_extra = html_text(dato_extra_raw)
    col_scientif = c(col_scientif, dato_extra[1])
    col_status = c(col_status, dato_extra[2])
    col_dir = c(col_dir, new_dir)
    # Creo el vector vec para despu?s rellenarlo con las caracteristicas de cada especie
    vec <- vector(mode="numeric", length=21)
    # Al primer elemento del vector le llamo como la especie que esta buscando
    vec[1]=i
    k= 1 
    l=2
    # Dado que la infomaci?n relevante esta cada dos cuadros realizo el siguiente while para
    # rescatar la informaci?n relevante y pegarla en los slots de vec
    while(k<21){vec[k+1] = info_refine[l] ;k = k+1;l=l+2}
    # Voy acumulando los vec en df_raw
    df_raw = rbind(df_raw, vec)
 
  }
}

# Remuevo las columnas de df_raw
rownames(df_raw) <- NULL

# Les asigno un nombre a las columnas de df raw
colnames(df_raw) = c("Especies","Familia","Genero", "Especie", "Sigla sp.", 
                     "Subespecie", "Sigla ssp", "Variedad", "Sigla Var", "Forma",
                     "Sigla f.", "Publicado en", "Volumen", "Paginas", "A?o", 
                     "Habito", "Status", "Elevacion (m s.m.)",
                     "Distribuci?n Argentina", "Pa?ses lim?trofes", "Notas")

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

print('############## Terminó de correr #####################')