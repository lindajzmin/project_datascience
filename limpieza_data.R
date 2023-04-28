

# Cargar librerías --------------------------------------------------------

library(readr)
library(stats)
library(tidyverse)
library(ggplot2)
library(openxlsx)



# Cargar data -------------------------------------------------------------
banco <- read_rds('data/DATA_RDS.rds')   ## se carga el archivo guardado

summary(banco)
glimpse(banco)
str(banco)



# Eliminar variables ------------------------------------------------------
banco <- banco %>% 
    select(-c(FECHA_PROCESO, DIA_PROCESO, MES_PROCESO, 
              ANIO_PROCESO, DIA_CONSUMO, MES_CONSUMO, 
              ANIO_CONSUMO))



# Limpieza de Datos ---------------------------------------------------

## Revisión de transacciones ----------------------------------------------------

## CONSUMOS POR ESPECIALIDAD DE NEGOCIO
## Tabla donde se agrupan las transacciones por línea de negocio y especialidad 
## de negocio
banco %>%
    group_by(LINEA_NEGOCIO, ESPECIALIDAD_NEGOCIO) %>%
    summarise(cantidad = n()) %>%
    arrange(desc(cantidad)) %>% 
    view()

clasificacion1 <- banco %>%
    group_by(LINEA_NEGOCIO, ESPECIALIDAD_NEGOCIO) %>%
    summarise(cantidad = n()) %>%
    arrange(desc(cantidad))

## aquí vemos 284,858 transacciones en linea de negocio no definida y 815 en 
## Mantenimiento de tarjeta y especialidad no definida
write.xlsx(clasificacion1, "data/clasificacion1_negocios.xlsx")

## CONSUMOS POR DESCRIPCIÓN DE NEGOCIO
## Tabla donde se agrupan las transacciones por linea de negocio, especialidad 
## de negocio y por establecimiento
banco %>%
    group_by(LINEA_NEGOCIO, ESPECIALIDAD_NEGOCIO, DES_ESTABLECIMIENTO) %>%
    summarise(cantidad = n()) %>%
    arrange(desc(cantidad)) %>% 
    view()

clasificacion2 <- banco %>%
    group_by(LINEA_NEGOCIO, ESPECIALIDAD_NEGOCIO, DES_ESTABLECIMIENTO) %>%
    summarise(cantidad = n()) %>%
    arrange(desc(cantidad))

## aquí vemos que aparece una categoría de no definido que contiene 123143 
## transacciones
write.xlsx(clasificacion2, "data/clasificacion2_negocios.xlsx")


## Revisión de consumos ----------------------------------------------------

## Tabla que presenta el valor de consumos y cantidad de transacciones por 
## establecimientos de los consumos "NO DEFINIDOS". Aquí vemos que hay valores 
## de la linea de negocio no definido que no podemos eliminar porque tienen un 
## valor alto de consumo.
banco %>% 
    group_by(ESPECIALIDAD_NEGOCIO,DES_ESTABLECIMIENTO) %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n()) %>%
    filter(ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    arrange(desc(total_gasto)) %>% 
    view()

no_definidos <- banco %>% 
    group_by(ESPECIALIDAD_NEGOCIO,DES_ESTABLECIMIENTO) %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n()) %>%
    filter(ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    arrange(desc(total_gasto))

sum(banco$VALOR_TOTAL_VALE)

write.xlsx(no_definidos, "data/consumos_no_definidos.xlsx")



#### Aquí consultamos los establecimientos con mayor transacciones para irlos recategorizando
banco %>% 
    group_by(ESPECIALIDAD_NEGOCIO,DES_ESTABLECIMIENTO) %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n()) %>%
    filter(ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    arrange(desc(total_gasto)) %>% 
    view()


##Aquí vemos que hay valores de la linea de negocio no definido que no podemos eliminar porque tienen un valor alto de consumo.
banco %>% 
    group_by(LINEA_NEGOCIO, ESPECIALIDAD_NEGOCIO) %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n()) %>%
    arrange(desc(LINEA_NEGOCIO, ESPECIALIDAD_NEGOCIO)) %>% 
    view()

banco %>% 
    group_by(LINEA_NEGOCIO, ESPECIALIDAD_NEGOCIO) %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n()) %>%
    arrange(desc(LINEA_NEGOCIO, ESPECIALIDAD_NEGOCIO)) %>% 
    view()


## Registros sin descripción -----------------------------------------------

## Aquí consultamos que los consumos no definidos sin descripción suman 
## 1'341,206, representan 0.25% del total de consumos, en total son 123,143 
## transacciones
banco %>% 
    group_by(ESPECIALIDAD_NEGOCIO,DES_ESTABLECIMIENTO) %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n()) %>%
    filter(ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & DES_ESTABLECIMIENTO == "") %>%
    arrange(desc(total_transacciones)) %>% 
    view()

## Eliminaremos los registros sin descripción, corresponde al 2.04% del total 
## de transacciones y el 0.25% del total de consumos
banco1 <- banco[!(banco$DES_ESTABLECIMIENTO == ""),]



## Recategorizar consumos --------------------------------------------------

### Especialidad NO DEFINIDO ------------------------------------------------

### Aquí consultamos que los consumos no definidos, sin categorizar, suman 
### 1'984,251, en total son 161,715 transacciones.
banco1 %>% 
    group_by(ESPECIALIDAD_NEGOCIO) %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n()) %>%
    filter(ESPECIALIDAD_NEGOCIO == "NO DEFINIDO")


### GOOGLE ------------------------------------------------------------------

### De acuerdo a la DESCRIPCIÓN observamos los consumos NO DEFINIDOS que tienen
### la palabra GOOGLE en su DESCRIPCIÓN de ESTABLECIMIENTO, suman 378,382.3, 
### en total son 54,748 transacciones.
banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, c("GOOGLE")) & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    group_by(LINEA_NEGOCIO, ESPECIALIDAD_NEGOCIO) %>% 
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n()) %>%
    view()

### Verificamos que la categoría LINEA_NEGOCIO == "EDUCACION Y OFICINA" & 
### ESPECIALIDAD_NEGOCIO == "INFORMATICA" es la adecuada para GOOGLE
banco1 %>% 
    filter(LINEA_NEGOCIO == "EDUCACION Y OFICINA" & ESPECIALIDAD_NEGOCIO == "INFORMATICA") %>% 
    group_by(LINEA_NEGOCIO, ESPECIALIDAD_NEGOCIO, DES_ESTABLECIMIENTO) %>% 
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())

### Recategorizar consumos GOOGLE que no están definidos en EDUCACION Y OFICINA 
### e INFORMATICA
banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "GOOGLE")), "ESPECIALIDAD_NEGOCIO"] <- "INFORMATICA"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "GOOGLE")), "LINEA_NEGOCIO"] <- "EDUCACION Y OFICINA"


### Google ------------------------------------------------------------------

### De acuerdo a la DESCRIPCIÓN observamos los consumos NO DEFINIDOS que tienen
### la palabra Google en su DESCRIPCIÓN de ESTABLECIMIENTO, suman 4.32, 
### en total es 1 transacción.
banco1 %>% 
    filter(str_detect(banco$DES_ESTABLECIMIENTO, "Google") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())

### Recategorizar consumos Google que no están definidos en EDUCACION Y OFICINA 
### e INFORMATICA
banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "Google")), "ESPECIALIDAD_NEGOCIO"] <- "INFORMATICA"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "Google")), "LINEA_NEGOCIO"] <- "EDUCACION Y OFICINA"


### BYTEFENCE (antivirus) ---------------------------------------------------------

### De acuerdo a la DESCRIPCIÓN observamos los consumos NO DEFINIDOS que tienen
### la palabra BYTEFENCE en su DESCRIPCIÓN de ESTABLECIMIENTO, suman 244.50, 
### en total son 10 transacciones.
banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "BYTEFENCE") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())

### Recategorizar consumos BYTEFENCE que no están definidos en EDUCACION Y 
### OFICINA e INFORMATICO
banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "BYTEFENCE")), "ESPECIALIDAD_NEGOCIO"] <- "INFORMATICA"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "BYTEFENCE")), "LINEA_NEGOCIO"] <- "EDUCACION Y OFICINA"


### CBA -------------------------------------------------------------------------

### De acuerdo a la DESCRIPCIÓN observamos los consumos NO DEFINIDOS que tienen
### la palabra CBA en su DESCRIPCIÓN de ESTABLECIMIENTO, suman 9,147.55, 
### en total son 179 transacciones.
banco %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "CBA") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())

### Recategorizar consumos CBA que no están definidos en EDUCACION Y 
### OFICINA e INFORMATICA
banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "CBA")), "ESPECIALIDAD_NEGOCIO"] <- "INFORMATICA"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "CBA")), "LINEA_NEGOCIO"] <- "EDUCACION Y OFICINA"


### Certificacion -----------------------------------------------------------------------

### De acuerdo a la DESCRIPCIÓN observamos los consumos NO DEFINIDOS que tienen
### la palabra CBA en su DESCRIPCIÓN de ESTABLECIMIENTO, suman 585.00, 
### en total son 3 transacciones.
banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "Certificacion") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())

### Recategorizar consumos Certificacion que no están definidos en EDUCACION Y 
### OFICINA y ACADEMIAS
banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "Certificacion")), "ESPECIALIDAD_NEGOCIO"] <- "ACADEMIAS"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "Certificacion")), "LINEA_NEGOCIO"] <- "EDUCACION Y OFICINA"



### SeminariosOnline ----------------------------------------------------------------------

### De acuerdo a la DESCRIPCIÓN observamos los consumos NO DEFINIDOS que tienen
### la palabra SeminariosOnline en su DESCRIPCIÓN de ESTABLECIMIENTO, suman 
### 43,016.67 en total son 358 transacciones.
banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "Seminario") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())

### Recategorizar consumos Certificacion que no están definidos en EDUCACION Y 
### OFICINA y ACADEMIAS
banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "Seminario")), "ESPECIALIDAD_NEGOCIO"] <- "ACADEMIAS"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "Seminario")), "LINEA_NEGOCIO"] <- "EDUCACION Y OFICINA"



### MICROSOFT -------------------------------------------------------------------------

### De acuerdo a la DESCRIPCIÓN observamos los consumos NO DEFINIDOS que tienen
### la palabra MICROSOFT*MICROSOFT 365 M en su DESCRIPCIÓN de ESTABLECIMIENTO, suman 
### 95812+13720.11 en total son 3776+633 transacciones.
banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "MICROSOFT") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())

banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "Microsoft") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())

### Recategorizar consumos Certificacion que no están definidos en EDUCACION Y 
### OFICINA e INFORMATICA
banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "MICROSOFT")), "ESPECIALIDAD_NEGOCIO"] <- "INFORMATICA"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "MICROSOFT")), "LINEA_NEGOCIO"] <- "EDUCACION Y OFICINA"

banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "Microsoft")), "ESPECIALIDAD_NEGOCIO"] <- "INFORMATICA"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "Microsoft")), "LINEA_NEGOCIO"] <- "EDUCACION Y OFICINA"



### BOUTIQUE -------------------------------------------------------------------------

### De acuerdo a la DESCRIPCIÓN observamos los consumos NO DEFINIDOS que tienen
### la palabra BOUTIQUE en su DESCRIPCIÓN de ESTABLECIMIENTO, suman 
### 14,490.84 en total son 28 transacciones.
banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "BOUTIQUE") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())

### Recategorizar consumos BOUTIQUE que no están definidos en SUNTUARIOS y JOYERIAS
banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "BOUTIQUE")), "ESPECIALIDAD_NEGOCIO"] <- "JOYERIAS"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "BOUTIQUE")), "LINEA_NEGOCIO"] <- "SUNTUARIOS"



### PLAZA VENDOME -----------------------------------------------------------------------

### De acuerdo a la DESCRIPCIÓN observamos los consumos NO DEFINIDOS que tienen
### la palabra PLAZA VENDOME en su DESCRIPCIÓN de ESTABLECIMIENTO, suman 
### 19,176.39 en total son 12 transacciones.
banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "PLAZA VENDOME") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())

### Recategorizar consumos PLAZA VENDOME que no están definidos en SUNTUARIOS y JOYERIAS
banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "PLAZA VENDOME")), "ESPECIALIDAD_NEGOCIO"] <- "JOYERIAS"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "PLAZA VENDOME")), "LINEA_NEGOCIO"] <- "SUNTUARIOS"



### JOYERIA --------------------------------------------------------------------------

### De acuerdo a la DESCRIPCIÓN observamos los consumos NO DEFINIDOS que tienen
### la palabra JOYERIA en su DESCRIPCIÓN de ESTABLECIMIENTO, suman 
### 10,395.78 en total son 6 transacciones.
banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "JOYERIA") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())

#### Recategorizar consumos JOYERIA que no están definidos en SUNTUARIOS y JOYERIAS
banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "JOYERIA")), "ESPECIALIDAD_NEGOCIO"] <- "JOYERIAS"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "JOYERIA")), "LINEA_NEGOCIO"] <- "SUNTUARIOS"



### TIFFANY --------------------------------------------------------------------------

### De acuerdo a la DESCRIPCIÓN observamos los consumos NO DEFINIDOS que tienen
### la palabra TIFFANY en su DESCRIPCIÓN de ESTABLECIMIENTO, suman 
### 2,987.04 en total son 2 transacciones.
banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "TIFFANY") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())

banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "TIFFANY")), "ESPECIALIDAD_NEGOCIO"] <- "JOYERIAS"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "TIFFANY")), "LINEA_NEGOCIO"] <- "SUNTUARIOS"



### PLAYSTATION --------------------------------------------------------------------------

### De acuerdo a la DESCRIPCIÓN observamos los consumos NO DEFINIDOS que tienen
### la palabra PLAYSTATION en su DESCRIPCIÓN de ESTABLECIMIENTO, suman 
### 204,599+39,322.75 en total son 12263+2381 transacciones.
banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "PLAYSTATION") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n()) 

banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "Playstation") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())  

banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "PLAYSTATION")), "ESPECIALIDAD_NEGOCIO"] <- "ENTRETENIMIENTO ELECTRONICO"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "PLAYSTATION")), "LINEA_NEGOCIO"] <- "DIVERSION"

banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "Playstation")), "ESPECIALIDAD_NEGOCIO"] <- "ENTRETENIMIENTO ELECTRONICO"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "Playstation")), "LINEA_NEGOCIO"] <- "DIVERSION"



### PAYPAL -----------------------------------------------------------------------------

### De acuerdo a la DESCRIPCIÓN observamos los consumos NO DEFINIDOS que tienen
### la palabra PAYPAL en su DESCRIPCIÓN de ESTABLECIMIENTO, suman 
### 128,574.1 en total son 11623 transacciones.
banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "PAYPAL") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())

banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "PAYPAL")), "ESPECIALIDAD_NEGOCIO"] <- "ENTRETENIMIENTO ELECTRONICO"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "PAYPAL")), "LINEA_NEGOCIO"] <- "DIVERSION"



### SPOTIFY -----------------------------------------------------------------------------

### De acuerdo a la DESCRIPCIÓN observamos los consumos NO DEFINIDOS que tienen
### la palabra SPOTIFY en su DESCRIPCIÓN de ESTABLECIMIENTO, suman 
### 37,561.39+589.17 en total son 5261+83 transacciones.
banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "SPOTIFY") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n()) 

banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "Spotify") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())

banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "SPOTIFY")), "ESPECIALIDAD_NEGOCIO"] <- "ENTRETENIMIENTO ELECTRONICO"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "SPOTIFY")), "LINEA_NEGOCIO"] <- "DIVERSION"

banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "Spotify")), "ESPECIALIDAD_NEGOCIO"] <- "ENTRETENIMIENTO ELECTRONICO"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "Spotify")), "LINEA_NEGOCIO"] <- "DIVERSION"



### NINTENDO -----------------------------------------------------------------------------

### De acuerdo a la DESCRIPCIÓN observamos los consumos NO DEFINIDOS que tienen
### la palabra NINTENDO en su DESCRIPCIÓN de ESTABLECIMIENTO, suman 
### 52,599.76+119.19  en total son 2624+3 transacciones.
banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "NINTENDO") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())

banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "Nintendo") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n()) 

banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "NINTENDO")), "ESPECIALIDAD_NEGOCIO"] <- "ENTRETENIMIENTO ELECTRONICO"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "NINTENDO")), "LINEA_NEGOCIO"] <- "DIVERSION"

banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "Nintendo")), "ESPECIALIDAD_NEGOCIO"] <- "ENTRETENIMIENTO ELECTRONICO"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "Nintendo")), "LINEA_NEGOCIO"] <- "DIVERSION"



### EPC*FORTNITE -----------------------------------------------------------------------------

### De acuerdo a la DESCRIPCIÓN observamos los consumos NO DEFINIDOS que tienen
### la palabra EPC*FORTNITE en su DESCRIPCIÓN de ESTABLECIMIENTO, suman 
### 33,200.72  en total son 2550 transacciones.
banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "EPC") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())

banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "EPC")), "ESPECIALIDAD_NEGOCIO"] <- "ENTRETENIMIENTO ELECTRONICO"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "EPC")), "LINEA_NEGOCIO"] <- "DIVERSION"



### STEAMGAMES.COM -----------------------------------------------------------------------------

### De acuerdo a la DESCRIPCIÓN observamos los consumos NO DEFINIDOS que tienen
### la palabra STEAMGAMES.COM en su DESCRIPCIÓN de ESTABLECIMIENTO, suman 
### 17,109.63+8,605.46  en total son 1194+646 transacciones.
banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "STEAM") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())

banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "Steam") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n()) 

banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "STEAM")), "ESPECIALIDAD_NEGOCIO"] <- "ENTRETENIMIENTO ELECTRONICO"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "STEAM")), "LINEA_NEGOCIO"] <- "DIVERSION"

banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "Steam")), "ESPECIALIDAD_NEGOCIO"] <- "ENTRETENIMIENTO ELECTRONICO"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "Steam")), "LINEA_NEGOCIO"] <- "DIVERSION"



### XSOLLA -----------------------------------------------------------------------------

### De acuerdo a la DESCRIPCIÓN observamos los consumos NO DEFINIDOS que tienen
### la palabra XSOLLA en su DESCRIPCIÓN de ESTABLECIMIENTO, suman 
### 4,744.5+10,097.5 en total son 425+947 transacciones.
banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "XSOLLA") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())  

banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "Xsolla") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n()) 

banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "XSOLLA")), "ESPECIALIDAD_NEGOCIO"] <- "ENTRETENIMIENTO ELECTRONICO"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "XSOLLA")), "LINEA_NEGOCIO"] <- "DIVERSION"

banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "Xsolla")), "ESPECIALIDAD_NEGOCIO"] <- "ENTRETENIMIENTO ELECTRONICO"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "Xsolla")), "LINEA_NEGOCIO"] <- "DIVERSION"



### Blizzard -----------------------------------------------------------------------------

### De acuerdo a la DESCRIPCIÓN observamos los consumos NO DEFINIDOS que tienen
### la palabra Blizzard en su DESCRIPCIÓN de ESTABLECIMIENTO, suman 
### 4,703.02 en total son 205 transacciones.
banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "Blizzard") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n()) 

banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "Blizzard")), "ESPECIALIDAD_NEGOCIO"] <- "ENTRETENIMIENTO ELECTRONICO"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "Blizzard")), "LINEA_NEGOCIO"] <- "DIVERSION"




### COMITE -----------------------------------------------------------------------------

### De acuerdo a la DESCRIPCIÓN observamos los consumos NO DEFINIDOS que tienen
### la palabra COMITE en su DESCRIPCIÓN de ESTABLECIMIENTO, suman 
### 30,250.4 en total son 140 transacciones.
banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "COMITE") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())  

banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "COMITE")), "ESPECIALIDAD_NEGOCIO"] <- "SERVICIO  MISCELANEOS PRIV"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "COMITE")), "LINEA_NEGOCIO"] <- "SERVICIOS"



### DGNET -------------------------------------------------------------------------------

### De acuerdo a la DESCRIPCIÓN observamos los consumos NO DEFINIDOS que tienen
### la palabra DGNET en su DESCRIPCIÓN de ESTABLECIMIENTO, suman 
### 13,915.85 en total son 46 transacciones.
banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "DGNET") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())  

banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "DGNET")), "ESPECIALIDAD_NEGOCIO"] <- "SUSCRIPCIONES"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "DGNET")), "LINEA_NEGOCIO"] <- "SERVICIOS"



### ENVATO -------------------------------------------------------------------------------

### De acuerdo a la DESCRIPCIÓN observamos los consumos NO DEFINIDOS que tienen
### la palabra ENVATO en su DESCRIPCIÓN de ESTABLECIMIENTO, suman 
### 5,813.8 en total son 104 transacciones.
banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "ENVATO") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())

banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "ENVATO")), "ESPECIALIDAD_NEGOCIO"] <- "SUSCRIPCIONES"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "ENVATO")), "LINEA_NEGOCIO"] <- "SERVICIOS"



### ADOBE -------------------------------------------------------------------------------

### De acuerdo a la DESCRIPCIÓN observamos los consumos NO DEFINIDOS que tienen
### la palabra ADOBE en su DESCRIPCIÓN de ESTABLECIMIENTO, suman 
### 173.27+7,581.5 en total son 7+299 transacciones.
banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "ADOBE") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())

banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "Adobe") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())

banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "ADOBE")), "ESPECIALIDAD_NEGOCIO"] <- "SUSCRIPCIONES"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "ADOBE")), "LINEA_NEGOCIO"] <- "SERVICIOS"

banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "Adobe")), "ESPECIALIDAD_NEGOCIO"] <- "SUSCRIPCIONES"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "Adobe")), "LINEA_NEGOCIO"] <- "SERVICIOS"



### SCRIBD -------------------------------------------------------------------------------

### De acuerdo a la DESCRIPCIÓN observamos los consumos NO DEFINIDOS que tienen
### la palabra SCRIBD en su DESCRIPCIÓN de ESTABLECIMIENTO, suman 
### 53.94+17,861.51 en total son 6+1961 transacciones.
banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "SCRIBD") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())

banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "Scribd") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())

banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "SCRIBD")), "ESPECIALIDAD_NEGOCIO"] <- "SERVICIOS PROFESIONALES"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "SCRIBD")), "LINEA_NEGOCIO"] <- "SERVICIOS"

banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "Scribd")), "ESPECIALIDAD_NEGOCIO"] <- "SERVICIOS PROFESIONALES"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "Scribd")), "LINEA_NEGOCIO"] <- "SERVICIOS"



### URBANIZACION -------------------------------------------------------------------------------

### De acuerdo a la DESCRIPCIÓN observamos los consumos NO DEFINIDOS que tienen
### la palabra URBANIZACION en su DESCRIPCIÓN de ESTABLECIMIENTO, suman 
### 65,966.18 en total son 453 transacciones.
banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "URBANIZACION") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())

banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "URBANIZACION")), "ESPECIALIDAD_NEGOCIO"] <- "SERVICIO  MISCELANEOS PRIV"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "URBANIZACION")), "LINEA_NEGOCIO"] <- "SERVICIOS"



### CABIFY -------------------------------------------------------------------------------

### De acuerdo a la DESCRIPCIÓN observamos los consumos NO DEFINIDOS que tienen
### la palabra CABIFY en su DESCRIPCIÓN de ESTABLECIMIENTO, suman 
### 1,135+209.72 en total son 162+87 transacciones.
banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "CABIFY") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())

banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "cabify") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())

banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "CABIFY")), "ESPECIALIDAD_NEGOCIO"] <- "CORREO Y TRANSPORTE"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "CABIFY")), "LINEA_NEGOCIO"] <- "VIAJES"

banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "cabify")), "ESPECIALIDAD_NEGOCIO"] <- "CORREO Y TRANSPORTE"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "cabify")), "LINEA_NEGOCIO"] <- "VIAJES"



### SIXT -------------------------------------------------------------------------------

### De acuerdo a la DESCRIPCIÓN observamos los consumos NO DEFINIDOS que tienen
### la palabra SIXT en su DESCRIPCIÓN de ESTABLECIMIENTO, suman 
### 46,845.61+610.79 en total son 87+3 transacciones.
banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "SIXT") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDA") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())

banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "Sixt") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDA") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())

banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDA" & str_detect(banco1$DES_ESTABLECIMIENTO, "SIXT")), "ESPECIALIDAD_NEGOCIO"] <- "CORREO Y TRANSPORTE"
banco1[(banco1$LINEA_NEGOCIO == "MANTENIMIENTO TARJETA" & str_detect(banco1$DES_ESTABLECIMIENTO, "SIXT")), "LINEA_NEGOCIO"] <- "VIAJES"

banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDA" & str_detect(banco1$DES_ESTABLECIMIENTO, "Sixt")), "ESPECIALIDAD_NEGOCIO"] <- "CORREO Y TRANSPORTE"
banco1[(banco1$LINEA_NEGOCIO == "MANTENIMIENTO TARJETA" & str_detect(banco1$DES_ESTABLECIMIENTO, "Sixt")), "LINEA_NEGOCIO"] <- "VIAJES"



### SPIRIT -------------------------------------------------------------------------------

### De acuerdo a la DESCRIPCIÓN observamos los consumos NO DEFINIDOS que tienen
### la palabra SPIRIT en su DESCRIPCIÓN de ESTABLECIMIENTO, suman 
### 165,961.4 en total son 667 transacciones.
banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "SPIRIT") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDA") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())

banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDA" & str_detect(banco1$DES_ESTABLECIMIENTO, "SPIRIT")), "ESPECIALIDAD_NEGOCIO"] <- "LINEAS AEREAS"
banco1[(banco1$LINEA_NEGOCIO == "MANTENIMIENTO TARJETA" & str_detect(banco1$DES_ESTABLECIMIENTO, "SPIRIT")), "LINEA_NEGOCIO"] <- "VIAJES"



### HYATT -------------------------------------------------------------------------------

### De acuerdo a la DESCRIPCIÓN observamos los consumos NO DEFINIDOS que tienen
### la palabra HYATT en su DESCRIPCIÓN de ESTABLECIMIENTO, suman 
### 3,299.06 en total son 10 transacciones.
banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "HYATT") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDA") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())

banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDA" & str_detect(banco1$DES_ESTABLECIMIENTO, "HYATT")), "ESPECIALIDAD_NEGOCIO"] <- "HOTELES"
banco1[(banco1$LINEA_NEGOCIO == "MANTENIMIENTO TARJETA" & str_detect(banco1$DES_ESTABLECIMIENTO, "HYATT")), "LINEA_NEGOCIO"] <- "VIAJES"



### EXTENDEDSTAY -------------------------------------------------------------------------------

### De acuerdo a la DESCRIPCIÓN observamos los consumos NO DEFINIDOS que tienen
### la palabra EXTENDEDSTAY en su DESCRIPCIÓN de ESTABLECIMIENTO, suman 
### 3,299.06 en total son 10 transacciones.
banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "EXTENDEDSTAY") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDA") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())

banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDA" & str_detect(banco1$DES_ESTABLECIMIENTO, "EXTENDED")), "ESPECIALIDAD_NEGOCIO"] <- "HOTELES"
banco1[(banco1$LINEA_NEGOCIO == "MANTENIMIENTO TARJETA" & str_detect(banco1$DES_ESTABLECIMIENTO, "EXTENDED")), "LINEA_NEGOCIO"] <- "VIAJES"



### ROXY -------------------------------------------------------------------------------

### De acuerdo a la DESCRIPCIÓN observamos los consumos NO DEFINIDOS que tienen
### la palabra ROXY en su DESCRIPCIÓN de ESTABLECIMIENTO, suman 
### 858.6 en total es 1 transacción.
banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "ROXY") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDA 3569") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())

banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDA 3569" & str_detect(banco1$DES_ESTABLECIMIENTO, "ROXY")), "ESPECIALIDAD_NEGOCIO"] <- "HOTELES"
banco1[(banco1$LINEA_NEGOCIO == "MANTENIMIENTO TARJETA" & str_detect(banco1$DES_ESTABLECIMIENTO, "ROXY")), "LINEA_NEGOCIO"] <- "VIAJES"



### CENTRO MEDICO -------------------------------------------------------------------------------

### De acuerdo a la DESCRIPCIÓN observamos los consumos NO DEFINIDOS que tienen
### la palabra CENTRO MEDICO en su DESCRIPCIÓN de ESTABLECIMIENTO, suman 
### 1,965.24 en total es 1 transacción.
banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "CENTRO MEDICO") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())

banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "CENTRO MEDICO")), "ESPECIALIDAD_NEGOCIO"] <- "CONSULTORIOS MEDICOS"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "CENTRO MEDICO")), "LINEA_NEGOCIO"] <- "SALUD"



### APPLE -------------------------------------------------------------------------------

### De acuerdo a la DESCRIPCIÓN observamos los consumos NO DEFINIDOS que tienen
### la palabra APPLE en su DESCRIPCIÓN de ESTABLECIMIENTO, suman 
### 260,601.8 en total es 42377 transacción.
banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "APPLE") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n()) 

banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "APPLE")), "ESPECIALIDAD_NEGOCIO"] <- "ALMACEN POR DEPARTAMENTOS"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "APPLE")), "LINEA_NEGOCIO"] <- "ALMACEN POR DEPARTAMENTOS"



### CLARO -------------------------------------------------------------------------------

### De acuerdo a la DESCRIPCIÓN observamos los consumos NO DEFINIDOS que tienen
### la palabra CLARO en su DESCRIPCIÓN de ESTABLECIMIENTO, suman 
### 27,592 en total es 4166 transacción.
banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "CLARO") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())

banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "CLARO")), "ESPECIALIDAD_NEGOCIO"] <- "TELEFONIA Y COMUNICACION"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "CLARO")), "LINEA_NEGOCIO"] <- "TELEFONIA Y COMUNICACION"



### TUENTI -------------------------------------------------------------------------------

### De acuerdo a la DESCRIPCIÓN observamos los consumos NO DEFINIDOS que tienen
### la palabra TUENTI en su DESCRIPCIÓN de ESTABLECIMIENTO, suman 
### 14,098 en total es 2047 transacción.
banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "TUENTI") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())

banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "TUENTI")), "ESPECIALIDAD_NEGOCIO"] <- "TELEFONIA Y COMUNICACION"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "TUENTI")), "LINEA_NEGOCIO"] <- "TELEFONIA Y COMUNICACION"



### NETFLIX -------------------------------------------------------------------------------

### De acuerdo a la DESCRIPCIÓN observamos los consumos NO DEFINIDOS que tienen
### la palabra NETFLIX en su DESCRIPCIÓN de ESTABLECIMIENTO, suman 
### 6,909.41+83.94 en total es 540+6 transacción.
banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "NETFLIX") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())

banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "Netflix") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())

banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "NETFLIX")), "ESPECIALIDAD_NEGOCIO"] <- "TELEFONIA Y COMUNICACION"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "NETFLIX")), "LINEA_NEGOCIO"] <- "TELEFONIA Y COMUNICACION"

banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "Netflix")), "ESPECIALIDAD_NEGOCIO"] <- "TELEFONIA Y COMUNICACION"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "Netflix")), "LINEA_NEGOCIO"] <- "TELEFONIA Y COMUNICACION"



### CIRCLE - gasolineras -------------------------------------------------------------------------------

### De acuerdo a la DESCRIPCIÓN observamos los consumos NO DEFINIDOS que tienen
### la palabra CIRCLE - gasolineras en su DESCRIPCIÓN de ESTABLECIMIENTO, suman 
### 17,300 en total es 18 transacción.
banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "CIRCLE") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())

banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "CIRCLE")), "ESPECIALIDAD_NEGOCIO"] <- "GASOLINERAS"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "CIRCLE")), "LINEA_NEGOCIO"] <- "GASOLINERAS"



### INMO - ferreterías -------------------------------------------------------------------------------

### De acuerdo a la DESCRIPCIÓN observamos los consumos NO DEFINIDOS que tienen
### la palabra CIRCLE - gasolineras en su DESCRIPCIÓN de ESTABLECIMIENTO, suman 
### 8,133.46 en total es 6 transacción.
banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "INMO") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())

banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "INMO")), "ESPECIALIDAD_NEGOCIO"] <- "FERRETERIAS Y CONSTRUCCION"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "INMO")), "LINEA_NEGOCIO"] <- "FERRETERIAS Y CONSTRUCCION"



### HUAWEI  -------------------------------------------------------------------------------

### De acuerdo a la DESCRIPCIÓN observamos los consumos NO DEFINIDOS que tienen
### la palabra HUAWEI en su DESCRIPCIÓN de ESTABLECIMIENTO, suman 
### 2,928.39+877.19  en total es 860+151 transacción.
banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "HUAWEI") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())

banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "Huawei") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())

banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "HUAWEI")), "ESPECIALIDAD_NEGOCIO"] <- "TELEFONIA Y COMUNICACION"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "HUAWEI")), "LINEA_NEGOCIO"] <- "TELEFONIA Y COMUNICACION"

banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "Huawei")), "ESPECIALIDAD_NEGOCIO"] <- "TELEFONIA Y COMUNICACION"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "Huawei")), "LINEA_NEGOCIO"] <- "TELEFONIA Y COMUNICACION"



### BRISMAR BRISAS DEL MAR  -------------------------------------------------------------------------------

### De acuerdo a la DESCRIPCIÓN observamos los consumos NO DEFINIDOS que tienen
### la palabra BRISMAR BRISAS en su DESCRIPCIÓN de ESTABLECIMIENTO, suman 
### 7,799.25  en total es 30 transacción.
banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "BRISMAR") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())

banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "BRISMAR")), "ESPECIALIDAD_NEGOCIO"] <- "SERVICIO  MISCELANEOS PRIV"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "BRISMAR")), "LINEA_NEGOCIO"] <- "SERVICIOS"



### TIENDA MIA  -------------------------------------------------------------------------------

### De acuerdo a la DESCRIPCIÓN observamos los consumos NO DEFINIDOS que tienen
### la palabra TIENDA MIA en su DESCRIPCIÓN de ESTABLECIMIENTO, suman 
### 10,341.59  en total es 44 transacción.
banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "MIA") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())

banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "MIA")), "ESPECIALIDAD_NEGOCIO"] <- "VENTAS POR INTERNET"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "MIA")), "LINEA_NEGOCIO"] <- "VENTA CATALOGO - INTERNET"



### MailChimp    -------------------------------------------------------------------------------

### De acuerdo a la DESCRIPCIÓN observamos los consumos NO DEFINIDOS que tienen
### la palabra MailChimp en su DESCRIPCIÓN de ESTABLECIMIENTO, suman 
### 5,795.73  en total es 87 transacción.
banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "MailChimp") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())

banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "MailChimp")), "ESPECIALIDAD_NEGOCIO"] <- "SERVICIOS DE PUBLICIDAD"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "MailChimp")), "LINEA_NEGOCIO"] <- "SERVICIOS"



### Deezer     ----------------------------------------------------------------------------------

### De acuerdo a la DESCRIPCIÓN observamos los consumos NO DEFINIDOS que tienen
### la palabra Deezer en su DESCRIPCIÓN de ESTABLECIMIENTO, suman 
### 3,582.95  en total es 499 transacciones.
banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "Deezer") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())

banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "Deezer")), "ESPECIALIDAD_NEGOCIO"] <- "SUSCRIPCIONES"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "Deezer")), "LINEA_NEGOCIO"] <- "SERVICIOS"




### EDIFICIO   ----------------------------------------------------------------------------------

### De acuerdo a la DESCRIPCIÓN observamos los consumos NO DEFINIDOS que tienen
### la palabra EDIFICIO en su DESCRIPCIÓN de ESTABLECIMIENTO, suman 
### 3,582.95  en total es 499 transacciones.
banco1 %>% 
    filter(str_detect(banco1$DES_ESTABLECIMIENTO, "EDIFICIO") & ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())

banco1[(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "EDIFICIO")), "ESPECIALIDAD_NEGOCIO"] <- "C0NDOMINIOS"
banco1[(banco1$LINEA_NEGOCIO == "NO DEFINIDO" & str_detect(banco1$DES_ESTABLECIMIENTO, "EDIFICIO")), "LINEA_NEGOCIO"] <- "SERVICIOS"



### BAÑOS TURCOS   ----------------------------------------------------------------------------------

banco[banco$ESPECIALIDAD_NEGOCIO == "GIMNASIOS Y BAÑOS TURCOS", "ESPECIALIDAD_NEGOCIO"] <- "GIMNASIOS Y BANIOS TURCOS"

banco %>% 
    group_by(LINEA_NEGOCIO,ESPECIALIDAD_NEGOCIO) %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n()) %>%
    view()



### ARTICULOS PARA NIÑOS  ----------------------------------------------------------------------------------

banco[banco$ESPECIALIDAD_NEGOCIO == "ARTICULOS PARA NIÑOS", "ESPECIALIDAD_NEGOCIO"] <- "ARTICULOS PARA NINIOS"



### ARTICULOS PARA NIÑOS  ----------------------------------------------------------------------------------

banco[banco$ESPECIALIDAD_NEGOCIO == "SUMIN. AGRICOLAS-VETERINARIOS", "ESPECIALIDAD_NEGOCIO"] <- "SUMIN AGRICOLAS-VETERINARIOS"



## Eliminar sobrantes ------------------------------------------------------
## ELIMINAR SOBRANTES "NO DEFINIDO" y "NO DEFINIDA"
banco1 %>% 
    filter(ESPECIALIDAD_NEGOCIO == "NO DEFINIDO") %>% 
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n())

banco <- banco1[!(banco1$ESPECIALIDAD_NEGOCIO == "NO DEFINIDO"),]

banco %>% 
    group_by(ESPECIALIDAD_NEGOCIO) %>%
    summarise(total_gasto = sum(VALOR_TOTAL_VALE),
              total_transacciones = n()) %>%
    filter(ESPECIALIDAD_NEGOCIO == "NO DEFINIDA")

banco <- banco[!(banco$ESPECIALIDAD_NEGOCIO == "NO DEFINIDA"),]



# Tipos de datos ----------------------------------------------------------
str(banco)
banco1 <- banco
str(banco1)


## Factores ----------------------------------------------------------------

unique(banco1$DES_TIPO_TDC)
banco1$DES_TIPO_TDC <- factor(banco1$DES_TIPO_TDC,
                              levels = c("1-PRINCIPAL NACIONAL",
                                         "2-ADICIONAL NACIONAL",
                                         "3-PRINCIPAL INTERNACIONAL",
                                         "4-ADICIONAL INTERNACIONAL",
                                         "7-CORPORATIVA PRINCIPAL INTERNACIONAL",
                                         "8-CORPORATIVA ADICIONAL INTERNACIONAL",
                                         "11-PRINCIPAL CONVENIO",
                                         "12-ADICIONAL CONVENIO",
                                         "14-ADICIONAL CORPORATIVA CONVENIO"))

unique(banco1$MARCA)
banco1$MARCA <- factor(banco1$MARCA,
                       levels = c("VISA", "MASTERCARD", "DISCOVER"))


unique(banco1$GIRO_NEGOCIO)
banco1$GIRO_NEGOCIO <- factor(banco1$GIRO_NEGOCIO,
                              levels = c("ESPECIALIDADES",
                                         "SERVICIOS FINANCIEROS",
                                         "EFECTIVO EXPRESS",
                                         "REFINANCIACION",
                                         "DIFERIDO ESPECIAL"))

unique(banco1$LINEA_NEGOCIO)
banco1$LINEA_NEGOCIO <- factor(banco1$LINEA_NEGOCIO,
                               levels = c("VIAJES", "SALUD", "SUPERMERCADOS",
                                          "COMIDA", "SUNTUARIOS", "DIVERSION",
                                          "ALMACEN POR DEPARTAMENTOS", "SERVICIOS", 
                                          "TELEFONIA Y COMUNICACION", "QUIMICO Y AGROPECUARIO",
                                          "EDUCACION Y OFICINA", "SEGUROS", "ARTICULOS PARA EL HOGAR",
                                          "REPUESTOS Y TALLERES Y VEHICULOS", "GASOLINERAS",
                                          "FERRETERIAS Y CONSTRUCCION",
                                          "VENTA CATALOGO - INTERNET", "EFECTIVO", 
                                          "REFINANCIACION", "MANTENIMIENTO TARJETA" ))

unique(banco1$ESPECIALIDAD_NEGOCIO)
banco1$ESPECIALIDAD_NEGOCIO <- factor(banco1$ESPECIALIDAD_NEGOCIO)

unique(banco1$DES_TIPO_ESTABLECIMIENTO)
banco1$DES_TIPO_ESTABLECIMIENTO <- factor(banco1$DES_TIPO_ESTABLECIMIENTO)

unique(banco1$DES_GRUPO_TIPO_CREDITO)
banco1 %>% 
    filter(DES_GRUPO_TIPO_CREDITO == "NO DEFINIDO") %>% 
    view()

## recategorizamos "NO DEFINIDO" a "CORRIENTE"
banco1[banco1$DES_GRUPO_TIPO_CREDITO == "NO DEFINIDO", "DES_GRUPO_TIPO_CREDITO"] <- "CORRIENTE"


unique(banco1$REGION)
banco1 %>% 
    filter(REGION == "") %>% 
    view()

## recategorizamos "" a "NO DISPONIBLE"
banco1[banco1$REGION == "", "REGION"] <- "NO DISPONIBLE"

banco1$REGION <- factor(banco1$REGION)

unique(banco1$SEGMENTO_N_2)
## recategorizamos "" a "NO DISPONIBLE"
banco1[banco1$SEGMENTO_N_2 == "", "SEGMENTO_N_2"] <- "NO DISPONIBLE"

banco1$SEGMENTO_N_2 <- factor(banco1$SEGMENTO_N_2)


unique(banco1$NIVEL_ESTUDIOS)
## recategorizamos "" a "NO DISPONIBLE"
banco1[banco1$NIVEL_ESTUDIOS == "", "NIVEL_ESTUDIOS"] <- "NO DISPONIBLE"

banco1$NIVEL_ESTUDIOS <- factor(banco1$NIVEL_ESTUDIOS)


unique(banco1$ESTADO_CIVIL)
## recategorizamos "" a "NO DISPONIBLE"
banco1[banco1$ESTADO_CIVIL == "", "ESTADO_CIVIL"] <- "NO DISPONIBLE"
banco1[banco1$ESTADO_CIVIL == "UNION LIBRE < 2 ANIOS", "ESTADO_CIVIL"] <- "UNION LIBRE"
banco1[banco1$ESTADO_CIVIL == "UNION LIBRE > 2 ANIOS", "ESTADO_CIVIL"] <- "UNION LIBRE"
banco1[banco1$ESTADO_CIVIL == "NO DEFINIDO", "ESTADO_CIVIL"] <- "NO DISPONIBLE"

banco1$ESTADO_CIVIL <- factor(banco1$ESTADO_CIVIL)


unique(banco1$GENERO)
## recategorizamos "" a "NO DISPONIBLE"
banco1[banco1$GENERO == "", "GENERO"] <- "NO DISPONIBLE"
banco1$GENERO <- factor(banco1$GENERO)

str(banco1)



## Fechas ------------------------------------------------------------------

library(lubridate)

fecha1 <- "21JAN2021:00:00:00.000"
class(fecha1)
dmy_hms(fecha1)

## CONVERTIR FECHAS A TIPO posixct
banco1$FECHA_CONSUMO <- dmy_hms(banco1$FECHA_CONSUMO)
banco1$FECHA_NACIMIENTO <- dmy_hms(banco1$FECHA_NACIMIENTO)

banco1$DIA_CONSUMO <- day(banco1$FECHA_CONSUMO)
banco1$MES_CONSUMO <- month(banco1$FECHA_CONSUMO)

banco1$DIA_CUMPLE <- day(banco1$FECHA_NACIMIENTO)
banco1$MES_CUMPLE <- month(banco1$FECHA_NACIMIENTO)
banco1$ANIO_CUMPLE <- year(banco1$FECHA_NACIMIENTO)

banco <- banco1
rm(banco1)


saveRDS(banco, "data/tidy_data.RDS")



tidy_banco <- read_rds('data/tidy_data.RDS')   ## se carga el archivo guardado
str(tidy_banco)
str(banco)

