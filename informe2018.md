![](https://github.com/Giremx/justiciareproductiva/blob/master/logo-chico.png)

# La pieza faltante 
## Justicia reproductiva
### Grupo de Información en Reproducción Elegida

***

El siguiente documento contiene el código utlizado para analizar y visualizar los datos publicados en nuestro informe _La pieza faltante_.

Todas las bases de datos -utilizadas y construídas- se encuentran en nuestro repositorio. Por lo tanto, si trabajas con este código, las ligas a éstas serán suficientes: no es necesario descargarlas, a menos de que así lo desees.

Éste es un esfuerzo para brindar más información de forma más transparente.

Los temas se dividieron de la siguiente manera:
* Embarazo adolescente
* Penalización del aborto
* Violencia obstétrica
* Muerte materna
* Seguridad social incompleta

***

### Paquetes requeridos
Forma arcaica de prender paquetes: usamos el comando library() o require(). Acá debemos ser cuidadosas, pues si no tenemos instalados los paquetes el comando no funcionará.
```{r}
# Estas dos bibliotecas las usamos para abrir bases de datos en distintos formatos
library(haven)
library(foreign)
library(Hmisc)
# Estas bibliotecas probablemente sean las más importantes: nos permiten manejar, transformar y analizar bases de datos
library(dplyr)
library(tidyr)
# Los siguientes paquetes se usan para visualización de datos
library(ggplot2)
library(ggalt)
library(ggthemes)
library(devtools)
# También tenemos que cargar paquetes que nos permitan visualizar mapas
library(maps)
library(mapdata)
library(ggmap)
# Estas bibliotecas son necesarias para el análisis adecuado de bases de datos generadas a partir de encuestas
library(forcats)
library(survey)
library(GDAtools)
# Las siguientes librerías sirven para extraer datos de un PDF
library(tm)
library(pdftools)
library(stringr)
library(janitor)
# library(data.table) pendiente
```

Un hack para evitar enredos: usar el paquete "pacman". Pacman instala, en caso de ser necesario, y prende el paquete.
```{r}
# Instalamos pacman
# install.packages("pacman")
library(pacman)
p_load(tidyverse, # el universo tidy carga varios paquetes: dplyr, tidyr, ggplot, etc
       foreign, Hmisc, # para abrir
       ggalt, ggthemes, devtools, maps, mapdata, ggmap, # para visualizar
       survey, GDAtools, # para análisis de encuestas
       tm, pdftools, stringr, janitor) # para extraer datos de un PDF.
```

Dos comandos bien importantes:
```{r}
# Este comando permite que R lea la "ñ" y los acentos.
Sys.setlocale("LC_ALL", "es_ES.UTF-8")
# Este comando desabilita la notación científica.
options(scipen=999)
```

Empecemos con algo sencillo que —si nuestras bases lo requieren— nos será muy útil: una pequeñita base de datos que contenga las claves y nombres de las entidades federativas (Fuente: INEGI), en caso de que las bases de datos que descarguemos no la contengan o que los nombres sean muy largos.
```{r}
entidades <- read.csv("https://raw.githubusercontent.com/Giremx/justiciareproductiva/master/entidades.csv")
```

Esta base tiene tres problemas: el primero, los nombres de las columnas son confusos; el segundo, hay nombres muy largos; el tercero, la columna con las claves por entidad está en formato número que no nos sirve. Atendamos los problemas.

Primero los nombres de las columnas:
```{r}
nombres_entidades <- c("cve_ent", "ent", "ent_abrv")
colnames(entidades) <- nombres_entidades
rm(nombres_entidades)
```

Segundo, vamos a cambiar un par de nombres para practicar la función "mapvalues" de plyr. Este paquete es conflictivo... entonces lo prenderemos y apagaremos en este mismo código
```{r}
require(plyr)
entidades$ent <- mapvalues(entidades$ent, 
                           from = c("Coahuila de Zaragoza", "Michoacán de Ocampo", "Veracruz de Ignacio de la Llave"),
                           to = c("Coahuila", "Michoacán", "Veracruz"))
detach(package:plyr)
```

Por último, cambiemos el formato de "cve_ent". Ojo acá: para las entidades del 1 al 9 hay que agregarles un "0" antes.
```{r}
# Usemos formatC
entidades$cve_ent <- formatC(entidades$cve_ent, width = 2, # mínimo ancho
                             format="d", # si no tiene el anco mínimo, en dónde le pongo los 0s (adelante)
                             flag="0") # con qué lo sustituye
entidades$cve_ent <- as.character(entidades$cve_ent)
```

¡Empecemos nuestro análisis!

***

### Embarazo adolescente
Este apartado tiene varias fuentes de información; seamos ordenadas y exploremos conforme se presenta en nuestro informe impreso.

#### Violencia sexual
¿Existe una relación entre embarazo adolescente y violencia sexual? 
Utilicemos la Encuesta Nacional sobre la Dinámica de las Relaciones (ENDIREH 2016) para tratar de contestar esta pregunta. Recordemos: el marco muestral de esta encuesta son mujeres mayores de 15 años de edad; la representatividad es nacional y estatal -es posible desglosar la información por entidad federativa.

Una observación antes de abrir esta (y otras bases de datos): con el objetivo de optimizar el espacio usado en nuestra nube, la base de datos disponible en el repositorio sólo cuenta con las variables que se utilizan en éste u otros apartados.

Desde acá puedes descargar la [ENDIREH 2016](http://www.beta.inegi.org.mx/contenidos/proyectos/enchogares/especiales/endireh/2016/microdatos/bd_mujeres_endireh2016_sitioinegi_stata.zip); para seleccionar las variables que usamos, corre el siguiente código:
```{r}
endireh2016 <- select(original, # dataframe que contiene la ENDIREH completa
                 id_muj, cve_ent, nom_ent, cve_mun, nom_mun, # id y geográficas
                 fac_muj, upm, estrato, # necesarias para estimaciones
                 edad, p2_13, niv, p2_10, # sociodem (edad, condición laboral, escolaridad, auto adscripción indígena)
                 p9_2, p9_7, # embarazo y lugar de atención médica del embarazo
                 starts_with("p11_12"), starts_with("p11_13"), # violencia sexual
                 p6_1, starts_with("p6_6"), # alguna vez estudió y violencia sexual en la escuela
                 starts_with("p9_8"), # violencia obstétrica
                 p12_2, p12_6, p12_7) # tuvo un embarazo adolescente, edad y consentimiento de primera relación sexual
```

Abramos la bases de datos
```{r}
endireh2016 <- read.csv("https://raw.githubusercontent.com/Giremx/justiciareproductiva/master/endireh_limpia.csv")

# Quedémonos sólo con las adolescentes
emb_ado <- subset(endireh2016, edad<20)
weight <- emb_ado$fac_muj
names(emb_ado)[names(emb_ado) == 'p9_2'] <- 'embarazo'
```

El 14.2% de las mujeres entre 15 y 19 años de edad, reportaron haber estado embarazadas en los últimos 5 años.
```{r}
prop.wtable(emb_ado$embarazo, w = weight, digits = 2, na = FALSE)
```

Violencia en la infancia
Seleccionemos nuestras variables:
```{r}
viosex <- select(endireh2016, 
                 starts_with("p11_12"),
                 starts_with("p11_13"), fac_muj, upm, estrato)
```

Creemos la variable "alguna" que refiere a si la mujer sufrió o no algún tipo de violencia sexual durante la infancia; para esto usaremos "ifelse":
```{r}
viosex$alguna <- ifelse(viosex$p11_12_1>1 & viosex$p11_12_2>1 & viosex$p11_12_3>1 & viosex$p11_12_4>1 & viosex$p11_12_5>1 & viosex$p11_12_6>1, 
                     0, 1)
```

Añadamos un par de variables sociodemográficas
```{r}
viosex$escolaridad <- endireh2016$niv
viosex$embarazo <- ifelse(endireh2016$p9_2==1,1,0)
viosex$embado <- endireh2016$p12_2
viosex$embado <- as.numeric(viosex$embado)
viosex$edad <- endireh2016$edad
viosex$indigena <- endireh2016$p2_10
viosex$indigena <- ifelse(viosex$indigena>2,2,1)
# Acá hay que ser extra cuidadosas con los missing values
viosex$edadsex <- endireh2016$p12_6
viosex$edadsex <- as.numeric(viosex$edadsex)
viosex$edadsex[viosex$edadsex==1099] <- NA
viosex$edadsex[viosex$edadsex==0] <- NA
viosex$edadsex[viosex$edadsex==109] <- NA
viosex$edadsex[viosex$edadsex==108] <- NA
viosex$consentimiento <- endireh2016$p12_7
viosex$consentimiento <- as.numeric(viosex$consentimiento)
# Las siguientes variables deben ser numéricas para poder hacer condiciones; éstas refieren a si la mujer sufrió algún tipo de violencia sexual y por parte de quién
viosex$p11_13_1_1 <- as.numeric(viosex$p11_13_1_1)
viosex$p11_13_1_2 <- as.numeric(viosex$p11_13_1_2)
viosex$p11_13_1_3 <- as.numeric(viosex$p11_13_1_3)

viosex$p11_13_2_1 <- as.numeric(viosex$p11_13_2_1)
viosex$p11_13_2_2 <- as.numeric(viosex$p11_13_2_3)
viosex$p11_13_2_3 <- as.numeric(viosex$p11_13_2_3)

viosex$p11_13_3_1 <- as.numeric(viosex$p11_13_3_1)
viosex$p11_13_3_2 <- as.numeric(viosex$p11_13_3_2)
viosex$p11_13_3_3 <- as.numeric(viosex$p11_13_3_3)

viosex$p11_13_4_1 <- as.numeric(viosex$p11_13_4_1)
viosex$p11_13_4_2 <- as.numeric(viosex$p11_13_4_2)
viosex$p11_13_4_3 <- as.numeric(viosex$p11_13_4_3)

viosex$p11_13_5_1 <- as.numeric(viosex$p11_13_5_1)
viosex$p11_13_5_2 <- as.numeric(viosex$p11_13_5_2)
viosex$p11_13_5_3 <- as.numeric(viosex$p11_13_5_3)

viosex$p11_13_6_1 <- as.numeric(viosex$p11_13_6_1)
viosex$p11_13_6_2 <- as.numeric(viosex$p11_13_6_2)
viosex$p11_13_6_3 <- as.numeric(viosex$p11_13_6_3)
```

¿Existe relación entre la edad de primer embarazo y haber sufrido violencia sexual durante la infancia?
De las mujeres que tuvieron un hijo antes de los 20 años, el 11.82% sufrió algún tipo de violencia sexual.
```{r}
# Quedémonos con las mujeres que tuvieron un embarazo antes de los 20
ado <- subset(viosex, embado<20)
prop.wtable(ado$alguna, w = ado$fac_muj, digits = 2, na = FALSE)
```

Hagamos una gráfica: violencia sexual en la infancia por edad de primer embarazo.
```{r}
# Ésta es nuestra primera pipa en dplyr; como veremos más adelante, hacer esto es muchísimo más fácil que rbindear muchos prop.wtable().
ado_alguna <- ado %>% # base de datos que transformaremos
  select(alguna, fac_muj, embado)%>% # variables seleccionadas
  group_by(embado) %>% # variable por la que se agrupará
  summarise(total_alguna = sum(alguna*fac_muj, na.rm = TRUE), # totales al tomar en cuenta el factor de expansión
            fac_muj = sum(fac_muj, na.rm = TRUE), # factor de expansión
            porcentaje=(total_alguna/fac_muj)*100)%>% # porcentajes al tomar en cuenta el factor de expansión
  select(embado, porcentaje) # variables con las que nos quedamos

# Colores
col1 = "#D9E1F1" 
col2 = "#325694"
fiuf <- "Porcentaje de mujeres que reportó haber sufrido un tipo de violencia sexual en su infancia, por edad de primer embarazo"
ggplot(data = ado_alguna,
       aes(x = reorder(embado, porcentaje),
           y = porcentaje,
           fill = porcentaje)) + 
  geom_col() +
  scale_fill_gradient(low = col1, high = col2) +
  geom_text(aes(label = paste0(round(porcentaje,1),"%")),
            size = 5, position= position_stack(vjust = 0.5)) +
  labs(title = str_wrap(fiuf, width = 75),
       fill = "%") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank()) +
  coord_flip()
```
![](https://github.com/Giremx/justiciareproductiva/raw/master/graficas_informe2018/embado_viosex_edad.png)

¿Hay relación? Estimemos un modelo lineal sencillo.
```{r}
# Diseño del modelo
design <- svydesign(data = ado, # base de datos
                    id=~upm, # ID
                    strata = ado$estrato, # estrato
                    weights =~ fac_muj, # peso
                    nest = T)
svymod1 <- svyglm(alguna ~ # variable dependiente
                    embado, # variable independiente; ojo, acá hacen falta variables de control
                     design = design, # diseño del modelo
                  family = "binomial") # familia binomial porque "alguna" es dummy
```

Entre las mujeres que reportaron un embarazo adolescente, parece haber una relación negativa: entre mayor se sea al momento de tener la primer relación sexual, se tiende a no haber sufrido algún tipo de violencia sexual durante la infancia:
```{r}
p_load(stargazer) # este paquete imprime bonito los outputs
stargazer(svymod1, type = "text")
```

Ahora, analicemos a los agresores. Acá, a diferencia del código anterior, usaremos prop.wtable(). ¿La razón? observar lo maravilloso que es dplyr.
```{r}
# Quedémonos con sólo aquellas mujeres que sí sufrieron un tipo de violencia sexual durante la infancia
quienes <- subset(ado, alguna == 1)
quienes <- select(quienes, starts_with("p11_13"), fac_muj)
quienes[is.na(quienes)] <- 0

# Las siguientes condiciones son necesarias por dos razones: primero, que hay muchas formas de violencia sexual; segundo, que la encuesta permite listar hasta tres tipos de agresor.
quienes$padre <- ifelse(quienes$p11_13_1_1==1 | 
                        quienes$p11_13_1_2==1 | 
                        quienes$p11_13_1_3==1 | 
                          quienes$p11_13_2_1==1 | 
                          quienes$p11_13_2_2==1 | 
                          quienes$p11_13_2_3==1 | 
                            quienes$p11_13_3_1==1 | 
                            quienes$p11_13_3_2==1 | 
                            quienes$p11_13_3_3==1 | 
                              quienes$p11_13_4_1==1 | 
                              quienes$p11_13_4_2==1 | 
                              quienes$p11_13_4_3==1 | 
                                quienes$p11_13_5_1==1 | 
                                quienes$p11_13_5_2==1 | 
                                quienes$p11_13_5_3==1 | 
                                  quienes$p11_13_6_1==1 | 
                                  quienes$p11_13_6_2==1 | 
                                  quienes$p11_13_6_3==1, 1,0)


quienes$madre <- ifelse(quienes$p11_13_1_1==2 | 
                          quienes$p11_13_1_2==2 | 
                          quienes$p11_13_1_3==2 | 
                          quienes$p11_13_2_1==2 | 
                          quienes$p11_13_2_2==2 | 
                          quienes$p11_13_2_3==2 | 
                          quienes$p11_13_3_1==2 | 
                          quienes$p11_13_3_2==2 | 
                          quienes$p11_13_3_3==2 | 
                          quienes$p11_13_4_1==2 | 
                          quienes$p11_13_4_2==2 | 
                          quienes$p11_13_4_3==2 | 
                          quienes$p11_13_5_1==2 | 
                          quienes$p11_13_5_2==2 | 
                          quienes$p11_13_5_3==2 | 
                          quienes$p11_13_6_1==2 | 
                          quienes$p11_13_6_2==2 | 
                          quienes$p11_13_6_3==2, 1,0)

quienes$steppar <- ifelse(quienes$p11_13_1_1==3 | 
                          quienes$p11_13_1_2==3 | 
                          quienes$p11_13_1_3==3 | 
                          quienes$p11_13_2_1==3 | 
                          quienes$p11_13_2_2==3 | 
                          quienes$p11_13_2_3==3 | 
                          quienes$p11_13_3_1==3 | 
                          quienes$p11_13_3_2==3 | 
                          quienes$p11_13_3_3==3 | 
                          quienes$p11_13_4_1==3 | 
                          quienes$p11_13_4_2==3 | 
                          quienes$p11_13_4_3==3 | 
                          quienes$p11_13_5_1==3 | 
                          quienes$p11_13_5_2==3 | 
                          quienes$p11_13_5_3==3 | 
                          quienes$p11_13_6_1==3 | 
                          quienes$p11_13_6_2==3 | 
                          quienes$p11_13_6_3==3, 1,0)

quienes$abuelx <- ifelse(quienes$p11_13_1_1==4 | 
                            quienes$p11_13_1_2==4 | 
                            quienes$p11_13_1_3==4 | 
                            quienes$p11_13_2_1==4 | 
                            quienes$p11_13_2_2==4 | 
                            quienes$p11_13_2_3==4 | 
                            quienes$p11_13_3_1==4 | 
                            quienes$p11_13_3_2==4 | 
                            quienes$p11_13_3_3==4 | 
                            quienes$p11_13_4_1==4 | 
                            quienes$p11_13_4_2==4 | 
                            quienes$p11_13_4_3==4 | 
                            quienes$p11_13_5_1==4 | 
                            quienes$p11_13_5_2==4 | 
                            quienes$p11_13_5_3==4 | 
                            quienes$p11_13_6_1==4 | 
                            quienes$p11_13_6_2==4 | 
                            quienes$p11_13_6_3==4, 1,0)

quienes$hermanx <- ifelse(quienes$p11_13_1_1==5 | 
                            quienes$p11_13_1_2==5 | 
                            quienes$p11_13_1_3==5 | 
                            quienes$p11_13_2_1==5 | 
                            quienes$p11_13_2_2==5 | 
                            quienes$p11_13_2_3==5 | 
                            quienes$p11_13_3_1==5 | 
                            quienes$p11_13_3_2==5 | 
                            quienes$p11_13_3_3==5 | 
                            quienes$p11_13_4_1==5 | 
                            quienes$p11_13_4_2==5 | 
                            quienes$p11_13_4_3==5 | 
                            quienes$p11_13_5_1==5 | 
                            quienes$p11_13_5_2==5 | 
                            quienes$p11_13_5_3==5 | 
                            quienes$p11_13_6_1==5 | 
                            quienes$p11_13_6_2==5 | 
                            quienes$p11_13_6_3==5, 1,0)

quienes$tix <- ifelse(quienes$p11_13_1_1==6 | 
                           quienes$p11_13_1_2==6 | 
                           quienes$p11_13_1_3==6 | 
                           quienes$p11_13_2_1==6 | 
                           quienes$p11_13_2_2==6 | 
                           quienes$p11_13_2_3==6 | 
                           quienes$p11_13_3_1==6 | 
                           quienes$p11_13_3_2==6 | 
                           quienes$p11_13_3_3==6 | 
                           quienes$p11_13_4_1==6 | 
                           quienes$p11_13_4_2==6 | 
                           quienes$p11_13_4_3==6 | 
                           quienes$p11_13_5_1==6 | 
                           quienes$p11_13_5_2==6 | 
                           quienes$p11_13_5_3==6 | 
                           quienes$p11_13_6_1==6 | 
                           quienes$p11_13_6_2==6 | 
                           quienes$p11_13_6_3==6,1,0)

quienes$primx <- ifelse(quienes$p11_13_1_1==7 | 
                           quienes$p11_13_1_2==7 | 
                           quienes$p11_13_1_3==7 | 
                           quienes$p11_13_2_1==7 | 
                           quienes$p11_13_2_2==7 | 
                           quienes$p11_13_2_3==7 | 
                           quienes$p11_13_3_1==7 | 
                           quienes$p11_13_3_2==7 | 
                           quienes$p11_13_3_3==7 | 
                           quienes$p11_13_4_1==7 | 
                           quienes$p11_13_4_2==7 | 
                           quienes$p11_13_4_3==7 | 
                           quienes$p11_13_5_1==7 | 
                           quienes$p11_13_5_2==7 | 
                           quienes$p11_13_5_3==7 | 
                           quienes$p11_13_6_1==7 | 
                           quienes$p11_13_6_2==7 | 
                           quienes$p11_13_6_3==7, 1,0)

quienes$otrofam <- ifelse(quienes$p11_13_1_1==8 | 
                          quienes$p11_13_1_2==8 | 
                          quienes$p11_13_1_3==8 | 
                          quienes$p11_13_2_1==8 | 
                          quienes$p11_13_2_2==8 | 
                          quienes$p11_13_2_3==8 | 
                          quienes$p11_13_3_1==8 | 
                          quienes$p11_13_3_2==8 | 
                          quienes$p11_13_3_3==8 | 
                          quienes$p11_13_4_1==8 | 
                          quienes$p11_13_4_2==8 | 
                          quienes$p11_13_4_3==8 | 
                          quienes$p11_13_5_1==8 | 
                          quienes$p11_13_5_2==8 | 
                          quienes$p11_13_5_3==8 | 
                          quienes$p11_13_6_1==8 | 
                          quienes$p11_13_6_2==8 | 
                          quienes$p11_13_6_3==8, 1,0)

quienes$nofam <- ifelse(quienes$p11_13_1_1==9 | 
                            quienes$p11_13_1_2==9 | 
                            quienes$p11_13_1_3==9 | 
                            quienes$p11_13_2_1==9 | 
                            quienes$p11_13_2_2==9 | 
                            quienes$p11_13_2_3==9 | 
                            quienes$p11_13_3_1==9 | 
                            quienes$p11_13_3_2==9 | 
                            quienes$p11_13_3_3==9 | 
                            quienes$p11_13_4_1==9 | 
                            quienes$p11_13_4_2==9 | 
                            quienes$p11_13_4_3==9 | 
                            quienes$p11_13_5_1==9 | 
                            quienes$p11_13_5_2==9 | 
                            quienes$p11_13_5_3==9 | 
                            quienes$p11_13_6_1==9 | 
                            quienes$p11_13_6_2==9 | 
                            quienes$p11_13_6_3==9, 1,0)

quienes$desc <- ifelse(quienes$p11_13_1_1==10 | 
                          quienes$p11_13_1_2==10 | 
                          quienes$p11_13_1_3==10 | 
                          quienes$p11_13_2_1==10 | 
                          quienes$p11_13_2_2==10 | 
                          quienes$p11_13_2_3==10 | 
                          quienes$p11_13_3_1==10 | 
                          quienes$p11_13_3_2==10 | 
                          quienes$p11_13_3_3==10 | 
                          quienes$p11_13_4_1==10 | 
                          quienes$p11_13_4_2==10 | 
                          quienes$p11_13_4_3==10 | 
                          quienes$p11_13_5_1==10 | 
                          quienes$p11_13_5_2==10 | 
                          quienes$p11_13_5_3==10 | 
                          quienes$p11_13_6_1==10 | 
                          quienes$p11_13_6_2==10 | 
                          quienes$p11_13_6_3==10, 1,0)

quienes$otro <- ifelse(quienes$p11_13_1_1==11 | 
                         quienes$p11_13_1_2==11 | 
                         quienes$p11_13_1_3==11 | 
                         quienes$p11_13_2_1==11 | 
                         quienes$p11_13_2_2==11 | 
                         quienes$p11_13_2_3==11 | 
                         quienes$p11_13_3_1==11 | 
                         quienes$p11_13_3_2==11 | 
                         quienes$p11_13_3_3==11 | 
                         quienes$p11_13_4_1==11 | 
                         quienes$p11_13_4_2==11 | 
                         quienes$p11_13_4_3==11 | 
                         quienes$p11_13_5_1==11 | 
                         quienes$p11_13_5_2==11 | 
                         quienes$p11_13_5_3==11 | 
                         quienes$p11_13_6_1==11 | 
                         quienes$p11_13_6_2==11 | 
                         quienes$p11_13_6_3==11, 1,0)
```

Hagamos la gráfica a la antigüita:
```{r}
porc <- rbind.data.frame(prop.wtable(quienes$padre,w=quienes$fac_muj),
                         prop.wtable(quienes$madre,w=quienes$fac_muj),
                         prop.wtable(quienes$steppar,w=quienes$fac_muj),
                         prop.wtable(quienes$abuelx,w=quienes$fac_muj),
                         prop.wtable(quienes$hermanx,w=quienes$fac_muj),
                         prop.wtable(quienes$tix,w=quienes$fac_muj),
                         prop.wtable(quienes$primx,w=quienes$fac_muj),
                         prop.wtable(quienes$otrofam,w=quienes$fac_muj),
                         prop.wtable(quienes$nofam,w=quienes$fac_muj),
                         prop.wtable(quienes$desc,w=quienes$fac_muj),
                         prop.wtable(quienes$otro,w=quienes$fac_muj))

# Aplicamos esta secuencia para deshacernos de los porcentajes que no corresponden a la respuesta de un agresor.
porc$quien <- seq(0,3,1)
porc <- filter(porc, quien==1)
# Acá hay que ser extra cuidadosas: si no incluimos una categoría, la gráfica va a estar mal; si alteramos el orden, la gráfica va a estar mal.
porc$quien <- c("padre", "madre", "padrastro/madrastra", "abuelx","hermanx","tix","primx","otrofam","vecinx/conocidx","desconocidx","otro")
colnames(porc)[1] <- "porc"

fiuf <- "Agresores de las mujeres que reportaron un embarazo adolescente y sufrieron algún tipo de violencia sexual durante su infancia"
fiuff <- "Las columnas suman más del 100% debido a que la encuesta permite elegir más de un agresor por cada situación."
ggplot(porc,
       aes(x = reorder(quien, -porc),
           y = porc,
           fill = porc)) +
  geom_col() +
  geom_text(aes(label = paste0(round(porc,2),"%")),
            size = 5, position= position_stack(vjust = 0.5)) +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  ggtitle(subtitle = str_wrap(fiuff, width = 80),
          label = str_wrap(fiuf, width = 80)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
```
![](https://github.com/Giremx/justiciareproductiva/raw/master/graficas_informe2018/embado_agresores.png)

Tarea: para desglosar la información por agresor, escribe un código con dplyr. ¡Aguas! Acá es necesario tomar en cuenta el factor de expansión.

Copia y pega este comando en tu consola para deshacerte de todos los objetos que creamos: rm(list=ls(all=TRUE))

#### Anticonceptivos
Para este apartado utilizaremos la Encuesta Nacional de la Dinámica Demográfica (ENADID 2014); desafortunadamente, la ola correspondiente a 2018 apenas se levantará... Habrá que actualizar este análisis y hacer una comparación a lo largo del tiempo.

Al igual que en el apartado anterior, la base de datos que subimos al repositorio ya contiene únicamente las variables que nos interesan. Puedes descargar la [ENADID 2014 aquí](http://www.beta.inegi.org.mx/contenidos/proyectos/enchogares/especiales/enadid/2014/datosabiertos/enadid_2014_csv.zip); para seleccionar las variables que usamos, corre el siguiente código:
```{r}
enadid2014 <- select(original, # dataframe que contiene la ENDIREH completa
                 llave_muj, ent, # id y geográficas
                 fac_per, upm_dis, estrato, # necesarias para estimaciones
                 p5_2, niv, # sociodem (edad, escolaridad)
                 starts_with("p8_1"), # conocimiento general de anticoncepción
                 starts_with("p8_2")) # conocimiento funcional de anticoncepción
```

Uno de las falacias recurrentes respecto al embarazo adolescente y al acceso al aborto es que, dado que existen métodos anticonceptivos, las mujeres deberían "hacerse responsables" del producto del embarazo. Uno de los problemas con esta argumentación es que no considera si, efectivamente, el Estado cumple con dos obligaciones: primero, brindar la información pertinente al uso de anticoncepción; segundo, garantizar el acceso a ésta. En este apartado sólo nos enfocaremos a la evaluación del primer punto. Si quieres investigar más respecto a la necesidad insatisfecha de anticonceptivos —para mujeres en edad reproductiva y para mujeres adolescentes—, te recomendamos visitar este sitio de [CONAPO](http://www.conapo.gob.mx/en/CONAPO/Necesidad_Insatisfecha_de_uso_de_metodos_anticonceptivos_2009_y_2014)

Ahora sí, abramos nuestra data:
```{r}
# Conocimiento de anticonceptivos (mujeres en edad reproductiva)
enadid2014 <- read.csv("https://raw.githubusercontent.com/Giremx/justiciareproductiva/master/enadid_limpia.csv")
```

La ENADID 2014 cuenta con dos tipos de preguntas que nos interesan: el conocimiento general de los métodos anticonceptivos ("¿Quisiera usted decirme de qué métodos o medios ha oído hablar?") y el conocimiento funcional de éstos (es decir, si la mujer conoce cómo utilizarlos de forma correcta). Dado que la edad es una variable muy importante en este tema, nuestro análisis se centrará en ésta.
```{r}
# Seleccionemos nuestras variables
anticon <- select(enadid2014, starts_with("p8_"), fac_per)
anticon$edad <- enadid2014$p5_2
# Sólo nos interesan mujeres en edad reproductiva
anticon <- subset(anticon, edad<50)
```

Estamos listas para crear una variable que nos indique si la mujer tiene conocimiento de algún método anticonceptivo. Usaremos ifelse().
```{r}
anticon$algun_anti <- ifelse(anticon$p8_1_01>2 & anticon$p8_1_02>2 & anticon$p8_1_03>2 & anticon$p8_1_04>2 & anticon$p8_1_05>2 & anticon$p8_1_06>2 & anticon$p8_1_07>2 & anticon$p8_1_08>2 & anticon$p8_1_09>2 & anticon$p8_1_10>2,
                         0,1)
```

El 98.61 de las mujeres en edad reproductiva conoce algún tipo de método anticonceptivo (operaciones, pastillas, óvulos, condones, parches, implantes, entre otros)
```{r}
prop.wtable(anticon$algun_anti,w=anticon$fac_per,digits = 2,na = F)
```

Podemos clasificar la anticoncepción en tres categorías: anticoncepción natural, anticoncepción de emergencia y métodos anticonceptivos. Con lo que hemos revisado hasta ahora, ya estás plenamente capacitada para poder: uno, crear variables de conocimiento general para cada categoría; dos, imprimir los porcentajes de éstos; tres, hacer una gráfica sencilla con esta comparación. Pista: consulta el cuestionario de la [ENADID acá](http://www.beta.inegi.org.mx/contenidos/proyectos/enchogares/especiales/enadid/2014/doc/mujer_enadid14.pdf). Escribe tu código y compara con el mío al final de este bash.
```{r}
















# Creamos categorías
# Naturales
anticon$algun_nat <- ifelse(anticon$p8_1_11>2 & anticon$p8_1_12>2,
                            0,1)
# Emergencia
anticon$algun_emerg <- ifelse(anticon$p8_1_13>2,
                              0,1)
# Métodos anticonceptivos
anticon$algun_metanti <- ifelse(anticon$p8_1_01>2 & anticon$p8_1_02>2 &
                                anticon$p8_1_03>2 & anticon$p8_1_04>2 &
                                anticon$p8_1_05>2 & anticon$p8_1_06>2 &
                                anticon$p8_1_07>2 & anticon$p8_1_08>2 &
                                anticon$p8_1_09>2 & anticon$p8_1_10>2,0,1)

anticoncepcion <- anticon %>%
  select(fac_per, algun_nat, algun_emerg, algun_metanti) %>%
  summarise(total_nat = sum(algun_nat*fac_per, na.rm = T),
            total_emerg = sum(algun_emerg*fac_per, na.rm = T),
            total_metanti = sum(algun_metanti*fac_per, na.rm = T),
            fac_per = sum(fac_per, na.rm = T),
            porc_nat = total_nat/fac_per*100,
            porc_emerg = total_emerg/fac_per*100,
            porc_metanti = total_metanti/fac_per*100) %>%
  select(starts_with("porc")) %>%
  # La siguiente función es nueva
  gather(antis,
         porcentajes,
         porc_nat:porc_metanti)
anticoncepcion$antis <- c("Anticoncepción natural",
                          "Anticoncepción de emergencia",
                          "Métodos anticonceptivos")

# Innovemos un poquito con la gráfica: hagamos un lollipop
fiuf <- "Conocimiento general de tipos de antinconcepción"
fiuff <- "Mujeres en edad reproductiva"
ggplot(anticoncepcion, 
       aes(x=reorder(antis, -porcentajes), 
           y=porcentajes,
           label=round(porcentajes, digits = 2))) + 
  geom_segment(aes(x=reorder(antis, -porcentajes), 
                   xend=reorder(antis, -porcentajes), 
                   y=0, 
                   yend=porcentajes)) +
  geom_point(size = 10)+ 
  geom_text(color="white", size=3) +
  labs(title=str_wrap(fiuf, width = 50), 
       subtitle=str_wrap(fiuff, width = 50)) +
  xlab("") +
  ylab("") +
  theme(axis.text.y = element_blank())
```
![](https://github.com/Giremx/justiciareproductiva/raw/master/graficas_informe2018/anti_congen.png)

¿Este conocimiento general en materia de anticoncepción representa un conocimiento funcional? ¿Existe una brecha entre grupos de edades o es homogéneo? Para simplificar este análisis, sólo tomaremos en cuenta el conocimiento funcional de condones masculinos y pastillas anticonceptivas.
```{r}
# Variable condicional de conocimiento funcional
anticon$func_anti <- ifelse(anticon$p8_2_03_1==1 & anticon$p8_2_03_2==1 & anticon$p8_2_08_2==1 & anticon$p8_2_08_3==1,
                            1,0)
```

El 25.78% de la mujeres que tienen un conocimiento general de métodos anticonceptivos, sabe cómo funcionan éstos correctamente.
```{r}
metanti <- filter(anticon,
                  algun_metanti==1)
prop.wtable(metanti$func_anti,w=metanti$fac_per,na=F,digits = 2)
rm(metanti)
```

Creemos los grupos de edad y visualicemos una gráfica que compare el conocimiento general vs. el funcional por edad.
```{r}
anticon$grupos_edad <- ifelse(anticon$edad<20, "Adolescentes (15- 19 años)",
                              ifelse(anticon$edad>19 & anticon$edad<30, "Jóvenes (20-29 años)",
                                     ifelse(anticon$edad>29 & anticon$edad<40, "Adultas 1 (30-39 años)",
                                            ifelse(anticon$edad>39, "Adultas 2 (40-49 años)", "ZETA")))) # este "ZETA" lo ponemos como una bandera para asegurarnos que no nos equivocamos en nuestras condiciones; spoiler alert: ¡no nos equivocamos!

conocimientos <- anticon%>%
  select(fac_per, grupos_edad, fac_per, func_anti, algun_metanti) %>%
  group_by(grupos_edad) %>%
  summarise(total_general = sum(algun_metanti*fac_per, na.rm = T),
            total_funcional = sum(func_anti*fac_per, na.rm = T),
            fac_per = sum(fac_per, na.rm = T),
            porc_general = total_general/fac_per*100,
            porc_funcional = total_funcional/fac_per*100) %>%
  select(grupos_edad, starts_with("porc")) %>%
  mutate(dif = porc_general - porc_funcional) %>%
  gather(grupo,
         porc,
         porc_general:porc_funcional)

ggplot(data= conocimientos,
       aes(x=reorder(grupos_edad,-dif),
           y=porc,
           group=grupo)) +
  geom_bar(stat = "identity",position = "dodge",
           aes(fill=grupo)) + 
  scale_fill_manual("",
                    values = c("#C1C1C1","#727272"),
                    labels = c("General", "Funcional")) +
  geom_text(aes(label = paste0(round(porc,2),"%")),
            size = 5, position= position_dodge(width = 1)) +
  xlab("Grupos de edad") + ylab("") + 
  theme(axis.text.y = element_blank())
```
![](https://github.com/Giremx/justiciareproductiva/raw/master/graficas_informe2018/anti_congenfunc.png)

Copia y pega este comando en tu consola para deshacerte de todos los objetos que creamos: rm(list=ls(all=TRUE))

#### Evaluación ENAPEA
La Estrategia Nacional para la Prevención del Embarazo Adolescente, implementada desde 2015, tiene dos objetivos: primero, reducir en 50% la tasa de fecundidad de las adolescentes entre 15 a 19 años; segundo, erradicar embarazos en niñas de 14 años o menos. Dado que es muy pronto para emitir una evaluación integral de esta política pública, nos limitaremos a observar esta tendencia a lo largo del tiempo y por entidad federativa.

Hay muchas formas de analizar esta información: por entidad de ocurrencia, por entidad de registro, por residencia habitual de la madre, etcétera. Elige bien a tu guerrero. Acá trabajaremos con la variable de residencia habitual; los datos son descargables desde los [tabulados de natalidad del INEGI](http://www.beta.inegi.org.mx/app/tabulados/pxweb/inicio.html?rxid=fdd12ae8-d551-46fd-a8b5-b5b159c1c3ea&db=Natalidad&px=Natalidad_2).
```{r}
embado <- read.csv("https://raw.githubusercontent.com/Giremx/justiciareproductiva/master/porc_embado.csv")
data <- data.frame(embado$entidad)
n <- 6
data <- do.call("rbind", replicate(n, data, simplify = FALSE))
data$anio <- rep(2012:2017, times=1, each=32)

# Y ahora asignaremos cada valor a su respectiva entidad
embado <- embado[c(-1)]
embado <-  tidyr::gather(embado)
data$embado <- embado$value
colnames(data)[1] <- "entidad"
# Colores
col1 = "#D9E1F1" 
col2 = "#325694"

# ploteamos
ggplot(data = data, 
       aes(x = anio, y = fct_rev(entidad))) + 
  geom_tile(aes(fill = embado), colour = "white") +
  scale_fill_gradient(low = col1, high = col2) +  
  guides(fill=guide_legend(title="%")) +
  labs(title = "% Embarazo Adolescente por entidad",
       x = "Año", y = "") +
  scale_x_continuous(breaks = data$anio)
```
![](https://github.com/Giremx/justiciareproductiva/raw/master/graficas_informe2018/embado_ent.png)

Ahora, ¿será que ésta es la mejor forma de procesar los datos de embarazo adolescente? ¿cuál crees que sea el problema en utilizar residencia habitual de la madre? ¿qué variable geográfica crees que sea más precisa? En nuestro informe utilizamos lugar de ocurrencia, descarga los datos [aquí](http://www.inegi.org.mx/sistemas/olap/Proyectos/bd/continuas/natalidad/nacimientos.asp) y juega con ellos.

Copia y pega este comando en tu consola para deshacerte de todos los objetos que creamos: rm(list=ls(all=TRUE))

***

### Muerte Materna
En el año 2000, uno de los Objetivos de Desarrollo del Milenio (ODM), propuestos por la Organización de Naciones Unidas y firmados por México, fue reducir la mortalidad materna en 75%; es decir, registrar alrededor de 22 muertes maternas por cada 100 mil nacidos vivos. ¿Se habrá cumplido el objetivo?

Abrimos nuestra data:
```{r}
mm <- read.csv("https://raw.githubusercontent.com/Giremx/justiciareproductiva/master/mm_tasa.csv")
```

Esta base de datos fue construida con base en dos fuentes:
* Los datos reportados entre 2002 y 2014, corresponden a los calculados por el Observatorio de Muerte Materna (OMM). El OMM utiliza datos reportados por la Secretaría de Salud.
* Los datos reportados en 2015 y 2016, fueron calculados con base en las estadísticas de natalidad y defunciones para calcular la muerte materna de INEGI, pues la información usada por el OMM (estadísticas reportadas por Secretaría de Salud) no se encontraba disponible.

Ahora, nos vamos a quedar con un objeto que sólo tenga la RMM nacional:
```{r}
mm_nac <- mm[33,]
mm_nac <- mm_nac[c(2:16)]
mm_nac <- t(mm_nac)
mm_nac <- as.data.frame(mm_nac)
colnames(mm_nac) <- "mm_nac"
mm_nac$anio <- seq(2002,2016,1)
```

El objetivo lo podemos sacar al resolver una ecuación bastante sencilla: primero, nuestras "x" son los años y nuestras "y" es la Razón de Muerte Materna; segundo, tenemos las coordenadas de dos puntos de nuestra línea objetivo:
$x~1~ = 2002; x~2~ = 2015$
$y~1~ = 54.18; y~2~ = 22$
Por lo tanto, podemos obtener la pendiente "m" si resolvemos la siguiente ecuación:
$m = (y~2~ - y~1~) / (x~2~ - x~1~)$
```{r}
x1 <- 2002
x2 <- 2015
y1 <- 54.18
y2 <- 22
m <- (y2-y1)/(x2-x1)
b <- y2-m*x2
eq = function(x){
  m*x+b
}
```

Ésta es nuestra línea objetivo
```{r}
ggplot(data.frame(x=c(0, 10)), aes(x=x)) + 
  stat_function(fun=eq, geom="line", size=1.5) +
  xlab("Año") + ylab("RMM") + 
  xlim(2002,2017)
```
![](https://github.com/Giremx/justiciareproductiva/raw/master/graficas_informe2018/mm_obj.png)

Ahora ploteemos el objetivo y la RMM nacional:
```{r}
fiuff <- "Razón de muerte materna en México (2002-2016)"
fiuf <- "Uno de los objetivos del milenio firmados por México fue reducir la mortalidad materna en 75% para 2015; es decir, registrar 22 muertes maternas por cada 100 mil nacidos vivos. Este objetivo, como lo muestra la gráfica, no se cumplió."

ggplot(data=mm_nac, aes(anio)) + 
  geom_line(aes(y = mm_nac, color="Nacional"))  + 
  stat_function(fun=eq, geom="line", aes(color="Objetivo")) +
  scale_color_manual("",
                     values = c("red","blue")) +
  ggtitle(subtitle = str_wrap(fiuf, width = 100), label = fiuff) +
  xlab("Año") +
  ylab("Razón de Muerte Materna") 
```
![](https://github.com/Giremx/justiciareproductiva/raw/master/graficas_informe2018/mm_nac.png)

Otro factor importante que analizar es la RMM por entidad federativa. Esta información ya la tenemos en nuestro df "mm".

Una forma bonita para visualizarla es hacer un mapa de calor. Tenemos que crear un dataframe adecuado para hacerlo.
```{r}
# Dropeamos el nacional
mm <- mm[1:32,]
# Creamos un dataframe con entidades y años en columnas
data <- data.frame(mm$Entidad)
colnames(data) <- "entidad"
n <- 15
data <- do.call("rbind", replicate(n, data, simplify = FALSE))
# Agregamos los años para cada entidad
data$anio <- rep(2002:2016, times=1, each=32)

# Y ahora asignaremos cada valor a su respectiva entidad
mm <- mm[c(-1)]
mm <-  tidyr::gather(mm)
data$rmm <- mm$value

# Colores
col1 = "#D9E1F1" 
col2 = "#325694"
anio <- data$anio

# ploteamos
ggplot(data = data, 
       aes(x = anio, y = fct_rev(entidad))) + 
  geom_tile(aes(fill = rmm), colour = "white") +
  scale_fill_gradient(low = col1, high = col2) +  
  guides(fill=guide_legend(title="RMM")) +
  labs(title = "Razón de Muerte Materna, por entidad",
       x = "Año", y = "Entidad") +
  scale_x_continuous(breaks = anio)
```
![](https://github.com/Giremx/justiciareproductiva/raw/master/graficas_informe2018/mm_ent.png)

Copia y pega este comando en tu consola para deshacerte de todos los objetos que creamos: rm(list=ls(all=TRUE))

***

### Violencia obstétrica
#### Manifestaciones de VOB
Para analizar los datos de violencia obstétrica en México, utilizaremos la Encuesta Nacional sobre la Dinámica de las Relaciones (ENDIREH 2016). El marco muestral de esta encuesta son mujeres mayores de 15 años de edad; la representatividad es nacional y estatal: es posible desglosar la información por entidad federativa.
```{r}
endireh2016 <- read.csv("https://raw.githubusercontent.com/Giremx/justiciareproductiva/master/endireh_limpia.csv")
```

El cuestionario de esta encuesta está disponible [aquí](http://www.beta.inegi.org.mx/contenidos/proyectos/enchogares/especiales/endireh/2016/doc/endireh2016_cuestionario_a.pdf).
Esta encuesta contiene varias preguntas respecto a si las mujeres han sufrido violencia obstétrica (preguntas 9.8). Si bien es necesario analizar las distintas manifestaciones de este fenómeno, el análisis agregado también es útil. Por lo tanto, seleccionaremos las preguntas relacionadas a este problema y crearemos la variable "alguna" que nos indica si una mujer ha sufrido algún tipo de violencia obstétrica.
```{r}
# Selección de preguntas y creación del dataframe "vob"
vob <- select(endireh2016, starts_with("p9_8"), fac_muj, nom_ent, edad, p9_2,upm,estrato)
# p9_2 indica si la mujer estuvo embarazada en los últimos 5 años
vob <- subset(vob, p9_2==1)
# Creamos variable "alguna"
vob$alguna <- ifelse(vob$p9_8_1>1 & vob$p9_8_2>1 & vob$p9_8_3>1 & vob$p9_8_4>1 & vob$p9_8_5>1 & vob$p9_8_6>1 & vob$p9_8_7>1 & vob$p9_8_8>1 & vob$p9_8_9>1 & vob$p9_8_10>1, 0, 1)
```

Esta variable nos indica que el 29.93% de las mujeres que estuvieron embarazadas —en los últimos cinco años— reportan haber sufrido un tipo de violencia obstétrica.
```{r}
prop.wtable(vob$alguna,w=vob$fac_muj,dir=0,digits=2,mar=TRUE,na=FALSE)
```

Respecto a las distintas manifestaciones de violencia obstétrica, crearemos un dataframe con los porcentajes de cada una para, posteriormente, visualizar todo en una sencilla gráfica. En esta parte usaremos el comando prop.wtable para obtener porcentajes ponderados; sin embargo, como se mostrará en otra parte del código, es una mejor práctica utilizar dplyr. Si ya pasaste por esta lección en alguno de nuestros otros módulos, ignora el siguiente código y escríbelo directo en dplyr.
```{r}
# Nos quedamos con las mujeres que reportaron haber sufrido algún tipo de violencia obstétrica.
vob_si <- subset(vob, alguna==1)
weight_si <- vob_si$fac_muj
# porc es un dataframe con los porcentajes de tipos de violencia obstétrica reportados
porc <- rbind.data.frame(prop.wtable(vob_si$p9_8_1,w=weight_si,dir=0,digits=2,mar=TRUE,na=FALSE),
                        prop.wtable(vob_si$p9_8_2,w=weight_si,dir=0,digits=2,mar=TRUE,na=FALSE),
                        prop.wtable(vob_si$p9_8_3,w=weight_si,dir=0,digits=2,mar=TRUE,na=FALSE),
                        prop.wtable(vob_si$p9_8_4,w=weight_si,dir=0,digits=2,mar=TRUE,na=FALSE),
                        prop.wtable(vob_si$p9_8_5,w=weight_si,dir=0,digits=2,mar=TRUE,na=FALSE),
                        prop.wtable(vob_si$p9_8_6,w=weight_si,dir=0,digits=2,mar=TRUE,na=FALSE),
                        prop.wtable(vob_si$p9_8_7,w=weight_si,dir=0,digits=2,mar=TRUE,na=FALSE),
                        prop.wtable(vob_si$p9_8_8,w=weight_si,dir=0,digits=2,mar=TRUE,na=FALSE),
                        prop.wtable(vob_si$p9_8_9,w=weight_si,dir=0,digits=2,mar=TRUE,na=FALSE),
                        prop.wtable(vob_si$p9_8_10,w=weight_si,dir=0,digits=2,mar=TRUE,na=FALSE))

porc$grupo <- seq(0,2,1)
porc <- subset(porc, grupo==0)
colnames(porc) <- c("porcent", "tipo")
porc$tipo <- c("Posiciones incómodas",
             "Gritos o regaños",
             "Ofensas",
             "Fue ignorada",
             "Anestecia denegada",
             "Atención tardada por gritos o quejas",
             "Método a.c. o esterlización forzada",
             "Presión para aceptar a.c. o esterilización",
             "Firma involuntaria de papeles",
             "Fue aislada de su bebé por más de 5 horas")
attach(porc)
new_porc <- porc[order(-porcent),] 
detach(porc)
# Colores
col1 = "#D9E1F1" 
col2 = "#325694"
# Ploteamos bonito
ggplot(new_porc,
       aes(x = reorder(tipo,-porcent),
           y = porcent,
           fill = porcent)) +
  geom_col()  +
  scale_x_discrete("",
                   breaks = waiver(),
                   labels = str_wrap(new_porc$tipo, width = 15)) +
  scale_fill_gradient(low = col1, high = col2) +
  geom_text(aes(label = paste0(round(porcent,2),"%")),
            size = 5, position= position_stack(vjust = 0.5)) +
  theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  labs(title = "Manifestaciones de violencia obstétrica en México",
       fill = "Porcentaje %") +
  coord_flip()
```
![](https://github.com/Giremx/justiciareproductiva/raw/master/graficas_informe2018/vob_manifest.png)

Como se mencionó, el análisis por entidad federativa es posible y necesario. Exploremos cuántos son los casos de violencia obstétrica por entidad federativa: desglosaremos por entidad federativa. Por ejemplo, en Aguascalientes, el 29.93% de las mujeres reportan haber sufrido violencia obstétrica.
```{r}
weight <- vob$fac_muj
prop.wtable(vob$alguna,vob$nom_ent=="Aguascalientes",w=weight,dir=0,digits=2,mar=TRUE,na=FALSE)
```

Creemos un dataframe con los porcentajes, por entidad federativa. Aquí usaremos dplyr.
```{r}
# Agrupamos por entidad
vob$alguna <- as.numeric(vob$alguna)
vob_ent <- vob %>%
  select(alguna, fac_muj, nom_ent)%>%
  group_by(nom_ent)%>%
  summarise(total_alguna = sum(alguna*fac_muj, na.rm = TRUE),
            fac_muj = sum(fac_muj, na.rm = TRUE),
            porcentaje=(total_alguna/fac_muj)*100)%>%
  select(nom_ent, porcentaje)
```

Las entidades con mayor porcentaje de violencia obstétrica son: Estado de México (33.67%), Ciudad de México (32.88%) y Tlaxcala (31.89%). Podemos hacer un sencillo plot para visualizar todas las entidades.
```{r}
# Colores
col1 = "#D9E1F1" 
col2 = "#325694"

fiuf <- "Porcentaje de mujeres que reportó haber sufrido un tipo de violencia obstétrica, por entidad federativa"
ggplot(data = vob_ent,
       aes(x = reorder(nom_ent, porcentaje),
           y = porcentaje,
           fill = porcentaje)) + 
  geom_col() +
  scale_fill_gradient(low = col1, high = col2) +
  geom_text(aes(label = paste0(round(porcentaje,2),"%")),
            size = 5, position= position_stack(vjust = 0.5)) +
  labs(title = str_wrap(fiuf, width = 65),
       fill = "%") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank()) +
  coord_flip()
```
![](https://github.com/Giremx/justiciareproductiva/raw/master/graficas_informe2018/vob_ent.png)

Ahora haremos un perfil de aquellas mujeres que sufren violencia obstétrica por características socio-económicas:
* Condición laboral
* Edad
* Auto-adscripción indígena
* Lugar de atención médica
```{r}
vob <- select(endireh2016, starts_with("p9_8"), fac_muj, nom_ent, edad,p9_2,upm,estrato)
vob$alguna <- ifelse(vob$p9_8_1>1 & vob$p9_8_2>1 & vob$p9_8_3>1 & vob$p9_8_4>1 & vob$p9_8_5>1 & vob$p9_8_6>1 & vob$p9_8_7>1 & vob$p9_8_8>1 & vob$p9_8_9>1 & vob$p9_8_10>1, 0, 1)
```

Agregamos las variables:
```{r}
vob$cond_lab <- endireh2016$p2_13
vob$escolaridad <- endireh2016$niv
vob$edad <- endireh2016$edad
vob$indigena <- endireh2016$p2_10
vob$indigena <- ifelse(vob$indigena>2,2,1)
vob$lug_at_med <- endireh2016$p9_7
vob <- subset(vob, p9_2==1)
```

Condición laboral: no parece haber grandes diferencias entre mujeres que trabajan y aquéllas que no trabajan (30.3% y 29.7%, respectivamente).
```{r}
vob$cond_lab[vob$cond_lab == 9] <- NA
vob_trabaja <- subset(vob, cond_lab==1)
vob_notrabaja <- subset(vob, cond_lab==2)

prop.wtable(vob_trabaja$alguna,w=vob_trabaja$fac_muj,na = FALSE)
prop.wtable(vob_notrabaja$alguna,w=vob_notrabaja$fac_muj,na = FALSE)
```

Edad: haremos el mismo análisis por grupos de edades. Respecto a esta característica, sí parece haber diferencias significativas: las jóvenes y adolescentes son el grupo poblacional más afectada por este problema.
```{r}
vob$edad[vob$edad == 99] <- NA
vob$edad[vob$edad == 98] <- NA
vob <- subset(vob, edad<50)

vob$grupos_edad <- ifelse(vob$edad<20, "Adolescentes (15- 19 años)",
                              ifelse(vob$edad>19 & vob$edad<30, "Jóvenes (20-29 años)",
                                     ifelse(vob$edad>29 & vob$edad<40, "Adultas 1 (30-39 años)",
                                            ifelse(vob$edad>39, "Adultas 2 (40-49 años)", "ZETA")))) # este "ZETA" lo ponemos como una bandera para asegurarnos que no nos equivocamos en nuestras condiciones; spoiler alert: ¡no nos equivocamos!

violencia <- vob%>%
  select(fac_muj, grupos_edad, alguna) %>%
  group_by(grupos_edad) %>%
  summarise(total_alguna = sum(alguna*fac_muj, na.rm = T),
            fac_muj = sum(fac_muj, na.rm = T),
            porc = total_alguna/fac_muj*100) %>%
  select(grupos_edad, porc)

col1 = "#D9E1F1" 
col2 = "#325694"

ggplot(data = violencia,
       aes(x=reorder(grupos_edad,-porc),
           y=porc,
           fill = porc)) +
  geom_col() +
  scale_fill_gradient(low = col1,
                      high = col2) +
  geom_text(aes(label = paste0(round(porc,2),"%")),
            size = 5, nudge_y = 1) +
  xlab("Grupos de edad") + ylab("") + 
  theme(axis.text.y = element_blank())
```
![](https://github.com/Giremx/justiciareproductiva/raw/master/graficas_informe2018/vob_edad.png)

Auto-adscripción indígena: el 29.9% de las mujeres auto-adscritas como indígenas reportan haber sufrido un tipo de violencia obstétrica, este porcentaje es muy similar al reportado por mujeres no indígenas (29.94%). Esto se puede deber a un sesgo en el que las personas indígenas tienden a no reportar esta característica (o no identificarse como tal), dados los estereotipos relacionados con ésta.
```{r}
ind <- subset(vob, indigena==1)
noind <- subset(vob, indigena==2)
prop.wtable(ind$alguna,w=ind$fac_muj,na=FALSE,digits = 2)
prop.wtable(noind$alguna,w=noind$fac_muj,na=FALSE,digits = 2)
```

La ENDIREH también nos permite desglosar el fenómeno de violencia obstétrica por lugar de atención médica. El siguiente código no está en dplyr. Date a la tarea de arreglarlo
```{r}
vob$lug_at_med[vob$lug_at_med == 99] <- NA
vob$lug_at_med[vob$lug_at_med == 99] <- NA
vob$lug_at_med[vob$lug_at_med == 8] <- NA
vob$lug_at_med[vob$lug_at_med == 9] <- NA
vob$lug_at_med[vob$lug_at_med == 10] <- NA

vob_centsal <- subset(vob, lug_at_med==1)
vob_imss <- subset(vob, lug_at_med==2)
vob_isste <- subset(vob, lug_at_med>=3)
vob_isste <- subset(vob_isste, lug_at_med<=4)
vob_est <- subset(vob, lug_at_med==5)
vob_priv <- subset(vob, lug_at_med>=6)
vob_priv <- subset(vob_priv, lug_at_med<=7)


data <- rbind.data.frame(prop.wtable(vob_centsal$alguna,w=vob_centsal$fac_muj,na=FALSE,digits = 2),
                 prop.wtable(vob_imss$alguna,w=vob_imss$fac_muj,na=FALSE,digits = 2),
                 prop.wtable(vob_isste$alguna,w=vob_isste$fac_muj,na=FALSE,digits = 2),
                 prop.wtable(vob_est$alguna,w=vob_est$fac_muj,na=FALSE,digits = 2),
                 prop.wtable(vob_priv$alguna,w=vob_priv$fac_muj,na=FALSE,digits = 2))

data$grupo <- seq(0,2,1)
data <- subset(data, grupo==1)

data$grupo <- c("Centro de Salud",
              "Clínica/hospital IMSS",
              "Clínica/hospital ISSSTE",
              "Otro hospital o clínica pública del estado",
              "Hospital/clínica/consultorio privado")
colnames(data)[1] <- "porc"

# Ploteemos
# Colores
col1 = "#D9E1F1" 
col2 = "#325694"

fiuf <- "Violencia obstétrica por lugar de atención médica"
ggplot(data= data,
       aes(x=reorder(grupo, -porc),
           y=porc,
           fill=porc)) +
  geom_col() + 
  scale_fill_gradient(low = col1, high = col2) +
  geom_text(aes(label = paste0(round(porc,1),"%")),
            size = 5, position= position_stack(vjust = 0.5)) +
  labs(title = str_wrap(fiuf, width = 100),
       fill = "%") +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank()) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))
```
![](https://github.com/Giremx/justiciareproductiva/raw/master/graficas_informe2018/vob_lugatmed.png)

Sí existen diferencias importantes en los reportes de violencia obstétrica por lugar de atención. Al observar los porcentajes, podríamos plantear que las mujeres que pueden costear una clínica privada, tienden a sufrir este problema en menor medida que aquéllas que optan por atención pública.

Esta encuesta también nos proporciona información respecto a consentimiento en los procedimientos de césares y los índices de cesáreas practicadas por lugar de atención médica y entidad federativa. Se reportó que al 9.69% de las mujeres a las que se le practicó una césarea no se le pidió autorización para llevarlo a cabo.
```{r}
prop.wtable(vob$p9_8_13,w=vob$fac_muj,na=FALSE,digits = 2)
```
