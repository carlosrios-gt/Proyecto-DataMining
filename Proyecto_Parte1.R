# Proyecto Data Mining

# Instalacion de librerias

install.packages("arules")
library(arules)
install.packages("readxl")
library(readxl)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
# Librerias especiales para FP-Growth
install.packages("pkgbuild")
pkgbuild::has_build_tools(debug = TRUE)
install.packages(c("Rcpp", "RcppArmadillo", "remotes"))
library(pkgbuild)
pkgbuild::find_rtools()
install.packages("C:/Users/Carlos Rios/Downloads/fim4r_1.8.tar.gz",
                 repos = NULL,
                 type = "source")


# Carga de datos (Por categoria)
datos_adjudicados <- read_excel("C:\\Users\\Carlos Rios\\Documents\\USAC\\MIICC\\MIICC408 - Introducción a la Minería de Datos\\Proyecto\\Concursos Publicados 2025 Finalizados Adjudicados.xlsx")
datos_anulados <- read_excel("C:\\Users\\Carlos Rios\\Documents\\USAC\\MIICC\\MIICC408 - Introducción a la Minería de Datos\\Proyecto\\Concursos Publicados 2025 Finalizados Anulados.xlsx")
datos_desiertos <- read_excel("C:\\Users\\Carlos Rios\\Documents\\USAC\\MIICC\\MIICC408 - Introducción a la Minería de Datos\\Proyecto\\Concursos Publicados 2025 Finalizados Desiertos.xlsx")

# NOTA: Al revisar el contenido de los distintos Data Set, se opto por tomar
# el set de datos de datos_adjudicados por la imoportancia en cuanto a la 
# informacion mas relevante para el proyecto

##-- Metodo Apriori --##

# Seleccion de las variables de interes
# Mantener solo las columnas identificadas
columnas_permitidas <- c(
  "tipoDeEntidadPadre",
  "tipoEntidad",
  "entidadCompradora",
  "unidadCompradora",
  "monto"
)

# Realizando el filtrado de las columnas seleccionadas
datos_adjudicados <- datos_adjudicados[, columnas_permitidas]

# Definiendo los parametros
transacciones <- as(datos_adjudicados, "transactions")

# Ejecutando el metodo Apriori
reglas <- apriori(
  transacciones,
  parameter = list(
# Definiendo el soporte
    support = 0.05, 
# Definiendo la confianza
    confidence = 0.5,
    minlen = 2
  )
)

# Verificando los 20 resultados mas significativos
inspect(head(sort(reglas, by="lift"), 20))

# Obtencion de Resultados
transacciones <- as(datos_adjudicados, "transactions")
summary(transacciones)


##-- Metodo FP-Growth --##
install.packages("pkgbuild")
pkgbuild::has_build_tools(debug = TRUE)
install.packages(c("Rcpp", "RcppArmadillo", "remotes"))
library(pkgbuild)
pkgbuild::find_rtools()
install.packages("C:/Users/Carlos Rios/Downloads/fim4r_1.8.tar.gz",
                 repos = NULL,
                 type = "source")
# Nota: En caso de tener problemas con la instalacion, referirse al
# Readme incluido en el repositorio para detalles.

# Uso del data set elegido
datos3 <- datos_adjudicados

# Eliminar filas con NA en las 5 columnas
datos3 <- tidyr::drop_na(datos3)

# Asegurando que la variable monto sea tipo numérico
datos3$monto <- as.numeric(datos3$monto)

# Verificando integridad de los datos
datos3 %>%
  summarise(
    across(
      everything(),
      list(
        clase     = ~ class(.),
        n_na      = ~ sum(is.na(.)),
        n_distinct = ~ n_distinct(.)
      ),
      .names = "{.col}_{.fn}"
    )
  )

# Revisando estado de los datos
lapply(datos3, function(x) sort(unique(x)))

# Ejecutando Metodo FP-Growth
reglas_fp <- fim4r(datos3, method="fpgrowth", target ="rules", 
# Soporte 20% y Confianza 50%
              supp =.2, conf=.5)

rf <- as(reglas_fp, "data.frame")

# Revisando los resultados del analisis con parametros definidos
reglas_fp
length(reglas_fp)
summary(reglas_fp)

# Reporte TOP 10 por Lift
top_lift <- rf %>%
  arrange(desc(lift)) %>%
  select(lhs, rhs, support, confidence, lift) %>%
  slice(1:10)

top_lift

# Reporte TOP 10 por Support
top_support <- rf %>%
  arrange(desc(support)) %>%
  select(lhs, rhs, support, confidence, lift) %>%
  slice(1:10)

top_support


##-- Metodo clustering k-Means --##
# Construyendo dataset para su uso
datos_km <- datos3

# Verificacion de variables: 'monto' que sea numérico
datos_km$monto <- as.numeric(datos_km$monto)
datos_km <- dplyr::filter(datos_km, is.finite(monto))

# Verificacion de variables categoricas hacia numericas
datos_km <- datos_km |>
  dplyr::mutate(
    tipoDeEntidadPadre = as.integer(factor(tipoDeEntidadPadre)),
    tipoEntidad        = as.integer(factor(tipoEntidad)),
    entidadCompradora  = as.integer(factor(entidadCompradora)),
    unidadCompradora   = as.integer(factor(unidadCompradora))
  )


# Asignando valor numerico a los datos nulos
datos_km[is.na(datos_km)] <- -1

# Creando los cálculos k-Means
cluster <- kmeans(datos_km, centers = 3)

# Primer Segmento
# Trazando los gráficos
ggplot(datos_km, aes(x = entidadCompradora, y = monto, 
                              color =as.factor(cluster$cluster)))+
  geom_point()+
  geom_point(data = as.data.frame(cluster$centers), 
             aes(x = entidadCompradora, y = monto), color="black", 
             size=4, shape=17)+
  labs(title = "Adjudicacion por monto para cada entidad") +
  theme_minimal()
