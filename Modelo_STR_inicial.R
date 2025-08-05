##_______________________________________________________________________________________________________
##_______________________________________________________________________________________________________
#                           BANCO DE LA REPÚBLICA
#                           UNIDAD DE ECONOMETRÍA
##                MODELO STR INFLACIÓN ALIMENTOS VS ENSO
##                      David Steven Parra Almeyda 
##_______________________________________________________________________________________________________
##_______________________________________________________________________________________________________

#CARGA Y DESCARGA DE PAQUETES---------------------------------------------------
if(0){
install.packages("pacman") 
pacman :: p_load(
  vars,         # Para usar modelos VAR
  urca,         # Para realizar pruebas de raíz unitaria
  tidyverse,    # Paquete que incluye ggplot2 y dplyr
  ggfortify,    # Para graficar series de tiempo
  gridExtra,    # Para concatenar gráficas en un solo plot
  forecast,     # Para hacer pronóstico con los modelos ARIMA
  lmtest,       # Obtener la significancia individual de los coeficientes ARIMA
  urca,         # Prueba de raíz unitaria (estacionariedad)
  tseries,      # Para estimar modelos de series de tiempo y pruebas de supuestos 
  readxl,       # Para leer archivos de excel
  stargazer,    # Presentación de resultados y tablas más estéticos
  psych,        # Para hacer estadística descriptiva 
  seasonal,     # Para desestacionalizar las series de tiempo
  aTSA,         # Para hacer prueba de efectos ARCH
  astsa,        # Para estimar, validar y pronosticar en modelos ARIMA/SARIMA
  xts,          # Para utilizar objetos de tipo xts
  car,          # Función qqplot
  fable,        # Forma moderna de hacer pronóstiocs en R (se recomienda su uso)  
  tsibble,      # Para poder emplear objetos de series de tiempo tsibble
  feasts,       # Provee una colección de herramientas para el análisis de datos de series de tiempo 
  xtable,       # Paquete xtable
  ARDL,         # Para estimar modelos lineales con rezagos distribuidos
  dynlm,        #Para estimar MCO dinámicos
  tsDyn,        #Para estimar series no lineales
  CADFtest      #Prueba de raiz unitaria no lineal
  )
}
#Limpiar el Espacio de Trabajo
rm(list = ls())
require(readxl)

#Definir directorio de trabajo
Ruta.Sv = 'C:/Users/ASUS X507UA/OneDrive/Documentos/PracticanteUnidadEconometria2025-2/STR_inicial1'
Ruta.LF = '~/Archivos/STR2025/Steven/STR_Inicial1'
ifelse(Sys.info()[1]=='Darwin', setwd(Ruta.LF), setwd(Ruta.Sv))

#1. Datos Inflación de Alimentos y ENSO (Mar 1962-Dic 2018)-----
#Base de Datos
datos <- 'Datos_ENSO_DINF.xlsx'
DINF_ENSO <- read_excel(datos) 

#1.1 Serie de Inflación de Alimentos----------------------------
INF<-ts(DINF_ENSO$INF, start = 1962, frequency = 12)

#1.2 Serie de ENSO----------------------------------------------
ENSO<-ts(DINF_ENSO$ENSO, start = 1962, frequency = 12)

INFvsENSO <- data.frame(
  Fecha=seq(as.Date("1962-03-01"), as.Date("2018-12-01"), by="month"),
ENSO=DINF_ENSO$ENSO,
INF=DINF_ENSO$INF
  )

#2. Gráfico de INF vs ENSO---------------------------------------
ggplot(INFvsENSO, aes(x = Fecha)) +
geom_line(aes(y = ENSO, color = "ENSO"), linewidth = 0.8) +
  geom_line(aes(y = INF * (max(ENSO, na.rm = TRUE)/max(INF, na.rm = TRUE)), color = "INF"), linewidth = 0.8) +
            scale_y_continuous(
                name = "ENSO",
                sec.axis = sec_axis(~ . / (max(ENSO, na.rm = TRUE)/max(INF, na.rm = TRUE)), name = "Inflación de alimentos (INF)")
              ) +
          
              scale_color_manual(values = c("ENSO" = "blue2", "INF" = "blue4")) +
              labs(
                title = "Inflación de alimentos y ENSO en Colombia (1962-2018)",
                x = "Año",
                color = "Series"
              ) +
              theme_minimal() +
              theme(
                plot.title = element_text(hjust = 0.5, face = "bold"),
                legend.position = "bottom"
              )
#3. Verificación del grado de integración de las series----------

#3.1 Grado de Integración de la serie INF------------------------

#Prueba de raíz unitaria según Hansen (1995) para INF

INF_ksstest<-CADFtest(INF, type="drift", max.lag.y=12, kerne1="ba",
                       criterion="AIC", method="OLS")
print(INF_ksstest)

#Como el p-value de 0.349 es mayor al nivel de significancia alfa=0.05, NO se rechaza la hipótesis nula que plantea la existencia de raíz unitaria.
#La serie INF NO es estacionaria

#3.1.1 Serie INF diferenciada-----------------------------------
DINF<-ts(DINF_ENSO$DINF, start = 1962, frequency = 12) #Primera diferencia de INF

#Prueba de raíz unitaria según Hansen (1995) para DINF
DINF_ksstest<-CADFtest(DINF, type="drift", max.lag.y=12, kerne1="ba",
                      criterion="AIC", method="OLS")
print(DINF_ksstest)

#Como el p-value de 2.2e-16 es menor que el el nivel de signifcancia alfa=0.05, se rechaza la hipótesis nula que plantea la existencia de raíz unitaria en la serie DINF.
#La serie DINF es estacionaria y la serie INF es I(1)

#3.2 Grado de integración de la serie ENSO-------------------

#Prueba de raíz unitaria según Hansen (1995) para ENSO
ENSO_ksstest<-CADFtest(ENSO, type="drift", max.lag.y=12, kerne1="ba",
                       criterion="AIC", method="OLS")
print(ENSO_ksstest)


#Como el p-value de 3.39e-08 es menor que el el nivel de signifcancia alfa=0.05, se rechaza la hipótesis nula que plantea la existencia de raíz unitaria en la serie ENSO.
#La serie ENSO es estacionaria, es decir, es I(0)

#Se agrega crea un data.frame con DINF y ENSO
#ENSO<-ENSO[-1] #Se quta la primera observación para hacerlo compatible con la seri DINF
#DINFvsENSO <- data.frame(
 #ENSO=ENSO,
 #DINF=DINF)
  
#4. Creación de función para el test de NO linealidad, Terarsvirta, (1995)------

terasvirta_testNL <- function(y, x, rez_y, rez_x, alfa) { 

  #y: serie explicada
  #x: serie explicativa
  #rez_y: número de rezagos de la variable endógena explicativa y
  #rez_x: número de rezagos de la variable exógena explicativa x
  #NOTA: Tanto los rezagos de y como los de x, conforman la lista de variables de transición candidatas
  
  #Función para crear el vector de variables explicativas respetando sus rezagos
  
  if (is.null(x)) {
    rez_x=NULL
    rez_max<-rez_y
    
    #Matriz de rezagos de y como variables explicativas
    base_explicativas <- embed(y, rez_max+1)
    colnames(base_explicativas) <- paste0('y_L', c('', 1:rez_max))
    
    #Variable explicada
    y_dep <- base_explicativas[, 'y_L']
    
    #Rezagos de 'y' como variables explicativas 
    explicativas <- paste0("y_L", 1:rez_y)
    
    #Matriz de variables explicativas candidatas a ser variable de transición
    X <- (base_explicativas[, explicativas])
    data_explicativas <- as.data.frame(base_explicativas)
    
    #Lista de variables de transición candidatas
    z_cand <- paste0('y_L', 1:rez_y)
    
  } else {
    
    !is.null(rez_x) 
    rez_max <- max(rez_x, rez_y) #rezago máximo general
  
   #Matriz de variables explicativas hasta el rezago máximo
   base_explicativas <- embed(cbind(y,x), rez_max+1)
   colnames(base_explicativas) <-c(
   paste0('y_L', c('', 1:rez_max)),
   paste0('x_L', c('', 1:rez_max))
   )
  
   #Variable explicada
   y_dep <- base_explicativas[, 'y_L']
  
   #Variables explicativas deseadas según sus rezagos
   explicativas <- c(
   paste0("y_L", 1:rez_y),
   paste0("x_L", 1:rez_x)
   )
   
   #Matriz de variables explicativas candidatas a ser variable de transición
   X <- (base_explicativas[, explicativas])
   data_explicativas <- as.data.frame(base_explicativas)
  
   #Lista de variables de transición candidatas
   z_cand <- c(
    paste0('y_L', 1:rez_y),
    paste0('x_L', 1:rez_x)
   )
  }
  
  resultados <- list()
  
  #Correr test de Teräsvirta para cada candidata a ser variable de transición
  for (s_name in z_cand)
  {
    s=data_explicativas[[s_name]]
    s2=s^2 #Variable de transición al cuadrado
    s3=s^3 #Variable de transición al cubo
    
    #Modelo H0 --> Modelo Lineal
    Mod_H0 <- lm(y_dep~X) #Modelo bajo la Hipótesis Nula H0 que plantea un modelo lineal 
    SSR_0 <- sum((residuals(Mod_H0))^2) #Suma de residuos al cuadrado del modelo según la Hipótesis Nula H0
    
    #Modelo H1 --> Modelo NO lineal completo 
    Mod_H1 <- lm(y_dep ~ X + X*s + X*s2 + X*s3) #Modelo bajo la Hipótesis Alternativa H1 que plantea Modelo NO lineal con la variable de transición s, s^2 y s^3 
    SSR_1 <- sum((residuals(Mod_H1))^2) #Suma de residuos al cuadrado del modelo según la Hipótesis Alternativa H1
    
    #Estadístico LM_1 para probar la no linealidad del modelo según distribución F
    LM_1 <- ((SSR_0-SSR_1)/(length(Mod_H1$coef)-length(Mod_H0$coef)))/(SSR_0/(length(y_dep)-length(Mod_H1$coef))) #Estadístico LM_1 calculado según distribución F
    pvalue.LM1 <- 1-pf(LM_1, df1=length(Mod_H1$coef)-length(Mod_H0$coef), df2=length(y_dep)-length(Mod_H1$coef))  #p_value arrojado para mirar no linealidad
    
    #Modelo H02 --> Modelo bajo la Hipótesis Nula H02 que plantea que b1=0 | b2=b3=0, es decir, el modelo es lineal
    #NOTA: La Hipótesis Nula H02 es la misma hipótesis H0 que plantea linealidad
    Mod_H2 <- lm(y_dep ~ X + X*s) #Modelo bajo la Hipótesis Alternativa H2 que plantea b1≠0 | b2=b3=0, es decir, el polinomio de Taylor de la variable de transición s, es de orden 1
    SSR_2 <- sum((residuals(Mod_H2))^2) #Suma de residuos al cuadrado del modelo según la Hipótesis Alternativa H2
    
    #Estadístico LM_2 para probar la Hipótesis Nula H02 del modelo según distribución F
    LM_2 <- ((SSR_0-SSR_2)/(length(Mod_H2$coef)-length(Mod_H0$coef)))/(SSR_0/(length(y_dep)-length(Mod_H2$coef))) #Estadístico LM_2 caclulado según distribución F
    pvalue.LM2 <- 1-pf(LM_2, df1=length(Mod_H2$coef)-length(Mod_H0$coef), df2=length(y_dep)-length(Mod_H2$coef))   #p_value arrojado del modelo H02
    
    #Modelo H03 --> Modelo bajo la Hipótesis Nula H03 que plantea que b2=0 | b3=0, es decir, el polinomio de Taylor de la variable de transición s, es de orden 1
    #NOTA: La Hipótesis Nula H03 es la misma hipótesis H2 que plantea que el polinomio de Taylor de la variable de transición s, es de orden 1
    Mod_H3 <- lm(y_dep ~ X + X*s + X*s2) #Modelo bajo la Hipótesis Alternativa H3 que plantea b2≠0 | b3=0, es decir, el polinomio de Taylor de la variable de transición s, es de orden 2
    SSR_3 <- sum((residuals(Mod_H3))^2) #Suma de residuos al cuadrado del modelo según la Hipótesis Alternativa H3
    
    #Estadístico LM_3 para probar la Hipótesis Nula H03 del modelo según distribución F
    LM_3 <- ((SSR_2-SSR_3)/(length(Mod_H3$coef)-length(Mod_H2$coef)))/(SSR_2/(length(y_dep)-length(Mod_H3$coef))) #Estadístico LM_3 caclulado según distribución F
    pvalue.LM3 <- 1-pf(LM_3, df1=length(Mod_H3$coef)-length(Mod_H2$coef), df2=length(y_dep)-length(Mod_H3$coef))   #p_value arrojado del modelo H03
    
    #Modelo H04 --> Modelo bajo la Hipótesis Nula H04 que plantea que b3=0, es decir, el polinomio de Taylor de la variable de transición s, es de orden 2
    #NOTA: La Hipótesis Nula H04 es la misma hipótesis H3 que plantea que el polinomio de Taylor de la variable de transición s, es de orden 2
    Mod_H4 <- Mod_H1 #Modelo bajo la Hipótesis Alternativa H4 que plantea b3≠0, es decir, el polinomio de Taylor de la variable de transición s, es de orden 3
    #NOTA: La Hipótesis Alternativa H1 es la misma hipótesis H1, que plantea el modelo no lineal completo
    SSR_4 <- sum((residuals(Mod_H4))^2) #Suma de residuos al cuadrado del modelo según la Hipótesis Alternativa H4
    
    #Estadístico LM_4 para probar la Hipótesis Nula H04 del modelo según distribución F
    LM_4 <- ((SSR_3-SSR_4)/(length(Mod_H4$coef)-length(Mod_H3$coef)))/(SSR_3/(length(y_dep)-length(Mod_H4$coef))) #Estadístico LM_3 caclulado según distribución F
    pvalue.LM4 <- 1-pf(LM_4, df1=length(Mod_H4$coef)-length(Mod_H3$coef), df2=length(y_dep)-length(Mod_H4$coef))   #p_value arrojado del modelo H03
      
    #Secuencia de decisión
    min_rechazo <- min(pvalue.LM2, pvalue.LM3, pvalue.LM4) #Hipótesis con mayor rechazo
    if (min_rechazo==pvalue.LM2 & min_rechazo<alfa & pvalue.LM1<alfa) { 
      recomendado <-"LSTR"
    } else if (min_rechazo==pvalue.LM3 & min_rechazo < alfa & pvalue.LM1<alfa) {
      recomendado <- "ESTR"
    } else if (min_rechazo==pvalue.LM4 & min_rechazo < alfa & pvalue.LM1<alfa) {
      recomendado <- "LSTR"
    } else recomendado <- "Modelo Lineal"
    
    
    resultados[[s_name]] <- list(
      variable_transicion = s_name,
      p_H01 = pvalue.LM1,
      p_H02 = pvalue.LM2,
      P_H03 = pvalue.LM3,
      p_H04 = pvalue.LM4,
      recomendado = recomendado
    )
  }
  resultados <- do.call(rbind, lapply(resultados, as.data.frame))
  rownames(resultados) <- NULL
  
  return(resultados)
  
}

#5. Función para estimar un modelo STR por máxima verosimilitud-----------------
str_mod <- function(y, x, s, rez_s, rez_y, rez_x, G) {
  #y = Variable endógena explicada
  #x = Variable exógena explicativa
  #s = Variable de la cual se usará algunos de sus rezagos para ser variable de transición
  #rez_s = Rezago de la variable s que se usará como variable de transición
  #rez_y = rezagos de la variable endógena 'y' que actúan como variables explicativas
  #rez_x = rezagos de la variable exógena 'x' que actúan como variables explicativas
  #G = Función de transición, 'LSTR' si es logística o 'ESTR' si es exponencial
  
  #Matriz de variables explicativas hasta el rezago máximo
  if (is.null(x)) {
    rez_x=NULL
    rez_max <- rez_y
    if (rez_s > rez_max) {
      warning(sprintf("rez_s (%d) es mayor que rez_max (%d). Ajustando rez_s = rez_max.", rez_s, rez_max))
      rez_s <- rez_max
    }
    
    #Matriz de variables explicativas
    base_explicativas           <- embed(y, rez_max+1)
    colnames(base_explicativas) <- paste0('y_L', c('', 1:rez_max))
    
    #Variable explicada
    y_dep <- base_explicativas[, 'y_L']
    
    #Variables explicativas deseadas según sus rezagos
    explicativas <- paste0("y_L", 1:rez_y)
  
    } else {

    !is.null(rez_x) 
    rez_max <- max(rez_x,rez_y)
    if (rez_s > rez_max) {
      warning(sprintf("rez_s (%d) es mayor que rez_max (%d). Ajustando rez_s = rez_max.", rez_s, rez_max))
      rez_s <- rez_max
    }
    
    #Matriz de variables explicativas
    base_explicativas <- embed(cbind(y,x), rez_max+1)
    colnames(base_explicativas) <-c(
    paste0('y_L', c('', 1:rez_max)),
    paste0('x_L', c('', 1:rez_max))
    )
    
    #Variable explicada
    y_dep <- base_explicativas[, 'y_L']
    
    #Variables explicativas deseadas según sus rezagos
    explicativas <- c(
    paste0("y_L", 1:rez_y),
    paste0("x_L", 1:rez_x)
    )
    
    }
  
  #Variables explicativas
  X <- cbind(intercepto=1, base_explicativas[, explicativas]) 
  
  k <- ncol(X) #número de variables explicativas parte lineal
  
  #Variable de transición ajustada 
  base_s <- embed(s, rez_max + 1) #Para que coincida con la base de datos de las variables explicativas
  colnames(base_s) <- paste0("s_L", 0:rez_max)
  
  #Extraer la variable de transición
  z <- base_s[, paste0("s_L", rez_s)]
  
  #Función de transición
  func_trans <- function(z, gamma, c) {  
    if (G=='LSTR') {
      return(1/(1+exp(-gamma*(z-c))))
    } else if (G=='ESTR') {
      return(1-exp(-gamma*((z-c)^2)))
    } else {
      stop("Entrada inválida. Por favor escriba 'LSTR' o 'ESTR' ")
    }
  }
  #Costrucción del logaritmo de verosimilitud
  Logverosimil_funcion <- function(parametros) {
    k                     <- ncol(X)                             #número de variables explicativas
    Phi_lineal            <- parametros[1:k]                     #parámetros de la parte lineal
    names(Phi_lineal)     <- paste0('phi_', 0:(k-1))             #nombres de los parámetros lineales
    theta_nolineal        <- parametros[(k+1):(2*k)]             #parámetros de la parte no lineal
    names(theta_nolineal) <- paste0('theta_', 0:(k-1))           #nombres de los parámetros no lineales
    gamma                 <- parametros[(2*k)+1]                 #Parámetro de velocidad de transición
    c                     <- parametros[(2*k)+2]                 #Umbral de transición

    f_trans               <- func_trans(z, gamma, c)                            #Función de transición
    y_estim               <- X %*% Phi_lineal + X %*% theta_nolineal*f_trans    #Variable endógena estimada
    residuos              <- y_dep - y_estim                                    #Residuos del modelo
    sigma2                <- mean(residuos^2)                                   #Sigma^2 en el logaritmo de verosimilitud
    Logverosimil          <- -0.5 * length(y_dep) * (log(2 * pi * sigma2) + 1)  #Logaritmo de verosimilitud
    return(-Logverosimil)
  }
  
  #Valores iniciales de los parámetros
  param_inicio <- c(rep(0, 2*k), 'gamma'=1, 'c'=mean(s))                      
  
  #Optimización del logaritmo de verosimilitud
  resultado <- optim(par     = param_inicio, 
                     fn      = Logverosimil_funcion,
                     method  = "BFGS",
                     hessian =TRUE,
                     control =list(maxit=5000))
  
  print(resultado$convergence)
  print(resultado$message)
  
  #Obtención de parámetros estimados con sus respectivos nombres
  param_lineal          <- resultado$par[1:k]
  names(param_lineal)   <- paste0('phi_', 0:(k-1))
  param_nolineal        <- resultado$par[(k+1):(2*k)]
  names(param_nolineal) <- paste0('theta_', 0:(k-1))
  gamma                 <- resultado$par[(2*k)+1]
  c                     <- resultado$par[(2*k)+2]
  
  #Tabla resumen de los parámetros lineales
  tabla_lin <- data.frame(
    var_param       = colnames(X), 
    param_estim     = round(param_lineal, 6),
    tipo            = 'lineal'
  )
  
  #Tabla resumen de los parámetros  no lineales
  tabla_nolin <- data.frame(
    var_param   = colnames(X), 
    param_estim = round(param_nolineal, 6),
    tipo        = 'no lineal'
  )
  
  #Tabla resumen de los demás parámetros
  tabla_otros <- data.frame(
   var_param   = c('gamma', 'c'),
   param_estim = round(c(gamma, c), 6),
   tipo        = c('velocidad de transición', 'umbral de transición')
  )
  
  #Pruebas de signficancia 
  coeficientes  <- resultado$par                  #coeficientes estimados
  varcov_matriz <- solve(resultado$hessian)       #Matriz de varianzas y covarianzas de los estimadores
  desv_est      <- sqrt(diag(varcov_matriz))      #Desviación estándar de los estimadores
  z_est         <- coeficientes/desv_est          #Estadístico z según el test de Wald
  p_values      <- 2 * (1 - pnorm(abs(z_est)))    #p-values arrojados
  
  #Tabla resumen de la significancia de los parámetros estimados
  tabla_signif <- data.frame(
    desv_est    = desv_est ,
    z_est       = z_est,
    p_value     = p_values
  )
  
  #Tabla resumen total
  tabla_parametros <- rbind(tabla_lin, tabla_nolin, tabla_otros)
  rownames(tabla_parametros)
  tabla_global     <- cbind(tabla_parametros, tabla_signif)
  rownames(tabla_global)
  
  tabla_global$signif <- ifelse(
    tabla_global$p_value < 0.001, '***',
    ifelse(tabla_global$p_value < 0.01, '**',
           ifelse(tabla_global$p_value < 0.05, '*',
                 ifelse(tabla_global$p_value < 0.1, '.', ''
                        )
                 )
           )
    )
  
  return(list(
    resumen = tabla_global,
    parámetros = resultado$par, 
    logLik = -resultado$value
    )
    )
}

#5.1 Creación de una función para eliminar variables no signficativas iterativamente---------------

str_simplificado <- function(str_original) {
  
  #str_original: Es la estimación de un modelo STR sin elininar variables no significativas 
  
  #Llamar el modelo STR estimado con antelación
  y            <- str_original$inputs$y
  x            <- str_original$inputs$x
  s            <- str_original$inputs$s
  rez_s        <- str_original$inputs$rez_s
  rez_y.actual <- str_original$inputs$rez_y
  rez_x.actual <- str_original$inputs$rez_x
  G            <- str_original$inputs$G
  resumen      <- str_original$resumen
  
  #Iniciar el modelo original si no se elimina nada
  resultado <- str_original
  
  #Proteger la variable de transición 
  if (identical(s, y)) {
    var_transicion <- paste0('y_L', rez_s)
  } else if (identical(s, x)) {
    var_transicion <- paste0('x_L', rez_s)
  } else {
    stop('La variable de transición no coincide con y ni con x')
  }
  
  #Proteger variables que no se pueden eliminar
  var_importantes <- c('intercepto', 'gamma', 'c', var_transicion)
  
  #Crear vector donde se guardarán las variables a eliminar
  eliminadas <- character(0)
  iter <- 1
  max_iter <- 20
  
  repeat {
    
    #Identificación de variables no signifcativas (p-value > 0.1)
    var_nosignif <- resumen$var_param[
      resumen$p_value > 0.1 & 
        !resumen$var_param %in% var_importantes &
        resumen$tipo %in% c('lineal', 'no lineal')
    ]
    
    #Condición de salida
    if (length(var_nosignif) == 0 || iter > max_iter) {
      cat("Finalizando iteración. Variables eliminadas:", paste(eliminadas, collapse = ", "), "\n")
      resultado <- modelo_simplificado  
      break
    }
    
    #Identificar la variable con el mayor p-value
    var_eliminar <- var_nosignif[which.max(resumen$p_value[resumen$var_param %in% var_nosignif])]
    p_val <- round(resumen$p_value[resumen$var_param == var_eliminar], 6)
    cat(sprintf("Iteración %d: Eliminando %s (p-value = %s)\n", iter, var_eliminar, p_val))
    
    
    eliminadas <- c(eliminadas, var_eliminar)
    
    #Ajustar rezagos según el tipo de variable eliminada
    
    if (startsWith(var_eliminar, 'x_L')) {
      rez_num <- as.numeric(sub('x_L', '', var_eliminar))
      rez_x.actual <- rez_x.actual[rez_x.actual != rez_num]
    } else if (startsWith(var_eliminar, 'y_L')) {
      rez_num <- as.numeric(sub('y_L', '', var_eliminar))
      rez_y.actual <- rez_y.actual[rez_y.actual != rez_num]
    } else {
      warning(paste('Variable no identificada para eliminación:', var_eliminar))
      repetir <- FALSE
    }
    
    
    #Protección antes de estimar de nuevo el modelo 
    if (length(rez_y.actual) == 0 && length(rez_x.actual) == 0) {
      cat("Sin variables explicativas. Se detiene.\n")
      break
    }
    
    #Volver a estimar el modelo con los nuevos rezagos
    modelo_simplificado <- str_mod(y, x, s, rez_s, rez_y.actual, rez_x.actual, G)
    
    #Actualizar resultados
    tabla_global <- modelo_simplificado$resumen
    resumen   <- modelo_simplificado$resumen
    resultado <- modelo_simplificado
    
    iter <- iter + 1
    
  }
  

  return(list(
    resumen = resultado$resumen,
    parámetros = resultado$parámetros, 
    logLik = -resultado$logLik, var_elimin=eliminadas))
}  


#6. Estimación de un modelo STR para ENSO-------------------

#6.1 Selección de rezagos de ENSO como variable explicativa p1-----

ENSO_ARIMA <- auto.arima(ENSO, max.p=20, max.q=0, ic="aic")
ENSO_ARIMA

summary(ENSO_ARIMA)
cat('La cantidad máxima de rezagos es de 20, de los cuales se selecionan 5 rezagos para ENSO como variable explicativa')

#6.2 Aplicación del test de Terarsvirta (1995) para ENSO------------------------
ENSO_NLtest <- terasvirta_testNL(y=ENSO, x=NULL, rez_y=5, rez_x, alfa=0.05)
ENSO_NLtest
cat('Según el test de no linealidad de Terarsvirta (1995), la variable de transición es ENSO_t-3 y la función de transición es una función logística LSTR')

ENSO_STR <- str_mod(y=ENSO, x=NULL, s=ENSO, rez_s=3, rez_y=5, rez_x=NULL, G="LSTR")
ENSO_STR

ENSO_STR.simplificado <- str_simplificado(ENSO_STR)

cat('Modelo STR para la serie ENSO, teniendo 3 rezagos de sí misma como variables explicativas, ENSO_t-3 como variable de transición y una función logística como función de transición')

#7. Estimación de un modelo STR para DINF---------------------------------------

#7.1. Estimación de un modelo ARIMAX para seleccionar los rezagos de DINF y de ENSO explicativos--------------

 #Creación de una función para estimar modelos ARIMAX y elegir los rezagos adecuados según crietrios AIC

 auto.arimax <- function(y, x_exo, rezmax_y, rezmax_x) {
   
   # y: es la serie explicada o variable endógena
   # x_exo: es la serie de una variable exógena cuyos rezagos explican a 'y'
   # rezmax_x: son los rezagos máximos de la variable exógena 'x' que se tienen en cuenta para elegir
   
   resultados=list()
   
     for (k in 1:rezmax_x) {
       x_exo.rez <- embed(x_exo, k + 1)[, -1]
       y_rez <- y[(k + 1):length(y)]
       
       mod_arimax <- auto.arima(y_rez,
                       xreg = x_exo.rez,
                       max.p = rezmax_y, max.q = 0,
                       max.d = 1,
                       seasonal = FALSE,
                       stepwise = FALSE,
                       approximation = FALSE,
                       ic="aic")
       cat('Para', k, 'rezago(s) de x, el modelo seleccionó', mod_arimax$arma[1], 'rezago(s) de y con AIC =', AIC(mod_arimax), "\n")
       
       resultados[[k]] <- list(mod_arimax = mod_arimax,
                               rezagos_x = k,
                               rezagos_y = mod_arimax$arma[1],
                               AIC = AIC(mod_arimax))
     }
       lista_AIC <- sapply(resultados, function(r) r$AIC)
       mejor_rezago <- which.min(lista_AIC)
       mejor_modelo <- resultados[[mejor_rezago]]
       
       cat("\n---\nMejor modelo:\n")
       cat("Rezagos de x:", mejor_modelo$rezagos_x, "\n")
       cat("Rezagos de y:", mejor_modelo$rezagos_y, "\n")
       cat("AIC:", mejor_modelo$AIC, "\n")
       
       return(resultados)
}

DINF_ARIMAX <- auto.arimax(y=DINF, x=ENSO, rezmax_y=12, rezmax_x=12)
DINF_ARIMAX
cat('Utilizando 12 rezagos máximos tanto para DINF como para ENSO, el mejor modelo ARIMAX para DINF con ENSO como variable exógena, es aquel en el que hay 3 rezagos de DINF y 12 rezagos de ENSO')


#7.2 Aplicación del test de Terarsvirta (1995) para DINF------------------------
DINF_NLtest <- terasvirta_testNL(y=DINF, x=ENSO, rez_y=3, rez_x=12, alfa=0.05)
DINF_NLtest
cat('Según el test de no linealidad de Terarsvirta (1995), la variable de transición es ENSO_t-11 y la función de transición es una función logística ESTR')



#7.3 Estimación del modelo STR--------------------------------------------------
DINF_STR <- str_mod(y=DINF, x=ENSO, s=ENSO, rez_s=11, rez_y=3, rez_x=12, G="ESTR")
DINF_STR
cat('Modelo STR para la serie DINF, teniendo 3 rezagos de sí misma y 12 rezagos de ENSO como variables explicativas, ENSO_t-11 como variable de transición y una función exponencial como función de transición')





