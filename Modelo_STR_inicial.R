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
str_mod <- function(y, x, s, rez_s, rez_y.lin=c(), rez_x.lin=c(), rez_y.nl=c(), rez_x.nl=c(), G) {
  #y = Variable endógena explicada
  #x = Variable exógena explicativa
  #s = Variable de la cual se usará algunos de sus rezagos para ser variable de transición
  #rez_s = Rezago de la variable s que se usará como variable de transición
  #rez_y.lin = vector de rezagos de la variable endógena 'y' en la parte lineal
  #rez_x.lin = vector rezagos de la variable exógena 'x' en la parte lineal
  #rez_y.nl = vector de rezagos de la variable endógena 'y' en la parte no lineal
  #rez_x.nl = vector rezagos de la variable exógena 'x' en la parte no lineal
  #G = Función de transición, 'LSTR' si es logística o 'ESTR' si es exponencial
  
  #Matriz de variables explicativas de la parte lineal hasta el rezago máximo
  if (is.null(x)) {
    rez_x.lin = NULL
    rez_x.nl  = NULL
    rez_max   <- max(rez_y.lin, rez_y.nl)
    
    #Matriz de variables explicativas
    base_explicativas           <- embed(y, rez_max+1)
    colnames(base_explicativas) <- paste0('y_L', c('', 1:rez_max))
    
    #Variable explicada
    y_dep <- base_explicativas[, 'y_L']
    
    #Variables explicativas deseadas según sus rezagos 
    explicativas.lin <- paste0("y_L", rez_y.lin) #Parte Lineal
    explicativas.nl  <- paste0("y_L", rez_y.nl)  #Parte no lineal
  
    } else {

    !is.null(rez_x.lin)
    !is.null(rez_x.nl) 
    rez_max <- max(rez_x.lin,rez_y.lin, rez_x.nl, rez_y.nl)
    
    #Matriz de variables explicativas
    base_explicativas           <- embed(cbind(y,x), rez_max+1)
    colnames(base_explicativas) <-c(
    paste0('y_L', c('', 1:rez_max)),
    paste0('x_L', c('', 1:rez_max))
    )
    
    #Variable explicada
    y_dep <- base_explicativas[, 'y_L']
    
    #Variables explicativas deseadas según sus rezagos
    explicativas.lin <- c(
    paste0("y_L", rez_y.lin),
    paste0("x_L", rez_x.lin)
    )
    explicativas.nl <- c(
      paste0("y_L", rez_y.nl),
      paste0("x_L", rez_x.nl)
    )
    
    }
  
  #Variables explicativas
  X <- cbind(intercepto=1, base_explicativas[, explicativas.lin, drop=FALSE]) #Variables explicativas para la parte lineal
  W <- cbind(intercepto=1, base_explicativas[, explicativas.nl, drop=FALSE]) #Variables explicativas de la parte no lineal
  k <- ncol(X) #número de variables explicativas de la parte lineal
  j <- ncol(W) #número de variables explicativas de la parte no lineal
  
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
  Logverosimil_funcion    <- function(parametros) {
    k                     <- ncol(X)                             #número de variables explicativas
    Phi_lineal            <- parametros[1:k]                     #parámetros de la parte lineal
    names(Phi_lineal)     <- paste0('phi_', 0:(k-1))             #nombres de los parámetros lineales
    theta_nolineal        <- parametros[(k+1):(k+j)]             #parámetros de la parte no lineal
    names(theta_nolineal) <- paste0('theta_', 0:(j-1))           #nombres de los parámetros no lineales
    gamma                 <- parametros[(k+j)+1]                 #Parámetro de velocidad de transición
    c                     <- parametros[(k+j)+2]                 #Umbral de transición

    f_trans               <- func_trans(z, gamma, c)                            #Función de transición
    y_estim               <- X %*% Phi_lineal + W %*% theta_nolineal*f_trans    #Variable endógena estimada
    residuos              <- y_dep - y_estim                                    #Residuos del modelo
    sigma2                <- mean(residuos^2)                                   #Sigma^2 en el logaritmo de verosimilitud
    Logverosimil          <- -0.5 * length(y_dep) * (log(2 * pi * sigma2) + 1)  #Logaritmo de verosimilitud
    return(-Logverosimil)
  }
  
  #Valores iniciales de los parámetros
  param_inicio <- c(rep(0, (k+j)), 'gamma'=1, 'c'=mean(z, na.rm=TRUE))                      
  
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
  param_nolineal        <- resultado$par[(k+1):(k+j)]
  names(param_nolineal) <- paste0('theta_', 0:(j-1))
  gamma                 <- resultado$par[(k+j)+1]
  c                     <- resultado$par[(k+j)+2]
  
  #Creación de función previa que ayuda a nombrar correctamente los parámetros. Ej: lineal_1.x_l1 --> el número coincide 
  indice_por_col <- function(nm) {
    nm <- trimws(as.character(nm))
    if (length(nm) == 0) return(integer(0))
    
    # Patrones válidos
    es_intercepto <- nm == "intercepto"
    es_yx_lag     <- grepl("^(?:y|x)_L[1-9][0-9]*$", nm)  # y_L1, x_L2, ... (>=1)
    
    # Si hay algo que no es ni intercepto ni y/x con lag >=1, error claro
    inval <- !(es_intercepto | es_yx_lag)
    if (any(inval)) {
      stop(
        "Regresores inválidos detectados (esperaba 'intercepto' o y_Lk/x_Lq con k,q>=1): ",
        paste(nm[inval], collapse = ", ")
      )
    }
    
    # Construye índices: 0 para intercepto, número para y/x
    out <- integer(length(nm))
    out[es_intercepto] <- 0L
    if (any(es_yx_lag)) {
      out[es_yx_lag] <- as.integer(sub("^(?:y|x)_L([0-9]+)$", "\\1", nm[es_yx_lag]))
    }
    out
  }
  
  idx_lin   <- vapply(colnames(X), indice_por_col, integer(1))
  idx_nl    <- vapply(colnames(W), indice_por_col, integer(1))
  
  #Tabla resumen de los parámetros lineales
  tabla_lin <- data.frame(
    var_param       = paste0("lineal_", idx_lin, ".", colnames(X)), 
    param_estim     = round(param_lineal, 6)
  )
  
  #Tabla resumen de los parámetros  no lineales
  tabla_nolin <- data.frame(
    var_param   = paste0("nolineal_", idx_nl, ".", colnames(W)), 
    param_estim = round(param_nolineal, 6)
  )
  
  #Tabla resumen de los demás parámetros
  tabla_otros <- data.frame(
   var_param   = c('gamma', 'c'),
   param_estim = round(c(gamma, c), 6)
  )
  
  #Pruebas de signficancia 
  coeficientes  <- resultado$par                  #coeficientes estimados
  
  #Garantizar que la matriz var-cov no admita Nas
  H <- resultado$hessian
  # simetriza por seguridad numérica
  H <- 0.5 * (H + t(H))
  
  #Intento 1: invertir directamente
  vcov_prueba <- tryCatch(solve(H), error = function(e) NULL)
  
  if (is.null(vcov_prueba) || any(!is.finite(diag(vcov_prueba))) || any(diag(vcov_prueba) < 0)) {
  
    #intento 2: regularizar a matriz definida positiva vía descomposición espectral
    eg  <- eigen(H, symmetric = TRUE)
    lam <- eg$values
    tol <- 1e-8 * max(1, max(abs(lam), na.rm = TRUE))
    lam[lam < tol] <- tol
    H_reg <- eg$vectors %*% diag(lam) %*% t(eg$vectors)
    varcov_matriz <- solve(H_reg)
  } else {
    varcov_matriz <- vcov_prueba
  }
  
  #Desviaciones estándar (trunca a >=0 para evitar -0 por redondeo)
  diag_var <- pmax(diag(varcov_matriz), 0)
  desv_est <- sqrt(diag_var)
  
  z_est    <- coeficientes / desv_est #Estadístico z calculado
  p_values <- 2 * pnorm(-abs(z_est))  #p-values de los parámetros
  
  #limpia infinitos/NaN
  z_est[!is.finite(z_est)]       <- NA_real_
  p_values[!is.finite(p_values)] <- NA_real_
  
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
    resumen    = tabla_global,
    parámetros = resultado$par, 
    logLik     = -resultado$value,
    y          = y,
    x          = x,
    s          = s,
    rez_s      = rez_s, 
    rez_x.lin  = rez_x.lin,
    rez_y.lin  = rez_y.lin,
    rez_x.nl   = rez_x.nl,
    rez_y.nl   = rez_y.nl,
    G          = G
    )
    )
}

#5.1 Creación de una función para eliminar variables no signficativas iterativamente---------------

str_simplificado <- function(str_original) {
  
  #str_original: Es la estimación de un modelo STR sin eliminar variables no significativas 
  
  #Función previa que ayuda a extraer correctamente los nombres del parámetro. Ej: lineal_2.x_L2 --> el número coincide
  extrae_lado_derecho <- function(v) sub("^.*?\\.", "", v)
  
  #Llamar los argumentos del modelo orginal
  y           <- str_original$y
  x           <- str_original$x
  s           <- str_original$s
  rez_s       <- str_original$rez_s
  rez_y.lin   <- str_original$rez_y.lin
  rez_x.lin   <- str_original$rez_x.lin
  rez_y.nl    <- str_original$rez_y.nl
  rez_x.nl    <- str_original$rez_x.nl
  G           <- str_original$G
  resumen     <- str_original$resumen
  
  #Identificar la variable de transición 
  if (identical(s, y)) {
    nombre_transicion <- paste0("y_L", rez_s)
  } else if (!is.null(x) && identical(s, x)) {
    nombre_transicion <- paste0("x_L", rez_s)
  } else {
    stop("La variable de transición no coincide con 'y' ni con 'x'")
  }
  
  
  #Eliminar parámetros no significativos iterativamente
  repetir <- TRUE
  while (repetir) {
    resumen <- str_original$resumen
    
    #Proteger las variables importantes: interceptos, variable de transición, gamma y c
    lado_der        <- extrae_lado_derecho(resumen$var_param)
    var_importantes <- (resumen$var_param %in% c("lineal_0.intercepto","nolineal_0.intercepto","gamma","c")) |
        (lado_der == nombre_transicion)
      
    #Identificar variables no significativas  
    var_nosignif <- subset(
                    resumen,
                    !var_importantes &
                     (p_value > 0.1)
                    )
    
    if (nrow(var_nosignif) == 0) {
      repetir <- FALSE
      break
    }
    
    mayor.p_value <- which.max(var_nosignif$p_value)
    peor_variable <- var_nosignif$var_param[mayor.p_value]
    
    lado <- sub("^.*?\\.", "", peor_variable)        # ej: "y_L11" o "x_L2"
    
    # patrón: y_Lk / x_Lq con k,q >= 1  (usa grupos CAPTURANTES)
    m <- regexec("^(y|x)_L(\\d+)$", lado)
    hits <- regmatches(lado, m)[[1]]
    if (length(hits) < 3) {
      warning(sprintf("Nombre inesperado '%s' (lado='%s'). Detengo para evitar inconsistencias.",
                      peor_variable, lado))
      break
    }
    var_yx <- hits[2]                 # "y" o "x"
    rez    <- as.integer(hits[3])     # número del rezago (>=1)
    
    # --- ACTUALIZAR EL CONJUNTO CORRECTO SEGÚN PARTE (lineal/nolineal) Y VARIABLE (y/x) ---
    if (grepl("^lineal_", peor_variable) && var_yx == "y") {
      rez_y.lin <- setdiff(rez_y.lin, rez)
    } else if (grepl("^nolineal_", peor_variable) && var_yx == "y") {
      rez_y.nl  <- setdiff(rez_y.nl,  rez)
    } else if (grepl("^lineal_", peor_variable) && var_yx == "x") {
      rez_x.lin <- setdiff(rez_x.lin, rez)
    } else if (grepl("^nolineal_", peor_variable) && var_yx == "x") {
      rez_x.nl  <- setdiff(rez_x.nl,  rez)
    } else {
      warning(sprintf("Patrón no reconocido para '%s'. Detengo para evitar bucle.", peor_variable))
      break
    }
    
    #if (grepl("^lineal_\\d+\\.y_L", peor_variable)) {
      #rez       <- as.numeric(sub("^lineal_\\d+\\.y_L", "\\1", peor_variable))
      #rez_y.lin <- setdiff(rez_y.lin, rez)
    #} else if (grepl("^nolineal_\\d+\\.y_L", peor_variable)) {
      #rez       <- as.numeric(sub("^nolineal_\\d+\\.y_L", "\\1", peor_variable))
      #rez_y.nl  <- setdiff(rez_y.nl, rez)
    #} else if (grepl("^lineal_\\d+\\.x_L", peor_variable)) {
      #rez <- as.numeric(sub("^lineal_\\d+\\.x_L", "\\1", peor_variable))
      #rez_x.lin <- setdiff(rez_x.lin, rez)
    #} else if (grepl("^nolineal_\\d+\\.x_L", peor_variable)) {
      #rez <- as.numeric(sub("^nolineal_\\d+\\.x_L", "", peor_variable))
      #rez_x.nl  <- setdiff(rez_x.nl, rez)
    #} else {
      #warning("No se reconoce el tipo de parámetro a eliminar.")
      #break
    #}
    
    #Reestimar modelo con los rezagos ajustados
    str_original <- str_mod(
      y         = y,
      x         = x,
      s         = s,
      rez_s     = rez_s,
      rez_y.lin = rez_y.lin,
      rez_x.lin = rez_x.lin,
      rez_y.nl  = rez_y.nl,
      rez_x.nl  = rez_x.nl,
      G         = G
    )
  }
  
  return(str_original)
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

ENSO_STR <- str_mod(y=ENSO, x=NULL, s=ENSO, rez_s=3, rez_y.lin=1:5, rez_x.lin = NULL, rez_y.nl=1:5, rez_x.nl = NULL,  G="LSTR")
ENSO_STR

ENSO_STR.simplificado <- str_simplificado(ENSO_STR)
ENSO_STR.simplificado

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
DINF_STR <- str_mod(y=DINF, x=ENSO, s=ENSO, rez_s=11, rez_y.lin=1:3, rez_x.lin = 1:12, rez_y.nl=1:3, rez_x.nl = 1:12,  G="ESTR")
DINF_STR
cat('Modelo STR para la serie DINF, teniendo 3 rezagos de sí misma y 12 rezagos de ENSO como variables explicativas, ENSO_t-11 como variable de transición y una función exponencial como función de transición')

DINF_STR.simplificado <- str_simplificado(DINF_STR)


