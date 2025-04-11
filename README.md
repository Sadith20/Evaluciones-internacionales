# Funciones para resultados en las Evaluaciones Internacionales
Estas funciones fueron desarrolladas con el objetivo de calcular medidas como los promedios de rendimiento y los niveles de desempeño de los estudiantes en las evaluaciones internacionales en las que participa el Perú. Entre estas evaluaciones se encuentran el Programa para la Evaluación Internacional de Estudiantes (PISA), el Estudio Internacional de Educación Cívica y Ciudadana (ICCS) y el Tercer Estudio Regional Comparativo y Explicativo (TERCE).

Dado que estas evaluaciones se basan en diseños metodológicos complejos, las funciones han sido diseñadas para tener en cuenta aspectos técnicos fundamentales. Entre ellos se encuentran el muestreo estratificado y por conglomerados, el uso de métodos de remuestreo a través de pesos replicados (tales como el jackknife o el bootstrap) y la incorporación de valores plausibles, que permiten representar de manera adecuada la distribución del rendimiento de los estudiantes. Estas herramientas estadísticas permiten obtener resultados representativos y comparables entre países, garantizando la solidez de los análisis derivados de estas evaluaciones.

## PISA
El diseño muestral de PISA es por conglomerados, estratificado y bietápico. La primera unidad de muestreo es la escuela (conglomerado), y la segunda, los estudiantes. La base de datos de PISA incluye 10 valores plausibles por cada área evaluada y 80 pesos replicados, calculados mediante el método BRR-Fay.

Para el cálculo de estimaciones, se tomó como referencia el documento PISA Data Analysis Manual, el cual puede descargarse en el siguiente enlace:
👉 https://www.oecd.org/en/publications/pisa-data-analysis-manual-spss-second-edition_9789264056275-en.html

La funciones MP_PISA y NL_PISA requieren los siguientes argumentos:

** data : Base de datos con la información a procesar.

** estrato: Nivel de desagregación para los resultados del estudio. Puede ser por país (COUNTRY), por sexo (TFGender), por área geográfica (area), o por tipo de gestión de la institución educativa (gestion).

** curso: Asignatura evaluada. En el PISA se incluyen Lectura, Matemática y Ciencias.

** año: Año del estudio, que puede ser 2009, 2012, 2015, 2018 y 2022.

A continuación, se pueden descargar las funciones para el cálculo de la medida promedio y los niveles de logro en las evaluaciones internacionales:
Medida promedio : devtools::source_url("https://raw.githubusercontent.com/Sadith20/Evaluciones-internacionales/refs/heads/main/Funcion_MP_PISA_VF.R")

Niveles de logro : devtools::source_url("https://raw.githubusercontent.com/Sadith20/Evaluciones-internacionales/refs/heads/main/Funcion_NL_PISA_VF.R")

 ## LLECE
El estudio TERCE/ERCE evalúa a estudiantes de tercero y sexto grado de primaria en diversas áreas del conocimiento.

Tercer grado: Lectura, Matemática y Escritura.
Sexto grado: Lectura, Matemática, Escritura y Ciencias.
El diseño muestral es por conglomerados, estratificado y bietápico. La primera unidad de muestreo es la escuela (conglomerado), y la segunda, una sección dentro de cada escuela.

La base de datos de TERCE/ERCE incluye 5 valores plausibles por cada área evaluada y 100 pesos replicados, calculados mediante el método BRR-Fay.

Las principales variables a considerar en la base de datos son:

- Peso final del estudiante: wgl, wgm, wgc
- Pesos replicados: BRR1 a BRR100
- Valores plausibles por área evaluada: VP1 a VP5

A continuación, se pueden descargar las funciones para el cálculo de la medida promedio y los niveles de logro en las evaluaciones internacionales:
- Primer intento
  
Medida promedio : devtools::source_url("https://raw.githubusercontent.com/Sadith20/Evaluciones-internacionales/refs/heads/main/Funcion%20mp_erce.R")
Niveles de logro : devtools::source_url("https://raw.githubusercontent.com/Sadith20/Evaluciones-internacionales/refs/heads/main/Funcion%20nl_ERCE.R")

 - Segundo intento
   
Ambas funciones requieren los siguientes argumentos:

** data : Base de datos con la información a procesar.

** estrato: Nivel de desagregación para los resultados del estudio. Puede ser por país (COUNTRY), por sexo (SEX), por área geográfica (RURAL), o por tipo de gestión de la institución educativa (DEP).

** grado: Nivel educativo evaluado. En el ERCE se consideran dos grados: 3P (tercer grado de primaria) y 6P (sexto grado de primaria).

** curso: Asignatura evaluada. En el TERCE se incluyen Lectura, Matemática y Ciencias solo para sexto grado de primaria.

** año: Año del estudio, que puede ser 2013 o 2019.

Medida promedio : devtools::source_url("https://raw.githubusercontent.com/Sadith20/Evaluciones-internacionales/refs/heads/main/Funcion_MP_ERCE_VF.R")
Niveles de logro : devtools::source_url("https://raw.githubusercontent.com/Sadith20/Evaluciones-internacionales/refs/heads/main/Funcion_NL_ERCE_VF.R")

 ## ICCS
El Estudio Internacional de Educación Cívica y Ciudadanía (ICCS) tiene como objetivo proporcionar información sobre el conocimiento y la comprensión que los estudiantes poseen respecto a conceptos y temas relacionados con la educación cívica y ciudadana. Además, busca analizar sus valores, actitudes y comportamientos ciudadanos, así como las características del contexto en el que se desarrollan estos aprendizajes.

El diseño muestral de ICCS es por conglomerados, estratificado y bietápico. La primera unidad de muestreo es la escuela (conglomerado) y la segunda, una sección dentro de cada escuela.

La base de datos de ICCS incluye 5 valores plausibles por cada área evaluada.
 
