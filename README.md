# Funciones para resultados en las Evaluaciones Internacionales
Estas funciones fueron diseñadas para calcular indicadores como la medida promedio y los niveles de desempeño en las evaluaciones internacionales en las que participa Perú (PISA, ICCS, TERCE). 
Estas evaluaciones se basan en diseños de muestreo complejo, métodos de remuestreo con pesos replicados y el uso de valores plausibles.

## PISA
El Programa para la Evaluación Internacional de Estudiantes (PISA) es un estudio comparativo desarrollado por la Organización para la Cooperación y el Desarrollo Económico (OCDE). Su objetivo principal es evaluar en qué medida los estudiantes de 15 años, próximos a finalizar su educación básica, han adquirido las competencias necesarias para desenvolverse en el mundo actual, continuar aprendiendo a lo largo de su vida y participar plenamente en la sociedad.

El diseño muestral de PISA es por conglomerados, estratificado y bietápico. La primera unidad de muestreo es la escuela (conglomerado), y la segunda, los estudiantes. La base de datos de PISA incluye 10 valores plausibles por cada área evaluada y 80 pesos replicados, calculados mediante el método BRR-Fay.

Las principales variables a considerar en la base de datos son:

- Peso final del estudiante: W_FSTUWT
- Pesos replicados: W_FSTURWT1 a W_FSTURWT80
- Valores plausibles por área evaluada:
- Matemática: PV1MATH a PV10MATH
- Lectura: PV1READ a PV10READ
- Ciencias: PV1SCIE a PV10SCIE

A continuación, se pueden descargar las funciones para el cálculo de la medida promedio y los niveles de logro en las evaluaciones internacionales:

Medida promedio : devtools::source_url("https://raw.githubusercontent.com/Sadith20/Evaluciones-internacionales/refs/heads/main/Funcion%20mp_pisa.R")

Niveles de logro : devtools::source_url("https://raw.githubusercontent.com/Sadith20/Evaluciones-internacionales/refs/heads/main/Funcion%20nl_pisa.R")

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

Medida promedio : devtools::source_url("https://raw.githubusercontent.com/Sadith20/Evaluciones-internacionales/refs/heads/main/Funcion%20mp_erce.R")

 

 ## ICCS
El Estudio Internacional de Educación Cívica y Ciudadanía (ICCS) tiene como objetivo proporcionar información sobre el conocimiento y la comprensión que los estudiantes poseen respecto a conceptos y temas relacionados con la educación cívica y ciudadana. Además, busca analizar sus valores, actitudes y comportamientos ciudadanos, así como las características del contexto en el que se desarrollan estos aprendizajes.

El diseño muestral de ICCS es por conglomerados, estratificado y bietápico. La primera unidad de muestreo es la escuela (conglomerado) y la segunda, una sección dentro de cada escuela.

La base de datos de ICCS incluye 5 valores plausibles por cada área evaluada.
 
