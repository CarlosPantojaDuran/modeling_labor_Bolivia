/*

		Diplomado en Métodos Cuantitativos para el Análisis Económico
		Trabajo Final: Modelos logit binomiales y multinomiales
		Docente: Andrea Alcaraz
		input: EH2021.dta
		output: modelos logit binomial y multinomiales
*/

* Limpiar la zona de trabajo
clear all
cls
macro drop all

	cd "/Users/usuario1/Documents/4. Microeconometria I/_DATA"

	use "EH2021_Persona"
describe, short //42.090 observaciones y 357 variables

********************************************************************************
* FASE I: IDENTIFICACIÓN
********************************************************************************
 

* VARIABLES IDENTIFICADAS
// Nro, Folio, Estrato, UPM, Factor, depto, area, sexo, pertenencia étnica, ylab, aestudio, nivel, ocupación, pet, pea, pei, ocupado, cesante, aspirante, desocupado, temporal, permanente.

svyset upm [pw = factor], strata(estrato) || folio
********************************************************************************
* VARIABLE DE ENDÓGENA (DEPENDIENTE/EXPLICADA)
********************************************************************************

********************************************************************************
* INGRESOS: Los tipos de ingreso considerados son ylab, yprilab, yseclab, yper ^ yhopc
********************************************************************************

codebook yper ylab yprilab yseclab yhogpc // Todas variables son numéricas del tipo double
sum yper ylab yprilab yseclab yhogpc
* Sin aplicar el factor de expansión ni acotar las observaciones, las medias de los ingresos son:
* yper: 1432,511
* ylab: 3054.763
* yprilab: 3002.504
* yseclab: 1013.799
* yhogpc: 1432.175

* yper: ingreso personal es igual al ingreso mensual total percibido por los miembros del hogar, ya sea por fuente laboral y no laboral. (42.090 observaciones) Universo - Todos los miembros del hogar

* ylab: Ingreso laboral mensual que percibe el trabajador en su primera y/o segunda ocupación. (16.362 observaciones) - universo personas de 7 años o más de edad

* yprilab: Ingreso laboral mensual percibido por el trabajador en la ocupación principal, sea como asalariado o independiente. (16325 observaciones) - universo personas de 7 años o más de edad

* yseclab: Ingreso laboral mensual percibido por el trabajador en la ocupación secundaria, sea como asalariado o independiente. (953 observaciones) - universo personas de 7 años o más de edad

* yhogpc: Relación del ingreso mensual total del hogar y el número de miembros del hogar, excluyendo a empleadas/os domésticos del hogar y/o parientes de estos. (42.061 observaciones) - Universo: Todos los miembros del hogar.

***************************************
* INGRESO LABORAL: 
* Ingreso laboral (Bs/Mes)
codebook ylab // Missing .: 25,728/42,090  (16362 observaciones)
sum ylab // mean: 3054.763
sum ylab [iweight=factor] // La media es 3033.044

sum ylab [iweight=factor] if s01a_03>=7  // la media del ylab es Bs.3033.044
sum ylab [iweight=factor] if ylab!= . // la media del ylab en Bolivia es Bs. 3033.044

histogram ylab, normal normopts(lcolor(green)) kdensity kdenopts(lcolor(red)) title("Histograma de Ingresos Laborales") subtitle("EH-2021") note("Fuente: INE")

graph box ylab, title("Ingreso Laboral") note("Fuente: INE - EH2021")

* criterio de ingresos laborales escala salarial min eco fin pub 
graph box ylab if ylab>2362 & ylab<25000

graph box ylab if ylab!=., scale(1) graphregion(color(white)) title("Boxplot: Ingreso Laboral (Bs/Mes)")

* Agrupación de los ingresos en intervalos
recode ylab (0/2362=1) (2362.01/5000=2) (5001/10000 =3) (10001/15000=4) (15001/20000=5) (20001/25000=6) (25001/30000=7) (30001/35000 =8) (35001/40000=9) (40001/45000 =10) (45001/50000=11) (50001/55000=12) (55001/60000=13), gen (ylab_int)
label define label_ylab_int 1 "0-2362" 2 "2363-5000" 3 "5001-10000" 4 "10001-15000" 5 "15001-20000" 6 "20001-25000" 7 "25001-30000" 8 "30001-35000" 9 "35001-40000" 10 "40001-45000" 11 "45001-50000" 12 "50001-55000" 13 "55001-60000" 
label value ylab_int label_ylab_int

tab ylab_int [iweight= factor]
* El salario mínimo de la gestión 2023 fue Bs.2.362
* El 45,15% de la población vive con un salario igual o menor al mínimo.
* El 85,76% de la población tiene un ingreso laboral menor o igual a Bs. 5.000.
* Sólo un 12,33% de la población tiene un ingreso laboral entre Bs.5.001-Bs.10.000 y un 1,41% gana entre Bs.10.001 y Bs.15.000
* hist(ylab_int) [iweight= factor], addlabel xlabel(, labels valuelabel) title("Histograma Ingreso Laboral")

* Análisis de la variable ylab
ladder ylab
gladder ylab
* Transformación funcional de ylab
gen lylab =ln(ylab) // logaritmo natural
hist lylab, normal normopts(lcolor(green)) kdensity kdenopts(lcolor(red)) title("Histograma de Ingresos Laborales en Logaritmos") subtitle("EH-2021") note("Fuente: INE")

gen sylab= (ylab)^(1/2) // raíz cuadrada
hist sylab, normal normopts(lcolor(green)) kdensity kdenopts(lcolor(red)) title("Histograma de Ingresos Laborales en Raíz Cuadrada") subtitle("EH-2021") note("Fuente: INE")
* En esta ocupación usted trabaja como: (s04b_12): 1. Obrero/Empleado 2. Empleador/a socio que sí recibe salario 3. Trabajador/a por cuenta propia 4. Empleador/a o socio/a que no recibe salario 5. Cooperativista de producción 6. Trabajador/a familiar sin remuneración 7. Aprendiz o persona en formación sin remuneración 8. Empleada/o del hogar

********************************************************************************
* DATOS GENERALES Y DEMOGRÁFICOS
********************************************************************************

* 1. FOLIO: Identificador alfanumérico único de cada hogar encuestado, que esta compuesto por los códigos de UPM, el número de orden de la vivienda y el número de hogar existente en la vivienda seleccionada.
codebook folio

* 2. NRO: Todos los miembros del hogar.
codebook nro

* Generarmos una variable con el número máximo de personas por folio/familia
bysort folio: egen nro_max = max(nro)

tab nro_max [iweight=factor]
tab nro_max area [iweight=factor], row column

sum nro_max [weight=factor], d
* El hogar promedio en Bolivia, está conformado por 4,16 personas y la mediana es de 4 personas.
* 14 es el número máximo de personas en una familia.

* 3. GENERO
codebook s01a_02
tab s01a_02 [iweight=factor]
recode s01a_02 (1=0) (2=1), gen (sex)

codebook sex
* Labels
label variable sex "Género"

label define label_sex 0 "Hombre" 1 "Mujer"
label value sex label_sex

tab sex [iweight=factor]
* Se estima que existen 11,903,958 personas en Bolivia para el año 2021.
* 49,25% de la población son hombres y 50,75% son mujeres

hist sex, discrete barwidth(0.7) addlabel xlabel(, labels valuelabel) title("Histograma Variable Género") caption(Fuente: INE)

graph pie, over(sex) pie(1, explode) plabel(_all percent, format(%9.1f)) title("Gráfico Variable Género") note("Fuente: INE -  EH2021")

* 4. EDAD Primer Infancia
tab s01a_03 [iweight=factor]
sum s01a_03 [iweight=factor]
* La edad promedio de los bolivianos para el año 2021 es 30,1 años y la máxima edad fueron 98 años.

gen edad = s01a_03
* el código de la variable educación se modificó respecto al 2019
recode edad 0/5=1 6/10=2 11/15 =3 16/20=4 21/25=5 26/30=6 31/35=7 36/40 =8 41/45=9 46/50=10 51/55=11 56/60=12 61/65=13 66/70=14 71/75=15 76/80=16 81/85=17 86/90=18 91/95=19 96/100=20
label define label_edad 1 "0-5" 2 "6-10" 3 "11-15" 4 "16-20" 5 "21-25" 6 "26-30" 7 "31-35" 8 "36-40" 9 "41-45" 10 "46-50" 11 "51-55" 12 "56-60" 13 "61-65" 14 "66-70" 15 "71-75" 16 "76-80" 17 "81-85" 18 "86-90" 19 "91-95" 20 "96-100"
label value edad label_edad
tab edad [iweight=factor]
table ( edad ) ( sex) () [iweight=factor]

* Para armar la pirámide poblacional
tab edad sex [iweight=factor]

* Indice de dependencia = 0.58371537
display (3482112 + 905370)/7516475
* Población menor de 15 años
tab s01a_03 [iweight=factor] if s01a_03<15
*  3,482,112
* Población mayor a 64 años
tab s01a_03 [iweight=factor] if s01a_03>64
* 905,370.18
* Población de 15 a 64 años
tab s01a_03 [iweight=factor] if s01a_03>=15 & s01a_03<=64
* 7,516,475

* Clasificación categórica: primera infancia, niños, adolescentes, jóvenes, adultos y adultos mayores
recode s01a_03 0/4=1 5/12=2 13/17 =3 18/30=4 31/65=5 66/80=6 81/100=7, gen (grupedad)

label define label_grupedad 1 "Primera infancia" 2 "Niños" 3 "Adolescentes" 4 "Jóvenes" 5 "Adultos" 6 "Tercera Edad" 7 "Cuarta Edad"
label value grupedad label_grupedad

tab grupedad [iweight=factor]
* El grupo de personas más grande es el conformado por adultos (37,27%).
* El grupo de personas jóvenes es 21,07% y el tercer grupo en importancia es de 16,73%

tab grupedad sex [iweight=factor]

table ( depto ) ( grupedad ) () [iweight=factor]
table ( depto ) ( grupedad) () [iweight=factor], statistic(percent) nformat(%9.1f percent) sformat(`"%s%%"' percent)

graph pie [pweight = factor], over(grupedad) plabel(_all percent, format(%9.1f)) title("Histograma por Grupo de Edad") caption(Fuente: INE - EH2021)

graph pie [pweight = factor], over(grupedad) plabel(_all percent, format(%9.1f)) pie(5, explode) pie(6, explode) title("Grupos Etáreos Bolivia", size(medlarge) ring(5)) note("Fuente: INE -  EH2021") legend(size(vsmall))

* 4. RELACION DE PARENTESCO
codebook s01a_05
tab s01a_05 [iweight= factor] 
* De los 11,9 millones de personas, 3,638 millones son jefes de hogar.

recode s01a_05 1=1 2/12=0, gen (jefe_h) // Jefe de hogar
label define label_jefeh 1 "Jefe de hogar" 0 "Otro"
label value jefe_h label_jefeh
tab jefe_h [iweight= factor]

* 5. PERTENENCIA ÉTNICA
* Como boliviana o boliviano ¿A que nación o pueblo indígena originario o campesino pertenece?
codebook s01a_09
tab s01a_09 [iweight=factor]
* 26,49% Pertenece y 73,33% No pertenece a una nación o pueblo indígena originario o campesino

recode s01a_09 (1=1) (2/3=0), gen(npioc)
label define npioc 1 "pioc" 0 "no_pioc"
label value npioc npioc

tab npioc [iweight=factor]

hist npioc, discrete barwidth(0.8) addlabel xlabel(, labels valuelabel) title("Histograma Variable NPIOC") caption(Fuente: INE) // 

graph pie [pweight = factor], over(s01a_09) plabel(_all percent, format(%9.1f)) title("Variable NPIOC") caption(Fuente: INE - EH2021)

* Como boliviana o boliviano ¿A que nación o pueblo indígena originario campesino o afro boliviano pertenece?¿A cuál? (s01a_09npioc)
codebook s01a_09npioc
tab s01a_09npioc [iweight=factor]
* 43,11% de las personas que consideran pertenecer a NPIOC son Aymaras
* 48,41% de las personas que consideran pertenecer a NPIOC son Quechuas

hist s01a_09npioc, discrete title("Histograma NPIOC") caption(Fuente: INE) // colocar el nombre de los NPIOC en el eje X

graph pie [pweight = factor], over(s01a_09npioc) pie(3, explode) pie(15, explode) plabel(3 percent) plabel(15 percent) title("DETALLE NPIOC", size(medlarge) ring(0)) note("Fuente: INE -  EH2021") legend(size(vsmall))
* 43,11% de las personas que se identican como NPIOC, pertenecen a la Nación Aymara.
* 48,41% de las personas que se identifican como NPIOC, pertenecen a la Nación Quechua

* 6. IDIOMA MATERNO
* ¿Cuál es el idioma o lengua en el que aprendió a hablar en su niñez?
codebook s01a_08
tab s01a_08 [iweight= factor]
* El 76,03% de la población aprendió a hablar en Castellano, sólo el 8,09% Aymara y el 14,89% en Quechua.

graph pie [pweight = factor], over(s01a_08) pie(1, explode) pie(3, explode) pie(12, explode) plabel(1 percent) plabel(3 percent) plabel(12 percent) title("Idioma Materno", size(medlarge)ring(5)) note("Fuente: INE -  EH2021") legend(size(vsmall))

* Transformación a variable dicotómica
recode s01a_08 2/4=1 6=2 7/33 =1 41/58=3, gen(impioc)

label define label_impioc 1 "impioc" 2 "castellano" 3 "idioma extranjero"
label value impioc label_impioc
tab impioc [iweight=factor]

* 7. IDIOMA EN QUE HABLA
* ¿Qué idiomas habla, incluidos los de las naciones y pueblos indígena originar
codebook s01a_07_1
tab s01a_07_1 [iweight=factor]
* El 76,94% de la población aprendió a hablar en Castellano, sólo el 8,39% Aymara y el 14,11% en Quechua

graph pie [pweight = factor], over(s01a_07_1) pie(1, explode) pie(4, explode) pie(11, explode) plabel(1 percent) plabel(4 percent) plabel(11 percent) title("Idioma que Habla", size(medlarge)ring(5)) note("Fuente: INE -  EH2021") legend(size(vsmall))

* Transformación a variable categórica reducida
recode s01a_07_1 2/5=1 6=2 7/33 =1 40/58=3 995/996=4, gen(ipioc)

label define label_ipioc 1 "impioc" 2 "castellano" 3 "idioma extranjero" 4 "no habla"
label value ipioc label_ipioc
tab ipioc [iweight=factor]

********************************************************************************
* DISTRIBUCIÓN TERRITORIAL
********************************************************************************
* 1. DEPARTAMENTO
codebook depto
tab depto [iweight= factor]
graph pie [pweight = factor], over(depto) plabel(_all percent, format(%9.1f)) title("Histograma Variable Departamento") caption(Fuente: INE - EH2021)

recode depto (1/6=0) (8/9=0) (7=1), gen(scz)
label define label_depto1 0 "Otros deptos" 1 "SCZ"
label value scz label_depto1

tab scz [iweight=factor]

* 2. AREA
codebook area
tab area [iweight=factor]

graph pie [pweight = factor], over(area) pie(1, explode) plabel(_all percent, format(%9.1f)) title("Histograma Variable Área") caption(Fuente: INE - EH2021)

recode area (1=1) (2=0), gen (area1)

label define label_area1 0 "Rural" 1 "Urbana"
label value area1 label_area1
tab area1 [iweight=factor]
* label drop label_area1

********************************************************************************
* CARACTERÍSTICAS DE EDUCACIÓN
********************************************************************************

* 1. ANALFABETISMO: Sabe leer y escribir: Permite indagar si las personas saben leer y escribir. (Universo: Personas de 4 años o más de edad)
codebook s03a_01
* numeric byte
tab s03a_01 [iweight=factor]
* 90,6% de la población mayor a 4 años sabe leer y escribir

graph pie [pweight = factor], over(s03a_01) plabel(_all percent, format(%9.1f)) title("Gráfico de Torta: Sabe leer y escribir") caption(Fuente: INE - EH2021)

tab s03a_01 depto [iweight=factor], row column

recode s03a_01 (2=0) (1=1), gen (alfab)
label define label_alfab 0 "No" 1 "Si"
label value alfab label_alfab

tab alfab [iweight=factor]

* Sabe sumar o multiplicar
* ¿Es (…) capaz de  sumar o multiplicar números, ya sea en papel o mentalmente
codebook s03a_01a
tab edad s03a_01a [iweight=factor], row column
table ( depto ) ( s03a_01a ) () [iweight=factor]
table ( edad ) ( s03a_01a ) () [iweight=factor]

graph pie [pweight = factor], over(s03a_01) plabel(_all percent, format(%9.1f)) title("Gráfico de Torta: Sabe sumar o multiplicar") caption(Fuente: INE - EH2021)

* 2. AÑOS DE ESTUDIO: Años de estudio de la población (universo: Personas de 4 años o más de edad)
codebook aestudio
* numeric byte
sum aestudio [weight=factor], detail
* el promedio de años de estudio en Bolivia es de 8,34 años.
* la mediana de años de estudio en Bolivia es de 8 años
* el mínimo de años de estudio en Bolivia es 0 años
* el máximo de años de estudio en Bolivia es 23 años
tab aestudio [iweight=factor]

table () () () [iweight = factor], statistic(mean aestudio) statistic(median aestudio) statistic(min aestudio) statistic(max aestudio) nformat(%9.2fc)

recode aestudio (0=1) (1/12=2) (13/17=3) (18/19=4) (20/23=5), gen (aest_nivel)
label define label_aest_nivel 1 "Pre-escolar" 2 "Bachillerato" 3 "Licenciatura" 4 "Maestría" 5 "Doctorado"
label value aest_nivel label_aest_nivel

* Transformar la variable aestudio por un comportamiento no lineal (cuadrático)
gen aestudio2= aestudio^2

tab aest_nivel [iweight=factor]
* 67,81% de la población tiene años de estudio como para contar sólo con bachillerato.
* 20.11% tiene los años de estudio suficientes como para contar con licenciatura

* 3. NIVEL DE ESTUDIO: ¿Cuál fue el NIVEL Y CURSO más alto de instrucción que aprobó? Nivel (s03a_02a)
codebook s03a_02a
tab s03a_02a [iweight=factor]
* Nivel educativo general (niv_ed_g)
codebook s03a_02a
tab s03a_02a [iweight=factor]

********************************************************************************
* CARACTERÍSTICAS ECONÓMICAS
********************************************************************************

* 1. EMPLEO
* Durante la semana pasada, ¿trabajó al menos una hora? (s04a_01)
codebook s04a_01 //Universo: Personas de 7 años o más de edad
tab s04a_01 [iweight=factor] // Están representadas 5.230.582 personas que es un número menor a la población ocupada.

recode s04a_01 (1=1) (2=0), gen (trabaja)
label define  label_trabaja 0 "no trabaja" 1 "trabaja"
label value trabaja label_trabaja

tab trabaja [iweight=factor] // del universo de 10,39 millones de personas trabajan 5,23 y no trabajan 5,16 millones.

* En esta ocupación usted trabaja como: (s04b_12)
codebook s04b_12 // Universo: Personas de 7 años o más de edad - 19.419 observaciones
* Se representan a 5.630.696 personas.
tab s04b_12 [iweight=factor] // el 32,29% es obrero/empleado; el 44,72% es trabajador por cuenta propia y 17,28% trabajador familiar sin remuneración

table (s04b_12) () (), statistic(frequency) statistic(percent) nformat(%9.2fc percent) 

recode s04b_12 (1=1) (5=1) (2=2) (4=2) (6/8=3) (3=4), gen(oficio)
label define  label_oficio 1 "obrero/empleado/Cooperativista" 2 "Empleador o socio" 3 "trabajador no remunerado" 4 "trabajador por cuenta propia"
label value oficio label_oficio

* La administración de la empresa, institución, negocio o lugar donde trabaja es... (s04b_13): 1 ¿Administración Pública?; 2.¿Empresa Pública (estratégica)?;  3. ¿Privada (Empresa mediana o grande)?; 4. ¿Privada (Negocio familiar, micro o pequeña empresa)?; 5.ONG (Organización no Gubernamental) y otras sin fines de lucro 6.Organismos internacionales, embajadas
codebook s04b_13 // Universo: Personas de 7 años o más de edad
tab s04b_13 [iweight=factor] // El 25,3% trabaja en la adminitración pública; el 22,53% en una empresa privada mediana o grande; y el 50,05% en una empresa privada (negocio familiar, micro o pequeña empresa)
table (s04b_13) () (), statistic(frequency) statistic(percent) nformat(%9.2fc percent) 

recode s04b_13 (1/2=1) (3/6=0), gen (adm_pub)
label define label_adm_pub 0 "Otros" 1 "Adm_Pub"
label value adm_pub label_adm_pub

tab adm_pub [iweight=factor]
* El 26,16% de las 1,83 millones de personas que contestaron la pregunta trabajan en el sector pública y empresas estratégicas.
graph pie, over(adm_pub) pie(1, explode) plabel(_all percent, format(%9.1f)) title("Gráfico Variable Administración Pública") note("Fuente: INE -  EH2021")

* La administración de la empresa, institución, negocio o lugar donde trabaja en esta otra ocupación es... (s04e_28)
codebook s04e_28 // Universo: Personas de 7 años o más de edad
tab s04e_28 [iweight=factor] 

recode s04e_28 (3/4=1) (1=0) (5=0), gen (emp_priv)
label define label_emp_priv 0 "Otros" 1 "Emp_Priv"
label value emp_priv label_emp_priv

tab emp_priv [iweight=factor]

* Poblacion en edad de trabajar 14 o más años (PET): Personas de 14 años o más con capacidad de realizar actividades dirigidas a la producción de bienes y servicios.
codebook pet // Universo: Personas de 7 años o más de edad: 36984 observaciones
tab pea [iweight=factor]

* Poblacion Ocupada PO (ocupado): Personas que declararon trabajar al menos una hora en la semana de referencia en una actividad económica (remunerada o no).
codebook ocupado // Missing .: 5,196/42,090 - Universo: Personas de 7 años o más de edad
tab ocupado [iweight=factor] // 36894 observaciones - 19419 personas respondieron que están ocupadas
* 5,63 millones de personas estuvieron ocupadas en Bolivia en la gestión 2021

* Poblacion Desocupada Cesante (cesante): Personas que no trabajaron en la semana de referencia, pero son buscadores activos y trabajaron alguna vez anteriormente.
codebook cesante // Missing .: 5,196/42,090 Universo: Personas de 7 años o más de edad: 36984 observaciones
tab cesante [iweight=factor] // 261531 personas se encontraban cesantes el 2021

* Poblacion Desocupada Aspirante (aspirante): Personas que no trabajaron en la semana de referencia, pero son buscadores activos y no trabajaron anteriormente.
codebook aspirante // Missing .: 5,196/42,090 Universo: Personas de 7 años o más de edad: 36984 observaciones
tab aspirante [iweight=factor] // 149  personas contestaro que no trabajaron pero son buscadores activos y trabajaron alguna vez anteriormente.
* 38497 personas fueron aspirantes en 2021

* Poblacion Desocupada (desocupado): Personas que no trabajaron en la semana de referencia, pero son buscadores activos de un empleo en las últimas 4 semanas.
codebook desocupado // Missing .: 5,196/42,090 Universo: Personas de 7 años o más de edad: 36984 
tab desocupado [iweight=factor] // 1166 personas contestaron encontrarse desocupadas
* 300028  personas se encontraban desocupadas en 2021.

* Poblacion Activa (pea): Fuerza de trabajo compuesta por las personas ocupadas y desocupadas.
codebook pea // Universo: Personas de 7 años o más de edad: 36984 observaciones
tab pea [iweight=factor] // 20585 personas se constituían en la fuerza de trabajo ocupada y desocupada.
* Aplicando el factor de expansión podemos inferir que 5,93 millones de personas eran la PEA en 2021.

* Población  Inactiva Temporal (temporal): Personas que no pertenecen a la fuerza de trabajo o son económicamente inactivas, que cumplen las condiciones para trabajar pero no se incorporan a la fuerza laboral, pero trabajaron alguna vez anteriormente.
codebook temporal // Universo: Personas de 7 años o más de edad: 36984 observaciones
tab temporal [iweight=factor] // 1,45 millones de personas son económicamente inactivas temporalmente

* Poblacion Inactiva Permanente (permanente): Personas que no pertenecen a la fuerza de trabajo o son económicamente inactivas, que cumplen las condiciones para trabajar pero no se incorporan a la fuerza laboral, y no trabajaron alguna vez anteriormente.
codebook permanente // Universo: Personas de 7 años o más de edad: 36984 observaciones
tab permanente [iweight=factor] // 11022 personas fueron económicamente inactivas permanentemente
* 25.872 personas son económicamente activas permanentemente por opuestos.
* 3,007,630 son económicamente activas de manera permanente

* Poblacion Inactiva (pei): Personas que no pertenecen a la fuerza de trabajo o están económicamente inactivas, por ser estudiantes, amas de casa, jubilados, etc.
codebook pei // Missing .: 5,196/42,090 - Universo: Personas de 7 años o más de edad: 36984 observaciones
tab pei [iweight=factor] // 16,309 no pertenecían a la fuerza de trabajo.
* 4,459,922 personas eran la pei en Bolivia

* Condicion de Actividad Ocupacion Principal (condact): Situación que distingue a la población en edad de trabajar de acuerdo a su participación en el mercado de trabajo.
codebook condact // Missing .: 5,196/42,090 - Universo: Personas de 7 años o más de edad: 36984 observaciones
tab condact [iweight=factor] // Nro de observaciones: 36894: 19419 ocupado; cesante 1017; 149 aspirante; temporal 5287; permanente 11022

* Horas trabajadas a la semana (tothrs): Horas de trabajo promedio en la semana que empleó el trabajador para desarrollar su ocupación principal y secundaria.
codebook tothrs //  Missing .: 23,266/42,090 - Universo: Personas de 7 años o más de edad: 36984 observaciones
sum tothrs [iweight=factor] // el promedio de horas totales trabajadas a la semana en Bolivia es de 43,01hrs.

* Grupo Ocupacional ocupación principal (cob_op): Clasificación ocupacional a nivel de gran grupo que permite identificar el tipo de trabajo o tarea específica que desarrollan los miembros del hogar ocupados en su ocupación principal.
codebook cob_op // Missing .: 22,671/42,090
tab cob_op [iweight=factor] // 19419 observaciones
* 24,48% pertenecen al grupo ocupacional de trabajadores en agricultura, pecuaria, pesca y otros
* 20,77% pertenecen al grupo ocupacional de trabajadores de servicio y vendedores
* 17,49% pertenecen al grupo ocupacional de trabajadores de la construcción, ind. manufacturera y otros

* Creación de variable años de experiencia: edad menos 4 años (edad preescolar) y se restringe a personas mayores de 14 años que es la edad acordada para empezar a trabajar según la OIT (Convenio 138)
	gen aexperiencia= s01a_03-aestudio-4 if s01a_03>=14 // se identificaron personas con valores negativos al realizar la corrección de aestudio-4 y la restricción de edad >= 14.
	replace aexperiencia = . if aexperiencia<0 // se restringe también la experiencia menor a 0
	replace aexperiencia = . if aexperiencia>70 // se restringe también la experiencia mayor a 70 por la edad de jubilación.
	sum aexperiencia s01a_03
	graph box aexperiencia
	
* Por teoría se genera la variable aexperiencia al cuadrado, porque el comportamiento de esta variable es cuadrática.
gen aexperiencia2 = aexperiencia^2
hist aexperiencia2

********************************************************************************
* ANÁLISIS ESTADÍSTICO:
********************************************************************************
* Análisis Gráfico:
* Ingresos Laborales
graph box ylab, over(s01a_02) over(area) over(npioc) title("Ingreso Laboral por sexo, área y pertenencia npioc") note("Fuente: INE - EH2021")

graph box ylab, over(s04b_13, label(angle(forty_five)labsize(vsmall))) title("Ingreso Laboral por tipo de Administración de la Empresa") note("Fuente: INE - EH2021")

graph box ylab, over(s04e_28, label(labsize(vsmall))) title("Ingreso Laboral por sector laboral") note("Fuente: INE - EH2021")

********************************************************************************
* REGRESIONES:
********************************************************************************

* Simple:
* Regresión lineal simple con años de estudio
reg ylab aestudio
* La significancia de la variable años de estudio es indiscutible para explicar el ingreso laboral
* Un año adicional de estudio incrementa en promedio Bs.188,29 el ingreso laboral.
* R2: es 0,1315 lo que implica que el modelo explica un 13,2% la variabilidad del ingreso laboral.

* PRUEBAS POST ESTIMACION
* 1. NORMALIDAD DE LOS RESIDUOS
* Leptocúrtica: Una distribución leptocúrtica tiene una curtosis positiva. Esto significa que tiene colas más pesadas y un pico más pronunciado en comparación con la distribución normal. En una distribución leptocúrtica, los valores de los datos están más concentrados alrededor de la media y hay menos valores atípicos. Esto indica una mayor concentración de datos en un rango estrecho.
* Mesocúrtica: Una distribución mesocúrtica tiene una curtosis cercana a cero. En este caso, la forma de la distribución es similar a la de una distribución normal. Tiene colas y picos moderados, lo que indica una dispersión y concentración de datos que se ajusta a lo esperado en una distribución típica.
* Platicúrtica: Una distribución platicúrtica tiene una curtosis negativa. Esto significa que tiene colas más ligeras y un pico más suave en comparación con la distribución normal. En una distribución platicúrtica, los valores de los datos están menos concentrados alrededor de la media y hay más valores atípicos. Esto indica una dispersión más amplia de datos y una mayor probabilidad de encontrar valores extremos.
* Predecimos los residuos
predict res_min, resid
* Graficamos los residuos
hist res_min, normal // el gráfico muestra que no existe distribución normal de los residuos
kdensity res_min, normal // lo mismo ocurre con la segunda prueba gráfica.
pnorm res_min // la tercera prueba gráfica confirma las dos anteriores.

twoway ((scatter ylab aestudio)(lfit ylab aestudio)) // la nube de dispersión de puntos observados muestra un comportamiento no lineal.

* Pruebas: La distribución normal por ser simétrica tiene un coeficiente de asimetría igual a 0 y un coeficiente de curtósis igual a 3.

* Podemos hacer el test de Shapiro-Wilk para ver si la distribucion sigue un comportamiento normal
swilk res_min // pvalue<0.05 por lo tanto RHo y decimos que los residuos no tienen distribución normal.
* Note: The normal approximation to the sampling distribution of W' is valid for 4<=n<=2000.
* Si rechazamos  Ho, es decir si no concluimos que la distribución sea normal, no deberíamos usar un test paramétrico.
* Las pruebas paramétricas son una herramienta estadística que se utiliza para el análisis de los factores de la población.

* JARQUE-BERA: sktest presents a test for normality based on skewness and another based on kurtosis and then combines the two tests into an overall test statistic.
jb res_min // pvalue es <0.05 por tanto los residuos no son normales.

sktest res_min // pvalue es <0.05 por tanto los residuos no son normales.

* si 5 ≤ n ≤ 5000 observaciones
sfrancia res_min // pvalue es <0.05 por tanto los residuos no son normales.

* 3. HETEROCEDASTICIDAD
* hettest:  performs three versions of the Breusch-Pagan (1979) and Cook-Weisberg (1983) test for heteroskedasticity. In the normal version, performed by default, the null hypothesis also includes the assumption that the regression disturbances are independent-normal draws with variance sigma^2.
*  iid causes estat hettest to compute the N*R2 version of the score test that drops the normality assumption.
hettest, iid

estat hettest // como p-value<0.05 entonces se RH0: constant variance

*Realizamos el test de White
estat imtest, white
* el p value muestra que existe suficiente evidencia para No RHo, lo que implica que no existe homocedasticidad
*Prueba visual
rvfplot, yline(0)

* Se observa que existen problemas en el modelo: de variable omitida y de heterocedasticidad. 
	
********************************************************************************	
* REGRESIONES MÚLTIPLES
********************************************************************************

* Modelo tradicional de Mincer
********************************************************************************	
* MODELO AJUSTADO MEDIANTE TRANSFORMACION FUNCIONAL DE LA VARIABLE DEPENDIENTE Y LA ADICION DE LA VARIABLE EXPERIENCIA RECOMENDADA POR TEORIA
* considerando que la regresión simple no pasa las pruebas post estimación, con eliminación de las observaciones atípicas de la variable endógena, incluimos la variable años de experiencia laboral que se recomienda en la teoría. Adicionalmente, se considera también el comportamiento cuadrático de esta variable, puesto que si bien el ingreso mejora con los años de experiencia, llega un momento en el que disminuye su efecto sobre el ingreso, hasta volverse negativo. 
reg lylab aestudio aexperiencia aexperiencia2

* Todas las variables propuestas son significativas
* Un año adicional de estudio genera en promedio 8% de incremento en el ingreso laboral, ceteris paribus el resto de las variables explicativas del modelo.
* Un año adicional de experiencia genera en promedio 4,9% de incremento en el ingreso laboral, ceteris paribus el resto de las variables explicativas del modelo.
* Cómo la experiencia tiene un comportamiento cuadrático, la variable años de experiencia al cuadrado genera en promedio una redución del 0,075% del ingreso laboral, ceteris paribus el resto de las variables explicativas.

* PRUEBAS POST ESTIMACION
* 1. NORMALIDAD DE LOS RESIDUOS
* Predecimos los residuos
predict res_mincer, resid
* Graficamos los residuos
hist res_mincer, normal // si bien mejora el ajuste de los residuos, gráficamente se ve todavía una curtósis mayor a la esperada y una leve asimetría hacia la derecha. 
* Por tanto pareciera ser una distribución leptocúrtica porque tiene una curtosis positiva. Esto significa que tiene colas más pesadas y un pico más pronunciado en comparación con la distribución normal. 

kdensity res_mincer, normal // La densidad estimada es diferente de la normal, especialmente en la curtosis.

pnorm res_mincer // La gráfica de los residuos debería sobreponerse a la recta y se observa que hay desviaciones en ambos extremos de la recta.

* Podemos hacer el test de Shapiro-Wilk para ver si la distribucion sigue un comportamiento normal
swilk res_mincer // pvalue<0.05 por lo tanto RHo y decimos que los residuos no tienen distribución normal.
* Note: The normal approximation to the sampling distribution of W' is valid for 4<=n<=2000.
* Si rechazamos  Ho, es decir si no concluimos que la distribución sea normal, no deberíamos usar un test paramétrico.

sktest res_mincer
* Las posibles causas de esto, podrían ser que existe un problema de variables omitidas, o que la transformación de la función de la variable dependiente debería ser difente.

* 2. MULTICOLINEALIDAD
estat vif //La variable de interés "años de estudio" no presenta multicolinealidad, sin embargo años de experiencia sí, pero se mantiene en el modelo porque tiene respaldo teórico.

* 3. HETEROCEDASTICIDAD
hettest // El modelo no pasa la prueba y se RHo: constant variance.
*Realizamos el test de White
estat imtest, white
* el p value muestra que existe suficiente evidencia para RHo: Homocedasticidad, lo que implica que no existe homocedasticidad
*Prueba visual
rvfplot, yline(0)

********************************************************************************
********************************************************************************
* Modelo logit de elección binaria
* Probabilidad pura
logit ocupado // log likelihood -25.521,732

logit ocupado aexperiencia // log likelihood -19.706,95
* la variable años de experiencia es significativa y el signo es positivo como se esperaba
/*Los coeficientes no se pueden interpretar directamente
				- Sí se puede ver la significancia
				- Sí se puede ver el signo 
				- Prob>chi2 =0.0000 muestra que el modelo es bueno (la variable incluida es acertada)
	Cuál es la probabilidad de estar ocupado si se cuenta con 10 años de experiencia*/
 
display "La probabilidad de estar ocupado es del " 1/(1+exp( -.167869 -  .0155352*10))*100 "%" // 58,01%

* Test de ratio de verosimilitud LR Test
display "/2 ln(L(Bu)/L(Br)) =" -2*ln(-19706.95/-25521.732)

logit ocupado aestudio // log likelihood -24.406,799
eststo Modelo1: margins, dydx(*)
/* la variable años de estudio es significativa y el signo es positivo como se esperaba
* Los coeficientes no se pueden interpretar directamente
				- Sí se puede ver la significancia
				- Sí se puede ver el signo 
Cuál es la probabilidad de estar ocupado si se cuenta con 10 años de estudio? 
Necesito calcular p = 1 / 1+e^-z, donde z= -0.7894612 + 0.0977463aestudio */
display "La probabilidad de estar ocupado es del "1/(1+exp(0.7894612 - 0.0977463*19))*100 "%" // más del 70% cuando tenemos 19 años de estudio (que equivale a un nivel de maestría)

********************************************************************************
* Modelo multinomial: 
probit ocupado s01a_03 aestudio aestudio2 aexperiencia aexperiencia2 i.sex i.npioc i.area1 i.scz
/* Los coeficientes no se pueden interpretar directamente, pero llama la atención la el signo negativo de años de estudio, de aexperiencia al cuadrado y que años de experiencia se omite en el modelo. Con referencia a los significancia y el signo del coeficiente:
 - Un año adicional de vida es significativo y el signo del coeficiente es positivo, como se espera. 
 - Un año adicional de estudio es significativo y el signo del coeficiente es negativo respecto a la probabilidad de estar ocupado. 
 - Un año adicional de estudio al cuadrado tiene signo positivo y es significativa
 - La variable sexo es significativa y su signo es negativo para la categoría mujer.
 - La variable npioc es significativa y su signo es negativo para la categoría de personas que se identifican con npioc.
 - La variable de distribución geográfica área es significativa y tiene signo negativo para la categoría urbana respecto a la probabilidad de estar ocupado.
 - La variable departamento para el caso de Santa Cruz es significativa y tiene signo positivo respecto al resto de los departamento cuando se trata de la probabilidad de esta ocupado. */ 

* 1. Efectos Marginales Promedio (AME) // dydx es la derivada para hallar la pendiente (*) significa para todas las variables
eststo Modelo2:	margins, dydx(*) // 
/* 	- Un año adicional de vida incrementa la probabilidad de estar ocupado en 2,99%
	- Un año adicional de estudio disminuye en 5,5% la probabilidad de estar ocupado
	- Un año adicional de estudio al cuadrado aumenta en 0,16% la probabilidad de estar ocupado
	- años de experiencia se omité.
	- Un año adicional de experiencia al cuadrado disminuye en -.04732% la probabilidad de estar ocupado
	- respecto al hombre, una mujer tiene 20,99% menos probabilidad de estar ocupada, en promedio para la base de datos.
	- Una que se identifica npioc tiene 8,5% menos probabilidad de estar ocupado respecto a una persona no npioc, en promedio para la base de datos.
	- Una persona que vive en el área urbana tiene -19,2% menos probabilidades de estar ocupado respecto a una que vive en el área rural.
	- Una persona que vive en el departamento de Santa Cruz tiene 2,1% más probabilidad de estar ocupado, que los que viven en el resto de los departamentos  */ 

* 	2. Efectos Marginales del individuo con características promedio (MEM - Marginal Effect at Mean)
margins, dydx(*) atmean

/* El individuo promedio representativo tiene:
 - 37,8 años
 - 10,5 años de estudio
 - 23,3 años de experiencia
 - La variable sexo, y otras categóricas no son fácilmente explicables
 
 - Un año adicional a los 37,8 años incrementa la probabilidad de estar ocupado en 3,7%.
 - Un año adicional de estudio a los 10,5 años, disminuye en 6,7% la probabilidad de estar ocupado.
 */

/* 	3. Efectos Marginales del Individuo con características medianas (MEM - Marginal Effects at Median) */
sum s01a_03 aestudio aestudio2 aexperiencia aexperiencia2 sex npioc area1 scz, detail
margins, dydx(*) at((median))
// - Se analiza respecto a las medianas de las diferentes variables.
// - Pasar de 27 a 28 años incrementa la probabilidad de estar ocupado en 2,9%
// - El hecho de ser mujer reduce la probabilidad de estar ocupado en -20,99%.
// - El hecho de no pertenecer a un npioc incrementa la probabilidad de estar ocupado en 8,5%.
// - Con la educación no existe una relación tan lineal, porque no es lo mismo pasar de un nivel de secundaria a licenciatura y luego a maestría o doctorado.

/* 	4. Efectos Marginales con características específicas
		(MER - Marginal Effects at a Reference Point) */
probit ocupado s01a_03 aestudio aestudio2 aexperiencia2 i.sex i.npioc i.area1 i.scz

eststo Modelo3: margins, dydx(s01a_03) at(s01a_03 = (14 50))  
/* funciona cuando se elimina la variable omitida aexperiencia 
- Se definen los dos puntos (at)
- A los 14 años un año más de vida incrementa las probabilidades de esta ocupado en 1,5%; en cambio a los 50 años sólo incrementa en 0,97%.
*/ 

* Para ver los modelos guardados
estout*, stats(r2_p aic bic N)
* Para exportar los resultados
esttab using res_logit_probit.rtf, label replace

margins, at(s01a_03 = (14 50))  // se obtiene la probabilidad de estar ocupado para alquien de 14 y para alguien de 50 años: En el primer caso la probabilidad es de 12,2% y en el segundo caso 82,3%
margins, dydx(s01a_03) at(s01a_03 = (7(5)75)) 
marginsplot // el comportamiento es cuadrático sube hasta los 27 años y dismuye posteriormente.


margins, at(aestudio = (12 19)) // extrañamente la probabilidad de estar ocupado con 12 años de estudio (53,4%) es superior al de 19 años de estudio (22,9%.) 
margins, dydx(aestudio) at(aestudio = (0(5)25)) 
	marginsplot // La tendencia no es lineal y cuando incrementan los años de estudio, inicialmente disminuye la probabilidad de estar ocupado, pero apartir de los 15 esta empieza a incrementarse.	

margins, at(npioc = (0 1)) // hay una mayor probabilidad de estar ocupado cuando la persona es npioc. (tiene una diferencia de 9% aproximadamente)
	marginsplot // El comportamiento de la variable es lineal y es ascendente para la categoría no npioc y descendente para la npioc. 

margins, at(s01a_03=(53) aestudio=(22) sex=(0) npioc=(0) area1=(1)) // la probabilidad de estar ocupado para una persona con las características: 53 años de edad, 22 años de estudio, hombre, que vive en el área urbana, no indígena orginiario campesino, es de 59,4%

* Para graficar realizamos diferentes análisis:
* 1. Edad y area
margins, at(s01a_03 = (7(5)75) area1=(0 1) sex=(0 1) npioc=(0 1)) 
marginsplot
* 2. Años de estudio y sexo
margins, at(aestudio = (0(5)25) sex=(0 1) area1=(0 1) npioc=(0 1)) 
marginsplot
margins, at(aestudio = (0(5)25) sex=(0 1)) 
marginsplot

margins, at (aestudio = (0(5)25) sex=(0 1)) 
marginsplot, recast (line) recastci(rarea) ciopt (fcolor(%10)) ///
title("Probabilidad de estar Ocupado") //para hacer más lindo el gráfico

margins, at (s01a_03 = (0(5)25) sex=(0 1)) 
marginsplot, recast (line) recastci(rarea) ciopt (fcolor(%10)) ///
title("Probabilidad de estar Ocupado") //para hacer más lindo el gráfico


* Modelo
probit ocupado s01a_03 aestudio aestudio2 aexperiencia2 i.sex i.npioc i.area1 i.scz

* Determinación de la CALIDAD DEL MODELO: Classified + if predicted Pr(D)>=0.5
estat classification // correctly classified 72,99% (de la muestra fue clasificado correctamente)
* La prueba muestra que el modelo tiene pronosticado/planificado que van ha estar ocupadas 21246 personas con más de 50% de probabilidad y con menos de 50% a 9.005.
* En la columna D observamos que estan ocupadas 18964 personas y no estan ocupadas 11287. (sin embargo, sum ocupado muestra que estan ocupados 19.119 personas y desocupadas 11716)
* En la diagonal de la tabla estan los valores que se predijeron correctamente. El modelo predijo 16019 ocupados y 6060 desocupados.

*_2 Márgenes: Interpretación para variable binaria explicativa
	margins r.sex // la probabilidad de estar ocupado en el caso de las mujeres disminuye en 21% con relación a los hombres
	
	margins r.npioc // la probabilidad de estar ocupado para las personas NPIOC, es mayor en 8.4% respecto a quienes no se consideran npioc.

*_3 Márgenes: Interpretación para una variable continua
	margins, at (aestudio= (12 20))
	
	* estimadores */
				matrix list r(b)
				matrix define m=r(b) // Se pide a la computadora que guarde los 
									 // resultados de la estimación en una matriz
				matrix list m 		 // Se guarda como una fila [fila,columna]
				di "La diferencia entre probabildades es: " m[1,1] - m[1,9] 	
									// Hace el cálculo entre los dos coeficientes
									// restando los elementos
		*De manera más interesante:
		margins, at(aestudio=(0(1)20)) 	// Puede tardar en calcularse, tabla grande
									// Mejor graficar
		marginsplot	
		
		*De forma más estética
		margins, at(aestudio=(0(1)20)) 	
		marginsplot, recast(line) recastci(rarea) ciopt(fcolor(%10)) ///
			title("Probabilidad según anos de escolaridad") ///
			legend(order(1 "No Ocupado" 2 "Ocupado"))
			
*_4 Relative Risk Ratios
tab s04b_12 [iweight=factor]
tab oficio [iweight=factor]

***************************************
*Modelo logit multinomial
mlogit  niv_ed_g i.sex i.npioc, rrr
	
* Para cambiar la categoría base
		mlogit  niv_ed_g i.sex i.npioc, rrr base(1)	

*_5 Prueba para verificar IAI (Independencia de Alternativas Irrelevantes)
mlogit  oficio aestudio i.sex i.npioc
	eststo Modelo_completo // guardar los coeficientes sin márgenes
	esttab Modelo_completo
//Si quitamos la alternativa trabajador no remunerado (3):
		mlogit oficio aestudio i.sex i.npioc if oficio!=3 	
	eststo Modelo_reducido // guardar los coeficientes sin márgenes
	esttab Modelo_reducido
	
	*Comparación entre coeficientes: Prueba de Hausman
		hausman Modelo_completo Modelo_reducido, alleqs 
		// alleqs que se incluyan todos los coeficientes
								// trabajador por cuenta propia es el coeficiente de base.
								// trabajador no remunerado está excluido en el modelo reducido.
								// No se puede encontrar el error estándar cuando el
								// estadístico es negativo (-81.18) 
								// Si hay ese problema
								// se soluciona cambiando el orden de los 

								
********************************************************************************
* Modelo Ordenado: 

codebook niv_ed_g // Cambiamos la variable dependiente por el nivel de educacion, donde teniamos un conflicto con la categoria otros. 
codebook ipioc // Que idioma habla

oprobit niv_ed_g s01a_03 i.sex i.ipioc  i.area1 i.scz
estimates store oprobit_1
margins, dydx(*) 

// No reconfiguramos ninguna categoria

oprobit niv_ed_g s01a_03 i.sex i.ipioc  i.area1 i.scz if niv_ed_g!=4 
estimates store oprobit_2 

// Reconfiguramos la distribucion de la categoria otros, ya que la cateogoria 4.otros no entra en la secuencia y orden e nivel de estudio general 
margins, dydx(*) 

oprobit niv_ed_g s01a_03 i.sex i.ipioc  i.area1 i.scz if niv_ed_g!=4 & ipioc!= 4 
estimates store oprobit_3 
// Reconfiguramos la categoria no habla en npioc
margins, dydx(*) 

esttab oprobit_1 oprobit_2 oprobit_3, se mtitles ar2 pr2 aic bic
//Realizamos una comparacion de modelos donde nos basamos en R2, pseudo r2 AIC BIC para escoger el mejor. Observando los loglikelihood tambien vemos que el menor es el del modelo 3

								
********************************************************************************
*Modelo tobit lnylab
	hist ylab 
	// Fuerte concentración alrededor del 0, el trabajo remunerado

*** Comparacion de modelo latente, condicionado y predictivo 
gen lnylab=ln(ylab)
tobit lnylab aestudio aexperiencia aexperiencia2, ll(7.6797136)
margins, dydx(*) at(aestudio=19 aexperiencia=10) predict(ystar(7.6797136,.)) 

margins, predict(ystar(.,.)) at (aestudio=(1(6)25))


* Comparacion Modelo lantente, no condicionado y condicionado/ Predictivo
margins, predict(e(7.6797136,.)) predict(ystar(7.6797136,.)) predict(ystar(.,.))  ///
				 at (aestudio=(1(1)25)) 
		marginsplot, yline(0) recast(line) noci legend(order (1 "Modelo cond." 2  "Modelo no cond" 3 "Modelo latente")) ///
					title("Modelos Tobit Logaritmo del Ingreso Laboral")


*** Comparacion por separado 
		estimates clear
		eststo Reg_lin:	reg lnylab aestudio aexperiencia aexperiencia2
		eststo Reg_lin_sin:	reg lnylab aestudio aexperiencia aexperiencia2 if (lnylab>7.6797136) // solo para los que ganan mas del minimo
		eststo Tobit:	tobit lnylab aestudio aexperiencia aexperiencia2, ll(7.6797136) 
		esttab
		
*Interpretamos el modelo predictivo: 
