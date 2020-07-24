Reportes automáticos Contaminación Atmosférica Chile
================
Consiste en un proyecto de ejemplo de como usar *R Markdown* a través de un caso práctico de análisis de la contaminación atmosférica en Chile

El proyecto tiene tres grandes sub proyectos:
* Descarga de datos de estaciones de monitoreo en el SINCA (Sistema de Información Nacional de Calidad del Aire) a través de Web Scraping (https://sinca.mma.gob.cl/)
* Descarga de datos de registros de contaminación o meteorología del SINCA, a partir de las estaciones de monitoreo
* Generación de un reporte automatizado con análisis de los datos de calidad del aire recolectados
	
El reporte se puede generar con los siguientes parámetros:
* Zona geográfica: Región, Provincia, Comunas o Estaciones de interés
* Contaminante
* Periodo temporal de análisis

**Notas:**
* En el Excel "Resumen_DatosEstacioneSINCA.xlsx" se pueden visualizar mediante tablas dinámicas los datos relativos  a las estaciones SINCA,para poder realizar mejor la descarga y reporte de los datos de calidad del aire

**Ejemplo de uso para generar reportes:**

	rmarkdown::render("Reporte_CalidadAire.Rmd",
			params = list(reg="M",
			estaciones=c("Las Condes", "Cerrillos","Puente Alto", "Pudahuel","La Florida", "Independencia"),
			descarga_datos=T,
			cont="mp2.5"),
		output_file = "Reportes/Reporte_Santiago")

Adicionalmente se incluyen análisis particulares a nivel nacional de los datos de contaminación de MP2.5, generando algunas visualizaciones interesantes.
