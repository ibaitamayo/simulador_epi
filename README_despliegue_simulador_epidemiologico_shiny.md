# Simulador epidemiológico Shiny — checklist de despliegue

## Archivos necesarios en tiempo de ejecución

Coloca estos archivos en la misma carpeta de despliegue que la app Shiny:

- `country_age_distribution_wpp2024_6groups.rds`
- `world_countries_simplified.rds`
- `fixed_covid_omicron_reference_age_adjusted_seird.rds`

Archivos opcionales heredados/de referencia:

- `fixed_covid_omicron_reference_sir.rds`
- `fixed_covid_omicron_reference_age_adjusted.rds`

## Comportamiento en ejecución

La app no genera ni escribe el comparador SEIRD en tiempo de ejecución. El comparador debe crearse fuera de la app alojada e incluirse en el entorno de despliegue.

## Valores principales por defecto

- El modo guiado usa SEIRD.
- Periodo expuesto por defecto: 4 días.
- Duración por defecto de la fase activa: 20 días.
- Ventana de contención por defecto en controles manuales: inicio día 210, final día 240.
- Intervalo de animación del mapa: 100 ms.

## Laboratorio de escenarios

El laboratorio de escenarios guarda escenarios durante la sesión de Shiny. Para reutilizarlos entre sesiones, exporta la configuración JSON e impórtala posteriormente.
