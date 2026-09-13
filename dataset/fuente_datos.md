## Variables

1.  Tasa de interés activa nominal en MN: $$\ln(\text{Tasa}_t) - \ln(\text{Tasa}_{t-1})$$

2.  $\text{Ratio de morosidad}$: $$\frac{\text{Vencidos} + \text{Cobro Judicial}}{\text{Cartera de Créditos Neta} + \text{Provisión de Cartera}}$$

- $\text{Ratio de morosidad}$: $$\text{Ratio de morosidad} - \text{Ratio de morosidad}_{t-1}$$

3.  $\text{Liquidez}$ : $$\frac{\text{Efectivo y Equivalentes de Efectivo}}{\text{Obligaciones con el Público (Depósitos)}}$$

- $\text{Ratio de morosidad}$: $$\text{Liquidez} - \text{Liquidez}_{t-1}$$

3.  $\text{Crecimiento IMAE}_t$: $$\ln(\text{IMAE}_t) - \ln(\text{IMAE}_{t-12})$$

4.  $\text{Crecimiento ITCER}_t$: $$\ln(\text{ITCER}_t) - \ln(\text{ITCER}_{t-12})$$

5.  $\text{Crecimiento precios promedio Cafe}_t$: $$\ln(\text{Cafe}_t) - \ln(\text{Cafe}_{t-1})$$

6.  $\text{Crecimiento precios promedio Banano}_t$: $$\ln(\text{Banano}_t) - \ln(\text{Banano}_{t-1})$$

7.  $\text{Crecimiento precios promedio Azucar}_t$: $$\ln(\text{Azucar}_t) - \ln(\text{Azucar}_{t-1})$$

8.  $\text{Inflación}_t$: $$\ln(\text{IPC}_t) - \ln(\text{IPC}_{t-12})$$

9.  Federal Funds Effective Rate (FEDFUNDS) o Tasa efectiva de fondos federales

10. Asegurados activos del INSS: $$\ln(\text{INSS}_t) - \ln(\text{INSS}_{t-12})$$

11. Tiempo: actúa como una tendencia determinística no lineal flexible

Nota: El precio promedio del azúcar se interpolo en términos logarítmicos para obtener valores mayores a 0, el método usado es un auto.arima

## Base de datos

Tasa de interés activa nominal en MN de [SECMCA](https://www.secmca.org/chart/?parent=Tasas+de+inter%C3%A9s+y+encaje&son=Tasas+de+inter%C3%A9s+en+moneda+nacional&url=51%2FN%2FNIC%2F166%2F24-23%2FPT%2FM%2F199601-202606&all_vars=1%7CTasa+de+inter%C3%A9s+activa+nominal+en+MN&cid=8)

- Datos no ajustados estacionalmente ni ajustados por calendario

- MN: Moneda Nacional

IMAE obtenido de [SECMCA](https://www.secmca.org/chart/?parent=Producci%C3%B3n&son=%C3%8Dndice+Mensual+de+la+Actividad+Econ%C3%B3mica&url=11%2FN%2FNIC%2F81%2F265%2FIX-PT%2FM%2F200601-202606&all_vars=1%7CIMAE+&cid=1)

ITCER obtenido de [SECMCA](https://www.secmca.org/chart/?parent=Tipos+de+cambio&son=%C3%8Dndice+tipo+de+cambio+efectivo+real&url=30%2FN%2FNIC%2F130%2F265%2FIX-PT%2FM%2F200001-202606&all_vars=1%7CITCER+con+USA&cid=3)

Cafe precio promedio en US\$/QQ (quintales) [SECMCA](https://www.secmca.org/chart/?parent=Comercio+exterior&son=Exportaci%C3%B3n+de+caf%C3%A9%2C+banano+y+az%C3%BAcar&url=21%2FN%2FNIC%2F121-542-541%2F266%2FUSD-QQ46-TN-KG-UV%2FM%2F200301-202603&all_vars=3%7CCaf%C3%A9+precio+promedio+de+exportaci%C3%B3n*6%7CBanano+precio+promedio+de+exportaci%C3%B3n*9%7CAz%C3%BAcar+precio+promedio+de+exportaci%C3%B3n&cid=5)

Banano precio promedio en US\$/T (toneladas) [SECMCA](https://www.secmca.org/chart/?parent=Comercio+exterior&son=Exportaci%C3%B3n+de+caf%C3%A9%2C+banano+y+az%C3%BAcar&url=21%2FN%2FNIC%2F120-125-123%2F266%2FUSD-QQ46-TN-KG-UV%2FM%2F200301-202603&all_vars=1%7CCaf%C3%A9+valor+exportado*4%7CBanano+valor+exportado*7%7CAz%C3%BAcar+valor+exportado&cid=5)

Azucar precio promedio en US\$/KG (kilogramos) [SECMCA](https://www.secmca.org/chart/?parent=Comercio+exterior&son=Exportaci%C3%B3n+de+caf%C3%A9%2C+banano+y+az%C3%BAcar&url=21%2FN%2FNIC%2F120-125-123%2F266%2FUSD-QQ46-TN-KG-UV%2FM%2F200301-202603&all_vars=1%7CCaf%C3%A9+valor+exportado*4%7CBanano+valor+exportado*7%7CAz%C3%BAcar+valor+exportado&cid=5)

IPC general obtenido de [SEMCA](https://www.secmca.org/chart/?parent=Precios&son=%C3%8Dndice+de+precios+al+consumidor&url=1%2FN%2FNIC%2F78%2F265%2FIX-PT%2FM%2F200601-202607&all_vars=1%7CIPC+general)

Federal Funds Effective Rate obtenido de [FRED](https://fred.stlouisfed.org/series/fedfunds)

Asegurados INSS obtenido de [BCN](https://www.bcn.gob.ni/mercado-laboral)

- Indicador Mensual: `Asegurados activos del INSS por actividad económica`

Series de Informe en Excel 2008 y 2019 [SIBOIF](https://www.siboif.gob.ni/consultas/informes?field_informes_value=1&field_categoria_informe_tid=1577)

A partir del 2019:

- Tipo de informe: [`Estado de Situación Financiera`](https://www.siboif.gob.ni/sites/default/files/documentos/serie-informes-excel/bancos/ib_balance_general_0.xlsx)
- Intendencia: `Bancos`
- Hoja del excel: `SISTEMA_BANCARIO`

Desde el 2008:

- Tipo de informe: [`Balance General`](https://www.siboif.gob.ni/sites/default/files/documentos/serie-informes-excel/bancos/ib_balance_general.xlsx)
- Intendencia: `Bancos`
- Hoja del excel: `SISTEMA_BANCARIO`

## Pregunta de la investivacion

Cómo un cambio mensual en la tasa afecta el cambio mensual en la morosidad?
