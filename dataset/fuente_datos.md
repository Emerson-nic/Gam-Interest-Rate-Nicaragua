## Variables

1.  Tasa de interés activa nominal en MN

2.  $\text{Ratio de morosidad}_{it}$: $\frac{\text{Vencidos}_{it} + \text{Cobro Judicial}_{it}}{\text{Cartera de Créditos Neta}_{it} + \text{Provisión de Cartera}_{it}}$

3.  $\text{Crecimiento IMAE}_t$: $\ln(\text{IMAE}_t) - \ln(\text{IMAE}_{t-12})$

4.  $\text{Crecimiento ITCER}_t$: $\ln(\text{ITCER}_t) - \ln(\text{ITCER}_{t-12})$

5.  $\text{Inflación}_t$: $\ln(\text{IPC}_t) - \ln(\text{IPC}_{t-12})$

6.  $\text{Liquidez}_{it}$ : $\frac{\text{Efectivo y Equivalentes de Efectivo}_{it}}{\text{Obligaciones con el Público (Depósitos)}_{it}}$

7.  Tiempo: actúa como una tendencia determinística no lineal flexible

Nota: subíndice $i$ representa a la entidad Bancaria, en cuanto el subíndice $t$ representa el tiempo

## Base de datos

Tasa de interés activa nominal en MN de [SECMCA](https://www.secmca.org/chart/?parent=Tasas+de+inter%C3%A9s+y+encaje&son=Tasas+de+inter%C3%A9s+en+moneda+nacional&url=51%2FN%2FNIC%2F166%2F24-23%2FPT%2FM%2F199601-202606&all_vars=1%7CTasa+de+inter%C3%A9s+activa+nominal+en+MN&cid=8)

- Datos no ajustados estacionalmente ni ajustados por calendario

- MN: Moneda Nacional

IMAE obtenido de [SECMCA](https://www.secmca.org/chart/?parent=Producci%C3%B3n&son=%C3%8Dndice+Mensual+de+la+Actividad+Econ%C3%B3mica&url=11%2FN%2FNIC%2F81%2F265%2FIX-PT%2FM%2F200601-202606&all_vars=1%7CIMAE+&cid=1)

ITCER obtenido de [SECMCA](https://www.secmca.org/chart/?parent=Tipos+de+cambio&son=%C3%8Dndice+tipo+de+cambio+efectivo+real&url=30%2FN%2FNIC%2F130%2F265%2FIX-PT%2FM%2F200001-202606&all_vars=1%7CITCER+con+USA&cid=3)

IPC general obtenido de [SEMCA](https://www.secmca.org/chart/?parent=Precios&son=%C3%8Dndice+de+precios+al+consumidor&url=1%2FN%2FNIC%2F78%2F265%2FIX-PT%2FM%2F200601-202607&all_vars=1%7CIPC+general)

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
