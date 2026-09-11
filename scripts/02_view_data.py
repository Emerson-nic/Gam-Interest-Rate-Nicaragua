'''
python3 -m venv env
source env/bin/activate
pip install -r requirements.txt
'''

import pandas as pd
from ydata_profiling import ProfileReport

# %% load non stationary csv

#load csv
banca = pd.read_csv("csv/sistema_bancario.csv")
banca['fecha'] = pd.to_datetime(banca['fecha'], format='%Y-%m-%d')
print(banca['fecha'].head()) 

banca['tasa_interes_activo'] = banca['tasa_interes_activo'].astype(float)
banca['ratio_morosidad_pct'] = banca['ratio_morosidad_pct'].astype(float)
banca['ratio_liquidez_pct'] = banca['ratio_liquidez_pct'].astype(float)
banca['d_ln_imae'] = banca['d_ln_imae'].astype(float)
banca['d_ln_ipc'] = banca['d_ln_ipc'].astype(float)
banca['tasa_implicita_pct_clean'] = banca['tasa_implicita_pct_clean'].astype(float)

banca.head()
print(banca.info())
print(banca.columns.tolist())

#automatic config
perfil = ProfileReport(banca, title="Sistema Bancario de Nicaragua")

#save results
perfil.to_file("html/reporte_bancario.html")

# %% load stationary csv

#load csv
banca_1 = pd.read_csv("csv/banca_nicaragua_estacionarios.csv")
print(banca_1.columns.tolist())
banca_1['fecha'] = pd.to_datetime(banca_1['fecha'], format='%Y-%m-%d')
print(banca_1['fecha'].head()) 

print(banca.columns.tolist())

banca_1['tasa_interes_activo'] = banca_1['tasa_interes_activo'].astype(float)
banca_1['ratio_morosidad_pct'] = banca_1['ratio_morosidad_pct'].astype(float)
banca_1['ratio_liquidez_pct'] = banca_1['ratio_liquidez_pct'].astype(float)
banca_1['d_ln_imae'] = banca_1['d_ln_imae'].astype(float)
banca_1['d_ln_ipc'] = banca_1['d_ln_ipc'].astype(float)
banca_1['tasa_implicita_pct_clean'] = banca_1['tasa_implicita_pct_clean'].astype(float)

banca_1.head()
print(banca_1.info())
print(banca_1.columns.tolist())

#automatic config
perfil = ProfileReport(banca_1, title="Sistema Bancario de Nicaragua")

#save results
perfil.to_file("html/reporte_bancario_estacionarios.html")