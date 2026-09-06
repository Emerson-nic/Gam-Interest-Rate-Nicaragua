'''
python3 -m venv env
source env/bin/activate
pip install -r requirements.txt
'''

import pandas as pd
from ydata_profiling import ProfileReport

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
