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

for col in banca.columns:
    if col != 'fecha':
        banca[col] = banca[col].astype(float)

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
banca_1['Fecha'] = pd.to_datetime(banca_1['Fecha'], format='%Y-%m-%d')
print(banca_1['Fecha'].head()) 

print(banca_1.columns.tolist())

variables_dummy = ['crisis_2008', 'crisis_2018', 'crisis_covid']

for col in variables_dummy:
    banca_1[col] = banca_1[col].astype(int)

no_float = ['Fecha'] + variables_dummy

for col in banca_1.columns:
    if col not in no_float:
        banca_1[col] = banca_1[col].astype(float)
banca_1.head()
print(banca_1.info())
print(banca_1.columns.tolist())

#automatic config
perfil = ProfileReport(banca_1, title="Sistema Bancario de Nicaragua")

#save results
perfil.to_file("html/reporte_bancario_estacionarios.html")
