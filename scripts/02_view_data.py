'''
python3 -m venv env
source env/bin/activate
pip install -r requirements.txt
'''

import pandas as pd
from ydata_profiling import ProfileReport

#load csv
banca = pd.read_csv("csv/sitema_bancario.csv")

#automatic config
perfil = ProfileReport(banca, title="Sistema Bancario de Nicaragua")

#save results
perfil.to_file("html/reporte_bancario.html")