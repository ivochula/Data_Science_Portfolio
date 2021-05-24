# import the libraries----------------------------------------------------
import plotly.express as px
import plotly.graph_objs as go
import numpy as np
import pandas as pd
import re
from datetime import timedelta
import datetime
import base64
import dash
import dash_table
import dash_core_components as dcc
import dash_html_components as html
import dash_bootstrap_components as dbc
from dash.dependencies import Output, Input

# read, clean, and filter the data----------------------------------------
df = pd.read_excel('Dados_demo.xlsx')
df = df.fillna('Sem_valores')

time_now = datetime.datetime.now() # horario atual

# Limpar os dados dos pontos de calibração e de medição

for i in df.index:
	df.iloc[i,2] = re.sub("\,", "/", df.iloc[i,2])
	df.iloc[i,2] = re.sub("\ ", "", df.iloc[i,2])
	df.iloc[i,3] = re.sub("\,", "/", df.iloc[i,3])
	df.iloc[i,3] = re.sub("\ ", "", df.iloc[i,3])


# Função para separar os valores de uma String e converter para lista de valores Float

def separate(lista):
	x = re.split("\/", lista)
	x = [ float(x) for x in x ]

	return x

# Função para verificar se a calibração ainda está em dia

def validade(data, validade):
	calib_date = data # Data da calibração
	next_calib = int(validade * 365) # Valor do tempo para próxima calibração
	now = datetime.datetime.now() # horario atual
	out = calib_date + timedelta(days=next_calib) # soma à data da calibração a quantidade de dias para a próxima calibração

	return out

# Copia do Dataframe Original

dff = df.copy()

# Aplicar a função às colunas de indice 2 e 3

dff['Medição'] = list(map(separate, dff.iloc[:,2]))
dff['Pontos padrão'] = list(map(separate, dff.iloc[:,3])) 

# Aplicar funções de Datetime para saber a Validade dos certificados de calibração

df['Validade'] = list(map(validade, df.iloc[:,5], df.iloc[:,6])) # cria coluna com validade de cada certificado
df['Data Calibração'] = pd.DatetimeIndex(df['Data Calibração']).strftime("%Y-%m-%d") # converte para formato yyyy-mm-dd
df['Agora'] = datetime.datetime.now().strftime("%Y-%m-%d")
dff['Validade'] = pd.DatetimeIndex(df['Validade']).strftime("%Y-%m-%d")

# Criar nova coluna com a comparação em Booleano da validade
# TRUE - Fora da Validade

df['Validade'] = pd.DatetimeIndex(df['Validade'])
df['DateColumn_A'] = pd.to_datetime(df['Validade'])
df['DateColumn_B'] = pd.to_datetime(df['Agora'])
dff['Comparacao'] = df.DateColumn_A < df.DateColumn_B

# app Inicialização--------------------------------------------------------------

app = dash.Dash(__name__, suppress_callback_exceptions=True,
				external_stylesheets=[dbc.themes.SPACELAB])

# importar imagem do Logotipo

image_filename = 'Logo_tecnovip.png' 
encoded_image = base64.b64encode(open(image_filename, 'rb').read())

# Criação de Card informativo

card_certificado = dbc.Card(
	[
		dbc.CardHeader(html.H4("Certificado", className = "card-title"), 
								className = 'bg-primary text-white',),
		dbc.CardBody(
			[         
				dbc.ListGroup(
					[
						dbc.ListGroupItem(id='numero_cert'),
						dbc.ListGroupItem(id='sit_calib'), #id='sit_calib'
						dbc.ListGroupItem(id='prox_calib'),
					],
				),
			], className = 'bg-info',
		),
	],
)

card_total = dbc.Card(
	[
		dbc.CardHeader(html.H4("Relatorio Geral", className = "card-title"), className = 'bg-primary text-white',),
		dbc.CardBody(
			[         
				dbc.ListGroup(
					[
						dbc.ListGroupItem("Quantidade Total Certificados: {}".format(len(df['Certificado'].unique()))),
						dbc.ListGroupItem("Certificados com o prazo vencido: {}".format(dff['Comparacao'].sum())),
						dbc.ListGroupItem("Certificados fora dos critérios de aceitação: XXX"),
					],
				),
			], className = 'bg-info',
		),
	],
)

#app DataTable--------------------------------------------------------------
app.layout = dbc.Container([

	html.Img(src='data:image/png;base64,{}'.format(encoded_image.decode())),

	dbc.Row([
		dbc.Col(html.H1(html.H1('Dashboard - Certificados de Calibração'), 
						className="text-center font-weight-bold, mb-4"))
		]),
	
	dbc.Row([
		dbc.Col(	
			dash_table.DataTable(
		id='datatable-interactivity',
		columns=[{"name": i, "id": i} for i in df.iloc[:,:7].columns],
		data=df.to_dict('records'),
		editable=False,             # allow editing of data inside all cells
		filter_action="native",     # allow filtering of data by user ('native') or not ('none')
		sort_action="none",         # enables data to be sorted per-column by user or not ('none')
		sort_mode="single",         # sort across 'multi' or 'single' columns
		column_selectable="single", # allow users to select 'multi' or 'single' columns
		row_selectable="single",    # allow users to select 'multi' or 'single' rows
		row_deletable=False,        # choose if user can delete a row (True) or not (False)
		selected_columns=[],        # ids of columns that user selects
		selected_rows=[],           # indices of rows that user selects
		page_action="native",       # all data is passed to the table up-front or not ('none')
		page_current=0,             # page number that user is on
		page_size=6,                # number of rows visible per page
		style_cell={                # ensure adequate header width when text is shorter than cell's text
			'minWidth': 95, 'maxWidth': 95, 'width': 95
		},

		tooltip_delay=0,
		tooltip_duration=None,

		tooltip_header={
		'Certificado': 'Certificados de Calibração',
		'Parâmetros': 'Parâmetros Fisicos',
		'Medição': 'Medição efetuada pelo equipamento em calibração',
		'Pontos padrão': 'Medição efetuada pelo Padrão da Tecnovip',
		'Critério de Aceitação': 'Critério de Aceitação',
		'Data Calibração': 'Data da execução da calibração',
		'Duração Calibração': 'Validade da Calibração em anos',
		},

		style_cell_conditional=[    # align text columns to left. By default they are aligned to right
			{
				'if': {'column_id': c},
				'textAlign': 'left'
			} for c in ['Certificado', 'Parâmetros']
		],
		style_data={                # overflow cells' content into multiple lines
			'whiteSpace': 'normal',
			'height': 'auto'
		},
		style_data_conditional=[
		{
			'if': {'row_index': 'odd'},
			'backgroundColor': 'rgb(248, 248, 248)'
		},
		{
			'if': {
				'filter_query': '{Validade} < {Agora}', # comparing columns to each other
				'column_id': 'Data Calibração'
			},
			'backgroundColor': 'rgba(255, 0, 0, 0.34)',
			'fontWeight': 'bold'
		},
		],
		style_header={
		'backgroundColor': 'rgb(230, 230, 230)',
		'fontWeight': 'bold'
		}
	),
	),
	]),

	html.Br(),
	#html.Img(src=app.get_asset_url('Logo_tecnovip.jpg')),

	# app Grafico
	dbc.Row([
		dbc.Col([card_certificado, card_total], width = {'size':4}, style={'marginTop':'0rem'}),
		dbc.Col(dcc.Graph(id='grafico'),style = {"border": "4px solid rgba(0, 141, 255, 0.79)"}, width = {'size':8},
	),  
	]),

	html.Br(),
		
], fluid=True)

# Callback - app interactivity section------------------------------------

# Callback Grafico

@app.callback(
	[Output(component_id='grafico', component_property='figure'),
	Output(component_id='numero_cert', component_property='children'),
	Output(component_id='prox_calib', component_property='children'),
	Output(component_id='sit_calib', component_property='children')],
	Input(component_id='datatable-interactivity', component_property='selected_rows'))
	
def update_graph(selected_rows):

	if len(selected_rows) > 0:
		x = dff.iloc[selected_rows[0], 3]
		y = dff.iloc[selected_rows[0], 2]
		val = dff.iloc[selected_rows[0], 4]
		legenda = dff.iloc[selected_rows[0], 1]
		numero_cert = dff.iloc[selected_rows[0], 0]
		prox_calib = dff.iloc[selected_rows[0], 7]
		sit_calib = dff.iloc[selected_rows[0], 8]
	else:
		x = dff.iloc[0, 3]
		y = dff.iloc[0, 2]
		val = dff.iloc[0, 4]
		legenda = dff.iloc[0, 1]
		numero_cert = dff.iloc[0, 0]
		prox_calib = dff.iloc[0, 7]
		sit_calib = dff.iloc[0, 8]

	# Conferir a validade do certificado
	if sit_calib:
		sit_calib = 'Fora da Validade'
	else:
		sit_calib = 'Em dia'
	
	# Fazer linha do critério de aceitação
	x_rev = x[::-1]
	x_upper = [ i+val for i in x]
	x_lower =  [ i-val for i in x]
	x_lower = x_lower[::-1]

	fig = go.Figure()
	fig.add_trace(go.Scatter(x = x, y = x, name = 'Padrão Tecnovip', connectgaps=True))
	fig.add_trace(go.Scatter(x = x, y = y, name='Medição Equipamento', connectgaps=True))
	fig.update_layout(title={'text': "Comparação entre valores medidos na sonda e no padrão",
						'y':0.9,
						'x':0.5,
						})
	fig.update_xaxes(title_text= legenda)
	fig.update_layout(title_font_size=24)

	fig.add_trace(go.Scatter(x=x+x_rev, y=x_upper+x_lower,
							fill='toself',
							fillcolor='rgba(0,176,246,0.2)',
							line_color='rgba(255,255,255,0)',
							showlegend=False,
							name='Fair',))


	return fig, 'Identificação: {}'.format(numero_cert), 'Proxima Calibração: {}'.format(prox_calib), 'Situação do Certificado: {}'.format(sit_calib)

# Código para correr a aplicação na porta 8002

if __name__=='__main__':
	app.run_server(debug=True, port=8002)

# http://127.0.0.1:8002/



