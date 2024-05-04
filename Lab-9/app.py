import dash
import dash_bootstrap_components as dbc
from dash import html, dcc
from dash.dependencies import Input, Output
import yfinance as yf
from datetime import datetime, timedelta
import plotly.graph_objs as go

today = datetime.today()
start_date = (today - timedelta(days=365)).strftime('%Y-%m-%d')
end_date = today.strftime('%Y-%m-%d')

app = dash.Dash(__name__, external_stylesheets=[dbc.themes.BOOTSTRAP])

app.layout = dbc.Container([
    dbc.Row([
        dbc.Col([
            html.H1("Stock Tracking Dashboard", className="text-center mb-4"),
            html.Div([
                html.H4("Select Stock"),
                dcc.Dropdown(
                    id='stock-dropdown',
                    options=[{'label': each, 'value': each} for each in ['AAPL', 'GOOGL', 'MSFT']],
                    value='AAPL',
                    clearable=False,
                    className="mb-4"
                ),
            ]),
            html.Div([
                html.H4("Date Range"),
                dcc.DatePickerRange(
                    id='date-picker-range',
                    start_date=start_date,
                    end_date=end_date,
                    min_date_allowed=datetime(2020, 1, 1),
                    max_date_allowed=datetime.today(),
                    start_date_placeholder_text="Start Period",
                    end_date_placeholder_text="End Period",
                ),
            ], className="mb-4"),
            html.Div([
                html.H4("Additional Options"),
                dcc.Checklist(
                    id='plot-type-checkbox',
                    options=[
                        {'label': ' Include Candlestick', 'value': 'candlestick'},
                        {'label': ' Include Moving Averages', 'value': 'moving_average'},
                        {'label': ' Include Bollinger Bands', 'value': 'bollinger_bands'}
                    ],
                    value=[]
                ),
            ], className="mb-4"),
            dbc.Card([
                dbc.CardBody(id='stock-info')
            ], className="mb-4")
        ], md=4),
        dbc.Col([
            dcc.Graph(id='stock-graph'),
            dbc.Row([
                dbc.Col([
                    dcc.Graph(id='unique-plot', className="mb-4")
                ], md=6),
                dbc.Col([
                    dcc.Graph(id='stock-volume-graph', className="mb-4")
                ], md=6)
            ])
        ], md=8)
    ])
], fluid=True)

@app.callback(
    [Output('stock-info', 'children'),
     Output('stock-graph', 'figure'),
     Output('stock-volume-graph', 'figure'),
     Output('unique-plot', 'figure')],
    [Input('stock-dropdown', 'value'),
     Input('date-picker-range', 'start_date'),
     Input('date-picker-range', 'end_date'),
     Input('plot-type-checkbox', 'value')]
)
def update_stock_info(stock, start_date, end_date, plot_type):
    df = yf.download(stock, start=start_date, end=end_date)
    latest_data = df.iloc[-1]
    latest_date = latest_data.name.strftime('%Y-%m-%d')
    latest_close = latest_data['Close']
    latest_open = latest_data['Open']
    latest_high = latest_data['High']
    latest_low = latest_data['Low']
    latest_volume = latest_data['Volume']

    stock_info = [
        html.H5(f"{stock} Stock Info", className="card-title"),
        dbc.Row([
            dbc.Col([
                dbc.Card([
                    dbc.CardBody([
                        html.H6("Latest Date", className="card-subtitle"),
                        html.P(latest_date, className="card-text")
                    ])
                ], className="mb-3")
            ], md=4),
            dbc.Col([
                dbc.Card([
                    dbc.CardBody([
                        html.H6("Latest Close Price", className="card-subtitle"),
                        html.P(f"${latest_close:.2f}", className="card-text")
                    ])
                ], className="mb-3")
            ], md=4),
            dbc.Col([
                dbc.Card([
                    dbc.CardBody([
                        html.H6("Latest Open Price", className="card-subtitle"),
                        html.P(f"${latest_open:.2f}", className="card-text")
                    ])
                ], className="mb-3")
            ], md=4),
            dbc.Col([
                dbc.Card([
                    dbc.CardBody([
                        html.H6("Highest Price", className="card-subtitle"),
                        html.P(f"${latest_high:.2f}", className="card-text")
                    ])
                ], className="mb-3")
            ], md=4),
            dbc.Col([
                dbc.Card([
                    dbc.CardBody([
                        html.H6("Lowest Price", className="card-subtitle"),
                        html.P(f"${latest_low:.2f}", className="card-text")
                    ])
                ], className="mb-3")
            ], md=4),
            dbc.Col([
                dbc.Card([
                    dbc.CardBody([
                        html.H6("Volume", className="card-subtitle"),
                        html.P(f"{latest_volume:,}", className="card-text")
                    ])
                ], className="mb-3")
            ], md=4)
        ])
    ]

    plot_data = [{
        'x': df.index,
        'y': df['Close'],
        'type': 'line',
        'name': stock
    }]
    if 'candlestick' in plot_type:
        plot_data.append({
            'x': df.index,
            'open': df['Open'],
            'high': df['High'],
            'low': df['Low'],
            'close': df['Close'],
            'type': 'candlestick',
            'name': 'Candlestick'
        })

    if 'moving_average' in plot_type:
        df['50_MA'] = df['Close'].rolling(window=50).mean()
        df['200_MA'] = df['Close'].rolling(window=200).mean()

        plot_data.extend([
            {'x': df.index, 'y': df['50_MA'], 'type': 'line', 'name': '50-day MA'},
            {'x': df.index, 'y': df['200_MA'], 'type': 'line', 'name': '200-day MA'}
        ])

    if 'bollinger_bands' in plot_type:
        df['20_MA'] = df['Close'].rolling(window=20).mean()
        df['20_std'] = df['Close'].rolling(window=20).std()
        df['Upper_BB'] = df['20_MA'] + 2 * df['20_std']
        df['Lower_BB'] = df['20_MA'] - 2 * df['20_std']

        plot_data.extend([
            {'x': df.index, 'y': df['Upper_BB'], 'type': 'line', 'name': 'Upper Bollinger Band'},
            {'x': df.index, 'y': df['Lower_BB'], 'type': 'line', 'name': 'Lower Bollinger Band'}
        ])

    stock_graph = {
        'data': plot_data,
        'layout': {
            'title': f'{stock} Stock Price',
            'xaxis': {'title': 'Date'},
            'yaxis': {'title': 'Price'}
        }
    }

    volume_graph = {
        'data': [{
            'x': df.index,
            'y': df['Volume'],
            'type': 'bar',
            'name': 'Volume'
        }],
        'layout': {
            'title': f'{stock} Volume',
            'xaxis': {'title': 'Date'},
            'yaxis': {'title': 'Volume'}
        }
    }

    unique_plot = {
        'data': [
            go.Scatterpolar(
                r=[df['Close'].std(), df['Volume'].mean(), df['Close'].iloc[-1] - df['Close'].iloc[0]],
                theta=['Volatility', 'Liquidity', 'Momentum'],
                fill='toself',
                name='Metrics'
            )
        ],
        'layout': go.Layout(
            title=f'{stock} Metrics',
            polar=dict(
                radialaxis=dict(
                    visible=True,
                    range=[0, max(df['Close'].std(), df['Volume'].mean(), df['Close'].iloc[-1] - df['Close'].iloc[0])],
                    title='Value'
                )
            )
        )
    }

    return stock_info, stock_graph, volume_graph, unique_plot


if __name__ == '__main__':
    app.run_server(debug=True)
