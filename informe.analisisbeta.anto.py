import yfinance as yf
import pandas as pd
import statsmodels.api as sm
import numpy as np

# Definir el ticker de la empresa y el índice de mercado
ticker_empresa = "ANTO.L"
ticker_mercado = "^GSPC" # S&P 500 como proxy del mercado

# Descargar datos para la empresa y el mercado en diferentes frecuencias
# Ajustar el periodo para asegurar suficientes datos (e.g., 5 años)
periodo = "5y"

data_diaria = yf.download([ticker_empresa, ticker_mercado], period=periodo, interval="1d")
data_semanal = yf.download([ticker_empresa, ticker_mercado], period=periodo, interval="1wk")
data_mensual = yf.download([ticker_empresa, ticker_mercado], period=periodo, interval="1mo")

print("Datos descargados para ANTO.L y ^GSPC en frecuencias diaria, semanal y mensual.")

def calculate_beta(asset_data, market_data, frequency_name):
    # Asegurarse de que tenemos los precios de cierre y renombrar columnas
    # YFinance descarga datos con un MultiIndex, necesitamos acceder al nivel 'Close'
    if isinstance(asset_data.columns, pd.MultiIndex):
        asset_prices = asset_data['Close'][ticker_empresa].dropna()
        market_prices = market_data['Close'][ticker_mercado].dropna()
    else:
        asset_prices = asset_data['Close'].dropna()
        market_prices = market_data['Close'].dropna()

    # Combinar los precios y alinear por fecha
    combined_prices = pd.DataFrame({
        'Asset': asset_prices,
        'Market': market_prices
    }).dropna()

    # Calcular retornos logarítmicos
    returns = np.log(combined_prices / combined_prices.shift(1)).dropna()

    if returns.empty or len(returns) < 2:
        print(f"No hay suficientes datos para calcular beta para {frequency_name}.")
        return {
            'Rentabilidad Promedio': np.nan,
            'Riesgo (Desviación Estándar)': np.nan,
            'Beta': np.nan
        }

    # Definir variables dependiente (y) e independiente (X) para la regresión
    y = returns['Asset']
    X = returns['Market']

    # Añadir una constante al modelo para la intercepción (alpha)
    X = sm.add_constant(X)

    # Realizar la regresión lineal
    model = sm.OLS(y, X)
    results = model.fit()

    # El coeficiente beta es el coeficiente de la variable 'Market'
    beta = results.params['Market']

    # Calcular rentabilidad promedio y riesgo
    rentabilidad_promedio = returns['Asset'].mean()
    riesgo_std = returns['Asset'].std()

    return {
        'Rentabilidad Promedio': rentabilidad_promedio,
        'Riesgo (Desviación Estándar)': riesgo_std,
        'Beta': beta
    }

# Calcular beta para cada frecuencia
results = {}

print("\n--- Calculando Beta y Estadísticas ---")
results['Diaria'] = calculate_beta(data_diaria, data_diaria, "Diaria")
results['Semanal'] = calculate_beta(data_semanal, data_semanal, "Semanal")
results['Mensual'] = calculate_beta(data_mensual, data_mensual, "Mensual")

# Convertir resultados a un DataFrame para una mejor visualización
df_results = pd.DataFrame.from_dict(results, orient='index')

print("\n--- Resumen de Rentabilidad, Riesgo y Beta ---")
print(df_results)

from fpdf import FPDF
import pandas as pd

# Asegurarse de que df_results esté disponible
# Si no lo está, ejecutar las celdas anteriores

class PDF(FPDF):
    def header(self):
        self.set_font('Arial', 'B', 12)
        self.cell(0, 10, 'Informe de Análisis de Inversión', 0, 1, 'C')

    def footer(self):
        self.set_y(-15)
        self.set_font('Arial', 'I', 8)
        self.cell(0, 10, f'Página {self.page_no()}', 0, 0, 'C')

    def chapter_title(self, title):
        self.set_font('Arial', 'B', 10)
        self.cell(0, 10, title, 0, 1, 'L')
        self.ln(5)

    def chapter_body(self, body):
        self.set_font('Arial', '', 10)
        self.multi_cell(0, 5, body)
        self.ln()

    def print_table(self, df, col_widths=None):
        # Añadir encabezados de la tabla
        self.set_font('Arial', 'B', 8)
        if col_widths is None:
            col_width = self.w / (len(df.columns) + 1)
            col_widths = [col_width] * len(df.columns)

        for i, col in enumerate(df.columns):
            self.cell(col_widths[i], 7, str(col), 1, 0, 'C')
        self.ln()

        # Añadir filas de la tabla
        self.set_font('Arial', '', 8)
        for index, row in df.iterrows():
            for i, col_val in enumerate(row.values):
                self.cell(col_widths[i], 7, f'{col_val:.4f}' if isinstance(col_val, (int, float)) else str(col_val), 1, 0, 'C')
            self.ln()
        self.ln(10)

# Crear el documento PDF
pdf = PDF()
pdf.add_page()
pdf.set_auto_page_break(auto=True, margin=15)

# Título General
pdf.chapter_title('Análisis de Cotización y Beta por Frecuencia')

# Introducción
pdf.chapter_body(
    "Este informe detalla el análisis de la cotización histórica de Antofagasta plc (ANTO.L) y el S&P 500 (como índice de mercado) durante los últimos 5 años, calculando la rentabilidad, el riesgo y el coeficiente Beta en diferentes frecuencias (diaria, semanal y mensual)."
)

# Sección de Resumen de Riesgo, Retorno y Beta
pdf.chapter_title('Resumen de Rentabilidad, Riesgo y Beta por Frecuencia')

# Formatear el DataFrame para la tabla del PDF
df_results_formatted = df_results.copy()
df_results_formatted.index.name = 'Frecuencia'
df_results_formatted = df_results_formatted.reset_index()

# Definir anchos de columna para la tabla (ajustar según sea necesario)
col_widths_beta = [30, 40, 40, 30] # Frecuencia, Rentabilidad Promedio, Riesgo, Beta
pdf.print_table(df_results_formatted, col_widths=col_widths_beta)


# Guardar el PDF final
pdf_output_path_beta = 'informe_analisis_beta.pdf'
pdf.output(pdf_output_path_beta)

print(f'Informe PDF con análisis de Beta generado exitosamente como {pdf_output_path_beta}')


