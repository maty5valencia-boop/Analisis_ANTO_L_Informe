
import yfinance as yf
import pandas as pd
import matplotlib.pyplot as plt
from fpdf import FPDF

# --- 1. Empresa elegida: ANTOFAGASTA MINERALS(Antofagasta plc en la Bolsa \de Londres) ---
ticker = "ANTO.L" 
# --- 2. Descarga de los últimos 5 años con frecuencia semanal ---
data = yf.download(ticker, period="5y", interval="1wk")
# --- 3 Cálculo de rentabilidad con precios de cierre(Usamos el precio de cierre ajustado 'Adj Close' para mayor precisión) ---
# Acceder a la columna 'Close' usando el MultiIndex
data['Rentabilidad'] = data[('Close', ticker)].pct_change()
# 4. Resumen de Rentabilidad Promedio y Riesgo (Desviación Estándar)
resumen_stats = pd.DataFrame({
    'Métrica': ['Rentabilidad Promedio Semanal', 'Riesgo (Desviación Estándar)']
})
# Utilizar .iloc para asignar los valores directamente a la columna 'Valor'
resumen_stats['Valor'] = [data['Rentabilidad'].mean(), data['Rentabilidad'].std()]

print("--- RESUMEN DE RIESGO Y RETORNO ---")
print(resumen_stats.to_string(index=False))

# --- 5. Grafico de la cotizacion del periodo --- plt.figure(figsize=(10, 6))
# Acceder a la columna 'Close' usando el MultiIndex
plt.plot(data[('Close', ticker)], color='blue', linewidth=1.5, label='Precio de Cierre')
plt.title(f'Cotización Semanal de {ticker} - Últimos 5 Años')
plt.xlabel('Fecha')
plt.ylabel('Precio (GBP)')
plt.legend()
plt.grid(True, linestyle='--', alpha=0.6)

# Guardar el gráfico como PNG
chart_file_name = 'cotizacion_historica_ANTO_L.png'
plt.savefig(chart_file_name)
plt.close() # Cierra la figura para evitar mostrarla si se ejecuta en un entorno sin GUI

# --- 6. Creación del informe PDF con fpdf2 ---
class PDF(FPDF):
    def header(self):
        self.set_font('Arial', 'B', 12)
        self.cell(0, 10, 'Informe de Análisis de Cotización', 0, 1, 'C')

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

    def print_table(self, df):
        # Añadir encabezados de la tabla
        self.set_font('Arial', 'B', 8)
        # Asegurar que el ancho de la columna sea proporcional y evite desbordamientos
        col_width = self.w / (len(df.columns) + 1) 
        for col in df.columns:
            self.cell(col_width, 7, str(col), 1, 0, 'C')
        self.ln()

        # Añadir filas de la tabla
        self.set_font('Arial', '', 8)
        for index, row in df.iterrows():
            for col in df.columns:
                self.cell(col_width, 7, str(row[col]), 1, 0, 'C')
            self.ln()
        self.ln(10)

# Crear el documento PDF
pdf = PDF()
pdf.add_page()
pdf.set_auto_page_break(auto=True, margin=15)

# Título
pdf.chapter_title('Análisis de Rentabilidad y Riesgo')

# Introducción
pdf.chapter_body(
    "Este informe presenta un análisis de la cotización histórica de Antofagasta plc (ANTO.L) durante los últimos 5 años, con una frecuencia semanal. Se incluye un resumen de la rentabilidad promedio y el riesgo, así como un gráfico de la evolución de su precio de cierre."
)

# Sección de Resumen de Riesgo y Retorno
pdf.chapter_title('Resumen de Riesgo y Retorno')
pdf.print_table(resumen_stats)

# Sección del Gráfico
pdf.chapter_title('Cotización Histórica (Últimos 5 años)')
img_width = 150 # Ancho de la imagen (en mm)
x_pos = (pdf.w - img_width) / 2
pdf.image(chart_file_name, x=x_pos, w=img_width)

# Guardar el PDF final
pdf_output_path = 'informe_analisis_ANTO_L.pdf'
pdf.output(pdf_output_path)

print(f'Informe PDF generado exitosamente como {pdf_output_path}')