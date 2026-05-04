import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
from scipy.stats import t as tdist
import warnings
import os
import datetime
warnings.filterwarnings('ignore')

# ============================================================
# CONFIGURACIÓN
# ============================================================
ARCHIVO = r"C:\Users\crist\Downloads\Datos_bursatiles_704551.xlsx"
DIR_SALIDA = os.path.dirname(os.path.abspath(__file__))  # misma carpeta del script

IMG1 = os.path.join(DIR_SALIDA, 'modelo1_ipsa_fundamentales.png')
IMG2 = os.path.join(DIR_SALIDA, 'modelo2_capm_residuos.png')
IMG3 = os.path.join(DIR_SALIDA, 'modelo3_fed_model.png')
PDF_SALIDA = os.path.join(DIR_SALIDA, 'informe_modelos_bursatiles.pdf')

# ============================================================
# CARGA DE DATOS
# ============================================================
print("Cargando datos...")
bolsas_raw = pd.read_excel(ARCHIVO, sheet_name='Bolsas', header=None)
countries = bolsas_raw.iloc[9, 1:].tolist()
bolsas = bolsas_raw.iloc[10:].copy()
bolsas.columns = ['Fecha'] + countries
bolsas['Fecha'] = pd.to_datetime(bolsas['Fecha'], errors='coerce')
bolsas = bolsas.dropna(subset=['Fecha']).set_index('Fecha').sort_index()
for c in countries:
    bolsas[c] = pd.to_numeric(bolsas[c], errors='coerce')

bolsas_usd_raw = pd.read_excel(ARCHIVO, sheet_name='Bolsas (USD)', header=None)
bolsas_usd = bolsas_usd_raw.iloc[10:].copy()
bolsas_usd.columns = ['Fecha'] + countries
bolsas_usd['Fecha'] = pd.to_datetime(bolsas_usd['Fecha'], errors='coerce')
bolsas_usd = bolsas_usd.dropna(subset=['Fecha']).set_index('Fecha').sort_index()
for c in countries:
    bolsas_usd[c] = pd.to_numeric(bolsas_usd[c], errors='coerce')

fin_raw = pd.read_excel(ARCHIVO, sheet_name='Otros Financieros', header=None)
fin = fin_raw.iloc[10:].copy()
fin.columns = ['Fecha', 'BCU10', 'PE_IPSA', 'MXEF']
fin['Fecha'] = pd.to_datetime(fin['Fecha'], errors='coerce')
fin = fin.dropna(subset=['Fecha']).set_index('Fecha').sort_index()
for c in ['BCU10', 'PE_IPSA', 'MXEF']:
    fin[c] = pd.to_numeric(fin[c], errors='coerce')

macro_raw = pd.read_excel(ARCHIVO, sheet_name='Otros macroeconomicos', header=None)
macro = macro_raw.iloc[6:].copy()
macro.columns = ['Fecha', 'Cobre', 'Petroleo', 'IMACEC']
macro['Fecha'] = pd.to_datetime(macro['Fecha'], errors='coerce')
macro = macro.dropna(subset=['Fecha']).set_index('Fecha').sort_index()
for c in ['Cobre', 'Petroleo', 'IMACEC']:
    macro[c] = pd.to_numeric(macro[c], errors='coerce')

# ============================================================
# FUNCIÓN OLS
# ============================================================
def ols_fit(y, X_df):
    Xm      = np.column_stack([np.ones(len(y))] + [X_df[c].values for c in X_df.columns])
    beta    = np.linalg.lstsq(Xm, y.values, rcond=None)[0]
    fitted  = Xm @ beta
    resid   = y.values - fitted
    n, k    = Xm.shape
    s2      = np.sum(resid**2) / (n - k)
    se      = np.sqrt(s2)
    se_b    = np.sqrt(np.diag(np.linalg.inv(Xm.T @ Xm)) * s2)
    t_stats = beta / se_b
    t_crit  = tdist.ppf(0.975, df=n - k)
    return (beta,
            pd.Series(fitted, index=y.index),
            pd.Series(resid,  index=y.index),
            se, t_stats, t_crit)

# ============================================================
# MODELO 1: IPSA ~ IMACEC + COBRE + PETROLEO
# ============================================================
print("Calculando Modelo 1...")
ipsa_m = bolsas['Chile'].resample('ME').last().dropna()
ipsa_m.index = ipsa_m.index.to_period('M').to_timestamp('M')
macro2 = macro.copy()
macro2.index = macro2.index.to_period('M').to_timestamp('M')

df1 = pd.DataFrame({'IPSA': ipsa_m}).join(
    macro2[['Cobre', 'Petroleo', 'IMACEC']], how='inner'
).dropna()

log_ipsa = np.log(df1['IPSA'])
X1 = pd.DataFrame({
    'log_cobre'  : np.log(df1['Cobre']),
    'log_petro'  : np.log(df1['Petroleo']),
    'log_imacec' : np.log(df1['IMACEC'])
}, index=df1.index)

beta1, fitted1, resid1, se1, tstat1, tcrit1 = ols_fit(log_ipsa, X1)

nombres1 = ['Constante', 'log(Cobre)', 'log(Petroleo)', 'log(IMACEC)']
last12    = resid1.iloc[-12:]
t_rec1    = last12.mean() / (se1 / np.sqrt(12))
sig_rec1  = abs(t_rec1) > tcrit1
dir1      = "SOBRE" if last12.mean() > 0 else "BAJO"
color_rec1 = '#E74C3C' if sig_rec1 else '#E67E22'

# R² Modelo 1
ss_res1 = np.sum(resid1.values**2)
ss_tot1 = np.sum((log_ipsa.values - log_ipsa.values.mean())**2)
r2_m1   = 1 - ss_res1 / ss_tot1

fig1, (ax1a, ax1b) = plt.subplots(2, 1, figsize=(13, 9), facecolor='white')
fig1.suptitle(
    'Modelo 1: IPSA — Determinacion por Fundamentales Economicos\n'
    'log(IPSA) = a + b1*log(Cobre) + b2*log(Petroleo) + b3*log(IMACEC SA)',
    fontsize=12, fontweight='bold', color='#1B3A6B'
)

fitted_nivel = np.exp(fitted1)
ci_up = np.exp(fitted1 + 1.96 * se1)
ci_dn = np.exp(fitted1 - 1.96 * se1)
first_rec1 = resid1.index[-12]
last_rec1  = resid1.index[-1]

ax1a.plot(df1.index, df1['IPSA'], color='#1B3A6B', lw=2, label='IPSA Observado')
ax1a.plot(fitted_nivel.index, fitted_nivel.values, color='#E74C3C', lw=1.8, linestyle='--', label='IPSA Estimado')
ax1a.fill_between(fitted_nivel.index, ci_dn, ci_up, alpha=0.12, color='#E74C3C', label='IC 95%')
ax1a.axvspan(first_rec1, last_rec1, alpha=0.10, color=color_rec1)
ax1a.set_title('Nivel Observado vs. Estimado', fontsize=10)
ax1a.set_ylabel('Puntos')
ax1a.legend(loc='upper left', fontsize=9)
ax1a.grid(alpha=0.25, linestyle='--')
ax1a.spines[['top', 'right']].set_visible(False)
ax1a.xaxis.set_major_formatter(mdates.DateFormatter('%Y'))
ax1a.xaxis.set_major_locator(mdates.YearLocator(2))

bar_colors1 = ['#E74C3C' if r > 0 else '#1B3A6B' for r in resid1.values]
ax1b.bar(resid1.index, resid1.values, width=20, color=bar_colors1, alpha=0.75)
ax1b.axhline(0, color='black', lw=0.9)
ax1b.axhline( 1.96*se1, color='#E67E22', lw=1.2, linestyle='--', label=f'+1.96s ({1.96*se1:.3f})')
ax1b.axhline(-1.96*se1, color='#E67E22', lw=1.2, linestyle='--', label=f'-1.96s ({-1.96*se1:.3f})')
ax1b.axvspan(first_rec1, last_rec1, alpha=0.10, color=color_rec1)
ax1b.set_title(f'Residuos  |  Ult.12m: IPSA {dir1} fundamental  |  t={t_rec1:.2f}  |  {"Sig. 5%" if sig_rec1 else "No sig. 5%"}',
               fontsize=10, color=color_rec1)
ax1b.set_ylabel('Residuo (log)')
ax1b.legend(fontsize=9)
ax1b.grid(alpha=0.25, linestyle='--')
ax1b.spines[['top', 'right']].set_visible(False)
ax1b.xaxis.set_major_formatter(mdates.DateFormatter('%Y'))
ax1b.xaxis.set_major_locator(mdates.YearLocator(2))

plt.tight_layout()
plt.savefig(IMG1, dpi=150, bbox_inches='tight')
plt.close()
print(f"  Guardado: {IMG1}")

# ============================================================
# MODELO 2: CAPM REGIONAL
# ============================================================
print("Calculando Modelo 2...")
mxef = fin['MXEF'].dropna()

def capm_pais(country):
    idx_m = bolsas_usd[country].resample('ME').last().dropna()
    idx_m.index = idx_m.index.to_period('M').to_timestamp('M')
    ref_m = mxef.resample('ME').last().dropna()
    ref_m.index = ref_m.index.to_period('M').to_timestamp('M')
    df = pd.DataFrame({'idx': idx_m, 'ref': ref_m}).dropna()
    if len(df) < 30:
        return None
    log_idx = np.log(df['idx'])
    X = pd.DataFrame({'log_ref': np.log(df['ref'])}, index=df.index)
    beta, fitted, resid, se, tstat, tcrit = ols_fit(log_idx, X)
    t_last = resid.iloc[-1] / se
    sig    = abs(t_last) > tcrit
    ss_res = np.sum(resid.values**2)
    ss_tot = np.sum((log_idx.values - log_idx.values.mean())**2)
    r2 = 1 - ss_res / ss_tot
    return {
        'beta': beta, 'resid': resid, 'se': se, 'r2': r2,
        't_last': t_last, 'sig': sig,
        'direction': 'Sobrevalorado' if resid.iloc[-1] > 0 else 'Subvalorado'
    }

paises = ['Chile', 'Argentina', 'Brasil', 'Colombia', 'Mexico', 'Peru',
          'EE.UU. (S&P 500)', 'China', 'India', 'Corea', 'Alemania', 'Japon']

# También intentar variantes con tilde
paises_alt = {
    'Mexico': 'México',
    'Peru': 'Perú',
    'Japon': 'Japón',
    'EE.UU. (S&P 500)': 'EE.UU. (S&P 500)',
}

resultados_capm = {}
for p in paises:
    col = p
    if col not in bolsas_usd.columns:
        # intentar variante
        for k, v in paises_alt.items():
            if k == p and v in bolsas_usd.columns:
                col = v
                break
    if col in bolsas_usd.columns:
        r = capm_pais(col)
        if r:
            resultados_capm[col] = r

n_paises = len(resultados_capm)
ncols = 3
nrows = (n_paises + ncols - 1) // ncols

fig2, axes2 = plt.subplots(nrows, ncols, figsize=(16, nrows * 3.8 + 1), facecolor='white')
fig2.suptitle(
    'Modelo 2: Componente No Explicado por MSCI Emergentes (USD)\n'
    'log(Indice USD) = a + b*log(MSCI EM)',
    fontsize=12, fontweight='bold', color='#1B3A6B'
)

axes_flat = axes2.flatten() if n_paises > 1 else [axes2]
for i, (pais, r) in enumerate(resultados_capm.items()):
    ax = axes_flat[i]
    resid = r['resid']
    se    = r['se']
    bar_colors2 = ['#E74C3C' if v > 0 else '#1B3A6B' for v in resid.values]
    ax.bar(resid.index, resid.values, width=20, color=bar_colors2, alpha=0.65)
    ax.axhline(0, color='black', lw=0.9)
    ax.axhline( 1.96*se, color='#E67E22', lw=1.1, linestyle='--')
    ax.axhline(-1.96*se, color='#E67E22', lw=1.1, linestyle='--')
    ax.fill_between(resid.index, -1.96*se, 1.96*se, alpha=0.06, color='gray')
    ax.scatter([resid.index[-1]], [resid.iloc[-1]], s=50,
               color='#E74C3C' if resid.iloc[-1] > 0 else '#1B3A6B', zorder=5)
    sig_str = 'Sig 5%' if r['sig'] else 'No sig'
    ax.set_title(f"{pais}\nb={r['beta'][1]:.2f} | {r['direction']} | {sig_str}", fontsize=8.5, color='#1B3A6B')
    ax.grid(alpha=0.25, linestyle='--')
    ax.spines[['top', 'right']].set_visible(False)
    ax.xaxis.set_major_formatter(mdates.DateFormatter('%y'))
    ax.xaxis.set_major_locator(mdates.YearLocator(4))
    ax.tick_params(labelsize=7)

for j in range(i + 1, len(axes_flat)):
    axes_flat[j].set_visible(False)

fig2.text(0.01, 0.005,
          'Rojo: Sobrevaloración  |  Azul: Subvaloración  |  Lineas naranja: +/-1.96s',
          fontsize=8, style='italic', color='#2C3E50')
plt.tight_layout(rect=[0, 0.02, 1, 0.97])
plt.savefig(IMG2, dpi=150, bbox_inches='tight')
plt.close()
print(f"  Guardado: {IMG2}")

# ============================================================
# MODELO 3: FED MODEL
# ============================================================
print("Calculando Modelo 3...")
df3 = pd.DataFrame({'PE': fin['PE_IPSA'], 'BCU10': fin['BCU10']}).dropna()
df3['UP']     = (1 / df3['PE']) * 100
df3['spread'] = df3['BCU10'] - df3['UP']

mu  = df3['spread'].mean()
std = df3['spread'].std()
ult = df3['spread'].iloc[-1]

if ult > mu + std:
    zona = "SOBRE el rango: acciones caras relativo a bonos"
    color_zona = '#E74C3C'
elif ult < mu - std:
    zona = "BAJO el rango: acciones baratas relativo a bonos"
    color_zona = '#27AE60'
else:
    zona = "DENTRO del rango +/-1s: valoracion neutra"
    color_zona = '#E67E22'

fig3, (ax3a, ax3b) = plt.subplots(2, 1, figsize=(13, 9), facecolor='white')
fig3.suptitle(
    'Modelo 3: Fed Model — BCU 10 anos vs. Earnings Yield (U/P) del IPSA',
    fontsize=12, fontweight='bold', color='#1B3A6B'
)

ax3a.plot(df3.index, df3['BCU10'], color='#1B3A6B', lw=1.5, label='BCU 10 anos (%)')
ax3a.plot(df3.index, df3['UP'],    color='#E74C3C', lw=1.5, linestyle='--', label='Earnings Yield IPSA (%)')
ax3a.set_title('Tasa Real (BCU10) y Earnings Yield del IPSA', fontsize=10)
ax3a.set_ylabel('% anual')
ax3a.legend(fontsize=9)
ax3a.grid(alpha=0.25, linestyle='--')
ax3a.spines[['top', 'right']].set_visible(False)
ax3a.xaxis.set_major_formatter(mdates.DateFormatter('%Y'))
ax3a.xaxis.set_major_locator(mdates.YearLocator(2))

ax3b.plot(df3.index, df3['spread'], color='#8E44AD', lw=1.4, label='Spread BCU10 - U/P')
ax3b.axhline(mu,       color='black',   lw=1.5, label=f'Promedio ({mu:.2f}%)')
ax3b.axhline(mu + std, color='#E67E22', lw=1.2, linestyle='--', label=f'+1s ({mu+std:.2f}%)')
ax3b.axhline(mu - std, color='#E67E22', lw=1.2, linestyle='--', label=f'-1s ({mu-std:.2f}%)')
ax3b.fill_between(df3.index, mu - std, mu + std, alpha=0.10, color='#E67E22')
ax3b.scatter([df3.index[-1]], [ult], s=80, color=color_zona, zorder=6, label=f'Ultimo ({ult:.2f}%)')
ax3b.set_title(f'Spread y Bandas +/-1s  |  {zona}', fontsize=10, color=color_zona)
ax3b.set_ylabel('Diferencia (p.p.)')
ax3b.legend(fontsize=8.5, ncol=2)
ax3b.grid(alpha=0.25, linestyle='--')
ax3b.spines[['top', 'right']].set_visible(False)
ax3b.xaxis.set_major_formatter(mdates.DateFormatter('%Y'))
ax3b.xaxis.set_major_locator(mdates.YearLocator(2))

plt.tight_layout()
plt.savefig(IMG3, dpi=150, bbox_inches='tight')
plt.close()
print(f"  Guardado: {IMG3}")

# ============================================================
# GENERACIÓN DEL INFORME PDF
# ============================================================
print("\nGenerando informe PDF...")

from reportlab.lib.pagesizes import A4
from reportlab.lib import colors
from reportlab.lib.units import cm
from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
from reportlab.lib.enums import TA_CENTER, TA_JUSTIFY, TA_LEFT
from reportlab.platypus import (SimpleDocTemplate, Paragraph, Spacer,
                                 Image as RLImage, Table, TableStyle,
                                 HRFlowable, PageBreak, KeepTogether)

AZUL      = colors.HexColor('#1B3A6B')
ROJO      = colors.HexColor('#E74C3C')
NARANJA   = colors.HexColor('#E67E22')
VERDE     = colors.HexColor('#27AE60')
GRIS_CLARO= colors.HexColor('#F5F5F5')
GRIS_TEXTO= colors.HexColor('#2C3E50')

doc = SimpleDocTemplate(
    PDF_SALIDA,
    pagesize=A4,
    leftMargin=2.2*cm, rightMargin=2.2*cm,
    topMargin=2*cm, bottomMargin=2*cm,
    title='Informe de Modelos Bursatiles'
)

ancho_util = A4[0] - 4.4*cm

styles = getSampleStyleSheet()
estilo_titulo    = ParagraphStyle('titulo',    fontSize=22, fontName='Helvetica-Bold',
                                   textColor=AZUL, alignment=TA_CENTER, spaceAfter=6)
estilo_subtitulo = ParagraphStyle('subtitulo', fontSize=13, fontName='Helvetica-Bold',
                                   textColor=AZUL, alignment=TA_CENTER, spaceAfter=4)
estilo_h2        = ParagraphStyle('h2',        fontSize=13, fontName='Helvetica-Bold',
                                   textColor=AZUL, spaceBefore=14, spaceAfter=6)
estilo_h3        = ParagraphStyle('h3',        fontSize=11, fontName='Helvetica-Bold',
                                   textColor=GRIS_TEXTO, spaceBefore=8, spaceAfter=4)
estilo_body      = ParagraphStyle('body',      fontSize=10, fontName='Helvetica',
                                   textColor=GRIS_TEXTO, alignment=TA_JUSTIFY,
                                   spaceBefore=4, spaceAfter=4, leading=15)
estilo_bullet    = ParagraphStyle('bullet',    fontSize=10, fontName='Helvetica',
                                   textColor=GRIS_TEXTO, leftIndent=14,
                                   spaceBefore=2, spaceAfter=2, leading=14)
estilo_caption   = ParagraphStyle('caption',   fontSize=8.5, fontName='Helvetica-Oblique',
                                   textColor=colors.grey, alignment=TA_CENTER,
                                   spaceBefore=2, spaceAfter=8)
estilo_conclu    = ParagraphStyle('conclu',    fontSize=11, fontName='Helvetica-Bold',
                                   textColor=AZUL, alignment=TA_CENTER,
                                   spaceBefore=6, spaceAfter=6)

def hr():
    return HRFlowable(width='100%', thickness=1, color=AZUL, spaceAfter=8, spaceBefore=4)

def img_centrada(path, ancho_pct=0.98):
    ancho_img = ancho_util * ancho_pct
    return RLImage(path, width=ancho_img, height=ancho_img * 0.69)

def caja_resultado(texto, color_fondo=GRIS_CLARO, color_borde=AZUL):
    data = [[Paragraph(texto, ParagraphStyle('caja', fontSize=10, fontName='Helvetica-Bold',
                                              textColor=AZUL, alignment=TA_CENTER))]]
    t = Table(data, colWidths=[ancho_util])
    t.setStyle(TableStyle([
        ('BACKGROUND', (0,0), (-1,-1), color_fondo),
        ('BOX',        (0,0), (-1,-1), 1.5, color_borde),
        ('TOPPADDING', (0,0), (-1,-1), 8),
        ('BOTTOMPADDING', (0,0), (-1,-1), 8),
        ('LEFTPADDING', (0,0), (-1,-1), 14),
        ('RIGHTPADDING', (0,0), (-1,-1), 14),
    ]))
    return t

# ---------- CONSTRUIR HISTORIA ----------
story = []
fecha_hoy = datetime.date.today().strftime('%d de %B de %Y').capitalize()

# ── PORTADA ──────────────────────────────────────────────────
story.append(Spacer(1, 1.5*cm))
story.append(Paragraph("INFORME DE VALORACION BURSATIL", estilo_titulo))
story.append(Paragraph("Analisis Cuantitativo del IPSA y Mercados Regionales", estilo_subtitulo))
story.append(Spacer(1, 0.4*cm))
story.append(hr())

data_portada = [
    ['Fecha de elaboracion:', fecha_hoy],
    ['Modelos aplicados:', 'Fundamentales Economicos | CAPM Regional | Fed Model'],
    ['Mercado objetivo:', 'IPSA (Chile) y Indices Regionales en USD'],
    ['Benchmark regional:', 'MSCI Emerging Markets (MXEF)'],
    ['Fuente de datos:', 'Datos_bursatiles_704551.xlsx'],
]
tabla_portada = Table(data_portada, colWidths=[5*cm, ancho_util - 5*cm])
tabla_portada.setStyle(TableStyle([
    ('FONTNAME',   (0,0), (0,-1), 'Helvetica-Bold'),
    ('FONTNAME',   (1,0), (1,-1), 'Helvetica'),
    ('FONTSIZE',   (0,0), (-1,-1), 10),
    ('TEXTCOLOR',  (0,0), (0,-1), AZUL),
    ('TEXTCOLOR',  (1,0), (1,-1), GRIS_TEXTO),
    ('ROWBACKGROUNDS', (0,0), (-1,-1), [colors.white, GRIS_CLARO]),
    ('TOPPADDING', (0,0), (-1,-1), 6),
    ('BOTTOMPADDING', (0,0), (-1,-1), 6),
    ('LEFTPADDING', (0,0), (-1,-1), 8),
]))
story.append(tabla_portada)
story.append(Spacer(1, 0.6*cm))
story.append(hr())

# ── INTRODUCCIÓN ─────────────────────────────────────────────
story.append(Paragraph("Introduccion y Metodologia", estilo_h2))
story.append(Paragraph(
    "El presente informe aplica tres modelos cuantitativos complementarios para evaluar el nivel "
    "de valoracion del IPSA chileno y de los principales indices bursatiles regionales. "
    "Cada modelo aborda una dimension distinta del analisis: los fundamentales macroeconomicos, "
    "la prima de riesgo relativa al mercado emergente global y la comparacion con tasas reales de largo plazo. "
    "Los tres enfoques en conjunto permiten obtener una vision integral sobre si el mercado se encuentra "
    "en niveles de sobre o subvaloracion estadisticamente significativos.",
    estilo_body
))
story.append(Spacer(1, 0.2*cm))

# ── MODELO 1 ─────────────────────────────────────────────────
story.append(PageBreak())
story.append(Paragraph("Modelo 1: IPSA por Fundamentales Economicos", estilo_h2))
story.append(hr())

story.append(Paragraph("Especificacion del Modelo", estilo_h3))
story.append(Paragraph(
    "Se estima una regresion OLS en logaritmos donde el nivel del IPSA queda explicado por "
    "los precios del cobre, del petroleo y el IMACEC desestacionalizado. La especificacion logaritmica "
    "permite interpretar los coeficientes directamente como elasticidades. La muestra se trabaja "
    "en frecuencia mensual utilizando el ultimo valor de cada mes.",
    estilo_body
))
story.append(Paragraph(
    "Ecuacion estimada: log(IPSA) = a + b1*log(Cobre) + b2*log(Petroleo) + b3*log(IMACEC)",
    ParagraphStyle('ecuacion', fontSize=10, fontName='Helvetica-Bold',
                   textColor=AZUL, alignment=TA_CENTER, spaceBefore=6, spaceAfter=8,
                   backColor=GRIS_CLARO, borderPadding=8)
))

story.append(Paragraph("Resultados de los Coeficientes", estilo_h3))

# tabla de coeficientes modelo 1
nombres1_tabla = ['Constante', 'log(Cobre)', 'log(Petroleo)', 'log(IMACEC)']
data_tabla1 = [['Variable', 'Coeficiente (b)', 't-estadistico', 'Significancia (5%)']]
for nom, b, t in zip(nombres1_tabla, beta1, tstat1):
    sig_txt = 'SI' if abs(t) > tcrit1 else 'NO'
    sig_color = 'SI' if abs(t) > tcrit1 else 'NO'
    data_tabla1.append([nom, f'{b:+.4f}', f'{t:+.2f}', sig_txt])

t1 = Table(data_tabla1, colWidths=[4.5*cm, 3.5*cm, 3.5*cm, 4*cm])
t1_style = TableStyle([
    ('BACKGROUND',    (0,0), (-1,0), AZUL),
    ('TEXTCOLOR',     (0,0), (-1,0), colors.white),
    ('FONTNAME',      (0,0), (-1,0), 'Helvetica-Bold'),
    ('FONTSIZE',      (0,0), (-1,-1), 9.5),
    ('ALIGN',         (1,0), (-1,-1), 'CENTER'),
    ('ROWBACKGROUNDS',(0,1), (-1,-1), [colors.white, GRIS_CLARO]),
    ('TOPPADDING',    (0,0), (-1,-1), 5),
    ('BOTTOMPADDING', (0,0), (-1,-1), 5),
    ('GRID',          (0,0), (-1,-1), 0.4, colors.lightgrey),
])
for i, (nom, b, t) in enumerate(zip(nombres1_tabla, beta1, tstat1), start=1):
    color_sig = VERDE if abs(t) > tcrit1 else NARANJA
    t1_style.add('TEXTCOLOR', (3, i), (3, i), color_sig)
    t1_style.add('FONTNAME',  (3, i), (3, i), 'Helvetica-Bold')
t1.setStyle(t1_style)
story.append(t1)
story.append(Spacer(1, 0.3*cm))

# resumen estadístico m1
story.append(Paragraph("Diagnostico del Periodo Reciente (ultimos 12 meses)", estilo_h3))
story.append(Paragraph(
    f"El R<super>2</super> del modelo es {r2_m1:.3f}, lo que indica que los tres fundamentales explican "
    f"aproximadamente el {r2_m1*100:.1f}% de la variabilidad del IPSA en terminos logaritmicos. "
    "El analisis de los residuos de los ultimos doce meses permite evaluar si el IPSA se encuentra "
    "actualmente desalineado de su nivel fundamental estimado.",
    estilo_body
))

resid_prom = last12.mean()
dir_txt = "por encima" if resid_prom > 0 else "por debajo"
sig_txt1 = "estadisticamente significativa al 5%" if sig_rec1 else "no estadisticamente significativa al 5%"
color_box1 = colors.HexColor('#FDECEA') if sig_rec1 else colors.HexColor('#FEF3E2')
borde_box1 = ROJO if sig_rec1 else NARANJA

story.append(caja_resultado(
    f"Residuo promedio ultimos 12m: {resid_prom:+.4f}  |  t = {t_rec1:+.2f}  |  "
    f"IPSA {dir1} su valor fundamental ({sig_txt1})",
    color_fondo=color_box1, color_borde=borde_box1
))
story.append(Spacer(1, 0.3*cm))

story.append(Paragraph("Interpretacion", estilo_h3))

if sig_rec1 and dir1 == "SOBRE":
    interp1 = (
        "El IPSA muestra una desviacion positiva y estadisticamente significativa respecto de su valor "
        "fundamental durante los ultimos doce meses. Esto sugiere que el mercado chileno cotiza a niveles "
        "superiores a los que justifican sus determinantes macroeconomicos (cobre, petroleo e IMACEC). "
        "Desde una perspectiva de valor, este resultado podria interpretarse como una senal de cautela "
        "para nuevas posiciones de largo plazo, sin que ello implique necesariamente una correccion inmediata, "
        "ya que los mercados pueden mantenerse en zonas de sobrevaloracion por periodos prolongados."
    )
elif sig_rec1 and dir1 == "BAJO":
    interp1 = (
        "El IPSA muestra una desviacion negativa y estadisticamente significativa respecto de su valor "
        "fundamental durante los ultimos doce meses. Esto sugiere que el mercado chileno cotiza por debajo "
        "de los niveles que justifican sus determinantes macroeconomicos (cobre, petroleo e IMACEC), "
        "lo que podria interpretarse como una oportunidad de entrada desde una perspectiva de valor fundamental."
    )
else:
    interp1 = (
        "La desviacion del IPSA respecto de su valor fundamental en los ultimos doce meses no es "
        "estadisticamente significativa al 5%. Esto indica que, dentro del marco de este modelo, "
        "el IPSA se encuentra en un rango de valoracion coherente con los determinantes macroeconomicos "
        "considerados, sin evidencia solida de sobre ni subvaloracion."
    )

story.append(Paragraph(interp1, estilo_body))
story.append(Spacer(1, 0.4*cm))

story.append(img_centrada(IMG1))
story.append(Paragraph(
    "Figura 1: Panel superior: IPSA observado (azul) vs. estimado por fundamentales (rojo punteado) "
    "con intervalo de confianza al 95%. Panel inferior: residuos mensuales con bandas +/-1.96 desviaciones "
    "estandar. La zona sombreada resalta los ultimos 12 meses.",
    estilo_caption
))

# ── MODELO 2 ─────────────────────────────────────────────────
story.append(PageBreak())
story.append(Paragraph("Modelo 2: CAPM Regional vs. MSCI Emergentes", estilo_h2))
story.append(hr())

story.append(Paragraph("Especificacion del Modelo", estilo_h3))
story.append(Paragraph(
    "Para cada pais se estima una regresion OLS que relaciona el logaritmo del indice bursatil "
    "local (en USD) con el logaritmo del MSCI Emerging Markets (MXEF). El residuo resultante "
    "captura el componente de retorno no explicado por el movimiento del mercado emergente global, "
    "sirviendo como medida de sobre o subvaloracion relativa. Se requiere un minimo de 30 "
    "observaciones mensuales para incluir un pais en el analisis.",
    estilo_body
))
story.append(Paragraph(
    "Ecuacion estimada: log(Indice USD) = a + b*log(MSCI EM)",
    ParagraphStyle('ecuacion2', fontSize=10, fontName='Helvetica-Bold',
                   textColor=AZUL, alignment=TA_CENTER, spaceBefore=6, spaceAfter=8,
                   backColor=GRIS_CLARO, borderPadding=8)
))

story.append(Paragraph("Resultados por Pais — Ultimo Periodo", estilo_h3))

data_tabla2 = [['Pais', 'Beta (b)', 't ultimo', 'Estado actual', 'Sig. 5%', 'R2']]
for pais, r in resultados_capm.items():
    sig_txt = 'SI' if r['sig'] else 'NO'
    data_tabla2.append([
        pais,
        f"{r['beta'][1]:.3f}",
        f"{r['t_last']:+.2f}",
        r['direction'],
        sig_txt,
        f"{r['r2']:.3f}"
    ])

col_anchos2 = [3.8*cm, 2.2*cm, 2.2*cm, 3.2*cm, 2.0*cm, 2.2*cm]
t2 = Table(data_tabla2, colWidths=col_anchos2)
t2_style = TableStyle([
    ('BACKGROUND',    (0,0), (-1,0), AZUL),
    ('TEXTCOLOR',     (0,0), (-1,0), colors.white),
    ('FONTNAME',      (0,0), (-1,0), 'Helvetica-Bold'),
    ('FONTSIZE',      (0,0), (-1,-1), 8.5),
    ('ALIGN',         (1,0), (-1,-1), 'CENTER'),
    ('ROWBACKGROUNDS',(0,1), (-1,-1), [colors.white, GRIS_CLARO]),
    ('TOPPADDING',    (0,0), (-1,-1), 4),
    ('BOTTOMPADDING', (0,0), (-1,-1), 4),
    ('GRID',          (0,0), (-1,-1), 0.4, colors.lightgrey),
])
for i, (pais, r) in enumerate(resultados_capm.items(), start=1):
    # color estado
    col_estado = ROJO if r['direction'] == 'Sobrevalorado' else AZUL
    t2_style.add('TEXTCOLOR', (3, i), (3, i), col_estado)
    t2_style.add('FONTNAME',  (3, i), (3, i), 'Helvetica-Bold')
    # color sig
    col_sig = VERDE if r['sig'] else NARANJA
    t2_style.add('TEXTCOLOR', (4, i), (4, i), col_sig)
    t2_style.add('FONTNAME',  (4, i), (4, i), 'Helvetica-Bold')
t2.setStyle(t2_style)
story.append(t2)
story.append(Spacer(1, 0.3*cm))

story.append(Paragraph("Interpretacion General", estilo_h3))

# Conteo sobre/sub
sobre = [p for p, r in resultados_capm.items() if r['direction'] == 'Sobrevalorado' and r['sig']]
sub   = [p for p, r in resultados_capm.items() if r['direction'] == 'Subvalorado'   and r['sig']]
neutro= [p for p, r in resultados_capm.items() if not r['sig']]

story.append(Paragraph(
    "El modelo CAPM regional evalua si cada mercado ha generado un retorno superior o inferior "
    "al que corresponde segun su correlacion historica con el MSCI Emergentes. Los residuos positivos "
    "(barras rojas) senalan sobrevaloración relativa; los negativos (barras azules) señalan subvaloracion.",
    estilo_body
))

if sobre:
    story.append(Paragraph(
        f"<b>Mercados con señal de sobrevaloración significativa:</b> {', '.join(sobre)}.",
        estilo_bullet
    ))
if sub:
    story.append(Paragraph(
        f"<b>Mercados con señal de subvaloracion significativa:</b> {', '.join(sub)}.",
        estilo_bullet
    ))
if neutro:
    story.append(Paragraph(
        f"<b>Sin desviacion significativa:</b> {', '.join(neutro)}.",
        estilo_bullet
    ))

# Interpretacion especifica Chile en M2
if 'Chile' in resultados_capm:
    r_cl = resultados_capm['Chile']
    estado_cl = r_cl['direction']
    sig_cl = "estadisticamente significativa" if r_cl['sig'] else "no estadisticamente significativa"
    story.append(Paragraph(
        f"Para Chile en particular, el residuo del ultimo periodo indica que el IPSA se encuentra "
        f"<b>{estado_cl.lower()}</b> respecto del MSCI Emergentes, con una desviacion {sig_cl} "
        f"(t = {r_cl['t_last']:+.2f}). Su beta estimado de {r_cl['beta'][1]:.2f} refleja la "
        f"sensibilidad del mercado chileno ante movimientos del mercado emergente global.",
        estilo_body
    ))

story.append(Spacer(1, 0.4*cm))
story.append(img_centrada(IMG2))
story.append(Paragraph(
    "Figura 2: Residuos mensuales por pais respecto del MSCI EM (USD). Rojo indica sobrevaloración; "
    "azul, subvaloración. Las bandas naranjas delimitan +/-1.96 desviaciones estandar. "
    "El punto destaca la observacion mas reciente.",
    estilo_caption
))

# ── MODELO 3 ─────────────────────────────────────────────────
story.append(PageBreak())
story.append(Paragraph("Modelo 3: Fed Model — Bonos vs. Acciones", estilo_h2))
story.append(hr())

story.append(Paragraph("Especificacion del Modelo", estilo_h3))
story.append(Paragraph(
    "El Fed Model compara la rentabilidad implicita de las acciones con la tasa de interes real "
    "de largo plazo. En este caso se utiliza la tasa BCU a 10 anos (bono soberano chileno en UF) "
    "como referencia libre de riesgo real, y el earnings yield del IPSA (inverso del P/E) como "
    "medida del retorno exigido implícito a las acciones. El spread entre ambas variables "
    "se compara con su media historica y con bandas de +/-1 desviacion estandar.",
    estilo_body
))
story.append(Paragraph(
    "Spread = BCU10 (%) - Earnings Yield (%) = BCU10 - (1/P·E) x 100",
    ParagraphStyle('ecuacion3', fontSize=10, fontName='Helvetica-Bold',
                   textColor=AZUL, alignment=TA_CENTER, spaceBefore=6, spaceAfter=8,
                   backColor=GRIS_CLARO, borderPadding=8)
))

story.append(Paragraph("Resultados del Periodo Actual", estilo_h3))

data_tabla3 = [
    ['Estadistico', 'Valor'],
    ['Media historica del spread', f'{mu:.2f}%'],
    ['Desviacion estandar', f'{std:.2f}%'],
    ['Limite superior (+1s)', f'{mu+std:.2f}%'],
    ['Limite inferior (-1s)', f'{mu-std:.2f}%'],
    ['Ultimo spread observado', f'{ult:.2f}%'],
    ['Posicion respecto a bandas', zona],
]
t3 = Table(data_tabla3, colWidths=[8*cm, ancho_util - 8*cm])
t3_style = TableStyle([
    ('BACKGROUND',    (0,0), (-1,0), AZUL),
    ('TEXTCOLOR',     (0,0), (-1,0), colors.white),
    ('FONTNAME',      (0,0), (-1,0), 'Helvetica-Bold'),
    ('FONTSIZE',      (0,0), (-1,-1), 10),
    ('FONTNAME',      (0,1), (0,-1), 'Helvetica-Bold'),
    ('TEXTCOLOR',     (0,1), (0,-1), GRIS_TEXTO),
    ('ROWBACKGROUNDS',(0,1), (-1,-1), [colors.white, GRIS_CLARO]),
    ('ALIGN',         (1,1), (1,-1), 'CENTER'),
    ('TOPPADDING',    (0,0), (-1,-1), 5),
    ('BOTTOMPADDING', (0,0), (-1,-1), 5),
    ('GRID',          (0,0), (-1,-1), 0.4, colors.lightgrey),
])
# Color ultima fila segun zona
t3_style.add('TEXTCOLOR', (1, -1), (1, -1), color_zona)
t3_style.add('FONTNAME',  (1, -1), (1, -1), 'Helvetica-Bold')
t3.setStyle(t3_style)
story.append(t3)
story.append(Spacer(1, 0.3*cm))

story.append(Paragraph("Interpretacion", estilo_h3))

if ult > mu + std:
    interp3 = (
        "El spread actual se ubica por encima de la banda superior (+1 desviacion estandar), "
        "lo que senala que la tasa real BCU10 supera ampliamente el earnings yield del IPSA. "
        "Desde la optica del Fed Model, esto indica que los bonos soberanos de largo plazo "
        "ofrecen una rentabilidad relativa mas atractiva que las acciones del IPSA, "
        "situacion que historicamente ha estado asociada a episodios de presion sobre las valoraciones accionarias. "
        "Dicho de otro modo, en este entorno las acciones lucen caras relativo a los bonos reales chilenos."
    )
elif ult < mu - std:
    interp3 = (
        "El spread actual se ubica por debajo de la banda inferior (-1 desviacion estandar), "
        "lo que indica que el earnings yield del IPSA supera ampliamente la tasa BCU10. "
        "Desde la perspectiva del Fed Model, esto sugiere que las acciones ofrecen una rentabilidad "
        "implicita atractiva en relacion con los bonos reales chilenos, lo cual historicamente "
        "ha estado asociado a periodos de mayor potencial de retorno para la renta variable."
    )
else:
    interp3 = (
        "El spread actual se encuentra dentro de la banda historica de +/-1 desviacion estandar, "
        "lo que indica una valoracion relativa neutra entre acciones y bonos reales chilenos. "
        "El earnings yield del IPSA y la tasa BCU10 se encuentran en niveles que historicamente "
        "corresponden a un equilibrio razonable entre renta variable y renta fija."
    )

story.append(Paragraph(interp3, estilo_body))
story.append(Spacer(1, 0.4*cm))

story.append(img_centrada(IMG3))
story.append(Paragraph(
    "Figura 3: Panel superior: BCU10 (azul) vs. Earnings Yield del IPSA (rojo punteado). "
    "Panel inferior: spread BCU10 - U/P con media historica, bandas +/-1s y ultimo dato destacado.",
    estilo_caption
))

# ── CONCLUSIONES INTEGRADAS ───────────────────────────────────
story.append(PageBreak())
story.append(Paragraph("Sintesis y Conclusiones Integradas", estilo_h2))
story.append(hr())

story.append(Paragraph(
    "La siguiente tabla sintetiza la señal de cada modelo para el momento actual:",
    estilo_body
))

def estado_semaforo(condicion_sobre, condicion_sub, sig):
    if not sig:
        return "Neutro / No significativo"
    if condicion_sobre:
        return "Sobrevaluado"
    if condicion_sub:
        return "Subvaluado"
    return "Neutro"

sig_m2_chile = resultados_capm.get('Chile', {}).get('sig', False)
dir_m2_chile = resultados_capm.get('Chile', {}).get('direction', 'N/D')

data_conclu = [
    ['Modelo', 'Señal actual', 'Significancia', 'Observacion clave'],
    [
        'M1: Fundamentales',
        f'IPSA {dir1} fundamental',
        'SI' if sig_rec1 else 'NO',
        f't = {t_rec1:+.2f} | Residuo prom 12m: {resid_prom:+.4f}'
    ],
    [
        'M2: CAPM Regional',
        dir_m2_chile if 'Chile' in resultados_capm else 'N/D',
        'SI' if sig_m2_chile else 'NO',
        f"t = {resultados_capm.get('Chile', {}).get('t_last', 0):+.2f}" if 'Chile' in resultados_capm else ''
    ],
    [
        'M3: Fed Model',
        'Acciones caras vs bonos' if ult > mu+std else ('Acciones baratas vs bonos' if ult < mu-std else 'Valoracion neutra'),
        'SI' if (ult > mu+std or ult < mu-std) else 'NO',
        f'Spread actual: {ult:.2f}% | Media: {mu:.2f}%'
    ],
]

col_conclu = [4.5*cm, 3.8*cm, 2.5*cm, ancho_util - 10.8*cm]
tc = Table(data_conclu, colWidths=col_conclu)
tc_style = TableStyle([
    ('BACKGROUND',    (0,0), (-1,0), AZUL),
    ('TEXTCOLOR',     (0,0), (-1,0), colors.white),
    ('FONTNAME',      (0,0), (-1,0), 'Helvetica-Bold'),
    ('FONTSIZE',      (0,0), (-1,-1), 9),
    ('ROWBACKGROUNDS',(0,1), (-1,-1), [colors.white, GRIS_CLARO]),
    ('ALIGN',         (2,0), (2,-1), 'CENTER'),
    ('TOPPADDING',    (0,0), (-1,-1), 5),
    ('BOTTOMPADDING', (0,0), (-1,-1), 5),
    ('GRID',          (0,0), (-1,-1), 0.4, colors.lightgrey),
    ('FONTNAME',      (0,1), (0,-1), 'Helvetica-Bold'),
])
# colorear señal
for i in range(1, 4):
    txt = data_conclu[i][1]
    if 'SOBRE' in txt.upper() or 'caro' in txt.lower() or 'Sobrev' in txt:
        tc_style.add('TEXTCOLOR', (1, i), (1, i), ROJO)
        tc_style.add('FONTNAME',  (1, i), (1, i), 'Helvetica-Bold')
    elif 'BAJO' in txt.upper() or 'barat' in txt.lower() or 'Subv' in txt:
        tc_style.add('TEXTCOLOR', (1, i), (1, i), VERDE)
        tc_style.add('FONTNAME',  (1, i), (1, i), 'Helvetica-Bold')
    else:
        tc_style.add('TEXTCOLOR', (1, i), (1, i), NARANJA)
tc.setStyle(tc_style)
story.append(tc)
story.append(Spacer(1, 0.4*cm))

story.append(Paragraph("Consideraciones Finales", estilo_h3))
story.append(Paragraph(
    "Es importante recordar que ningun modelo por si solo determina de manera definitiva el "
    "nivel correcto de un mercado. Los tres enfoques presentados son complementarios y cada uno "
    "captura una dimension distinta del valor: los fundamentales macroeconomicos (M1), "
    "el posicionamiento relativo frente al ciclo global emergente (M2) y el atractivo relativo "
    "frente a la renta fija real (M3). La convergencia o divergencia de sus señales otorga "
    "mayor o menor robustez a las conclusiones.",
    estilo_body
))
story.append(Paragraph(
    ".",
    ParagraphStyle('disclaimer', fontSize=9, fontName='Helvetica-Oblique',
                   textColor=colors.grey, alignment=TA_JUSTIFY,
                   spaceBefore=10, spaceAfter=4, leading=13)
))

# ── PIE DE PÁGINA (número de página via canvasmaker) ──────────
def pie_pagina(canvas_obj, doc_obj):
    canvas_obj.saveState()
    canvas_obj.setFont('Helvetica', 8)
    canvas_obj.setFillColor(colors.grey)
    canvas_obj.drawCentredString(A4[0]/2, 1.2*cm,
        f'Informe de Valoracion Bursatil  —  Pagina {doc_obj.page}  —  {fecha_hoy}')
    canvas_obj.restoreState()

doc.build(story, onFirstPage=pie_pagina, onLaterPages=pie_pagina)
print(f"\nInforme PDF generado: {PDF_SALIDA}")
print("=" * 55)
print("FIN — Se generaron 3 graficos + 1 informe PDF")
print("=" * 55)
