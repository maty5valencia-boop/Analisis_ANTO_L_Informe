"""
╔══════════════════════════════════════════════════════════════════╗
║   ANÁLISIS TÉCNICO — 5 ACTIVOS FINANCIEROS                      ║
║   Medias Móviles (SMA10, SMA20, SMA50) + RSI (14 períodos)      ║
║   Horizonte: próximos 7 días                                     ║
║   Requiere: pip install yfinance pandas numpy matplotlib reportlab║
╚══════════════════════════════════════════════════════════════════╝
"""

import numpy as np
import pandas as pd
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.gridspec import GridSpec
from datetime import datetime, timedelta
import warnings, os, pickle
warnings.filterwarnings('ignore')

import yfinance as yf

from reportlab.lib.pagesizes import A4
from reportlab.lib import colors
from reportlab.lib.units import cm
from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
from reportlab.lib.enums import TA_CENTER, TA_LEFT
from reportlab.platypus import (
    SimpleDocTemplate, Paragraph, Spacer, Image, Table, TableStyle,
    HRFlowable, PageBreak
)
from reportlab.lib.colors import HexColor, white

# ─────────────────────────────────────────────
# CONFIGURACIÓN
# ─────────────────────────────────────────────
TICKERS = {
    "AAPL":    {"nombre": "Apple Inc.",   "tipo": "Acción",        "moneda": "USD"},
    "BTC-USD": {"nombre": "Bitcoin",      "tipo": "Criptomoneda",  "moneda": "USD"},
    "GC=F":    {"nombre": "Gold Futures", "tipo": "Materia Prima", "moneda": "USD"},
    "EURUSD=X":{"nombre": "Euro / USD",   "tipo": "Divisa",        "moneda": "USD"},
    "TSLA":    {"nombre": "Tesla Inc.",   "tipo": "Acción",        "moneda": "USD"},
}

END_DATE   = datetime.today()
START_DATE = END_DATE - timedelta(days=180)   # ~6 meses de historia
OUTPUT_DIR = "."                               # carpeta de salida

# ─────────────────────────────────────────────
# 1. DESCARGA DE DATOS VIA YFINANCE
# ─────────────────────────────────────────────
print("=" * 60)
print("  Descargando datos via yfinance...")
print("=" * 60)

assets = {}
for ticker, meta in TICKERS.items():
    print(f"  → {ticker} ({meta['nombre']})...", end=" ")
    try:
        raw = yf.download(ticker, start=START_DATE, end=END_DATE,
                          auto_adjust=True, progress=False)
        if raw.empty:
            print("SIN DATOS")
            continue
        df = raw[["Close"]].copy()
        # yfinance reciente puede devolver MultiIndex en columnas; lo aplanamos
        if isinstance(df.columns, pd.MultiIndex):
            df.columns = df.columns.get_level_values(0)
        df["Close"] = df["Close"].squeeze()
        df.dropna(inplace=True)
        meta["df"] = df
        assets[ticker] = meta
        print(f"OK — {len(df)} sesiones")
    except Exception as e:
        print(f"ERROR: {e}")

print()

# ─────────────────────────────────────────────
# 2. INDICADORES TÉCNICOS
# ─────────────────────────────────────────────
def calc_indicators(df):
    df = df.copy()
    df["SMA10"] = df["Close"].rolling(10).mean()
    df["SMA20"] = df["Close"].rolling(20).mean()
    df["SMA50"] = df["Close"].rolling(50).mean()

    # RSI Wilder
    delta     = df["Close"].diff()
    gain      = delta.clip(lower=0)
    loss      = (-delta).clip(lower=0)
    avg_gain  = gain.ewm(com=13, adjust=False).mean()
    avg_loss  = loss.ewm(com=13, adjust=False).mean()
    rs        = avg_gain / avg_loss.replace(0, np.nan)
    df["RSI"] = 100 - (100 / (1 + rs))

    # Señales de cruce SMA10/SMA20
    df["Signal"]    = np.where(df["SMA10"] > df["SMA20"], 1, -1)
    df["CrossUp"]   = (df["Signal"] == 1) & (df["Signal"].shift(1) == -1)
    df["CrossDown"] = (df["Signal"] == -1) & (df["Signal"].shift(1) == 1)
    return df

for ticker, info in assets.items():
    info["df"] = calc_indicators(info["df"])

# ─────────────────────────────────────────────
# 3. RECOMENDACIÓN COMPUESTA
# ─────────────────────────────────────────────
def get_recommendation(df):
    last  = df.iloc[-1]
    def _scalar(x):
        if hasattr(x, 'iloc'): return float(x.iloc[0])
        if hasattr(x, 'item'): return float(x.item())
        return float(x)
    rsi   = _scalar(last["RSI"])
    sma10 = _scalar(last["SMA10"])
    sma20 = _scalar(last["SMA20"])
    sma50 = _scalar(last["SMA50"])
    close = _scalar(last["Close"])
    score = 0
    reasons = []

    recent = df.tail(3)
    if recent["CrossUp"].any():
        score += 2;  reasons.append("Cruce alcista SMA10/SMA20 reciente")
    if recent["CrossDown"].any():
        score -= 2;  reasons.append("Cruce bajista SMA10/SMA20 reciente")

    if pd.notna(sma50):
        if sma10 > sma20 > sma50:
            score += 2;  reasons.append("Alineación alcista SMA10>SMA20>SMA50")
        elif sma10 < sma20 < sma50:
            score -= 2;  reasons.append("Alineación bajista SMA10<SMA20<SMA50")

    if close > sma20:
        score += 1;  reasons.append("Precio sobre SMA20")
    else:
        score -= 1;  reasons.append("Precio bajo SMA20")

    if rsi < 30:
        score += 3;  reasons.append(f"RSI sobrevendido ({rsi:.1f})")
    elif rsi > 70:
        score -= 3;  reasons.append(f"RSI sobrecomprado ({rsi:.1f})")
    elif rsi > 60:
        score += 1;  reasons.append(f"RSI momentum alcista ({rsi:.1f})")
    elif rsi < 40:
        score -= 1;  reasons.append(f"RSI momentum bajista ({rsi:.1f})")

    if score >= 3:
        rec, color = "COMPRA",  "#27ae60"
    elif score <= -3:
        rec, color = "VENTA",   "#e74c3c"
    else:
        rec, color = "NEUTRAL", "#f39c12"

    return rec, color, score, reasons, rsi, sma10, sma20

for ticker, info in assets.items():
    rec, color, score, reasons, rsi_val, sma10_val, sma20_val = get_recommendation(info["df"])
    info.update({"rec": rec, "color": color, "score": score,
                 "reasons": reasons, "rsi_val": rsi_val,
                 "sma10_val": sma10_val, "sma20_val": sma20_val})

# ─────────────────────────────────────────────
# 4. GRÁFICOS
# ─────────────────────────────────────────────
PALETA = {
    "price": "#2c3e50", "sma10": "#3498db", "sma20": "#e67e22",
    "sma50": "#8e44ad", "buy":   "#27ae60", "sell":  "#e74c3c",
    "rsi":   "#2980b9", "bg":    "#f8f9fa", "grid":  "#dfe6e9",
}

chart_files = []

for ticker, info in assets.items():
    df  = info["df"].copy()
    # Usar sólo las últimas 65 sesiones para el gráfico
    df  = df.tail(65)
    rec = info["rec"]
    rc  = info["color"]

    fig = plt.figure(figsize=(14, 9), facecolor="white")
    gs  = GridSpec(3, 1, figure=fig, height_ratios=[3, 1.2, 1.2], hspace=0.08)

    # Panel 1 — Precio + MAs
    ax1 = fig.add_subplot(gs[0])
    ax1.set_facecolor(PALETA["bg"])
    ax1.plot(df.index, df["Close"], color=PALETA["price"], lw=1.8, label="Precio", zorder=3)
    ax1.plot(df.index, df["SMA10"], color=PALETA["sma10"], lw=1.4, ls="--",  label="SMA 10", zorder=2)
    ax1.plot(df.index, df["SMA20"], color=PALETA["sma20"], lw=1.4, ls="-.",  label="SMA 20", zorder=2)
    ax1.plot(df.index, df["SMA50"], color=PALETA["sma50"], lw=1.4, ls=":",   label="SMA 50", zorder=2)

    buy_sig  = df[df["CrossUp"]]
    sell_sig = df[df["CrossDown"]]
    ax1.scatter(buy_sig.index,  buy_sig["Close"],  marker="^", color=PALETA["buy"],  s=120, zorder=5, label="Señal Compra")
    ax1.scatter(sell_sig.index, sell_sig["Close"], marker="v", color=PALETA["sell"], s=120, zorder=5, label="Señal Venta")
    close_min = float(df["Close"].values.min()) * 0.995
    close_max = float(df["Close"].values.max()) * 1.005
    ax1.axhspan(close_min, close_max, alpha=0.04, color=rc)

    ax1.set_title(f"{info['nombre']}  ({ticker})  |  {info['tipo']}",
                  fontsize=14, fontweight="bold", pad=10)
    ax1.set_ylabel(f"Precio ({info['moneda']})", fontsize=10)
    ax1.legend(loc="upper left", fontsize=9, framealpha=0.9)
    ax1.grid(color=PALETA["grid"], linewidth=0.6)
    ax1.set_xlim(df.index[0], df.index[-1])
    ax1.tick_params(labelbottom=False)
    ax1.text(0.99, 0.97, f"  {rec}  ", transform=ax1.transAxes,
             fontsize=13, fontweight="bold", va="top", ha="right", color="white",
             bbox=dict(facecolor=rc, edgecolor="none", boxstyle="round,pad=0.4", alpha=0.9))

    # Panel 2 — RSI
    ax2 = fig.add_subplot(gs[1], sharex=ax1)
    ax2.set_facecolor(PALETA["bg"])
    ax2.plot(df.index, df["RSI"], color=PALETA["rsi"], lw=1.6)
    ax2.axhline(70, color=PALETA["sell"], lw=1.0, ls="--", alpha=0.8)
    ax2.axhline(30, color=PALETA["buy"],  lw=1.0, ls="--", alpha=0.8)
    ax2.axhline(50, color="#95a5a6",      lw=0.8, ls=":")
    ax2.fill_between(df.index, df["RSI"], 70, where=(df["RSI"]>=70), alpha=0.25, color=PALETA["sell"])
    ax2.fill_between(df.index, df["RSI"], 30, where=(df["RSI"]<=30), alpha=0.25, color=PALETA["buy"])
    ax2.set_ylim(0, 100)
    ax2.set_ylabel("RSI (14)", fontsize=10)
    ax2.text(df.index[1], 72, "Sobrecomprado (70)", fontsize=8, color=PALETA["sell"], alpha=0.9)
    ax2.text(df.index[1], 22, "Sobrevendido (30)",  fontsize=8, color=PALETA["buy"],  alpha=0.9)
    ax2.grid(color=PALETA["grid"], linewidth=0.6)
    ax2.tick_params(labelbottom=False)

    # Panel 3 — Spread SMA10−SMA20
    ax3 = fig.add_subplot(gs[2], sharex=ax1)
    ax3.set_facecolor(PALETA["bg"])
    spread = df["SMA10"] - df["SMA20"]
    ax3.bar(df.index, spread,
            color=[PALETA["buy"] if v >= 0 else PALETA["sell"] for v in spread],
            alpha=0.75, width=0.8)
    ax3.axhline(0, color="#2c3e50", lw=0.8)
    ax3.set_ylabel("SMA10 − SMA20", fontsize=9)
    ax3.grid(color=PALETA["grid"], linewidth=0.6, axis="y")

    fig.autofmt_xdate(rotation=30, ha="right")
    plt.setp(ax3.get_xticklabels(), fontsize=8)

    fname = os.path.join(OUTPUT_DIR, f"{ticker.replace('=','_').replace('-','_')}_chart.png")
    plt.savefig(fname, dpi=140, bbox_inches="tight", facecolor="white")
    plt.close()
    chart_files.append((ticker, fname))
    print(f"  ✓ Gráfico: {fname}")

# ─────────────────────────────────────────────
# 5. TABLA RESUMEN CONSOLA
# ─────────────────────────────────────────────
print("\n" + "="*95)
print(f"{'TICKER':<12} {'NOMBRE':<18} {'P.INICIAL':>11} {'P.FINAL':>11} "
      f"{'SMA10':>11} {'SMA20':>11} {'RSI':>7} {'RENT%':>8}  {'RECOM.'}")
print("="*95)
summary_data = []
for ticker, info in assets.items():
    df = info["df"]
    p0 = float(df["Close"].iloc[0].item() if hasattr(df["Close"].iloc[0], "item") else df["Close"].iloc[0])
    pf = float(df["Close"].iloc[-1].item() if hasattr(df["Close"].iloc[-1], "item") else df["Close"].iloc[-1])
    rent = (pf/p0 - 1)*100
    row = dict(Ticker=ticker, Nombre=info["nombre"], Tipo=info["tipo"],
               P_Inicial=round(p0,4), P_Final=round(pf,4),
               SMA10=round(float(info["sma10_val"]),4),
               SMA20=round(float(info["sma20_val"]),4),
               RSI=round(float(info["rsi_val"]),1),
               Rent_pct=round(rent,2), Rec=info["rec"], Score=info["score"])
    summary_data.append(row)
    print(f"{ticker:<12} {info['nombre']:<18} {p0:>11.4f} {pf:>11.4f} "
          f"{float(info['sma10_val']):>11.4f} {float(info['sma20_val']):>11.4f} "
          f"{float(info['rsi_val']):>7.1f} {rent:>7.2f}%  {info['rec']}")
print("="*95)

# ─────────────────────────────────────────────
# 6. GENERACIÓN DEL PDF
# ─────────────────────────────────────────────
C_DARK   = HexColor("#1a252f");  C_BLUE   = HexColor("#2980b9")
C_GREEN  = HexColor("#27ae60");  C_RED    = HexColor("#e74c3c")
C_ORANGE = HexColor("#f39c12");  C_LGRAY  = HexColor("#ecf0f1")
C_MGRAY  = HexColor("#bdc3c7");  C_HEADER = HexColor("#2c3e50")
REC_COLORS = {"COMPRA": C_GREEN, "VENTA": C_RED, "NEUTRAL": C_ORANGE}

styles = getSampleStyleSheet()
def sty(base, **kw):
    b = styles.get(base, styles["Normal"])
    return ParagraphStyle(base+"_c", parent=b, **kw)

h1    = sty("Heading1", fontSize=14, fontName="Helvetica-Bold",
            textColor=C_HEADER, spaceBefore=12, spaceAfter=6)
h2    = sty("Heading2", fontSize=11, fontName="Helvetica-Bold",
            textColor=C_BLUE,   spaceBefore=8,  spaceAfter=4)
body  = sty("Normal",   fontSize=9.5, fontName="Helvetica",
            textColor=C_DARK,  spaceAfter=4, leading=13)
bul   = sty("Normal",   fontSize=9,   fontName="Helvetica",
            textColor=C_DARK,  spaceAfter=3, leftIndent=14, leading=12)
cap   = sty("Normal",   fontSize=8,   fontName="Helvetica-Oblique",
            textColor=HexColor("#7f8c8d"), alignment=TA_CENTER, spaceAfter=6)
rec_h = sty("Normal",   fontSize=13,  fontName="Helvetica-Bold",
            textColor=white, alignment=TA_CENTER)

W, H   = A4
CW     = W - 4*cm
output = os.path.join(OUTPUT_DIR, "analisis_tecnico_yfinance.pdf")
doc    = SimpleDocTemplate(output, pagesize=A4,
                           leftMargin=2*cm, rightMargin=2*cm,
                           topMargin=2*cm,  bottomMargin=2*cm,
                           title="Análisis Técnico — 5 Activos Financieros")

story = []

# Portada
def cover(canvas, doc):
    canvas.saveState()
    canvas.setFillColor(C_HEADER);  canvas.rect(0,0,W,H,fill=1,stroke=0)
    canvas.setFillColor(C_BLUE);    canvas.rect(0,H*0.38,W,H*0.28,fill=1,stroke=0)
    canvas.setFillColor(C_GREEN);   canvas.rect(0,H*0.36,W,4,fill=1,stroke=0)
    canvas.setFillColor(C_ORANGE);  canvas.rect(0,H*0.38+H*0.28,W,4,fill=1,stroke=0)
    canvas.setFillColor(white)
    canvas.setFont("Helvetica-Bold", 28)
    canvas.drawCentredString(W/2, H*0.60, "ANÁLISIS TÉCNICO")
    canvas.setFont("Helvetica-Bold", 20)
    canvas.drawCentredString(W/2, H*0.555, "5 Activos Financieros")
    canvas.setFont("Helvetica", 12)
    canvas.setFillColor(HexColor("#bdc3c7"))
    canvas.drawCentredString(W/2, H*0.505,
        "SMA10 · SMA20 · SMA50  +  RSI (14)  |  Datos via yfinance")
    canvas.setFont("Helvetica-Bold", 11)
    canvas.setFillColor(white)
    canvas.drawCentredString(W/2, H*0.47,
        f"Análisis al {END_DATE.strftime('%d de %B de %Y')}  |  Horizonte: próximos 7 días")
    canvas.setFont("Helvetica", 9)
    canvas.setFillColor(HexColor("#7f8c8d"))
    canvas.drawCentredString(W/2, 2*cm,
        "Este informe tiene fines educativos. No constituye asesoría financiera.")
    canvas.restoreState()

def later(canvas, doc):
    canvas.saveState()
    canvas.setFont("Helvetica", 8); canvas.setFillColor(HexColor("#95a5a6"))
    canvas.drawString(2*cm, 1.2*cm,
        f"Análisis Técnico · 5 Activos · {END_DATE.strftime('%d/%m/%Y')}")
    canvas.drawRightString(W-2*cm, 1.2*cm, f"Página {doc.page}")
    canvas.setStrokeColor(C_MGRAY); canvas.setLineWidth(0.5)
    canvas.line(2*cm, 1.5*cm, W-2*cm, 1.5*cm)
    canvas.restoreState()

story.append(Spacer(1, 0.5*cm))

# ── Metodología ──
story.append(PageBreak())
story.append(Paragraph("1. Introducción y Metodología", h1))
story.append(HRFlowable(width=CW, thickness=1.5, color=C_BLUE, spaceAfter=8))
story.append(Paragraph(
    "Este informe descarga datos reales de mercado vía <b>yfinance</b> y aplica "
    "análisis técnico con medias móviles simples (SMA10, SMA20, SMA50) y el RSI de 14 períodos. "
    "La recomendación de compra/venta/neutral se calcula mediante una puntuación compuesta "
    "que integra cruces de medias, alineación de tendencia y niveles del RSI. "
    f"Datos descargados el <b>{END_DATE.strftime('%d/%m/%Y')}</b>.",
    body))
story.append(Spacer(1, 6))

ind_data = [["Indicador","Período","Descripción"],
            ["SMA 10","10 sesiones","Momentum de corto plazo / señales de cruce"],
            ["SMA 20","20 sesiones","Soporte/resistencia dinámico de medio plazo"],
            ["SMA 50","50 sesiones","Referencia de tendencia estructural"],
            ["RSI",   "14 sesiones","Sobrecompra >70 · Neutral 30-70 · Sobreventa <30"]]
it = Table(ind_data, colWidths=[4*cm, 3*cm, 9.5*cm])
it.setStyle(TableStyle([
    ("BACKGROUND",(0,0),(-1,0),C_HEADER),("TEXTCOLOR",(0,0),(-1,0),white),
    ("FONTNAME",(0,0),(-1,0),"Helvetica-Bold"),("FONTNAME",(0,1),(-1,-1),"Helvetica"),
    ("FONTSIZE",(0,0),(-1,-1),9),("ROWBACKGROUNDS",(0,1),(-1,-1),[white,C_LGRAY]),
    ("GRID",(0,0),(-1,-1),0.4,C_MGRAY),("VALIGN",(0,0),(-1,-1),"MIDDLE"),
    ("PADDING",(0,0),(-1,-1),5),("TOPPADDING",(0,0),(-1,-1),6),
]))
story.append(it)

# ── Una página por activo ──
rsi_label = lambda r: (
    "Sobrevendido — posible rebote" if r<30 else
    "Sobrecomprado — posible corrección" if r>70 else
    "Momentum alcista moderado" if r>60 else
    "Momentum bajista moderado" if r<40 else "Zona neutra"
)
rec_desc = {
    "COMPRA":  "Los indicadores técnicos apuntan a tendencia alcista. Se recomienda considerar posición larga durante la próxima semana.",
    "VENTA":   "Los indicadores reflejan presión bajista. Se recomienda posición corta o liquidar posiciones largas.",
    "NEUTRAL": "Sin señal clara. Se recomienda esperar confirmación antes de entrar al mercado.",
}

for i, row in enumerate(summary_data):
    story.append(PageBreak())
    ticker = row["Ticker"]
    info   = assets[ticker]
    rc     = REC_COLORS.get(row["Rec"], C_ORANGE)

    story.append(Paragraph(
        f"2.{i+1}  {info['nombre']}  <font color='#7f8c8d'>({ticker})</font>", h1))
    story.append(HRFlowable(width=CW, thickness=1.5, color=C_BLUE, spaceAfter=6))

    # Sub-header
    sh = [[Paragraph(f"<b>Clase:</b> {info['tipo']}", body),
           Paragraph(f"<b>Moneda:</b> {info['moneda']}", body),
           Paragraph(f"<b>Recomendación:</b>", body),
           Paragraph(f"<b> {row['Rec']} </b>",
                     ParagraphStyle("r",fontName="Helvetica-Bold",fontSize=11,
                                    textColor=white,backColor=rc,
                                    alignment=TA_CENTER,borderPadding=(3,6,3,6)))]]
    sht = Table(sh, colWidths=[4*cm,3*cm,4*cm,5*cm])
    sht.setStyle(TableStyle([("VALIGN",(0,0),(-1,-1),"MIDDLE"),
                              ("PADDING",(0,0),(-1,-1),4)]))
    story.append(sht); story.append(Spacer(1,8))

    # Gráfico
    img_path = next((f for t,f in chart_files if t==ticker), None)
    if img_path and os.path.exists(img_path):
        story.append(Image(img_path, width=CW, height=CW*0.60))
        story.append(Paragraph(
            f"Figura {i+1}. Precio, SMA10/20/50 y RSI (14) para {ticker}. "
            "Triángulos ▲ = señal compra; ▼ = señal venta. "
            "Histograma = spread SMA10−SMA20.", cap))
    story.append(Spacer(1,6))

    # Métricas
    story.append(Paragraph("Métricas del período", h2))
    rent = row["Rent_pct"]
    mdata = [
        ["Métrica","Valor","Interpretación"],
        ["Precio Inicial",f"${row['P_Inicial']:,.4f}","Primer precio del período descargado"],
        ["Precio Final",  f"${row['P_Final']:,.4f}", "Último precio de cierre disponible"],
        ["Rentabilidad",  f"{rent:+.2f}%",           "Variación % acumulada en el período"],
        ["SMA 10",        f"${row['SMA10']:,.4f}",    "Media móvil corto plazo"],
        ["SMA 20",        f"${row['SMA20']:,.4f}",    "Media móvil medio plazo"],
        ["RSI (14)",      f"{row['RSI']:.1f}",        rsi_label(row["RSI"])],
        ["Score técnico", f"{row['Score']:+d} / 8",   ">+3=COMPRA · <-3=VENTA · entre=NEUTRAL"],
    ]
    mt = Table(mdata, colWidths=[4.5*cm,3*cm,9*cm])
    mt_style = [
        ("BACKGROUND",(0,0),(-1,0),C_HEADER),("TEXTCOLOR",(0,0),(-1,0),white),
        ("FONTNAME",(0,0),(-1,0),"Helvetica-Bold"),("FONTNAME",(0,1),(-1,-1),"Helvetica"),
        ("FONTSIZE",(0,0),(-1,-1),8.5),("ROWBACKGROUNDS",(0,1),(-1,-1),[white,C_LGRAY]),
        ("GRID",(0,0),(-1,-1),0.4,C_MGRAY),("VALIGN",(0,0),(-1,-1),"MIDDLE"),
        ("PADDING",(0,0),(-1,-1),5),("TOPPADDING",(0,0),(-1,-1),6),
        ("TEXTCOLOR",(1,3),(1,3),C_GREEN if rent>=0 else C_RED),
        ("FONTNAME",(1,3),(1,3),"Helvetica-Bold"),
        ("TEXTCOLOR",(1,6),(1,6),C_RED if row["RSI"]>70 else C_GREEN if row["RSI"]<30 else C_DARK),
        ("FONTNAME",(1,6),(1,6),"Helvetica-Bold"),
    ]
    mt.setStyle(TableStyle(mt_style))
    story.append(mt); story.append(Spacer(1,8))

    story.append(Paragraph("Interpretación", h2))
    story.append(Paragraph(rec_desc[row["Rec"]], body))

# ── Resumen comparativo ──
story.append(PageBreak())
story.append(Paragraph("3. Resumen Comparativo — 5 Activos", h1))
story.append(HRFlowable(width=CW, thickness=1.5, color=C_BLUE, spaceAfter=10))
story.append(Paragraph(
    f"Consolidado de indicadores técnicos al {END_DATE.strftime('%d/%m/%Y')}. "
    "Horizonte de recomendación: próximos 7 días.", body))
story.append(Spacer(1,10))

hdr = ["Activo","Nombre","Tipo","P.Inicial","P.Final","SMA10","SMA20","RSI","Rent.%","Recom."]
tbl = [hdr]
for row in summary_data:
    tbl.append([row["Ticker"], row["Nombre"][:16], row["Tipo"][:12],
                f"${row['P_Inicial']:,.2f}", f"${row['P_Final']:,.2f}",
                f"${row['SMA10']:,.2f}",     f"${row['SMA20']:,.2f}",
                f"{row['RSI']:.1f}",         f"{row['Rent_pct']:+.2f}%",
                row["Rec"]])

cw = [1.8*cm,3.2*cm,2.6*cm,2.2*cm,2.2*cm,2.2*cm,2.2*cm,1.4*cm,1.8*cm,2.0*cm]
st = Table(tbl, colWidths=cw)
ts = [("BACKGROUND",(0,0),(-1,0),C_HEADER),("TEXTCOLOR",(0,0),(-1,0),white),
      ("FONTNAME",(0,0),(-1,0),"Helvetica-Bold"),("FONTNAME",(0,1),(-1,-1),"Helvetica"),
      ("FONTSIZE",(0,0),(-1,-1),8),("ROWBACKGROUNDS",(0,1),(-1,-1),[white,C_LGRAY]),
      ("GRID",(0,0),(-1,-1),0.4,C_MGRAY),("ALIGN",(3,0),(-1,-1),"RIGHT"),
      ("ALIGN",(0,0),(2,-1),"LEFT"),("ALIGN",(-1,0),(-1,-1),"CENTER"),
      ("VALIGN",(0,0),(-1,-1),"MIDDLE"),("PADDING",(0,0),(-1,-1),4),
      ("TOPPADDING",(0,0),(-1,-1),6)]
for idx, row in enumerate(summary_data, 1):
    rc = REC_COLORS.get(row["Rec"], C_ORANGE)
    ts += [("BACKGROUND",(-1,idx),(-1,idx),rc),
           ("TEXTCOLOR", (-1,idx),(-1,idx),white),
           ("FONTNAME",  (-1,idx),(-1,idx),"Helvetica-Bold"),
           ("TEXTCOLOR",(-2,idx),(-2,idx), C_GREEN if row["Rent_pct"]>=0 else C_RED),
           ("FONTNAME",(-2,idx),(-2,idx),"Helvetica-Bold")]
st.setStyle(TableStyle(ts))
story.append(st); story.append(Spacer(1,16))

story.append(Paragraph("4. Notas", h1))
story.append(HRFlowable(width=CW, thickness=1.5, color=C_BLUE, spaceAfter=8))
for note in [
    "Datos descargados en tiempo real vía <b>yfinance</b>. Los precios pueden diferir levemente de distintas fuentes.",
    "El análisis técnico identifica patrones históricos pero no garantiza rentabilidad futura.",
    "RSI y medias móviles son indicadores rezagados. Combínelos con análisis fundamental.",
    "Establezca niveles de stop-loss antes de ejecutar cualquier operación."
]:
    story.append(Paragraph(f"⚠  {note}", bul))

story.append(Spacer(1,12))
story.append(Paragraph(
    f"Informe generado el {datetime.now().strftime('%d/%m/%Y %H:%M')} hrs.", cap))

doc.build(story, onFirstPage=cover, onLaterPages=later)
print(f"\n✓ PDF generado: {output}")
print("  Ábrelo con cualquier visor de PDF (Acrobat, Preview, etc.)")
