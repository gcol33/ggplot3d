from pathlib import Path
import csv
import subprocess
from concurrent.futures import ThreadPoolExecutor
import numpy as np
from PIL import Image, ImageDraw

root = Path(__file__).resolve().parent
before, after = root / 'before', root / 'after'

def rasterise(pdf):
    png = pdf.with_suffix('.pdf.png')
    subprocess.run(['pdftoppm', '-singlefile', '-scale-to', '1000', '-png', str(pdf), str(pdf.with_suffix('.pdf'))], capture_output=True, check=True)
    return png

pairs = [(p, after / p.name) for p in before.glob('*.pdf') if (after / p.name).exists()]
with ThreadPoolExecutor(max_workers=4) as pool:
    list(pool.map(rasterise, [p for pair in pairs for p in pair]))
rows = []
for a in before.glob('*.png'):
    b = after / a.name
    if not b.exists():
        continue
    x, y = np.asarray(Image.open(a).convert('RGB')), np.asarray(Image.open(b).convert('RGB'))
    if x.shape != y.shape:
        continue
    d = np.abs(x.astype(float) - y.astype(float))
    rows.append(dict(file=a.name, mean_difference=d.mean(), changed_fraction=(d.max(axis=2)>0).mean(),
                     large_fraction=(d.max(axis=2)>32).mean(), maximum=d.max()))
rows.sort(key=lambda x: -x['mean_difference'])
with (root / 'image-comparison.csv').open('w', newline='') as f:
    writer = csv.DictWriter(f, fieldnames=rows[0].keys())
    writer.writeheader(); writer.writerows(rows)
print('Compared', len(rows), 'images; identical', sum(r['maximum']==0 for r in rows))
for row in rows[:15]:
    print(row)
for group in ('gallery', 'grid'):
    selected = [r for r in rows if r['file'].startswith(group)][:6]
    sheet = Image.new('RGB', (1200, len(selected)*300), 'white')
    draw = ImageDraw.Draw(sheet)
    for i,r in enumerate(selected):
        a = Image.open(before / r['file']).convert('RGB'); b = Image.open(after / r['file']).convert('RGB')
        a.thumbnail((570, 280)); b.thumbnail((570, 280))
        sheet.paste(a, (0, i*300+20)); sheet.paste(b, (600, i*300+20))
        draw.text((0,i*300), r['file']+' before', fill='black')
        draw.text((600,i*300), r['file']+' after', fill='black')
    sheet.save(root / (group+'-comparison.png'))
