"""Alternate PRINT layout for Office Depot POSTER sizes — pack the ORGANISM art onto posters (everything
is circles/sheets you cut out). Two paper types:
  ADHESIVE posters -> the stuff that mounts on chipboard (both board faces, power board, token sheets)
  STANDARD posters -> hand cards you cut out (player aids, mutation cards)
The Ø540mm boards ONLY fit 24x36, so that's the size. Each piece gets a light cut-guide + a corner label.
  ../.venv/bin/python pieces/prototype-poster/build_posters.py
"""
import os, glob, gc
import numpy as np
from scipy import ndimage
from PIL import Image, ImageDraw, ImageOps
Image.MAX_IMAGE_PIXELS=None
HOME=os.path.expanduser("~"); SRC=f"{HOME}/Downloads/organism/prototype"
OUT=os.path.dirname(os.path.abspath(__file__))+"/out"; os.makedirs(OUT,exist_ok=True)
DPI=300
def PX(mm): return int(round(mm/25.4*DPI))
def flat(im):
    im=im.convert("RGBA"); bg=Image.new("RGBA",im.size,(255,255,255,255)); bg.alpha_composite(im); return bg.convert("RGB")
_C={}
def art(src):
    if src not in _C: _C[src]=flat(Image.open(f"{SRC}/{src}"))
    return _C[src]
def mut_circles(rel):                                           # LIFT each round card cleanly out of the sheet: detect its circle, crop centred on it, white outside. No square-clipping, no stray neighbour bits.
    im=art(rel); g=np.asarray(im).mean(axis=2); dark=g<215      # dark card content on a white sheet
    lbl,n=ndimage.label(dark)
    if n==0: return [im]
    sizes=ndimage.sum(dark,lbl,range(1,n+1)); big=[i+1 for i in np.argsort(sizes)[::-1][:2]]   # the 2 largest blobs = the 2 cards (ignores tiny stray dots)
    PAD=120; imp=ImageOps.expand(im,border=PAD,fill=(255,255,255)); out=[]
    for idx in big:
        ys,xs=np.where(lbl==idx); cx,cy=(int(xs.min())+int(xs.max()))/2,(int(ys.min())+int(ys.max()))/2
        r=max(xs.max()-xs.min(),ys.max()-ys.min())/2*1.03; L=int(r)                            # square that FULLY contains the circle, centred
        c=imp.crop((int(cx-L)+PAD,int(cy-L)+PAD,int(cx+L)+PAD,int(cy+L)+PAD)).convert("RGB")
        mk=Image.new("L",c.size,0); ImageDraw.Draw(mk).ellipse([0,0,c.size[0]-1,c.size[1]-1],fill=255)
        out.append((cx, Image.composite(c,Image.new("RGB",c.size,(255,255,255)),mk)))           # keep the disc, white in the corners
    return [img for _,img in sorted(out,key=lambda t:t[0])]                                     # left-to-right

# ---------------- item lists ----------------
def circ(label,d,src,**k): return dict(label=label,w=d,h=d,src=src,circle=True,**k)
def rect(label,w,h,src,**k): return dict(label=label,w=w,h=h,src=src,circle=False,**k)
MOUNT=[circ("PENT board (side A)",540,"27_Pent_54cm_01.png"),
       circ("HEX board (side B)",540,"27_HEX_54cm_01.png"),
       circ("power / score board",165,"ScoreCard_01_print.png")]
for nm,f in [("tokens Green/Red","Green_Red-01.png"),("tokens Purple/Blue","Purple_Blue-01.png"),("tokens Yellow/Dark","Yellow_Dark-01.png")]:
    MOUNT.append(rect(nm,432,279,f,rot=True))                   # rotate the 11x17 token sheet to landscape so it tucks under a board
CARD=[circ(f"player aid {i+1}",190,"25_PLAYER-AID_09_print.png") for i in range(5)]
for f in sorted(glob.glob(f"{SRC}/mutations/MutationCard*.png")):
    for img in mut_circles("mutations/"+os.path.basename(f)):
        CARD.append(dict(label="mutation",w=92,h=92,circle=True,mutimg=img))

# ---------------- first-fit shelf packer (fills the strips UNDER the boards; tries every open poster before a new one) ----------------
def pack(items,Wmm,Hmm,margin=5,gap=4):
    items=sorted(items,key=lambda it:-it['h']); posters=[]
    def place(P,it):
        w,h=it['w'],it['h']
        for s in P['sh']:                                       # existing shelf on this poster
            if s['x']+w<=Wmm-margin and h<=s['h']:
                P['put'].append((it,s['x'],s['y'])); s['x']+=w+gap; return True
        ny=(P['sh'][-1]['y']+P['sh'][-1]['h']+gap) if P['sh'] else margin   # new shelf
        if ny+h<=Hmm-margin:
            P['sh'].append({'y':ny,'h':h,'x':margin+w+gap}); P['put'].append((it,margin,ny)); return True
        return False
    for it in items:
        if not any(place(P,it) for P in posters):
            P={'sh':[],'put':[]}; posters.append(P); place(P,it)
    return [P['put'] for P in posters]

# ---------------- render one poster ----------------
def render(placements,Wmm,Hmm,path):
    W,H=PX(Wmm),PX(Hmm); cv=Image.new("RGB",(W,H),(255,255,255)); dr=ImageDraw.Draw(cv)
    for it,xmm,ymm in placements:
        x,y,w,h=PX(xmm),PX(ymm),PX(it['w']),PX(it['h'])
        im=it['mutimg'] if it.get('mutimg') else art(it['src'])
        if it.get('rot'): im=im.rotate(90,expand=True)
        cv.paste(im.resize((w,h)),(x,y))
        if it['circle']: dr.ellipse([x,y,x+w-1,y+h-1],outline=(140,140,140),width=2)
        else: dr.rectangle([x,y,x+w-1,y+h-1],outline=(140,140,140),width=2)
        dr.text((x+4,y+4),it['label'],fill=(90,90,90))
    cv.save(path,"PDF",resolution=DPI)
    print(f"  {os.path.basename(path):26s} {Wmm/25.4:.0f}x{Hmm/25.4:.0f}in  {len(placements)} pieces")
    del cv,dr; gc.collect()

Wmm,Hmm=610,914                                                 # 24x36 in
print("=== ADHESIVE 24x36 (mount on chipboard: boards + power + tokens) ===")
for i,pl in enumerate(pack(MOUNT,Wmm,Hmm)): render(pl,Wmm,Hmm,f"{OUT}/ADHESIVE_24x36_{i+1}.pdf")
print("=== STANDARD 24x36 (cut-out hand cards: aids + mutations) ===")
for i,pl in enumerate(pack(CARD,Wmm,Hmm)): render(pl,Wmm,Hmm,f"{OUT}/STANDARD_24x36_{i+1}.pdf")
print("DONE ->",OUT)
