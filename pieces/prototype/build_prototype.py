"""Generate print-shop-ready PDFs for an ORGANISM prototype from the current-version print masters.
Every master is authored at ~300 DPI, so each PDF has the TRUE physical size baked in -> tell the
printer "print at 100% / actual size / no scaling" and it comes out right.
  ../.venv/bin/python pieces/prototype/build_prototype.py
Source masters: ~/Downloads/organism/prototype/  (== the 'current version' zip; identical md5s)
"""
import os, glob, shutil
from PIL import Image
Image.MAX_IMAGE_PIXELS=None
HOME=os.path.expanduser("~"); SRC=f"{HOME}/Downloads/organism/prototype"
OUT=os.path.dirname(os.path.abspath(__file__))+"/print_ready"
MNT=f"{OUT}/01_MOUNT_chipboard"; CARD=f"{OUT}/02_CARDSTOCK"; BOX=f"{OUT}/03_BOX"; RULE=f"{OUT}/04_RULEBOOK"

def flat(im, bg=(255,255,255)):
    if im.mode=="RGBA":
        b=Image.new("RGB",im.size,bg); b.paste(im,mask=im.split()[3]); return b
    return im.convert("RGB")
def to_pdf(src, dst, size_mm, note=""):
    im=flat(Image.open(src)); w,h=im.size
    dpi=w/(size_mm/25.4)                                    # DPI that makes the long-ish edge == size_mm
    im.save(dst,"PDF",resolution=dpi)
    print(f"  {os.path.basename(dst):40s} {w}x{h}px -> {size_mm:.0f}mm ({size_mm/25.4:.1f}in) @ {dpi:.0f}dpi {note}")

print("== MOUNT ON CHIPBOARD ==")
# main board (two shape options); full for large-format
def tile_board(src, tag):                                       # full (large-format) + 4 quadrant tiles (== the 4 quad-fold panels) for 11x17
    to_pdf(src, f"{MNT}/main_board_{tag}_540mm_FULL.pdf", 540, "(large-format single sheet)")
    board=flat(Image.open(src)); W,H=board.size; half=W//2; OV=96; DPI_B=W/(540/25.4)   # ~8mm overlap to trim
    d=f"{MNT}/main_board_{tag}_tiles_11x17"; os.makedirs(d,exist_ok=True)
    tiles={"1_TL":(0,0,half+OV,half+OV),"2_TR":(half-OV,0,W,half+OV),"3_BL":(0,half-OV,half+OV,H),"4_BR":(half-OV,half-OV,W,H)}
    for nm,(l,t,r,b) in tiles.items():
        board.crop((l,t,r,b)).save(f"{d}/tile_{nm}.pdf","PDF",resolution=DPI_B)
    print(f"  main_board_{tag}_540mm_FULL.pdf + _{tag}_tiles_11x17/ (4x ~{(half+OV)/DPI_B*25.4:.0f}mm quadrants @ {DPI_B:.0f}dpi)")
tile_board(f"{SRC}/27_Pent_54cm_01.png","PENT")                 # BOTH boards are components -> print both
tile_board(f"{SRC}/27_HEX_54cm_01.png","HEX")
to_pdf(f"{SRC}/ScoreCard_01_print.png", f"{MNT}/power_score_board_165mm.pdf", 165, "(real size from build_setup_scene)")

print("== CARDSTOCK ==")
to_pdf(f"{SRC}/25_PLAYER-AID_09_print.png", f"{CARD}/player_aid_190mm_print5.pdf", 190, "(real size 190mm/7.5in from build_setup_scene; print x5)")
for sheet in ["Green_Red","Purple_Blue","Yellow_Dark"]:
    to_pdf(f"{SRC}/{sheet}-01.png", f"{CARD}/tokens_{sheet}_11x17.pdf", 279, "(Ø37mm tokens; punch out)")
# mutation cards -> one multipage PDF (13 pages x 2 ROUND cards). 386 dpi => each card ~Ø92mm (matches build_setup_scene)
muts=sorted(glob.glob(f"{SRC}/mutations/MutationCard*.png"))
mim=[flat(Image.open(m)) for m in muts]
mim[0].save(f"{CARD}/mutation_cards_26_round92mm.pdf","PDF",resolution=386,save_all=True,append_images=mim[1:])
print(f"  mutation_cards_26_round92mm.pdf          {len(muts)} pages x2 = {len(muts)*2} ROUND cards ~Ø92mm @386dpi")

print("== BOX ==")
to_pdf(f"{SRC}/Packaging_Print_01_300dpi.png", f"{BOX}/box_wrap_457mm.pdf", 457, "(full net; wrap on box)")
shutil.copy(f"{SRC}/Packaging_Print_01.pdf", f"{BOX}/box_wrap_VECTOR.pdf")
shutil.copy(f"{SRC}/TOP_Print.pdf", f"{BOX}/box_top_VECTOR.pdf")
print("  box_wrap_VECTOR.pdf, box_top_VECTOR.pdf  (original vector PDFs — prefer these if the shop takes them)")

print("== RULEBOOK ==")
shutil.copy(f"{SRC}/02_Manual_18.pdf", f"{RULE}/rulebook_18pp.pdf")
print("  rulebook_18pp.pdf                        (print as saddle-stitch booklet)")
print("\nDONE ->", OUT)
