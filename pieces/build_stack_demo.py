import bpy, math
from mathutils import Vector
P="/home/youdonotexist/code/organism/pieces"; OUT=f"{P}/scene"
PLATEAU={"EAT":48.0,"MOVE":60.0,"GROW":36.0}     # peg base z per piece
NFOOD={"EAT":1,"MOVE":2,"GROW":3}                 # different stack per piece
PITCH=4.76                                        # measured food-on-food stack pitch

bpy.ops.wm.read_factory_settings(use_empty=True)
sc=bpy.context.scene
try: sc.render.engine='BLENDER_EEVEE_NEXT'
except Exception: sc.render.engine='BLENDER_EEVEE'
sc.render.resolution_x=1600; sc.render.resolution_y=1000
w=bpy.data.worlds.new("W"); sc.world=w; w.use_nodes=True
w.node_tree.nodes["Background"].inputs[0].default_value=(0.6,0.61,0.64,1)
for nm,en,rot in [("S",4,(math.radians(52),math.radians(10),math.radians(35))),("S2",1.5,(math.radians(62),0,math.radians(-120)))]:
    d=bpy.data.lights.new(nm,'SUN'); d.energy=en; o=bpy.data.objects.new(nm,d); sc.collection.objects.link(o); o.rotation_euler=rot
def C(): return bpy.context.collection
def mat(name,rgb,rough=0.5):
    m=bpy.data.materials.new(name); m.use_nodes=True; b=m.node_tree.nodes["Principled BSDF"]
    b.inputs["Base Color"].default_value=(*rgb,1); b.inputs["Roughness"].default_value=rough; return m
piece_mat=mat("piece",(0.60,0.63,0.68)); food_mat=mat("food",(0.88,0.52,0.16))
# table
me=bpy.data.meshes.new("T"); o=bpy.data.objects.new("T",me); C().objects.link(o)
me.from_pydata([(-400,-400,0),(400,-400,0),(400,400,0),(-400,400,0)],[],[(0,1,2,3)]); me.update()
me.materials.append(mat("table",(0.5,0.5,0.52),0.9)); o.location=(0,0,-0.2)
def imp(path,name,m):
    bpy.ops.wm.obj_import(filepath=path, forward_axis='NEGATIVE_Z', up_axis='Y')
    obj=[x for x in bpy.context.selected_objects if x.type=='MESH'][0]; obj.name=name
    obj.data.materials.clear(); obj.data.materials.append(m)
    obj.hide_render=True; obj.location=(9000,9000,0); return obj
T={n:imp(f"{P}/{n}_connected.obj",n,piece_mat) for n in ["EAT","MOVE","GROW"]}
foodT=imp(f"{P}/FOOD.obj","FOOD",food_mat)
def place(tmpl,m,loc):
    o=tmpl.copy(); C().objects.link(o); o.hide_render=False; o.location=loc
    o.material_slots[0].link='OBJECT'; o.material_slots[0].material=m; return o
cols={"EAT":-85,"MOVE":0,"GROW":85}
for n in ["EAT","MOVE","GROW"]:
    x=cols[n]
    place(T[n],piece_mat,(x, 60,0))                          # bare (back row)
    place(T[n],piece_mat,(x,-60,0))                          # with food (front row)
    for i in range(NFOOD[n]):
        place(foodT,food_mat,(x,-60, PLATEAU[n]+i*PITCH))    # telescoping food stack
cd=bpy.data.cameras.new("C"); cd.lens=52; cd.clip_end=20000
cam=bpy.data.objects.new("C",cd); C().objects.link(cam); sc.camera=cam
cam.location=(135,-310,180); look=Vector((0,-5,40))
cam.rotation_euler=(look-Vector(cam.location)).to_track_quat('-Z','Y').to_euler()
sc.render.filepath=f"{OUT}/stack_demo.png"; bpy.ops.render.render(write_still=True)
bpy.ops.wm.save_as_mainfile(filepath=f"{OUT}/stack_demo.blend")
print("WROTE stack_demo.png + .blend")
