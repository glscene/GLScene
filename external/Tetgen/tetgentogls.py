#!/usr/bin/python
#tetgentoglscene.py
#Small tetgen to gmsh .msh file converter
#because the .mesh file created by tetgen does not keep surface markers
#Written by frederic Renou frederic.renou@gmail.com 2009

import sys

verbose=False
if sys.argv[1] in ('h','-h','-help'):
	print '-v for verbose mode'
	print 'l argument doit etre le fichier tetgen sans le .node'
	raise 'fin du fichier'
else:
	if sys.argv[1] in ('v','-v'):
		print '-v for verbose mode'
		fname=sys.argv[2]
		verbose=True
	else:
		fname=sys.argv[1]

#ouverture des fichiers
fo=file(fname+'.msh',"w")
fn=file(fname+'.node',"r")
ff=file(fname+'.face',"r")
fe=file(fname+'.ele',"r")


#lecture du fichier .node
l=[int(x) for x in fn.readline().split()]
npoints,dim,nattrib,nbound=l
assert dim==3
if npoints < 3:
	raise "tetgen file empty"

l=fo.write("$NOD\n")
l=fo.write("%d\n"%npoints)

npointlu=0
for line in fn:
	if line[0]=="#": continue
        l=[float(x) for x in line.split()]
        npointlu=npointlu + 1
	l[0]=int(l[0])
        fo.write("%d "%l[0]+"%f "%l[1]+"%f "%l[2]+"%f\n"%l[3])
if verbose: print "nombre de point lut %d nombre de point par tetgen %d"%(npointlu,npoints)
assert npoints==npointlu



#lecture des fichiers elements
l=[int(x) for x in fe.readline().split()]
ntetra,nnod,nattribe=l
if nnod!=4:
	raise "tetgen element are not linear not supported"
#if nattribe!=0:
#	raise "tetgen did assign an entity number to each element"

l=[int(x) for x in ff.readline().split()]
nfaces,nattribf=l
if nattribf!=1:
	raise "tetgen didn't assign an entity number to each face"

#fin de l ecriture des noeud
l=fo.write("$ENDNOD\n")
l=fo.write("$ELM\n")
l=fo.write("%d\n"%(ntetra+nfaces))

#lecture du fichier face
nfacelu=0
for line in ff:
	if line[0]=="#": continue
	l=[int(x) for x in line.split()]
	nfacelu=nfacelu+1
	assert len(l)==5
	n=[     #elm-number 
                l[0],
                #elm-type 
                2,
                #reg-phys
                l[4],
                #reg-elem 
                l[4],
                #number-of-nodes
                3,
		#coord1 
                l[1],
                #coord 
                l[2],
                #coord 
                l[3]
                ]
	l=fo.write("%d %d %d %d %d %d %d %d\n"%tuple(n))
if verbose: print "nombre de face lut %d nombre de face par tetgen %d"%(nfacelu,nfaces)
assert nfaces==nfacelu

ntetralu=0
for line in fe:
	if line[0]=="#": continue
	l=[int(x) for x in line.split()]
	ntetralu=ntetralu+1
	if nattribe==1: regnumber=l[5] 
	else: regnumber=3
	assert len(l)==(nattrib+5)
	n=[     #elm-number 
                l[0],
                #elm-type 
                4,
                #reg-phys
                regnumber,
                #reg-elem 
		regnumber,
                #number-of-nodes
                4,
		#coord 
                l[1],
                #coord 
                l[2],
                #coord 
                l[3],
                #coord 
                l[4]
                ]
	l=fo.write("%d %d %d %d %d %d %d %d %d\n"%tuple(n))
if verbose: print "nombre de tetra lut %d nombre de tetra par tetgen %d"%(nfacelu,nfaces)
assert ntetra==ntetralu