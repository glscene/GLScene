Z-Shadows for GLScene
----------------------
Rene Lindsay
22 October 2001

Overview
---------
This component is an extention to Eric Grange's excelent GLScene OpenGL Library,
which is based on the original GLScene library by Mike Lischke.
It allows any visible objects in the scene to cast shadows, using the Z-Buffer method.
Any visible object casts shadows onto any other objects, and even onto itself.
The shadows are NOT pre-calculated. They are dynamically calculated, in Real-time.
So, when an object is moved, or animated, its shadow moves with it.
I did NOT use GeForce extentions, so it will work on older Graphics controllers.

How to Install
---------------
This component was writen to work with the October CVS version of GLScene 0.8.3.
The component consists of 2 files:
GLzBuffer.pas 
GLzBuffer.dcr

Copy these 2 files to the GLScene\Source directory.

Open GLScene\Source\DesignTime\GLSceneRegister.pas, and make the following 2 changes.
1) Add GLzBuffer to the Uses clause at line 125.
2) Register the shadow object into the GLScene IDE, by adding the following line to the
   end of the file (line 1335)
   RegisterSceneObject(TZShadows, 'Shadows', '');
Save this file.

Open GLScene\Delphi5\GLScene.dpk, and add the unit to GLScene package.
Click the Compile button, and save.

The Shadow object will now be available in the TGLScene object menu.

How to use
-----------
The Shadow object uses a second camera as a lightsource, via a TGLMemoryViewer object.
So, you need 2 camera objects. Link the GLSceneViewer to one camera, and
link a TGLMemoryViewer to the other camera.
Add the shadow object to the end of your GLScene tree.
Link it to the TGLSceneViewer, and the TGLMemoryViewer.

At Runtime, you need to call ZShadows1.CastShadow,
to recaclulate the shadows, every time an object is moved.
(Do not refresh the MemoryViewer directly, because then the
shadow-object wont fecth the z-buffer.)

How it works
------------
The second camera, which is linked to the TGLMemoryViewer acts as the 
Lightsource Point Of view (pov).
Any object that is visible from the lightsource, is in direct light.
Anything not directly visible for the lightsource, is in shadow.

More details of how it works
----------------------------
1: When you call the shadow object's CastShadow method, the scene is rendered
   from the lightsource pov, and its z-buffer (depth buffer) is grabbed.
2: The scene is rendered from the main camera, to the point where the
   TZShadows object is placed in the scene tree.
3: At this point, the Main scene's z-buffer is grabbed.
   Every rendered pixel's world coordinate(x,y,z) is then calculated, by using
   the pixel's vector, and z-buffer depth.
4: The program then calculates at which pixel on the lightsource view this 
   world coordinate would be drawn, and its z-distance from the lightsource.
   If this z-distance matches the actual z-distance of that pixel, then
   that pixel is in direct light.
   If the coordinate's distance is greater than the rendered z-value, then
   something is obscuring that pixel, and it is in shadow.
5: Shadowed pixels on the main view, are drawn to a texture.
6: The shadow textute is overlayed onto the scene, 
   like a semi-transparent HUD-Sprite.

Properties and Methods
----------------------
Method: 
 CastShadow:	VERY INPORTANT! 
		Used to grab the z-buffer from the lightsource pov.
		Call this every time any object(besides the main camera) moves.
Properties
 Caster:	Specifies Lightsource TGLMemoryView object
 Viewer:	Specifies Main View TGLSceneViewer object
 Soft:		Gives shadows soft edges, by sampling 4 pixels on the lightsource z-buffer
 Optimise:	Speeds up the shadow-map generation, by skipping pixels when testing for
		shadows. It ocasionally makes mistakes jumps over very thin shadows, or light beams.
		In places where shadow edges are detected, it falls back 
		to testing every pixel. In theory, "op16in1" is the fastest.
 XRes, YRes:	Specify the resolution of the shadow map.
		Smaller sizes run faster, but cause edges to look blocky
 Tolerance:	Specify the tolerance between the z-value and lightsource distance.
		Too small values cause ugly dot patterns, caused when surfaces 
		casts shadows onto itself, due to small inaduracies in calculations.
		Too large values cause light to shine too far through a surface.
 DepthFade:	Causes the brightness of the lightsource to fade with distance.
		For now, this uses linear fading, just like the fog-algorythm.
		A Spherical mapping will be added in a future version, which
		is more realistic, but also slower.
 FrustShadow:	Specify if the area outside the lightsource view frustrum is in shadow, or light.


Please send any comments to: rjklindsay@hotmail.com

