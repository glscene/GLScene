GLScene "Earth" demo.

A small (code-wise) demo that features earth, with night and day
texture maps, atmospheric effects, an orbiting moon, the sun,
a stars background and optional constellation lines.

The only technical part lies with the atmospheric effect, which
is raytraced on key points, and then interpolated in between. 
Yes, I know earth atmosphere's is much thinner, but a thicker 
atmosphere looks better :)

Requirements:
- Hardware accelerated graphics with OpenGL support
- multitexturing and ARB_texture_combine support for city lights
  (ie. recent drivers and GeForce/Radeon-class hardware)
- 64 MB video memory for high-resolution resources support 
  (optional, and 128 MB recommended for optimal performance)

Keys and Controls:
- Mouse left click : move around your target body
- Mouse wheel : move comera closer/farther
- Mouse right click : adjust focal length
- 'E' : center and target Earth
- 'M' : center and target Moon
- '0'..'9' : adjust the speed, from paused (0) to superfast (9)
- 'C' : toggle constellation lines visibility
- 'H' : loads high-resolution resources

The high resolution resources are intended for GeForce3 and higher
video cards, with 64 MB or more of video memory, they are available
for download in a separate package at:

https://sourceforge.net/project/showfiles.php?group_id=29749

they include 4096x2048 textures for earth (night & day),
2048x1024 texture for the moon and the hipparcos star database
(87000 stars, rather than the 9100 ones in the default Yale db).
High-resolution mode also turns on FSAA (if available).

Resources credits and links:
- Earth textures from the NASA Blue Marble project
  (http://earthobservatory.nasa.gov/Newsroom/BlueMarble/)
- Moon texture from... dunno. Some bits looks like shots
  from the Clementine mission, with some manual cleaning up.
- Star databases are the Yale Bright Stars Catalog 
  (http://amase.gsfc.nasa.gov/amase/MissionPages/YALEBSC.html)
  and the Hipparcos Catalog up to magnitude 9.0
  (http://astro.estec.esa.nl/Hipparcos/catalog.html)
- Constellation lines file comes from "Cartes du Ciel"/"SkyCharts"
  (http://skychart.sourceforge.net/index.php)

Known issues:
- antediluvian video boards may suffer *a lot* from the smoothed
  points used to render the stars, or smoothed lines used to render
  constellations, consider yourself warned. If too horrible, delete
  or rename 'Yale_BSC.stars' to prevent the stars from being loaded.
- if you get crashes or freezes with the high-resolution textures
  on a 64MB or less video card, consider clocking down your RAM or CPU 
  (underclocking), they can put a lot of strain on these during AGP 
  texturing.

Eric Grange
http://glscene.org