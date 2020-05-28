Archipelago GLScene demo v1.01 (http://glscene.org)

>>>>>> BEFORE starting the demo: <<<<<<

  "splitter.exe" from the data subdirectory will be run, 
  to cut the big .jpg into many .bmp (textures). 
  Uses 48MB of disk space for full-resolution textures
  (recommended if you have a board with 64+ MB of memory).
  You can re-run it later to generate higher/lower resolution
  textures.

---------------------------------------

This demo illustrates the terrain rendering as well as use
of several other components for custom rendering needs.
The terrain rendered is based on a 512x512 heightmap which
is dynamically tesselated (by a ROAM-like algorithm), and
a 4096x4096 texture map split into 16 1024x1024 tiles
(at medium resolution, that's 512x512 tiles, and at low
resolution, 256x256 tiles).

Graphics board memory requirements:
- 128+ MB: no issues, you can even comment out the texture
  compression request in the code for reduced loading times.
- 64 MB: no issues, as long as texture compression is on.
- 32 MB: use medium resolution option, may require to use
  low resolution to achieve decent performance.
- 16 MB or less : use low resolution.

When the program is running, you can access a mini-help via 'F1'.
Movements and camera orientation are controled by arrow keys and
the mouse respectively, camera altitude is controled with 
pageup/pagedown. Exit with the ESC key.

Approximative framerates at startup camera angle & position:
- Athlon XP 1800+ / GF4 Ti 4200: >210 FPS
- Athlon XP 1800+ / GF2 Pro: >140 FPS

Credits and kindly contributed resources by:

- Terrain elevation map and textures : Mattias Fagerlund
  (http://www.cambrianlabs.com/Mattias/), heightmap and sedimentation
  map created by World Machine from Stephen Schmitt
  (http://students.washington.edu/sschmitt/world/)
- Sailboat model and textures : Daniel Polli
  (Daniel@dansteph.com, http://virtualsailor.dansteph.com)
- Water and detail textures from Lemog's 3D Textures
  (http://www.3dtextures.fr.st/)
- Inno Setup was used for the installer
  (http://www.jrsoftware.org/isinfo.php)

Please, do not use/reuse the resources without prior consent 
of their respective owners, only the code is under MPL!

Changes to v1.01:
- added test to warn users that forgot to run "splitter.exe"
- texture filtering CLAMP_TO_EDGE to fix seams on ATI hardware
- reversed sailboat texture name (was mirrored horizontally)
- added warning for graphics boards memory requirements
- splitter.exe includes basic (low quality) resampling support

Eric Grange 
http://glscene.org