{: A variation of the magacube bench for testing sorting.

	The cubes in this sample are transparent, and as such need to be depth-sorted
   to render correctly, this uses an alternate sorting code which this benchmark
   helps testing. The sorting mode is osRenderBlended last, which isn't the most
   efficient for that particular case (as a matter of fact, this bench is a worst
   case situation for osRenderBlendedLast), osRenderFarthestFirst would be more
   suited since all the objets must be sorted (osRenderBlendedLast attempts
   ta take advantange of the fact that only a fraction of the objects must be
   depth-sorted, which is a disadvantage if most of them must be sorted).

	Results :

	Size	 Triangles     FPS	    CPU      OpenGL     ColorDepth

	  5      15972      90.0     K7-1145    GF2 Pro       32Bits (vs 139.3 for megacube)
	  5      15972      27.6     Du-800     TNT2 M64      32Bits
   --- 26/01/02 --- Sorting optims, XOpenGL change
	  5      15972      68.2     K7-1145    GF2 Pro       32Bits (vs 110.4 for megacube)
   --- 26/01/02 --- Introduced bench
}
program Megaglasscube;

uses
  Forms,
  MegaglassCubeFm in 'MegaglassCubeFm.pas' {FormMegaglasscube};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormMegaglasscube, FormMegaglasscube);
  Application.Run;
end.
