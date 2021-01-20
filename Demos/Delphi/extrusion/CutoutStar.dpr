{: Dynamic sample for the ExtrusionSolid.

   In this sample we extrude a complex solid made of an outer star-like contour
   and an inner square cutout that is moves around. The TGLExtrusionSolid takes
   care of the calculations, so all that is left is defining the contours
   (one in the FormCreate event, and the other in the Cadencer.Progress event).

   Be aware that for TGLExtrusionSolid, like TGLMultiPolygon, the way you describe
   your polygons IS important:<ul>
   <li>the polygons must be in the X, Y plane
   <li>if all your polygons are defined in a counterclockwise manner, the first
       will define the solid outer, the second, third etc. will be the cutouts.
   </ul>
}
program CutoutStar;

uses
  Forms,
  CutoutStarFm in 'CutoutStarFm.pas' {FormCutoutStar};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormCutoutStar, FormCutoutStar);
  Application.Run;
end.
