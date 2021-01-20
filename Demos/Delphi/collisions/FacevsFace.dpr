{: Face vs Face collision detection.

   This sample illustrates:<ul>
   <li>collisions between FreeForm Objects (Triangle-based)
   <li>collisions between Cubes
   <li>basic user-driven camera movements.
   <li>picking Objects
   <li>moving Objects
   </ul></li>

   Usage:<ul>
   <li>left Mouse will move Camera
   <li>right Mouse will move an object
   <li>left Mouse + shift will roll and pitch the object
   <li>Wheel scroll will zoom in/out
   </ul>
   Bernd Klaiber.
   (modified by DanB 08/07/2003)
}
program FacevsFace;

uses
  Forms,
  FacevsFaceFm in 'FacevsFaceFm.pas' {FormFacevsFace};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormFacevsFace, FormFacevsFace);
  Application.Run;
end.
