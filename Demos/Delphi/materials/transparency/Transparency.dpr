{: A basic sample to demonstrate how transparency & Z-buffer work/fight together.
   In this sample, only the sphere are transparent. The form has a few options
   that allow to adjust in which order objects are rendered, and what kind of
   transparency is used.

   Transparency in GLScene is activated by setting Material.Blending to either
   'bmTransparency' or 'bmAdditive', AND giving a <1.0 value to the Diffuse
   color alpha channel (alpha = 0.0 means absolute transparency, alpha = 1.0
   means absolute opacity).

   How do Z-Buffer & transparency work ? When point has to be rendered, OpenGL
   first checks its distance to the camera, the "Z" axis. If the distance
   check is successfull (the new point is closer), it is rendered, and if
   the point is part of a "transparent" object, then OpenGL will mix the existing
   point's color with our new point's color. If the Z check fails, OpenGL doesn't
   even bother about checking transparency.

   This is why, if you want to render transparent objects, you must make sure
   you render the farthest objects first, to give transparency a chance.
   However this effect can be usefull if you want to render mixed, half-transparent
   half-opaque objects.

   They are two ways to order objects in GLScene :<ul>
   <li>ordering : can be done at design-time in the editor or at runtime with
      MoveUp/MoveDown methods
   <li>sorting : adjust the ObjectSorting property (see help for more details)
   </ul>
}
program Transparency;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
