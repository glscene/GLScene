{: Ultra-basic collision detection sample.

   Two sphere have been placed in the scene, both have a TGLBCollision behaviour
   linked to the collision manager component.

   Move them and click the button, if they collide, you will get a message, if
   not, nothing will happen.
}
program SphereD;

uses
  Forms,
  fSphereD in 'fSphereD.pas' {FormSphere};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormSphere, FormSphere);
  Application.Run;
end.
