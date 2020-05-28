{: Ultra-basic collision detection sample.

   Two sphere have been placed in the scene, both have a TGLBCollision behaviour
   linked to the collision manager component.

   Move them and click the button, if they collide, you will get a message, if
   not, nothing will happen.
}
program sphere;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
