{: GLCameraController demo.

   This demo shows how the TGLCameraController can be used to control the
   camera's movement around a target using minimal code.

}
program CameraControllerD;

uses
  Forms,
  fCameraControllerD in 'fCameraControllerD.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'CameraControllerDemo';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
