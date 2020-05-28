program lclteapot;

{$APPTYPE gui}

uses Forms, Controls, GLScene, GLTeapot, SysUtils,
     {$IFDEF FPC}
         GLLCLViewer, Interfaces
     {$ELSE}
         GLWin32Viewer
     {$ENDIF};

var
   mainForm : TForm;
   viewer : TGLSceneViewer;
   scene : TGLScene;
   camera : TGLCamera;
   teapot : TGLTeapot;
   light : TGLLightSource;
begin
   Application.Initialize;

   scene:=TGLScene.Create(nil);

   teapot:=TGLTeapot(scene.Objects.AddNewChild(TGLTeapot));

   light:=TGLLightSource(scene.Objects.AddNewChild(TGLLightSource));
   light.Position.SetPoint(10, 15, 20);

   camera:=TGLCamera(scene.Objects.AddNewChild(TGLCamera));
   camera.TargetObject:=teapot;
   camera.SceneScale:=4;
   camera.Position.SetPoint(4, 2, 1);

   mainForm:=TForm.Create(nil);
   {$IFDEF FPC}
   mainForm.Caption:='LCL Teapot';
   {$ELSE}
   mainForm.Caption:='VCL Teapot';
   {$ENDIF};
   mainForm.Width:=400;
   mainForm.Height:=300;
   mainForm.Position:=poScreenCenter;
   viewer:=TGLSceneViewer.Create(mainForm);
   viewer.Parent:=mainForm;
   viewer.Align:=alClient;
   viewer.Camera:=camera;

   mainForm.Show;
   
   while mainForm.Visible do begin
      // Message queue is not operational, but there may still be some messages
      Forms.Application.ProcessMessages;
      // Relinquish some of that CPU time
      Sleep(1);
      // Slowly rotate the teapot
      teapot.TurnAngle:=4*Frac(Now*24)*3600;
   end;

   viewer.Free;
   
   scene.Free;
end.
