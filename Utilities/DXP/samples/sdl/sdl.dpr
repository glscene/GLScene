program sdl_test;

{$APPTYPE gui}

uses
  Forms, GLScene, GLSDLContext, GLSDL, GLTeapot, SysUtils
  {$IFDEF FPC}, Interfaces{$ENDIF};

type
   TMySDLViewer = class (TGLSDLViewer)
      procedure EventPollDone(Sender : TObject);
   end;

procedure TMySDLViewer.EventPollDone(Sender : TObject);
begin
   Render;
end;

var
   sdlViewer : TMySDLViewer;
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

   sdlViewer:=TMySDLViewer.Create(nil);
   sdlViewer.Camera:=camera;
   sdlViewer.OnEventPollDone:=sdlViewer.EventPollDone;

   sdlViewer.Render;
   while sdlViewer.Active do begin
      // Message queue is not operational, but there may still be some messages
      Forms.Application.ProcessMessages;
      // Relinquish some of that CPU time
      SDL_Delay(1);
      // Slowly rotate the teapot
      teapot.TurnAngle:=4*Frac(Now*24)*3600;
   end;

   sdlViewer.Free;
   
   scene.Free;
end.
