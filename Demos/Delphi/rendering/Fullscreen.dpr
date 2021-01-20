(*
   Basic demo for using the FullScreen Viewer and GLCanvas.
   This demo uses no forms, but switches directly to 800x600x32bpp fullscreen
   mode and renders a teapot with a colored light, hitting 'ESC' will leave
   full screen mode.

   The demo also makes use of GLCanvas to render a custom 2D overlay with
   TGLCanvas, here a yellow reticle at mouse position.
*)
program Fullscreen;

uses
  Forms,
  FullscreenFm in 'FullscreenFm.pas' {DataModuleFS: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDataModuleFS, DataModuleFS);
  Application.Run;
end.
