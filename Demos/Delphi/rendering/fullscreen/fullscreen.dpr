{: Basic demo for using the FullScreen Viewer and GLCanvas in GLScene.

   This demo uses no forms, but switches directly to 800x600x32bpp fullscreen
   mode and renders a teapot with a colored light, hitting 'ESC' will leave
   full screen mode.

   The demo also makes use of GLCanvas to render a custom 2D overlay with
   TGLCanvas, here a yellow reticle at mouse position.
}
program fullscreen;

uses
  Forms,
  Unit1 in 'Unit1.pas' {DataModule1: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDataModule1, DataModule1);
  Application.Run;
end.
