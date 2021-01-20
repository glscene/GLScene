(*
   Basic demo for using the SDLViewer in GLScene.

   The SDL Viewer allows to use SDL for setting up OpenGL, but still render
   with GLScene. The main differences are that SDL has no design-time preview
   and for SDL 1.2 you may have standard forms around it, but as soon as the SDL
   window is closed, the application terminates.

   The SDL viewer is more suited for games or simple apps that aim for
   cross-platform support, for SDL is available on multiple platforms.
   SDL also provides several game-related support APIs for sound, controlers,
   video etc. (see http://www.libsdl.org).

   The rendered scene is similar to the one in the materials/cubemap demo.
*)
program BasicSDL;

uses
  Forms,
  BasicSDLFm in 'BasicSDLFm.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDataModule1, DataModule1);
  Application.Run;
end.
