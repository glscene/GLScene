(*
   PFX Cursor Demo
   Use the mouse left and right buttons
   to change cursor TGLHUDSprite
*)
program PfxCursor;

uses
  Forms,
  fPfxCursor in 'fPfxCursor.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
