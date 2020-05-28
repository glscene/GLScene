{
  GLDynamicTexture Demo.
  Use F2 and F3 to toggle between PBO and non-PBO updates,
  if your card supports it.
  Use F4 to toggle partial updates.

  Version history:
    16/10/07 - LC - Updated to use DirtyRectangle property
    12/07/07 - DaStr - Restored FPC compatibility
    29/06/07 - DaStr - Initial version (by LordCrc)
}
program dynamictexture;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
