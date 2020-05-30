{
Demo of the new FBO Renderer component.

Version History:
  30/04/2010 - Yar - Fixed for ATI
  12/11/2009 - DaStr - Initial version (by YarUnderoaker)

}
program ShadowmappingFBO;

uses
  Forms,
  uMainForm in 'uMainForm.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
