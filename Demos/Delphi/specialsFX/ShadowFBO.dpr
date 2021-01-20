{
Demo of the new FBO Renderer component.

Version History:
  30/04/2010 - Yar - Fixed for ATI
  12/11/2009 - DaStr - Initial version (by YarUnderoaker)

}
program ShadowFBO;

uses
  Forms,
  ShadowFBOFm in 'ShadowFBOFm.pas' {FormShadowFBO};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormShadowFBO, FormShadowFBO);
  Application.Run;
end.
