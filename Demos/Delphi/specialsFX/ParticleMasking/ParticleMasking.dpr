{
Demo of ParticleMasking

Version History:
  29/01/2007 - DaStr - Initial version (a bit modified demo by Kenneth Poulter)

}

program ParticleMasking;

uses
  Forms,
  uParticleMasking in 'uParticleMasking.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
