{
Demo of ParticleMasking

Version History:
  29/01/2007 - DaStr - Initial version (a bit modified demo by Kenneth Poulter)

}

program ParticleMaskingD;

uses
  Forms,
  fParticleMaskingD in 'fParticleMaskingD.pas' {FormParticleMasking};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormParticleMasking, FormParticleMasking);
  Application.Run;
end.
