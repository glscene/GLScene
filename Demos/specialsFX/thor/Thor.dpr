// The ThorFX special effect basic sample by René Lindsay. 10/3/2001
// The code is largely based on the FireFX code.

program Thor;

uses
  Forms,
  fThor in 'fThor.pas' {FormThor};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormThor, FormThor);
  Application.Run;
end.
