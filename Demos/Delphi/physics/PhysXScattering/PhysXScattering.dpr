program PhysXScattering;

uses
  Forms,
  Unit1 in 'Unit1.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
