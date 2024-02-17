program ImpostersD;

uses
  Forms,
  fImpostersD in 'fImpostersD.pas' {FormImposters};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormImposters, FormImposters);
  Application.Run;
end.
