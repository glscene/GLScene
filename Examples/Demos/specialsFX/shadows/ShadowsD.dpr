{:
  Shadow casting with GLzBuffer by Rene Lindsay.
}
program ShadowsD;

uses
  Forms,
  fShadowsD in 'fShadowsD.pas' {FormShadows};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormShadows, FormShadows);
  Application.Run;
end.
