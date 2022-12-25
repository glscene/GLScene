program Cg_BumpMapping;

uses
  Forms,
  fBumpMap in 'fBumpMap.pas' {BumpDemo_frm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TBumpDemo_frm, BumpDemo_frm);
  Application.Run;
end.
