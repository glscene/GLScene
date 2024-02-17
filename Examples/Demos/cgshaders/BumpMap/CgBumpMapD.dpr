(*
Bump Mapping
Author: Antic (edited by Da Stranger)
Date:  09 June 2006
*)
program CgBumpMapD;

uses
  Forms,
  fBumpMapD in 'fBumpMapD.pas' {FormBumpMap};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormBumpMap, FormBumpMap);
  Application.Run;
end.
