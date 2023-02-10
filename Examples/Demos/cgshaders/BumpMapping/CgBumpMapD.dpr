(*
Bump Mapping
Author: Antic (edited by Da Stranger)
Date:  09 June 2006
*)
program CgBumpMapD;

uses
  Forms,
  fBumpMapD in 'fBumpMapD.pas' {BumpDemo_frm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TBumpDemo_frm, BumpDemo_frm);
  Application.Run;
end.
