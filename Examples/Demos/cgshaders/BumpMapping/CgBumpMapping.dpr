(*
Bump Mapping
Author: Antic (edited by Da Stranger)
Date:  09 June 2006
*)
program CgBumpMapping;

uses
  Forms,
  fBumpMap in 'fBumpMap.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TBumpDemo_frm, BumpDemo_frm);
  Application.Run;
end.
