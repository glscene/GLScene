(*
   A simple utility that allows combining RGB and Alpha channel into a single
   32 bits texture, also allows to view RGB & Alpha channel of a 32 bits texture.
   The implementation isn't high performance, just sufficiently fast for
   interactive use.
*)
program TexCombineD;

uses
  Forms,
  fTexCombineD in 'fTexCombineD.pas' {TTBMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTTBMain, TTBMain);
  Application.Run;
end.
