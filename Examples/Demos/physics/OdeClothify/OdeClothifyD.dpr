(*
   Clothify demo.
   This demo mixes several experimental thingies, and will probably be
   cleaned-up/split to be easier to follow, ad interim, you enter
   the jungle below at your own risks.

   Created by MF 10/12/03
*)
program OdeClothifyD;

uses
  Forms,
  fOdeClothifyD in 'fOdeClothifyD.pas' {FormClothify};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormClothify, FormClothify);
  Application.Run;
end.
