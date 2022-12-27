{: A "pawn"-like revolution solid.

   Allows playing with a few settings for a revolution solid and see the visual
   (and triangle count) impact they have.
}
program PawnD;

uses
  Forms,
  fPawnD in 'fPawnD.pas' {FormPawn};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormPawn, FormPawn);
  Application.Run;
end.
