(*
Loading and controlling a Quake 3 model
by Stuart Gooding and Marcus Oblak
*)

program QuakeActorD;

uses
  Forms,
  fQuakeActorD in 'fQuakeActorD.pas' {FormQuakeActor};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormQuakeActor, FormQuakeActor);
  Application.Run;
end.
