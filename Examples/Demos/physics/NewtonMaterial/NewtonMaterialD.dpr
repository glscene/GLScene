(* Newton Game Dynamics Physics Engine demo.

  This demo demonstrates how to use material (or surface) effects with newton.
  Manager owns SurfaceItems and SurfacePair list where we can adjust
  elasticity,friction... between two SurfaceItems.
  We set SurfaceItems for each NGDBehaviours, and in SurfacePair,
  we choose the two group-id wich perform these effects.

  Actually we can't set surfaceItem on behaviour (or on surfacePair)
  in design time. This must be done in runtime.

  Update for GLScene by Franck Papouin 31/01/11
*)

program NewtonMaterialD;

uses
  Forms,
  fNewtonMaterialD in 'fNewtonMaterialD.pas' {FormNewtonMaterial};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormNewtonMaterial, FormNewtonMaterial);
  Application.Run;
end.
