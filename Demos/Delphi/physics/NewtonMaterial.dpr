{ : Newton Game Dynamics Physics Engine demo.

  This demo demontrate how to use material (or surface) effect of newton.
  Manager owns SurfaceItems and SurfacePair list where we can adjust
  elasticity,friction... between two SurfaceItems.
  We set SurfaceItems for each NGDBehaviours, and in SurfacePair,
  we choose the two group-id wich perform these effects.

  Actually we can't set surfaceItem on behaviour (or on surfacePair)
  in design time. This must be done in runtime.

  <b>History : </b><font size=-1><ul>
  <li>31/01/11 - FP - Update for GLNGDManager
  <li>17/09/10 - FP - Created by Franck Papouin
  </ul>
}
program NewtonMaterial;

uses
  Forms,
  NewtonMaterialFm in 'NewtonMaterialFm.pas' {FormNewtonMaterial};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormNewtonMaterial, FormNewtonMaterial);
  Application.Run;
end.
