{: Basic raycast/mesh sample.

   Demonstrating how to find the intersection point between eye-screen ray
   and a simple mesh in orthogonal and perspective views (click on the mushroom
   and intersection point and normal will be calculated).
}
program MeshHitD;

uses
  Forms,
  fMeshHitD in 'fMeshHitD.pas' {FormMeshHit};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMeshHit, FormMeshHit);
  Application.Run;
end.
