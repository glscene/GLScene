{
  GLRagdoll Demo by Lucas Goraieb

  This demo shows how to use ODE skinned ragdolls in GLScene.
  To use it, you must set every bone of your model, otherwise some
  vertices won't be aligned to the ragdoll causing weird effects.
  Also this should not be the official demo, it really need some cleanup and
  some improvements, I hope someone do this soon.

  This demo is based on Dave Gravel's ODE Ragdoll.

  Version history:

  <li> 21/11/2009 - DaStr - Added to the CVS
}

program OdeRagdoll;

uses
  Forms,
  fRagdoll in 'fRagdoll.pas' {frmRagdoll};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmRagdoll, frmRagdoll);
  Application.Run;
end.
