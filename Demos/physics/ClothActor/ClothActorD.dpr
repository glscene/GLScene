(* Verlet cloth simulation and verlet constraints controlled by an
   actor's skeleton.

   Verlet physics is used to simulate a cloth-like effect on a mesh.
   In this demo, the cape mesh is linked to the verlet world and the
   verlet nodes control the surface of the mesh. Verlet constraints
   define boundaries that the verlet nodes cannot enter.

   The skeleton colliders define the verlet constraints for the actor
   using simple spheres and capsules (sphere-capped cylinders). The
   constraints get updated each frame to match the actor's current
   skeleton frame. Using simple primitives to approximate a mesh is
   much quicker than determining mesh volume -> verlet interactions.

   A dynamic octree is used here to give a little performace boost to
   the verlet testing.
*)
program ClothActorD;

uses
  Forms,
  fClothActorD in 'fClothActorD.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormClothActor, FormClothActor);
  Application.Run;
end.
