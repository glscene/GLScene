(* Basic Skeletal Animation sample.

   This demo loads a SMD model and 3 of its animation (SMD are part of and
   Half-Life MDL file, and may be extracted to individual files with tools
   like MilkShape).
   SMD loading order matters: the "model" SMD must be loaded first, it contains
   bones and vertex data, the "animation" SMD are loaded afterwards with
   AddDataFromFile, they contain only bone animation data. Don't forget to link
   the actor to a material library, SMD models commonly use several textures!

   If you hit one of the jump buttons, the character will perform the jump,
   and once it has been completed, revert to the walk or run animation.

   The slider makes the character look left/right by using blending (through
   and animation controler).

   Why, why, why didn't the model moves it arms? Because it's not
   in the animations frames! HL uses blends to move the arms and accomodate gestures
   such has aiming a weapon...
   Side note: the look_left_right.smd animation was added by me, so don't blame
   the model's author if it ain't anatomically correct (hand edited smd with
   only three keyframes).

   Model Author: Neal 'Guplik' Corbett, edited by ~A.u.s.t.i.n. (manny@cgocable.net)
   Thanks!
*)
program SkeletalD;

uses
  Forms,
  fSkeletalD in 'fSkeletalD.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormSkeletal, FormSkeletal);
  Application.Run;
end.
