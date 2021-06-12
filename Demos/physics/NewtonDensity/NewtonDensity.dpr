{ : Newton Game Dynamics Physics Engine demo.

  This demo explain how to use customForceAndTorque, and show the newton api
  buoyancy effect as exemple.

  Density unit is the number of mass unit per volume unit: D=M/V
  Body volume is calculated by Newton, and Mass is the product result of
  Density*Volume.

  If fluidDensity=bodyDensity, the body will be immerged like a submarine.

  Bugs:
  -Viscosities does not seem to affect buoyancy.
  -Small bodies have huge Viscosities.
  -Sphere and Capsule flows when their density equal fluid density.
  (For sphere we can correct this by multiplying fluidDensity by 1.43)

  Density is also an important parameter when two bodies collide.
  The middle mouse button shoot a small cube with impulse.
  You can see the result when the cube hit the paper ball or the lead ball.

  <b>History : </b><font size=-1><ul>
  <li>31/01/11 - FP - Update for GLNGDManager
  <li>17/09/10 - FP - Created by Franck Papouin
  </ul>
}
program NewtonDensity;

uses
  Forms,
  NewtonDensityFm in 'NewtonDensityFm.pas' {FormNewtonDensity};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormNewtonDensity, FormNewtonDensity);
  Application.Run;
end.
