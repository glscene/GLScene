{ : Newton Game Dynamics Physics Engine demo.

  This exemple show how to move a body by calling the pick function of joint.
  Change kinematicControllerOptions.PickModeLinear to feel the difference.
  Most of the code must be written by developer in mouse events.

  <b>History : </b><font size=-1><ul>
  <li>01/02/11 - FP - Removed unused variable and use only mouseLeft to pick
  <li>31/01/11 - Yar - Updated after Huge update GLNGDManager (thaks Dev)
  <li>17/09/10 - FP - Created by Franck Papouin
  </ul>
}
program NewtonMousePickD;

uses
  Forms,
  fNewtonMousePickD in 'fNewtonMousePickD.pas' {FormNewtonMousePick};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormNewtonMousePick, FormNewtonMousePick);
  Application.Run;
end.
