{ : Newton Game Dynamics Physics Engine demo.

  This is the simplest exemple of how to create dynamic body suported by NGD.
  To execute the simulation we need to indicate to the physics engine the time
  elapsed for update in GLCadencer1Progress.
  The floor is static, so it can't move.

  <b>History : </b><font size=-1><ul>
  <li>31/01/11 - FP - Update for GLNGDManager
  <li>17/09/10 - FP - Created by Franck Papouin
  </ul>

}
program NewtonSimpleSpawn;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
