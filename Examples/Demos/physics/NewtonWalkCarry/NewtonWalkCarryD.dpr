{ : Walk and carry demo.

  <b>History : </b><font size=-1><ul>
  <li>31/01/11 - Yar - Updated after Huge update GLNGDManager (by Dev)
  <li>25/12/10 - Dev - Created
  </ul>
}
program NewtonWalkCarryD;

uses
  Forms,
  fNewtonWalkCarryD in 'fNewtonWalkCarryD.pas' {FormNewtonWalkCarry};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormNewtonWalkCarry, FormNewtonWalkCarry);
  Application.Run;
end.
