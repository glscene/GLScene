{ : Walk and carry demo.

  <b>History : </b><font size=-1><ul>
  <li>31/01/11 - Yar - Updated after Huge update GLNGDManager (by Dev)
  <li>25/12/10 - Dev - Created
  </ul>
}
program NewtonWalkAndCarry;

uses
  Forms,
  Unit1 in 'unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
