{ : Advanced Normal/Bump shading with CgShaders.<p>}
program Cg_BlinnSheen;

uses
  Forms,
  fBlinnSheen in 'fBlinnSheen.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
