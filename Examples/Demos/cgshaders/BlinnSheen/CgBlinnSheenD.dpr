(* Advanced Normal/Bump shading with CgShaders *)
program CgBlinnSheenD;

uses
  Forms,
  fdBlinnSheen in 'fdBlinnSheen.pas' {FormBlinnSheen};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormBlinnSheen, FormBlinnSheen);
  Application.Run;
end.
