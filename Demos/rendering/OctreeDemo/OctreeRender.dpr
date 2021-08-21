program OctreeRender;

uses
  Forms,
  fOctreeRender in 'fOctreeRender.pas' {frmOctreeDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmOctreeDemo, frmOctreeDemo);
  Application.Run;
end.
