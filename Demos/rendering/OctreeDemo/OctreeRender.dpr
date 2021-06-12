program OctreeRender;

uses
  Forms,
  OctreeRenderFm in 'OctreeRenderFm.pas' {frmOctreeDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmOctreeDemo, frmOctreeDemo);
  Application.Run;
end.
