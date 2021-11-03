program OctreeRenderD;

uses
  Forms,
  fOctreeRenderD in 'fOctreeRenderD.pas' {frmOctreeDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmOctreeDemo, frmOctreeDemo);
  Application.Run;
end.
