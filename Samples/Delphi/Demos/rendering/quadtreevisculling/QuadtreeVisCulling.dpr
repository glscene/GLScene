{: Using the TOctreeSpacePartition for visibility culling.

  Demo by HRLI slightly reworked to be a quadtree demo and committed by MF.
}
program QuadtreeVisCulling;

uses
  Forms,
  fQuadtreeVisCulling in 'fQuadtreeVisCulling.pas' {frmQuadtreeVisCulling};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmQuadtreeVisCulling, frmQuadtreeVisCulling);
  Application.Run;
end.
