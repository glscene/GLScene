{: Using the TOctreeSpacePartition for visibility culling.

  Demo by HRLI slightly reworked to be a quadtree demo and committed by MF.
}
program QuadtreeCulling;

uses
  Forms,
  fQuadtreeCulling in 'fQuadtreeCulling.pas' {frmQuadtreeVisCulling};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmQuadtreeVisCulling, frmQuadtreeVisCulling);
  Application.Run;
end.
