{:
  History:
  13/01/07 - Da Stranger - Added a FPS counter
  30/11/06 - Tim "Sivael" Kapuœciñski [sivael@gensys.pl]
           -  Added the CenterBranchConstant TrackBar to
              use the new functions of TGLTree.
}
program Tree;

uses
  Forms,
  TreeFm in 'TreeFm.pas' {FormTree};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTree, FormTree);
  Application.Run;
end.
