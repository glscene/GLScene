{:
  History:
  13/01/07 - Da Stranger - Added a FPS counter
  30/11/06 - Tim "Sivael" Kapuœciñski [sivael@gensys.pl]
           -  Added the CenterBranchConstant TrackBar to
              use the new functions of TGLTree.
}
program tree;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
