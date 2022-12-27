program ArchiveEditD;

uses
  Forms,
  fMainD in 'fMainD.pas' {Form1},
  fFolderDlg in 'fFolderDlg.pas' {FDialog},
  FolderSelect in 'FolderSelect.pas' {FolderSel};

{$R *.res}
{.$R Icons.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TFDialog, FDialog);
  Application.CreateForm(TFolderSel, FolderSel);
  Application.Run;
end.
