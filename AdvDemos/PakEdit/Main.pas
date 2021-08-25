unit Main;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.ImageList,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  StrUtils,
  Vcl.Menus,
  Vcl.ImgList,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  GLS.FileVfsPAK;

type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    TreeView: TTreeView;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    ImageList1: TImageList;
    ListView: TListView;
    File1: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    Exit1: TMenuItem;
    Bevel1: TBevel;
    Edit1: TMenuItem;
    Createfolder1: TMenuItem;
    Addfiles1: TMenuItem;
    Deleteselectedfile1: TMenuItem;
    Deleteselectedfolder1: TMenuItem;
    N2: TMenuItem;
    Extractselectedfiles1: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    N1: TMenuItem;
    Converttocompressed: TMenuItem;
    Converttouncompressed: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure TreeViewRefresh;
    procedure FileListRefresh;
    procedure AddNode(text: string; node: TTreeNode);
    function TreeIndexOf(s: string; node: TTreeNode): integer;
    function ListIndexOf(s: string): integer;
    procedure TreeViewCollapsing(Sender: TObject; node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure ListViewClick(Sender: TObject);
    procedure ListViewDblClick(Sender: TObject);
    procedure TreeViewChange(Sender: TObject; node: TTreeNode);
    procedure ListViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure New1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Createfolder1Click(Sender: TObject);
    procedure Addfiles1Click(Sender: TObject);
    procedure Deleteselectedfile1Click(Sender: TObject);
    procedure Deleteselectedfolder1Click(Sender: TObject);
    procedure TreeViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Extractselectedfiles1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ConverttocompressedClick(Sender: TObject);
    procedure ConverttouncompressedClick(Sender: TObject);
  private

  public

    procedure RefreshMenu;
  end;

var
  Form1: TForm1;
  CurPath, Selection: string;
  Pak: TGLVfsPak;

implementation

uses
  FolderDialog, FolderSelect, Frm_CompressionRatio, ShellApi;

{$R *.dfm}
{$R icons.res}

procedure TForm1.AddNode(text: string; node: TTreeNode);
var
  s: string;
  c: integer;
begin
  c := Pos('/', text);
  if c = 0 then
    exit;
  s := Copy(text, 1, c - 1);
  Delete(text, 1, c);
  if TreeIndexOf(s, node) = -1 then
    AddNode(text, TreeView.Items.AddChild(node, s))
  else
    AddNode(text, node[TreeIndexOf(s, node)]);
end;

function TForm1.TreeIndexOf(s: string; node: TTreeNode): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to node.Count - 1 do
    if node[i].text = s then
      Result := i;
end;

function TForm1.ListIndexOf(s: string): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to ListView.Items.Count - 1 do
    if ListView.Items[i].Caption = s then
      Result := i;
end;

procedure TForm1.TreeViewRefresh;
var
  i: integer;
begin
  with TreeView do
  begin
    Items.Clear;
    Items.AddChild(Items.GetFirstNode, ExtractFileName(Pak.PakFileName));
    for i := 0 to Pak.Files.Count - 1 do
      AddNode(Pak.Files[i], Items.GetFirstNode);
    Items[0].Expanded := True;
    Items[0].SelectedIndex := 1;
    Items[0].ImageIndex := 1;
  end;
  TreeView.Selected := TreeView.Items.GetFirstNode;
end;

function MakeMemSize(size: integer): string;
const
  kb = 1024;
  mb = kb * kb;
  gb = mb * kb;
begin
  case size of
    0 .. kb - 1:
      Result := IntToStr(size) + ' B';
    kb .. mb - 1:
      Result := Format('%.2f KB', [size / kb]);
    mb .. gb - 1:
      Result := Format('%.2f MB', [size / mb]);
  else
    Result := Format('%.2f GB', [size / gb]);
  end;
end;

procedure TForm1.FileListRefresh;
var
  i, j: integer;
  s, name: string;
  n: TTreeNode;
  dir: Boolean;
begin
  n := TreeView.Selected;
  s := n.text;
  if s = ExtractFileName(Pak.PakFileName) then
    s := '';
  while Assigned(n.parent) do
  begin
    n := n.parent;
    if n.AbsoluteIndex <> 0 then
      s := n.text + '/' + s;
  end;
  if s <> '' then
    s := s + '/';
  CurPath := s;

  ListView.Clear;
  for i := 0 to Pak.Files.Count - 1 do
    if Copy(Pak.Files[i], 1, Length(s)) = s then
    begin
      name := RightStr(Pak.Files[i], Length(Pak.Files[i]) - Length(s));
      j := Pos('/', name);
      if j = 0 then
        dir := False
      else
        dir := True;

      if dir = True then
        name := Copy(name, 1, j - 1);
      if ListIndexOf(name) = -1 then
        with ListView.Items.Add do
        begin
          Caption := Name;
          if dir = True then
          begin
            SubItems.Add('-');
            SubItems.Add('-')
          end
          else
          begin
            SubItems.Add(MakeMemSize(Pak.GetFileSize(i)));
            SubItems.Add(MakeMemSize(Pak.GetFileSize(i)))
          end;

          if dir = True then
            ImageIndex := 0
          else
            ImageIndex := 2;
        end;
    end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  Bmp: TBitmap;
begin
  Pak := TGLVfsPak.Create(nil);
  Bmp := TBitmap.Create;
  Bmp.LoadFromResourceName(HInstance, 'ICONS');
  ImageList1.AddMasked(Bmp, clWhite);
  Bmp.Free;
  RefreshMenu;
end;

procedure TForm1.TreeViewCollapsing(Sender: TObject; node: TTreeNode;
  var AllowCollapse: Boolean);
begin
  if node.AbsoluteIndex = 0 then
    AllowCollapse := False
  else
    AllowCollapse := True;
end;

procedure TForm1.ListViewClick(Sender: TObject);
var
  s: string;
begin
  if not Assigned(ListView.Selected) then
    exit;
  s := ListView.Selected.Caption;
  Selection := CurPath + s;
end;

procedure TForm1.ListViewDblClick(Sender: TObject);
var
  s: string;
  n: TTreeNode;
begin
  if not Assigned(ListView.Selected) then
    exit;
  if ListView.Selected.ImageIndex = 2 then
    exit;
  s := ListView.Selected.Caption;
  n := TreeView.Selected;
  TreeView.Selected := n[TreeIndexOf(s, n)];
end;

procedure TForm1.TreeViewChange(Sender: TObject; node: TTreeNode);
begin
  FileListRefresh;
end;

procedure TForm1.ListViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ListView.Enabled = False then
    exit;
  ListViewClick(Sender);
  if Key = VK_RETURN then
    ListViewDblClick(Sender);
  if Key = VK_DELETE then
    Deleteselectedfile1Click(Self);
end;

procedure TForm1.ListViewChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  ListViewClick(Sender);
end;

procedure TForm1.New1Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    Pak.ClearPakFiles;
    Pak.LoadFromFile(SaveDialog1.FileName, fmCreate or fmShareDenyWrite);
    TreeViewRefresh;
    ListView.Enabled := True;
    RefreshMenu;
  end;
end;

procedure TForm1.Open1Click(Sender: TObject);
begin
  OpenDialog1.DefaultExt := SaveDialog1.DefaultExt;
  OpenDialog1.Filter := SaveDialog1.Filter;
  OpenDialog1.Options := OpenDialog1.Options - [ofAllowMultiSelect];
  if OpenDialog1.Execute then
  begin
    Pak.ClearPakFiles;
    Pak.LoadFromFile(OpenDialog1.FileName, fmOpenReadWrite or fmShareDenyWrite);
    TreeViewRefresh;
    ListView.Enabled := True;
    RefreshMenu;
  end;
end;

procedure TForm1.Createfolder1Click(Sender: TObject);
begin
  if not Assigned(Pak) then
    exit;
  if FDialog.ShowModal = mrOk then
  begin
    Pak.AddEmptyFile('temp.tmp', CurPath + FDialog.Edit1.text + '/');
    TreeViewRefresh;
  end;
end;

procedure TForm1.Addfiles1Click(Sender: TObject);
var
  i: integer;
begin
  if not Assigned(Pak) then
    exit;
  OpenDialog1.DefaultExt := '';
  OpenDialog1.Filter := 'All Files|*.*';
  OpenDialog1.Options := OpenDialog1.Options + [ofAllowMultiSelect];
  if OpenDialog1.Execute then
  begin
    for i := 0 to OpenDialog1.Files.Count - 1 do
      if ExtractFileName(OpenDialog1.Files[i]) <> '' then
        Pak.AddFromFile(OpenDialog1.Files[i], CurPath);
    TreeViewRefresh;
  end;
end;

procedure TForm1.Deleteselectedfile1Click(Sender: TObject);
var
  i: integer;
  s: TStrings;
begin
  if ListView.SelCount = 0 then
    exit;
  s := TStringList.Create;
  for i := 0 to ListView.Items.Count - 1 do
    if ListView.Items[i].Selected then
      s.Add(CurPath + ListView.Items[i].Caption);
  for i := 0 to s.Count - 1 do
    Pak.RemoveFile(s[i]);

  s.Free;
  FileListRefresh;
  if ListView.Items.Count = 0 then
    TreeViewRefresh;
end;

procedure TForm1.Deleteselectedfolder1Click(Sender: TObject);
var
  i, l: integer;
  s: TStrings;
begin
  s := TStringList.Create;
  s.AddStrings(Pak.Files);
  l := Length(CurPath);
  for i := 0 to s.Count - 1 do
    if Copy(s[i], 1, l) = CurPath then
      Pak.RemoveFile(s[i]);
  s.Free;
  TreeViewRefresh;
end;

procedure TForm1.TreeViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ListView.Enabled = False then
    exit;
  if Key = VK_DELETE then
    Deleteselectedfolder1Click(Self);
end;

procedure TForm1.Extractselectedfiles1Click(Sender: TObject);
var
  i: integer;
begin
  if ListView.SelCount = 0 then
    exit;
  if FolderSel.ShowModal = mrOk then
  begin
    for i := 0 to ListView.Items.Count - 1 do
      if ListView.Items[i].Selected then
        Pak.Extract(CurPath + ListView.Items[i].Caption,
          FolderSel.ShellView.Path + '\' + ListView.Items[i].Caption);
  end;
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(Pak) then
    Pak.Free;
end;

// TForm1.ConverttocompressedClick
//
procedure TForm1.ConverttocompressedClick(Sender: TObject);
var
  cbrRatio: TZCompressedMode;
  newPak, oldPak: TGLVfsPak;
  i: integer;
  oldFileName, newFileName: String;
begin
  cbrRatio := SelectCompressionRatio;
  if cbrRatio <> None then
  begin
    oldFileName := Pak.PakFileName;
    Pak.Free;
    Pak := nil;
    newFileName := ChangeFileExt(oldFileName, '.bak');
    if FileExists(newFileName) then
      DeleteFile(newFileName);
    RenameFile(oldFileName, newFileName);
    newPak := TGLVfsPak.Create(nil, cbrRatio);
    newPak.LoadFromFile(oldFileName, fmCreate or fmShareDenyWrite);
    oldPak := TGLVfsPak.Create(nil);
    oldPak.LoadFromFile(newFileName, fmOpenRead or fmShareDenyWrite);
    for i := 0 to oldPak.FileCount - 1 do
    begin
      newPak.AddFromStream(ExtractFileName(oldPak.Files[i]),
        ExtractFilePath(oldPak.Files[i]), oldPak.GetFile(i));
    end;
    oldPak.Free;
    oldPak := nil;
    Pak := newPak;
    TreeViewRefresh;
    ListView.Enabled := True;
    RefreshMenu;
  end;
end;

// TForm1.ConverttouncompressedClick
//
procedure TForm1.ConverttouncompressedClick(Sender: TObject);
var
  newPak, oldPak: TGLVfsPak;
  i: integer;
  oldFileName, newFileName: String;
begin
  if not Pak.Compressed then
    exit;
  oldFileName := Pak.PakFileName;
  Pak.Free;
  Pak := nil;
  newFileName := ChangeFileExt(oldFileName, '.bak');
  if FileExists(newFileName) then
    DeleteFile(newFileName);
  RenameFile(oldFileName, newFileName);
  newPak := TGLVfsPak.Create(nil);
  newPak.LoadFromFile(oldFileName, fmCreate or fmShareDenyWrite);
  oldPak := TGLVfsPak.Create(nil);
  oldPak.LoadFromFile(newFileName, fmOpenRead or fmShareDenyWrite);
  for i := 0 to oldPak.FileCount - 1 do
  begin
    newPak.AddFromStream(ExtractFileName(oldPak.Files[i]),
      ExtractFilePath(oldPak.Files[i]), oldPak.GetFile(i));
  end;
  oldPak.Free;
  oldPak := nil;
  Pak := newPak;
  TreeViewRefresh;
  ListView.Enabled := True;
  RefreshMenu;
end;

// TForm1.RefreshMenu
//
procedure TForm1.RefreshMenu;
begin
  Converttocompressed.Visible := not Pak.Compressed;
  Converttouncompressed.Visible := Pak.Compressed;
end;

end.
