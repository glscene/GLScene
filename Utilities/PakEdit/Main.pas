unit Main;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StrUtils, Menus, ImgList, ExtCtrls, ComCtrls, GLVfsPAK;

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
    N1: TMenuItem;
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
    Compression1: TMenuItem;
    Pak: TGLVfsPAK;
    procedure FormCreate(Sender: TObject);
    procedure TreeViewRefresh;
    procedure FileListRefresh;
    procedure AddNode(text: string; node: TTreeNode);
    function TreeIndexOf(s: string; node: TTreeNode): integer;
    function ListIndexOf(s: string): integer;
    procedure TreeViewCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure ListViewClick(Sender: TObject);
    procedure ListViewDblClick(Sender: TObject);
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
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
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  CurPath,
  Selection: string;
  Pak: TGLVfsPak;

implementation

uses FolderDialog, FolderSelect;

{$R *.dfm}
{$R icons.res}

procedure TForm1.AddNode(text: string; node: TTreeNode);
var
   s: string;
   c: integer;
begin
   c:=Pos('/',Text);
   if c=0 then exit;
   s:=Copy(text,1,c-1);
   Delete(text,1,c);
   if TreeIndexOf(s,node)=-1 then
   AddNode(text, TreeView.Items.AddChild(node,s)) else
   AddNode(text, node[TreeIndexOf(s,node)]);
end;

function TForm1.TreeIndexOf(s: string; node: TTreeNode): integer;
var
   i: integer;
begin
   Result:=-1;
      for i:=0 to node.Count-1 do
         if node[i].Text=s then Result:=i;
end;

function TForm1.ListIndexOf(s: string): integer;
var
   i: integer;
begin
   Result:=-1;
      for i:=0 to ListView.Items.Count-1 do
         if ListView.Items[i].Caption=s then Result:=i;
end;

procedure TForm1.TreeViewRefresh;
var
   i: integer;
begin
   with TreeView do begin
      Items.Clear;
      Items.AddChild(Items.GetFirstNode,ExtractFileName(Pak.PakFileName));
      for i:=0 to Pak.Files.Count-1 do
         AddNode(Pak.Files[i],Items.GetFirstNode);
      Items[0].Expanded:=True;
      Items[0].SelectedIndex:=1;
      Items[0].ImageIndex:=1;
   end;
   TreeView.Selected:=TreeView.Items.GetFirstNode;
end;

function MakeMemSize(size: integer): string;
const
   kb=1024;
   mb=kb*kb;
   gb=mb*kb;
begin
   case size of
      0..kb-1:Result:=IntToStr(size)+' B';
      kb..mb-1:Result:=Format('%.2f KB',[size/kb]);
      mb..gb-1:Result:=Format('%.2f MB',[size/mb]);
   else
      Result:=Format('%.2f GB',[size/gb]);
   end;
end;

procedure TForm1.FileListRefresh;
var
   i, j: integer;
   s, name: string;
   n: TTreeNode;
   dir: boolean;
begin
   n:=TreeView.Selected;
   s:=n.Text;
   if s=ExtractFileName(Pak.PakFileName) then s:='';
   while Assigned(n.parent) do begin
      n:=n.Parent;
      if n.AbsoluteIndex<>0 then
      s:=n.Text+'/'+s;
   end;
   if s<>'' then s:=s+'/';
   CurPath:=s;

   ListView.Clear;
   for i:=0 to Pak.Files.Count-1 do
      if Copy(Pak.Files[i],1,Length(s))=s then begin
         name:=RightStr(Pak.Files[i],Length(Pak.Files[i])-Length(s));
         j:=Pos('/',name);
         if j=0 then
            Dir:=False else
            Dir:=True;

         if Dir=True then
            name:=Copy(name,1,j-1);
         if ListIndexOf(name)=-1 then
         with ListView.Items.Add do begin
            Caption:=Name;
            if Dir=True then begin
                  SubItems.Add('-');
                  SubItems.Add('-')
               end else begin
                  SubItems.Add(MakeMemSize(Pak.GetFileSize(i)));
                  SubItems.Add(MakeMemSize(Pak.GetFileSize(i)))
               end;

            if Dir=True then
               ImageIndex:=0 else
               ImageIndex:=2;
         end;
      end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
   Bmp: TBitmap;
begin
   Bmp:=TBitmap.Create;
   Bmp.LoadFromResourceName(HInstance, 'ICONS');
   ImageList1.AddMasked(Bmp,clWhite);
   Bmp.Free;
end;

procedure TForm1.TreeViewCollapsing(Sender: TObject; Node: TTreeNode;
  var AllowCollapse: Boolean);
begin
   if Node.AbsoluteIndex=0 then
   AllowCollapse:=False else
   AllowCollapse:=True;
end;

procedure TForm1.ListViewClick(Sender: TObject);
var
   s: string;
begin
   if not Assigned(ListView.Selected) then Exit;
   s:=ListView.Selected.Caption;
   Selection:=CurPath+s;
end;

procedure TForm1.ListViewDblClick(Sender: TObject);
var
   s: string;
   n: TTreeNode;
begin
   if not Assigned(ListView.Selected) then Exit;
   if ListView.Selected.ImageIndex=2 then Exit;
   s:=ListView.Selected.Caption;
   n:=TreeView.Selected;
   TreeView.Selected:=n[TreeIndexOf(s,n)];
end;

procedure TForm1.TreeViewChange(Sender: TObject; Node: TTreeNode);
begin
   FileListRefresh;
end;

procedure TForm1.ListViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
   if ListView.Enabled=False then Exit;
   ListViewClick(Sender);
   if Key=VK_RETURN then ListViewDblClick(Sender);
   if Key=VK_DELETE then Deleteselectedfile1Click(Self);
end;

procedure TForm1.ListViewChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
   ListViewClick(Sender);
end;

procedure TForm1.New1Click(Sender: TObject);
begin
   if SaveDialog1.Execute then begin
      Pak.ClearPakFiles;
      Pak.LoadFromFile(SaveDialog1.FileName, fmCreate or fmShareDenyWrite);
      TreeViewRefresh;
      ListView.Enabled:=True;
   end;
end;

procedure TForm1.Open1Click(Sender: TObject);
begin
   OpenDialog1.DefaultExt:=SaveDialog1.DefaultExt;
   OpenDialog1.Filter:=SaveDialog1.Filter;
   OpenDialog1.Options:=OpenDialog1.Options-[ofAllowMultiSelect];
   if OpenDialog1.Execute then begin
      Pak.ClearPakFiles;
      Pak.LoadFromFile(OpenDialog1.FileName, fmOpenReadWrite or fmShareDenyWrite);
      TreeViewRefresh;
      ListView.Enabled:=True;
   end;
end;

procedure TForm1.Createfolder1Click(Sender: TObject);
begin
   if not Assigned(Pak) then Exit;
   if FDialog.ShowModal=mrOk then begin
      Pak.AddEmptyFile('temp.tmp',CurPath+FDialog.Edit1.Text+'/');
      TreeViewRefresh;
   end;
end;

procedure TForm1.Addfiles1Click(Sender: TObject);
var
   i: integer;
begin
   if not Assigned(Pak) then Exit;
   OpenDialog1.DefaultExt:='';
   OpenDialog1.Filter:='All Files|*.*';
   OpenDialog1.Options:=OpenDialog1.Options+[ofAllowMultiSelect];
   if OpenDialog1.Execute then begin
      for i:=0 to OpenDialog1.Files.Count-1 do
         if ExtractFileName(OpenDialog1.Files[i])<>'' then
            Pak.AddFromFile(OpenDialog1.Files[i],CurPath);
      TreeViewRefresh;
   end;
end;

procedure TForm1.Deleteselectedfile1Click(Sender: TObject);
var
   i: integer;
   S: TStrings;
begin
   if ListView.SelCount=0 then Exit;
   S:=TStringList.Create;
   for i:=0 to ListView.Items.Count-1 do
      if ListView.Items[i].Selected then S.Add(CurPath+ListView.Items[i].Caption);
   for i:=0 to S.Count-1 do
      Pak.RemoveFile(S[i]);

   S.Free;
   FileListRefresh;
   if ListView.Items.Count=0 then TreeViewRefresh;
end;

procedure TForm1.Deleteselectedfolder1Click(Sender: TObject);
var
   i, l: integer;
   S: TStrings;
begin                       
   s:=TStringList.Create;
   s.AddStrings(Pak.Files);
   l:=Length(CurPath);
   for i:=0 to S.Count-1 do
      if Copy(S[i],1,l)=CurPath then
         Pak.RemoveFile(S[i]);
   s.Free;
   TreeViewRefresh;
end;

procedure TForm1.TreeViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
   if ListView.Enabled=False then Exit;
   if Key=VK_DELETE then Deleteselectedfolder1Click(Self);
end;

procedure TForm1.Extractselectedfiles1Click(Sender: TObject);
var
   i: integer;
begin
   if ListView.SelCount=0 then Exit;
   if FolderSel.ShowModal=mrOK then begin
      for i:=0 to ListView.Items.Count-1 do
         if ListView.Items[i].Selected then
            Pak.Extract(CurPath+ListView.Items[i].Caption, FolderSel.ShellView.Path+'\'+ListView.Items[i].Caption);
   end;
end;


procedure TForm1.Exit1Click(Sender: TObject);
begin
   Application.Terminate;
end;

end.
