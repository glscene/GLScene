unit MainFormUnit;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Menus,
  Vcl.StdCtrls,
  Vcl.ExtDlgs,
  
  GLGui, GLScene, GLWin32Viewer,
  GLObjects, GLHUDObjects, GLWindows, GLBitmapFont, GLWindowsFont,
  GLTexture, FGuiSkinEditor, GLCrossPlatform, GLMaterial, GLCoordinates,
  GLBaseClasses;

type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    Close1: TMenuItem;
    N1: TMenuItem;
    Import1: TMenuItem;
    N2: TMenuItem;
    Exit1: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    GLGuiLayout1: TGLGuiLayout;
    ImportDialog: TOpenDialog;
    Edit1: TMenuItem;
    EditLayout1: TMenuItem;
    ListBox: TListBox;
    ListPopup: TPopupMenu;
    Add1: TMenuItem;
    Remove1: TMenuItem;
    Edit2: TMenuItem;
    N3: TMenuItem;
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLPanel1: TGLPanel;
    WindowsBitmapFont1: TGLWindowsBitmapFont;
    Image1: TMenuItem;
    Load1: TMenuItem;
    OpenPictureDialog: TOpenPictureDialog;
    GLMaterialLibrary1: TGLMaterialLibrary;
    HUDSprite1: TGLHUDSprite;
    Edit3: TEdit;
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
    procedure Open1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure Import1Click(Sender: TObject);
    procedure EditLayout1Click(Sender: TObject);
    procedure Add1Click(Sender: TObject);
    procedure Remove1Click(Sender: TObject);
    procedure Edit2Click(Sender: TObject);
    procedure ListBoxClick(Sender: TObject);
    procedure Load1Click(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Edit3KeyPress(Sender: TObject; var Key: Char);
    procedure Button2Click(Sender: TObject);
  private
  public
    Procedure UpdateLayoutList;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  MediaPath : String;
  I : Integer;
begin
  MediaPath := ExtractFilePath(ParamStr(0));
  I := Pos(UpperCase('Samples'), UpperCase(MediaPath));
  if (I <> 0) then
  begin
    Delete(MediaPath, I+8, Length(MediaPath)-I);
    SetCurrentDir(MediaPath+'Media\');
  end;
  GLMaterialLibrary1.Materials[0].Material.Texture.Image.
     LoadFromFile('DefaultSkin.bmp');
end;

procedure TForm1.Open1Click(Sender: TObject);
begin
  If OpenDialog.Execute then
  Begin
    GLScene1.BeginUpdate;
    try
      GLGuiLayout1.Clear;
      GLGuiLayout1.LoadFromFile(OpenDialog.FileName);
      UpdateLayoutList;
    finally
      GLScene1.EndUpdate;
    end;
  End;
end;

procedure TForm1.Save1Click(Sender: TObject);
begin
  if SaveDialog.Execute then
  Begin
    GLGuiLayout1.SaveToFile(SaveDialog.FileName);
  End;
end;

procedure TForm1.Close1Click(Sender: TObject);
begin
  GLScene1.BeginUpdate;
  try
    GLGuiLayout1.Clear;
    UpdateLayoutList;
  finally
    GLScene1.EndUpdate;
  end;
End;

procedure TForm1.Import1Click(Sender: TObject);
Var
  XC : Integer;

begin
  if ImportDialog.Execute then
  Begin
    GLScene1.BeginUpdate;
    try
      For XC := 0 to ImportDialog.Files.Count-1 do
      Begin
        try
          GLGuiLayout1.LoadFromFile(ImportDialog.Files[XC]);
        except
        end;
      End;
      UpdateLayoutList;
    finally
      GLScene1.EndUpdate;
    end;
  End;
end;

procedure TForm1.EditLayout1Click(Sender: TObject);
begin
  Hide;
  GLScene1.BeginUpdate;
  try
    If ListBox.ItemIndex >= 0 then
    GUIComponentDialog((ListBox.Items.Objects[ListBox.ItemIndex] as TGLGuiComponent).Elements)
  finally
    GLScene1.EndUpdate;
    Show;
  end;
end;

Procedure TForm1.UpdateLayoutList;
var
  i : Integer;
begin
  ListBox.Clear;
  With GLGuiLayout1.GuiComponents do for i:=0 to Count-1 do
    ListBox.Items.AddObject(Items[i].Name, Items[i]);
  ListBox.Selected[GLGuiLayout1.GuiComponents.Count-1] := True;
End;

procedure TForm1.Add1Click(Sender: TObject);
Var
  GuiComp : TGLGuiComponent;

begin
  GuiComp := GLGuiLayout1.GuiComponents.Add as TGLGuiComponent;

  If ListBox.ItemIndex >= 0 then
  begin
    GuiComp.Name := 'Newly Added';
  end else GuiComp.Name := Edit3.Text;

  UpdateLayoutList;
end;

procedure TForm1.Remove1Click(Sender: TObject);
Var
  S : String;
begin
  If ListBox.ItemIndex >= 0 then
  Begin
    GLScene1.BeginUpdate;
    try
      GLGuiLayout1.GUIComponents.Delete(ListBox.ItemIndex);
      ListBox.Items.Delete(ListBox.ItemIndex);
      S := GLPanel1.GuiLayoutName;
      GLPanel1.GuiLayoutName := '';
      GLPanel1.GuiLayoutName := S;
    finally
      GLScene1.EndUpdate;
    end;
  End;
end;

procedure TForm1.Edit2Click(Sender: TObject);
begin
  If ListBox.ItemIndex >= 0 then
  GUIComponentDialog((ListBox.Items.Objects[ListBox.ItemIndex] as TGLGuiComponent).Elements)
end;

procedure TForm1.ListBoxClick(Sender: TObject);
begin
  GLScene1.BeginUpdate;
  try
    If ListBox.ItemIndex >= 0 then
    Begin
      GLPanel1.GuiLayoutName := GLGuiLayout1.GuiComponents.Items[ListBox.ItemIndex].Name;
      Edit3.text := GLPanel1.GuiLayoutName;
    End else
    Begin
      GLPanel1.GuiLayoutName := '';
      Edit3.text := 'Newly Added';
    End;
    GLPanel1.DoChanges;
  finally
    GLScene1.EndUpdate;
  end;
end;

procedure TForm1.Load1Click(Sender: TObject);
Var
  Mat : TGLLibMaterial;
  MatName : String;
begin
  If OpenPictureDialog.Execute then
  Begin
    GLScene1.BeginUpdate;
    try
      MatName := ExtractFileName(OpenPictureDialog.FileName);
      Mat := GLMaterialLibrary1.Materials.GetLibMaterialByName(MatName);
      If not Assigned(Mat) then
      Begin
        GLMaterialLibrary1.AddTextureMaterial(MatName,OpenPictureDialog.FileName).Material.Texture.TextureMode := tmReplace;
      End;
      GLGuiLayout1.Material.LibMaterialName := MatName;
      HUDSprite1.Material.LibMaterialName := MatName;
    finally
      GLScene1.EndUpdate;
    end;
  End;
end;

procedure TForm1.Edit3Change(Sender: TObject);
begin
  If (ListBox.ItemIndex >= 0) then
  Begin
    ListBox.Items[ListBox.ItemIndex] := Edit3.Text;
    GLGuiLayout1.GuiComponents.Items[ListBox.ItemIndex].Name := Edit3.Text;
  End;
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
   Application.Terminate;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Add1Click(Sender);
end;

procedure TForm1.Edit3KeyPress(Sender: TObject; var Key: Char);
begin
  If Key = #13 then Add1Click(Sender);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Edit2Click(Sender);
end;

end.
