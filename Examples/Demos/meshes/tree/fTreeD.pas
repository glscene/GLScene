unit fTreeD;

interface

uses
  Winapi.OpenGL,
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Menus,
  Vcl.ExtDlgs,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.StdCtrls,

  GLS.Scene,
  GLS.Objects,
  GLS.SceneViewer,
  GLS.Tree,
  GLS.Texture,
  GLS.VectorFileObjects,
  GLS.AsyncTimer,
  GLS.Cadencer,

  GLS.Material,
  GLS.Coordinates,
  GLS.BaseClasses,
  Stage.Utils,
  GLS.FileTGA;

type
  TFormTree = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    GLMaterialLibrary1: TGLMaterialLibrary;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    Label3: TLabel;
    TrackBar3: TTrackBar;
    Label4: TLabel;
    TrackBar4: TTrackBar;
    Label5: TLabel;
    TrackBar5: TTrackBar;
    Label6: TLabel;
    TrackBar6: TTrackBar;
    Label7: TLabel;
    TrackBar7: TTrackBar;
    Label8: TLabel;
    TrackBar8: TTrackBar;
    Label9: TLabel;
    TrackBar9: TTrackBar;
    TrackBar10: TTrackBar;
    Label10: TLabel;
    GLFreeForm1: TGLFreeForm;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    LoadTree1: TMenuItem;
    SaveTree1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    NewTree1: TMenuItem;
    Material1: TMenuItem;
    LeafFrontTexture1: TMenuItem;
    LeafBackTexture1: TMenuItem;
    BranchTexture1: TMenuItem;
    N2: TMenuItem;
    ExportMesh1: TMenuItem;
    Label11: TLabel;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    GLPlane1: TGLPlane;
    Label12: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    CheckBox1: TCheckBox;
    TrackBar11: TTrackBar;
    SaveDialog2: TSaveDialog;
    SaveDialog3: TSaveDialog;
    ExportMaterialLibrary1: TMenuItem;
    OpenPictureDialog1: TOpenPictureDialog;
    Label13: TLabel;
    TrackBar12: TTrackBar;
    AsyncTimer1: TGLAsyncTimer;
    GLCadencer1: TGLCadencer;
    miFPS: TMenuItem;
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure TrackBar3Change(Sender: TObject);
    procedure TrackBar4Change(Sender: TObject);
    procedure TrackBar5Change(Sender: TObject);
    procedure TrackBar6Change(Sender: TObject);
    procedure TrackBar7Change(Sender: TObject);
    procedure TrackBar8Change(Sender: TObject);
    procedure TrackBar9Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TrackBar10Change(Sender: TObject);
    procedure NewTree1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure LoadTree1Click(Sender: TObject);
    procedure SaveTree1Click(Sender: TObject);
    procedure ExportMesh1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure LeafFrontTexture1Click(Sender: TObject);
    procedure LeafBackTexture1Click(Sender: TObject);
    procedure BranchTexture1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure TrackBar11Change(Sender: TObject);
    procedure ExportMaterialLibrary1Click(Sender: TObject);
    procedure TrackBar12Change(Sender: TObject);
    procedure AsyncTimer1Timer(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: Double);
  public
    mx, my: Integer;
    GLTree1: TGLTree;
    procedure AlignControlsToTree;
    procedure NewTree;
  end;

var
  FormTree: TFormTree;

implementation

{$R *.dfm}

procedure TFormTree.AlignControlsToTree;
begin
  TrackBar1.Position := GLTree1.Depth;
  TrackBar2.Position := Round(GLTree1.BranchTwist);
  TrackBar3.Position := Round(GLTree1.BranchAngle * 100);
  TrackBar4.Position := Round(GLTree1.BranchAngleBias * 100);
  TrackBar5.Position := Round(GLTree1.BranchSize * 10);
  TrackBar6.Position := Round(GLTree1.BranchRadius * 25);
  TrackBar7.Position := Round(GLTree1.BranchNoise * 100);
  TrackBar8.Position := Round(GLTree1.LeafSize * 100);
  TrackBar9.Position := Round(GLTree1.LeafThreshold * 100);
  TrackBar10.Position := GLTree1.BranchFacets;
  Edit1.Text := IntToStr(GLTree1.Seed);
  CheckBox1.Checked := GLTree1.CentralLeader;
  TrackBar11.Position := Round(GLTree1.CentralLeaderBias * 100);
  GLTree1.AutoRebuild := True;
  GLTree1.RebuildTree;
end;

procedure TFormTree.NewTree;
begin
  GLTree1.Free;
  GLTree1 := TGLTree(GLScene1.Objects.AddNewChild(TGLTree));
  GLTree1.AutoRebuild := False;
  with GLTree1 do
  begin
    MaterialLibrary := GLMaterialLibrary1;
    LeafMaterialName := 'LeafFront';
    LeafBackMaterialName := 'LeafBack';
    BranchMaterialName := 'Branch';
    Depth := 6;
    LeafSize := 0.2;
    BranchRadius := 0.08;
    BranchNoise := 0.5;

    Randomize;
    Seed := Round((2 * Random - 1) * (MaxInt - 1));
  end;
  AlignControlsToTree;
end;

// Start up

procedure TFormTree.FormCreate(Sender: TObject);
begin
  var Path: TFileName := GetCurrentAssetPath();
  SetCurrentDir(Path + '\texture');

  // Set up default textures
  with GLMaterialLibrary1.AddTextureMaterial('LeafFront', 'maple_multi.tga') do
  begin
    Material.BlendingMode := bmAlphaTest50;
    Material.Texture.TextureMode := tmModulate;
    Material.Texture.TextureFormat := tfRGBA;
  end;
  with GLMaterialLibrary1.AddTextureMaterial('LeafBack', 'maple_multi.tga') do
  begin
    Material.BlendingMode := bmAlphaTest50;
    Material.Texture.TextureMode := tmModulate;
    Material.Texture.TextureFormat := tfRGBA;
  end;
  with GLMaterialLibrary1.AddTextureMaterial('Branch', 'zbark_016.jpg') do
    Material.Texture.TextureMode := tmModulate;

  // Set a up a tree
  NewTree;
end;

// Camera controls
procedure TFormTree.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

procedure TFormTree.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
    GLCamera1.MoveAroundTarget(my - Y, mx - X)
  else if ssRight in Shift then
    GLCamera1.AdjustDistanceToTarget(1 + (my - Y) * 0.01);
  mx := X;
  my := Y;
end;

// Tree controls
procedure TFormTree.TrackBar1Change(Sender: TObject);
begin
  GLTree1.Depth := Integer(TrackBar1.Position);
end;

procedure TFormTree.TrackBar2Change(Sender: TObject);
begin
  GLTree1.BranchTwist := Integer(TrackBar2.Position);
end;

procedure TFormTree.TrackBar3Change(Sender: TObject);
begin
  GLTree1.BranchAngle := TrackBar3.Position / 100;
end;

procedure TFormTree.TrackBar4Change(Sender: TObject);
begin
  GLTree1.BranchAngleBias := TrackBar4.Position / 100;
end;

procedure TFormTree.TrackBar5Change(Sender: TObject);
begin
  GLTree1.BranchSize := TrackBar5.Position / 10;
end;

procedure TFormTree.TrackBar6Change(Sender: TObject);
begin
  GLTree1.BranchRadius := TrackBar6.Position / 25;
end;

procedure TFormTree.TrackBar7Change(Sender: TObject);
begin
  GLTree1.BranchNoise := TrackBar7.Position / 100;
end;

procedure TFormTree.TrackBar8Change(Sender: TObject);
begin
  GLTree1.LeafSize := TrackBar8.Position / 100;
end;

procedure TFormTree.TrackBar9Change(Sender: TObject);
begin
  GLTree1.LeafThreshold := TrackBar9.Position / 100;
end;

procedure TFormTree.TrackBar10Change(Sender: TObject);
begin
  GLTree1.BranchFacets := Integer(TrackBar10.Position);
end;

procedure TFormTree.TrackBar11Change(Sender: TObject);
begin
  GLTree1.CentralLeaderBias := TrackBar11.Position / 100;
end;

procedure TFormTree.TrackBar12Change(Sender: TObject);
begin
  GLTree1.CenterBranchConstant := TrackBar12.Position / 100;
end;

procedure TFormTree.Button1Click(Sender: TObject);
begin
  try
    GLTree1.Seed := StrToInt(Edit1.Text);
  except
    on E: Exception do
    begin
      Application.MessageBox('Invalid seed value. Resetting.', 'Error', MB_OK);
      Edit1.Text := IntToStr(GLTree1.Seed);
    end;
  end;
end;

procedure TFormTree.CheckBox1Click(Sender: TObject);
begin
  GLTree1.CentralLeader := CheckBox1.Checked;
end;

// Menu options

procedure TFormTree.NewTree1Click(Sender: TObject);
begin
  NewTree;
end;

procedure TFormTree.LoadTree1Click(Sender: TObject);
begin
  if not OpenDialog1.Execute then
    exit;

  GLTree1.LoadFromFile(OpenDialog1.FileName);
  AlignControlsToTree;
end;

procedure TFormTree.SaveTree1Click(Sender: TObject);
begin
  if not SaveDialog1.Execute then
    exit;

  GLTree1.SaveToFile(SaveDialog1.FileName);
end;

procedure TFormTree.ExportMesh1Click(Sender: TObject);
begin
  if not SaveDialog2.Execute then
    exit;

  GLTree1.BuildMesh(GLFreeForm1);
  GLFreeForm1.SaveToFile(SaveDialog2.FileName);
end;

procedure TFormTree.ExportMaterialLibrary1Click(Sender: TObject);
begin
  if not SaveDialog3.Execute then
    exit;

  GLMaterialLibrary1.SaveToFile(SaveDialog3.FileName);
end;

procedure TFormTree.Exit1Click(Sender: TObject);
begin
  FormTree.Close;
end;

procedure TFormTree.LeafFrontTexture1Click(Sender: TObject);
begin
  if not OpenPictureDialog1.Execute then
    exit;

  with GLMaterialLibrary1.Materials.GetLibMaterialByName('LeafFront') do
    Material.Texture.Image.LoadFromFile(OpenPictureDialog1.FileName);
  GLTree1.StructureChanged;
end;

procedure TFormTree.LeafBackTexture1Click(Sender: TObject);
begin
  if not OpenPictureDialog1.Execute then
    exit;

  with GLMaterialLibrary1.Materials.GetLibMaterialByName('LeafBack') do
    Material.Texture.Image.LoadFromFile(OpenPictureDialog1.FileName);
  GLTree1.StructureChanged;
end;

procedure TFormTree.BranchTexture1Click(Sender: TObject);
begin
  if not OpenPictureDialog1.Execute then
    exit;

  with GLMaterialLibrary1.Materials.GetLibMaterialByName('Branch') do
    Material.Texture.Image.LoadFromFile(OpenPictureDialog1.FileName);
  GLTree1.StructureChanged;
end;

procedure TFormTree.AsyncTimer1Timer(Sender: TObject);
begin
  miFPS.Caption := 'Tree Editor - ' + GLSceneViewer1.FramesPerSecondText;
end;

procedure TFormTree.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: Double);
begin
  GLSceneViewer1.Invalidate;
end;

end.
