unit fTilesD;

interface

uses
  Winapi.OpenGL,
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.Imaging.Jpeg,
  Vcl.StdCtrls,

  GLS.VectorTypes,
  GLS.Objects,
  GLS.Graph,
  GLS.Scene,
  GLS.SceneViewer,
  GLS.VectorGeometry,
  GLS.TilePlane,
  GLS.Texture,
  GLS.Cadencer,
  GLS.Context,
 
  GLS.Material,
  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.RenderContextInfo,
  GLS.TextureFormat,
  GLS.Keyboard,
  GLS.Utils, GLS.SimpleNavigation;

type
  TFormTiles = class(TForm)
    GLScene: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera: TGLCamera;
    DCTarget: TGLDummyCube;
    GLXYZGrid: TGLXYZGrid;
    Panel1: TPanel;
    GLMaterialLibrary: TGLMaterialLibrary;
    GLLightSource: TGLLightSource;
    Timer1: TTimer;
    GLCadencer1: TGLCadencer;
    Label1: TLabel;
    CBMaterial: TComboBox;
    GLTilePlane: TGLTilePlane;
    GLDirectOpenGL: TGLDirectOpenGL;
    DCSelection: TGLDummyCube;
    GLLines1: TGLLines;
    BUPack: TButton;
    Label2: TLabel;
    CBShowGrid: TCheckBox;
    CBSortByMaterials: TCheckBox;
    GLSimpleNavigation1: TGLSimpleNavigation;
    GLDummyCube1: TGLDummyCube;
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure GLDirectOpenGLRender(Sender: TObject;
      var rci: TGLRenderContextInfo);
    procedure BUPackClick(Sender: TObject);
    procedure CBShowGridClick(Sender: TObject);
    procedure CBSortByMaterialsClick(Sender: TObject);
  private
     
  public
     
    mx, my: Integer;
    tileX, tileY: Integer;
    mip, translateOffset: TGLVector;
    translating: Boolean;
  end;

var
  FormTiles: TFormTiles;

implementation

{$R *.dfm}

procedure TFormTiles.FormCreate(Sender: TObject);
var
  i, j: Integer;
begin
  SetGLSceneMediaDir();
  GLMaterialLibrary.TexturePaths := GetCurrentDir();
  GLMaterialLibrary.LibMaterialByName('beigemarble').Material.Texture.Image.LoadFromFile('beigemarble.jpg');
  GLMaterialLibrary.LibMaterialByName('marbletiles').Material.Texture.Image.LoadFromFile('marbletiles.jpg');
  GLMaterialLibrary.LibMaterialByName('walkway').Material.Texture.Image.LoadFromFile('walkway.jpg');

  // fill the tiled area with random tiles
  RandSeed := 0;
  for i := -20 to 20 do
    for j := -20 to 20 do
      GLTilePlane.Tiles[i, j] :=
        Random(GLMaterialLibrary.Materials.Count - 1) + 1;

  // set all tile materials to anisotropic,
  // add them to the material selection combo
  for i := 0 to GLMaterialLibrary.Materials.Count - 1 do
  begin
    GLMaterialLibrary.Materials[i].Material.Texture.FilteringQuality :=
      tfAnisotropic;
    CBMaterial.Items.Add(GLMaterialLibrary.Materials[i].Name);
  end;
  CBMaterial.ItemIndex := 0;
end;

procedure TFormTiles.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
  if Shift = [ssLeft] then
  begin
    GLTilePlane.Tiles[tileX, tileY] := CBMaterial.ItemIndex;
    GLTilePlane.StructureChanged;
  end
  else if Shift = [ssRight] then
  begin
    GLTilePlane.Tiles[tileX, tileY] := 0;
    GLTilePlane.StructureChanged;
  end;
end;

procedure TFormTiles.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  GLCamera.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
end;

procedure TFormTiles.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
var
  ip: TGLVector;
  mp: TPoint;
  shiftDown: Boolean;
begin
  shiftDown := (IsKeyDown(VK_LSHIFT) or IsKeyDown(VK_RSHIFT));
  DCSelection.Visible := not shiftDown;
  if DCSelection.Visible then
    GLSceneViewer1.Cursor := crDefault
  else
    GLSceneViewer1.Cursor := crHandPoint;

  GetCursorPos(mp);
  mp := GLSceneViewer1.ScreenToClient(mp);
  if PtInRect(GLSceneViewer1.ClientRect, mp) then
  begin
    GLSceneViewer1.Buffer.ScreenVectorIntersectWithPlaneXY
      (VectorMake(mp.X, GLSceneViewer1.Height - mp.Y, 0), 0, ip);
    tileX := Round(ip.X - 0.5);
    tileY := Round(ip.Y - 0.5);
    DCSelection.Position.SetPoint(tileX, tileY, 0);

    if shiftDown then
    begin
      if IsKeyDown(VK_LBUTTON) then
      begin
        if not translating then
        begin
          translateOffset := ip;
          translating := True;
        end;
        DCTarget.Position.Translate(VectorAdd(VectorSubtract(mip, ip),
          translateOffset))
      end
      else
        translating := False;
      if IsKeyDown(VK_RBUTTON) then
      begin
        GLCamera.MoveAroundTarget((my - mp.Y) * 0.5, (mx - mp.X) * 0.5);
      end;
    end
    else
    begin
      translating := False;
      if IsKeyDown(VK_LBUTTON) then
      begin
        GLTilePlane.Tiles[tileX, tileY] := CBMaterial.ItemIndex;
        GLTilePlane.StructureChanged;
      end;
      if IsKeyDown(VK_RBUTTON) then
      begin
        GLTilePlane.Tiles[tileX, tileY] := 0;
        GLTilePlane.StructureChanged;
      end;
    end;
    mx := mp.X;
    my := mp.Y;
  end;

  GLSceneViewer1.Invalidate;
end;

procedure TFormTiles.GLDirectOpenGLRender(Sender: TObject;
  var rci: TGLRenderContextInfo);
begin
  // we clear the depth buffer, so that the grid is always in front of the
  // tile plane and won't Z-Fight with it
  glClear(GL_DEPTH_BUFFER_BIT);
end;

procedure TFormTiles.BUPackClick(Sender: TObject);
begin
  // packing a tile area removes unused area from the in-memory structures
  GLTilePlane.Tiles.Pack;
end;

procedure TFormTiles.CBShowGridClick(Sender: TObject);
begin
  GLXYZGrid.Visible := CBShowGrid.Checked;
end;

procedure TFormTiles.CBSortByMaterialsClick(Sender: TObject);
begin
  GLTilePlane.SortByMaterials := CBSortByMaterials.Checked;
end;

procedure TFormTiles.Timer1Timer(Sender: TObject);
begin
  Caption := GLSceneViewer1.FramesPerSecondText;
  GLSceneViewer1.ResetPerformanceMonitor;
end;

end.
