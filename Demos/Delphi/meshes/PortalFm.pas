unit PortalFm;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Types,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Grids,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Imaging.Jpeg,

  GLS.Scene,
  GLS.VectorTypes,
  GLS.Texture,
  GLS.VectorFileObjects,
  GLS.PersistentClasses,
  GLS.Objects,
  GLS.Cadencer,
  GLS.Portal,
  GLS.SceneViewer,
 
  GLS.Material,
  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.Keyboard,
  GLS.Utils;

type
  TFormPortal = class(TForm)
    Label1: TLabel;
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    Label2: TLabel;
    BUForward: TButton;
    BUTurnLeft: TButton;
    BUTurnRight: TButton;
    BUBackward: TButton;
    SGMap: TStringGrid;
    GLMaterialLibrary1: TGLMaterialLibrary;
    BBProcess: TButton;
    GLLightSource1: TGLLightSource;
    DummyCube1: TGLDummyCube;
    GLCamera1: TGLCamera;
    Timer1: TTimer;
    GLCadencer1: TGLCadencer;
    Portal1: TGLPortal;
    Label3: TLabel;
    CBAuto: TCheckBox;
    CBFog: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure BBProcessClick(Sender: TObject);
    procedure BUTurnLeftClick(Sender: TObject);
    procedure BUTurnRightClick(Sender: TObject);
    procedure BUForwardClick(Sender: TObject);
    procedure BUBackwardClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure SGMapSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
    procedure CBFogClick(Sender: TObject);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private

    mx, my: Integer;
  public

    portalCount, triangleCount: Integer;
  end;

var
  FormPortal: TFormPortal;

implementation

{$R *.DFM}

procedure TFormPortal.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  SetGLSceneMediaDir();
  for i := 0 to 15 do
    SGMap.Cells[i, i] := 'X';
  SGMap.Cells[8, 8] := '';
  SGMap.Col := 8;
  SGMap.Row := 12;
  with GLMaterialLibrary1 do
  begin
    AddTextureMaterial('gnd', 'walkway.jpg');
    with AddTextureMaterial('wall', 'rawwall.jpg') do
    begin
      TextureScale.Y := 3;
    end;
  end;
  BBProcessClick(Self);
end;

procedure TFormPortal.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  GLCamera1.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
end;

procedure TFormPortal.BBProcessClick(Sender: TObject);
var
  X, Y, n: Integer;
  h: Single;
  Sector: TGLSectorMeshObject;
  Poly: TFGPolygon;
begin
  h := 3;
  portalCount := 0;
  triangleCount := 0;
  Portal1.MeshObjects.Clear;
  for X := -7 to 8 do
    for Y := -7 to 8 do
    begin
      Sector := TGLSectorMeshObject.CreateOwned(Portal1.MeshObjects);
      with Sector.Vertices do
      begin
        n := Count;
        Add(X, 0, Y);
        Add(X + 1, 0, Y);
        Add(X + 1, 0, Y + 1);
        Add(X, 0, Y + 1);
        Add(X, h, Y);
        Add(X + 1, h, Y);
        Add(X + 1, h, Y + 1);
        Add(X, h, Y + 1);
      end;
      with Sector.TexCoords do
      begin
        Add(0, 0, 0);
        Add(1, 0, 0);
        Add(1, 1, 0);
        Add(0, 1, 0);
      end;
      // ground
      Sector.Normals.Add(0, 1, 0);
      if SGMap.Cells[X + 7, Y + 7] = '' then
      begin
        Poly := TFGPolygon.CreateOwned(Sector.FaceGroups);
        with Poly do
        begin
          MaterialName := 'gnd';
          Add(n + 0, 0, 0);
          Add(n + 3, 0, 3);
          Add(n + 2, 0, 2);
          Add(n + 1, 0, 1);
        end;
      end;
      // front wall
      Sector.Normals.Add(0, 0, 1);
      if (Y = -7) or (SGMap.Cells[X + 7, Y - 1 + 7] <> '') then
      begin
        Poly := TFGPolygon.CreateOwned(Sector.FaceGroups);
        Poly.MaterialName := 'wall';
        Inc(triangleCount, 2);
      end
      else
      begin
        Poly := TFGPortalPolygon.CreateOwned(Sector.FaceGroups);
        TFGPortalPolygon(Poly).DestinationSectorIndex := (X + 7) * 16 +
          (Y - 1 + 7);
        Inc(portalCount);
      end;
      with Poly do
      begin
        Add(n + 0, 1, 3);
        Add(n + 1, 1, 2);
        Add(n + 5, 1, 1);
        Add(n + 4, 1, 0);
      end;
      // left wall
      Sector.Normals.Add(1, 0, 0);
      if (X = -7) or (SGMap.Cells[X - 1 + 7, Y + 7] <> '') then
      begin
        Poly := TFGPolygon.CreateOwned(Sector.FaceGroups);
        Poly.MaterialName := 'wall';
        Inc(triangleCount, 2);
      end
      else
      begin
        Poly := TFGPortalPolygon.CreateOwned(Sector.FaceGroups);
        TFGPortalPolygon(Poly).DestinationSectorIndex := (X - 1 + 7) * 16
          + (Y + 7);
        Inc(portalCount);
      end;
      with Poly do
      begin
        Add(n + 4, 2, 1);
        Add(n + 7, 2, 0);
        Add(n + 3, 2, 3);
        Add(n + 0, 2, 2);
      end;
      // right wall
      Sector.Normals.Add(-1, 0, 0);
      if (X = 8) or (SGMap.Cells[X + 1 + 7, Y + 7] <> '') then
      begin
        Poly := TFGPolygon.CreateOwned(Sector.FaceGroups);
        Poly.MaterialName := 'wall';
        Inc(triangleCount, 2);
      end
      else
      begin
        Poly := TFGPortalPolygon.CreateOwned(Sector.FaceGroups);
        TFGPortalPolygon(Poly).DestinationSectorIndex := (X + 1 + 7) * 16
          + (Y + 7);
        Inc(portalCount);
      end;
      with Poly do
      begin
        Add(n + 1, 3, 3);
        Add(n + 2, 3, 2);
        Add(n + 6, 3, 1);
        Add(n + 5, 3, 0);
      end;
      // back wall
      Sector.Normals.Add(0, 0, 1);
      if (Y = 8) or (SGMap.Cells[X + 7, Y + 1 + 7] <> '') then
      begin
        Poly := TFGPolygon.CreateOwned(Sector.FaceGroups);
        Poly.MaterialName := 'wall';
        Inc(triangleCount, 2);
      end
      else
      begin
        Poly := TFGPortalPolygon.CreateOwned(Sector.FaceGroups);
        TFGPortalPolygon(Poly).DestinationSectorIndex := (X + 7) * 16 +
          (Y + 1 + 7);
        Inc(portalCount);
      end;
      with Poly do
      begin
        Add(n + 3, 4, 2);
        Add(n + 7, 4, 1);
        Add(n + 6, 4, 0);
        Add(n + 2, 4, 3);
      end;
    end;
  Portal1.StructureChanged;
end;

procedure TFormPortal.BUTurnLeftClick(Sender: TObject);
begin
  DummyCube1.Turn(-15);
  GLCamera1.TransformationChanged;
end;

procedure TFormPortal.BUTurnRightClick(Sender: TObject);
begin
  DummyCube1.Turn(+15);
  GLCamera1.TransformationChanged;
end;

procedure TFormPortal.BUForwardClick(Sender: TObject);
begin
  DummyCube1.Move(-0.25);
  GLCamera1.TransformationChanged;
end;

procedure TFormPortal.BUBackwardClick(Sender: TObject);
begin
  DummyCube1.Move(0.25);
  GLCamera1.TransformationChanged;
end;

procedure TFormPortal.Timer1Timer(Sender: TObject);
begin
  Caption := Format('%.2f FPS - %d Portals - %d Triangles',
    [GLSceneViewer1.FramesPerSecond, portalCount, triangleCount]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TFormPortal.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  if IsKeyDown('Z') or IsKeyDown('W') then
    DummyCube1.Move(-3 * deltaTime)
  else if IsKeyDown('S') then
    DummyCube1.Move(3 * deltaTime);
  if IsKeyDown('A') or IsKeyDown('Q') then
    DummyCube1.Turn(-60 * deltaTime)
  else if IsKeyDown('D') then
    DummyCube1.Turn(60 * deltaTime);
  GLCamera1.TransformationChanged;
end;

procedure TFormPortal.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

procedure TFormPortal.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if ssLeft in Shift then
  begin
    GLCamera1.MoveAroundTarget(my - Y, mx - X);
  end;
  mx := X;
  my := Y;
end;

procedure TFormPortal.SGMapSetEditText(Sender: TObject; ACol, ARow: Integer;
  const Value: String);
begin
  if CBAuto.Checked then
    BBProcessClick(Self);
end;

procedure TFormPortal.CBFogClick(Sender: TObject);
begin
  if CBFog.Checked then
    GLCamera1.DepthOfView := 11
  else
    GLCamera1.DepthOfView := 100;
  GLSceneViewer1.Buffer.FogEnable := CBFog.Checked;
end;

end.
