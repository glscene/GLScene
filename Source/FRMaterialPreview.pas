//
// The graphics engine GLXEngine. The unit of GLScene for Delphi
//
unit FRMaterialPreview;

(* Material Preview frame *)

interface

{$I Stage.Defines.inc}

uses
  System.Types,
  System.Classes,
  Vcl.Graphics,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.Controls,

  GLS.Scene,
  Stage.VectorTypes,
  GLS.Objects,
  GLS.Texture,
  GLS.HUDObjects,
  GLS.SceneViewer,
  GLS.GeomObjects,
  GLS.Color,
  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.Material;

type
  TRMaterialPreview = class(TFrame)
    GLScene: TGLScene;
    GLSceneViewer: TGLSceneViewer;
    CBObject: TComboBox;
    Camera: TGLCamera;
    Cube: TGLCube;
    Sphere: TGLSphere;
    LightSource: TGLLightSource;
    CBBackground: TComboBox;
    BackGroundSprite: TGLHUDSprite;
    Teapot: TGLTeapot;
    World: TGLDummyCube;
    Light: TGLDummyCube;
    FireSphere: TGLSphere;
    GLMaterialLibrary: TGLMaterialLibrary;
   
    procedure CBObjectChange(Sender: TObject);
    procedure CBBackgroundChange(Sender: TObject);
    procedure SceneViewerMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure SceneViewerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SceneViewerMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    FLibMaterial: TGLAbstractLibMaterial;
    function GetMaterial: TGLMaterial;
    procedure SetMaterial(const Value: TGLMaterial);
    function GetLibMaterial: TGLAbstractLibMaterial;
    procedure SetLibMaterial(const Value: TGLAbstractLibMaterial);
  public
    constructor Create(AOwner: TComponent); override;
    property Material: TGLMaterial read GetMaterial write SetMaterial;
    property LibMaterial: TGLAbstractLibMaterial read GetLibMaterial write SetLibMaterial;
  end;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

{$R *.dfm}

var
  MX, MY: Integer;

constructor TRMaterialPreview.Create(AOwner: TComponent);
begin
  inherited;
  BackGroundSprite.Position.X := GLSceneViewer.Width div 2;
  BackGroundSprite.Position.Y := GLSceneViewer.Height div 2;
  BackGroundSprite.Width := GLSceneViewer.Width;
  BackGroundSprite.Height := GLSceneViewer.Height;

  CBObject.ItemIndex := 0;
  CBObjectChange(Self);
  CBBackground.ItemIndex := 0;
  CBBackgroundChange(Self);
end;

procedure TRMaterialPreview.CBObjectChange(Sender: TObject);
var
  i: Integer;
begin
  i := CBObject.ItemIndex;
  Cube.Visible := i = 0;
  Sphere.Visible := i = 1;
  Teapot.Visible := i = 2;
end;

procedure TRMaterialPreview.CBBackgroundChange(Sender: TObject);
var
  bgColor: TColor;
begin
  case CBBackground.ItemIndex of
    1: bgColor := clWhite;
    2: bgColor := clBlack;
    3: bgColor := clBlue;
    4: bgColor := clRed;
    5: bgColor := clGreen;
  else
    bgColor := clNone;
  end;
  with BackGroundSprite.Material do
  begin
    Texture.Disabled := (bgColor <> clNone);
    FrontProperties.Diffuse.Color := ConvertWinColor(bgColor);
  end;
end;

procedure TRMaterialPreview.SceneViewerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if (ssRight in Shift) and (ssLeft in Shift) then
    Camera.AdjustDistanceToTarget(1 - 0.01 * (MY - Y))
  else if (ssRight in Shift) or (ssLeft in Shift) then
    Camera.MoveAroundTarget(Y - MY, X - MX);

  MX := X;
  MY := Y;
end;

procedure TRMaterialPreview.SceneViewerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MX := X;
  MY := Y;
end;

procedure TRMaterialPreview.SceneViewerMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  Camera.AdjustDistanceToTarget(1 - 0.1 * (Abs(WheelDelta) / WheelDelta));
end;

function TRMaterialPreview.GetMaterial: TGLMaterial;
begin
  Result := GLMaterialLibrary.Materials[0].Material;
end;

procedure TRMaterialPreview.SetMaterial(const Value: TGLMaterial);
begin
  GLMaterialLibrary.Materials[0].Material.Assign(Value.GetActualPrimaryMaterial);
end;

function TRMaterialPreview.GetLibMaterial: TGLAbstractLibMaterial;
begin
  Result := FLibMaterial;
end;

procedure TRMaterialPreview.SetLibMaterial(const Value: TGLAbstractLibMaterial);
begin
  FLibMaterial := Value;
  if Assigned(FLibMaterial) then
  begin
    with GLMaterialLibrary.Materials[0] do
    begin
      Material.MaterialLibrary := FLibMaterial.MaterialLibrary;
      Material.LibMaterialName := FLibMaterial.Name
    end;
  end
  else
    with GLMaterialLibrary.Materials[0] do
    begin
      Material.MaterialLibrary := nil;
      Material.LibMaterialName := '';
    end;
end;

end.
