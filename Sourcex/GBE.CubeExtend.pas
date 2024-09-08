unit GBE.CubeExtend;

interface

uses
  System.SysUtils,
  System.Classes,
  FMX.Types,
  FMX.Controls3D,
  FMX.Objects3D,
  FMX.MaterialSources;

type
  TGBECubeExtend = class(TDummy)
  private
    FMaterialSourceFaceFront, FMaterialSourceFaceRight, FMaterialSourceFaceBack,
      FMaterialSourceFaceLeft, FMaterialSourceFaceTop, FMaterialSourceFaceBottom
      : TMaterialSource;
    fSubdivisionsWidth, fSubdivisionsHeight, fSubdivisionsDepth: integer;
    fFaceFront, fFaceRight, fFaceBack, fFaceLeft, fFaceTop, fFaceBottom: TPlane;
    fFaceFrontVisible, fFaceRightVisible, fFaceBackVisible, fFaceLeftVisible,
      fFaceTopVisible, fFaceBottomVisible: boolean;
    fWidth, fDepth, fHeight: single;
    // Materials
    procedure SetMaterialSourceFaceFront(const Value: TMaterialSource);
    procedure SetMaterialSourceFaceBack(const Value: TMaterialSource);
    procedure SetMaterialSourceFaceBottom(const Value: TMaterialSource);
    procedure SetMaterialSourceFaceLeft(const Value: TMaterialSource);
    procedure SetMaterialSourceFaceRight(const Value: TMaterialSource);
    procedure SetMaterialSourceFaceTop(const Value: TMaterialSource);
    // Subdivisions
    procedure SetSubdivisionsDepth(const Value: integer);
    procedure SetSubdivisionsHeight(const Value: integer);
    procedure SetSubdivisionsWidth(const Value: integer);
    // Faces
    procedure SetFaceFrontVisible(const Value: boolean);
    procedure SetFaceRightVisible(const Value: boolean);
    procedure SetFaceBackVisible(const Value: boolean);
    procedure SetFaceLeftVisible(const Value: boolean);
    procedure SetFaceTopVisible(const Value: boolean);
    procedure SetFaceBottomVisible(const Value: boolean);
    // Drawing
    procedure DrawCube;
    procedure SetDepth(const Value: single);
    procedure SetHeight(const Value: single);
    procedure SetWidth(const Value: single);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property SubdivisionsDepth: integer read fSubdivisionsDepth
      write SetSubdivisionsDepth;
    property SubdivisionsHeight: integer read fSubdivisionsHeight
      write SetSubdivisionsHeight;
    property SubdivisionsWidth: integer read fSubdivisionsWidth
      write SetSubdivisionsWidth;
    property MaterialSourceFaceFront: TMaterialSource
      read FMaterialSourceFaceFront write SetMaterialSourceFaceFront;
    property MaterialSourceFaceRight: TMaterialSource
      read FMaterialSourceFaceRight write SetMaterialSourceFaceRight;
    property MaterialSourceFaceBack: TMaterialSource
      read FMaterialSourceFaceBack write SetMaterialSourceFaceBack;
    property MaterialSourceFaceLeft: TMaterialSource
      read FMaterialSourceFaceLeft write SetMaterialSourceFaceLeft;
    property MaterialSourceFaceTop: TMaterialSource read FMaterialSourceFaceTop
      write SetMaterialSourceFaceTop;
    property MaterialSourceFaceBottom: TMaterialSource
      read FMaterialSourceFaceBottom write SetMaterialSourceFaceBottom;
    property Width: single read fWidth write SetWidth;
    property Height: single read fHeight write SetHeight;
    property Depth: single read fDepth write SetDepth;
    property FaceFrontVisible: boolean read fFaceFrontVisible
      write SetFaceFrontVisible;
    property FaceRightVisible: boolean read fFaceRightVisible
      write SetFaceRightVisible;
    property FaceBackVisible: boolean read fFaceBackVisible
      write SetFaceBackVisible;
    property FaceLeftVisible: boolean read fFaceLeftVisible
      write SetFaceLeftVisible;
    property FaceTopVisible: boolean read fFaceTopVisible
      write SetFaceTopVisible;
    property FaceBottomVisible: boolean read fFaceBottomVisible
      write SetFaceBottomVisible;
  end;

procedure Register;

implementation // -------------------------------------------------------------

// TGBECube

constructor TGBECubeExtend.Create(AOwner: TComponent);
begin
  inherited;
  fWidth := 1;
  fHeight := 1;
  fDepth := 1;
  fSubdivisionsHeight := 1;
  fSubdivisionsWidth := 1;
  fSubdivisionsDepth := 1;

  fFaceFront := TPlane.Create(nil);
  fFaceFront.Parent := self;
  fFaceFront.Stored := false;
  fFaceFront.HitTest := false;
  fFaceFront.Locked := true;

  fFaceRight := TPlane.Create(nil);
  fFaceRight.Parent := self;
  fFaceRight.Stored := false;
  fFaceRight.HitTest := false;
  fFaceRight.Locked := true;

  fFaceBack := TPlane.Create(nil);
  fFaceBack.Parent := self;
  fFaceBack.Stored := false;
  fFaceBack.HitTest := false;
  fFaceBack.Locked := true;

  fFaceLeft := TPlane.Create(nil);
  fFaceLeft.Parent := self;
  fFaceLeft.Stored := false;
  fFaceLeft.HitTest := false;
  fFaceLeft.Locked := true;

  fFaceTop := TPlane.Create(nil);
  fFaceTop.Parent := self;
  fFaceTop.Stored := false;
  fFaceTop.HitTest := false;
  fFaceTop.Locked := true;

  fFaceBottom := TPlane.Create(nil);
  fFaceBottom.Parent := self;
  fFaceBottom.Stored := false;
  fFaceBottom.HitTest := false;
  fFaceBottom.Locked := true;

  fFaceFrontVisible := true;
  fFaceRightVisible := true;
  fFaceBackVisible := true;
  fFaceLeftVisible := true;
  fFaceTopVisible := true;
  fFaceBottomVisible := true;

  DrawCube;
end;

destructor TGBECubeExtend.Destroy;
begin
  DeleteChildren;
  inherited;
end;

procedure TGBECubeExtend.DrawCube;
begin

  fFaceFront.Visible := fFaceFrontVisible;
  if fFaceFrontVisible then
  begin
    fFaceFront.SubdivisionsHeight := fSubdivisionsHeight;
    fFaceFront.SubdivisionsWidth := fSubdivisionsWidth;
  end
  else
  begin
    fFaceFront.SubdivisionsHeight := 1;
    fFaceFront.SubdivisionsWidth := 1;
  end;

  fFaceRight.Visible := fFaceRightVisible;
  if fFaceRightVisible then
  begin
    fFaceRight.SubdivisionsHeight := fSubdivisionsHeight;
    fFaceRight.SubdivisionsWidth := fSubdivisionsWidth;
  end
  else
  begin
    fFaceRight.SubdivisionsHeight := 1;
    fFaceRight.SubdivisionsWidth := 1;
  end;

  fFaceBack.Visible := fFaceBackVisible;
  if fFaceBackVisible then
  begin
    fFaceBack.SubdivisionsHeight := fSubdivisionsHeight;
    fFaceBack.SubdivisionsWidth := fSubdivisionsWidth;
  end
  else
  begin
    fFaceBack.SubdivisionsHeight := 1;
    fFaceBack.SubdivisionsWidth := 1;
  end;

  fFaceLeft.Visible := fFaceLeftVisible;
  if fFaceLeftVisible then
  begin
    fFaceLeft.SubdivisionsHeight := fSubdivisionsHeight;
    fFaceLeft.SubdivisionsWidth := fSubdivisionsWidth;
  end
  else
  begin
    fFaceLeft.SubdivisionsHeight := 1;
    fFaceLeft.SubdivisionsWidth := 1;
  end;

  fFaceTop.Visible := fFaceTopVisible;
  if fFaceTopVisible then
  begin
    fFaceTop.SubdivisionsHeight := fSubdivisionsHeight;
    fFaceTop.SubdivisionsWidth := fSubdivisionsWidth;
  end
  else
  begin
    fFaceTop.SubdivisionsHeight := 1;
    fFaceTop.SubdivisionsWidth := 1;
  end;

  fFaceBottom.Visible := fFaceBottomVisible;
  if fFaceBottomVisible then
  begin
    fFaceBottom.SubdivisionsHeight := fSubdivisionsHeight;
    fFaceBottom.SubdivisionsWidth := fSubdivisionsWidth;
  end
  else
  begin
    fFaceBottom.SubdivisionsHeight := 1;
    fFaceBottom.SubdivisionsWidth := 1;
  end;

  fFaceFront.Position.X := 0;
  fFaceFront.Position.Y := 0;
  fFaceFront.Position.Z := -Depth * 0.5;

  fFaceRight.Position.X := Width * 0.5;
  fFaceRight.Position.Y := 0;
  fFaceRight.Position.Z := 0;
  fFaceRight.RotationAngle.Y := -90;

  fFaceBack.Position.X := 0;
  fFaceBack.Position.Y := 0;
  fFaceBack.Position.Z := Depth * 0.5;
  fFaceBack.RotationAngle.Y := 180;

  fFaceLeft.Position.X := -Width * 0.5;
  fFaceLeft.Position.Y := 0;
  fFaceLeft.Position.Z := 0;
  fFaceLeft.RotationAngle.Y := 90;

  fFaceTop.Position.X := 0;
  fFaceTop.Position.Y := -Height * 0.5;
  fFaceTop.Position.Z := 0;
  fFaceTop.RotationAngle.X := -90;

  fFaceBottom.Position.X := 0;
  fFaceBottom.Position.Y := Height * 0.5;
  fFaceBottom.Position.Z := 0;
  fFaceBottom.RotationAngle.X := 90;
end;

procedure TGBECubeExtend.SetDepth(const Value: single);
begin
  fDepth := Value;
  fFaceFront.Depth := Value;
  fFaceRight.Width := Value;
  fFaceBack.Depth := Value;
  fFaceLeft.Width := Value;
  fFaceTop.Height := Value;
  fFaceBottom.Height := Value;
  DrawCube;
end;

procedure TGBECubeExtend.SetFaceBackVisible(const Value: boolean);
begin
  fFaceBackVisible := Value;
  DrawCube;
end;

procedure TGBECubeExtend.SetFaceBottomVisible(const Value: boolean);
begin
  fFaceBottomVisible := Value;
  DrawCube;
end;

procedure TGBECubeExtend.SetFaceFrontVisible(const Value: boolean);
begin
  fFaceFrontVisible := Value;
  DrawCube;
end;

procedure TGBECubeExtend.SetFaceLeftVisible(const Value: boolean);
begin
  fFaceLeftVisible := Value;
  DrawCube;
end;

procedure TGBECubeExtend.SetFaceRightVisible(const Value: boolean);
begin
  fFaceRightVisible := Value;
  DrawCube;
end;

procedure TGBECubeExtend.SetFaceTopVisible(const Value: boolean);
begin
  fFaceTopVisible := Value;
  DrawCube;
end;

procedure TGBECubeExtend.SetHeight(const Value: single);
begin
  fHeight := Value;
  fFaceFront.Height := Value;
  fFaceRight.Height := Value;
  fFaceBack.Height := Value;
  fFaceLeft.Height := Value;
  DrawCube;
end;

procedure TGBECubeExtend.SetMaterialSourceFaceBack
  (const Value: TMaterialSource);
begin
  FMaterialSourceFaceBack := Value;
  fFaceBack.MaterialSource := Value;
end;

procedure TGBECubeExtend.SetMaterialSourceFaceBottom
  (const Value: TMaterialSource);
begin
  FMaterialSourceFaceBottom := Value;
  fFaceBottom.MaterialSource := Value;
end;

procedure TGBECubeExtend.SetMaterialSourceFaceFront
  (const Value: TMaterialSource);
begin
  FMaterialSourceFaceFront := Value;
  fFaceFront.MaterialSource := Value;
end;

procedure TGBECubeExtend.SetMaterialSourceFaceLeft
  (const Value: TMaterialSource);
begin
  FMaterialSourceFaceLeft := Value;
  fFaceLeft.MaterialSource := Value;
end;

procedure TGBECubeExtend.SetMaterialSourceFaceRight
  (const Value: TMaterialSource);
begin
  FMaterialSourceFaceRight := Value;
  fFaceRight.MaterialSource := Value;
end;

procedure TGBECubeExtend.SetMaterialSourceFaceTop(const Value: TMaterialSource);
begin
  FMaterialSourceFaceTop := Value;
  fFaceTop.MaterialSource := Value;
end;

procedure TGBECubeExtend.SetSubdivisionsDepth(const Value: integer);
begin
  fSubdivisionsDepth := Value;
  fFaceRight.SubdivisionsWidth := Value;
  fFaceLeft.SubdivisionsWidth := Value;
  fFaceTop.SubdivisionsHeight := Value;
  fFaceBottom.SubdivisionsHeight := Value;
  DrawCube;
end;

procedure TGBECubeExtend.SetSubdivisionsHeight(const Value: integer);
begin
  fSubdivisionsHeight := Value;
  fFaceFront.SubdivisionsHeight := Value;
  fFaceBack.SubdivisionsHeight := Value;
  fFaceLeft.SubdivisionsHeight := Value;
  fFaceRight.SubdivisionsHeight := Value;
  DrawCube;
end;

procedure TGBECubeExtend.SetSubdivisionsWidth(const Value: integer);
begin
  fSubdivisionsWidth := Value;
  fFaceFront.SubdivisionsWidth := Value;
  fFaceBack.SubdivisionsWidth := Value;
  fFaceTop.SubdivisionsWidth := Value;
  fFaceBottom.SubdivisionsWidth := Value;
  DrawCube;
end;

procedure TGBECubeExtend.SetWidth(const Value: single);
begin
  fWidth := Value;
  fFaceFront.Width := Value;
  fFaceRight.Depth := Value;
  fFaceBack.Width := Value;
  fFaceLeft.Depth := Value;
  fFaceTop.Width := Value;
  fFaceBottom.Width := Value;
  DrawCube;
end;

// ----------------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('GXScene GBE', [TGBECubeExtend]);
end;

end.
