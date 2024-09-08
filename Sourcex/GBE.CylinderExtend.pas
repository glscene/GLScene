unit GBE.CylinderExtend;

interface

uses
  System.SysUtils,
  System.Classes,
  FMX.Types,
  FMX.Controls3D,
  FMX.Objects3D,
  FMX.Types3D,
  FMX.MaterialSources,
  System.Math.Vectors;

type
  TCustomMeshHelper = class(TCustomMesh);

  TGBECylinderExtend = class(TCylinder)
  private
    fDiskTop, fDiskBottom: TDisk;
    FMaterialSourceTop, FMaterialSourceBottom: TMaterialSource;
    procedure CreateGBECylinder;
    procedure SetMaterialSourceBottom(const Value: TMaterialSource);
    procedure SetMaterialSourceTop(const Value: TMaterialSource);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render; override;
  published
    property MaterialSourceTop: TMaterialSource read FMaterialSourceTop
      write SetMaterialSourceTop;
    property MaterialSourceBottom: TMaterialSource read FMaterialSourceBottom
      write SetMaterialSourceBottom;
  end;

procedure Register;

implementation // --------------------------------------------------------------

// TMesh1

constructor TGBECylinderExtend.Create(AOwner: TComponent);
begin
  inherited;
  CreateGBECylinder;
end;

procedure TGBECylinderExtend.CreateGBECylinder;
begin
  fDiskTop := TDisk.Create(nil);
  fDiskTop.Locked := true;
  fDiskTop.Stored := false;
  fDiskTop.SubdivisionsAxes := SubdivisionsAxes;
  fDiskTop.SubdivisionsCap := SubdivisionsCap;
  fDiskTop.Parent := self;
  fDiskTop.Position.Y := -fHeight * 0.5;
  fDiskTop.Height := fHeight;
  fDiskTop.Width := fWidth;
  fDiskTop.Depth := fDepth;
  fDiskTop.MaterialSource := MaterialSourceTop;

  fDiskBottom := TDisk.Create(nil);
  fDiskBottom.Locked := true;
  fDiskBottom.Stored := false;
  fDiskBottom.SubdivisionsAxes := SubdivisionsAxes;
  fDiskBottom.SubdivisionsCap := SubdivisionsCap;
  fDiskBottom.Parent := self;
  fDiskBottom.Position.Y := fHeight * 0.5 + 0.001;
  fDiskBottom.Height := fHeight;
  fDiskBottom.Width := fWidth;
  fDiskBottom.Depth := fDepth;
  fDiskBottom.MaterialSource := MaterialSourceBottom;
end;

destructor TGBECylinderExtend.Destroy;
begin
  DoDeleteChildren;
  inherited;
end;

procedure TGBECylinderExtend.Render;
begin
  inherited;

  fDiskTop.SubdivisionsAxes := SubdivisionsAxes;
  fDiskTop.SubdivisionsCap := SubdivisionsCap;
  fDiskTop.Position.Y := -fHeight * 0.5;
  fDiskTop.Height := fHeight;
  fDiskTop.Width := fWidth;
  fDiskTop.Depth := fDepth;

  fDiskBottom.SubdivisionsAxes := SubdivisionsAxes;
  fDiskBottom.SubdivisionsCap := SubdivisionsCap;
  fDiskBottom.Position.Y := fHeight * 0.5 + 0.001;
  fDiskBottom.Height := fHeight;
  fDiskBottom.Width := fWidth;
  fDiskBottom.Depth := fDepth;
end;

procedure TGBECylinderExtend.SetMaterialSourceBottom
  (const Value: TMaterialSource);
begin
  FMaterialSourceBottom := Value;
  fDiskBottom.MaterialSource := FMaterialSourceBottom;
end;

procedure TGBECylinderExtend.SetMaterialSourceTop(const Value: TMaterialSource);
begin
  FMaterialSourceTop := Value;
  fDiskTop.MaterialSource := FMaterialSourceTop;
end;

// ---------------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('GXScene GBE', [TGBECylinderExtend]);
end;

end.
