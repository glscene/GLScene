unit GBE.PlayerPosition;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Math.Vectors,
  FMX.Types,
  FMX.Controls3D,
  FMX.Objects3D;

type
  TGBETypePosition = (firstPerson, thirdPerson, other);

  TGBEPlayerPosition = class(TDummy)
  private
    fDummyOrientation, fNextPosition, fPositionDirection: TDummy;
    fCamera: TCamera;
    fTypePosition: TGBETypePosition;
    fWidth: single;
    fDepth: single;
    fHeight: single;
    function GetPositionCamera: TPoint3D;
    procedure SetPositionCamera(const Value: TPoint3D);
    function GetAngleOfView: single;
    procedure SetAngleOfView(const Value: single);
    procedure SetTypePosition(const Value: TGBETypePosition);
    procedure SetWidth(const Value: single);
    procedure SetDepth(const Value: single);
    procedure SetHeight(const Value: single);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDummyOrientation: TDummy;
    function GetCamera: TCamera;
    function GetPositionDirection: TDummy;
  published
    property PositionCameraThirdPerson: TPoint3D read GetPositionCamera
      write SetPositionCamera;
    property AngleOfView: single read GetAngleOfView write SetAngleOfView;
    property TypePosition: TGBETypePosition read fTypePosition
      write SetTypePosition;
    property NextPosition: TDummy read fNextPosition write fNextPosition;
    property HitTest default False;
    property Width: single read fWidth write SetWidth;
    property Height: single read fHeight write SetHeight;
    property Depth: single read fDepth write SetDepth;
  end;

procedure Register;

implementation // --------------------------------------------------------------

// TGBEPlayerPosition

constructor TGBEPlayerPosition.Create(AOwner: TComponent);
begin
  inherited;
  fDummyOrientation := TDummy.Create(Self);
  fDummyOrientation.Locked := true;
  fDummyOrientation.Stored := False;
  fDummyOrientation.Width := Self.Width;
  fDummyOrientation.Height := Self.Height;
  fDummyOrientation.Depth := Self.Depth;
  AddObject(fDummyOrientation);
  fCamera := TCamera.Create(Self);
  fCamera.Parent := fDummyOrientation;

  fNextPosition := TDummy.Create(Self);
  fNextPosition.Locked := true;
  fNextPosition.Stored := False;
  fNextPosition.Width := Self.Width;
  fNextPosition.Height := Self.Height;
  fNextPosition.Depth := Self.Depth;

  fPositionDirection := TDummy.Create(Self);
  fPositionDirection.Locked := true;
  fPositionDirection.Stored := False;
  fPositionDirection.Width := Self.Width;
  fPositionDirection.Height := Self.Height;
  fPositionDirection.Depth := Self.Depth;
  fPositionDirection.Parent := fDummyOrientation;

  fPositionDirection.position.X := 0;
  fPositionDirection.position.Y := 0;
  fPositionDirection.position.Z := -0.01;

  fTypePosition := TGBETypePosition.thirdPerson;
end;

destructor TGBEPlayerPosition.Destroy;
begin
  DoDeleteChildren;
  inherited;
end;

function TGBEPlayerPosition.GetAngleOfView: single;
begin
  result := fCamera.AngleOfView;
end;

function TGBEPlayerPosition.GetCamera: TCamera;
begin
  result := fCamera;
end;

function TGBEPlayerPosition.GetPositionDirection: TDummy;
begin
  result := fPositionDirection;
end;

function TGBEPlayerPosition.GetDummyOrientation: TDummy;
begin
  result := fDummyOrientation;
end;

function TGBEPlayerPosition.GetPositionCamera: TPoint3D;
begin
  result := fCamera.position.Point;
end;

procedure TGBEPlayerPosition.SetAngleOfView(const Value: single);
begin
  fCamera.AngleOfView := Value;
end;

procedure TGBEPlayerPosition.SetDepth(const Value: single);
begin
  fDepth := Value;
  fDummyOrientation.Depth := Value;
  fNextPosition.Depth := Value;
  fPositionDirection.Depth := Value;
end;

procedure TGBEPlayerPosition.SetHeight(const Value: single);
begin
  fHeight := Value;
  fDummyOrientation.Height := Value;
  fNextPosition.Height := Value;
  fPositionDirection.Height := Value;
end;

procedure TGBEPlayerPosition.SetPositionCamera(const Value: TPoint3D);
begin
  fCamera.position.Point := Value;
end;

procedure TGBEPlayerPosition.SetTypePosition(const Value: TGBETypePosition);
begin
  fTypePosition := Value;
  case Value of
    firstPerson:
      begin
        fCamera.position.Point := Point3D(0, 0, 0);
        fCamera.Target := nil;
      end;
    thirdPerson:
      begin
        fCamera.position.Point := Point3D(0, -1, -3);
        fCamera.Target := Self;
      end;
    other:
      begin
        fCamera.Target := nil;
      end;
  end;
end;

procedure TGBEPlayerPosition.SetWidth(const Value: single);
begin
  fWidth := Value;
  fDummyOrientation.Width := Value;
  fNextPosition.Width := Value;
  fPositionDirection.Width := Value;
end;

procedure Register;
begin
  RegisterComponents('GXScene GBE', [TGBEPlayerPosition]);
end;

end.
