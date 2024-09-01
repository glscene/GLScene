//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.EParticleMasksManager;

(*
   A pretty particle mask effect manager.
   Original Header:

   This unit is part of GLE - GLScene Game Utilities Engine set by Kenneth Poulter difacane@telkomsa.net
   Module Number: 37

   Description: This is merely an addon to GXS.Scene, since i don't want to edit GLScene's source code directly
                and make changes (since GXScene's source code constantly changes). What the manager does
                is to provide a basic tool for newly created particles to be modified (their position currently).
                Their position is set from 3 different masks, which create a "virtual" 3d object... meaning,
                an actual 3d object is not created, but an outline for particles or any other objects are positioned.

   ActualUsage: Create the component, create a new ParticleMask, set the material library, set the materials,
                and use the procedures provided in the managers root. positioning and scaling applicable as well.

                The images should be

   Licenses: Removed. Donated to GXScene's Code Base as long as the author (Kenneth Poulter) is not altered in this file.
             Theft of code also is not allowed, although alterations are allowed.
 *)

interface

{$I GXS.Scene.inc}

uses
  System.Types,
  System.SysUtils,
  System.Classes,
  System.UITypes,
  FMX.Graphics,

  GXS.Color,
  GXS.Texture,
  GXS.Material,
  GXS.Scene,
  GXS.VectorGeometry,
  GXS.VectorTypes,
  GXS.ParticleFX,
  GXS.Coordinates;

type

  TgxEProjectedParticleMask = (pptXMask, pptYMask, pptZMask);

  TgxEParticleMask = class;
  TgxEParticleMasks = class;

  TgxEParticleMask = class(TCollectionItem, IgxMaterialLibrarySupported)
  private
    FName: string;
    FScale: TgxCoordinates;
    FPosition: TgxCoordinates;
    FYMask: TgxLibMaterialName;
    FZMask: TgxLibMaterialName;
    FXMask: TgxLibMaterialName;
    FMaterialLibrary: TgxMaterialLibrary;
    FBackgroundColor: TColor;
    FMaskColor: TColor;
    FMaxX, FMaxY, FMaxZ, FMinX, FMinY, FMinZ: Integer;
    IXW, IXH, IYW, IYH, IZW, IZH: Integer;
    LX, LY, LZ: Integer;
    MX, MY: Integer;
    BogusMask, BogusMaskX, BogusMaskY, BogusMaskZ: Boolean;
    // we might have a pitch mask
    FRollAngle: Single;
    FPitchAngle: Single;
    FTurnAngle: Single;
    procedure SetName(const Value: string);
    procedure SetXMask(const Value: TgxLibMaterialName);
    procedure SetYMask(const Value: TgxLibMaterialName);
    procedure SetZMask(const Value: TgxLibMaterialName);
    procedure SetMaterialLibrary(const Value: TgxMaterialLibrary);
    function XCan: TBitmap;
    function YCan: TBitmap;
    function ZCan: TBitmap;
    //implementing IGLMaterialLibrarySupported
    function GetMaterialLibrary: TgxAbstractMaterialLibrary;
    //implementing IInterface
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure UpdateExtents;
    procedure Roll(Angle: Single);
    procedure Turn(Angle: Single);
    procedure Pitch(Angle: Single);
    // this generates a xmask from another mask just to fill gaps,
    // depth is dependant on frommask width and height
    procedure GenerateMaskFromProjection(FromMask, ToMask:
      TgxEProjectedParticleMask; Depth: Integer);
  published
    // scales and positions
    property Scale: TgxCoordinates read FScale write FScale;
    property Position: TgxCoordinates read FPosition write FPosition;
    // the reference name of the particle mask
    property Name: string read FName write SetName;
    property MaterialLibrary: TgxMaterialLibrary read FMaterialLibrary write
      SetMaterialLibrary;
    // mask images, make sure materiallibrary is assigned
    property XMask: TgxLibMaterialName read FXMask write SetXMask;
    property YMask: TgxLibMaterialName read FYMask write SetYMask;
    property ZMask: TgxLibMaterialName read FZMask write SetZMask;
    // background color is the color that prevents particles from being positioned there
    property BackgroundColor: TColor read FBackgroundColor write
      FBackgroundColor;
    // maskcolor is where particles are allowed to be positioned
    property MaskColor: TColor read FMaskColor write FMaskColor;
    // just the average angles for orientation
    property RollAngle: Single read FRollAngle write FRollAngle;
    property PitchAngle: Single read FPitchAngle write FPitchAngle;
    property TurnAngle: Single read FTurnAngle write FTurnAngle;
  end;

  TgxEParticleMasks = class(TCollection)
  protected
    Owner: TComponent;
    function GetOwner: TPersistent; override;
    procedure SetItems(Index: Integer; const Val: TgxEParticleMask);
    function GetItems(Index: Integer): TgxEParticleMask;
  public
    function Add: TgxEParticleMask;
    constructor Create(AOwner: TComponent);
    property Items[Index: Integer]: TgxEParticleMask read GetItems write
      SetItems; default;
  end;

  TgxEParticleMasksManager = class(TComponent)
  private
    FParticleMasks: TgxEParticleMasks;
  protected
    procedure ApplyOrthoGraphic(var Vec: TVector3f; Mask: TgxEParticleMask);
    procedure ApplyRotation(var Vec: TVector3f; Mask: TgxEParticleMask);
    procedure ApplyRotationTarget(var Vec: TVector3f; Mask: TgxEParticleMask;
      TargetObject: TgxBaseSceneObject);
    procedure ApplyScaleAndPosition(var Vec: TVector3f; Mask: TgxEParticleMask);
    procedure ApplyScaleAndPositionTarget(var Vec: TVector3f; Mask:
      TgxEParticleMask; TargetObject: TgxBaseSceneObject);
    procedure FindParticlePosition(var Vec: TVector3f; Mask: TgxEParticleMask);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CreateParticlePositionFromMask(MaskName: string): TVector3f;
    function TargetParticlePositionFromMask(TargetObject: TgxBaseSceneObject;
      MaskName: string): TVector3f;
    procedure SetParticlePositionFromMask(Particle: TgxParticle; MaskName:
      string);
    procedure SetParticlePositionFromMaskTarget(Particle: TgxParticle; MaskName:
      string; TargetObject: TgxBaseSceneObject);
    function ParticleMaskByName(MaskName: string): TgxEParticleMask;
  published
    property ParticleMasks: TgxEParticleMasks read FParticleMasks write
      FParticleMasks;
  end;

//----------------------------------------
implementation
//----------------------------------------

//----------------------------------------
// TgxEParticleMasks
//----------------------------------------

function TgxEParticleMasks.Add: TgxEParticleMask;
begin
  Result := (inherited Add) as TgxEParticleMask;
end;

constructor TgxEParticleMasks.Create(AOwner: TComponent);
begin
  inherited Create(TgxEParticleMask);
  Owner := AOwner;
end;

function TgxEParticleMasks.GetItems(Index: Integer): TgxEParticleMask;
begin
  Result := TgxEParticleMask(inherited Items[Index]);
end;

function TgxEParticleMasks.GetOwner: TPersistent;
begin
  Result := Owner;
end;

procedure TgxEParticleMasks.SetItems(Index: Integer; const Val:
  TgxEParticleMask);
begin
  inherited Items[Index] := Val;
end;

//----------------------------------------
// TgxEParticleMask
//----------------------------------------

procedure TgxEParticleMask.Assign(Source: TPersistent);
begin
  if Source is TgxEParticleMask then
  begin
    FScale.Assign(TgxEParticleMask(Source).FScale);
    FPosition.Assign(TgxEParticleMask(Source).FPosition);
    FMaterialLibrary := TgxEParticleMask(Source).FMaterialLibrary;
    FXMask := TgxEParticleMask(Source).FXMask;
    FYMask := TgxEParticleMask(Source).FYMask;
    FZMask := TgxEParticleMask(Source).FZMask;
  end
  else
    inherited Assign(Source);
end;

constructor TgxEParticleMask.Create(Collection: TCollection);
begin
  inherited Create(Collection);

  FName := 'ParticleMask' + IntToStr(ID);

  FScale := TgxCoordinates.CreateInitialized(Self, XYZHMGVector, csPoint);
  FPosition := TgxCoordinates.CreateInitialized(Self, NullHmgPoint, csPoint);
  FMaterialLibrary := nil;

  FMaskColor := TColorRec.White;
  FBackGroundColor := TColorRec.Black;

  FTurnAngle := 0;
  FRollAngle := 0;
  FPitchAngle := 0;

  FXMask := '';
  FYMask := '';
  FZMask := '';

  UpdateExtents;
end;

destructor TgxEParticleMask.Destroy;
begin
  FScale.Free;
  FPosition.Free;
  FMaterialLibrary := nil;
  FBackgroundColor := TColorRec.Black;
  FMaskColor := TColorRec.White;
  FXMask := '';
  FYMask := '';
  FZMask := '';
  inherited Destroy;
end;

procedure TgxEParticleMask.GenerateMaskFromProjection(FromMask,
  ToMask: TgxEProjectedParticleMask; Depth: Integer);
var
  FromBitMap: TBitmap;
  ToBitMap: TBitmap;
  X, Y: Integer;
  Rect: TRect;
begin
  FromBitMap := nil;
  ToBitMap := nil;
  if not assigned(FMaterialLibrary) then
    Exit;
  if FromMask = ToMask then
    Exit; // we can't project to the same mask
  if Depth < 0 then
    Exit;
  case FromMask of
    pptXMask: FromBitMap := XCan;
    pptYMask: FromBitMap := YCan;
    pptZMask: FromBitMap := ZCan;
  end;
  if (FromBitMap.Width = 0) and (FromBitMap.Height = 0) then
    Exit; // we can't use something that has no image
  case ToMask of
    pptXMask: ToBitMap := XCan;
    pptYMask: ToBitMap := YCan;
    pptZMask: ToBitMap := ZCan;
  end;

  ToBitMap.Width := FromBitMap.Width;
  ToBitMap.Height := FromBitMap.Height;

  // TODO : E2003 Undeclared identifier: 'Pen'
  (*
  ToBitMap.Canvas.Pen.Color := FBackgroundColor;
  ToBitMap.Canvas.Pen.Style := psSolid;
  ToBitMap.Canvas.Brush.Color := FBackgroundColor;
  ToBitMap.Canvas.Brush.Style := bsSolid;
  *)

  Rect.Left := 0;
  Rect.Top := 0;
  Rect.Right := ToBitMap.Width;
  Rect.Bottom := ToBitMap.Height;

  { TODO : E2250 There is no overloaded version of 'FillRect' that can be called with these arguments }
  (*ToBitMap.Canvas.FillRect(Rect);*)

  { TODO : E2003 Undeclared identifier: 'Pen' }
  (*
  ToBitMap.Canvas.Pen.Color := FMaskColor;
  ToBitMap.Canvas.Brush.Color := FMaskColor;
  *)
  for X := 0 to ToBitMap.Width do
    for Y := 0 to ToBitMap.Height do
    begin
      // from x mask
      if (FromMask = pptXMask) and (ToMask = pptYMask) then
      // TODO : E2003 Undeclared identifier: 'Pixels'
        (*
      if FromBitMap.Canvas.Pixels[X, Y] = FMaskColor then
        begin
          ToBitMap.Canvas.MoveTo(((FromBitmap.Width - Depth) div 2), X);
          ToBitMap.Canvas.LineTo(((FromBitmap.Width + Depth) div 2), X);
        end;
      if (FromMask = pptXMask) and (ToMask = pptZMask) then
        if FromBitMap.Canvas.Pixels[X, Y] = FMaskColor then
        begin
          ToBitMap.Canvas.MoveTo(((FromBitmap.Width - Depth) div 2), Y);
          ToBitMap.Canvas.LineTo(((FromBitmap.Width + Depth) div 2), Y);
        end;
      // from y mask
      if (FromMask = pptYMask) and (ToMask = pptXMask) then
        if FromBitMap.Canvas.Pixels[X, Y] = FMaskColor then
        begin
          ToBitMap.Canvas.MoveTo(Y, ((FromBitmap.Height - Depth) div 2));
          ToBitMap.Canvas.LineTo(Y, ((FromBitmap.Height + Depth) div 2));
        end;
      if (FromMask = pptYMask) and (ToMask = pptZMask) then
        if FromBitMap.Canvas.Pixels[X, Y] = FMaskColor then
        begin
          ToBitMap.Canvas.MoveTo(X, ((FromBitmap.Height - Depth) div 2));
          ToBitMap.Canvas.LineTo(X, ((FromBitmap.Height + Depth) div 2));
        end;
      // from z mask
      if (FromMask = pptZMask) and (ToMask = pptXMask) then
        if FromBitMap.Canvas.Pixels[X, Y] = FMaskColor then
        begin
          ToBitMap.Canvas.MoveTo(((FromBitmap.Width - Depth) div 2), Y);
          ToBitMap.Canvas.LineTo(((FromBitmap.Width + Depth) div 2), Y);
        end;
      if (FromMask = pptZMask) and (ToMask = pptYMask) then
        if FromBitMap.Canvas.Pixels[X, Y] = FMaskColor then
        begin
          ToBitMap.Canvas.MoveTo(X, ((FromBitmap.Height - Depth) div 2));
          ToBitMap.Canvas.LineTo(X, ((FromBitmap.Height + Depth) div 2));
        end;
       *)
    end;

  UpdateExtents;
end;

function TgxEParticleMask.GetDisplayName: string;
begin
  Result := '';
  if FName <> '' then
    Result := FName
  else
    Result := 'TgxEParticleMask';
end;

function TgxEParticleMask.GetMaterialLibrary: TgxAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

procedure TgxEParticleMask.Pitch(Angle: Single);
begin
  FPitchAngle := FPitchAngle + Angle;
end;

procedure TgxEParticleMask.Roll(Angle: Single);
begin
  FRollAngle := FRollAngle + Angle;
end;

procedure TgxEParticleMask.SetMaterialLibrary(const Value: TgxMaterialLibrary);
begin
  FMaterialLibrary := Value;
  UpdateExtents;
end;

procedure TgxEParticleMask.SetName(const Value: string);
var
  I: Integer;
begin
  for I := 1 to Length(Value) do
    if Value[I] = ' ' then
    begin
      raise Exception.Create('Cannot contain spaces or special characters.');
      Exit;
    end;
  FName := Value;
end;

procedure TgxEParticleMask.SetXMask(const Value: TgxLibMaterialName);
begin
  FXMask := Value;
  if assigned(FMaterialLibrary) then
    if not assigned(FMaterialLibrary.LibMaterialByName(FXMask)) then
    begin
      XCan.Width := 0;
      XCan.Height := 0;
    end;
  UpdateExtents;
end;

procedure TgxEParticleMask.SetYMask(const Value: TgxLibMaterialName);
begin
  FYMask := Value;
  if assigned(FMaterialLibrary) then
    if not assigned(FMaterialLibrary.LibMaterialByName(FYMask)) then
    begin
      YCan.Width := 0;
      YCan.Height := 0;
    end;
  UpdateExtents;
end;

procedure TgxEParticleMask.SetZMask(const Value: TgxLibMaterialName);
begin
  FZMask := Value;
  if assigned(FMaterialLibrary) then
    if not assigned(FMaterialLibrary.LibMaterialByName(FZMask)) then
    begin
      ZCan.Width := 0;
      ZCan.Height := 0;
    end;
  UpdateExtents;
end;

procedure TgxEParticleMask.Turn(Angle: Single);
begin
  FTurnAngle := FTurnAngle + Angle;
end;

procedure TgxEParticleMask.UpdateExtents;
var
  MinXX, MinXY, MinYX, MinYY, MinZX, MinZY: Integer;
  MaxXX, MaxXY, MaxYX, MaxYY, MaxZX, MaxZY: Integer;
  X, Y: Integer;
begin

  FMinX := 0; // min extents
  FMinY := 0;
  FMinZ := 0;
  FMaxX := 0; // max extents
  FMaxY := 0;
  FMaxZ := 0;

  IXW := 0; // widths
  IYW := 0;
  IZW := 0;
  IXH := 0; // heights
  IYH := 0;
  IZH := 0;

  MinXX := 0; // min plane mask extents
  MinXY := 0;
  MinYX := 0;
  MinYY := 0;
  MinZX := 0;
  MinZY := 0;

  MaxXX := 0; // max plane mask extents
  MaxXY := 0;
  MaxYX := 0;
  MaxYY := 0;
  MaxZX := 0;
  MaxZY := 0;

  BogusMask := True; // prevents system locks
  BogusMaskX := True;
  BogusMaskY := True;
  BogusMaskZ := True;

  // we don't find it? no point in continuing
  if not assigned(FMaterialLibrary) then
    Exit;

  // it is recommended to have 3 different masks
  // if there is only 2, the 3rd image will just take the largest extents and use them...
  // creating not a very good effect

  if XCan <> nil then
  begin
    IXW := XCan.Width;
    IXH := XCan.Height;
  end;

  if YCan <> nil then
  begin
    IYW := YCan.Width;
    IYH := YCan.Height;
  end;

  if ZCan <> nil then
  begin
    IZW := ZCan.Width;
    IZH := ZCan.Height;
  end;

  // we find the largest dimensions of each image and give them to min mask extents so we work backwards

  MX := MaxInteger(MaxInteger(IXW, IYW), IZW);
  MY := MaxInteger(MaxInteger(IXH, IYH), IZH);

  if XCan <> nil then
  begin
    MinXX := MX;
    MinXY := MY;
  end;
  if YCan <> nil then
  begin
    MinYX := MX;
    MinYY := MY;
  end;
  if ZCan <> nil then
  begin
    MinZX := MX;
    MinZY := MY;
  end;

  // this is where we work backwards from to find the max size of the dimensions...
  // in a sense, this provides information for the randomizing, and speeds up the code
  for X := 0 to MX do
    for Y := 0 to MY do
    begin
      if XCan <> nil then
        if (X <= XCan.Width) and (Y <= XCan.Height) then
          // TODO : E2003 Undeclared identifier: 'Pixels'
          (*if (XCan.Canvas.Pixels[X, Y] = FMaskColor) then*)
          begin
            if X > MaxXX then
              MaxXX := X;
            if Y > MaxXY then
              MaxXY := Y;
            if X < MinXX then
              MinXX := X;
            if X < MinXY then
              MinXY := Y;
            BogusMaskX := False;
          end;
      if YCan <> nil then
        if (X <= YCan.Width) and (Y <= YCan.Height) then
         // TODO : E2003 Undeclared identifier: 'Pixels'
         (* if (YCan.Canvas.Pixels[X, Y] = FMaskColor) then*)
          begin
            if X > MaxYX then
              MaxYX := X;
            if Y > MaxYY then
              MaxYY := Y;
            if X < MinYX then
              MinYX := X;
            if X < MinYY then
              MinYY := Y;
            BogusMaskY := False;
          end;
      if ZCan <> nil then
        if (X <= ZCan.Width) and (Y <= ZCan.Height) then
         // TODO : E2003 Undeclared identifier: 'Pixels'
         (*if (ZCan.Canvas.Pixels[X, Y] = FMaskColor) then*)
          begin
            if X > MaxZX then
              MaxZX := X;
            if Y > MaxZY then
              MaxZY := Y;
            if X < MinZX then
              MinZX := X;
            if X < MinZY then
              MinZY := Y;
            BogusMaskZ := False;
          end;
    end;

  BogusMask := (BogusMaskX or BogusMaskY or BogusMaskZ);

  // here we find our 3d extents from a 1st angle orthographic shape

  FMinX := MinInteger(MinZX, MinYX);
  FMinY := MinInteger(MinXY, MinZY);
  FMinZ := MinInteger(MinXX, MinYY);

  FMaxX := MaxInteger(MaxZX, MaxYX);
  FMaxY := MaxInteger(MaxXY, MaxZY);
  FMaxZ := MaxInteger(MaxXX, MaxYY);

  // this is the largest mask image sizes converted to orthographic and extents... used later on

  LX := MaxInteger(IZW, IYW);
  LY := MaxInteger(IXH, IZH);
  LZ := MaxInteger(IXW, IYH);
end;

function TgxEParticleMask.XCan: TBitmap;
begin
  Result := nil;
  if not assigned(FMaterialLibrary) then
    Exit;
  if not assigned(FMaterialLibrary.LibMaterialByName(FXMask)) then
    Exit;
  if FMaterialLibrary.LibMaterialByName(FXMask).Material.Texture.ImageClassName
    <> TgxPersistentImage.ClassName then
    Exit;

  Result :=
    TBitmap((FMaterialLibrary.LibMaterialByName(FXMask).Material.Texture.Image as
    TgxPersistentImage).Picture.Bitmap);
end;

function TgxEParticleMask.YCan: TBitmap;
begin
  Result := nil;
  if not assigned(FMaterialLibrary) then
    Exit;
  if not assigned(FMaterialLibrary.LibMaterialByName(FYMask)) then
    Exit;
  if FMaterialLibrary.LibMaterialByName(FYMask).Material.Texture.ImageClassName
    <> TgxPersistentImage.ClassName then
    Exit;

  Result :=
    TBitmap((FMaterialLibrary.LibMaterialByName(FYMask).Material.Texture.Image as
    TgxPersistentImage).Picture.Bitmap);
end;

function TgxEParticleMask.ZCan: TBitmap;
begin
  Result := nil;
  if not assigned(FMaterialLibrary) then
    Exit;
  if not assigned(FMaterialLibrary.LibMaterialByName(FZMask)) then
    Exit;
  if FMaterialLibrary.LibMaterialByName(FZMask).Material.Texture.ImageClassName
    <> TgxPersistentImage.ClassName then
    Exit;

  Result :=
    TBitmap((FMaterialLibrary.LibMaterialByName(FZMask).Material.Texture.Image as
    TgxPersistentImage).Picture.Bitmap);
end;

function TgxEParticleMask.QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TgxEParticleMask._AddRef: Integer; stdcall;
begin
  Result := -1; //ignore
end;

function TgxEParticleMask._Release: Integer; stdcall;
begin
  Result := -1; //ignore
end;

//----------------------------------------
// TgxEParticleMasksManager
//----------------------------------------

procedure TgxEParticleMasksManager.ApplyOrthoGraphic(var Vec: TVector3f; Mask:
  TgxEParticleMask);
begin
  Vec.X := (Mask.LX / 2 - Vec.X) / Mask.LX;
  Vec.Y := (Mask.LY / 2 - Vec.Y) / Mask.LY;
  Vec.Z := (Mask.LZ / 2 - Vec.Z) / Mask.LZ;
end;

procedure TgxEParticleMasksManager.ApplyRotation(var Vec: TVector3f; Mask:
  TgxEParticleMask);
begin
  Vec := VectorRotateAroundX(Vec, DegToRadian(Mask.FPitchAngle));
  Vec := VectorRotateAroundY(Vec, DegToRadian(Mask.FTurnAngle));
  Vec := VectorRotateAroundZ(Vec, DegToRadian(Mask.FRollAngle));
end;

procedure TgxEParticleMasksManager.ApplyRotationTarget(var Vec: TVector3f; Mask:
  TgxEParticleMask; TargetObject: TgxBaseSceneObject);
begin

  Vec := VectorRotateAroundX(Vec, DegToRadian(Mask.FPitchAngle +
    TargetObject.Rotation.X));
  Vec := VectorRotateAroundY(Vec, DegToRadian(Mask.FTurnAngle +
    TargetObject.Rotation.Y));
  Vec := VectorRotateAroundZ(Vec, DegToRadian(Mask.FRollAngle +
    TargetObject.Rotation.Z));
end;

procedure TgxEParticleMasksManager.ApplyScaleAndPosition(var Vec: TVector3f;
  Mask: TgxEParticleMask);
begin
  Vec.X := Vec.X * Mask.FScale.DirectX + Mask.FPosition.DirectX;
  Vec.Y := Vec.Y * Mask.FScale.DirectY + Mask.FPosition.DirectY;
  Vec.Z := Vec.Z * Mask.FScale.DirectZ + Mask.FPosition.DirectZ;
end;

procedure TgxEParticleMasksManager.ApplyScaleAndPositionTarget(var Vec:
  TVector3f; Mask: TgxEParticleMask; TargetObject: TgxBaseSceneObject);
begin
  Vec.X := Vec.X * Mask.FScale.DirectX * TargetObject.Scale.DirectX +
    Mask.FPosition.DirectX + TargetObject.AbsolutePosition.X;
  Vec.Y := Vec.Y * Mask.FScale.DirectY * TargetObject.Scale.DirectY +
    Mask.FPosition.DirectY + TargetObject.AbsolutePosition.Y;
  Vec.Z := Vec.Z * Mask.FScale.DirectZ * TargetObject.Scale.DirectZ +
    Mask.FPosition.DirectZ + TargetObject.AbsolutePosition.Z;
end;

constructor TgxEParticleMasksManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParticleMasks := TgxEParticleMasks.Create(Self);
end;

function TgxEParticleMasksManager.CreateParticlePositionFromMask(MaskName:
  string): TVector3f;
var
  Mask: TgxEParticleMask;
begin
  Result := NullVector;

  Mask := ParticleMaskByName(MaskName);

  if not assigned(Mask) then
    Exit;
  if Mask.BogusMask then
    Exit;

  // finds the particle position on the masks
  FindParticlePosition(Result, Mask);
  // this converts 1st angle orthographic to 3rd angle orthograhic
  ApplyOrthoGraphic(Result, Mask);
  // this just turns it accordingly
  ApplyRotation(Result, Mask);
  // this applies the scales and positioning
  ApplyScaleAndPosition(Result, Mask);
end;

destructor TgxEParticleMasksManager.Destroy;
begin
  FParticleMasks.Destroy;
  inherited Destroy;
end;

procedure TgxEParticleMasksManager.FindParticlePosition(var Vec: TVector3f;
  Mask: TgxEParticleMask);
var
  X, Y, Z: Integer;
begin
 // TODO : E2003 Undeclared identifier: 'Pixels'
 (*
  repeat
    X := Random(Mask.FMaxX - Mask.FMinX) + Mask.FMinX;
    Y := Random(Mask.FMaxY - Mask.FMinY) + Mask.FMinY;
    Z := Random(Mask.FMaxZ - Mask.FMinZ) + Mask.FMinZ;
  until (Mask.XCan.Canvas.Pixels[Z, Y] = Mask.FMaskColor) and
    (Mask.YCan.Canvas.Pixels[X, Z] = Mask.FMaskColor) and
    (Mask.ZCan.Canvas.Pixels[X, Y] = Mask.FMaskColor);
  *)
  MakeVector(Vec, X, Y, Z);
end;

function TgxEParticleMasksManager.ParticleMaskByName(MaskName: string):
  TgxEParticleMask;
var
  I: Integer;
begin
  Result := nil;
  if FParticleMasks.Count > 0 then
    for I := 0 to FParticleMasks.Count - 1 do
      if FParticleMasks.Items[I].FName = MaskName then
        Result := FParticleMasks.Items[I];
end;

procedure TgxEParticleMasksManager.SetParticlePositionFromMask(
  Particle: TgxParticle; MaskName: string);
begin
  if not assigned(Particle) then
    Exit;
  Particle.Position := CreateParticlePositionFromMask(MaskName);
end;

procedure TgxEParticleMasksManager.SetParticlePositionFromMaskTarget(
  Particle: TgxParticle; MaskName: string; TargetObject: TgxBaseSceneObject);
begin
  if not assigned(Particle) then
    Exit;

  Particle.Position := TargetParticlePositionFromMask(TargetObject, MaskName);
end;

function TgxEParticleMasksManager.TargetParticlePositionFromMask(
  TargetObject: TgxBaseSceneObject; MaskName: string): TVector3f;
var
  Mask: TgxEParticleMask;
begin

  Result := NullVector;
  if not assigned(TargetObject) then
    Exit;

  Mask := ParticleMaskByName(MaskName);
  if not assigned(Mask) then
    Exit;

  if Mask.BogusMask then
    Exit;

  // finds the particle position on the masks
  FindParticlePosition(Result, Mask);
  // this converts 1st angle orthographic to 3rd angle orthograhic
  ApplyOrthoGraphic(Result, Mask);
  // this just turns it accordingly
  ApplyRotationTarget(Result, Mask, TargetObject);
  // this applies the scales and positioning
  ApplyScaleAndPositionTarget(Result, Mask, TargetObject);
end;

end.

