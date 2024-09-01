//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.AnimatedSprite;

(* A sprite that uses a scrolling texture for animation *)

interface

{$I GXS.Scene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,

  System.Classes,
  System.SysUtils,
  System.Math,

  GXS.XCollection,
  GXS.VectorTypes,
  GXS.VectorGeometry,
  GXS.PersistentClasses,
  GXS.Strings,

  GXS.Scene,
  GXS.Context,
  GXS.Material,
  GXS.RenderContextInfo,
  GXS.BaseClasses,
  GXS.State;

type
  TgxSpriteAnimFrame = class;
  TgxSpriteAnimFrameList = class;
  TgxSpriteAnimation = class;
  TgxSpriteAnimationList = class;
  TgxAnimatedSprite = class;

  (* Used by the SpriteAnimation when Dimensions are set manual. The animation
    will use the offsets, width and height to determine the texture coodinates for this frame. *)
  TgxSpriteAnimFrame = class(TXCollectionItem)
  private
    FOffsetX,
      FOffsetY,
      FWidth,
      FHeight: Integer;
    procedure DoChanged;
  protected
    procedure SetOffsetX(const Value: Integer);
    procedure SetOffsetY(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    procedure SetHeight(const Value: Integer);
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
  public
    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;
  published
    property OffsetX: Integer read FOffsetX write SetOffsetX;
    property OffsetY: Integer read FOffsetY write SetOffsetY;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
  end;

  TgxSpriteAnimFrameList = class(TXCollection)
  public
    constructor Create(aOwner: TPersistent); override;
    class function ItemsClass: TXCollectionItemClass; override;
  end;

  (* Determines if the texture coordinates are Automatically generated
     from the Animations properties or if they are Manually set through
     the Frames collection. *)
  TgxSpriteFrameDimensions = (sfdAuto, sfdManual);

  (* Used to mask the auto generated frames. The Left, Top, Right and
    Bottom properties determines the number of pixels to be cropped
    from each corresponding side of the frame. Only applicable to auto dimensions. *)
  TgxSpriteAnimMargins = class(TPersistent)
  private
    FOwner: TgxSpriteAnimation;
    FLeft, FTop, FRight, FBottom: Integer;
  protected
    procedure SetLeft(const Value: Integer);
    procedure SetTop(const Value: Integer);
    procedure SetRight(const Value: Integer);
    procedure SetBottom(const Value: Integer);
    procedure DoChanged;
  public
    constructor Create(Animation: TgxSpriteAnimation);
    property Owner: TgxSpriteAnimation read FOwner;
  published
    property Left: Integer read FLeft write SetLeft;
    property Top: Integer read FTop write SetTop;
    property Right: Integer read FRight write SetRight;
    property Bottom: Integer read FBottom write SetBottom;
  end;

  // Animations define how the texture coordinates for each offset are to be determined.
  TgxSpriteAnimation = class(TXCollectionItem, IgxMaterialLibrarySupported)
  private
    FCurrentFrame,
    FStartFrame,
    FEndFrame,
    FFrameWidth,
    FFrameHeight,
    FInterval: NativeInt;
    FFrames: TgxSpriteAnimFrameList;
    FLibMaterialName: TgxLibMaterialName;
    FLibMaterialCached: TgxLibMaterial;
    FDimensions: TgxSpriteFrameDimensions;
    FMargins: TgxSpriteAnimMargins;
    procedure DoChanged;
  protected
    procedure SetCurrentFrame(const Value: NativeInt);
    procedure SetFrameWidth(const Value: NativeInt);
    procedure SetFrameHeight(const Value: NativeInt);
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure SetDimensions(const Value: TgxSpriteFrameDimensions);
    procedure SetLibMaterialName(const val: TgxLibMaterialName);
    function GetLibMaterialCached: TgxLibMaterial;
    procedure SetInterval(const Value: NativeInt);
    procedure SetFrameRate(const Value: Single);
    function GetFrameRate: Single;
    // Implementing IGLMaterialLibrarySupported.
    function GetMaterialLibrary: TgxAbstractMaterialLibrary; virtual;
  public
    constructor Create(aOwner: TXCollection); override;
    destructor Destroy; override;
    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;
    property LibMaterialCached: TgxLibMaterial read GetLibMaterialCached;
  published
    // The current showing frame for this animation.
    property CurrentFrame: NativeInt read FCurrentFrame write SetCurrentFrame;
    // Defines the starting frame for auto dimension animations.
    property StartFrame: NativeInt read FStartFrame write FStartFrame;
    // Defines the ending frame for auto dimension animations.
    property EndFrame: NativeInt read FEndFrame write FEndFrame;
    // Width of each frame in an auto dimension animation.
    property FrameWidth: NativeInt read FFrameWidth write SetFrameWidth;
    // Height of each frame in an auto dimension animation.
    property FrameHeight: NativeInt read FFrameHeight write SetFrameHeight;
    (* The name of the lib material the sprites associated material library
       for this animation. *)
    property LibMaterialName: TgxLibMaterialName read FLibMaterialName write
      SetLibMaterialName;
    (* Manual dimension animation frames. Stores the offsets and dimensions
       for each frame in the animation. *)
    property Frames: TgxSpriteAnimFrameList read FFrames;
    // Automatic or manual texture coordinate generation.
    property Dimensions: TgxSpriteFrameDimensions read FDimensions write
      SetDimensions;
    (* The number of milliseconds between each frame in the animation.
       Will automatically calculate the FrameRate value when set.
       Will override the TgxAnimatedSprite Interval is greater than zero. *)
    property Interval: NativeInt read FInterval write SetInterval;
    (* The number of frames per second for the animation.
       Will automatically calculate the Interval value when set.
       Precision will depend on Interval since Interval has priority. *)
    property FrameRate: Single read GetFrameRate write SetFrameRate;
    // Sets cropping margins for auto dimension animations.
    property Margins: TgxSpriteAnimMargins read FMargins;
  end;

  // A collection for storing SpriteAnimation objects.
  TgxSpriteAnimationList = class(TXCollection)
  public
    constructor Create(aOwner: TPersistent); override;
    class function ItemsClass: TXCollectionItemClass; override;
  end;

  (* Sets the current animation playback mode:
     samNone - No playback, the animation does not progress.
     samPlayOnce - Plays the animation once then switches to samNone.
     samLoop - Play the animation forward in a continuous loop.
     samLoopBackward - Same as samLoop but reversed direction.
     samBounceForward - Plays forward and switches to samBounceBackward
        when EndFrame is reached.
     samBounceBackward - Plays backward and switches to samBounceForward
        when StartFrame is reached.*)
  TgxSpriteAnimationMode = (samNone, samPlayOnce, samLoop, samBounceForward,
    samBounceBackward, samLoopBackward);

  // An animated version for using offset texture coordinate animation.
  TgxAnimatedSprite = class(TgxBaseSceneObject)
  private
    FAnimations: TgxSpriteAnimationList;
    FMaterialLibrary: TgxMaterialLibrary;
    FAnimationIndex,
      FInterval,
      FRotation,
      FPixelRatio: Integer;
    FMirrorU,
      FMirrorV: Boolean;
    FAnimationMode: TgxSpriteAnimationMode;
    FCurrentFrameDelta: Double;
    FOnFrameChanged: TNotifyEvent;
    FOnEndFrameReached: TNotifyEvent;
    FOnStartFrameReached: TNotifyEvent;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure WriteAnimations(Stream: TStream);
    procedure ReadAnimations(Stream: TStream);
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure SetInterval(const val: Integer);
    procedure SetAnimationIndex(const val: Integer);
    procedure SetAnimationMode(const val: TgxSpriteAnimationMode);
    procedure SetMaterialLibrary(const val: TgxMaterialLibrary);
    procedure SetPixelRatio(const val: Integer);
    procedure SetRotation(const val: Integer);
    procedure SetMirrorU(const val: Boolean);
    procedure SetMirrorV(const val: Boolean);
    procedure SetFrameRate(const Value: Single);
    function GetFrameRate: Single;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BuildList(var rci: TgxRenderContextInfo); override;
    procedure DoProgress(const progressTime: TgxProgressTimes); override;
    // Steps the current animation to the next frame
    procedure NextFrame;
  published
    // A collection of animations. Stores the settings for animating then sprite.
    property Animations: TgxSpriteAnimationList read FAnimations;
    // The material library that stores the lib materials for the animations.
    property MaterialLibrary: TgxMaterialLibrary read FMaterialLibrary write
      SetMaterialLibrary;
    (* Sets the number of milliseconds between each frame. Will recalculate
       the Framerate when set. Will be overridden by the TgxSpriteAnimation
       Interval if it is greater than zero. *)
    property Interval: Integer read FInterval write SetInterval;
    // Index of the sprite animation to be used.
    property AnimationIndex: Integer read FAnimationIndex write
      SetAnimationIndex;
    // Playback mode for the current animation.
    property AnimationMode: TgxSpriteAnimationMode read FAnimationMode write
      SetAnimationMode;
    (* Used to automatically calculate the width and height of a sprite based
       on the size of the frame it is showing. For example, if PixelRatio is
       set to 100 and the current animation frame is 100 pixels wide it will
       set the width of the sprite to 1. If the frame is 50 pixels width the
       sprite will be 0.5 wide. *)
    property PixelRatio: Integer read FPixelRatio write SetPixelRatio;
    // Rotates the sprite (in degrees).
    property Rotation: Integer read FRotation write SetRotation;
    // Mirror the generated texture coords in the U axis.
    property MirrorU: Boolean read FMirrorU write SetMirrorU;
    // Mirror the generated texture coords in the V axis.
    property MirrorV: Boolean read FMirrorV write SetMirrorV;
    (* Sets the frames per second for the current animation. Automatically
       calculates the Interval. Precision will be restricted to the values
       of Interval since Interval takes priority. *)
    property FrameRate: Single read GetFrameRate write SetFrameRate;
    property Position;
    property Scale;
    property Visible;
    // An event fired when the animation changes to it's next frame.
    property OnFrameChanged: TNotifyEvent read FOnFrameChanged write
      FOnFrameChanged;
    // An event fired when the animation reaches the end frame.
    property OnEndFrameReached: TNotifyEvent read FOnEndFrameReached write
      FOnEndFrameReached;
    // An event fired when the animation reaches the start frame.
    property OnStartFrameReached: TNotifyEvent read FOnStartFrameReached write
      FOnStartFrameReached;
  end;

// -----------------------------------------------------------------------------
implementation
// -----------------------------------------------------------------------------

// ----------
// ---------- TgxSpriteAnimFrame ----------
// ----------
procedure TgxSpriteAnimFrame.DoChanged;
begin
  if Assigned(Owner) then
  begin
    if Assigned(Owner.Owner) then
      if Owner.Owner is TgxSpriteAnimation then
        TgxSpriteAnimation(Owner.Owner).DoChanged;
  end;
end;

class function TgxSpriteAnimFrame.FriendlyName: string;
begin
  Result := 'Frame';
end;

class function TgxSpriteAnimFrame.FriendlyDescription: string;
begin
  Result := 'Sprite Animation Frame';
end;

procedure TgxSpriteAnimFrame.WriteToFiler(writer: TWriter);
begin
  inherited;
  writer.WriteInteger(0); // Archive version number
  with writer do
  begin
    WriteInteger(OffsetX);
    WriteInteger(OffsetY);
    WriteInteger(Width);
    WriteInteger(Height);
  end;
end;

procedure TgxSpriteAnimFrame.ReadFromFiler(reader: TReader);
var
  archiveVersion: Integer;
begin
  inherited;
  archiveVersion := reader.ReadInteger;
  Assert(archiveVersion = 0);
  with reader do
  begin
    OffsetX := ReadInteger;
    OffsetY := ReadInteger;
    Width := ReadInteger;
    Height := ReadInteger;
  end;
end;

procedure TgxSpriteAnimFrame.SetOffsetX(const Value: Integer);
begin
  if Value <> FOffsetX then
  begin
    FOffsetX := Value;
    DoChanged;
  end;
end;

procedure TgxSpriteAnimFrame.SetOffsetY(const Value: Integer);
begin
  if Value <> FOffsetY then
  begin
    FOffsetY := Value;
    DoChanged;
  end;
end;

procedure TgxSpriteAnimFrame.SetWidth(const Value: Integer);
begin
  if Value <> FWidth then
  begin
    FWidth := Value;
    DoChanged;
  end;
end;

procedure TgxSpriteAnimFrame.SetHeight(const Value: Integer);
begin
  if Value <> FHeight then
  begin
    FHeight := Value;
    DoChanged;
  end;
end;

// ----------
// ---------- TgxSpriteAnimFrameList ----------
// ----------

constructor TgxSpriteAnimFrameList.Create(aOwner: TPersistent);
begin
  inherited;
end;

class function TgxSpriteAnimFrameList.ItemsClass: TXCollectionItemClass;
begin
  Result := TgxSpriteAnimFrame;
end;

// ----------
// ---------- TgxSpriteAnimaMargins ----------
// ----------

constructor TgxSpriteAnimMargins.Create(Animation: TgxSpriteAnimation);
begin
  inherited Create;
  FOwner := Animation;
end;

procedure TgxSpriteAnimMargins.SetLeft(const Value: Integer);
begin
  if Value <> FLeft then
  begin
    FLeft := Value;
    DoChanged;
  end;
end;

procedure TgxSpriteAnimMargins.SetTop(const Value: Integer);
begin
  if Value <> FTop then
  begin
    FTop := Value;
    DoChanged;
  end;
end;

procedure TgxSpriteAnimMargins.SetRight(const Value: Integer);
begin
  if Value <> FRight then
  begin
    FRight := Value;
    DoChanged;
  end;
end;

procedure TgxSpriteAnimMargins.SetBottom(const Value: Integer);
begin
  if Value <> FBottom then
  begin
    FBottom := Value;
    DoChanged;
  end;
end;


procedure TgxSpriteAnimMargins.DoChanged;
begin
  if Assigned(Owner) then
    Owner.DoChanged;
end;

// ----------
// ---------- TgxSpriteAnimation ----------
// ----------

constructor TgxSpriteAnimation.Create(aOwner: TXCollection);
begin
  inherited;
  FFrames := TgxSpriteAnimFrameList.Create(Self);
  FMargins := TgxSpriteAnimMargins.Create(Self);
end;

destructor TgxSpriteAnimation.Destroy;
begin
  FFrames.Free;
  FMargins.Free;
  inherited;
end;

function TgxSpriteAnimation.GetMaterialLibrary: TgxAbstractMaterialLibrary;
begin
  if not (Owner is TgxSpriteAnimationList) then
    Result := nil
  else
  begin
    if not (TgxSpriteAnimationList(Owner).Owner is TgxAnimatedSprite) then
      Result := nil
    else
      Result :=
        TgxAnimatedSprite(TgxSpriteAnimationList(Owner).Owner).FMaterialLibrary;
  end;
end;

class function TgxSpriteAnimation.FriendlyName: string;
begin
  Result := 'Animation';
end;

class function TgxSpriteAnimation.FriendlyDescription: string;
begin
  Result := 'Sprite Animation';
end;

procedure TgxSpriteAnimation.WriteToFiler(writer: TWriter);
begin
  inherited;
  writer.WriteInteger(2); // Archive version number
  Frames.WriteToFiler(writer);
  with writer do
  begin
    // Version 0
    WriteString(LibMaterialName);
    WriteInteger(CurrentFrame);
    WriteInteger(StartFrame);
    WriteInteger(EndFrame);
    WriteInteger(FrameWidth);
    WriteInteger(FrameHeight);
    WriteInteger(Integer(Dimensions));
    // Version 1
    WriteInteger(Interval);
    // Version 2
    WriteInteger(Margins.Left);
    WriteInteger(Margins.Top);
    WriteInteger(Margins.Right);
    WriteInteger(Margins.Bottom);
  end;
end;

procedure TgxSpriteAnimation.ReadFromFiler(reader: TReader);
var
  archiveVersion: Integer;
begin
  inherited;
  archiveVersion := reader.ReadInteger;
  Assert((archiveVersion >= 0) and (archiveVersion <= 2));
  Frames.ReadFromFiler(reader);
  with reader do
  begin
    FLibMaterialName := ReadString;
    CurrentFrame := ReadInteger;
    StartFrame := ReadInteger;
    EndFrame := ReadInteger;
    FrameWidth := ReadInteger;
    FrameHeight := ReadInteger;
    Dimensions := TgxSpriteFrameDimensions(ReadInteger);

    if archiveVersion >= 1 then
    begin
      Interval := ReadInteger;
    end;

    if archiveVersion >= 2 then
    begin
      Margins.Left := ReadInteger;
      Margins.Top := ReadInteger;
      Margins.Right := ReadInteger;
      Margins.Bottom := ReadInteger;
    end;
  end;
end;

procedure TgxSpriteAnimation.DoChanged;
begin
  if Assigned(Owner) then
  begin
    if Assigned(Owner.Owner) then
      if Owner.Owner is TgxBaseSceneObject then
        TgxBaseSceneObject(Owner.Owner).NotifyChange(Self);
  end;
end;

procedure TgxSpriteAnimation.SetCurrentFrame(const Value: NativeInt);
begin
  if Value <> FCurrentFrame then
  begin
    FCurrentFrame := Value;
    if FCurrentFrame < 0 then
      FCurrentFrame := -1;
    DoChanged;
  end;
end;

procedure TgxSpriteAnimation.SetFrameWidth(const Value: NativeInt);
begin
  if Value <> FFrameWidth then
  begin
    FFrameWidth := Value;
    DoChanged;
  end;
end;

procedure TgxSpriteAnimation.SetFrameHeight(const Value: NativeInt);
begin
  if Value <> FFrameHeight then
  begin
    FFrameHeight := Value;
    DoChanged;
  end;
end;

procedure TgxSpriteAnimation.SetDimensions(
  const Value: TgxSpriteFrameDimensions);
begin
  if Value <> FDimensions then
  begin
    FDimensions := Value;
    DoChanged;
  end;
end;

procedure TgxSpriteAnimation.SetLibMaterialName(const val: TgxLibMaterialName);
begin
  if val <> FLibMaterialName then
  begin
    FLibMaterialName := val;
    FLibMaterialCached := nil;
  end;
end;

function TgxSpriteAnimation.GetLibMaterialCached: TgxLibMaterial;
begin
  Result := nil;
  if FLibMaterialName = '' then
    exit;

  if not Assigned(FLibMaterialCached) then
    if Assigned(Owner) then
      if Assigned(Owner.Owner) then
        if Owner.Owner is TgxAnimatedSprite then
          if Assigned(TgxAnimatedSprite(Owner.Owner).MaterialLibrary) then
            FLibMaterialCached :=
              TgxAnimatedSprite(Owner.Owner).MaterialLibrary.Materials.GetLibMaterialByName(FLibMaterialName);

  Result := FLibMaterialCached;
end;

procedure TgxSpriteAnimation.SetInterval(const Value: NativeInt);
begin
  if Value <> FInterval then
  begin
    FInterval := Value;
    DoChanged;
  end;
end;

procedure TgxSpriteAnimation.SetFrameRate(const Value: Single);
begin
  if Value > 0 then
    Interval := Round(1000 / Value)
  else
    Interval := 0;
end;

function TgxSpriteAnimation.GetFrameRate: Single;
begin
  if Interval > 0 then
    Result := 1000 / Interval
  else
    Result := 0;
end;

// ----------
// ---------- TgxSpriteAnimationList ----------
// ----------

constructor TgxSpriteAnimationList.Create(aOwner: TPersistent);
begin
  inherited;
end;

class function TgxSpriteAnimationList.ItemsClass: TXCollectionItemClass;
begin
  Result := TgxSpriteAnimation;
end;

// ----------
// ---------- TgxAnimatedSprite ----------
// ----------

constructor TgxAnimatedSprite.Create(AOwner: TComponent);
begin
  inherited;

  FAnimations := TgxSpriteAnimationList.Create(Self);
  FAnimationIndex := -1;
  FInterval := 100;
  FPixelRatio := 100;
  FRotation := 0;
  FMirrorU := False;
  FMirrorV := False;

  ObjectStyle := [osDirectDraw];
end;

destructor TgxAnimatedSprite.Destroy;
begin
  FAnimations.Free;
  inherited;
end;

{$WARNINGS Off}

procedure TgxAnimatedSprite.BuildList(var rci: TgxRenderContextInfo);
var
  vx, vy: TAffineVector;
  w, h, temp: Single;
  mat: TMatrix4f;
  u0, v0, u1, v1: Single;
  x0, y0, x1, y1, TexWidth, TexHeight: Integer;
  Anim: TgxSpriteAnimation;
  Frame: TgxSpriteAnimFrame;
  libMat: TgxLibMaterial;
  IsAuto: Boolean;
begin
  if (FAnimationIndex <> -1) and (FAnimationIndex < Animations.Count) then
  begin
    Anim := TgxSpriteAnimation(Animations[FAnimationIndex]);

    if (Anim.CurrentFrame >= 0) then
    begin
      if (Anim.Dimensions = sfdManual) and (Anim.CurrentFrame <
        Anim.Frames.Count) then
        Frame := TgxSpriteAnimFrame(Anim.Frames[Anim.CurrentFrame])
      else
        Frame := nil;
      IsAuto := (Anim.CurrentFrame <= Anim.EndFrame) and
        (Anim.CurrentFrame >= Anim.StartFrame) and
        (Anim.Dimensions = sfdAuto);
      if Assigned(Frame) or IsAuto then
      begin
        libMat := Anim.LibMaterialCached;

        h := 0.5;
        w := 0.5;
        u0 := 0;
        v0 := 0;
        u1 := 0;
        v1 := 0;

        if Assigned(libMat) then
        begin
          TexWidth := libMat.Material.Texture.Image.Width;
          TexHeight := libMat.Material.Texture.Image.Height;
          if Anim.Dimensions = sfdManual then
          begin
            x0 := Frame.OffsetX;
            y0 := Frame.OffsetY;
            x1 := x0 + Frame.Width - 1;
            y1 := y0 + Frame.Height - 1;
          end
          else
          begin
            if (TexWidth > 0) and (Anim.FrameWidth > 0)
              and (TexHeight > 0) and (Anim.FrameHeight > 0) then
            begin
              x0 := Anim.FrameWidth * (Anim.CurrentFrame mod (TexWidth div
                Anim.FrameWidth));
              y0 := Anim.FrameHeight * (Anim.CurrentFrame div (TexWidth div
                Anim.FrameWidth));
            end
            else
            begin
              x0 := 0;
              y0 := 0;
            end;
            x1 := x0 + Anim.FrameWidth - 1;
            y1 := y0 + Anim.FrameHeight - 1;

            x0 := x0 + Anim.Margins.Left;
            y0 := y0 + Anim.Margins.Top;
            x1 := x1 - Anim.Margins.Right;
            y1 := y1 - Anim.Margins.Bottom;
          end;
          if (TexWidth > 0) and (TexHeight > 0) and (x0 <> x1) and (y0 <> y1)
            then
          begin
            u0 := x0 / TexWidth;
            v0 := 1 - y1 / TexHeight;
            u1 := x1 / TexWidth;
            v1 := 1 - y0 / TexHeight;
            w := 0.5 * (x1 - x0) / FPixelRatio;
            h := 0.5 * (y1 - y0) / FPixelRatio;
          end;
        end;

        glGetFloatv(GL_MODELVIEW_MATRIX, @mat);
        vx.X := mat.V[0].X;
        vy.X := mat.V[0].Y;
        vx.Y := mat.V[1].X;
        vy.Y := mat.V[1].Y;
        vx.Z := mat.V[2].X;
        vy.Z := mat.V[2].Y;
        ScaleVector(vx, w * VectorLength(vx));
        ScaleVector(vy, h * VectorLength(vy));

        if FMirrorU then
        begin
          temp := u0;
          u0 := u1;
          u1 := temp;
        end;
        if FMirrorV then
        begin
          temp := v0;
          v0 := v1;
          v1 := temp;
        end;

        if Assigned(libMat) then
          libMat.Apply(rci);
        rci.gxStates.Disable(stLighting);
        if FRotation <> 0 then
        begin
          glMatrixMode(GL_MODELVIEW);
          glPushMatrix;
          glRotatef(FRotation, mat.V[0].Z, mat.V[1].Z, mat.V[2].Z);
        end;
        glBegin(GL_QUADS);
        glTexCoord2f(u1, v1);
        glVertex3f(vx.X + vy.X, vx.Y + vy.Y,
                    vx.Z + vy.Z);
        glTexCoord2f(u0, v1);
        glVertex3f(-vx.X + vy.X,
                    -vx.Y + vy.Y,
                    -vx.Z + vy.Z);
        glTexCoord2f(u0, v0);
        glVertex3f(-vx.X - vy.X, -vx.Y - vy.Y, -vx.Z - vy.Z);
        glTexCoord2f(u1, v0);
        glVertex3f(vx.X - vy.X, vx.Y - vy.Y, vx.Z - vy.Z);
        glEnd;
        if FRotation <> 0 then
        begin
          glPopMatrix;
        end;
        if Assigned(libMat) then
          libMat.UnApply(rci);
      end;
    end;
  end;
end;
{$WARNINGS On}

procedure TgxAnimatedSprite.DoProgress(const progressTime: TgxProgressTimes);
var
  i, intr: Integer;
begin
  inherited;
  if (AnimationIndex = -1) then
    exit;
  intr := TgxSpriteAnimation(Animations[AnimationIndex]).Interval;
  if intr = 0 then
    intr := Interval;
  if (FAnimationMode <> samNone) and (intr > 0) then
  begin
    FCurrentFrameDelta := FCurrentFrameDelta + (progressTime.deltaTime * 1000) /
      intr;
    if FCurrentFrameDelta >= 1 then
    begin
      for i := 0 to Floor(FCurrentFrameDelta) - 1 do
      begin
        NextFrame;
        FCurrentFrameDelta := FCurrentFrameDelta - 1;
      end;
    end;
  end;
end;


procedure TgxAnimatedSprite.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  if (Operation = opRemove) and (AComponent = FMaterialLibrary) then
    FMaterialLibrary := nil;
  inherited;
end;

procedure TgxAnimatedSprite.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('SpriteAnimations',
    ReadAnimations, WriteAnimations,
    FAnimations.Count > 0);
end;

procedure TgxAnimatedSprite.WriteAnimations(Stream: TStream);
var
  writer: TWriter;
begin
  writer := TWriter.Create(stream, 16384);
  try
    Animations.WriteToFiler(writer);
  finally
    writer.Free;
  end;
end;

procedure TgxAnimatedSprite.ReadAnimations(Stream: TStream);
var
  reader: TReader;
begin
  reader := TReader.Create(stream, 16384);
  try
    Animations.ReadFromFiler(reader);
  finally
    reader.Free;
  end;
end;

procedure TgxAnimatedSprite.NextFrame;
var
  currentFrame,
    startFrame,
    endFrame: Integer;
  Anim: TgxSpriteAnimation;
begin
  if (FAnimationIndex = -1) or (FAnimationIndex >= Animations.Count) then
    exit;

  Anim := TgxSpriteAnimation(Animations[FAnimationIndex]);

  currentFrame := Anim.CurrentFrame;
  if Anim.Dimensions = sfdManual then
  begin
    startFrame := 0;
    endFrame := Anim.Frames.Count - 1
  end
  else
  begin
    startFrame := Anim.StartFrame;
    endFrame := Anim.EndFrame;
  end;

  case AnimationMode of
    samLoop, samBounceForward, samPlayOnce:
      begin
        if (currentFrame = endFrame) and Assigned(FOnEndFrameReached) then
          FOnEndFrameReached(Self);
        Inc(currentFrame);
      end;
    samBounceBackward, samLoopBackward:
      begin
        if (currentFrame = startFrame) and Assigned(FOnStartFrameReached) then
          FOnStartFrameReached(Self);
        Dec(CurrentFrame);
      end;
  end;

  if (AnimationMode <> samNone) and Assigned(FOnFrameChanged) then
    FOnFrameChanged(Self);

  case AnimationMode of

    samPlayOnce:
      begin
        if currentFrame > endFrame then
          AnimationMode := samNone;
      end;

    samLoop:
      begin
        if currentFrame > endFrame then
          currentFrame := startFrame;
      end;

    samBounceForward:
      begin
        if currentFrame = endFrame then
          AnimationMode := samBounceBackward;
      end;

    samLoopBackward:
      begin
        if currentFrame < startFrame then
          CurrentFrame := endFrame;
      end;

    samBounceBackward:
      begin
        if currentFrame = startFrame then
          AnimationMode := samBounceForward;
      end;

  end;

  Anim.CurrentFrame := currentFrame;
end;

procedure TgxAnimatedSprite.SetInterval(const val: Integer);
begin
  if val <> FInterval then
  begin
    FInterval := val;
    NotifyChange(Self);
  end;
end;


procedure TgxAnimatedSprite.SetFrameRate(const Value: Single);
begin
  if Value > 0 then
    Interval := Round(1000 / Value)
  else
    Interval := 0;
end;

function TgxAnimatedSprite.GetFrameRate: Single;
begin
  if Interval > 0 then
    Result := 1000 / Interval
  else
    Result := 0;
end;

procedure TgxAnimatedSprite.SetAnimationIndex(const val: Integer);
begin
  if val <> FAnimationIndex then
  begin
    FAnimationIndex := val;
    if FAnimationIndex < 0 then
      FAnimationIndex := -1;
    if (FAnimationIndex <> -1) and (FAnimationIndex < Animations.Count) then
      with TgxSpriteAnimation(Animations[FAnimationIndex]) do
        case AnimationMode of
          samNone, samPlayOnce, samLoop, samBounceForward:
            CurrentFrame := StartFrame;
          samLoopBackward, samBounceBackward:
            CurrentFrame := EndFrame;
        end;
    NotifyChange(Self);
  end;
end;

procedure TgxAnimatedSprite.SetAnimationMode(const val: TgxSpriteAnimationMode);
begin
  if val <> FAnimationMode then
  begin
    FAnimationMode := val;
    NotifyChange(Self);
  end;
end;

procedure TgxAnimatedSprite.SetMaterialLibrary(const val: TgxMaterialLibrary);
var
  i: Integer;
begin
  if val <> FMaterialLibrary then
  begin
    if FMaterialLibrary <> nil then
      FMaterialLibrary.RemoveFreeNotification(Self);
    FMaterialLibrary := val;
    if FMaterialLibrary <> nil then
      FMaterialLibrary.FreeNotification(Self);
    for i := 0 to Animations.Count - 1 do
      TgxSpriteAnimation(Animations[i]).FLibMaterialCached := nil;
    NotifyChange(Self);
  end;
end;

procedure TgxAnimatedSprite.SetPixelRatio(const val: Integer);
begin
  if (FPixelRatio <> val) and (val > 0) then
  begin
    FPixelRatio := val;
    NotifyChange(Self);
  end;
end;

procedure TgxAnimatedSprite.SetRotation(const val: Integer);
begin
  if val <> FRotation then
  begin
    FRotation := val;
    NotifyChange(Self);
  end;
end;

procedure TgxAnimatedSprite.SetMirrorU(const val: Boolean);
begin
  if val <> FMirrorU then
  begin
    FMirrorU := val;
    NotifyChange(Self);
  end;
end;

procedure TgxAnimatedSprite.SetMirrorV(const val: Boolean);
begin
  if val <> FMirrorV then
  begin
    FMirrorV := val;
    NotifyChange(Self);
  end;
end;

// -----------------------------------------------------------------------------
initialization
// -----------------------------------------------------------------------------

  RegisterClasses([TgxAnimatedSprite,
    TgxSpriteAnimFrame, TgxSpriteAnimFrameList,
      TgxSpriteAnimation, TgxSpriteAnimationList]);

  RegisterXCollectionItemClass(TgxSpriteAnimFrame);
  RegisterXCollectionItemClass(TgxSpriteAnimation);

finalization

  UnregisterXCollectionItemClass(TgxSpriteAnimFrame);
  UnregisterXCollectionItemClass(TgxSpriteAnimation);

end.

