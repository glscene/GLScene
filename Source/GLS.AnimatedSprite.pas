//
// The graphics rendering engine GLScene http://glscene.org
//

unit GLS.AnimatedSprite;

(* A sprite that uses a scrolling texture for animation. *)

interface

{$I GLScene.inc}

uses
  Winapi.OpenGL,
  System.Classes,
  System.SysUtils,
  System.Math,
   
  GLS.OpenGLTokens,
  GLS.Scene,
  GLS.VectorTypes,
  GLS.VectorGeometry,
  GLS.Material,
  GLS.PersistentClasses,
  GLS.XCollection,
  GLS.RenderContextInfo,
  GLS.BaseClasses, 
  GLS.Context, 
  GLS.State;

type
  TGLSpriteAnimFrame = class;
  TGLSpriteAnimFrameList = class;
  TGLSpriteAnimation = class;
  TGLSpriteAnimationList = class;
  TGLAnimatedSprite = class;

  (* Used by the SpriteAnimation when Dimensions are set manual. The animation
    will use the offsets, width and height to determine the texture coodinates for this frame. *)
  TGLSpriteAnimFrame = class(TXCollectionItem)
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

  TGLSpriteAnimFrameList = class(TXCollection)
  public
    constructor Create(aOwner: TPersistent); override;
    class function ItemsClass: TXCollectionItemClass; override;
  end;

  (* Determines if the texture coordinates are Automatically generated
     from the Animations properties or if they are Manually set through
     the Frames collection. *)
  TGLSpriteFrameDimensions = (sfdAuto, sfdManual);

  (* Used to mask the auto generated frames. The Left, Top, Right and
    Bottom properties determines the number of pixels to be cropped
    from each corresponding side of the frame. Only applicable to auto dimensions. *)
  TGLSpriteAnimMargins = class(TPersistent)
  private
    FOwner: TGLSpriteAnimation;
    FLeft, FTop, FRight, FBottom: Integer;
  protected
    procedure SetLeft(const Value: Integer);
    procedure SetTop(const Value: Integer);
    procedure SetRight(const Value: Integer);
    procedure SetBottom(const Value: Integer);
    procedure DoChanged;
  public
    constructor Create(Animation: TGLSpriteAnimation);
    property Owner: TGLSpriteAnimation read FOwner;
  published
    property Left: Integer read FLeft write SetLeft;
    property Top: Integer read FTop write SetTop;
    property Right: Integer read FRight write SetRight;
    property Bottom: Integer read FBottom write SetBottom;
  end;

  // Animations define how the texture coordinates for each offset are to be determined. 
  TGLSpriteAnimation = class(TXCollectionItem, IGLMaterialLibrarySupported)
  private
    FCurrentFrame,
      FStartFrame,
      FEndFrame,
      FFrameWidth,
      FFrameHeight,
      FInterval: Integer;
    FFrames: TGLSpriteAnimFrameList;
    FLibMaterialName: TGLLibMaterialName;
    FLibMaterialCached: TGLLibMaterial;
    FDimensions: TGLSpriteFrameDimensions;
    FMargins: TGLSpriteAnimMargins;
    procedure DoChanged;
  protected
    procedure SetCurrentFrame(const Value: Integer);
    procedure SetFrameWidth(const Value: Integer);
    procedure SetFrameHeight(const Value: Integer);
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure SetDimensions(const Value: TGLSpriteFrameDimensions);
    procedure SetLibMaterialName(const val: TGLLibMaterialName);
    function GetLibMaterialCached: TGLLibMaterial;
    procedure SetInterval(const Value: Integer);
    procedure SetFrameRate(const Value: Single);
    function GetFrameRate: Single;
    // Implementing IGLMaterialLibrarySupported.
    function GetMaterialLibrary: TGLAbstractMaterialLibrary;
  public
    constructor Create(aOwner: TXCollection); override;
    destructor Destroy; override;
    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;
    property LibMaterialCached: TGLLibMaterial read GetLibMaterialCached;
  published
    // The current showing frame for this animation.
    property CurrentFrame: Integer read FCurrentFrame write SetCurrentFrame;
    // Defines the starting frame for auto dimension animations.
    property StartFrame: Integer read FStartFrame write FStartFrame;
    // Defines the ending frame for auto dimension animations.
    property EndFrame: Integer read FEndFrame write FEndFrame;
    // Width of each frame in an auto dimension animation.
    property FrameWidth: Integer read FFrameWidth write SetFrameWidth;
    // Height of each frame in an auto dimension animation.
    property FrameHeight: Integer read FFrameHeight write SetFrameHeight;
    (* The name of the lib material the sprites associated material library
       for this animation. *)
    property LibMaterialName: TGLLibMaterialName read FLibMaterialName write
      SetLibMaterialName;
    (* Manual dimension animation frames. Stores the offsets and dimensions
       for each frame in the animation. *)
    property Frames: TGLSpriteAnimFrameList read FFrames;
    // Automatic or manual texture coordinate generation.
    property Dimensions: TGLSpriteFrameDimensions read FDimensions write
      SetDimensions;
    (* The number of milliseconds between each frame in the animation.
       Will automatically calculate the FrameRate value when set.
       Will override the TGLAnimatedSprite Interval is greater than zero. *)
    property Interval: Integer read FInterval write SetInterval;
    (* The number of frames per second for the animation.
       Will automatically calculate the Interval value when set.
       Precision will depend on Interval since Interval has priority. *)
    property FrameRate: Single read GetFrameRate write SetFrameRate;
    // Sets cropping margins for auto dimension animations.
    property Margins: TGLSpriteAnimMargins read FMargins;
  end;

  // A collection for storing SpriteAnimation objects. 
  TGLSpriteAnimationList = class(TXCollection)
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
       when StartFrame is reached. *)
  TGLSpriteAnimationMode = (samNone, samPlayOnce, samLoop, samBounceForward,
    samBounceBackward, samLoopBackward);

  // An animated version for using offset texture coordinate animation. 
  TGLAnimatedSprite = class(TGLBaseSceneObject)
  private
    FAnimations: TGLSpriteAnimationList;
    FMaterialLibrary: TGLMaterialLibrary;
    FAnimationIndex,
      FInterval,
      FRotation,
      FPixelRatio: Integer;
    FMirrorU,
      FMirrorV: Boolean;
    FAnimationMode: TGLSpriteAnimationMode;
    FCurrentFrameDelta: Double;
    FOnFrameChanged: TNotifyEvent;
    FOnEndFrameReached: TNotifyEvent;
    FOnStartFrameReached: TNotifyEvent;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure WriteAnimations(Stream: TStream);
    procedure ReadAnimations(Stream: TStream);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetInterval(const val: Integer);
    procedure SetAnimationIndex(const val: Integer);
    procedure SetAnimationMode(const val: TGLSpriteAnimationMode);
    procedure SetMaterialLibrary(const val: TGLMaterialLibrary);
    procedure SetPixelRatio(const val: Integer);
    procedure SetRotation(const val: Integer);
    procedure SetMirrorU(const val: Boolean);
    procedure SetMirrorV(const val: Boolean);
    procedure SetFrameRate(const Value: Single);
    function GetFrameRate: Single;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BuildList(var rci: TGLRenderContextInfo); override;
    procedure DoProgress(const progressTime: TGLProgressTimes); override;
    // Steps the current animation to the next frame
    procedure NextFrame;
  published
    // A collection of animations. Stores the settings for animating then sprite. 
    property Animations: TGLSpriteAnimationList read FAnimations;
    // The material library that stores the lib materials for the animations.
    property MaterialLibrary: TGLMaterialLibrary read FMaterialLibrary write
      SetMaterialLibrary;
    (* Sets the number of milliseconds between each frame. Will recalculate
       the Framerate when set. Will be overridden by the TGLSpriteAnimation
       Interval if it is greater than zero. *)
    property Interval: Integer read FInterval write SetInterval;
    // Index of the sprite animation to be used.
    property AnimationIndex: Integer read FAnimationIndex write
      SetAnimationIndex;
    // Playback mode for the current animation.
    property AnimationMode: TGLSpriteAnimationMode read FAnimationMode write
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
// ---------- TGLSpriteAnimFrame ----------
// ----------

procedure TGLSpriteAnimFrame.DoChanged;
begin
  if Assigned(Owner) then
  begin
    if Assigned(Owner.Owner) then
      if Owner.Owner is TGLSpriteAnimation then
        TGLSpriteAnimation(Owner.Owner).DoChanged;
  end;
end;

class function TGLSpriteAnimFrame.FriendlyName: string;
begin
  Result := 'Frame';
end;

class function TGLSpriteAnimFrame.FriendlyDescription: string;
begin
  Result := 'Sprite Animation Frame';
end;

procedure TGLSpriteAnimFrame.WriteToFiler(writer: TWriter);
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

procedure TGLSpriteAnimFrame.ReadFromFiler(reader: TReader);
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

procedure TGLSpriteAnimFrame.SetOffsetX(const Value: Integer);
begin
  if Value <> FOffsetX then
  begin
    FOffsetX := Value;
    DoChanged;
  end;
end;

procedure TGLSpriteAnimFrame.SetOffsetY(const Value: Integer);
begin
  if Value <> FOffsetY then
  begin
    FOffsetY := Value;
    DoChanged;
  end;
end;

procedure TGLSpriteAnimFrame.SetWidth(const Value: Integer);
begin
  if Value <> FWidth then
  begin
    FWidth := Value;
    DoChanged;
  end;
end;

procedure TGLSpriteAnimFrame.SetHeight(const Value: Integer);
begin
  if Value <> FHeight then
  begin
    FHeight := Value;
    DoChanged;
  end;
end;

// ----------
// ---------- TGLSpriteAnimFrameList ----------
// ----------

constructor TGLSpriteAnimFrameList.Create(aOwner: TPersistent);
begin
  inherited;
end;

class function TGLSpriteAnimFrameList.ItemsClass: TXCollectionItemClass;
begin
  Result := TGLSpriteAnimFrame;
end;

// ----------
// ---------- TGLSpriteAnimMargins ----------
// ----------

constructor TGLSpriteAnimMargins.Create(Animation: TGLSpriteAnimation);
begin
  inherited Create;
  FOwner := Animation;
end;

procedure TGLSpriteAnimMargins.SetLeft(const Value: Integer);
begin
  if Value <> FLeft then
  begin
    FLeft := Value;
    DoChanged;
  end;
end;

procedure TGLSpriteAnimMargins.SetTop(const Value: Integer);
begin
  if Value <> FTop then
  begin
    FTop := Value;
    DoChanged;
  end;
end;


procedure TGLSpriteAnimMargins.SetRight(const Value: Integer);
begin
  if Value <> FRight then
  begin
    FRight := Value;
    DoChanged;
  end;
end;


procedure TGLSpriteAnimMargins.SetBottom(const Value: Integer);
begin
  if Value <> FBottom then
  begin
    FBottom := Value;
    DoChanged;
  end;
end;


procedure TGLSpriteAnimMargins.DoChanged;
begin
  if Assigned(Owner) then
    Owner.DoChanged;
end;

// ----------
// ---------- TGLSpriteAnimation ----------
// ----------

constructor TGLSpriteAnimation.Create(aOwner: TXCollection);
begin
  inherited;
  FFrames := TGLSpriteAnimFrameList.Create(Self);
  FMargins := TGLSpriteAnimMargins.Create(Self);
end;

destructor TGLSpriteAnimation.Destroy;
begin
  FFrames.Free;
  FMargins.Free;
  inherited;
end;

function TGLSpriteAnimation.GetMaterialLibrary: TGLAbstractMaterialLibrary;
begin
  if not (Owner is TGLSpriteAnimationList) then
    Result := nil
  else
  begin
    if not (TGLSpriteAnimationList(Owner).Owner is TGLAnimatedSprite) then
      Result := nil
    else
      Result :=
        TGLAnimatedSprite(TGLSpriteAnimationList(Owner).Owner).FMaterialLibrary;
  end;
end;

class function TGLSpriteAnimation.FriendlyName: string;
begin
  Result := 'Animation';
end;

class function TGLSpriteAnimation.FriendlyDescription: string;
begin
  Result := 'Sprite Animation';
end;

procedure TGLSpriteAnimation.WriteToFiler(writer: TWriter);
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

procedure TGLSpriteAnimation.ReadFromFiler(reader: TReader);
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
    Dimensions := TGLSpriteFrameDimensions(ReadInteger);

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

procedure TGLSpriteAnimation.DoChanged;
begin
  if Assigned(Owner) then
  begin
    if Assigned(Owner.Owner) then
      if Owner.Owner is TGLBaseSceneObject then
        TGLBaseSceneObject(Owner.Owner).NotifyChange(Self);
  end;
end;

procedure TGLSpriteAnimation.SetCurrentFrame(const Value: Integer);
begin
  if Value <> FCurrentFrame then
  begin
    FCurrentFrame := Value;
    if FCurrentFrame < 0 then
      FCurrentFrame := -1;
    DoChanged;
  end;
end;

procedure TGLSpriteAnimation.SetFrameWidth(const Value: Integer);
begin
  if Value <> FFrameWidth then
  begin
    FFrameWidth := Value;
    DoChanged;
  end;
end;

procedure TGLSpriteAnimation.SetFrameHeight(const Value: Integer);
begin
  if Value <> FFrameHeight then
  begin
    FFrameHeight := Value;
    DoChanged;
  end;
end;

procedure TGLSpriteAnimation.SetDimensions(
  const Value: TGLSpriteFrameDimensions);
begin
  if Value <> FDimensions then
  begin
    FDimensions := Value;
    DoChanged;
  end;
end;

procedure TGLSpriteAnimation.SetLibMaterialName(const val: TGLLibMaterialName);
begin
  if val <> FLibMaterialName then
  begin
    FLibMaterialName := val;
    FLibMaterialCached := nil;
  end;
end;

function TGLSpriteAnimation.GetLibMaterialCached: TGLLibMaterial;
begin
  Result := nil;
  if FLibMaterialName = '' then
    exit;

  if not Assigned(FLibMaterialCached) then
    if Assigned(Owner) then
      if Assigned(Owner.Owner) then
        if Owner.Owner is TGLAnimatedSprite then
          if Assigned(TGLAnimatedSprite(Owner.Owner).MaterialLibrary) then
            FLibMaterialCached :=
              TGLAnimatedSprite(Owner.Owner).MaterialLibrary.Materials.GetLibMaterialByName(FLibMaterialName);

  Result := FLibMaterialCached;
end;

procedure TGLSpriteAnimation.SetInterval(const Value: Integer);
begin
  if Value <> FInterval then
  begin
    FInterval := Value;
    DoChanged;
  end;
end;

procedure TGLSpriteAnimation.SetFrameRate(const Value: Single);
begin
  if Value > 0 then
    Interval := Round(1000 / Value)
  else
    Interval := 0;
end;

function TGLSpriteAnimation.GetFrameRate: Single;
begin
  if Interval > 0 then
    Result := 1000 / Interval
  else
    Result := 0;
end;

// ----------
// ---------- TGLSpriteAnimationList ----------
// ----------

constructor TGLSpriteAnimationList.Create(aOwner: TPersistent);
begin
  inherited;
end;

class function TGLSpriteAnimationList.ItemsClass: TXCollectionItemClass;
begin
  Result := TGLSpriteAnimation;
end;

// ----------
// ---------- TGLAnimatedSprite ----------
// ----------

constructor TGLAnimatedSprite.Create(AOwner: TComponent);
begin
  inherited;

  FAnimations := TGLSpriteAnimationList.Create(Self);
  FAnimationIndex := -1;
  FInterval := 100;
  FPixelRatio := 100;
  FRotation := 0;
  FMirrorU := False;
  FMirrorV := False;

  ObjectStyle := [osDirectDraw];
end;

destructor TGLAnimatedSprite.Destroy;
begin
  FAnimations.Free;
  inherited;
end;

{$WARNINGS Off}

procedure TGLAnimatedSprite.BuildList(var rci: TGLRenderContextInfo);
var
  vx, vy: TAffineVector;
  w, h, temp: Single;
  mat: TGLMatrix;
  u0, v0, u1, v1: Single;
  x0, y0, x1, y1, TexWidth, TexHeight: Integer;
  Anim: TGLSpriteAnimation;
  Frame: TGLSpriteAnimFrame;
  libMat: TGLLibMaterial;
  IsAuto: Boolean;
begin
  if (FAnimationIndex <> -1) and (FAnimationIndex < Animations.Count) then
  begin
    Anim := TGLSpriteAnimation(Animations[FAnimationIndex]);

    if (Anim.CurrentFrame >= 0) then
    begin
      if (Anim.Dimensions = sfdManual) and (Anim.CurrentFrame <
        Anim.Frames.Count) then
        Frame := TGLSpriteAnimFrame(Anim.Frames[Anim.CurrentFrame])
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

        gl.GetFloatv(GL_MODELVIEW_MATRIX, @mat);
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
        rci.GLStates.Disable(stLighting);
        if FRotation <> 0 then
        begin
          gl.MatrixMode(GL_MODELVIEW);
          gl.PushMatrix;
          gl.Rotatef(FRotation, mat.V[0].Z, mat.V[1].Z, mat.V[2].Z);
        end;
        gl.Begin_(GL_QUADS);
        gl.TexCoord2f(u1, v1);
        gl.Vertex3f(vx.X + vy.X, vx.Y + vy.Y,
                    vx.Z + vy.Z);
        gl.TexCoord2f(u0, v1);
        gl.Vertex3f(-vx.X + vy.X,
                    -vx.Y + vy.Y,
                    -vx.Z + vy.Z);
        gl.TexCoord2f(u0, v0);
        gl.Vertex3f(-vx.X - vy.X, -vx.Y - vy.Y, -vx.Z - vy.Z);
        gl.TexCoord2f(u1, v0);
        gl.Vertex3f(vx.X - vy.X, vx.Y - vy.Y, vx.Z - vy.Z);
        gl.End_;
        if FRotation <> 0 then
        begin
          gl.PopMatrix;
        end;
        if Assigned(libMat) then
          libMat.UnApply(rci);
      end;
    end;
  end;
end;
{$WARNINGS On}

procedure TGLAnimatedSprite.DoProgress(const progressTime: TGLProgressTimes);
var
  i, intr: Integer;
begin
  inherited;
  if (AnimationIndex = -1) then
    exit;
  intr := TGLSpriteAnimation(Animations[AnimationIndex]).Interval;
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


procedure TGLAnimatedSprite.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  if (Operation = opRemove) and (AComponent = FMaterialLibrary) then
    FMaterialLibrary := nil;
  inherited;
end;

procedure TGLAnimatedSprite.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('SpriteAnimations',
    ReadAnimations, WriteAnimations,
    FAnimations.Count > 0);
end;

procedure TGLAnimatedSprite.WriteAnimations(Stream: TStream);
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

procedure TGLAnimatedSprite.ReadAnimations(Stream: TStream);
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

procedure TGLAnimatedSprite.NextFrame;
var
  currentFrame,
    startFrame,
    endFrame: Integer;
  Anim: TGLSpriteAnimation;
begin
  if (FAnimationIndex = -1) or (FAnimationIndex >= Animations.Count) then
    exit;

  Anim := TGLSpriteAnimation(Animations[FAnimationIndex]);

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

procedure TGLAnimatedSprite.SetInterval(const val: Integer);
begin
  if val <> FInterval then
  begin
    FInterval := val;
    NotifyChange(Self);
  end;
end;


procedure TGLAnimatedSprite.SetFrameRate(const Value: Single);
begin
  if Value > 0 then
    Interval := Round(1000 / Value)
  else
    Interval := 0;
end;

function TGLAnimatedSprite.GetFrameRate: Single;
begin
  if Interval > 0 then
    Result := 1000 / Interval
  else
    Result := 0;
end;

procedure TGLAnimatedSprite.SetAnimationIndex(const val: Integer);
begin
  if val <> FAnimationIndex then
  begin
    FAnimationIndex := val;
    if FAnimationIndex < 0 then
      FAnimationIndex := -1;
    if (FAnimationIndex <> -1) and (FAnimationIndex < Animations.Count) then
      with TGLSpriteAnimation(Animations[FAnimationIndex]) do
        case AnimationMode of
          samNone, samPlayOnce, samLoop, samBounceForward:
            CurrentFrame := StartFrame;
          samLoopBackward, samBounceBackward:
            CurrentFrame := EndFrame;
        end;
    NotifyChange(Self);
  end;
end;

procedure TGLAnimatedSprite.SetAnimationMode(const val: TGLSpriteAnimationMode);
begin
  if val <> FAnimationMode then
  begin
    FAnimationMode := val;
    NotifyChange(Self);
  end;
end;

procedure TGLAnimatedSprite.SetMaterialLibrary(const val: TGLMaterialLibrary);
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
      TGLSpriteAnimation(Animations[i]).FLibMaterialCached := nil;
    NotifyChange(Self);
  end;
end;

procedure TGLAnimatedSprite.SetPixelRatio(const val: Integer);
begin
  if (FPixelRatio <> val) and (val > 0) then
  begin
    FPixelRatio := val;
    NotifyChange(Self);
  end;
end;

procedure TGLAnimatedSprite.SetRotation(const val: Integer);
begin
  if val <> FRotation then
  begin
    FRotation := val;
    NotifyChange(Self);
  end;
end;

procedure TGLAnimatedSprite.SetMirrorU(const val: Boolean);
begin
  if val <> FMirrorU then
  begin
    FMirrorU := val;
    NotifyChange(Self);
  end;
end;

procedure TGLAnimatedSprite.SetMirrorV(const val: Boolean);
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

  RegisterClasses([TGLAnimatedSprite,
    TGLSpriteAnimFrame, TGLSpriteAnimFrameList,
      TGLSpriteAnimation, TGLSpriteAnimationList]);

  RegisterXCollectionItemClass(TGLSpriteAnimFrame);
  RegisterXCollectionItemClass(TGLSpriteAnimation);

finalization

  UnregisterXCollectionItemClass(TGLSpriteAnimFrame);
  UnregisterXCollectionItemClass(TGLSpriteAnimation);

end.

