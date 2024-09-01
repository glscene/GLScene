//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.Blur;

(* Applies a blur effect over the viewport *)

interface

{$I GXS.Scene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,

  System.Classes,
  System.SysUtils,
  System.UITypes,
  FMX.Graphics,

  GXS.VectorTypes,
  GXS.PersistentClasses,
  GXS.Scene,
  GXS.Coordinates,
  GXS.VectorGeometry,
  GXS.Objects,
  GXS.BitmapFont,
  GXS.Texture,
  GXS.Material,
  GXS.HudObjects,
  GXS.Color,
  GXS.Graphics,
  GXS.Context,
  GXS.State,
  GXS.Strings,
  GXS.TextureFormat,
  GXS.BaseClasses,
  GXS.RenderContextInfo;

type

  TgxBlurPreset = (pNone, pGlossy, pBeastView, pOceanDepth, pDream, pOverBlur,
    pAdvancedBlur);
  TgxBlurkind = (bNone, bSimple, bAdvanced);

  TRGBPixel = record
    R, G, B: GLubyte;
  end;

  TRGBPixelBuffer = array of TRGBPixel;
  TgxAdvancedBlurImagePrepareEvent = procedure(Sender: TObject;
    BMP32: TgxBitmap32; var DoBlur: boolean) of object;

  EGLMotionBlurException = class(Exception);

  TgxBlur = class(TgxHUDSprite)
  private
    FViewer: TgxMemoryViewer;
    OldTime: Double;
    FDoingMemView: boolean;
    FBlurDeltaTime: Double;
    FBlurTop: Single;
    FBlurBottom: Single;
    FBlurLeft: Single;
    FBlurRight: Single;
    FRenderHeight: Integer;
    FRenderWidth: Integer;
    FPreset: TgxBlurPreset;
    FTargetObject: TgxbaseSceneObject;
    FOnAdvancedBlurImagePrepareEvent: TgxAdvancedBlurImagePrepareEvent;
    FBlur: TgxBlurkind;
    Pixelbuffer: TRGBPixelBuffer;
    FAdvancedBlurPasses: Integer;
    FOnAfterTargetRender: TNotifyEvent;
    FOnBeforeTargetRender: TNotifyEvent;
    FAdvancedBlurAmp: Single;
    FBlurSelf: boolean;
    FAdvancedBlurLowClamp: byte;
    FAdvancedBlurHiClamp: byte;
    FRenderBackgroundColor: TColor;
    procedure DoMemView(baseObject: TgxbaseSceneObject);
    procedure SetRenderHeight(const Value: Integer);
    procedure SetRenderWidth(const Value: Integer);
    procedure UpdateImageSettings;
    procedure SetPreset(const Value: TgxBlurPreset);
    function StoreBlurBottom: boolean;
    function StoreBlurDeltaTime: boolean;
    function StoreBlurRight: boolean;
    function StoreBlurTop: boolean;
    function StoreBlurLeft: boolean;
    procedure SetTargetObject(const Value: TgxbaseSceneObject);
    procedure SetOnAdvancedBlurImagePrepareEvent(const Value
      : TgxAdvancedBlurImagePrepareEvent);
    procedure SetBlur(const Value: TgxBlurkind);
    procedure SetAdvancedBlurPasses(const Value: Integer);
    procedure SetOnAfterTargetRender(const Value: TNotifyEvent);
    procedure SetOnBeforeTargetRender(const Value: TNotifyEvent);
    procedure SetAdvancedBlurAmp(const Value: Single);
    procedure SetBlurSelf(const Value: boolean);
    procedure SetAdvancedBlurLowClamp(const Value: byte);
    procedure SetAdvancedBlurHiClamp(const Value: byte);
    procedure SetRenderBackgroundColor(const Value: TColor);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoProgress(const progressTime: TgxProgressTimes); override;
    procedure DoRender(var ARci: TgxRenderContextInfo;
      ARenderSelf, ARenderChildren: boolean); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  published
    property Blur: TgxBlurkind read FBlur write SetBlur;
    property BlurDeltaTime: Double read FBlurDeltaTime write FBlurDeltaTime
      stored StoreBlurDeltaTime;
    property BlurLeft: Single read FBlurLeft write FBlurLeft
      stored StoreBlurLeft;
    property BlurTop: Single read FBlurTop write FBlurTop stored StoreBlurTop;
    property BlurRight: Single read FBlurRight write FBlurRight
      stored StoreBlurRight;
    property BlurBottom: Single read FBlurBottom write FBlurBottom
      stored StoreBlurBottom;
    property RenderWidth: Integer read FRenderWidth write SetRenderWidth
      default 256;
    property RenderHeight: Integer read FRenderHeight write SetRenderHeight
      default 256;
    property Preset: TgxBlurPreset read FPreset write SetPreset stored false;
    property TargetObject: TgxbaseSceneObject read FTargetObject
      write SetTargetObject;
    property AdvancedBlurPasses: Integer read FAdvancedBlurPasses
      write SetAdvancedBlurPasses;
    property AdvancedBlurAmp: Single read FAdvancedBlurAmp
      write SetAdvancedBlurAmp;
    property AdvancedBlurLowClamp: byte read FAdvancedBlurLowClamp
      write SetAdvancedBlurLowClamp;
    property AdvancedBlurHiClamp: byte read FAdvancedBlurHiClamp
      write SetAdvancedBlurHiClamp;
    property BlurSelf: boolean read FBlurSelf write SetBlurSelf;
    property RenderBackgroundColor: TColor read FRenderBackgroundColor
      write SetRenderBackgroundColor;
    property OnAdvancedBlurImagePrepareEvent: TgxAdvancedBlurImagePrepareEvent
      read FOnAdvancedBlurImagePrepareEvent
      write SetOnAdvancedBlurImagePrepareEvent;
    property OnBeforeTargetRender: TNotifyEvent read FOnBeforeTargetRender
      write SetOnBeforeTargetRender;
    property OnAfterTargetRender: TNotifyEvent read FOnAfterTargetRender
      write SetOnAfterTargetRender;
  end;

  (* This component blurs everything thatis rendered BEFORE it. So if you want part
    of your scene blured, the other not blured, make sure that the other part is
    rendered after this component. It is fast and does not require shaders.
    Note: it is FPS-dependant. Also also can produce a "blury trail effect", which
    stays on the screen until something new is rendered over it. It can be overcome
    by changing the Material.FrontProperties.Diffuse property. This, however, also
    has a drawback - the picture becomes more blured altogether. For example, if
    your backgroud color is Black, set the Material.FrontProperties.Diffuse to White.
    If it is White, set Material.FrontProperties.Diffuse to Black. I haven't tried
    any others, but I hope you get the idea ;)
    I've seen this effect in different Bruring components, even in shaders, but if
    anyone knows another way to fix this issue - please post it on the glscene newsgroup *)
  TgxMotionBlur = class(TgxCustomSceneObject, IgxInitializable)
  private
    FIntensity: Single;
    function StoreIntensity: boolean;
  protected
    procedure DoOnAddedToParent; override;
    procedure InitializeObject(ASender: TObject;
      const ARci: TgxRenderContextInfo); virtual;
  public
    { This function is only valid AFTER OpenGL has been initialized. }
    function SupportsRequiredExtensions: boolean;
    procedure DoRender(var ARci: TgxRenderContextInfo;
      ARenderSelf, ARenderChildren: boolean); override;
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
  published
    // The more the intensity, the more blur you have.
    property Intensity: Single read FIntensity write FIntensity
      stored StoreIntensity;
    // From TgxBaseSceneObject.
    property Visible;
    property OnProgress;
    property Behaviours;
    property Effects;
    property Hint;
  end;

// ------------------------------------------------------------------------
implementation
// ------------------------------------------------------------------------

const
  EPS = 0.001;

constructor TgxBlur.Create(AOwner: TComponent);
begin
  inherited;
  FBlurDeltaTime := 0.02;
  FBlurTop := 0.01;
  FBlurLeft := 0.01;
  FBlurRight := 0.01;
  FBlurBottom := 0.01;
  FRenderHeight := 256;
  FRenderWidth := 256;
  FViewer := TgxMemoryViewer.Create(Self);
  FPreset := pNone;
  Material.Texture.Disabled := false;
  FAdvancedBlurPasses := 1;
  FAdvancedBlurAmp := 1.1;
  FBlurSelf := true;
  FAdvancedBlurLowClamp := 0;
  FAdvancedBlurHiClamp := 255;
  FRenderBackgroundColor := TColors.Black;
end;

destructor TgxBlur.Destroy;
begin
  FViewer.Free;
  inherited;
end;

procedure TgxBlur.UpdateImageSettings;
var
  B: TBitmap;
begin
  if Material.Texture.Image is TgxBlankImage then
    with TgxBlankImage(Material.Texture.Image) do
    begin
      Width := RenderWidth;
      Height := RenderHeight;
    end
  else if Material.Texture.Image is TgxPersistentImage then
  begin
    B := TgxPersistentImage(Material.Texture.Image).Picture.Bitmap;
    B.Width := 0;
    B.Height := 0;
    B.Width := RenderWidth;
    B.Height := RenderHeight;
  end;

  with FViewer do
  begin
    Width := RenderWidth;
    Height := RenderHeight;
  end;

  SetLength(Pixelbuffer, RenderWidth * RenderHeight);
end;

procedure TgxBlur.DoProgress(const progressTime: TgxProgressTimes);
begin
  inherited;
  if Self.Visible and (progressTime.newTime - OldTime > FBlurDeltaTime) then
  begin
    OldTime := progressTime.newTime;
    if TargetObject <> nil then
      DoMemView(TargetObject);
  end;

end;

procedure TgxBlur.DoMemView(baseObject: TgxbaseSceneObject);
var
  OldFocalLength: Single;
  refsiz: Single;
  BMP: TgxBitmap32;
  x, y: Integer;
  line: PgxPixel32Array;
  by: Integer;
  bp: Integer;
  DoBlur: boolean;

  procedure ApplyBlur(const passes: Integer);
  var
    t: Integer;
    x, y: Integer;
    lin, linu, lind, linuu, lindd: PgxPixel32Array;
    R, G, B: Single;
    ir, ig, ib: Smallint;

    procedure ApplyBlurClampAndSetPixel;
    begin
      // 0.1111 = 1/7 (where 7 is the times each pixel is summed with neighbours or self)
      ir := round(R * FAdvancedBlurAmp * 0.1111);
      ig := round(G * FAdvancedBlurAmp * 0.1111);
      ib := round(B * FAdvancedBlurAmp * 0.1111);

      // Hi Clamp
      if ir > FAdvancedBlurHiClamp then
        ir := FAdvancedBlurHiClamp;
      if ig > FAdvancedBlurHiClamp then
        ig := FAdvancedBlurHiClamp;
      if ib > FAdvancedBlurHiClamp then
        ib := FAdvancedBlurHiClamp;

      lin^[x].R := ir;
      lin^[x].G := ig;
      lin^[x].B := ib;
    end;

  begin
    for t := 0 to passes do
    begin
      for y := 2 to BMP.Height - 3 do
      begin
        linuu := BMP.ScanLine[y - 2];
        linu := BMP.ScanLine[y - 1];
        lin := BMP.ScanLine[y];
        lind := BMP.ScanLine[y + 1];
        lindd := BMP.ScanLine[y + 2];
        by := y * BMP.Height;
        // X = 0 PART:
        x := 0;
        R := lin^[x].R + lin^[x + 1].R + lin^[x + 2].R + linu^[x].R + lind^[x].R
          + linuu^[x].R + lindd^[x].R;
        G := lin^[x].G + lin^[x + 1].G + lin^[x + 2].G + linu^[x].G + lind^[x].G
          + linuu^[x].G + lindd^[x].G;
        B := lin^[x].B + lin^[x + 1].B + lin^[x + 2].B + linu^[x].B + lind^[x].B
          + linuu^[x].B + lindd^[x].B;
        ApplyBlurClampAndSetPixel;
        // X = 1 PART:
        x := 1;
        R := lin^[x].R + lin^[x + 1].R + lin^[x - 1].R + lin^[x + 2].R +
          linu^[x].R + lind^[x].R + linuu^[x].R + lindd^[x].R;
        G := lin^[x].G + lin^[x + 1].G + lin^[x - 1].G + lin^[x + 2].G +
          linu^[x].G + lind^[x].G + linuu^[x].G + lindd^[x].G;
        B := lin^[x].B + lin^[x + 1].B + lin^[x - 1].B + lin^[x + 2].B +
          linu^[x].B + lind^[x].B + linuu^[x].B + lindd^[x].B;
        ApplyBlurClampAndSetPixel;
        // ALL X IN MIDDLE PART:
        for x := 2 to BMP.Width - 3 do
        begin
          R := lin^[x].R + lin^[x + 1].R + lin^[x - 1].R + lin^[x + 2].R +
            lin^[x - 2].R + linu^[x].R + lind^[x].R + linuu^[x].R + lindd^[x].R;
          G := lin^[x].G + lin^[x + 1].G + lin^[x - 1].G + lin^[x + 2].G +
            lin^[x - 2].G + linu^[x].G + lind^[x].G + linuu^[x].G + lindd^[x].G;
          B := lin^[x].B + lin^[x + 1].B + lin^[x - 1].B + lin^[x + 2].B +
            lin^[x - 2].B + linu^[x].B + lind^[x].B + linuu^[x].B + lindd^[x].B;
          ApplyBlurClampAndSetPixel;
        end;
        // X = NEXT TO LAST PART:
        x := BMP.Width - 2;
        R := lin^[x].R + lin^[x + 1].R + lin^[x - 1].R + lin^[x - 2].R +
          linu^[x].R + lind^[x].R + linuu^[x].R + lindd^[x].R;
        G := lin^[x].G + lin^[x + 1].G + lin^[x - 1].G + lin^[x - 2].G +
          linu^[x].G + lind^[x].G + linuu^[x].G + lindd^[x].G;
        B := lin^[x].B + lin^[x + 1].B + lin^[x - 1].B + lin^[x - 2].B +
          linu^[x].B + lind^[x].B + linuu^[x].B + lindd^[x].B;
        ApplyBlurClampAndSetPixel;
        // X = LAST PART:
        x := BMP.Width - 1;
        R := lin^[x].R + lin^[x - 1].R + lin^[x - 2].R + linu^[x].R + lind^[x].R
          + linuu^[x].R + lindd^[x].R;
        G := lin^[x].G + lin^[x - 1].G + lin^[x - 2].G + linu^[x].G + lind^[x].G
          + linuu^[x].G + lindd^[x].G;
        B := lin^[x].B + lin^[x - 1].B + lin^[x - 2].B + linu^[x].B + lind^[x].B
          + linuu^[x].B + lindd^[x].B;
        ApplyBlurClampAndSetPixel;
      end;
    end;
  end;

begin
  if FViewer.Camera <> Scene.CurrentCamera then
    FViewer.Camera := Scene.CurrentCamera;

  if FViewer.Camera <> nil then
  begin
    FDoingMemView := true;

    // Scene.RenderScene(FViewer.Buffer,FViewer.Width,FViewer.Height,dsRendering,baseObject);
    FViewer.Camera.BeginUpdate;

    OldFocalLength := FViewer.Camera.FocalLength;

    // CALCULATE SCALED FOCAL LENGTH FOR VIEWER
    if Scene.CurrentBuffer.Width > Scene.CurrentBuffer.Height then
      refsiz := Scene.CurrentBuffer.Width
    else
      refsiz := Scene.CurrentBuffer.Height;

    FViewer.Camera.FocalLength := FViewer.Camera.FocalLength *
      FViewer.Buffer.Width / refsiz;

    if FViewer.Buffer.BackgroundColor <> FRenderBackgroundColor then
      FViewer.Buffer.BackgroundColor := FRenderBackgroundColor;

    try
      case FBlur of
        bNone:
          begin
            // do nothing
          end;
        bSimple:
          begin
            if Assigned(FOnBeforeTargetRender) then
              FOnBeforeTargetRender(Self);
            // RENDER
            FViewer.Render(baseObject);
            // Copy to texture (unfortunatelly, after this, the bitmap cannot be red back from the hardware.. i think)
            FViewer.CopyToTexture(Material.Texture);
            if Assigned(FOnAfterTargetRender) then
              FOnAfterTargetRender(Self);
          end;
        bAdvanced:
          begin
            if Assigned(FOnBeforeTargetRender) then
              FOnBeforeTargetRender(Self);

            // RENDER
            FViewer.Render(baseObject);
            // Read pixels from buffer. This is slow, but ok with reasonably small render size.
            FViewer.Buffer.RenderingContext.Activate;
            try
              glReadPixels(0, 0, FViewer.Buffer.Width, FViewer.Buffer.Height,
                GL_RGB, GL_UNSIGNED_BYTE, Pixelbuffer);
            except
              FViewer.Buffer.RenderingContext.Deactivate;
            end;
            if Assigned(FOnAfterTargetRender) then
              FOnAfterTargetRender(Self);

            BMP := Material.Texture.Image.GetBitmap32;
            BMP.Narrow;
            FViewer.Buffer.RenderingContext.Deactivate;
            // FILLS THE BITMAP with the pixelbuffer captured from the internal memoryViewer
            for y := 0 to RenderHeight - 1 do
            begin
              line := BMP.ScanLine[y];
              by := y * RenderHeight;
              for x := 0 to RenderWidth - 1 do
              begin
                bp := x + by;
                line^[x].R := Pixelbuffer[bp].R;
                line^[x].G := Pixelbuffer[bp].G;
                line^[x].B := Pixelbuffer[bp].B;

                // Low clamp
                if line^[x].R < FAdvancedBlurLowClamp then
                  line^[x].R := 0;
                if line^[x].G < FAdvancedBlurLowClamp then
                  line^[x].G := 0;
                if line^[x].B < FAdvancedBlurLowClamp then
                  line^[x].B := 0;
              end;
            end;

            DoBlur := true;
            if Assigned(FOnAdvancedBlurImagePrepareEvent) then
            begin
              FOnAdvancedBlurImagePrepareEvent(Self, BMP, DoBlur);
            end;

            if DoBlur then
              ApplyBlur(FAdvancedBlurPasses);

            Material.Texture.Image.NotifyChange(Self);

          end;
      end;
    finally
      FViewer.Camera.FocalLength := OldFocalLength;
      FViewer.Camera.EndUpdate;
      FDoingMemView := false;
    end;
  end;
end;

{$WARNINGS Off} // Suppress "unsafe" warning

procedure TgxBlur.DoRender(var ARci: TgxRenderContextInfo;
  ARenderSelf, ARenderChildren: boolean);
var
  vx, vy, vx1, vy1, f: Single;
  offsx, offsy: Single;
  MaxMeasure: Integer;
begin
  if FDoingMemView and (FBlurSelf = false) then
    Exit;
  if (csDesigning in ComponentState) then
  begin
    if Count > 0 then
      Self.RenderChildren(0, Count - 1, ARci);
    Exit;
  end;
  if ARci.ignoreMaterials then
    Exit;
  Material.Apply(ARci);
  repeat
    if AlphaChannel <> 1 then
      ARci.gxStates.SetMaterialAlphaChannel(GL_FRONT, AlphaChannel);
    // Prepare matrices
    glMatrixMode(GL_MODELVIEW);
    glPushMatrix;
    glLoadMatrixf(@TgxSceneBuffer(ARci.Buffer).BaseProjectionMatrix);
    if ARci.renderDPI = 96 then
      f := 1
    else
      f := ARci.renderDPI / 96;
    glScalef(2 / ARci.viewPortSize.cx, 2 / ARci.viewPortSize.cy, 1);

    // center of viewport:
    glTranslatef(0, 0, Position.Z);

    if Rotation <> 0 then
      glRotatef(Rotation, 0, 0, 1);
    glMatrixMode(GL_PROJECTION);
    glPushMatrix;
    glLoadIdentity;
    ARci.gxStates.Disable(stDepthTest);
    ARci.gxStates.DepthWriteMask := false;

    // calculate offsets in order to keep the quad a square centered in the view
    if ARci.viewPortSize.cx > ARci.viewPortSize.cy then
    begin
      offsx := 0;
      offsy := (ARci.viewPortSize.cx - ARci.viewPortSize.cy) * 0.5;
      MaxMeasure := ARci.viewPortSize.cx;
    end
    else
    begin
      offsx := (ARci.viewPortSize.cy - ARci.viewPortSize.cx) * 0.5;
      offsy := 0;
      MaxMeasure := ARci.viewPortSize.cy;
    end;

    // precalc coordinates
    vx := -ARci.viewPortSize.cx * 0.5 * f;
    vx1 := vx + ARci.viewPortSize.cx * f;
    vy := +ARci.viewPortSize.cy * 0.5 * f;
    vy1 := vy - ARci.viewPortSize.cy * f;

    vx := vx - offsx;
    vx1 := vx1 + offsx;
    vy := vy + offsy;
    vy1 := vy1 - offsy;

    // Cause the radial scaling
    if FDoingMemView then
    begin
      vx := vx - FBlurLeft * MaxMeasure;
      vx1 := vx1 + FBlurRight * MaxMeasure;
      vy := vy + FBlurTop * MaxMeasure;
      vy1 := vy1 - FBlurBottom * MaxMeasure;
    end;

    // issue quad
    glBegin(GL_QUADS);
    glNormal3fv(@YVector);
    glTexCoord2f(0, 0);
    glVertex2f(vx, vy1);
    glTexCoord2f(XTiles, 0);
    glVertex2f(vx1, vy1);
    glTexCoord2f(XTiles, YTiles);
    glVertex2f(vx1, vy);
    glTexCoord2f(0, YTiles);
    glVertex2f(vx, vy);
    glEnd;
    // restore state
    glPopMatrix;
    glMatrixMode(GL_MODELVIEW);
    glPopMatrix;
  until not Material.UnApply(ARci);
  if Count > 0 then
    Self.RenderChildren(0, Count - 1, ARci);
end;

procedure TgxBlur.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = FTargetObject then
      FTargetObject := nil;
  inherited;
end;

{$WARNINGS On}

procedure TgxBlur.SetRenderBackgroundColor(const Value: TColor);
begin
  FRenderBackgroundColor := Value;
end;

procedure TgxBlur.SetRenderHeight(const Value: Integer);
begin
  FRenderHeight := Value;
  UpdateImageSettings;
end;

procedure TgxBlur.SetRenderWidth(const Value: Integer);
begin
  FRenderWidth := Value;
  UpdateImageSettings;
end;

procedure TgxBlur.SetTargetObject(const Value: TgxbaseSceneObject);
begin
  FTargetObject := Value;
end;

procedure TgxBlur.SetAdvancedBlurAmp(const Value: Single);
begin
  FAdvancedBlurAmp := Value;
end;

procedure TgxBlur.SetAdvancedBlurHiClamp(const Value: byte);
begin
  FAdvancedBlurHiClamp := Value;
end;

procedure TgxBlur.SetAdvancedBlurLowClamp(const Value: byte);
begin
  FAdvancedBlurLowClamp := Value;
end;

procedure TgxBlur.SetAdvancedBlurPasses(const Value: Integer);
begin
  FAdvancedBlurPasses := Value;
end;

procedure TgxBlur.SetBlur(const Value: TgxBlurkind);
begin
  if FBlur <> Value then
  begin
    case Value of
      bNone:
        begin
          // do Nothing
        end;
      bSimple:
        begin
          Material.Texture.ImageClassName := TgxBlankImage.ClassName;
        end;
      bAdvanced:
        begin
          Material.Texture.ImageClassName := TgxPersistentImage.ClassName;
        end;
    end;
    UpdateImageSettings;
  end;
  FBlur := Value;
end;

procedure TgxBlur.SetBlurSelf(const Value: boolean);
begin
  FBlurSelf := Value;
end;

procedure TgxBlur.SetOnAdvancedBlurImagePrepareEvent
  (const Value: TgxAdvancedBlurImagePrepareEvent);
begin
  FOnAdvancedBlurImagePrepareEvent := Value;
end;

procedure TgxBlur.SetOnAfterTargetRender(const Value: TNotifyEvent);
begin
  FOnAfterTargetRender := Value;
end;

procedure TgxBlur.SetOnBeforeTargetRender(const Value: TNotifyEvent);
begin
  FOnBeforeTargetRender := Value;
end;

procedure TgxBlur.SetPreset(const Value: TgxBlurPreset);
begin
  FPreset := Value;

  case FPreset of
    pNone:
      begin
        // do nothing
      end;
    pAdvancedBlur:
      begin
        Blur := bAdvanced;
        Material.BlendingMode := bmAdditive;
        Material.FrontProperties.Diffuse.SetColor(1, 1, 1, 1);
        BlurTop := 0;
        BlurLeft := 0;
        BlurRight := 0;
        BlurBottom := 0;
        BlurDeltaTime := 0;
        BlurSelf := false;
        AdvancedBlurPasses := 1;
        AdvancedBlurAmp := 1.2;
        RenderWidth := 64;
        RenderHeight := 64;
      end;
    pGlossy:
      begin
        Material.BlendingMode := bmAdditive;
        Material.FrontProperties.Diffuse.SetColor(1, 1, 1, 0.7);
        BlurTop := 0.02;
        BlurLeft := 0.02;
        BlurRight := 0.02;
        BlurBottom := 0.02;
        BlurDeltaTime := 0.02;
        BlurSelf := true;
      end;
    pBeastView:
      begin
        Blur := bSimple;
        Material.BlendingMode := bmAdditive;
        Material.FrontProperties.Diffuse.SetColor(1, 0, 0, 0.8);
        BlurTop := 0.001;
        BlurLeft := 0.03;
        BlurRight := 0.03;
        BlurBottom := 0.001;
        BlurDeltaTime := 0.02;
        BlurSelf := true;
      end;
    pOceanDepth:
      begin
        Blur := bSimple;
        Material.BlendingMode := bmTransparency;
        Material.FrontProperties.Diffuse.SetColor(0.2, 0.2, 1, 0.99);
        BlurTop := 0.04;
        BlurLeft := 0.02;
        BlurRight := 0.02;
        BlurBottom := 0.04;
        BlurDeltaTime := 0.02;
        BlurSelf := true;
      end;
    pDream:
      begin
        Blur := bSimple;
        Material.BlendingMode := bmTransparency;
        Material.FrontProperties.Diffuse.SetColor(1, 1, 1, 0.992);
        BlurTop := 0.02;
        BlurLeft := 0.02;
        BlurRight := 0.02;
        BlurBottom := 0.02;
        BlurDeltaTime := 0.1;
        BlurSelf := true;
      end;
    pOverBlur:
      begin
        Blur := bSimple;
        Material.BlendingMode := bmAdditive;
        Material.FrontProperties.Diffuse.SetColor(0.95, 0.95, 0.95, 0.98);
        BlurTop := 0.01;
        BlurLeft := 0.01;
        BlurRight := 0.01;
        BlurBottom := 0.01;
        BlurDeltaTime := 0.02;
        BlurSelf := true;
      end;
  end;

end;

function TgxBlur.StoreBlurBottom: boolean;
begin
  Result := Abs(FBlurBottom - 0.01) > EPS;
end;

function TgxBlur.StoreBlurDeltaTime: boolean;
begin
  Result := Abs(FBlurDeltaTime - 0.02) > EPS;
end;

function TgxBlur.StoreBlurLeft: boolean;
begin
  Result := Abs(FBlurLeft - 0.01) > EPS;
end;

function TgxBlur.StoreBlurRight: boolean;
begin
  Result := Abs(FBlurRight - 0.01) > EPS;
end;

function TgxBlur.StoreBlurTop: boolean;
begin
  Result := Abs(FBlurTop - 0.01) > EPS;
end;

// -------------------------------------------------
// TgxMotionBlur
// -------------------------------------------------
procedure TgxMotionBlur.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TgxMotionBlur then
  begin
    FIntensity := TgxMotionBlur(Source).FIntensity;
  end;
end;

constructor TgxMotionBlur.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Material.FrontProperties.Diffuse.Initialize(clrBlack);
  Material.MaterialOptions := [moNoLighting, moIgnoreFog];
  Material.Texture.Disabled := false;
  Material.BlendingMode := bmTransparency;
  FIntensity := 0.975;
end;

procedure TgxMotionBlur.DoOnAddedToParent;
begin
  inherited;
  // Request to be initialized on next render.
  if Scene <> nil then
    Scene.InitializableObjects.Add(Self);
end;

procedure TgxMotionBlur.DoRender(var ARci: TgxRenderContextInfo;
  ARenderSelf, ARenderChildren: boolean);
begin
  if not(ARci.ignoreMaterials or (csDesigning in ComponentState) or
    (ARci.drawState = dsPicking)) then
    with ARci.gxStates do
    begin
      ARci.ignoreDepthRequests := true;
      Material.Apply(ARci);
      ActiveTextureEnabled[ttTextureRect] := true;
      glMatrixMode(GL_PROJECTION);
      glPushMatrix;
      glLoadIdentity;
      glOrtho(0, ARci.viewPortSize.cx, ARci.viewPortSize.cy, 0, 0, 1);
      glMatrixMode(GL_MODELVIEW);
      glPushMatrix;
      glLoadIdentity;
      Disable(stDepthTest);
      DepthWriteMask := false;

      glBegin(GL_QUADS);
      glTexCoord2f(0.0, ARci.viewPortSize.cy);
      glVertex2f(0, 0);
      glTexCoord2f(0.0, 0.0);
      glVertex2f(0, ARci.viewPortSize.cy);
      glTexCoord2f(ARci.viewPortSize.cx, 0.0);
      glVertex2f(ARci.viewPortSize.cx, ARci.viewPortSize.cy);
      glTexCoord2f(ARci.viewPortSize.cx, ARci.viewPortSize.cy);
      glVertex2f(ARci.viewPortSize.cx, 0);
      glEnd;

      glPopMatrix;
      glMatrixMode(GL_PROJECTION);
      glPopMatrix;
      glMatrixMode(GL_MODELVIEW);
      ActiveTextureEnabled[ttTextureRect] := false;
      Material.UnApply(ARci);
      ARci.ignoreDepthRequests := false;

      glCopyTexImage2D(GL_TEXTURE_RECTANGLE, 0, GL_RGB, 0, 0,
        ARci.viewPortSize.cx, ARci.viewPortSize.cy, 0);

      Material.FrontProperties.Diffuse.Alpha := FIntensity;
    end;

  if Count <> 0 then
    Self.RenderChildren(0, Count - 1, ARci);
end;

procedure TgxMotionBlur.InitializeObject(ASender: TObject;
  const ARci: TgxRenderContextInfo);
begin
  // If extension is not supported, silently disable this component.
  if not(csDesigning in ComponentState) then
    if not SupportsRequiredExtensions then
      Visible := false;
end;

function TgxMotionBlur.StoreIntensity: boolean;
begin
  Result := Abs(FIntensity - 0.975) > EPS;
end;

function TgxMotionBlur.SupportsRequiredExtensions: boolean;
begin
  Result := true;
  // (GL_TEXTURE_RECTANGLE or GL_TEXTURE_RECTANGLE_ARB or GL_TEXTURE_RECTANGLE_NV);
end;

// ------------------------------------------------------------------
initialization

// ------------------------------------------------------------------

RegisterClass(TgxBlur);
RegisterClass(TgxMotionBlur);

end.
