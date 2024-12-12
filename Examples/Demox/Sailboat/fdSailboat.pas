unit fdSailboat; // the Sailboat 3d view - Used by SailboatDemo and OPYC game
// ------------------//  a FMX 3d sailboat simulation, w/ waves, ship, clouds, birds..

interface

{ ..$DEFINE OPYC }   // false for SailboatDemo, true for OPYC game

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Math,
  System.Math.Vectors,

  FMX.Forms,

{$IFDEF USE_SKIA}  // tried SKIA but didnt work (app hangs)
  // FMX.Types,
  // Skia.FMX,
{$ENDIF USE_SKIA}
  FMX.Types,
  FMX.Controls,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Ani,
  FMX.MaterialSources,
  FMX.Controls3D,
  FMX.Objects3D,
  FMX.Viewport3D,
  FMX.StdCtrls,
  FMX.Controls.Presentation,
  FMX.Objects,
  FMX.Layouts,
  FMX.Edit,
  FMX.EditBox,
  FMX.SpinBox,
  FMX.Types3D,
  FMX.ListBox,
  FMX.Gestures,
{$IFDEF OPYC}
  fmxStateBox,
  // TfmxStateBox - component persists form control states to ini file
  // Note: fmxStateBox.pas must be installed as a design component, prior to loading this form
{$ENDIF OPYC}

  GXS.SailSurface,
  GXS.OceanWaves,
  GBE.Clouds,
  GBE.Heightmap;

const
  // For some reason, the 3d world boat is  3m long, while the box2d boat is 10 m long
  // The correct thing to do is to match scales
  // as a workaround I used this scale factor ( 0.3 )
  B2Dto3Dscale = 0.30;

  // OPYC characters, 3d style
  charDolphin = 1;
  charWhiteBirds = 2;
  charBrownBirds = 3;
  charBoiaCross = 4;
  charPelican = 5;
  charPurpleBoat = 6;
  charContainer = 7;
  charRock = 8; // rock w/ lighthouse
  charBarril = 9; // floating barrel
  charWhale = 10; // jumping humpback whale

  numMarks = 4; // up to 8 simultaneous marks

type
  TPointF_Array = array of System.Types.TPointF;
  // yet another ... for sail segments exports

  TFormSailboatDemo = class(TForm)
    Viewport3D1: TViewport3D;
    FloatAnimation1: TFloatAnimation;
    tbAmplitude: TTrackBar;
    Label1: TLabel;
    Label2: TLabel;
    tbWaveLenght: TTrackBar;
    Label3: TLabel;
    tbVitesse: TTrackBar;
    Label4: TLabel;
    SpinBox1: TSpinBox;
    Label5: TLabel;
    SpinBox2: TSpinBox;
    Label6: TLabel;
    SpinBox3: TSpinBox;
    ColorMaterialSource1: TColorMaterialSource;
    cbShowLines: TSwitch;
    Label7: TLabel;
    Label8: TLabel;
    tbOpacite: TTrackBar;
    textureOceanSurface: TLightMaterialSource;
    Light1: TLight;
    OceanSurface: TgxOceanSurface;
    modelBoat: TModel3D;
    Camera1: TCamera;
    Label9: TLabel;
    tbCap: TTrackBar;
    tbHeel: TTrackBar;
    Label10: TLabel;
    dummyBoatCap: TDummy;
    cubeBoat: TCube;
    tbSelObjY: TTrackBar;
    tbSelObjZ: TTrackBar;
    labSelObjY: TLabel;
    labSelObjZ: TLabel;
    labBoatPitch: TLabel;
    modelBoatMat01: TLightMaterialSource;
    modelBoatMat11: TLightMaterialSource;
    modelBoatMat12: TLightMaterialSource;
    MainSail: TgxSailSurface;
    JibSail: TgxSailSurface;
    materialMainSail: TLightMaterialSource;
    texJibSail: TLightMaterialSource;
    Label11: TLabel;
    cbMoveSea: TSwitch;
    Label12: TLabel;
    cbDesignCamera: TSwitch;
    tbSelObjX: TTrackBar;
    labSelObjX: TLabel;
    comboSelObj: TComboBox;
    dummyBoatPitch: TDummy;
    labxxx: TLabel;
    tbAngleOfView: TTrackBar;
    dummyJib: TDummy;
    dummyBoom: TDummy;
    Label13: TLabel;
    tbMainRot: TTrackBar;
    Label14: TLabel;
    tbJibRot: TTrackBar;
    Label15: TLabel;
    tbBoatSpeed: TTrackBar;
    sphereRock: TSphere;
    materialBoia: TLightMaterialSource;
    dummyBoatHeel: TDummy;
    cubeJibStay: TCube;
    materialSilver: TColorMaterialSource;
    cylinderBoom: TCylinder;
    planeBoiaMan: TPlane;
    materialBoiaMan: TLightMaterialSource;
    dummyBoiaMan: TDummy;
    materialPelican: TLightMaterialSource;
    dummyPelican: TDummy;
    planePelican: TPlane;
    diskBubble: TDisk;
    listboxControls: TListBox;
    lbiBoatControls: TListBoxItem;
    lbiWaveSettings: TListBoxItem;
    lbiSelectedObject: TListBoxItem;
    lbiCameraControls: TListBoxItem;
    lbiTerrain: TListBoxItem;
    labMainRot: TLabel;
    labJibRot: TLabel;
    labBoatSpeed: TLabel;
    LabHeel: TLabel;
    labCap: TLabel;
    labAmplitude: TLabel;
    labLongueur: TLabel;
    labVitesse: TLabel;
    labOpacite: TLabel;
    labCameraViewAngle: TLabel;
    comboWave: TComboBox;
    WaveSystem1: TgxWaveSystem;
    OceanSurfaceTop: TgxOceanSurface;
    diskSeaHorizon: TDisk;
    OceanSurfaceLeft: TgxOceanSurface;
    cylinderLighthouse: TCylinder;
    materialFarol: TLightMaterialSource;
    cylinderLighthouseTop: TCylinder;
    textureRock: TLightMaterialSource;
    labCountTerrainBuilds: TLabel;
    labDataTexCoordinates: TLabel;
    dummyRock: TDummy;
    btnToggleControls: TSpeedButton;
    dummyMain: TDummy;
    texMainSail: TLightMaterialSource;
    texCodeZero: TLightMaterialSource;
    texSpinaker: TLightMaterialSource;
    dummyCameraTarget: TDummy;
    Label18: TLabel;
    tbCameraAz: TTrackBar;
    labCameraAz: TLabel;
    text3dNorth: TText3D;
    Label19: TLabel;
    tbCameraElev: TTrackBar;
    labCameraElev: TLabel;
    WaveSystem2: TgxWaveSystem;
    OceanSurfaceBot: TgxOceanSurface;
    OceanSurfaceRight: TgxOceanSurface;
    text3dSouth: TText3D;
    btnRandomizeWaveSystem1: TSpeedButton;
    btnCloseControls: TSpeedButton;
    labFPS: TLabel;
    timerOneSecondTick: TTimer;
    Light2: TLight;
    text3DWest: TText3D;
    text3DEast: TText3D;
    textureSeaPhoto: TLightMaterialSource;
    materialBrownBirds: TLightMaterialSource;
    materialPurpleBoat: TLightMaterialSource;
    materialDolphinsTrio: TLightMaterialSource;
    materialWhiteBirds: TLightMaterialSource;
    dummyBrownBirds: TDummy;
    planeBrownBirds: TPlane;
    dummy3Dolphins: TDummy;
    dummyPurpleBoat: TDummy;
    planePurplaBoat: TPlane;
    dummyWhiteBirds: TDummy;
    planeWhiteBirds: TPlane;
    materialMAERSK: TLightMaterialSource;
    dummyContainer: TDummy;
    cubeContainer: TCube;
    btnSetWaveOrigine: TSpeedButton;
    materialFlag: TLightMaterialSource;
    birutaSail: TgxSailSurface;
    ImageTerrain: TImage;
    heightmapTerrain: TGBEHeightmap;
    Label16: TLabel;
    cbUseRamp: TSwitch;
    materialTerrain: TLightMaterialSource;
    materialTerrainRamp: TLightMaterialSource;
    dummyTerrain: TDummy;
    materialCloud1: TTextureMaterialSource;
    materialCloud2: TTextureMaterialSource;
    materialCloud3: TTextureMaterialSource;
    GBEClouds1: TGBEClouds;
    Label17: TLabel;
    cbClouds: TSwitch;
    rectBitmapCenter: TRectangle;
    dummyBarril: TDummy;
    cylinderBarril: TCylinder;
    textureSeaSurfaceLargeScale: TLightMaterialSource;
    cylinderBuoy: TCylinder;
    colorBuoy: TColorMaterialSource;
    dummyMark: TDummy;
    lightmaterialMark: TLightMaterialSource;
    rectBlackListboxBackground: TRectangle;
    Container: TLayout;
    ContainerMaterials: TLayout;
    btnClose3dview: TButton;
    modelBoatMat13: TLightMaterialSource;
    modelBoatMat14: TLightMaterialSource;
    modelBoatMat02: TLightMaterialSource;
    dummyRudder: TDummy;
    cylinderRudder: TCylinder;
    dummyCrew: TDummy;
    planeChuck: TPlane;
    materialChuck: TLightMaterialSource;
    materialIvone: TLightMaterialSource;
    materialWheel: TLightMaterialSource;
    materialCatraca: TLightMaterialSource;
    planeIvone: TPlane;
    textureWindArrow: TLightMaterialSource;
    dummyWindArrow: TDummy;
    WindArrow1: TgxWindArrowSurface;
    dummyShip: TDummy;
    modelShip: TModel3D;
    modelShipMat01: TLightMaterialSource;
    modelDolphin: TModel3D;
    modelDolphinMat01: TLightMaterialSource;
    LabXX: TLabel;
    cbShowDolphin: TSwitch;
    Label20: TLabel;
    cbShowWindArrow: TSwitch;
    dummyWhale: TDummy;
    modelWhale: TModel3D;
    modelWhaleMat01: TLightMaterialSource;
    labDerivative: TLabel;
    procedure FloatAnimation1Process(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tbAmplitudeTracking(Sender: TObject);
    procedure tbWaveLenghtTracking(Sender: TObject);
    procedure tbVitesseTracking(Sender: TObject);
    procedure SpinBox1ChangeClick(Sender: TObject);
    procedure cbShowLinesSwitch(Sender: TObject);
    procedure tbOpaciteTracking(Sender: TObject);
    procedure tbCapTracking(Sender: TObject);
    procedure tbHeelTracking(Sender: TObject);
    procedure tbSelObjYTracking(Sender: TObject);
    procedure tbSelObjZTracking(Sender: TObject);
    procedure cbDesignCameraSwitch(Sender: TObject);
    procedure tbSelObjXTracking(Sender: TObject);
    procedure comboSelObjChange(Sender: TObject);
    procedure tbAngleOfViewTracking(Sender: TObject);
    procedure tbMainRotTracking(Sender: TObject);
    procedure tbJibRotTracking(Sender: TObject);
    procedure tbBoatSpeedTracking(Sender: TObject);
    procedure comboWaveChange(Sender: TObject);
    procedure lbTexCoordXTracking(Sender: TObject);
    procedure btnToggleControlsClick(Sender: TObject);
    procedure tbCameraAzTracking(Sender: TObject);
    procedure tbCameraElevTracking(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
    procedure btnRandomizeWaveSystem1Click(Sender: TObject);
    procedure timerOneSecondTickTimer(Sender: TObject);
    procedure Viewport3D1Paint(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
    procedure cbUseRampSwitch(Sender: TObject);
    procedure cbCloudsSwitch(Sender: TObject);
    procedure btnClose3dviewClick(Sender: TObject);
    procedure FormGesture(Sender: TObject; const EventInfo: TGestureEventInfo;
      var Handled: Boolean);
    procedure Viewport3D1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Viewport3D1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Single);
    procedure Viewport3D1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure FormActivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbShowDolphinSwitch(Sender: TObject);
    procedure cbShowWindArrowSwitch(Sender: TObject);
  private
    fFirstShow: Boolean;
    fBowSail: Integer;

    fLastFPS: TDatetime; // last time fPS was computed
    fLastFPScount: Integer;
    fFPS: Single;
    fFrameCount: Integer; // Viewport paint count

    fMarks: Array [1 .. numMarks] of TDummy; // fixed number of gater marks
    // gesture related vars
    fLastDistance: Integer; // last distance between fingers
    fLastPanLocation: TPointF;
    fMouseDragBeginPt: TPointF;
    fMouseDragging: Boolean;
    fTimeDolphinAnim: Single; // in sec

    function getSelected3DObject: TControl3d;
    function getSelectedWave: Integer;
    procedure setBowSail(const Value: Integer);
    procedure WaveSystem1RandomOrigines;
    procedure LoadTerrain;
    procedure InitClouds;
    procedure CreateMarks;
    procedure handlePan(EventInfo: TGestureEventInfo);
    procedure handleZoom(EventInfo: TGestureEventInfo);
    procedure handlePanDelta(aDelta: TPointF);
    procedure ScaleSailsForDemo;
  public
    fWasInsideSeasurface: Boolean;
    center: TPoint3D;
    fBoatAttitude: TPoint3D; // (pitch, yaw, roll)
    // OPYC automation ( that is Box2D simulation controlling everything from 2D world )
    procedure SetBoatState(const aCap, aHeel, aSpeed, aBoomAngle, aRudderAngle,
      aWindDir, aWindSpeed: Single);
    procedure SetSailShape(ixSail: Integer; aPtArray: TPointF_Array);
    procedure Set3DcharacterState(ix: Integer; const X, Y, alfa: Single);
    // ix = which char
    procedure Set3dMarks(ix: Integer; const ax, ay: Single);
    procedure CrewRandomPositions; // move Ivone and Chuck along the boat lenght
    procedure Begin3DChange;
    procedure End3DChange;
    procedure ChangeZoom(WheelDelta: Integer); // changes Angle of view
    procedure DoSaveState;
    procedure SetTerrainBitmap(bVisible: Boolean; aBMP: TBitmap);

    property BowSail: Integer read fBowSail write setBowSail;
  end;

var
  FormSailboatDemo: TFormSailboatDemo = nil;

implementation //-------------------------------------------------------------

{$IFDEF OPYC}
// undef for SailboatDemo, define for OPYC ( integration to sailing game )
uses
  fSailboatApp;
{$ENDIF OPYC}
{$R *.fmx}

function RandomFloat(const lo, hi: Single): Single;
begin
  Result := (hi - lo) * Random + lo;
end;

{ TForm1 }

procedure TFormSailboatDemo.CrewRandomPositions;
begin
  // move characters along the boat length. x in this case
  planeChuck.Position.X := RandomFloat(-1.3, 0.1);
  planeChuck.RotationAngle.Y := RandomFloat(45, 120);

  planeIvone.Position.X := RandomFloat(-1.2, 0.9);
  planeIvone.RotationAngle.Y := RandomFloat(450, 150);

  dummy3Dolphins.Position.X := RandomFloat(-2, 4);
  dummy3Dolphins.Position.Y := RandomFloat(-4, 2);

  // dummyWhale.Position.x  :=  RandomFloat( -10,  8 );
  // dummyWhale.Position.y  :=  RandomFloat( -10,  5 );
end;

procedure TFormSailboatDemo.FormActivate(Sender: TObject);
begin
  if fFirstShow then
  // on first show, retrieve control states ( and all extra persistent form state )
  begin
{$IFDEF OPYC}
    try
      StateBox.ReadStateFromIni; // retrieve state
    except // ignore read state error   // possibly not saved ( first use or file not found )
    end;
    tbAngleOfViewTracking(nil);
    // update camera w/ retrieved camera state. call tracking events
    tbCameraAzTracking(nil);
    tbCameraElevTracking(nil);
{$ENDIF OPYC}
{$IFNDEF OPYC}      // not OPYC --> is SailboatDemo
    ScaleSailsForDemo; // set default boat state and adjust sail sizes
{$ENDIF OPYC}
    fFirstShow := false; // reset 1st show
  end;
end;

procedure TFormSailboatDemo.FormCreate(Sender: TObject);
begin
  fFirstShow := true;

  // init trackbars w/ selected wave
  tbAmplitude.Value := WaveSystem1.Amplitude;
  tbWaveLenght.Value := WaveSystem1.Longueur;
  tbVitesse.Value := WaveSystem1.Vitesse;
  tbOpacite.Value := OceanSurface.Opacity;

  fBoatAttitude := Point3D(0, 0, 0); // (pitch, yaw, roll)
  fWasInsideSeasurface := true; // start inside

  FloatAnimation1.Start;

  // the wave system Origines are not editable at design time. So we set defauts here
  WaveSystem1RandomOrigines;
  CrewRandomPositions;
  // make crew move around ( front and aft, mostly doing nothing in this state-of-the-art sailboat )

  // WaveSystem1.Origine  := Point3D(0,0,0);
  // WaveSystem1.Origine2 := Point3D(0,0,20000);
  // WaveSystem1.Origine3 := Point3D(-10000,0,-5000);

  fBowSail := 0; // 0=jib 1=genoa 2=spi 3=main only

  fLastFPS := 0; // never
  fLastFPScount := 0;
  fFPS := 0;
  fFrameCount := 0; // boat simulation paintbox paint count

  fTimeDolphinAnim := 0;

  fLastPanLocation := PointF(0, 0);

  LoadTerrain;
  InitClouds;
  CreateMarks;

  rectBlackListboxBackground.Visible := true;
  // controls shown by default at runtime

  fMouseDragBeginPt := PointF(0, 0);
  fMouseDragging := false;

  // gbePlaneWindArrow.Origine := Point3d(0, -10,0);  // Origina cannot be set at design time !! :(
  // set WindArrow Origine to have a wave ging forward

end;

procedure TFormSailboatDemo.FormDestroy(Sender: TObject);
begin
{$IFDEF MSWINDOWS} // on windows use form destroy to save state (desktop + controls)
  DoSaveState;
{$ENDIF MSWINDOWS}
end;

procedure TFormSailboatDemo.handlePanDelta(aDelta: TPointF);
var
  aValue: Single;
begin
  if (aDelta.X <> 0) then // horiz pan --> change Camera Az
  begin
    aValue := tbCameraAz.Value;
    aValue := aValue + aDelta.X / 5; // linear pan - 5 ad hoc
    if aValue > tbCameraAz.Max then
      aValue := aValue - tbCameraAz.Max
    else if aValue < tbCameraAz.Min then
      aValue := tbCameraAz.Max + aValue; // ?
    tbCameraAz.Value := aValue;
  end;

  if (aDelta.Y <> 0) then // vert pan, chg elev
  begin
    aValue := tbCameraElev.Value;
    aValue := aValue * (1 + aDelta.Y / 100);
    // this gives a quadratic elevation pan
    if aValue > tbCameraElev.Max then
      aValue := tbCameraElev.Max
    else if aValue < tbCameraElev.Min then
      aValue := tbCameraElev.Min;
    tbCameraElev.Value := aValue;
  end;
end;

procedure TFormSailboatDemo.handlePan(EventInfo: TGestureEventInfo);
var
  Delta: System.Types.TPointF;
begin
  if (TInteractiveGestureFlag.gfBegin in EventInfo.Flags) then
  // begin. save inicial state
  begin
    fLastPanLocation := EventInfo.Location; // save (center?) point
  end
  else if (TInteractiveGestureFlag.gfEnd in EventInfo.Flags) then
  // end. do nothing
  begin
    // nada
  end
  else
  begin
    // inicialize last, if needed (should not)
    if (fLastPanLocation.X = 0) and (fLastPanLocation.Y = 0) then
      fLastPanLocation := EventInfo.Location;

    Delta := EventInfo.Location - fLastPanLocation;

    handlePanDelta(Delta);

    // save new previous
    fLastPanLocation := EventInfo.Location;
  end;
end;

// gesture zoom controls camera ViewAngle
procedure TFormSailboatDemo.handleZoom(EventInfo: TGestureEventInfo);
var
  aScale, K, aValue: Single;
begin
  if (not(TInteractiveGestureFlag.gfBegin in EventInfo.Flags)) and
    (not(TInteractiveGestureFlag.gfEnd in EventInfo.Flags)) then
  begin
    if (fLastDistance > 0) then // sanity test
    begin
      aValue := tbAngleOfView.Value;
      K := EventInfo.Distance / fLastDistance; // chg zoom factor
      if (K > 0) then
      begin
        aValue := aValue * K;
        if aValue > tbAngleOfView.Max then
          aValue := tbAngleOfView.Max
        else if aValue < tbAngleOfView.Min then
          aValue := tbAngleOfView.Min;
        tbAngleOfView.Value := aValue;
      end;
    end;
  end;
  fLastDistance := EventInfo.Distance;
end;

procedure TFormSailboatDemo.FormGesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
var
  LObj: IControl;
begin
  if (EventInfo.GestureID = igiPan) then
    Label12.Text := 'pan'
  else if (EventInfo.GestureID = igiZoom) then
    Label12.Text := 'zoom'
  else
    Label12.Text := 'gesture?';

  LObj := Self.ObjectAtPoint(ClientToScreen(EventInfo.Location));
  if (LObj is TViewport3D) then
  begin
    if (EventInfo.GestureID = igiPan) then
      handlePan(EventInfo)
    else if (EventInfo.GestureID = igiZoom) then
      handleZoom(EventInfo);
    Handled := true;
  end;
end;

procedure TFormSailboatDemo.CreateMarks;
var
  aDummy: TDummy;
  aProxy: TProxyObject;
  i: Integer;
begin
  for i := 1 to numMarks do
  begin
    aDummy := TDummy.Create(Self); // fixed number of gater marks
    aDummy.Visible := false;
    aProxy := TProxyObject.Create(Self);
    aDummy.AddObject(aProxy);
    OceanSurface.AddObject(aDummy);

    aProxy.SourceObject := cylinderBuoy; // insert a proxy
    aProxy.Position.Point := Point3D(0, 0, 0); // center on dummy
    aProxy.Height := cylinderBuoy.Height; // copy buoy size
    aProxy.Width := cylinderBuoy.Width;
    aProxy.Depth := cylinderBuoy.Depth;
    aProxy.Opacity := 1.0;
    aProxy.RotationAngle.X := 90;

    aDummy.Position.Point := Point3D(10, -5 + i, 0);
    // default pos to the side ( boat at 0,0 )

    fMarks[i] := aDummy; // save gate dummy
  end;
end;

procedure TFormSailboatDemo.Viewport3D1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  fMouseDragBeginPt := PointF(X, Y);
  fMouseDragging := true;
end;

procedure TFormSailboatDemo.Viewport3D1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Single);
var
  M, Delta: TPointF;
begin
  if fMouseDragging then
  begin
    M := PointF(X, Y); // mouse
    Delta := M - fMouseDragBeginPt;
    fMouseDragBeginPt := M;

    handlePanDelta(Delta);
  end;
end;

procedure TFormSailboatDemo.Viewport3D1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  M, Delta: TPointF;
begin
  if fMouseDragging then
  begin
    M := PointF(X, Y); // mouse
    Delta := M - fMouseDragBeginPt;
    fMouseDragBeginPt := M;

    handlePanDelta(Delta); // last delta, if any

    fMouseDragging := false;
  end;
end;

procedure TFormSailboatDemo.Viewport3D1Paint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
begin
  // inc(fFrameCount);
end;

// randomize wave systems
procedure TFormSailboatDemo.WaveSystem1RandomOrigines;
begin
  WaveSystem1.Origine := Point3D(2000 + RandomFloat(-1500, 2400),
    2000 + RandomFloat(-1200, 1300), 0);
  WaveSystem1.Origine2 := Point3D(450 + RandomFloat(-1320, 1340),
    350 + RandomFloat(-320, 350), 0);
  WaveSystem1.Origine3 := Point3D(-150 + RandomFloat(-330, 1330),
    -150 + RandomFloat(+350, 250), 0);
  WaveSystem1.Origine4 := Point3D(-150 + RandomFloat(+330, 330),
    -150 + RandomFloat(+1350, 250), 0);
  WaveSystem1.Origine5 := Point3D(+150 + RandomFloat(+330, 330),
    -150 + RandomFloat(+350, 1250), 0);

  // not only origines. Randomize sizes and speeds
  // small Longueurs resulted in boat pitching too much
  WaveSystem1.Longueur := RandomFloat(0.5, 4.6); // 1= long wave
  WaveSystem1.Longueur2 := RandomFloat(0.3, 2.9);
  // lesser waves that move w/ the boat
  WaveSystem1.Longueur3 := RandomFloat(0.5, 2.7);
  WaveSystem1.Longueur4 := RandomFloat(0.5, 1.7);
  WaveSystem1.Longueur5 := RandomFloat(0.5, 2.1);

  WaveSystem1.Amplitude := RandomFloat(0.5, 1.6); // main waves (larger)
  WaveSystem1.Amplitude2 := RandomFloat(0.5, 1.4);
  WaveSystem1.Amplitude3 := RandomFloat(0.4, 2.1);
  WaveSystem1.Amplitude4 := RandomFloat(0.3, 1.3);
  WaveSystem1.Amplitude5 := RandomFloat(0.3, 1.4);

  WaveSystem1.Vitesse := RandomFloat(1.1, 3.6);
  WaveSystem1.Vitesse2 := RandomFloat(1.1, 5.7);
  WaveSystem1.Vitesse3 := RandomFloat(1.3, 3.5);
  WaveSystem1.Vitesse4 := RandomFloat(1.1, 2.1);
  WaveSystem1.Vitesse5 := RandomFloat(1.3, 2.0);

  // WaveSystem2 uses the same Wave1 as WaveSystem1
  // copy wave1 of WaveSystem1
  // WaveSystem2 powers periferic SeaSurfaces

  WaveSystem2.Origine := WaveSystem1.Origine;
  WaveSystem2.Longueur := WaveSystem1.Longueur;
  WaveSystem2.Amplitude := WaveSystem1.Amplitude;
  WaveSystem2.Vitesse := WaveSystem1.Vitesse;
end;

procedure TFormSailboatDemo.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
var
  aScale: Double;
begin
  if not listboxControls.IsVisible then // only Zoom if control not visible.
  begin // if control visible, wheel scrolls the listbox
    ChangeZoom(WheelDelta);
    Handled := true; // we did
  end;
end;

procedure TFormSailboatDemo.ChangeZoom(WheelDelta: Integer);
begin
  tbAngleOfView.Value := tbAngleOfView.Value - WheelDelta / 100;
  // - --> mesma convençao do view do box2d
end;

procedure TFormSailboatDemo.DoSaveState;
begin // save form state
{$IFDEF OPYC}
  try
    StateBox.WriteStateToIni; // save state in iOS
  except
    // report error ?     // possibly unexisting app Documents folder ( installer should have created that ! )
  end;
{$ENDIF OPYC}
end;

procedure TFormSailboatDemo.btnClose3dviewClick(Sender: TObject);
begin
{$IFDEF OPYC}
  if Assigned(FormSailboatApp) then
    FormSailboatApp.Show;
  Hide;
{$ENDIF OPYC}
end;

procedure TFormSailboatDemo.btnRandomizeWaveSystem1Click(Sender: TObject);
begin
  WaveSystem1RandomOrigines; // randomize winds
  CrewRandomPositions; // move crew
end;

procedure TFormSailboatDemo.btnToggleControlsClick(Sender: TObject);
var
  bVisible: Boolean;
begin
  bVisible := not listboxControls.Visible; // toggle visibility
  listboxControls.Visible := bVisible;
  rectBlackListboxBackground.Visible := bVisible;
end;

procedure TFormSailboatDemo.cbCloudsSwitch(Sender: TObject);
begin
  GBEClouds1.Visible := cbClouds.IsChecked;
  if GBEClouds1.Visible then
    GBEClouds1.Position.Y := RandomFloat(-20, -10);
  // set cloud covert at random alltitude
end;

procedure TFormSailboatDemo.cbDesignCameraSwitch(Sender: TObject);
begin
  Self.Viewport3D1.UsingDesignCamera := cbDesignCamera.IsChecked;
end;

procedure TFormSailboatDemo.cbUseRampSwitch(Sender: TObject);
begin
  if cbUseRamp.IsChecked then
    heightmapTerrain.MaterialSource := materialTerrainRamp
  else
    heightmapTerrain.MaterialSource := materialTerrain;
  heightmapTerrain.UseRamp := cbUseRamp.IsChecked;
end;

procedure TFormSailboatDemo.comboSelObjChange(Sender: TObject);
var
  v: Single;
  aObj: TControl3d;
  P: TPoint3D;
begin
  aObj := getSelected3DObject;

  tbSelObjX.Value := aObj.Position.X;
  tbSelObjY.Value := aObj.Position.Y;
  tbSelObjZ.Value := aObj.Position.Z;
end;

procedure TFormSailboatDemo.comboWaveChange(Sender: TObject);
begin
  case getSelectedWave of
    0:
      begin
        tbVitesse.Value := WaveSystem1.Vitesse;
        tbWaveLenght.Value := WaveSystem1.Longueur;
        tbAmplitude.Value := WaveSystem1.Amplitude;

        SpinBox1.Value := WaveSystem1.Origine.X;
        SpinBox2.Value := WaveSystem1.Origine.Y;
        SpinBox3.Value := WaveSystem1.Origine.Z;
      end;
    1:
      begin
        tbVitesse.Value := WaveSystem1.Vitesse2;
        tbWaveLenght.Value := WaveSystem1.Longueur2;
        tbAmplitude.Value := WaveSystem1.Amplitude2;
        SpinBox1.Value := WaveSystem1.Origine2.X;
        SpinBox2.Value := WaveSystem1.Origine2.Y;
        SpinBox3.Value := WaveSystem1.Origine2.Z;
      end;
    2:
      begin
        tbVitesse.Value := WaveSystem1.Vitesse3;
        tbWaveLenght.Value := WaveSystem1.Longueur3;
        tbAmplitude.Value := WaveSystem1.Amplitude3;
        SpinBox1.Value := WaveSystem1.Origine3.X;
        SpinBox2.Value := WaveSystem1.Origine3.Y;
        SpinBox3.Value := WaveSystem1.Origine3.Z;
      end;
    3:
      begin
        tbVitesse.Value := WaveSystem1.Vitesse4;
        tbWaveLenght.Value := WaveSystem1.Longueur4;
        tbAmplitude.Value := WaveSystem1.Amplitude4;
        SpinBox1.Value := WaveSystem1.Origine4.X;
        SpinBox2.Value := WaveSystem1.Origine4.Y;
        SpinBox3.Value := WaveSystem1.Origine4.Z;
      end;

    4:
      begin
        tbVitesse.Value := WaveSystem1.Vitesse5;
        tbWaveLenght.Value := WaveSystem1.Longueur5;
        tbAmplitude.Value := WaveSystem1.Amplitude5;
        SpinBox1.Value := WaveSystem1.Origine5.X;
        SpinBox2.Value := WaveSystem1.Origine5.Y;
        SpinBox3.Value := WaveSystem1.Origine5.Z;
      end;
  end;
end;

procedure TFormSailboatDemo.SetSailShape(ixSail: Integer;
  aPtArray: TPointF_Array); // 0=main, 1=bow sail
var
  aSail: TgxSailSurface;
  n: Integer;
begin
  case ixSail of
    0:
      aSail := MainSail;
    1:
      aSail := JibSail;
    2:
      aSail := birutaSail;
    // TODO: Other bow  sails
  else
    aSail := nil;
  end;

  if Assigned(aSail) then
  begin
    n := Length(aPtArray) - 1;
    if (n > 0) then
    begin
      aSail.SubdivisionsWidth := n; // upd mesh width  (Spis are wilder )
      aSail.SetMeshWith2Dline(GXS.SailSurface.TPointF_Array(aPtArray));
    end;
  end;
end;

procedure TFormSailboatDemo.Begin3DChange;
begin
  Viewport3D1.BeginUpdate;
end;

procedure TFormSailboatDemo.End3DChange;
begin
  Viewport3D1.EndUpdate;
end;

Procedure TFormSailboatDemo.SetTerrainBitmap(bVisible: Boolean; aBMP: TBitmap);
begin
  if bVisible then
  begin
    ImageTerrain.Bitmap := aBMP; // use provided bmp to set terrain mesh

    try
      LoadTerrain; // from ImageTerrain
    except
      labCountTerrainBuilds.Text := 'error in terrain';
      exit; // ??
    end;

    labCountTerrainBuilds.Tag := labCountTerrainBuilds.Tag + 1;
    // Tag counts terrain builds
    labCountTerrainBuilds.Text := IntToStr(labCountTerrainBuilds.Tag);
  end;
  heightmapTerrain.Visible := bVisible;

end;

procedure TFormSailboatDemo.Set3DcharacterState(ix: Integer;
  const X, Y, alfa: Single);
var
  aDummy: TDummy;
  Z: Single;
begin
  aDummy := nil;
  case ix of
    charDolphin:
      aDummy := nil;
      // dummy3Dolphins;  // dont chg dolphin position. Always along the boat
    charWhiteBirds:
      aDummy := dummyWhiteBirds;
    charBrownBirds:
      aDummy := dummyBrownBirds;
    charBoiaCross:
      aDummy := dummyBoiaMan;
    charPelican:
      aDummy := dummyPelican;
    charPurpleBoat:
      aDummy := dummyPurpleBoat;
    charContainer:
      aDummy := dummyContainer;
    charRock:
      aDummy := dummyRock;
    charBarril:
      aDummy := dummyBarril;
    charWhale:
      aDummy := dummyWhale;
  end;

  if Assigned(aDummy) then
  begin
    Z := aDummy.Position.Z; // save z pos set by waves
    aDummy.Position.Point := Point3D(X, -Y, Z);
    aDummy.RotationAngle.Z := alfa + 90; // ??
    aDummy.TagFloat := Frac(Now) * 3600 * 24;
    // save time last moved in TagFloat ( a Single ) in seconds since 12:00AM
    aDummy.Visible := true;
  end;
end;

procedure TFormSailboatDemo.Set3dMarks(ix: Integer; const ax, ay: Single);
// ix 1 based
var
  az: Single;
begin
  if (ix < 1) or (ix > numMarks) then
    exit; // invalid index
  az := fMarks[ix].Position.Z; // keep z
  fMarks[ix].Position.Point := Point3D(ax, -ay, az);
  fMarks[ix].Visible := true;
end;

procedure TFormSailboatDemo.FloatAnimation1Process(Sender: TObject);
// 0.2 sec tick
var
  aAmpl, aPitch, aCap, aAng, aSpd, DHeel, aHeelAng, xb, zb, sz, dx, dy, v,
    Tsec: Single;
  D, P, Po, Pnew: TPoint3D;
  newBubble: TProxyObject;
  aControl: TControl3d;
  isOutside, bWasMovedRecently: Boolean;
  i, n: Integer;
  aPh, aAlt, aDeriv: Single;

const
  CINCOSEC = 5 / 3600 / 24;
  // after 5 seconds w/o being moved by simulation, move by sea

begin
  if not Visible then
    exit; // avoid animating if form not visible, to save CPU
  // keep animating would be better, but performance sucks on mobiles :(

  inc(fFrameCount);

  Begin3DChange;
  try
    // precalc stuff
    P := dummyBoatCap.Position.Point;
    // boat position on the virtual ocean surface = position ( boat is independent from sea)
    aCap := dummyBoatCap.RotationAngle.Y; // get boat cap

    aAng := aCap * Pi / 180; // cap to radians
    aSpd := tbBoatSpeed.Value;
    // get boat speed from trackbar ( em m/s  max = 15)
    D := Point3D(-sin(aAng), 0, -cos(aAng)) * aSpd / 100;
    // displacement in one sec, in m
    dx := -D.X * B2Dto3Dscale;
    // B2Dto3Dscale = scale factor between Box2D and 3D world
    dy := D.Z * B2Dto3Dscale;

    // u    := -(D.x/OceanSurface.SubdivisionsWidth /20);  //ad hoc
    // v    :=  (D.z/OceanSurface.SubdivisionsHeight/20);

    if cbMoveSea.IsChecked then // moving sea = moving boat
    begin
      OceanSurface.MoveTextureBy(dx, dy); // shifts virtual sea  TexCoordinates

      OceanSurfaceTop.MoveTextureBy(dx, dy); // all of them ?
      OceanSurfaceLeft.MoveTextureBy(dx, dy);
      OceanSurfaceBot.MoveTextureBy(dx, dy);
      OceanSurfaceRight.MoveTextureBy(dx, dy);
    end;

    // Position boat floating on the sea surface and pitching accordingly
    if OceanSurface.calcWaveAmplitudeAndPitch(P, aCap, { out: } aAmpl, aPitch)
    then
    begin
      // when the boat heels, lift it some. In fact the center of buoyancy lifts as the boat side goes under water..

      aHeelAng := dummyBoatHeel.RotationAngle.Z;
      if (aHeelAng > 180) then
        aHeelAng := 360 - aHeelAng; // put in the -90..90 range

      aHeelAng := Abs(aHeelAng * Pi / 180); // to rad abs
      DHeel := -0.20 * sin(aHeelAng); // numbers ad hoc

      dummyBoatCap.Position.Y := aAmpl * 0.95 + DHeel; // mk boat float
      fBoatAttitude.X := aPitch; // pitch boat
      dummyBoatPitch.RotationAngle.X := aPitch * 0.6;
      // 0.6 avoids too much pitching

      labBoatPitch.Text := Format('%5.1f', [aPitch]) + 'd'; // show pitch
    end;

    // OceanSurface.Children includes wake bubbles, characters, rocks, and even terrain
    // those shift in x,y in sync with the sea surface, and some float on the surface
    Tsec := Frac(Now) * 3600 * 24; // time in seconds since 12:00AM
    for i := 0 to OceanSurface.ChildrenCount - 1 do
    // move bubbles to wave amplitude, so they stay afloat
    begin
      aControl := TControl3d(OceanSurface.Children[i]);
      if not aControl.Visible then
        continue; // ignore hiden
      if cbMoveSea.IsChecked then
      // first move then in relation to the boat at 0,0
      begin
        // some controls are moved by the box2d simulation, so we don't mess w/ them here
        bWasMovedRecently := (aControl.TagFloat > 0) and
          (Tsec - aControl.TagFloat < 10.0);
        if (aControl is TDummy) and (aControl.TagFloat > 0) and
          (not bWasMovedRecently) and (aControl <> dummyRock) and
          (aControl <> dummyTerrain) then // never hide rock and terrain
        begin // hide animations that are not being moved
          aControl.Visible := false; // hide
          continue;
        end;

        // not moved child are moved with the shifting texture
        if not bWasMovedRecently then
        // if not moved, move children with the surface
        begin
          aControl.Position.X := aControl.Position.X - dx;
          // move it with the shifting surface
          aControl.Position.Y := aControl.Position.Y - dy;
        end;
      end;

      if (aControl = dummyRock) or (aControl = dummyTerrain) then
        continue; // rock and terrain dont float :)
      // other stuff float: containers, barrels, even flying birds
      // set object z with the wave amplitude ( floating objects )
      P := aControl.Position.Point;
      // - Point3d(0.50, 0, 0.50);    // set P in div units
      xb := P.X;
      zb := P.Y;

      // P  := Point3D(xb,0,zb)/OceanSurface.SubdivisionsHeight;    // to subd
      P := Point3D(xb, 0, zb);
      if OceanSurface.calcWaveAmplitudeAndPitch(P, aCap, aAmpl, aPitch) then
      begin
        aControl.Position.Z := aAmpl - 0.03; // mk bubble float
        // not using pitch
      end;
    end;

    if cbMoveSea.IsChecked then // move sea ( boat perspective )
    begin
      // emit bubbles !
      if (fFrameCount mod 7) = 0 then
      // every few ticks, add a bubble to the wake..
      begin
        newBubble := TProxyObject.Create(Self); // bubble is a proxy to a TDisk
        OceanSurface.AddObject(newBubble);
        // parent buoy to sea surface, so it moves w/ it
        newBubble.SourceObject := diskBubble; // sphereBuoy;
        newBubble.SetSize((Random(20) + 10) / 150, 0.01,
          (Random(20) + 10) / 150); // small flat whiote disk

        P := OceanSurface.Position.Point; // z makes the bubble float.
        newBubble.Position.Point := Point3D(-P.X, +P.Z, 0) +
          Point3D((Random(10) - 5) / 20, (Random(10) - 5) / 20, 0.2);
        // position at -P on the sea surface. some randoness too
        newBubble.Opacity := 0.1; // ?? doesnt work !?
        newBubble.RotationAngle.X := 90;

        if OceanSurface.ChildrenCount > 121 then
        // keep a maximum of 121 bubbles. If more , clear some old bubbles
        begin
          // change opacity of some
          n := Random(100);
          aControl := TControl3d(OceanSurface.Children[n]);
          if not(aControl is TDummy) then
            aControl.Opacity := 0.4;
          // aControl is TDummy --> TDummy is a permanent obj ( not a bubble )

          n := Random(50);
          aControl := TControl3d(OceanSurface.Children[n]);
          if not(aControl is TDummy) then
            aControl.Opacity := 0.2;

          // dispose a few. Randomic choice
          n := Random(100);
          aControl := TControl3d(OceanSurface.Children[n]);
          if not(aControl is TDummy) then
          // dont remove dummies. These are design time characters
          begin
            OceanSurface.RemoveObject(aControl);
            aControl.DisposeOf;
          end;

          n := Random(50);
          aControl := TControl3d(OceanSurface.Children[n]);
          if not(aControl is TDummy) then
          begin
            OceanSurface.RemoveObject(aControl);
            aControl.DisposeOf;
          end;

          n := Random(20); // dispose older
          aControl := TControl3d(OceanSurface.Children[n]);
          if not(aControl is TDummy) then
          begin
            OceanSurface.RemoveObject(aControl);
            aControl.DisposeOf;
          end;
        end;
      end;
    end;
    if (fFrameCount mod 100 = 0) and GBEClouds1.Visible then
      GBEClouds1.moveClouds;

    if cbShowDolphin.IsChecked then // Animate jumping Dolphin
    begin
      // animate dolphin
      const
        DolphinWaveAmplitude = 0.35; // in m some ad-hoc factors
      const
        DolphinWaveSpeed = 7 * Pi; // in rad/s
      aPh := DolphinWaveSpeed * fTimeDolphinAnim;
      // calc wave phase at the point
      aAlt := DolphinWaveAmplitude * sin(aPh); // sum sin() wave amplitude
      aDeriv := cos(aPh) * 180 / Pi;
      // dolphin pitch in deg        derivative of sin() is cos()

      modelDolphin.Position.Y := aAlt + 0.3; // set dolphin altitude
      modelDolphin.RotationAngle.Z := 180 + aDeriv / 2; // set dolphin pitch

      // dummyDolphin is not parented to OcceanSurface, so it doesnt float by default
      // so make dummyDolphin float.  more or less
      // dummy3Dolphins is parented to dummyBoatCap, w/ coordinates x,y
      P := modelDolphin.LocalToAbsolute3D(Point3D(0, 0, 0));
      // get dolphin abs coordinates
      P := Point3D(P.X, 0, P.Y); // OceanSurface uses x,z as x,y
      if OceanSurface.calcWaveAmplitudeAndPitch(P, aCap, aAmpl, aPitch) then
      // probe wave amplitude
        dummy3Dolphins.Position.Y := aAmpl; // move dolphin dummy up and down

      // animate whale
      if dummyWhale.Visible then
      begin
        const
          WhaleWaveAmplitude = 1.2; // in m some ad-hoc factors
        const
          WhaleWaveSpeed = 3 * Pi; // in rad/s

        aPh := WhaleWaveSpeed * fTimeDolphinAnim;
        // calc wave phase at the point
        aAlt := WhaleWaveAmplitude * sin(aPh); // sum sin() wave amplitude
        aDeriv := cos(aPh) * 180 / Pi;
        // dolphin pitch in deg        derivative of sin() is cos()

        modelWhale.Position.Z := aAlt + 0.1; // set altitude
        // modelWhale.RotationAngle.z  := 180+aDeriv/2;    // set pitch
        modelWhale.RotationAngle.X := modelWhale.RotationAngle.X + 3;
        // rotate whale on its length axis
        P := modelDolphin.LocalToAbsolute3D(Point3D(0, 0, 0));
        // get dolphin abs coordinates

        P := Point3D(P.X, 0, P.Y); // OceanSurface uses x,z as x,y
        if OceanSurface.calcWaveAmplitudeAndPitch(P, aCap, aAmpl, aPitch) then
        // probe wave amplitude
          modelWhale.Position.Y := aAmpl; // move dolphin dummy up and down
      end;

      // adv dolphin animation time.
      fTimeDolphinAnim := fTimeDolphinAnim + 0.01; // in sec
    end;

  finally
    End3DChange;
  end;

  // Viewport3D1.Repaint;
end;

procedure TFormSailboatDemo.SpinBox1ChangeClick(Sender: TObject);
var
  P: TPoint3D;
begin
  P := Point3D(SpinBox1.Value, SpinBox2.Value, SpinBox3.Value);
  case getSelectedWave of
    0:
      WaveSystem1.Origine := P;
    1:
      WaveSystem1.Origine2 := P;
    2:
      WaveSystem1.Origine3 := P;
    3:
      WaveSystem1.Origine4 := P;
    4:
      WaveSystem1.Origine5 := P;

  end;
end;

procedure TFormSailboatDemo.cbShowDolphinSwitch(Sender: TObject);
begin
  dummy3Dolphins.Visible := cbShowDolphin.IsChecked;
  dummyWhale.Visible := cbShowDolphin.IsChecked;
end;

procedure TFormSailboatDemo.cbShowLinesSwitch(Sender: TObject);
begin
  OceanSurface.ShowLines := cbShowLines.IsChecked;
end;

procedure TFormSailboatDemo.cbShowWindArrowSwitch(Sender: TObject);
begin
  dummyWindArrow.Visible := cbShowWindArrow.IsChecked;
end;

procedure TFormSailboatDemo.tbAmplitudeTracking(Sender: TObject);
begin
  case getSelectedWave of
    0:
      WaveSystem1.Amplitude := tbAmplitude.Value;
    1:
      WaveSystem1.Amplitude2 := tbAmplitude.Value;
    2:
      WaveSystem1.Amplitude3 := tbAmplitude.Value;
    3:
      WaveSystem1.Amplitude4 := tbAmplitude.Value;
    4:
      WaveSystem1.Amplitude5 := tbAmplitude.Value;
  end;

  labAmplitude.Text := Format('%6.2f', [tbAmplitude.Value]);
end;

procedure TFormSailboatDemo.tbAngleOfViewTracking(Sender: TObject);
begin
  Camera1.AngleOfView := tbAngleOfView.Value;
  labCameraViewAngle.Text := Format('%5.0f', [Camera1.AngleOfView]) + 'o';
end;

procedure TFormSailboatDemo.tbBoatSpeedTracking(Sender: TObject);
begin
  labBoatSpeed.Text := Format('%5.0f', [tbBoatSpeed.Value]);
end;

procedure TFormSailboatDemo.tbWaveLenghtTracking(Sender: TObject);
begin
  case getSelectedWave of
    0:
      WaveSystem1.Longueur := tbWaveLenght.Value;
    1:
      WaveSystem1.Longueur2 := tbWaveLenght.Value;
    2:
      WaveSystem1.Longueur3 := tbWaveLenght.Value;
    3:
      WaveSystem1.Longueur4 := tbWaveLenght.Value;
    4:
      WaveSystem1.Longueur5 := tbWaveLenght.Value;
  end;

  labLongueur.Text := Format('%5.1f', [tbWaveLenght.Value]);
end;

procedure TFormSailboatDemo.timerOneSecondTickTimer(Sender: TObject);
var
  nFrames: Integer;
  DT: TDatetime;
  T: TDatetime;

begin
  T := Now;
  // upd FPS
  nFrames := (fFrameCount - fLastFPScount);
  DT := (T - fLastFPS) * 3600 * 24; // DT in secs
  if (DT > 0) and (nFrames > 0) then
    fFPS := nFrames / DT // upd fps
  else
    fFPS := 0;
  fLastFPS := T; // save last state
  fLastFPScount := fFrameCount;

  labFPS.Text := 'fps: ' + Trim(Format('%4.0f', [fFPS]));
  // upc fps display every sec
end;

procedure TFormSailboatDemo.tbVitesseTracking(Sender: TObject);
begin
  case getSelectedWave of
    0:
      WaveSystem1.Vitesse := tbVitesse.Value;
    1:
      WaveSystem1.Vitesse2 := tbVitesse.Value;
    2:
      WaveSystem1.Vitesse3 := tbVitesse.Value;
    3:
      WaveSystem1.Vitesse4 := tbVitesse.Value;
    4:
      WaveSystem1.Vitesse5 := tbVitesse.Value;
  end;
  labVitesse.Text := Format('%5.1f', [tbVitesse.Value]);
end;

procedure TFormSailboatDemo.tbOpaciteTracking(Sender: TObject);
begin
  OceanSurface.Opacity := tbOpacite.Value;
  labOpacite.Text := Format('%5.2f', [tbOpacite.Value]);
end;

procedure TFormSailboatDemo.tbCameraAzTracking(Sender: TObject);
begin
  dummyCameraTarget.RotationAngle.Y := tbCameraAz.Value;
  labCameraAz.Text := Format('%5.0f', [tbCameraAz.Value]);
end;

// exponential scale trackbar helper.   Used a 100% = e^5  ( or exp(5) )
// change between trackbar value 1..100 range to exponenmtial elevatio between 0.17 and 1000
function percValueToExponential(const aValue, VMin, VMax: Single): Single;
// value in 1..100 range
const
  exp5 = 148.41; // exp(5) corresponds to Value=100 --> VMax
begin
  Result := VMin + Exp(aValue / 20) / exp5 * (VMax - VMin) - 3;
end;

procedure TFormSailboatDemo.tbCameraElevTracking(Sender: TObject);
var
  aElev, aValue: Single;
begin
  aValue := tbCameraElev.Value; // 1..100
  aElev := percValueToExponential(aValue, { VMin: } -5, { VMax: } 1000);
  Camera1.Position.Y := -aElev;
  labCameraElev.Text := Format('%5.0f', [aElev]);
end;

procedure TFormSailboatDemo.tbCapTracking(Sender: TObject);
begin
  dummyBoatCap.RotationAngle.Y := tbCap.Value; // cap = boat course
  labCap.Text := Format('%5.0f', [tbCap.Value]);
end;

procedure TFormSailboatDemo.tbHeelTracking(Sender: TObject);
begin
  dummyBoatHeel.RotationAngle.Z := tbHeel.Value; // boat heel
  LabHeel.Text := Format('%5.1f', [tbHeel.Value]);
end;

procedure TFormSailboatDemo.tbJibRotTracking(Sender: TObject);
begin
  dummyJib.RotationAngle.Y := tbJibRot.Value;
  // JibSail.CamberRight := (tbJibRot.Value>0);
  labJibRot.Text := Format('%5.1f', [tbJibRot.Value]);
end;

procedure TFormSailboatDemo.tbMainRotTracking(Sender: TObject);
begin
{$IFDEF OPYC}
  // for OPYC, main rotation is controlled by simu
{$ELSE}
  dummyMain.RotationAngle.Y := tbMainRot.Value;
  MainSail.CamberRight := (tbMainRot.Value < 0);
  labMainRot.Text := Format('%5.1f', [tbMainRot.Value]);
{$ENDIF OPYC}
end;

function TFormSailboatDemo.getSelectedWave: Integer; // 0, 1 or 2
begin
  if comboWave.ItemIndex = -1 then
    Result := 0
  else
    Result := comboWave.ItemIndex;
end;

// demo automation ( used by OPYC)
procedure TFormSailboatDemo.SetBoatState(const aCap, aHeel, aSpeed, aBoomAngle,
  aRudderAngle, aWindDir, aWindSpeed: Single);
var
  aSc: Single;
begin
  // dummyBoatCap.RotationAngle.y  := aCap;
  // dummyBoatHeel.RotationAngle.z := aHeel;

  tbCap.Value := aCap; // this calls trackbar events that set boat vars
  tbHeel.Value := aHeel;
  tbBoatSpeed.Value := aSpeed;

  dummyBoom.RotationAngle.Y := aBoomAngle;
  dummyRudder.RotationAngle.Y := aRudderAngle;

  if dummyWindArrow.Visible then
  begin
    dummyWindArrow.RotationAngle.Y := 90 + aWindDir; // + Random(2)-1;
    aSc := System.Math.Max(aWindSpeed / 12, 0.3);
    dummyWindArrow.Scale.Point := Point3D(aSc, aSc, aSc);
    // scale arrow according to WindSpeed
  end;
end;

procedure TFormSailboatDemo.setBowSail(const Value: Integer);
var
  ax, ay, az: Single;
  bVisible: Boolean;
begin
  if (fBowSail <> Value) then // changed sail
  begin
    fBowSail := Value;

    bVisible := true;
    case fBowSail of
      0:
        begin
          JibSail.MaterialSource := Self.texJibSail;
          az := -0.62;
          // sail position numbers found by adjusting obj pos at runtime
          ay := -1.91;
        end;
      1:
        begin
          JibSail.MaterialSource := Self.texCodeZero; // genoa = code zero
          az := -0.52;
          ay := -1.86;
        end;
      2:
        begin
          JibSail.MaterialSource := Self.texSpinaker;
          az := -0.32;
          ay := -1.91;
        end;
      3:
        begin // 3=main sail only
          bVisible := false;
          // dont care about pos
        end;
    else
    end;

    JibSail.Visible := bVisible;
    if bVisible then
    begin
      ax := JibSail.Position.X; // keep x
      JibSail.Position.Point := Point3D(ax, ay, az);
    end;
  end;
end;

procedure TFormSailboatDemo.lbTexCoordXTracking(Sender: TObject);
// X and Y actually
begin
  // P := '';
  // TC:= '';
  // OceanSurface.GetPointsTexCoordinates(P, TC);

  // OceanSurface.MoveTextureBy(u,v);
  // labDataTexCoordinates.Text := P;
  // labDataPoints.Text         := TC;

  // OceanSurface.SetTextureCoordinates(u,v);
end;

Procedure TFormSailboatDemo.LoadTerrain;
var
  stream: TMemoryStream;
begin
  stream := TMemoryStream.Create;
  ImageTerrain.Bitmap.SaveToStream(stream);
  heightmapTerrain.loadHeightmapFromStream(stream);
  stream.Free;

  dummyTerrain.Position.Point := Point3D(0, 0, 0);
  // loaded new terrain in the center. Bring terrain back
end;

procedure TFormSailboatDemo.InitClouds;
begin
  GBEClouds1.addTextureCloud(materialCloud1);
  GBEClouds1.addTextureCloud(materialCloud2);
  GBEClouds1.addTextureCloud(materialCloud3);
  GBEClouds1.NbClouds := 15;
  GBEClouds1.WindSpeed := 0.01;
  GBEClouds1.Limits := 100;
  GBEClouds1.ActiveWind := true;
  // FloatAnimation1.Start;
end;

function TFormSailboatDemo.getSelected3DObject: TControl3d;
begin
  case comboSelObj.ItemIndex of
    0:
      Result := modelBoat; // Hull
    1:
      Result := MainSail; // Main
    2:
      Result := JibSail; // Jib
    3:
      Result := dummyMain; // dummyMain
    4:
      Result := dummyJib; // dummyJib
    5:
      Result := cylinderBoom; // Boom
    6:
      Result := dummyBoom; // dummyBoom
    7:
      Result := dummyBoatCap; // dummyBoat
    8:
      Result := cubeJibStay; // cubeJibStay
    9:
      Result := heightmapTerrain;
    10:
      Result := GBEClouds1;
    11:
      Result := dummyCrew;
    12:
      Result := dummyRudder;
  else
    Result := modelBoat; // ??
  end;
end;

procedure TFormSailboatDemo.tbSelObjXTracking(Sender: TObject);
var
  v: Single;
  aObj: TControl3d;
begin
  aObj := getSelected3DObject;

  v := tbSelObjX.Value;
  labSelObjX.Text := Format('%5.2f', [v]);

  aObj.Position.X := v;
end;

procedure TFormSailboatDemo.tbSelObjYTracking(Sender: TObject);
var
  v: Single;
  aObj: TControl3d;
begin
  aObj := getSelected3DObject;

  v := tbSelObjY.Value;
  labSelObjY.Text := Format('%5.2f', [v]);

  aObj.Position.Y := v;
end;

procedure TFormSailboatDemo.tbSelObjZTracking(Sender: TObject);
var
  v: Single;
  aObj: TControl3d;
begin
  aObj := getSelected3DObject;

  v := tbSelObjZ.Value;
  labSelObjZ.Text := Format('%5.2f', [v]);
  aObj.Position.Z := v;
end;

procedure TFormSailboatDemo.ScaleSailsForDemo;
// ad hoc object positioning for sailboat demo
begin
  // scale jib and main
  JibSail.Width := JibSail.Width / 1.8;
  JibSail.Depth := JibSail.Depth / 1.8;

  MainSail.Width := MainSail.Width / 1.8;
  MainSail.Depth := MainSail.Depth / 1.8;

  MainSail.Position.Point := Point3D(0.17, -2.4, -0.49);
  // ad hoc positioning obtained from the app itself
  JibSail.Position.Point := Point3D(0.29, -1.95, -0.24);

  tbHeel.Value := 12; // some heel and some sail sheet
  tbJibRot.Value := -10;
  tbMainRot.Value := -8;
  tbBoatSpeed.Value := 5;
end;

end.
