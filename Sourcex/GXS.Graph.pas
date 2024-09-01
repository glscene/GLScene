//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.Graph;

(* Graph plotting objects for Scene *)

interface

{$I GXS.Scene.inc}

uses
  System.Classes,
  System.SysUtils,

  GXS.OpenGL,
  GXS.XOpenGL,
  GXS.Scene,
  GXS.Context,
  GXS.VectorGeometry,
  GXS.Material,
  GXS.Objects,
  GXS.VectorLists,
  GXS.Color,
  GXS.BaseClasses,
  GXS.RenderContextInfo,
  GXS.State,
  GXS.VectorTypes;

type

  TgxSamplingScale = class(TgxUpdateAbleObject)
  private
    FMin: Single;
    FMax: Single;
    FOrigin: Single;
    FStep: Single;
  protected
    procedure SetMin(const val: Single);
    procedure SetMax(const val: Single);
    procedure SetOrigin(const val: Single);
    procedure SetStep(const val: Single);
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    { Returns the Base value for Step browsing.
      ie. the lowest value (superior to Min) that verifies
      Frac((Origin-StepBase)/Step)=0.0, this value may be superior to Max. }
    function StepBase: Single;
    { Maximum number of steps that can occur between Min and Max. }
    function MaxStepCount: Integer;
    function IsValid: Boolean;
    procedure SetBaseStepMaxToVars(var Base, Step, Max: Single;
      SamplingEnabled: Boolean = True);
  published
    property Min: Single read FMin write SetMin;
    property Max: Single read FMax write SetMax;
    property Origin: Single read FOrigin write SetOrigin;
    property Step: Single read FStep write SetStep;
  end;

  TgxHeightFieldGetHeightEvent = procedure(const x, y: Single; var z: Single;
    var Color: TgxColorVector; var TexPoint: TTexPoint) of object;
  TgxHeightFieldGetHeight2Event = procedure(Sender: TObject; const x, y: Single;
    var z: Single; var Color: TgxColorVector; var TexPoint: TTexPoint) of object;

  TgxHeightFieldOption = (hfoTextureCoordinates, hfoTwoSided);
  TgxHeightFieldOptions = set of TgxHeightFieldOption;

  TgxHeightFieldColorMode = (hfcmNone, hfcmEmission, hfcmAmbient, hfcmDiffuse,
    hfcmAmbientAndDiffuse);

  { Renders a sampled height-field.
    HeightFields are used to materialize z=f(x, y) surfaces, you can use it to
    render anything from math formulas to statistics. Most important properties
    of an height field are its sampling scales (X & Y) that determine the extents
    and the resolution of the base grid.
    The component will then invoke it OnGetHeight event to retrieve Z values for
    all of the grid points (values are retrieved only once for each point). Each
    point may have an additionnal color and texture coordinate. }
  TgxHeightField = class(TgxSceneObject)
  private
    FOnGetHeight: TgxHeightFieldGetHeightEvent;
    FOnGetHeight2: TgxHeightFieldGetHeight2Event;
    FXSamplingScale: TgxSamplingScale;
    FYSamplingScale: TgxSamplingScale;
    FOptions: TgxHeightFieldOptions;
    FTriangleCount: Integer;
    FColorMode: TgxHeightFieldColorMode;
  protected
    procedure SetXSamplingScale(const val: TgxSamplingScale);
    procedure SetYSamplingScale(const val: TgxSamplingScale);
    procedure SetOptions(const val: TgxHeightFieldOptions);
    procedure SetOnGetHeight(const val: TgxHeightFieldGetHeightEvent);
    procedure SetOnGetHeight2(const val: TgxHeightFieldGetHeight2Event);
    procedure SetColorMode(const val: TgxHeightFieldColorMode);
    procedure DefaultHeightField(const x, y: Single; var z: Single;
      var Color: TgxColorVector; var TexPoint: TTexPoint);
    procedure Height2Field(const x, y: Single; var z: Single;
      var Color: TgxColorVector; var texPoint: TTexPoint);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var rci: TgxRenderContextInfo); override;
    procedure NotifyChange(Sender: TObject); override;
    property TriangleCount: Integer read FTriangleCount;
  published
    property XSamplingScale: TgxSamplingScale read FXSamplingScale
      write SetXSamplingScale;
    property YSamplingScale: TgxSamplingScale read FYSamplingScale
      write SetYSamplingScale;
    { Define if and how per vertex color is used. }
    property ColorMode: TgxHeightFieldColorMode read FColorMode write SetColorMode
      default hfcmNone;
    property Options: TgxHeightFieldOptions read FOptions write SetOptions
      default [hfoTwoSided];
    { Primary event to return heights. }
    property OnGetHeight: TgxHeightFieldGetHeightEvent read FOnGetHeight
      write SetOnGetHeight;
    { Alternate this event to return heights.
      This events passes an extra "Sender" parameter, it will be invoked
      only if OnGetHeight isn't defined. }
    property OnGetHeight2: TgxHeightFieldGetHeight2Event read FOnGetHeight2
      write SetOnGetHeight2;
  end;

  TXYZGridPart = (gpX, gpY, gpZ);
  TXYZGridParts = set of TXYZGridPart;

  { Rendering Style for grid lines.
    - glsLine : a single line is used for each grid line (from Min to Max),
    this provides the fastest rendering
    - glsSegments : line segments are used between each node of the grid,
    this enhances perspective and quality, at the expense of computing
    power. }
  TXYZGridLinesStyle = (strLine, glsSegments);

  { An XYZ Grid object.
    Renders an XYZ grid using lines. }
  TgxXYZGrid = class(TgxLineBase)
  private
    FXSamplingScale: TgxSamplingScale;
    FYSamplingScale: TgxSamplingScale;
    FZSamplingScale: TgxSamplingScale;
    FParts: TXYZGridParts;
    FLinesStyle: TXYZGridLinesStyle;
  protected
    procedure SetXSamplingScale(const val: TgxSamplingScale);
    procedure SetYSamplingScale(const val: TgxSamplingScale);
    procedure SetZSamplingScale(const val: TgxSamplingScale);
    procedure SetParts(const val: TXYZGridParts);
    procedure SetLinesStyle(const val: TXYZGridLinesStyle);
    procedure SetLinesSmoothing(const val: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var rci: TgxRenderContextInfo); override;
    procedure NotifyChange(Sender: TObject); override;
  published
    property XSamplingScale: TgxSamplingScale read FXSamplingScale
      write SetXSamplingScale;
    property YSamplingScale: TgxSamplingScale read FYSamplingScale
      write SetYSamplingScale;
    property ZSamplingScale: TgxSamplingScale read FZSamplingScale
      write SetZSamplingScale;
    property Parts: TXYZGridParts read FParts write SetParts default [gpX, gpY];
    property LinesStyle: TXYZGridLinesStyle read FLinesStyle write SetLinesStyle
      default glsSegments;
    { Adjusts lines smoothing (or antialiasing).
      Obsolete, now maps to Antialiased property. }
    property LinesSmoothing: Boolean write SetLinesSmoothing stored False;
  end;

//=====================================================================
implementation
//=====================================================================

// ------------------
// ------------------ TgxSamplingScale ------------------
// ------------------

constructor TgxSamplingScale.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FStep := 0.1;
end;

destructor TgxSamplingScale.Destroy;
begin
  inherited Destroy;
end;

procedure TgxSamplingScale.Assign(Source: TPersistent);
begin
  if Source is TgxSamplingScale then
  begin
    FMin := TgxSamplingScale(Source).FMin;
    FMax := TgxSamplingScale(Source).FMax;
    FOrigin := TgxSamplingScale(Source).FOrigin;
    FStep := TgxSamplingScale(Source).FStep;
    NotifyChange(Self);
  end
  else
    inherited Assign(Source);
end;

procedure TgxSamplingScale.SetMin(const val: Single);
begin
  FMin := val;
  if FMax < FMin then
    FMax := FMin;
  NotifyChange(Self);
end;

procedure TgxSamplingScale.SetMax(const val: Single);
begin
  FMax := val;
  if FMin > FMax then
    FMin := FMax;
  NotifyChange(Self);
end;

procedure TgxSamplingScale.SetOrigin(const val: Single);
begin
  FOrigin := val;
  NotifyChange(Self);
end;

procedure TgxSamplingScale.SetStep(const val: Single);
begin
  if val > 0 then
    FStep := val
  else
    FStep := 1;
  NotifyChange(Self);
end;

function TgxSamplingScale.StepBase: Single;
begin
  if FOrigin <> FMin then
  begin
    Result := (FOrigin - FMin) / FStep;
    if Result >= 0 then
      Result := Trunc(Result)
    else
      Result := Trunc(Result) - 1;
    Result := FOrigin - FStep * Result;
  end
  else
    Result := FMin;
end;

function TgxSamplingScale.MaxStepCount: Integer;
begin
  Result := Round(0.5 + (Max - Min) / Step);
end;

function TgxSamplingScale.IsValid: Boolean;
begin
  Result := (Max <> Min);
end;

// SetBaseStepMaxToVars
//

procedure TgxSamplingScale.SetBaseStepMaxToVars(var Base, Step, Max: Single;
  samplingEnabled: Boolean = True);
begin
  Step := FStep;
  if samplingEnabled then
  begin
    Base := StepBase;
    Max := FMax + ((FMax - Base) / Step) * 1E-6; // add precision loss epsilon
  end
  else
  begin
    Base := FOrigin;
    Max := Base;
  end;
end;

// ------------------
// ------------------ TgxHeightField ------------------
// ------------------

constructor TgxHeightField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FXSamplingScale := TgxSamplingScale.Create(Self);
  FYSamplingScale := TgxSamplingScale.Create(Self);
  FOptions := [hfoTwoSided];
end;

// Destroy
//

destructor TgxHeightField.Destroy;
begin
  FXSamplingScale.Free;
  FYSamplingScale.Free;
  inherited Destroy;
end;

// Assign
//

procedure TgxHeightField.Assign(Source: TPersistent);
begin
  if Source is TgxHeightField then
  begin
    XSamplingScale := TgxHeightField(Source).XSamplingScale;
    YSamplingScale := TgxHeightField(Source).YSamplingScale;
    FOnGetHeight := TgxHeightField(Source).FOnGetHeight;
    FOptions := TgxHeightField(Source).FOptions;
    FColorMode := TgxHeightField(Source).FColorMode;
  end;
  inherited Assign(Source);
end;

// NotifyChange
//

procedure TgxHeightField.NotifyChange(Sender: TObject);
begin
  if Sender is TgxSamplingScale then
    StructureChanged;
  inherited NotifyChange(Sender);
end;

// BuildList
//

procedure TgxHeightField.BuildList(var rci: TgxRenderContextInfo);
type
  TRowData = packed record
    Color: TgxColorVector;
    Z: Single;
    TexPoint: TTexPoint;
    Normal: TAffineVector;
  end;

  TRowDataArray = array [0 .. Maxint shr 6] of TRowData;
  PRowData = ^TRowDataArray;
const
  cHFCMtoEnum: array [hfcmEmission .. hfcmAmbientAndDiffuse] of GLEnum =
    (GL_EMISSION, GL_AMBIENT, GL_DIFFUSE, GL_AMBIENT_AND_DIFFUSE);

var
  nx, m, k: Integer;
  x, y, x1, y1, y2, xStep, yStep, xBase, dx, dy: Single;
  invXStep, invYStep: Single;
  row: packed array [0 .. 2] of PRowData;
  rowTop, rowMid, rowBottom: PRowData;
  func: TgxHeightFieldGetHeightEvent;

  procedure IssuePoint(var x, y: Single; const pt: TRowData);
  begin
    with pt do
    begin
      glNormal3fv(@normal);
      if ColorMode <> hfcmNone then
        glColor4fv(@color);
      if hfoTextureCoordinates in Options then
        glTexCoord2fv(@texPoint);
      glVertex4f(x, y, z, 1);
    end;
  end;

  procedure RenderRow(pHighRow, pLowRow: PRowData);
  var
    k: Integer;
  begin
    glBegin(GL_TRIANGLE_STRIP);
    x := xBase;
    IssuePoint(x, y1, pLowRow^[0]);
    for k := 0 to m - 2 do
    begin
      x1 := x + xStep;
      IssuePoint(x, y2, pHighRow^[k]);
      IssuePoint(x1, y1, pLowRow^[k + 1]);
      x := x1;
    end;
    IssuePoint(x, y2, pHighRow^[m - 1]);
    glEnd;
  end;

begin
  if not(XSamplingScale.IsValid and YSamplingScale.IsValid) then
    Exit;
  if Assigned(FOnGetHeight) and (not(csDesigning in ComponentState)) then
    func := FOnGetHeight
  else if Assigned(FOnGetHeight2) and (not(csDesigning in ComponentState)) then
    func := Height2Field
  else
    func := DefaultHeightField;
  // allocate row cache
  nx := (XSamplingScale.MaxStepCount + 1) * SizeOf(TRowData);
  for k := 0 to 2 do
  begin
    GetMem(row[k], nx);
    FillChar(row[k][0], nx, 0);
  end;
  try
    // precompute grid values
    xBase := XSamplingScale.StepBase;
    xStep := XSamplingScale.Step;
    invXStep := 1 / xStep;
    yStep := YSamplingScale.Step;
    invYStep := 1 / yStep;
    // get through the grid
    if (hfoTwoSided in Options) or (ColorMode <> hfcmNone) then
    begin
      // if we're not two-sided, we doesn't have to enable face-culling, it's
      // controled at the sceneviewer level
      if hfoTwoSided in Options then
      begin
        rci.gxStates.Disable(stCullFace);
        rci.gxStates.PolygonMode := Material.PolygonMode;
      end;
      if ColorMode <> hfcmNone then
      begin
        rci.gxStates.Enable(stColorMaterial);
        glColorMaterial(GL_FRONT_AND_BACK, cHFCMtoEnum[ColorMode]);
        rci.gxStates.SetMaterialColors(cmFront, clrBlack, clrGray20,
          clrGray80, clrBlack, 0);
        rci.gxStates.SetMaterialColors(cmBack, clrBlack, clrGray20, clrGray80,
          clrBlack, 0);
      end;
    end;
    rowBottom := nil;
    rowMid := nil;
    nx := 0;
    y := YSamplingScale.StepBase;
    y1 := y;
    y2 := y;
    while y <= YSamplingScale.Max do
    begin
      rowTop := rowMid;
      rowMid := rowBottom;
      rowBottom := row[nx mod 3];
      x := xBase;
      m := 0;
      while x <= XSamplingScale.Max do
      begin
        with rowBottom^[m] do
        begin
          with texPoint do
          begin
            S := x;
            T := y;
          end;
          func(x, y, z, color, texPoint);
        end;
        Inc(m);
        x := x + xStep;
      end;
      if Assigned(rowMid) then
      begin
        for k := 0 to m - 1 do
        begin
          if k > 0 then
            dx := (rowMid^[k - 1].z - rowMid^[k].z) * invXStep
          else
            dx := 0;
          if k < m - 1 then
            dx := dx + (rowMid^[k].z - rowMid^[k + 1].z) * invXStep;
          if Assigned(rowTop) then
            dy := (rowTop^[k].z - rowMid^[k].z) * invYStep
          else
            dy := 0;
          if Assigned(rowBottom) then
            dy := dy + (rowMid^[k].z - rowBottom^[k].z) * invYStep;
          rowMid^[k].normal := VectorNormalize(AffineVectorMake(dx, dy, 1));
        end;
      end;
      if nx > 1 then
      begin
        RenderRow(rowTop, rowMid);
      end;
      Inc(nx);
      y2 := y1;
      y1 := y;
      y := y + yStep;
    end;
    for k := 0 to m - 1 do
    begin
      if k > 0 then
        dx := (rowBottom^[k - 1].z - rowBottom^[k].z) * invXStep
      else
        dx := 0;
      if k < m - 1 then
        dx := dx + (rowBottom^[k].z - rowBottom^[k + 1].z) * invXStep;
      if Assigned(rowMid) then
        dy := (rowMid^[k].z - rowBottom^[k].z) * invYStep
      else
        dy := 0;
      rowBottom^[k].normal := VectorNormalize(AffineVectorMake(dx, dy, 1));
    end;
    if Assigned(rowMid) and Assigned(rowBottom) then
      RenderRow(rowMid, rowBottom);
    FTriangleCount := 2 * (nx - 1) * (m - 1);
  finally
    FreeMem(row[0]);
    FreeMem(row[1]);
    FreeMem(row[2]);
  end;
end;

// SetXSamplingScale
//

procedure TgxHeightField.SetXSamplingScale(const val: TgxSamplingScale);
begin
  FXSamplingScale.Assign(val);
end;

// SetYSamplingScale
//

procedure TgxHeightField.SetYSamplingScale(const val: TgxSamplingScale);
begin
  FYSamplingScale.Assign(val);
end;

// SetOptions
//

procedure TgxHeightField.SetOptions(const val: TgxHeightFieldOptions);
begin
  if FOptions <> val then
  begin
    FOptions := val;
    StructureChanged;
  end;
end;

// SetOnGetHeight
//

procedure TgxHeightField.SetOnGetHeight(const val: TgxHeightFieldGetHeightEvent);
begin
  FOnGetHeight := val;
  StructureChanged;
end;

// SetOnGetHeight2
//

procedure TgxHeightField.SetOnGetHeight2(const val
  : TgxHeightFieldGetHeight2Event);
begin
  FOnGetHeight2 := val;
  StructureChanged;
end;

// SetColorMode
//

procedure TgxHeightField.SetColorMode(const val: TgxHeightFieldColorMode);
begin
  if val <> FColorMode then
  begin
    FColorMode := val;
    StructureChanged;
  end;
end;

// DefaultHeightField
//

procedure TgxHeightField.DefaultHeightField(const x, y: Single; var z: Single;
  var color: TgxColorVector; var texPoint: TTexPoint);
begin
  z := VectorNorm(x, y);
  z := cos(z * 12) / (2 * (z * 6.28 + 1));
  color := clrGray80;
end;

// Height2Field
//

procedure TgxHeightField.Height2Field(const x, y: Single; var z: Single;
  var color: TgxColorVector; var texPoint: TTexPoint);
begin
  FOnGetHeight2(Self, x, y, z, color, texPoint);
end;

// ------------------
// ------------------ TgxXYZGrid ------------------
// ------------------

// Create
//

constructor TgxXYZGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FXSamplingScale := TgxSamplingScale.Create(Self);
  FYSamplingScale := TgxSamplingScale.Create(Self);
  FZSamplingScale := TgxSamplingScale.Create(Self);
  FParts := [gpX, gpY];
  FLinesStyle := glsSegments;
end;

// Destroy
//

destructor TgxXYZGrid.Destroy;
begin
  FXSamplingScale.Free;
  FYSamplingScale.Free;
  FZSamplingScale.Free;
  inherited Destroy;
end;

// Assign
//

procedure TgxXYZGrid.Assign(Source: TPersistent);
begin
  if Source is TgxXYZGrid then
  begin
    XSamplingScale := TgxXYZGrid(Source).XSamplingScale;
    YSamplingScale := TgxXYZGrid(Source).YSamplingScale;
    ZSamplingScale := TgxXYZGrid(Source).ZSamplingScale;
    FParts := TgxXYZGrid(Source).FParts;
    FLinesStyle := TgxXYZGrid(Source).FLinesStyle;
  end;
  inherited Assign(Source);
end;

// SetXSamplingScale
//

procedure TgxXYZGrid.SetXSamplingScale(const val: TgxSamplingScale);
begin
  FXSamplingScale.Assign(val);
end;

// SetYSamplingScale
//

procedure TgxXYZGrid.SetYSamplingScale(const val: TgxSamplingScale);
begin
  FYSamplingScale.Assign(val);
end;

// SetZSamplingScale
//

procedure TgxXYZGrid.SetZSamplingScale(const val: TgxSamplingScale);
begin
  FZSamplingScale.Assign(val);
end;

// SetParts
//

procedure TgxXYZGrid.SetParts(const val: TXYZGridParts);
begin
  if FParts <> val then
  begin
    FParts := val;
    StructureChanged;
  end;
end;

// SetLinesStyle
//

procedure TgxXYZGrid.SetLinesStyle(const val: TXYZGridLinesStyle);
begin
  if FLinesStyle <> val then
  begin
    FLinesStyle := val;
    StructureChanged;
  end;
end;

// SetLinesSmoothing
//

procedure TgxXYZGrid.SetLinesSmoothing(const val: Boolean);
begin
  AntiAliased := val;
end;

// NotifyChange
//

procedure TgxXYZGrid.NotifyChange(Sender: TObject);
begin
  if Sender is TgxSamplingScale then
    StructureChanged;
  inherited NotifyChange(Sender);
end;

// BuildList
//

procedure TgxXYZGrid.BuildList(var rci: TgxRenderContextInfo);
var
  xBase, x, xStep, xMax, yBase, y, yStep, yMax, zBase, z, zStep, zMax: Single;
begin
  SetupLineStyle(rci);
  // precache values
  XSamplingScale.SetBaseStepMaxToVars(xBase, xStep, xMax, (gpX in Parts));
  YSamplingScale.SetBaseStepMaxToVars(yBase, yStep, yMax, (gpY in Parts));
  ZSamplingScale.SetBaseStepMaxToVars(zBase, zStep, zMax, (gpZ in Parts));
  // render X parallel lines
  if gpX in Parts then
  begin
    y := yBase;
    while y <= yMax do
    begin
      z := zBase;
      while z <= zMax do
      begin
        glBegin(GL_LINE_STRIP);
        if LinesStyle = glsSegments then
        begin
          x := xBase;
          while x <= xMax do
          begin
            glVertex3f(x, y, z);
            x := x + xStep;
          end;
        end
        else
        begin
          glVertex3f(XSamplingScale.Min, y, z);
          glVertex3f(XSamplingScale.Max, y, z);
        end;
        glEnd;
        z := z + zStep;
      end;
      y := y + yStep;
    end;
  end;
  // render Y parallel lines
  if gpY in Parts then
  begin
    x := xBase;
    while x <= xMax do
    begin
      z := zBase;
      while z <= zMax do
      begin
        glBegin(GL_LINE_STRIP);
        if LinesStyle = glsSegments then
        begin
          y := yBase;
          while y <= yMax do
          begin
            glVertex3f(x, y, z);
            y := y + yStep;
          end;
        end
        else
        begin
          glVertex3f(x, YSamplingScale.Min, z);
          glVertex3f(x, YSamplingScale.Max, z);
        end;
        glEnd;
        z := z + zStep;
      end;
      x := x + xStep;
    end;
  end;
  // render Z parallel lines
  if gpZ in Parts then
  begin
    x := xBase;
    while x <= xMax do
    begin
      y := yBase;
      while y <= yMax do
      begin
        glBegin(GL_LINE_STRIP);
        if LinesStyle = glsSegments then
        begin
          z := zBase;
          while z <= zMax do
          begin
            glVertex3f(x, y, z);
            z := z + zStep;
          end;
        end
        else
        begin
          glVertex3f(x, y, ZSamplingScale.Min);
          glVertex3f(x, y, ZSamplingScale.Max);
        end;
        glEnd;
        y := y + yStep;
      end;
      x := x + xStep;
    end;
  end;
end;

// -------------------------------------------------------------
// -------------------------------------------------------------
// -------------------------------------------------------------
initialization

// -------------------------------------------------------------
// -------------------------------------------------------------
// -------------------------------------------------------------

RegisterClasses([TgxHeightField, TgxXYZGrid]);

end.
