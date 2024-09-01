//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.Gizmo;

(*
  Invisible component for helping to Move, Rotate and Scale an Object
  (usefull for an Editor).
*)

interface

{$I GXS.Scene.inc}

uses
  System.Classes,
  System.SysUtils,
  FMX.TextLayout,

  GXS.PersistentClasses,
  GXS.VectorGeometry,
  GXS.VectorTypes,
  GXS.Strings,
  GXS.Scene,
  GXS.Color,
  GXS.Objects,
  GXS.Material,
  GXS.GeomObjects,
  GXS.BitmapFont,
  GXS.SceneViewer,
  GXS.VectorFileObjects,
  GXS.Utils,
  GXS.Coordinates,
  GXS.RenderContextInfo,
  GXS.State,
  GXS.Selection;

type
  TgxGizmoUndoCollection = class;
  TgxGizmo = class;

  TgxGizmoUndoItem = class(TCollectionItem)
  private
    FOldLibMaterialName: string;
    FOldAutoScaling: TgxCoordinates;
    FEffectedObject: TgxCustomSceneObject;
    FOldMatr: TMatrix4f;
    FOldMatrix: TMatrix4f;
    procedure SetEffectedObject(const Value: TgxCustomSceneObject);
    procedure SetOldAutoScaling(const Value: TgxCoordinates);
    procedure SetOldMatrix(const Value: TMatrix4f);
  protected
    procedure DoUndo; virtual;
    function GetParent: TgxGizmoUndoCollection;
    function GetGizmo: TgxGizmo;
  public
    constructor Create(AOwner: TCollection); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); virtual;
    procedure AssignFromObject(const AObject: TgxCustomSceneObject);
    // TODO: create a special type for Matrix.
    property OldMatrix: TMatrix4f read FOldMatrix write SetOldMatrix;
  published
    property EffectedObject: TgxCustomSceneObject read FEffectedObject
      write SetEffectedObject;
    property OldAutoScaling: TgxCoordinates read FOldAutoScaling
      write SetOldAutoScaling;
    property OldLibMaterialName: string read FOldLibMaterialName
      write FOldLibMaterialName;
  end;

  TgxGizmoUndoCollection = class(TOwnedCollection)
  private
    function GetItems(const Index: Integer): TgxGizmoUndoItem;
    procedure SetItems(const Index: Integer; const Value: TgxGizmoUndoItem);
  protected
    function GetParent: TgxGizmo;
  public
    procedure Notification(AComponent: TComponent; Operation: TOperation);
    procedure RemoveByObject(const AObject: TgxCustomSceneObject);
    function Add: TgxGizmoUndoItem;
    property Items[const Index: Integer]: TgxGizmoUndoItem read GetItems
      write SetItems; default;
  end;

  TgxGizmoElement = (geMove, geRotate, geScale, geAxisLabel, geObjectInfos,
    geBoundingBox);
  TgxGizmoElements = set of TgxGizmoElement;

  TgxGizmoVisibleInfoLabel = (vliName, vliOperation, vliCoords);
  TgxGizmoVisibleInfoLabels = set of TgxGizmoVisibleInfoLabel;

  TgxGizmoAxis = (gaNone, gaX, gaY, gaZ, gaXY, gaXZ, gaYZ);

  TgxGizmoOperation = (gopMove, gopRotate, gopScale, gopNone, gpMoveGizmo,
    gpRotateGizmo);

  TgxGizmoAcceptEvent = procedure(Sender: TObject; var Obj: TgxBaseSceneObject;
    var Accept: Boolean; var Dimensions: TVector4f) of object;
  TgxGizmoUpdateEvent = procedure(Sender: TObject; Obj: TgxBaseSceneObject;
    Axis: TgxGizmoAxis; Operation: TgxGizmoOperation; var Vector: TVector4f)
    of object;

  TgxGizmoPickMode = (pmGetPickedObjects, pmRayCast);

  TgxGizmoRayCastHitData = class(TPersistent)
  public
    Obj: TgxBaseSceneObject;
    Point: TVector4f;
  end;

  TgxGizmoPickCube = class(TgxCube)
  end;

  TgxGizmoPickTorus = class(TgxTorus)
  end;

  TgxGizmo = class(TComponent)
  private
    _GZObaseGizmo: TgxBaseSceneObject;
    _GZOBoundingcube: TgxCube;
    _GZOrootHelpers: TgxBaseSceneObject;
    _GZOrootLines: TgxBaseSceneObject;
    _GZOrootTorus: TgxBaseSceneObject;
    _GZOrootCubes: TgxBaseSceneObject;
    _GZORootAxisLabel: TgxBaseSceneObject;
    _GZORootVisibleInfoLabels: TgxBaseSceneObject;
    _GZOlineX, _GZOlineY, _GZOlineZ, _GZOplaneXY, _GZOplaneXZ,
      _GZOplaneYZ: TgxLines; // For Move
    _GZOTorusX, _GZOTorusY, _GZOTorusZ: TgxGizmoPickTorus; // For Rotate
    _GZOCubeX, _GZOCubeY, _GZOCubeZ: TgxGizmoPickCube; // For Scale
    _GZOAxisLabelX, _GZOAxisLabelY, _GZOAxisLabelZ: TgxFlatText;
    _GZOVisibleInfoLabels: TgxFlatText;
    FRootGizmo: TgxBaseSceneObject;
    FSelectedObj: TgxBaseSceneObject;
    // FLastOperation,
    FOperation: TgxGizmoOperation;
    FSelAxis: TgxGizmoAxis;
    FBoundingBoxColor: TgxColor;
    FSelectedColor: TgxColor;
    FVisibleInfoLabelsColor: TgxColor;
    FBoundingBoxColorChanged: Boolean;
    FVisibleInfoLabelsColorChanged: Boolean;
    FForceOperation: Boolean;
    FForceAxis: Boolean;
    FForceUniformScale: Boolean;
    FAutoZoom: Boolean;
    FExcludeObjects: Boolean;
    FNoZWrite: Boolean;
    FEnabled: Boolean;
    FAutoZoomFactor: Single;
    FZoomFactor: Single;
    FMoveCoef: Single;
    FRotationCoef: Single;
    FViewer: TgxSceneViewer;
    FGizmoElements: TgxGizmoElements;
    FVisibleVisibleInfoLabels: TgxGizmoVisibleInfoLabels;
    FExcludeObjectsList: TStrings;
    Moving: Boolean;
    Mx, My: Integer;
    Rx, Ry: Integer;
    dglEnable, dglDisable, dgtEnable, dgtDisable, dgcEnable, dgcDisable,
      dglaEnable, dglaDisable, dgliEnable, dgliDisable: TgxDirectOpenGL;
    LastMousePos: TVector4f;
    ObjDimensions: TVector4f;
    FOnBeforeSelect: TgxGizmoAcceptEvent;
    FOnBeforeUpdate: TgxGizmoUpdateEvent;
    FOnSelectionLost: TNotifyEvent;
    FScaleCoef: Single;
    FGizmoThickness: Single;
    FPickMode: TgxGizmoPickMode;
    FInternalRaycastHitData: TList;
    FUndoHistory:  TgxGizmoUndoCollection;
    FLabelFont: TgxCustomBitmapFont;
    procedure SetRootGizmo(const AValue: TgxBaseSceneObject);
    procedure SetGizmoElements(const AValue: TgxGizmoElements);
    procedure SetGizmoVisibleInfoLabels(const AValue
      : TgxGizmoVisibleInfoLabels);
    procedure SetBoundingBoxColor(const AValue: TgxColor);
    procedure SetSelectedColor(const AValue: TgxColor);
    procedure SetVisibleInfoLabelsColor(const AValue: TgxColor);
    procedure SetExcludeObjectsList(const AValue: TStrings);
    procedure DirectGLDisable(Sender: TObject; var Rci: TgxRenderContextInfo);
    procedure DirectGLEnable(Sender: TObject; var Rci: TgxRenderContextInfo);
    function MouseWorldPos(const X, Y: Integer): TVector4f;
    function CheckObjectInExcludeList(const Obj: TgxBaseSceneObject): Boolean;
    procedure UpdateVisibleInfoLabels;
    procedure SetGizmoThickness(const Value: Single);
    function InternalGetPickedObjects(const X1, Y1, X2, Y2: Integer;
      const GuessCount: Integer = 8): TgxPickList;
    procedure ClearInternalRaycastHitData;
    procedure SetViewer(const Value: TgxSceneViewer);
    procedure SetLabelFont(const Value: TgxCustomBitmapFont);
    procedure SetSelectedObj(const Value: TgxBaseSceneObject);
  public
    PickableObjectsWithRayCast: TList;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure ViewerMouseMove(const X, Y: Integer);
    procedure ViewerMouseDown(const X, Y: Integer);
    procedure ViewerMouseUp(const X, Y: Integer);
    procedure UpdateGizmo; overload;
    procedure UpdateGizmo(const NewDimensions: TVector4f); overload;
    procedure SetVisible(const AValue: Boolean);
    function GetPickedObjectPoint(const Obj: TgxBaseSceneObject): TVector4f;
    procedure LooseSelection; virtual;
    procedure UndoAdd(const AObject: TgxCustomSceneObject);
    property RootGizmo: TgxBaseSceneObject read FRootGizmo write SetRootGizmo;
    // --------------------------------------------------------------------
  published
    property Viewer: TgxSceneViewer read FViewer write SetViewer;
    property GizmoElements: TgxGizmoElements read FGizmoElements
      write SetGizmoElements;
    property BoundingBoxColor: TgxColor read FBoundingBoxColor
      write SetBoundingBoxColor;
    property SelectedColor: TgxColor read FSelectedColor write SetSelectedColor;
    property SelAxis: TgxGizmoAxis read FSelAxis write FSelAxis;
    property ForceAxis: Boolean read FForceAxis write FForceAxis;
    property SelectedObj: TgxBaseSceneObject read FSelectedObj
      write SetSelectedObj;
    property Operation: TgxGizmoOperation read FOperation write FOperation;
    property ForceOperation: Boolean read FForceOperation write FForceoperation;
    property ForceUniformScale: Boolean read FForceUniformScale
      write FForceUniformScale;
    property ExcludeObjects: Boolean read FExcludeObjects write FExcludeObjects;
    property ExcludeObjectsList: TStrings read FExcludeObjectsList
      write SetExcludeObjectsList;
    property VisibleInfoLabels: TgxGizmoVisibleInfoLabels
      read FVisibleVisibleInfoLabels write SetGizmoVisibleInfoLabels;
    property VisibleInfoLabelsColor: TgxColor read FVisibleInfoLabelsColor
      write SetVisibleInfoLabelsColor;
    property AutoZoom: Boolean read FAutoZoom write FAutoZoom;
    property AutoZoomFactor: Single read FAutoZoomFactor write FAutoZoomFactor;
    property ZoomFactor: Single read FZoomFactor write FZoomFactor;
    property MoveCoef: Single read FMoveCoef write FMoveCoef;
    property RotationCoef: Single read FRotationCoef write FRotationCoef;
    property ScaleCoef: Single read FScaleCoef write FScaleCoef;
    property NoZWrite: Boolean read FNoZWrite write FNoZWrite;
    property GizmoThickness: Single read FGizmoThickness
      write SetGizmoThickness;
    (* Indicates whether the gizmo is enabled or not.
      WARNING: When loading/editing (possibly whenever a structureChanged
      call is made) a model, sometimes the gizmo will trigger a
      bug if the mouse is inside the Viewer. To prevent that,
      remember to disable the gizmo before loading, then process windows
      messages (i.e. application.processMessage) and then enable the gizmo
      again. *)
    (* Warning Enable is ReadOnly property if you set to False, Gizmo is not Hidden
      use Visible instead if you want to Hide, if you want to Hide but keep enabled
      see the VisibleGizmo property *)
    property Enabled: Boolean read FEnabled write FEnabled default False;
    property LabelFont: TgxCustomBitmapFont read FLabelFont write SetLabelFont
      default nil;
    property OnBeforeSelect: TgxGizmoAcceptEvent read FOnBeforeSelect
      write FOnBeforeSelect;
    property OnSelectionLost: TNotifyEvent read FOnSelectionLost
      write FOnSelectionLost;
    (* Called before an Update is applied. The "vector" parameter is the difference
      that will be applied to the object, according to the axis and
      operation selected. *)
    property OnBeforeUpdate: TgxGizmoUpdateEvent read FOnBeforeUpdate
      write FOnBeforeUpdate;
    property PickMode: TgxGizmoPickMode read FPickMode write FPickMode
      default PmGetPickedObjects;
  end;

//=========================================================
implementation
//=========================================================

procedure RotateAroundArbitraryAxis(const AnObject: TgxBaseSceneObject;
  const Axis, Origin: TAffineVector; const Angle: Single);
var
  M, M1, M2, M3: TMatrix4f;
begin
  M1 := CreateTranslationMatrix(VectorNegate(Origin));
  M2 := CreateRotationMatrix(Axis, Angle * PI / 180);
  M3 := CreateTranslationMatrix(Origin);
  M := MatrixMultiply(M1, M2);
  M := MatrixMultiply(M, M3);
  AnObject.SetMatrix(MatrixMultiply(AnObject.Matrix^, M));

  // Just a workarround to Update angles...
  AnObject.Roll(0);
  AnObject.Pitch(0);
  AnObject.Turn(0);
end;

// ------------------------------------------------------------------------------

procedure TgxGizmo.ClearInternalRaycastHitData;
var
  T: Integer;
begin
  for T := FInternalRaycastHitData.Count - 1 downto 0 do
  begin
    TgxGizmoRayCastHitData(FInternalRaycastHitData[T]).Free;
  end;
  FInternalRaycastHitData.Clear;
end;

constructor TgxGizmo.Create(AOwner: TComponent);
var
  Cub: TgxCube;
begin
  inherited Create(AOwner);
  FUndoHistory := TgxGizmoUndoCollection.Create(Self, TgxGizmoUndoItem);
  FPickMode := PmGetPickedObjects;
  PickableObjectsWithRayCast := TList.Create;
  FRotationCoef := 1;
  FMoveCoef := 0.1;
  FScaleCoef := 0.1;
  FGizmoThickness := 1;

  FInternalRaycastHitData := TList.Create;
  FBoundingBoxColor := TgxColor.Create(Self);
  FBoundingBoxColor.Color := ClrWhite;
  FBoundingBoxColorChanged := False;
  FSelectedColor := TgxColor.Create(Self);
  FSelectedColor.Color := ClrYellow;
  FVisibleInfoLabelsColor := TgxColor.Create(Self);
  FVisibleInfoLabelsColor.Color := ClrYellow;
  FVisibleInfoLabelsColorChanged := False;

  _GZObaseGizmo := TgxDummyCube.Create(Self);
  _GZORootHelpers := TgxDummyCube(_GZObaseGizmo.AddNewChild(TgxDummyCube));
  _GZOBoundingcube := TgxCube(_GZORootHelpers.AddNewChild(TgxCube));

  _GZORootLines := _GZORootHelpers.AddNewChild(TgxDummyCube);
  _GZORootTorus := _GZORootHelpers.AddNewChild(TgxDummyCube);
  _GZORootCubes := _GZORootHelpers.AddNewChild(TgxDummyCube);
  _GZORootAxisLabel := _GZORootHelpers.AddNewChild(TgxDummyCube);
  _GZORootVisibleInfoLabels := _GZORootHelpers.AddNewChild(TgxDummyCube);

  DglDisable := TgxDirectOpenGL(_GZORootLines.AddNewChild(TgxDirectOpenGL));
  DglDisable.OnRender := DirectGlDisable;
  DgtDisable := TgxDirectOpenGL(_GZORootTorus.AddNewChild(TgxDirectOpenGL));
  DgtDisable.OnRender := DirectGlDisable;
  DgcDisable := TgxDirectOpenGL(_GZORootCubes.AddNewChild(TgxDirectOpenGL));
  DgcDisable.OnRender := DirectGlDisable;
  DglaDisable := TgxDirectOpenGL
    (_GZORootAxisLabel.AddNewChild(TgxDirectOpenGL));
  DglaDisable.OnRender := DirectGlDisable;
  DgliDisable := TgxDirectOpenGL(_GZORootVisibleInfoLabels.AddNewChild
    (TgxDirectOpenGL));
  DgliDisable.OnRender := DirectGlDisable;

  with _GZOBoundingcube.Material do
  begin
    FaceCulling := FcNoCull;
    PolygonMode := PmLines;
    with FrontProperties do
    begin
      Diffuse.Color := FBoundingBoxColor.Color;
      Ambient.Color := FBoundingBoxColor.Color;
      Emission.Color := FBoundingBoxColor.Color;
    end;
    with BackProperties do
    begin
      Diffuse.Color := FBoundingBoxColor.Color;
      Ambient.Color := FBoundingBoxColor.Color;
      Emission.Color := FBoundingBoxColor.Color;
    end;
  end;

  _GZOlinex := TgxLines(_GZORootLines.AddnewChild(TgxLines));
  with _GZOlinex do
  begin
    LineColor.Color := clrRed;
    LineWidth := 3;
    NodesAspect := LnaInvisible;
    AddNode(0, 0, 0);
    AddNode(1, 0, 0);
    AddNode(0.9, 0, -0.1);
    AddNode(1, 0, 0);
    AddNode(0.9, 0, 0.1);
    // Raycast pickable object
    Cub := TgxGizmoPickCube(AddNewChild(TgxGizmoPickCube));
    Cub.Up.SetVector(1, 0, 0);
    Cub.CubeWidth := 0.1;
    Cub.CubeHeight := 1;
    Cub.CubeDepth := 0.1;
    Cub.Position.SetPoint(0.5, 0, 0);
    Cub.Visible := False;
  end;

  _GZOliney := TgxLines(_GZORootLines.AddnewChild(TgxLines));
  with _GZOliney do
  begin
    LineColor.Color := clrLime;
    LineWidth := 3;
    NodesAspect := LnaInvisible;
    AddNode(0, 0, 0);
    AddNode(0, 1, 0);
    AddNode(0.1, 0.9, 0);
    AddNode(0, 1, 0);
    AddNode(-0.1, 0.9, 0);
    // Raycast pickable object
    Cub := TgxGizmoPickCube(AddNewChild(TgxGizmoPickCube));
    Cub.Up.SetVector(0, 1, 0);
    Cub.CubeWidth := 0.1;
    Cub.CubeHeight := 1;
    Cub.CubeDepth := 0.1;
    Cub.Position.SetPoint(0, 0.5, 0);
    Cub.Visible := False;
  end;

  _GZOlinez := TgxLines(_GZORootLines.AddnewChild(TgxLines));
  with _GZOlinez do
  begin
    LineColor.Color := clrBlue;
    LineWidth := 3;
    NodesAspect := LnaInvisible;
    AddNode(0, 0, 0);
    AddNode(0, 0, 1);
    AddNode(0.1, 0, 0.9);
    AddNode(0, 0, 1);
    AddNode(-0.1, 0, 0.9);
    // Raycast pickable object
    Cub := TgxGizmoPickCube(AddNewChild(TgxGizmoPickCube));
    Cub.Up.SetVector(0, 0, 1);
    Cub.CubeWidth := 0.1;
    Cub.CubeHeight := 1;
    Cub.CubeDepth := 0.1;
    Cub.Position.SetPoint(0, 0, 0.5);
    Cub.Visible := False;
  end;

  _GZOplaneXY := TgxLines(_GZORootLines.AddnewChild(TgxLines));
  with _GZOplaneXY do
  begin
    LineWidth := 3;
    Options := [LoUseNodeColorForLines];
    NodesAspect := LnaInvisible;
    SplineMode := LsmSegments;
    AddNode(0.8, 1, 0);
    TgxLinesNode(Nodes[0]).Color.Color := clrRed;
    AddNode(1, 1, 0);
    TgxLinesNode(Nodes[1]).Color.Color := clrRed;
    AddNode(1, 1, 0);
    TgxLinesNode(Nodes[2]).Color.Color := clrLime;
    AddNode(1, 0.8, 0);
    TgxLinesNode(Nodes[3]).Color.Color := clrLime;
    // Raycast pickable object
    Cub := TgxGizmoPickCube(AddNewChild(TgxGizmoPickCube));
    Cub.Up.SetVector(1, 0, 0);
    Cub.CubeWidth := 0.2;
    Cub.CubeHeight := 0.2;
    Cub.CubeDepth := 0.1;
    Cub.Position.SetPoint(0.9, 0.9, 0);
    Cub.Visible := False;
  end;

  _GZOplaneXZ := TgxLines(_GZORootLines.AddnewChild(TgxLines));
  with _GZOplaneXZ do
  begin
    LineWidth := 3;
    Options := [LoUseNodeColorForLines];
    NodesAspect := LnaInvisible;
    SplineMode := LsmSegments;
    AddNode(1, 0, 0.8);
    TgxLinesNode(Nodes[0]).Color.Color := clrBlue;
    AddNode(1, 0, 1);
    TgxLinesNode(Nodes[1]).Color.Color := clrBlue;
    AddNode(1, 0, 1);
    TgxLinesNode(Nodes[2]).Color.Color := clrRed;
    AddNode(0.8, 0, 1);
    TgxLinesNode(Nodes[3]).Color.Color := clrRed;
    // Raycast pickable object
    Cub := TgxGizmoPickCube(AddNewChild(TgxGizmoPickCube));
    Cub.Up.SetVector(1, 0, 0);
    Cub.CubeWidth := 0.1;
    Cub.CubeHeight := 0.2;
    Cub.CubeDepth := 0.2;
    Cub.Position.SetPoint(0.9, 0, 0.9);
    Cub.Visible := False;
  end;

  _GZOplaneYZ := TgxLines(_GZORootLines.AddnewChild(TgxLines));
  with _GZOplaneYZ do
  begin
    LineWidth := 3;
    Options := [LoUseNodeColorForLines];
    NodesAspect := LnaInvisible;
    SplineMode := LsmSegments;
    AddNode(0, 0.8, 1);
    TgxLinesNode(Nodes[0]).Color.Color := clrLime;
    AddNode(0, 1, 1);
    TgxLinesNode(Nodes[1]).Color.Color := clrLime;
    AddNode(0, 1, 1);
    TgxLinesNode(Nodes[2]).Color.Color := clrBlue;
    AddNode(0, 1, 0.8);
    TgxLinesNode(Nodes[3]).Color.Color := clrBlue;
    // Raycast pickable object
    Cub := TgxGizmoPickCube(AddNewChild(TgxGizmoPickCube));
    Cub.Up.SetVector(0, 0, 1);
    Cub.CubeWidth := 0.2;
    Cub.CubeHeight := 0.2;
    Cub.CubeDepth := 0.1;
    Cub.Position.SetPoint(0, 0.9, 0.9);
    Cub.Visible := False;
  end;

  _GZOTorusX := TgxGizmoPickTorus(_GZORootTorus.AddnewChild(TgxGizmoPickTorus));
  with _GZOTorusX do
  begin
    Rings := 16;
    Sides := 4;
    MajorRadius := 0.6;
    MinorRadius := 0.03;
    PitchAngle := 90;
    TurnAngle := 90;
    with Material do
    begin
      // FaceCulling:= fcNoCull;
      PolygonMode := PmFill;
      // BackProperties.PolygonMode:= pmFill;
      FrontProperties.Emission.Color := clrBlue;
    end;
  end;

  _GZOTorusY := TgxGizmoPickTorus(_GZORootTorus.AddnewChild(TgxGizmoPickTorus));
  with _GZOTorusY do
  begin
    Rings := 16;
    Sides := 4;
    MajorRadius := 0.6;
    MinorRadius := 0.03;
    PitchAngle := 90;
    with Material do
    begin
      // FaceCulling:= fcNoCull;
      PolygonMode := PmFill;
      // BackProperties.PolygonMode:= pmFill;
      FrontProperties.Emission.Color := clrRed;
    end;
  end;

  _GZOTorusZ := TgxGizmoPickTorus(_GZORootTorus.AddnewChild(TgxGizmoPickTorus));
  with _GZOTorusZ do
  begin
    Rings := 16;
    Sides := 4;
    MajorRadius := 0.6;
    MinorRadius := 0.03;
    with Material do
    begin
      // FaceCulling:= fcNoCull;
      PolygonMode := PmFill;
      // BackProperties.PolygonMode:= pmFill;
      FrontProperties.Emission.Color := clrLime;
    end;
  end;

  _GZOCubeX := TgxGizmoPickCube(_GZORootCubes.AddnewChild(TgxGizmoPickCube));
  with _GZOCubeX do
  begin
    CubeDepth := 0.1;
    CubeHeight := 0.1;
    CubeWidth := 0.1;
    Position.X := 1.15;
    with Material do
    begin
      FaceCulling := FcNoCull;
      PolygonMode := PmFill;
      FrontProperties.Emission.Color := clrRed;
    end;
  end;

  _GZOCubeY := TgxGizmoPickCube(_GZORootCubes.AddnewChild(TgxGizmoPickCube));
  with _GZOCubeY do
  begin
    CubeDepth := 0.1;
    CubeHeight := 0.1;
    CubeWidth := 0.1;
    Position.Y := 1.15;
    with Material do
    begin
      FaceCulling := FcNoCull;
      PolygonMode := PmFill;
      FrontProperties.Emission.Color := clrLime;
    end;
  end;

  _GZOCubeZ := TgxGizmoPickCube(_GZORootCubes.AddnewChild(TgxGizmoPickCube));
  with _GZOCubeZ do
  begin
    CubeDepth := 0.1;
    CubeHeight := 0.1;
    CubeWidth := 0.1;
    Position.Z := 1.15;
    with Material do
    begin
      FaceCulling := FcNoCull;
      PolygonMode := PmFill;
      FrontProperties.Emission.Color := clrBlue;
    end;
  end;

  _GZOAxisLabelX := TgxFlatText(_GZORootAxisLabel.AddNewChild(TgxFlatText));
  with _GZOAxisLabelX do
  begin
    ModulateColor.Color := ClrRed;
    Alignment := TaCenter;
    Layout := TlCenter;
    Options := Options + [FtoTwoSided];
    Position.X := 1.5;
    Scale.X := 0.02;
    Scale.Y := 0.02;
    Text := 'X';
  end;

  _GZOAxisLabelY := TgxFlatText(_GZORootAxisLabel.AddNewChild(TgxFlatText));
  with _GZOAxisLabelY do
  begin
    ModulateColor.Color := clrLime;
    Alignment := taCenter;
    Layout := tlCenter;
    Options := Options + [FtoTwoSided];
    Position.Y := 1.5;
    Scale.X := 0.02;
    Scale.Y := 0.02;
    Text := 'Y';
  end;

  _GZOAxisLabelZ := TgxFlatText(_GZORootAxisLabel.AddNewChild(TgxFlatText));
  with _GZOAxisLabelZ do
  begin
    ModulateColor.Color := ClrBlue;
    Alignment := taCenter;
    Layout := tlCenter;
    Options := Options + [FtoTwoSided];
    Position.Z := 1.5;
    Scale.X := 0.02;
    Scale.Y := 0.02;
    Text := 'Z';
  end;

  _GZOVisibleInfoLabels :=
    TgxFlatText(_GZORootVisibleInfoLabels.AddNewChild(TgxFlatText));
  with _GZOVisibleInfoLabels do
  begin
    ModulateColor.Color := clrYellow;
    Alignment := taCenter;
    Layout := tlCenter;
    Options := Options + [FtoTwoSided];
    Position.Y := 1.8;
    Position.X := 0;
    Scale.X := 0.01;
    Scale.Y := 0.01;
    Text := '';
  end;

  DglEnable := TgxDirectOpenGL(_GZORootLines.AddNewChild(TgxDirectOpenGL));
  DglEnable.OnRender := DirectGlEnable;
  DgtEnable := TgxDirectOpenGL(_GZORootTorus.AddNewChild(TgxDirectOpenGL));
  DgtEnable.OnRender := DirectGlEnable;
  DgcEnable := TgxDirectOpenGL(_GZORootCubes.AddNewChild(TgxDirectOpenGL));
  DgcEnable.OnRender := DirectGlEnable;
  DglaEnable := TgxDirectOpenGL(_GZORootAxisLabel.AddNewChild(TgxDirectOpenGL));
  DglaEnable.OnRender := DirectGlEnable;
  DgliEnable := TgxDirectOpenGL(_GZORootVisibleInfoLabels.AddNewChild
    (TgxDirectOpenGL));
  DgliEnable.OnRender := DirectGlEnable;

  _GZObaseGizmo.Visible := False;
  FGizmoElements := FGizmoElements + [GeMove, GeRotate, GeScale, GeAxisLabel,
    GeObjectInfos, GeBoundingBox];
  FVisibleVisibleInfoLabels := FVisibleVisibleInfoLabels +
    [VliName, VliOperation, VliCoords];
  AutoZoom := True;
  AutoZoomFactor := 5.0;
  ZoomFactor := 0.35;
  ForceOperation := False;
  ForceAxis := False;
  ForceUniformScale := False;
  Enabled := True;
  FNoZWrite := True;
  FExcludeObjectsList := TStringList.Create;
end;

destructor TgxGizmo.Destroy;
begin
  if Assigned(FRootGizmo) then
    FRootGizmo.DeleteChildren
  else
  begin
    _GZOBaseGizmo.DeleteChildren;
    _GZOBaseGizmo.Free;
  end;

  FBoundingBoxColor.Free;
  FSelectedColor.Free;
  FVisibleInfoLabelsColor.Free;
  PickableObjectsWithRayCast.Free;
  FExcludeObjectsList.Free;
  ClearInternalRaycastHitData;
  FInternalRaycastHitData.Free;

  // FUndoHistory has to be nil before Notification() is called.
  FreeAndNil(FUndoHistory);
  inherited Destroy;
end;

procedure TgxGizmo.SetVisible(const AValue: Boolean);
begin
  _GZObaseGizmo.Visible := AValue;
end;

procedure TgxGizmo.SetGizmoElements(const AValue: TgxGizmoElements);
begin
  if AValue <> FGizmoElements then
  begin
    FGizmoElements := AValue;
    _GZORootLines.Visible := GeMove in FGizmoElements;
    _GZORootTorus.Visible := GeRotate in FGizmoElements;
    _GZORootCubes.Visible := GeScale in FGizmoElements;
    _GZORootAxisLabel.Visible := GeAxisLabel in FGizmoElements;
    _GZORootVisibleInfoLabels.Visible := GeObjectInfos in FGizmoElements;
    _GZOBoundingcube.Visible := GeBoundingBox in FGizmoElements;
  end;
end;

procedure TgxGizmo.SetBoundingBoxColor(const AValue: TgxColor);
begin
  // Bug Here New Color is not Updated
  if AValue <> FBoundingBoxColor then
  begin
    FBoundingBoxColor.Color := AValue.Color;
    with _GZOBoundingcube.Material do
    begin
      with FrontProperties do
      begin
        Diffuse.Color := FBoundingBoxColor.Color;
        Ambient.Color := FBoundingBoxColor.Color;
        Emission.Color := FBoundingBoxColor.Color;
      end;
      with BackProperties do
      begin
        Diffuse.Color := FBoundingBoxColor.Color;
        Ambient.Color := FBoundingBoxColor.Color;
        Emission.Color := FBoundingBoxColor.Color;
      end;
    end;
    FBoundingBoxColorChanged := True;
  end;
end;

procedure TgxGizmo.SetSelectedColor(const AValue: TgxColor);
begin
  if AValue <> FSelectedColor then
  begin
    FSelectedColor.Color := AValue.Color;
  end;
end;

procedure TgxGizmo.SetVisibleInfoLabelsColor(const AValue: TgxColor);
begin
  // Bug Here New Color is not Updated
  if AValue <> FSelectedColor then
  begin
    FVisibleInfoLabelsColor.Color := AValue.Color;
    _GZOVisibleInfoLabels.ModulateColor.Color := AValue.Color;
    FVisibleInfoLabelsColorChanged := True;
  end;
end;

procedure TgxGizmo.SetGizmoVisibleInfoLabels(const AValue
  : TgxGizmoVisibleInfoLabels);
begin
  if AValue <> FVisibleVisibleInfoLabels then
  begin
    FVisibleVisibleInfoLabels := AValue;
    if not(CsDesigning in ComponentState) then
      UpdateGizmo;
  end;
end;

procedure TgxGizmo.UndoAdd(const AObject: TgxCustomSceneObject);
begin
  if AObject <> nil then
  begin
    FUndoHistory.Add.AssignFromObject(AObject)
  end;
end;

procedure TgxGizmo.SetRootGizmo(const AValue: TgxBaseSceneObject);
begin
  if FRootGizmo <> AValue then
  begin
    if FRootGizmo <> nil then
      FRootGizmo.RemoveFreeNotification(Self);
    FRootGizmo := AValue;
    if FRootGizmo <> nil then
      FRootGizmo.FreeNotification(Self);
    _GZObaseGizmo.MoveTo(AValue);
  end;
end;

procedure TgxGizmo.SetExcludeObjectsList(const AValue: TStrings);
begin
  FExcludeObjectsList.Clear;
  FExcludeObjectsList.AddStrings(AValue);
end;

procedure TgxGizmo.SetGizmoThickness(const Value: Single);
var
  Thk: Single;
begin
  if FGizmoThickness <> Value then
  begin
    Thk := MaxInteger(1, Round(3 * Value));
    _GZOlinex.LineWidth := Thk;
    _GZOliney.LineWidth := Thk;
    _GZOlinez.LineWidth := Thk;
    _GZOplaneXY.LineWidth := Thk;
    _GZOplaneXZ.LineWidth := Thk;
    _GZOplaneYZ.LineWidth := Thk;

    _GZOTorusX.MinorRadius := 0.03 * Value;
    _GZOTorusY.MinorRadius := 0.03 * Value;
    _GZOTorusZ.MinorRadius := 0.03 * Value;

    with _GZOCubeX do
    begin
      CubeDepth := 0.1 * Value;
      CubeHeight := 0.1 * Value;
      CubeWidth := 0.1 * Value;
    end;
    with _GZOCubeY do
    begin
      CubeDepth := 0.1 * Value;
      CubeHeight := 0.1 * Value;
      CubeWidth := 0.1 * Value;
    end;
    with _GZOCubeZ do
    begin
      CubeDepth := 0.1 * Value;
      CubeHeight := 0.1 * Value;
      CubeWidth := 0.1 * Value;
    end;

    FGizmoThickness := Value;
  end;
end;

// ------------------------------------------------------------------------------

procedure TgxGizmo.DirectGlDisable(Sender: TObject;
  var Rci: TgxRenderContextInfo);
begin
  if FNoZWrite then
    Rci.gxStates.Disable(StDepthTest);
end;

procedure TgxGizmo.SetLabelFont(const Value: TgxCustomBitmapFont);
begin
  if FLabelFont <> Value then
  begin
    if FLabelFont <> nil then
      FLabelFont.RemoveFreeNotification(Self);
    FLabelFont := Value;
    if FLabelFont <> nil then
      FLabelFont.FreeNotification(Self);

    _GZOAxisLabelX.BitmapFont := Value;
    _GZOAxisLabelY.BitmapFont := Value;
    _GZOAxisLabelZ.BitmapFont := Value;
    _GZOVisibleInfoLabels.BitmapFont := Value;
  end;
end;

procedure TgxGizmo.DirectGlEnable(Sender: TObject; var Rci: TgxRenderContextInfo);
begin
  if FNoZWrite then
    Rci.gxStates.Enable(StDepthTest);
end;

function TgxGizmo.GetPickedObjectPoint(const Obj: TgxBaseSceneObject): TVector4f;
var
  T: Integer;
  R: TgxGizmoRayCastHitData;
begin
  for T := 0 to FInternalRaycastHitData.Count - 1 do
  begin
    R := TgxGizmoRayCastHitData(FInternalRaycastHitData[T]);
    if R.Obj = Obj then
    begin
      Result := R.Point;
      Break;
    end;
  end;
end;

function TgxGizmo.InternalGetPickedObjects(const X1, Y1, X2, Y2: Integer;
  const GuessCount: Integer): TgxPickList;
var
  T: Integer;
  RayStart, RayVector, IPoint, INormal: TVector4f;
  O: TgxBaseSceneObject;
  Dist: Single;
  HitData: TgxGizmoRayCastHitData;

  procedure AddGizmosToPicklListRecurse(const Root: TgxBaseSceneObject);
  var
    U: Integer;
  begin
    for U := 0 to Root.Count - 1 do
    begin
      if ((Root[U] is TgxGizmoPickTorus) or (Root[U] is TgxGizmoPickCube)) then
        PickableObjectsWithRayCast.Add(Root[U]);
      AddGizmosToPicklListRecurse(Root[U]);
    end;
  end;

begin
  case FPickMode of
    PmGetPickedObjects:
      begin
        Result := Viewer.Buffer.GetPickedObjects(Rect(X1, Y1, X2, Y2),
          GuessCount);
      end;

    PmRayCast:
      begin
        Result := TgxPickList.Create(PsMinDepth);
        ClearInternalRaycastHitData;
        SetVector(RayStart, Viewer.Camera.AbsolutePosition);
        SetVector(RayVector, Viewer.Buffer.ScreenToVector
          (AffineVectorMake((X1 + X2) * 0.5,
          Viewer.Height - ((Y1 + Y2) * 0.5), 0)));
        NormalizeVector(RayVector);
        // Add gizmos
        if (RootGizmo <> nil) and (SelectedObj <> nil) then
          AddGizmosToPicklListRecurse(RootGizmo);
        // pick
        for T := 0 to PickableObjectsWithRayCast.Count - 1 do
        begin
          O := TgxBaseSceneObject(PickableObjectsWithRayCast[T]);
          if (O.RayCastIntersect(RayStart, RayVector, @IPoint, @INormal)) and
            (VectorDotProduct(RayVector, INormal) < 0) then
          begin
            try
              Dist := VectorLength(VectorSubtract(IPoint, RayStart));
              Result.AddHit(O, nil, Dist, 0);
              HitData := TgxGizmoRayCastHitData.Create;
              HitData.Obj := O;
              MakeVector(HitData.Point, IPoint);
              FInternalRaycastHitData.Add(HitData);
            except
              //
            end;
          end;
        end;
      end;

  else
    begin
      Result := nil;
      Assert(False, strErrorEx + strUnknownType);
    end;

  end;
end;

procedure TgxGizmo.Loaded;
begin
  inherited;
  SetGizmoThickness(GizmoThickness);
end;

// ------------------------------------------------------------------------------
procedure TgxGizmo.UpdateVisibleInfoLabels;
var
  T: string;
  X, Y, Z: Single;
begin
  T := '';
  if not(Assigned(SelectedObj)) then
    Exit;
  if VliName in FVisibleVisibleInfoLabels then
    T := SelectedObj.Name;

  if VliOperation in FVisibleVisibleInfoLabels then
  begin
    if (Operation <> GopNone) then
    begin
      if Length(T) > 0 then
        T := T + ' - ';
      case Operation of
        GopMove:
          T := T + 'Move';
        GopRotate:
          T := T + 'Rotate';
        GopScale:
          T := T + 'Scale';
      end;
    end;
  end;

  if VliCoords in FVisibleVisibleInfoLabels then
  begin
    if (Operation <> GopNone) then
    begin
      if Length(T) > 0 then
        T := T + ' - ';
      case Operation of
        GopMove:
          begin
            X := SelectedObj.Position.X;
            Y := SelectedObj.Position.Y;
            Z := SelectedObj.Position.Z;
            T := T + 'X : ' + Format('%2.3f', [X]);
            T := T + ' Y : ' + Format('%2.3f', [Y]);
            T := T + ' Z : ' + Format('%2.3f', [Z]);
          end;
        GopRotate:
          begin
            X := SelectedObj.Rotation.X;
            Y := SelectedObj.Rotation.Y;
            Z := SelectedObj.Rotation.Z;
            T := T + 'X : ' + Format('%2.3f', [X]);
            T := T + ' Y : ' + Format('%2.3f', [Y]);
            T := T + ' Z : ' + Format('%2.3f', [Z]);
          end;
        GopScale:
          begin
            X := SelectedObj.Scale.X;
            Y := SelectedObj.Scale.Y;
            Z := SelectedObj.Scale.Z;
            T := T + 'X : ' + Format('%2.3f', [X]);
            T := T + ' Y : ' + Format('%2.3f', [Y]);
            T := T + ' Z : ' + Format('%2.3f', [Z]);
          end;
      end;
    end;
  end;

  _GZOVisibleInfoLabels.Text := T;
  _GZOVisibleInfoLabels.StructureChanged;
end;

// ------------------------------------------------------------------------------

function TgxGizmo.CheckObjectInExcludeList
  (const Obj: TgxBaseSceneObject): Boolean;
var
  I: Integer;
begin
  Result := False;
  if FExcludeObjects then
  begin
    for I := 0 to FExcludeObjectsList.Count - 1 do
    begin
      if UpperCase(Obj.Name) = UpperCase(FExcludeObjectsList[I]) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

function TgxGizmo.MouseWorldPos(const X, Y: Integer): TVector4f;
var
  V: TVector4f;
  InvertedY: Integer;
begin
  InvertedY := Round(Viewer.Height) - Y;
  if Assigned(SelectedObj) then
  begin
    SetVector(V, X, InvertedY, 0);

    case SelAxis of
      GaX:
        if not Viewer.Buffer.ScreenVectorIntersectWithPlaneXZ(V,
          SelectedObj.AbsolutePosition.Y, Result) then
          MakeVector(Result, X / 5, 0, 0);

      GaY:
        if not Viewer.Buffer.ScreenVectorIntersectWithPlaneYZ(V,
          SelectedObj.AbsolutePosition.X, Result) then
          MakeVector(Result, 0, InvertedY / 5, 0);

      GaZ:
        if not Viewer.Buffer.ScreenVectorIntersectWithPlaneYZ(V,
          SelectedObj.AbsolutePosition.X, Result) then
          MakeVector(Result, 0, 0, -InvertedY / 5);

      GaXY:
        begin
          Viewer.Buffer.ScreenVectorIntersectWithPlaneXY(V,
            SelectedObj.AbsolutePosition.Z, Result);
        end;
      GaXZ:
        begin
          Viewer.Buffer.ScreenVectorIntersectWithPlaneXZ(V,
            SelectedObj.AbsolutePosition.Y, Result);
        end;
      GaYZ:
        begin
          Viewer.Buffer.ScreenVectorIntersectWithPlaneYZ(V,
            SelectedObj.AbsolutePosition.X, Result);
        end;
    end;

  end
  else
    SetVector(Result, NullVector);
end;

procedure TgxGizmo.ViewerMouseMove(const X, Y: Integer);
var
  PickList: TgxPickList;
  MousePos: TVector4f;

  function IndexOf(Obj: TgxBaseSceneObject): Integer;
  var
    I: Integer;
  begin
    Result := -1;
    for I := 0 to PickList.Count - 1 do
      if PickList.Hit[I] = Obj then
      begin
        Result := I;
        Break;
      end;
  end;

  function LightLine(const Line: TgxLines; const Dark: TVector4f;
    const Axis: TgxGizmoAxis; AlterStyle: Boolean = False): Boolean;
  var
    PickObj: TgxBaseSceneObject;
  begin
    case FPickMode of
      PmGetPickedObjects:
        PickObj := Line;
      PmRayCast:
        PickObj := Line;
    else
      begin
        PickObj := nil;
        Assert(False, strErrorEx + strUnknownType);
      end;
    end;

    if IndexOf(PickObj) > -1 then
    begin
      Line.LineColor.Color := FSelectedColor.Color;
      if not(FForceOperation) then
        if Operation <> GopMove then
          Operation := GopMove;
      Line.Options := [];
      if not(FForceAxis) then
        SelAxis := Axis;
      Result := True;
    end
    else
    begin
      Line.LineColor.Color := Dark;
      if not(FForceOperation) then
        Operation := GopNone;
      if AlterStyle then
        Line.Options := [LoUseNodeColorForLines];
      if not(FForceAxis) then
        if SelAxis = Axis then
          SelAxis := GaNone;
      Result := False;
    end;
  end;

  function LightTorus(const Torus: TgxGizmoPickTorus; const Dark: TVector4f;
    const Axis: TgxGizmoAxis; AlterStyle: Boolean = False): Boolean;
  begin
    if IndexOf(Torus) > -1 then
    begin
      Torus.Material.FrontProperties.Emission.Color := FSelectedColor.Color;
      if not(FForceOperation) then
        if Operation <> GopRotate then
          Operation := GopRotate;
      if not(FForceAxis) then
        SelAxis := Axis;
      Result := True;
    end
    else
    begin
      Torus.Material.FrontProperties.Emission.Color := Dark;
      if not(FForceOperation) then
        Operation := GopNone;
      if not(FForceAxis) then
        if SelAxis = Axis then
          SelAxis := GaNone;
      Result := False;
    end;
  end;

  function LightCube(const Cube: TgxCube; const Dark: TVector4f;
    const Axis: TgxGizmoAxis; AlterStyle: Boolean = False): Boolean;
  begin
    if IndexOf(Cube) > -1 then
    begin
      Cube.Material.FrontProperties.Emission.Color := FSelectedColor.Color;
      if not(FForceOperation) then
        if Operation <> GopScale then
          Operation := GopScale;
      if not(FForceAxis) then
        SelAxis := Axis;
      Result := True;
    end
    else
    begin
      Cube.Material.FrontProperties.Emission.Color := Dark;
      if not(FForceOperation) then
        Operation := GopNone;
      if not(FForceAxis) then
        if SelAxis = Axis then
          SelAxis := GaNone;
      Result := False;
    end;
  end;

  procedure OpeMove(MousePos: TVector4f);
  var
    Vec1, Vec2: TVector4f;
    QuantizedMousePos, QuantizedMousePos2: TVector4f;
    T: Integer;
  begin
    for T := 0 to 3 do
    begin
      QuantizedMousePos.V[T] := (Round(MousePos.V[T] / MoveCoef)) * MoveCoef;
      QuantizedMousePos2.V[T] := (Round(LastMousePos.V[T] / MoveCoef)) * MoveCoef;
    end;
    case SelAxis of
      GaX:
        begin
          MakeVector(Vec1, QuantizedMousePos.X, 0, 0);
          MakeVector(Vec2, QuantizedMousePos2.X, 0, 0);
        end;
      GaY:
        begin
          MakeVector(Vec1, 0, QuantizedMousePos.Y, 0);
          MakeVector(Vec2, 0, QuantizedMousePos2.Y, 0);
        end;
      GaZ:
        begin
          MakeVector(Vec1, 0, 0, QuantizedMousePos.Z);
          MakeVector(Vec2, 0, 0, QuantizedMousePos2.Z);
        end;
    else
      begin
        Vec1 := QuantizedMousePos;
        Vec2 := QuantizedMousePos2;
      end;
    end;
    SubtractVector(Vec1, Vec2);
    if Assigned(OnBeforeUpdate) then
      OnBeforeUpdate(Self, SelectedObj, SelAxis, Operation, Vec1);
    Vec1 := SelectedObj.Parent.AbsoluteToLocal(Vec1);
    if (VectorLength(Vec1) > 0) then // prevents NAN problems
    begin
      SelectedObj.Position.Translate(Vec1);
    end;
  end;

  procedure OpeRotate(const X, Y: Integer);
  var
    Vec1: TVector4f;
    RotV: TAffineVector;
    Pmat: TMatrix4f;

  begin
    Vec1.X := 0;
    Vec1.Y := 0;
    if Abs(X - Rx) >= RotationCoef then
    begin
      if RotationCoef > 1 then
        Vec1.X := RotationCoef * (Round((X - Rx) / (RotationCoef)))
      else
        Vec1.X := RotationCoef * (X - Rx);
      Rx := X;
    end;
    if Abs(Y - Ry) >= RotationCoef then
    begin
      if RotationCoef > 1 then
        Vec1.Y := RotationCoef * (Round((Y - Ry) / (RotationCoef)))
      else
        Vec1.Y := RotationCoef * (Y - Ry);
      Ry := Y;
    end;

    Vec1.Z := 0;
    Vec1.W := 0;
    if Assigned(OnBeforeUpdate) then
      OnBeforeUpdate(Self, SelectedObj, SelAxis, Operation, Vec1);

    Pmat := SelectedObj.Parent.InvAbsoluteMatrix;
    SetVector(Pmat.W, NullHmgPoint);
    case SelAxis of
      GaX:
        begin
          RotV := VectorTransform(XVector, Pmat);
          RotateAroundArbitraryAxis(SelectedObj, RotV,
            AffineVectorMake(SelectedObj.Position.AsVector), Vec1.Y);
        end;
      GaY:
        begin
          RotV := VectorTransform(YVector, Pmat);
          RotateAroundArbitraryAxis(SelectedObj, RotV,
            AffineVectorMake(SelectedObj.Position.AsVector), Vec1.X);
        end;
      GaZ:
        begin
          RotV := VectorTransform(ZVector, Pmat);
          RotateAroundArbitraryAxis(SelectedObj, RotV,
            AffineVectorMake(SelectedObj.Position.AsVector), Vec1.Y);
        end;
      GaXY:
        begin
          RotV := VectorTransform(XVector, Pmat);
          RotateAroundArbitraryAxis(SelectedObj, RotV,
            AffineVectorMake(SelectedObj.Position.AsVector), Vec1.Y);
          RotV := VectorTransform(YVector, Pmat);
          RotateAroundArbitraryAxis(SelectedObj, RotV,
            AffineVectorMake(SelectedObj.Position.AsVector), Vec1.X);
        end;
      GaXZ:
        begin
          RotV := VectorTransform(XVector, Pmat);
          RotateAroundArbitraryAxis(SelectedObj, RotV,
            AffineVectorMake(SelectedObj.Position.AsVector), Vec1.Y);
          RotV := VectorTransform(ZVector, Pmat);
          RotateAroundArbitraryAxis(SelectedObj, RotV,
            AffineVectorMake(SelectedObj.Position.AsVector), Vec1.X);
        end;
      GaYZ:
        begin
          RotV := VectorTransform(YVector, Pmat);
          RotateAroundArbitraryAxis(SelectedObj, RotV,
            AffineVectorMake(SelectedObj.Position.AsVector), Vec1.Y);
          RotV := VectorTransform(ZVector, Pmat);
          RotateAroundArbitraryAxis(SelectedObj, RotV,
            AffineVectorMake(SelectedObj.Position.AsVector), Vec1.X);
        end;
    end;
  end;

  procedure OpeScale(const MousePos: TVector4f);
  var
    Vec1, Vec2: TVector4f;
    QuantizedMousePos, QuantizedMousePos2: TVector4f;
    T: Integer;
  begin
    for T := 0 to 3 do
    begin
      QuantizedMousePos.V[T] := (Round(MousePos.V[T] / ScaleCoef)) * FScaleCoef;
      QuantizedMousePos2.V[T] := (Round(LastMousePos.V[T] / FScaleCoef)) *
        FScaleCoef;
    end;
    case SelAxis of
      GaX:
        begin
          if FForceUniformScale then
          begin
            MakeVector(Vec1, QuantizedMousePos.X, QuantizedMousePos.X,
              QuantizedMousePos.X);
            MakeVector(Vec2, QuantizedMousePos2.X, QuantizedMousePos2.X,
              QuantizedMousePos2.X);
          end
          else
          begin
            MakeVector(Vec1, QuantizedMousePos.X, 0, 0);
            MakeVector(Vec2, QuantizedMousePos2.X, 0, 0);
          end;

        end;

      GaY:
        begin
          if FForceUniformScale then
          begin
            MakeVector(Vec1, QuantizedMousePos.Y, QuantizedMousePos.Y,
              QuantizedMousePos.Y);
            MakeVector(Vec2, QuantizedMousePos2.Y, QuantizedMousePos2.Y,
              QuantizedMousePos2.Y);
          end
          else
          begin
            MakeVector(Vec1, 0, QuantizedMousePos.Y, 0);
            MakeVector(Vec2, 0, QuantizedMousePos2.Y, 0);
          end;
        end;

      GaZ:
        begin
          if FForceUniformScale then
          begin
            MakeVector(Vec1, QuantizedMousePos.Z, QuantizedMousePos.Z,
              QuantizedMousePos.Z);
            MakeVector(Vec2, QuantizedMousePos2.Z, QuantizedMousePos2.Z,
              QuantizedMousePos2.Z);
          end
          else
          begin
            MakeVector(Vec1, 0, 0, QuantizedMousePos.Z);
            MakeVector(Vec2, 0, 0, QuantizedMousePos2.Z);
          end;
        end;
    else
      begin
        Vec1 := QuantizedMousePos;
        Vec2 := QuantizedMousePos2;
      end;
    end;
    SubtractVector(Vec1, Vec2);
    if Assigned(OnBeforeUpdate) then
      OnBeforeUpdate(Self, SelectedObj, SelAxis, Operation, Vec1);
    SelectedObj.Scale.Translate(Vec1);
    UpdateGizmo;
  end;

begin
  if not Enabled then
    Exit;

  if Assigned(SelectedObj) and (SelAxis <> GaNone) and Moving then
  begin
    MousePos := MouseWorldPos(X, Y);

    // moving object...
    if Operation = GopMove then
    begin
      // FLastOperation = gopMove;
      OpeMove(MousePos);
    end
    else if Operation = GopRotate then
    begin
      // FLastOperation = gopRotate;
      OpeRotate(X, Y);
    end
    else if Operation = GopScale then
    begin
      // FLastOperation = gopScale;
      OpeScale(MousePos);
    end;

    UpdateGizmo;
    Mx := X;
    My := Y;
    LastMousePos := MousePos;
    Exit;
  end;

  Assert(FViewer <> nil, 'Viewer not Assigned to gizmo');
  Picklist := InternalGetPickedObjects(X - 1, Y - 1, X + 1, Y + 1, 8);
  // Viewer.buffer.GetPickedObjects(rect(x-1, y-1, x+1, y+1), 8);

  if not LightLine(_GZOlinex, ClrRed, GaX) and not LightLine(_GZOliney, ClrLime,
    GaY) and not LightLine(_GZOlinez, ClrBlue, GaZ) and
    not LightTorus(_GZOTorusX, ClrRed, GaX) and
    not LightTorus(_GZOTorusY, ClrLime, GaY) and
    not LightTorus(_GZOTorusz, ClrBlue, GaZ) and
    not LightCube(_GZOCubeX, ClrRed, GaX) and not LightCube(_GZOCubeY, ClrLime,
    GaY) and not LightCube(_GZOCubeZ, ClrBlue, GaZ) and
    not LightLine(_GZOplaneXY, ClrWhite, GaXY, True) and
    not LightLine(_GZOplaneXZ, ClrWhite, GaXZ, True) and
    not LightLine(_GZOplaneYZ, ClrWhite, GaYZ, True) then
  begin
    if not(FForceAxis) then
      SelAxis := GaNone;
    if not(FForceOperation) then
      Operation := GopNone;
  end;

  Picklist.Free;

  Mx := X;
  My := Y;
end;

procedure TgxGizmo.ViewerMouseDown(const X, Y: Integer);
var
  Pick: TgxPickList;
  I: Integer;
  Accept: Boolean;
  Dimensions: TVector4f;
  GotPick: Boolean;
  PickedObj: TgxBaseSceneObject;
begin
  Mx := X;
  My := Y;
  Rx := X;
  Ry := Y;

  if not Enabled then
    Exit;

  Pick := InternalGetPickedObjects(X - 1, Y - 1, X + 1, Y + 1);
  // Viewer.Buffer.GetPickedObjects(rect(x-1, y-1, x+1, y+1));
  GotPick := False;
  Accept := False;

  case FPickMode of
    PmGetPickedObjects:
      begin
        // primeiro, ver se é uma das linhas/planos
        for I := 0 to Pick.Count - 1 do
          if (_GZOrootLines.IndexOfChild(TgxBaseSceneObject(Pick.Hit[I])) > -1)
            or (_GZOrootTorus.IndexOfChild(TgxBaseSceneObject(Pick.Hit[I])) >
            -1) or (_GZOrootCubes.IndexOfChild(TgxBaseSceneObject(Pick.Hit[I]))
            > -1) then
            GotPick := True;
      end;

    PmRayCast:
      begin
        for I := 0 to Pick.Count - 1 do
        begin
          if (Pick.Hit[I] is TgxGizmoPickCube) or
            (Pick.Hit[I] is TgxGizmoPickTorus) then
            GotPick := True;
        end;
      end;
  else
    begin
      Assert(False, strErrorEx + strUnknownType);
    end;

  end;

  if not GotPick then
  begin
    for I := 0 to Pick.Count - 1 do

      if (Pick.Hit[I] <> _GZOBoundingcube) and (Pick.Hit[I] <> _GZOAxisLabelX)
        and (Pick.Hit[I] <> _GZOAxisLabelY) and (Pick.Hit[I] <> _GZOAxisLabelZ)
        and (Pick.Hit[I] <> _GZOVisibleInfoLabels) and
        not(CheckObjectInExcludeList(TgxBaseSceneObject(Pick.Hit[I]))) then
      begin
        Accept := True;
        PickedObj := TgxBaseSceneObject(Pick.Hit[I]);
        Dimensions := PickedObj.AxisAlignedDimensions;
        if Assigned(OnBeforeSelect) then
          OnBeforeSelect(Self, PickedObj, Accept, Dimensions);

        Break;
      end;

    if Accept then
      SetSelectedObj(PickedObj)
    else
      SetSelectedObj(nil);
  end
  else
    UpdateVisibleInfoLabels();

  Pick.Free;

  Moving := True;
  LastMousePos := MouseWorldPos(X, Y);
end;

procedure TgxGizmo.ViewerMouseUp(const X, Y: Integer);
begin
  Moving := False;
end;

// ------------------------------------------------------------------------------

procedure TgxGizmo.UpdateGizmo;
var
  D: Single;
begin
  if SelectedObj = nil then
  begin
    _GZObaseGizmo.Visible := False;
    Exit;
  end;

  _GZObaseGizmo.Position.AsVector := SelectedObj.AbsolutePosition;
  if GeObjectInfos in FGizmoElements then
    UpdateVisibleInfoLabels;

  _GZOBoundingcube.SetMatrix(SelectedObj.AbsoluteMatrix);
  _GZOBoundingcube.Position.SetPoint(0, 0, 0);

  // We must Update Color Of the BoundingBox And VisibleInfoLabels Here
  // If not Color is not Updated;

  // if FBoundingBoxColorChanged then
  // Begin
  with _GZOBoundingcube.Material do
  begin
    with FrontProperties do
    begin
      Diffuse.Color := FBoundingBoxColor.Color;
      Ambient.Color := FBoundingBoxColor.Color;
      Emission.Color := FBoundingBoxColor.Color;
    end;
    with BackProperties do
    begin
      Diffuse.Color := FBoundingBoxColor.Color;
      Ambient.Color := FBoundingBoxColor.Color;
      Emission.Color := FBoundingBoxColor.Color;
    end;
  end;
  // FBoundingBoxColorChanged:=False;
  // End;
  // If FVisibleInfoLabelsColorChanged then
  // Begin
  _GZOVisibleInfoLabels.ModulateColor.Color := FVisibleInfoLabelsColor.Color;
  // FVisibleInfoLabelsColorChanged:=False;
  // End;

  ObjDimensions := SelectedObj.AxisAlignedDimensions;
  _GZOBoundingcube.Scale.AsVector := VectorScale(ObjDimensions, 2);

  Assert(Viewer <> nil, 'Viewer not Assigned to gizmo');

  _GZOAxisLabelX.PointTo(Viewer.Camera.Position.AsVector,
    Viewer.Camera.Up.AsVector);
  _GZOAxisLabelX.StructureChanged;
  _GZOAxisLabelY.PointTo(Viewer.Camera.Position.AsVector,
    Viewer.Camera.Up.AsVector);
  _GZOAxisLabelY.StructureChanged;
  _GZOAxisLabelZ.PointTo(Viewer.Camera.Position.AsVector,
    Viewer.Camera.Up.AsVector);
  _GZOAxisLabelZ.StructureChanged;
  _GZOVisibleInfoLabels.PointTo(Viewer.Camera.Position.AsVector,
    Viewer.Camera.Up.AsVector);
  _GZOVisibleInfoLabels.StructureChanged;
  if FAutoZoom then
    D := Viewer.Camera.DistanceTo(SelectedObj) / FAutoZoomFactor
  else
    D := FZoomFactor;
  _GZOrootLines.Scale.AsVector := VectorMake(D, D, D);
  _GZOrootTorus.Scale.AsVector := VectorMake(D, D, D);
  _GZOrootCubes.Scale.AsVector := VectorMake(D, D, D);
  _GZOrootAxisLabel.Scale.AsVector := VectorMake(D, D, D);
  _GZOrootVisibleInfoLabels.Scale.AsVector := VectorMake(D, D, D);
end;

procedure TgxGizmo.UpdateGizmo(const NewDimensions: TVector4f);
begin
  ObjDimensions := NewDimensions;
  UpdateGizmo;
end;

procedure TgxGizmo.LooseSelection;
begin
  SelectedObj := nil;
  UpdateGizmo;
  if Assigned(OnSelectionLost) then
    OnSelectionLost(Self);
end;

procedure TgxGizmo.SetViewer(const Value: TgxSceneViewer);
begin
  if FViewer <> Value then
  begin
    if FViewer <> nil then
      FViewer.RemoveFreeNotification(Self);
    FViewer := Value;
    if FViewer <> nil then
      FViewer.FreeNotification(Self);
  end;
end;

procedure TgxGizmo.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = OpRemove then
  begin
    if AComponent = FViewer then
      FViewer := nil;
    if AComponent = FRootGizmo then
      FRootGizmo := nil;
  end;

  if FUndoHistory <> nil then
    FUndoHistory.Notification(AComponent, Operation);
end;

procedure TgxGizmoUndoItem.AssignFromObject(const AObject
  : TgxCustomSceneObject);
begin
  SetEffectedObject(AObject);
  SetOldMatrix(AObject.Matrix^);
  if AObject is TgxFreeForm then
  begin
    FOldAutoScaling.Assign(TgxFreeForm(AObject).AutoScaling);
  end;
  FOldLibMaterialName := AObject.Material.LibMaterialName;
end;

constructor TgxGizmoUndoItem.Create(AOwner: TCollection);
begin
  inherited;
  FOldAutoScaling := TgxCoordinates.CreateInitialized(Self,
    NullHmgVector, CsPoint);
end;

destructor TgxGizmoUndoItem.Destroy;
begin
  FOldAutoScaling.Free;
  inherited;
end;

procedure TgxGizmoUndoItem.DoUndo;
begin
  FEffectedObject.SetMatrix(FOldMatr);
  if FEffectedObject is TgxFreeForm then
    TgxFreeForm(FEffectedObject).AutoScaling.Assign(FOldAutoScaling);
  FEffectedObject.Material.LibMaterialName := FOldLibMaterialName;
end;

function TgxGizmoUndoItem.GetGizmo: TgxGizmo;
begin
  if GetParent <> nil then
    Result := GetPArent.GetParent
  else
    Result := nil;
end;

function TgxGizmoUndoItem.GetParent: TgxGizmoUndoCollection;
begin
  Result := TgxGizmoUndoCollection(GetOwner);
end;

procedure TgxGizmoUndoItem.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = OpRemove then
  begin
    if AComponent = FEffectedObject then
      FEffectedObject := nil;
  end;
end;

procedure TgxGizmoUndoItem.SetEffectedObject(const Value: TgxCustomSceneObject);
begin
  if FEffectedObject <> nil then
    FEffectedObject.RemoveFreeNotification(GetGizmo);
  FEffectedObject := Value;
  if FEffectedObject <> nil then
    FEffectedObject.FreeNotification(GetGizmo);
end;

procedure TgxGizmoUndoItem.SetOldAutoScaling(const Value: TgxCoordinates);
begin
  FOldAutoScaling.Assign(Value);
end;

procedure TgxGizmoUndoItem.SetOldMatrix(const Value: TMatrix4f);
begin
  FOldMatrix := Value;
end;

{ TgxGizmoUndoCollection }

function TgxGizmoUndoCollection.Add: TgxGizmoUndoItem;
begin
  Result := TgxGizmoUndoItem(inherited Add);
end;

function TgxGizmoUndoCollection.GetItems(const Index: Integer)
  : TgxGizmoUndoItem;
begin
  Result := TgxGizmoUndoItem(inherited GetItem(Index));
end;

function TgxGizmoUndoCollection.GetParent: TgxGizmo;
begin
  Result := TgxGizmo(GetOwner);
end;

procedure TgxGizmoUndoCollection.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  I: Integer;
begin
  if Count <> 0 then
    for I := 0 to Count - 1 do
      GetItems(I).Notification(AComponent, Operation);
end;

procedure TgxGizmoUndoCollection.RemoveByObject(const AObject
  : TgxCustomSceneObject);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if GetItems(I).FEffectedObject = AObject then
      GetItems(I).Free;
end;

procedure TgxGizmoUndoCollection.SetItems(const Index: Integer;
  const Value: TgxGizmoUndoItem);
begin
  GetItems(Index).Assign(Value);
end;

procedure TgxGizmo.SetSelectedObj(const Value: TgxBaseSceneObject);
begin
  if FSelectedObj <> Value then
  begin
    FSelectedObj := Value;

    if Value <> nil then
    begin
      SetVisible(True);
      UpdateVisibleInfoLabels();
      UpdateGizmo();
    end
    else
    begin
      LooseSelection();
      SetVisible(False);
    end;
  end;
end;

end.
