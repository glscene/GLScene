//
// The multimedia graphics platform GLScene https://github.com/glscene
//

unit GLS.Gizmo;

(*
  Invisible component for helping to Move, Rotate and Scale an Object
  under GLScene (usefull for an Editor). 
*)
//
// Original Header:
//
// ------------------------------------------------------------------------------
// Unit : GLS.Gizmo  RC 1.0
// ------------------------------------------------------------------------------
// Original Author : ???????  (GLS.Gizmo In an ODEEditor)
// ------------------------------------------------------------------------------
// Modified by     : J.Delauney
// Web Site        : http://KheopsInteractive.cjb.net
// EMail           : wmkheops@free.fr
// Date            : 08/05/2005
//
// Modified by     : Marcus Oblak (8/3/2007)
// - Corrected moving/rotating for children objects
// - Better quantization for mouse operations (MoveCoef,RotationCoef)
// - Added ScaleCoef
// - Added GizmoThickness
//
// If you make some changes, please send your new version. Thanks
// ------------------------------------------------------------------------------
// Description :
// Invisible component for helping to Move, Rotate and Scale an Object
// under GLScene (usefull for an Editor)
// ------------------------------------------------------------------------------
// Features :
// - Interaction When All Gizmo parts are Invisible
// - Add "gpMoveGizmo and  gpRotateGizmo" operations and use Like a "Pivot"
// or use RootGizmo As "Pivot"
// - Add Interactive Camera Movements
// - Adding Extended Controls with Keys
// - Maybe An Undo Function
// - Others Ideas ???
// ------------------------------------------------------------------------------
// Bugs Known :
// - When you change the BoundingBoxColor and LabelInfosColor
// The New Color is not Updated immediately, only after a new Click
// (see in UpdateGizmo, SetBoundingBoxColor
// and SetLabelInfosColor Procedures)
// -  DaStr: Bounding Box is not always drawn correctly because it does not
// use objects' BarryCenter. For Example, if you select Space Text.
// ------------------------------------------------------------------------------

interface

{$I GLScene.inc}

uses
  System.Classes,
  System.SysUtils,
  Vcl.StdCtrls,

  GLS.Scene,
  GLS.PersistentClasses,
  GLS.Color,
  GLS.Objects,
  GLS.VectorGeometry,
  GLS.Material,
  GLS.Strings,
  GLS.GeomObjects,
  GLS.BitmapFont,
  GLS.SceneViewer,
  GLS.VectorFileObjects,
  GLS.Coordinates,
  GLS.RenderContextInfo,
  GLS.State,
  GLS.Selection,
  GLS.VectorTypes;

type
  TGLGizmoUndoCollection = class;
  TGLGizmo = class;

  TGLGizmoUndoItem = class(TCollectionItem)
  private
    FOldLibMaterialName: string;
    FOldAutoScaling: TGLCoordinates;
    FEffectedObject: TGLCustomSceneObject;
    FOldMatr: TGLMatrix;
    FOldMatrix: TGLMatrix;
    procedure SetEffectedObject(const Value: TGLCustomSceneObject);
    procedure SetOldAutoScaling(const Value: TGLCoordinates);
    procedure SetOldMatrix(const Value: TGLMatrix);
  protected
    procedure DoUndo; virtual;
    function GetParent: TGLGizmoUndoCollection;
    function GetGizmo: TGLGizmo;
  public
    constructor Create(AOwner: TCollection); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); virtual;
    procedure AssignFromObject(const AObject: TGLCustomSceneObject);
    // TODO: create a special type for Matrix.
    property OldMatrix: TGLMatrix read FOldMatrix write SetOldMatrix;
  published
    property EffectedObject: TGLCustomSceneObject read FEffectedObject
      write SetEffectedObject;
    property OldAutoScaling: TGLCoordinates read FOldAutoScaling
      write SetOldAutoScaling;
    property OldLibMaterialName: string read FOldLibMaterialName
      write FOldLibMaterialName;
  end;

  TGLGizmoUndoCollection = class(TOwnedCollection)
  private
    function GetItems(const Index: Integer): TGLGizmoUndoItem;
    procedure SetItems(const Index: Integer; const Value: TGLGizmoUndoItem);
  protected
    function GetParent: TGLGizmo;
  public
    procedure Notification(AComponent: TComponent; Operation: TOperation);
    procedure RemoveByObject(const AObject: TGLCustomSceneObject);
    function Add: TGLGizmoUndoItem;
    property Items[const Index: Integer]: TGLGizmoUndoItem read GetItems
      write SetItems; default;
  end;

  TGLGizmoElement = (geMove, geRotate, geScale, geAxisLabel, geObjectInfos,
    geBoundingBox);
  TGLGizmoElements = set of TGLGizmoElement;

  TGLGizmoVisibleInfoLabel = (vliName, vliOperation, vliCoords);
  TGLGizmoVisibleInfoLabels = set of TGLGizmoVisibleInfoLabel;

  TGLGizmoAxis = (gaNone, gaX, gaY, gaZ, gaXY, gaXZ, gaYZ);

  TGLGizmoOperation = (gopMove, gopRotate, gopScale, gopNone, gpMoveGizmo,
    gpRotateGizmo);

  TGLGizmoAcceptEvent = procedure(Sender: TObject; var Obj: TGLBaseSceneObject;
    var Accept: Boolean; var Dimensions: TGLVector) of object;
  TGLGizmoUpdateEvent = procedure(Sender: TObject; Obj: TGLBaseSceneObject;
    Axis: TGLGizmoAxis; Operation: TGLGizmoOperation; var Vector: TGLVector)
    of object;

  TGLGizmoPickMode = (pmGetPickedObjects, pmRayCast);

  TGLGizmoRayCastHitData = class(TPersistent)
  public
    Obj: TGLBaseSceneObject;
    Point: TGLVector;
  end;

  TGLGizmoPickCube = class(TGLCube)
  end;

  TGLGizmoPickTorus = class(TGLTorus)
  end;

  TGLGizmo = class(TComponent)
  private
    _GZObaseGizmo: TGLBaseSceneObject;
    _GZOBoundingcube: TGLCube;
    _GZOrootHelpers: TGLBaseSceneObject;
    _GZOrootLines: TGLBaseSceneObject;
    _GZOrootTorus: TGLBaseSceneObject;
    _GZOrootCubes: TGLBaseSceneObject;
    _GZORootAxisLabel: TGLBaseSceneObject;
    _GZORootVisibleInfoLabels: TGLBaseSceneObject;
    _GZOlineX, _GZOlineY, _GZOlineZ, _GZOplaneXY, _GZOplaneXZ,
      _GZOplaneYZ: TGLLines; // For Move
    _GZOTorusX, _GZOTorusY, _GZOTorusZ: TGLGizmoPickTorus; // For Rotate
    _GZOCubeX, _GZOCubeY, _GZOCubeZ: TGLGizmoPickCube; // For Scale
    _GZOAxisLabelX, _GZOAxisLabelY, _GZOAxisLabelZ: TGLFlatText;
    _GZOVisibleInfoLabels: TGLFlatText;
    FRootGizmo: TGLBaseSceneObject;
    FSelectedObj: TGLBaseSceneObject;
    // FLastOperation,
    FOperation: TGLGizmoOperation;
    FSelAxis: TGLGizmoAxis;
    FBoundingBoxColor: TGLColor;
    FSelectedColor: TGLColor;
    FVisibleInfoLabelsColor: TGLColor;
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
    FViewer: TGLSceneViewer;
    FGizmoElements: TGLGizmoElements;
    FVisibleVisibleInfoLabels: TGLGizmoVisibleInfoLabels;
    FExcludeObjectsList: TStrings;
    Moving: Boolean;
    Mx, My: Integer;
    Rx, Ry: Integer;
    dglEnable, dglDisable, dgtEnable, dgtDisable, dgcEnable, dgcDisable,
      dglaEnable, dglaDisable, dgliEnable, dgliDisable: TGLDirectOpenGL;
    LastMousePos: TGLVector;
    ObjDimensions: TGLVector;
    FOnBeforeSelect: TGLGizmoAcceptEvent;
    FOnBeforeUpdate: TGLGizmoUpdateEvent;
    FOnSelectionLost: TNotifyEvent;
    FScaleCoef: Single;
    FGizmoThickness: Single;
    FPickMode: TGLGizmoPickMode;
    FInternalRaycastHitData: TList;
    FUndoHistory: TGLGizmoUndoCollection;
    FLabelFont: TGLCustomBitmapFont;
    procedure SetRootGizmo(const AValue: TGLBaseSceneObject);
    procedure SetGizmoElements(const AValue: TGLGizmoElements);
    procedure SeTGLGizmoVisibleInfoLabels(const AValue
      : TGLGizmoVisibleInfoLabels);
    procedure SetBoundingBoxColor(const AValue: TGLColor);
    procedure SetSelectedColor(const AValue: TGLColor);
    procedure SetVisibleInfoLabelsColor(const AValue: TGLColor);
    procedure SetExcludeObjectsList(const AValue: TStrings);
    procedure DirectGlDisable(Sender: TObject; var Rci: TGLRenderContextInfo);
    procedure DirectGlEnable(Sender: TObject; var Rci: TGLRenderContextInfo);
    function MouseWorldPos(const X, Y: Integer): TGLVector;
    function CheckObjectInExcludeList(const Obj: TGLBaseSceneObject): Boolean;
    procedure UpdateVisibleInfoLabels;
    procedure SetGLGizmoThickness(const Value: Single);
    function InternalGetPickedObjects(const X1, Y1, X2, Y2: Integer;
      const GuessCount: Integer = 8): TGLPickList;
    procedure ClearInternalRaycastHitData;
    procedure SetViewer(const Value: TGLSceneViewer);
    procedure SetLabelFont(const Value: TGLCustomBitmapFont);
    procedure SetSelectedObj(const Value: TGLBaseSceneObject);
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
    procedure UpdateGizmo(const NewDimensions: TGLVector); overload;
    procedure SetVisible(const AValue: Boolean);
    function GetPickedObjectPoint(const Obj: TGLBaseSceneObject): TGLVector;
    procedure LooseSelection; virtual;
    procedure UndoAdd(const AObject: TGLCustomSceneObject);
    property RootGizmo: TGLBaseSceneObject read FRootGizmo write SetRootGizmo;
    // --------------------------------------------------------------------
  published
    property Viewer: TGLSceneViewer read FViewer write SetViewer;
    property GizmoElements: TGLGizmoElements read FGizmoElements
      write SetGizmoElements;
    property BoundingBoxColor: TGLColor read FBoundingBoxColor
      write SetBoundingBoxColor;
    property SelectedColor: TGLColor read FSelectedColor write SetSelectedColor;
    property SelAxis: TGLGizmoAxis read FSelAxis write FSelAxis;
    property ForceAxis: Boolean read FForceAxis write FForceAxis;
    property SelectedObj: TGLBaseSceneObject read FSelectedObj
      write SetSelectedObj;
    property Operation: TGLGizmoOperation read FOperation write FOperation;
    property ForceOperation: Boolean read FForceOperation write FForceoperation;
    property ForceUniformScale: Boolean read FForceUniformScale
      write FForceUniformScale;
    property ExcludeObjects: Boolean read FExcludeObjects write FExcludeObjects;
    property ExcludeObjectsList: TStrings read FExcludeObjectsList
      write SetExcludeObjectsList;
    property VisibleInfoLabels: TGLGizmoVisibleInfoLabels
      read FVisibleVisibleInfoLabels write SeTGLGizmoVisibleInfoLabels;
    property VisibleInfoLabelsColor: TGLColor read FVisibleInfoLabelsColor
      write SetVisibleInfoLabelsColor;
    property AutoZoom: Boolean read FAutoZoom write FAutoZoom;
    property AutoZoomFactor: Single read FAutoZoomFactor write FAutoZoomFactor;
    property ZoomFactor: Single read FZoomFactor write FZoomFactor;
    property MoveCoef: Single read FMoveCoef write FMoveCoef;
    property RotationCoef: Single read FRotationCoef write FRotationCoef;
    property ScaleCoef: Single read FScaleCoef write FScaleCoef;
    property NoZWrite: Boolean read FNoZWrite write FNoZWrite;
    property GizmoThickness: Single read FGizmoThickness
      write SeTGLGizmoThickness;
    {  Indicates whether the gizmo is enabled or not.
      WARNING: When loading/editing (possibly whenever a structureChanged
      call is made) a model, sometimes the gizmo will trigger a
      bug if the mouse is inside the glscene Viewer. To prevent that,
      remember to disable the gizmo before loading, then process windows
      messages (i.e. application.processMessage) and then enable the gizmo
      again. }
    {  Warning Enable is ReadOnly property if you set to False, Gizmo is not Hidden
      use Visible instead if you want to Hide, if you want to Hide but keep enabled
      see the VisibleGizmo property }
    property Enabled: Boolean read FEnabled write FEnabled default False;
    property LabelFont: TGLCustomBitmapFont read FLabelFont write SetLabelFont
      default nil;
    property OnBeforeSelect: TGLGizmoAcceptEvent read FOnBeforeSelect
      write FOnBeforeSelect;
    property OnSelectionLost: TNotifyEvent read FOnSelectionLost
      write FOnSelectionLost;
    {  Called before an Update is applied. The "vector" parameter is the difference
      that will be applied to the object, according to the axis and
      operation selected. }
    property OnBeforeUpdate: TGLGizmoUpdateEvent read FOnBeforeUpdate
      write FOnBeforeUpdate;
    property PickMode: TGLGizmoPickMode read FPickMode write FPickMode
      default PmGetPickedObjects;
  end;

//=========================================================
implementation
//=========================================================

procedure RotateAroundArbitraryAxis(const AnObject: TGLBaseSceneObject;
  const Axis, Origin: TAffineVector; const Angle: Single);
var
  M, M1, M2, M3: TGLMatrix;
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

procedure TGLGizmo.ClearInternalRaycastHitData;
var
  T: Integer;
begin
  for T := FInternalRaycastHitData.Count - 1 downto 0 do
  begin
    TGLGizmoRayCastHitData(FInternalRaycastHitData[T]).Free;
  end;
  FInternalRaycastHitData.Clear;
end;

constructor TGLGizmo.Create(AOwner: TComponent);
var
  Cub: TGLCube;
begin
  inherited Create(AOwner);
  FUndoHistory := TGLGizmoUndoCollection.Create(Self, TGLGizmoUndoItem);
  FPickMode := PmGetPickedObjects;
  PickableObjectsWithRayCast := TList.Create;
  FRotationCoef := 1;
  FMoveCoef := 0.1;
  FScaleCoef := 0.1;
  FGizmoThickness := 1;

  FInternalRaycastHitData := TList.Create;
  FBoundingBoxColor := TGLColor.Create(Self);
  FBoundingBoxColor.Color := ClrWhite;
  FBoundingBoxColorChanged := False;
  FSelectedColor := TGLColor.Create(Self);
  FSelectedColor.Color := ClrYellow;
  FVisibleInfoLabelsColor := TGLColor.Create(Self);
  FVisibleInfoLabelsColor.Color := ClrYellow;
  FVisibleInfoLabelsColorChanged := False;

  _GZObaseGizmo := TGLDummyCube.Create(Self);
  _GZORootHelpers := TGLDummyCube(_GZObaseGizmo.AddNewChild(TGLDummyCube));
  _GZOBoundingcube := TGLCube(_GZORootHelpers.AddNewChild(TGLCube));

  _GZORootLines := _GZORootHelpers.AddNewChild(TGLDummyCube);
  _GZORootTorus := _GZORootHelpers.AddNewChild(TGLDummyCube);
  _GZORootCubes := _GZORootHelpers.AddNewChild(TGLDummyCube);
  _GZORootAxisLabel := _GZORootHelpers.AddNewChild(TGLDummyCube);
  _GZORootVisibleInfoLabels := _GZORootHelpers.AddNewChild(TGLDummyCube);

  DglDisable := TGLDirectOpenGL(_GZORootLines.AddNewChild(TGLDirectOpenGL));
  DglDisable.OnRender := DirectGlDisable;
  DgtDisable := TGLDirectOpenGL(_GZORootTorus.AddNewChild(TGLDirectOpenGL));
  DgtDisable.OnRender := DirectGlDisable;
  DgcDisable := TGLDirectOpenGL(_GZORootCubes.AddNewChild(TGLDirectOpenGL));
  DgcDisable.OnRender := DirectGlDisable;
  DglaDisable := TGLDirectOpenGL
    (_GZORootAxisLabel.AddNewChild(TGLDirectOpenGL));
  DglaDisable.OnRender := DirectGlDisable;
  DgliDisable := TGLDirectOpenGL(_GZORootVisibleInfoLabels.AddNewChild
    (TGLDirectOpenGL));
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

  _GZOlinex := TGLLines(_GZORootLines.AddnewChild(TGLLines));
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
    Cub := TGLGizmoPickCube(AddNewChild(TGLGizmoPickCube));
    Cub.Up.SetVector(1, 0, 0);
    Cub.CubeWidth := 0.1;
    Cub.CubeHeight := 1;
    Cub.CubeDepth := 0.1;
    Cub.Position.SetPoint(0.5, 0, 0);
    Cub.Visible := False;
  end;

  _GZOliney := TGLLines(_GZORootLines.AddnewChild(TGLLines));
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
    Cub := TGLGizmoPickCube(AddNewChild(TGLGizmoPickCube));
    Cub.Up.SetVector(0, 1, 0);
    Cub.CubeWidth := 0.1;
    Cub.CubeHeight := 1;
    Cub.CubeDepth := 0.1;
    Cub.Position.SetPoint(0, 0.5, 0);
    Cub.Visible := False;
  end;

  _GZOlinez := TGLLines(_GZORootLines.AddnewChild(TGLLines));
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
    Cub := TGLGizmoPickCube(AddNewChild(TGLGizmoPickCube));
    Cub.Up.SetVector(0, 0, 1);
    Cub.CubeWidth := 0.1;
    Cub.CubeHeight := 1;
    Cub.CubeDepth := 0.1;
    Cub.Position.SetPoint(0, 0, 0.5);
    Cub.Visible := False;
  end;

  _GZOplaneXY := TGLLines(_GZORootLines.AddnewChild(TGLLines));
  with _GZOplaneXY do
  begin
    LineWidth := 3;
    Options := [LoUseNodeColorForLines];
    NodesAspect := LnaInvisible;
    SplineMode := LsmSegments;
    AddNode(0.8, 1, 0);
    TGLLinesNode(Nodes[0]).Color.Color := clrRed;
    AddNode(1, 1, 0);
    TGLLinesNode(Nodes[1]).Color.Color := clrRed;
    AddNode(1, 1, 0);
    TGLLinesNode(Nodes[2]).Color.Color := clrLime;
    AddNode(1, 0.8, 0);
    TGLLinesNode(Nodes[3]).Color.Color := clrLime;
    // Raycast pickable object
    Cub := TGLGizmoPickCube(AddNewChild(TGLGizmoPickCube));
    Cub.Up.SetVector(1, 0, 0);
    Cub.CubeWidth := 0.2;
    Cub.CubeHeight := 0.2;
    Cub.CubeDepth := 0.1;
    Cub.Position.SetPoint(0.9, 0.9, 0);
    Cub.Visible := False;
  end;

  _GZOplaneXZ := TGLLines(_GZORootLines.AddnewChild(TGLLines));
  with _GZOplaneXZ do
  begin
    LineWidth := 3;
    Options := [LoUseNodeColorForLines];
    NodesAspect := LnaInvisible;
    SplineMode := LsmSegments;
    AddNode(1, 0, 0.8);
    TGLLinesNode(Nodes[0]).Color.Color := clrBlue;
    AddNode(1, 0, 1);
    TGLLinesNode(Nodes[1]).Color.Color := clrBlue;
    AddNode(1, 0, 1);
    TGLLinesNode(Nodes[2]).Color.Color := clrRed;
    AddNode(0.8, 0, 1);
    TGLLinesNode(Nodes[3]).Color.Color := clrRed;
    // Raycast pickable object
    Cub := TGLGizmoPickCube(AddNewChild(TGLGizmoPickCube));
    Cub.Up.SetVector(1, 0, 0);
    Cub.CubeWidth := 0.1;
    Cub.CubeHeight := 0.2;
    Cub.CubeDepth := 0.2;
    Cub.Position.SetPoint(0.9, 0, 0.9);
    Cub.Visible := False;
  end;

  _GZOplaneYZ := TGLLines(_GZORootLines.AddnewChild(TGLLines));
  with _GZOplaneYZ do
  begin
    LineWidth := 3;
    Options := [LoUseNodeColorForLines];
    NodesAspect := LnaInvisible;
    SplineMode := LsmSegments;
    AddNode(0, 0.8, 1);
    TGLLinesNode(Nodes[0]).Color.Color := clrLime;
    AddNode(0, 1, 1);
    TGLLinesNode(Nodes[1]).Color.Color := clrLime;
    AddNode(0, 1, 1);
    TGLLinesNode(Nodes[2]).Color.Color := clrBlue;
    AddNode(0, 1, 0.8);
    TGLLinesNode(Nodes[3]).Color.Color := clrBlue;
    // Raycast pickable object
    Cub := TGLGizmoPickCube(AddNewChild(TGLGizmoPickCube));
    Cub.Up.SetVector(0, 0, 1);
    Cub.CubeWidth := 0.2;
    Cub.CubeHeight := 0.2;
    Cub.CubeDepth := 0.1;
    Cub.Position.SetPoint(0, 0.9, 0.9);
    Cub.Visible := False;
  end;

  _GZOTorusX := TGLGizmoPickTorus(_GZORootTorus.AddnewChild(TGLGizmoPickTorus));
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

  _GZOTorusY := TGLGizmoPickTorus(_GZORootTorus.AddnewChild(TGLGizmoPickTorus));
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

  _GZOTorusZ := TGLGizmoPickTorus(_GZORootTorus.AddnewChild(TGLGizmoPickTorus));
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

  _GZOCubeX := TGLGizmoPickCube(_GZORootCubes.AddnewChild(TGLGizmoPickCube));
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

  _GZOCubeY := TGLGizmoPickCube(_GZORootCubes.AddnewChild(TGLGizmoPickCube));
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

  _GZOCubeZ := TGLGizmoPickCube(_GZORootCubes.AddnewChild(TGLGizmoPickCube));
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

  _GZOAxisLabelX := TGLFlatText(_GZORootAxisLabel.AddNewChild(TGLFlatText));
  with _GZOAxisLabelX do
  begin
    ModulateColor.Color := ClrRed;
    Alignment := TaCenter;
    Layout := TTextLayout.tlCenter;
    Options := Options + [FtoTwoSided];
    Position.X := 1.5;
    Scale.X := 0.02;
    Scale.Y := 0.02;
    Text := 'X';
  end;

  _GZOAxisLabelY := TGLFlatText(_GZORootAxisLabel.AddNewChild(TGLFlatText));
  with _GZOAxisLabelY do
  begin
    ModulateColor.Color := clrLime;
    Alignment := TaCenter;
    Layout := TlCenter;
    Options := Options + [FtoTwoSided];
    Position.Y := 1.5;
    Scale.X := 0.02;
    Scale.Y := 0.02;
    Text := 'Y';
  end;

  _GZOAxisLabelZ := TGLFlatText(_GZORootAxisLabel.AddNewChild(TGLFlatText));
  with _GZOAxisLabelZ do
  begin
    ModulateColor.Color := ClrBlue;
    Alignment := TaCenter;
    Layout := TlCenter;
    Options := Options + [FtoTwoSided];
    Position.Z := 1.5;
    Scale.X := 0.02;
    Scale.Y := 0.02;
    Text := 'Z';
  end;

  _GZOVisibleInfoLabels :=
    TGLFlatText(_GZORootVisibleInfoLabels.AddNewChild(TGLFlatText));
  with _GZOVisibleInfoLabels do
  begin
    ModulateColor.Color := clrYellow;
    Alignment := TaCenter;
    Layout := TlCenter;
    Options := Options + [FtoTwoSided];
    Position.Y := 1.8;
    Position.X := 0;
    Scale.X := 0.01;
    Scale.Y := 0.01;
    Text := '';
  end;

  DglEnable := TGLDirectOpenGL(_GZORootLines.AddNewChild(TGLDirectOpenGL));
  DglEnable.OnRender := DirectGlEnable;
  DgtEnable := TGLDirectOpenGL(_GZORootTorus.AddNewChild(TGLDirectOpenGL));
  DgtEnable.OnRender := DirectGlEnable;
  DgcEnable := TGLDirectOpenGL(_GZORootCubes.AddNewChild(TGLDirectOpenGL));
  DgcEnable.OnRender := DirectGlEnable;
  DglaEnable := TGLDirectOpenGL(_GZORootAxisLabel.AddNewChild(TGLDirectOpenGL));
  DglaEnable.OnRender := DirectGlEnable;
  DgliEnable := TGLDirectOpenGL(_GZORootVisibleInfoLabels.AddNewChild
    (TGLDirectOpenGL));
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

destructor TGLGizmo.Destroy;
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

procedure TGLGizmo.SetVisible(const AValue: Boolean);
begin
  _GZObaseGizmo.Visible := AValue;
end;

procedure TGLGizmo.SetGizmoElements(const AValue: TGLGizmoElements);
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

procedure TGLGizmo.SetBoundingBoxColor(const AValue: TGLColor);
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

procedure TGLGizmo.SetSelectedColor(const AValue: TGLColor);
begin
  if AValue <> FSelectedColor then
  begin
    FSelectedColor.Color := AValue.Color;
  end;
end;

procedure TGLGizmo.SetVisibleInfoLabelsColor(const AValue: TGLColor);
begin
  // Bug Here New Color is not Updated
  if AValue <> FSelectedColor then
  begin
    FVisibleInfoLabelsColor.Color := AValue.Color;
    _GZOVisibleInfoLabels.ModulateColor.Color := AValue.Color;
    FVisibleInfoLabelsColorChanged := True;
  end;
end;

procedure TGLGizmo.SeTGLGizmoVisibleInfoLabels(const AValue
  : TGLGizmoVisibleInfoLabels);
begin
  if AValue <> FVisibleVisibleInfoLabels then
  begin
    FVisibleVisibleInfoLabels := AValue;
    if not(CsDesigning in ComponentState) then
      UpdateGizmo;
  end;
end;

procedure TGLGizmo.UndoAdd(const AObject: TGLCustomSceneObject);
begin
  if AObject <> nil then
  begin
    FUndoHistory.Add.AssignFromObject(AObject)
  end;
end;

procedure TGLGizmo.SetRootGizmo(const AValue: TGLBaseSceneObject);
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

procedure TGLGizmo.SetExcludeObjectsList(const AValue: TStrings);
begin
  FExcludeObjectsList.Clear;
  FExcludeObjectsList.AddStrings(AValue);
end;

procedure TGLGizmo.SetGLGizmoThickness(const Value: Single);
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

procedure TGLGizmo.DirectGlDisable(Sender: TObject;
  var Rci: TGLRenderContextInfo);
begin
  if FNoZWrite then
    Rci.GLStates.Disable(StDepthTest);
end;

procedure TGLGizmo.SetLabelFont(const Value: TGLCustomBitmapFont);
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

procedure TGLGizmo.DirectGlEnable(Sender: TObject; var Rci: TGLRenderContextInfo);
begin
  if FNoZWrite then
    Rci.GLStates.Enable(StDepthTest);
end;

function TGLGizmo.GetPickedObjectPoint(const Obj: TGLBaseSceneObject): TGLVector;
var
  T: Integer;
  R: TGLGizmoRayCastHitData;
begin
  for T := 0 to FInternalRaycastHitData.Count - 1 do
  begin
    R := TGLGizmoRayCastHitData(FInternalRaycastHitData[T]);
    if R.Obj = Obj then
    begin
      Result := R.Point;
      Break;
    end;
  end;
end;

function TGLGizmo.InternalGetPickedObjects(const X1, Y1, X2, Y2: Integer;
  const GuessCount: Integer): TGLPickList;
var
  T: Integer;
  RayStart, RayVector, IPoint, INormal: TGLVector;
  O: TGLBaseSceneObject;
  Dist: Single;
  HitData: TGLGizmoRayCastHitData;

  procedure AddGizmosToPicklListRecurse(const Root: TGLBaseSceneObject);
  var
    U: Integer;
  begin
    for U := 0 to Root.Count - 1 do
    begin
      if ((Root[U] is TGLGizmoPickTorus) or (Root[U] is TGLGizmoPickCube)) then
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
        Result := TGLPickList.Create(PsMinDepth);
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
          O := TGLBaseSceneObject(PickableObjectsWithRayCast[T]);
          if (O.RayCastIntersect(RayStart, RayVector, @IPoint, @INormal)) and
            (VectorDotProduct(RayVector, INormal) < 0) then
          begin
            try
              Dist := VectorLength(VectorSubtract(IPoint, RayStart));
              Result.AddHit(O, nil, Dist, 0);
              HitData := TGLGizmoRayCastHitData.Create;
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

procedure TGLGizmo.Loaded;
begin
  inherited;
  SeTGLGizmoThickness(GizmoThickness);
end;

// ------------------------------------------------------------------------------
procedure TGLGizmo.UpdateVisibleInfoLabels;
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

function TGLGizmo.CheckObjectInExcludeList
  (const Obj: TGLBaseSceneObject): Boolean;
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

function TGLGizmo.MouseWorldPos(const X, Y: Integer): TGLVector;
var
  V: TGLVector;
  InvertedY: Integer;
begin
  InvertedY := Viewer.Height - Y;
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

procedure TGLGizmo.ViewerMouseMove(const X, Y: Integer);
var
  PickList: TGLPickList;
  MousePos: TGLVector;

  function IndexOf(Obj: TGLBaseSceneObject): Integer;
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

  function LightLine(const Line: TGLLines; const Dark: TGLVector;
    const Axis: TGLGizmoAxis; AlterStyle: Boolean = False): Boolean;
  var
    PickObj: TGLBaseSceneObject;
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

  function LightTorus(const Torus: TGLGizmoPickTorus; const Dark: TGLVector;
    const Axis: TGLGizmoAxis; AlterStyle: Boolean = False): Boolean;
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

  function LightCube(const Cube: TGLCube; const Dark: TGLVector;
    const Axis: TGLGizmoAxis; AlterStyle: Boolean = False): Boolean;
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

  procedure OpeMove(MousePos: TGLVector);
  var
    Vec1, Vec2: TGLVector;
    QuantizedMousePos, QuantizedMousePos2: TGLVector;
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
    Vec1: TGLVector;
    RotV: TAffineVector;
    Pmat: TGLMatrix;

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
    SetVector(Pmat.V[3], NullHmgPoint);
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

  procedure OpeScale(const MousePos: TGLVector);
  var
    Vec1, Vec2: TGLVector;
    QuantizedMousePos, QuantizedMousePos2: TGLVector;
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

procedure TGLGizmo.ViewerMouseDown(const X, Y: Integer);
var
  Pick: TGLPickList;
  I: Integer;
  Accept: Boolean;
  Dimensions: TGLVector;
  GotPick: Boolean;
  PickedObj: TGLBaseSceneObject;
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
          if (_GZOrootLines.IndexOfChild(TGLBaseSceneObject(Pick.Hit[I])) > -1)
            or (_GZOrootTorus.IndexOfChild(TGLBaseSceneObject(Pick.Hit[I])) >
            -1) or (_GZOrootCubes.IndexOfChild(TGLBaseSceneObject(Pick.Hit[I]))
            > -1) then
            GotPick := True;
      end;

    PmRayCast:
      begin
        for I := 0 to Pick.Count - 1 do
        begin
          if (Pick.Hit[I] is TGLGizmoPickCube) or
            (Pick.Hit[I] is TGLGizmoPickTorus) then
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
        not(CheckObjectInExcludeList(TGLBaseSceneObject(Pick.Hit[I]))) then
      begin
        Accept := True;
        PickedObj := TGLBaseSceneObject(Pick.Hit[I]);
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

procedure TGLGizmo.ViewerMouseUp(const X, Y: Integer);
begin
  Moving := False;
end;

// ------------------------------------------------------------------------------

procedure TGLGizmo.UpdateGizmo;
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

procedure TGLGizmo.UpdateGizmo(const NewDimensions: TGLVector);
begin
  ObjDimensions := NewDimensions;
  UpdateGizmo;
end;

procedure TGLGizmo.LooseSelection;
begin
  SelectedObj := nil;
  UpdateGizmo;
  if Assigned(OnSelectionLost) then
    OnSelectionLost(Self);
end;

procedure TGLGizmo.SetViewer(const Value: TGLSceneViewer);
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

procedure TGLGizmo.Notification(AComponent: TComponent; Operation: TOperation);
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

procedure TGLGizmoUndoItem.AssignFromObject(const AObject
  : TGLCustomSceneObject);
begin
  SetEffectedObject(AObject);
  SetOldMatrix(AObject.Matrix^);
  if AObject is TGLFreeForm then
  begin
    FOldAutoScaling.Assign(TGLFreeForm(AObject).AutoScaling);
  end;
  FOldLibMaterialName := AObject.Material.LibMaterialName;
end;

constructor TGLGizmoUndoItem.Create(AOwner: TCollection);
begin
  inherited;
  FOldAutoScaling := TGLCoordinates.CreateInitialized(Self,
    NullHmgVector, CsPoint);
end;

destructor TGLGizmoUndoItem.Destroy;
begin
  FOldAutoScaling.Free;
  inherited;
end;

procedure TGLGizmoUndoItem.DoUndo;
begin
  FEffectedObject.SetMatrix(FOldMatr);
  if FEffectedObject is TGLFreeForm then
    TGLFreeForm(FEffectedObject).AutoScaling.Assign(FOldAutoScaling);
  FEffectedObject.Material.LibMaterialName := FOldLibMaterialName;
end;

function TGLGizmoUndoItem.GetGizmo: TGLGizmo;
begin
  if GetParent <> nil then
    Result := GetPArent.GetParent
  else
    Result := nil;
end;

function TGLGizmoUndoItem.GetParent: TGLGizmoUndoCollection;
begin
  Result := TGLGizmoUndoCollection(GetOwner);
end;

procedure TGLGizmoUndoItem.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = OpRemove then
  begin
    if AComponent = FEffectedObject then
      FEffectedObject := nil;
  end;
end;

procedure TGLGizmoUndoItem.SetEffectedObject(const Value: TGLCustomSceneObject);
begin
  if FEffectedObject <> nil then
    FEffectedObject.RemoveFreeNotification(GetGizmo);
  FEffectedObject := Value;
  if FEffectedObject <> nil then
    FEffectedObject.FreeNotification(GetGizmo);
end;

procedure TGLGizmoUndoItem.SetOldAutoScaling(const Value: TGLCoordinates);
begin
  FOldAutoScaling.Assign(Value);
end;

procedure TGLGizmoUndoItem.SetOldMatrix(const Value: TGLMatrix);
begin
  FOldMatrix := Value;
end;

{ TGLGizmoUndoCollection }

function TGLGizmoUndoCollection.Add: TGLGizmoUndoItem;
begin
  Result := TGLGizmoUndoItem(inherited Add);
end;

function TGLGizmoUndoCollection.GetItems(const Index: Integer)
  : TGLGizmoUndoItem;
begin
  Result := TGLGizmoUndoItem(inherited GetItem(Index));
end;

function TGLGizmoUndoCollection.GetParent: TGLGizmo;
begin
  Result := TGLGizmo(GetOwner);
end;

procedure TGLGizmoUndoCollection.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  I: Integer;
begin
  if Count <> 0 then
    for I := 0 to Count - 1 do
      GetItems(I).Notification(AComponent, Operation);
end;

procedure TGLGizmoUndoCollection.RemoveByObject(const AObject
  : TGLCustomSceneObject);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if GetItems(I).FEffectedObject = AObject then
      GetItems(I).Free;
end;

procedure TGLGizmoUndoCollection.SetItems(const Index: Integer;
  const Value: TGLGizmoUndoItem);
begin
  GetItems(Index).Assign(Value);
end;

procedure TGLGizmo.SetSelectedObj(const Value: TGLBaseSceneObject);
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
