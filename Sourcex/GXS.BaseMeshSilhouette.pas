//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.BaseMeshSilhouette;

(* Silhouette classes for BaseMesh and FaceGroups *)

interface

{$I GXS.Scene.inc}

uses
  System.Classes,
  GXS.VectorGeometry,
  GXS.VectorLists,
  GXS.VectorFileObjects,
  GXS.Silhouette;

type
  TgxFaceGroupConnectivity = class(TConnectivity)
  private
    FMeshObject: TgxMeshObject;
    FOwnsVertices: boolean;
    procedure SetMeshObject(const Value: TgxMeshObject);
  public
    procedure Clear; override;
    //  Builds the connectivity information. 
    procedure RebuildEdgeList;
    property MeshObject: TgxMeshObject read FMeshObject write SetMeshObject;
    constructor Create(APrecomputeFaceNormal: boolean); override;
    constructor CreateFromMesh(aMeshObject: TgxMeshObject; APrecomputeFaceNormal: boolean);
    destructor Destroy; override;
  end;

  TgxBaseMeshConnectivity = class(TBaseConnectivity)
  private
    FBaseMesh: TgxBaseMesh;
    FFaceGroupConnectivityList: TList;
    function GetFaceGroupConnectivity(i: integer): TgxFaceGroupConnectivity;
    function GetConnectivityCount: integer;
    procedure SetBaseMesh(const Value: TgxBaseMesh);
  protected
    function GetEdgeCount: integer; override;
    function GetFaceCount: integer; override;
  public
    property ConnectivityCount: integer read GetConnectivityCount;
    property FaceGroupConnectivity[i: integer]: TgxFaceGroupConnectivity read GetFaceGroupConnectivity;
    property BaseMesh: TgxBaseMesh read FBaseMesh write SetBaseMesh;
    procedure Clear(SaveFaceGroupConnectivity: boolean);
    // Builds the connectivity information. 
    procedure RebuildEdgeList;
    procedure CreateSilhouette(const SilhouetteParameters: TgxSilhouetteParameters; var aSilhouette: TgxSilhouette;
	  AddToSilhouette: boolean); 
    constructor Create(APrecomputeFaceNormal: boolean); override;
    constructor CreateFromMesh(aBaseMesh: TgxBaseMesh);
    destructor Destroy; override;
  end;

//==================================================================
implementation
//==================================================================

// ------------------
// ------------------ TgxFaceGroupConnectivity ------------------
// ------------------

procedure TgxFaceGroupConnectivity.Clear;
  begin
    if Assigned(FVertices) then
    begin
      if FOwnsVertices then
        FVertices.Clear
      else
        FVertices := nil;

      inherited;

      if not FOwnsVertices and Assigned(FMeshObject) then
        FVertices := FMeshObject.Vertices;
    end
    else
      inherited;
  end;

constructor TgxFaceGroupConnectivity.Create(APrecomputeFaceNormal: boolean);
  begin
    inherited;
    FOwnsVertices := true;
  end;

procedure TgxFaceGroupConnectivity.SetMeshObject(const Value: TgxMeshObject);
  begin
    Clear;
    FMeshObject := Value;
    if FOwnsVertices then
      FVertices.Free;
    FVertices := FMeshObject.Vertices;
    FOwnsVertices := false;
    RebuildEdgeList;
  end;

constructor TgxFaceGroupConnectivity.CreateFromMesh(aMeshObject: TgxMeshObject; APrecomputeFaceNormal: boolean);
  begin
    Create(APrecomputeFaceNormal);
    MeshObject := aMeshObject;
  end;

destructor TgxFaceGroupConnectivity.Destroy;
  begin
    if FOwnsVertices then
      FVertices.Free;
    FVertices := nil;
    inherited;
  end;

procedure TgxFaceGroupConnectivity.RebuildEdgeList;
  var
    iFaceGroup, iFace, iVertex: integer;
    FaceGroup: TfgxVertexIndexList;
    List: PIntegerArray;
  begin
    // Make sure that the connectivity information is empty
    Clear;
    // Create a list of edges for the meshobject
    for iFaceGroup := 0 to FMeshObject.FaceGroups.Count - 1 do
    begin
      Assert(FMeshObject.FaceGroups[iFaceGroup] is TfgxVertexIndexList, 'Method only works for descendants of TfgxVertexIndexList.');
      FaceGroup := TfgxVertexIndexList(FMeshObject.FaceGroups[iFaceGroup]);
      case FaceGroup.Mode of
        fgmmTriangles, fgmmFlatTriangles:
          begin
            for iFace := 0 to FaceGroup.TriangleCount - 1 do
            begin
              List := @FaceGroup.VertexIndices.List[iFace * 3 + 0];
              AddIndexedFace(List^[0], List^[1], List^[2]);
            end;
          end;
        fgmmTriangleStrip:
          begin
            for iFace := 0 to FaceGroup.VertexIndices.Count - 3 do
            begin
              List := @FaceGroup.VertexIndices.List[iFace];
              if (iFace and 1) = 0 then
                AddIndexedFace(List^[0], List^[1], List^[2])
              else
                AddIndexedFace(List^[2], List^[1], List^[0]);
            end;
          end;
        fgmmTriangleFan:
          begin
            List := FaceGroup.VertexIndices.List;

            for iVertex := 2 to FaceGroup.VertexIndices.Count - 1 do
              AddIndexedFace(List^[0], List^[iVertex - 1], List^[iVertex])
          end;
      else
        Assert(false, 'Not supported');
      end;
    end;
  end;

// ------------------
// ------------------ TgxBaseMeshConnectivity ------------------
// ------------------

procedure TgxBaseMeshConnectivity.RebuildEdgeList;
  var
    i: integer;
  begin
    for i := 0 to ConnectivityCount - 1 do
      FaceGroupConnectivity[i].RebuildEdgeList;
  end;

procedure TgxBaseMeshConnectivity.Clear(SaveFaceGroupConnectivity: boolean);
  var
    i: integer;
  begin
    if SaveFaceGroupConnectivity then
    begin
      for i := 0 to ConnectivityCount - 1 do
        FaceGroupConnectivity[i].Clear;
    end
    else
    begin
      for i := 0 to ConnectivityCount - 1 do
        FaceGroupConnectivity[i].Free;
      FFaceGroupConnectivityList.Clear;
    end;
  end;

constructor TgxBaseMeshConnectivity.Create(APrecomputeFaceNormal: boolean);
  begin
    FFaceGroupConnectivityList := TList.Create;
    inherited;
  end;

constructor TgxBaseMeshConnectivity.CreateFromMesh(aBaseMesh: TgxBaseMesh);
  begin
    Create(not(aBaseMesh is TgxActor));
    BaseMesh := aBaseMesh;
  end;

procedure TgxBaseMeshConnectivity.SetBaseMesh(const Value: TgxBaseMesh);
  var
    i: integer;
    MO: TgxMeshObject;
    Connectivity: TgxFaceGroupConnectivity;
  begin
    Clear(false);
    FBaseMesh := Value;
    // Only precompute normals if the basemesh isn't an actor (because they change)
    FPrecomputeFaceNormal := not(Value is TgxActor);
    FBaseMesh := Value;
    for i := 0 to Value.MeshObjects.Count - 1 do
    begin
      MO := Value.MeshObjects[i];
      if MO.Visible then
      begin
        Connectivity := TgxFaceGroupConnectivity.CreateFromMesh(MO, FPrecomputeFaceNormal);
        FFaceGroupConnectivityList.Add(Connectivity);
      end;
    end;
  end;

procedure TgxBaseMeshConnectivity.CreateSilhouette(const SilhouetteParameters: TgxSilhouetteParameters; var aSilhouette: TgxSilhouette; AddToSilhouette: boolean);
var
  i: integer;
begin
  if aSilhouette = nil then
    aSilhouette := TgxSilhouette.Create
  else
    aSilhouette.Flush;

  for i := 0 to ConnectivityCount - 1 do
    FaceGroupConnectivity[i].CreateSilhouette(silhouetteParameters, aSilhouette, true);
end;

destructor TgxBaseMeshConnectivity.Destroy;
  begin
    Clear(false);
    FFaceGroupConnectivityList.Free;
    inherited;
  end;

function TgxBaseMeshConnectivity.GetConnectivityCount: integer;
  begin
    result := FFaceGroupConnectivityList.Count;
  end;

function TgxBaseMeshConnectivity.GetEdgeCount: integer;
  var
    i: integer;
  begin
    result := 0;
    for i := 0 to ConnectivityCount - 1 do
      result := result + FaceGroupConnectivity[i].EdgeCount;
  end;

function TgxBaseMeshConnectivity.GetFaceCount: integer;
  var
    i: integer;
  begin
    result := 0;
    for i := 0 to ConnectivityCount - 1 do
      result := result + FaceGroupConnectivity[i].FaceCount;
  end;

function TgxBaseMeshConnectivity.GetFaceGroupConnectivity(i: integer): TgxFaceGroupConnectivity;
  begin
    result := TgxFaceGroupConnectivity(FFaceGroupConnectivityList[i]);
  end;

end.
