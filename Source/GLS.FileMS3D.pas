//
// The graphics rendering engine GLScene http://glscene.org
//

unit GLS.FileMS3D;

(* Support for MS3D file format. *)

interface

{$I GLScene.inc}

uses
  System.Math,
  System.Classes,
  System.SysUtils, 
   
  GLS.VectorFileObjects,
  GLS.VectorTypes, 
  GLS.Material, 
  GLS.Color, 
  GLS.Texture,
  GLS.VectorGeometry, 
  GLS.VectorLists, 
  GLS.ApplicationFileIO;
  

const
  MAX_MS3D_VERTICES  = 8192;
  MAX_MS3D_TRIANGLES = 16384;
  MAX_MS3D_GROUPS    = 128;
  MAX_MS3D_MATERIALS = 128;
  MAX_MS3D_JOINTS    = 128;
  MAX_MS3D_KEYFRAMES = 5000;

type
  // typedef struct
  // (*
  //     byte            flags;                              // SELECTED | HIDDEN
  //     char            name[32];                           //
  //     word            numtriangles;                       //
  //     word            triangleIndices[numtriangles];      // the groups group the triangles
  //     char            materialIndex;                      // -1 = no material
  // *) ms3d_group_t;
  TMS3DGroup = class
  public
    Flags: byte;
    Name: array[0..31] of AnsiChar;
    NumTriangles: word;
    TriangleIndices: TList;
    MaterialIndex: Shortint;
    constructor Create;
    destructor Destroy; override;
  end;

  // typdef struct
  // (*
  //    char    id[10];                                     // always "MS3D000000"
  //    int     version;                                    // 4
  // *) ms3d_header_t;
  {$A-}

  TMS3DHeader = record
    ID: array[0..9] of AnsiChar;
    Version: integer;
  end;


  // typedef struct
  // (*
  //     byte    flags;                                      // SELECTED | SELECTED2 | HIDDEN
  //     float   vertex[3];                                  //
  //     char    boneId;                                     // -1 = no bone
  //     byte    referenceCount;
  // *) ms3d_vertex_t;

  TMS3DVertex = record
    Flags: byte;
    Vertex: TD3DVector;
    BoneId: AnsiChar;
    ReferenceCount: byte;
  end;

  PMS3DVertexArray = ^TMS3DVertexArray;
  TMS3DVertexArray = array[0..MAX_MS3D_VERTICES - 1] of TMS3DVertex;

  // typedef struct
  // (*
  //     word    flags;                                      // SELECTED | SELECTED2 | HIDDEN
  //     word    vertexIndices[3];                           //
  //     float   vertexNormals[3][3];                        //
  //     float   s[3];                                       //
  //     float   t[3];                                       //
  //     byte    smoothingGroup;                             // 1 - 32
  //     byte    groupIndex;                                 //
  // *) ms3d_triangle_t;

  TMS3DTriangle = record
    Flags: word;
    VertexIndices: array[0..2] of word;
    VertexNormals: array[0..2] of TD3DVector;
    S: array[0..2] of single;
    T: array[0..2] of single;
    SmoothingGroup: byte;  // 1 - 32
    GroupIndex: byte;
  end;

  PMS3DTriangleArray = ^TMS3DTriangleArray;
  TMS3DTriangleArray = array[0..MAX_MS3D_TRIANGLES - 1] of TMS3DTriangle;

  // typedef struct
  // (*
  //     char            name[32];                           //
  //     float           ambient[4];                         //
  //     float           diffuse[4];                         //
  //     float           specular[4];                        //
  //     float           emissive[4];                        //
  //     float           shininess;                          // 0.0f - 128.0f
  //     float           transparency;                       // 0.0f - 1.0f
  //     char            mode;                               // 0, 1, 2 is unused now
  //     char            texture[128];                        // texture.bmp
  //     char            alphamap[128];                       // alpha.bmp
  // *) ms3d_material_t;
  TMS3DMaterial = record
    Name: array[0..31] of AnsiChar;
    Ambient: TGLColorVector;
    Diffuse: TGLColorVector;
    Specular: TGLColorVector;
    Emissive: TGLColorVector;
    Shininess: single;
    Transparency: single;
    Mode: AnsiChar;
    Texture: array[0..127] of AnsiChar;
    Alphamap: array[0..127] of AnsiChar;
  end;


  // typedef struct
  // (*
  //     float           time;                               // time in seconds
  //     float           rotation[3];                        // x, y, z angles
  // *) ms3d_keyframe_rot_t;
  TMS3DKeyframeRotation = record
    Time: single;
    Rotation: TD3DVector;
  end;

  PMS3DKeyframeRotationArray = ^TMS3DKeyframeRotationArray;
  TMS3DKeyframeRotationArray = array[0..MAX_MS3D_KEYFRAMES - 1] of TMS3DKeyframeRotation;

  // typedef struct
  // (*
  //     float           time;                               // time in seconds
  //     float           position[3];                        // local position
  // *) ms3d_keyframe_pos_t;
  TMS3DKeyframePosition = record
    Time: single;
    Position: TD3DVector;
  end;

  PMS3DKeyframePositionArray = ^TMS3DKeyframePositionArray;
  TMS3DKeyframePositionArray = array[0..MAX_MS3D_KEYFRAMES - 1] of TMS3DKeyframePosition;

  // typedef struct
  // (*
  //     byte            flags;                              // SELECTED | DIRTY
  //     char            name[32];                           //
  //     char            parentName[32];                     //
  //     float           rotation[3];                        // local reference matrix
  //     float           position[3];
  //
  //     word            numKeyFramesRot;                    //
  //     word            numKeyFramesTrans;                  //
  //
  //     ms3d_keyframe_rot_t keyFramesRot[numKeyFramesRot];      // local animation matrices
  //     ms3d_keyframe_pos_t keyFramesTrans[numKeyFramesTrans];  // local animation matrices
  // *) ms3d_joint_t;

  TMS3DJointBase = record
    Flags: byte;
    Name: array[0..31] of AnsiChar;
    ParentName: array[0..31] of AnsiChar;
    Rotation: TD3DVector;
    Position: TD3DVector;
    NumKeyFramesRot: word;
    NumKeyFramesTrans: word;
  end;

  TMS3DJoint = record
    Base : TMS3DJointBase;
    KeyFramesRot: PMS3DKeyframeRotationArray;
    KeyFramesTrans: PMS3DKeyframePositionArray;
  end;

  PMS3DJointArray = ^TMS3DJointArray;
  TMS3DJointArray = array[0..MAX_MS3D_JOINTS - 1] of TMS3DJoint;

  TMS3DComment=record
      index: Integer;
      commentLength: integer;
      comment: array of AnsiChar;
  end;
  pMS3DComment=^TMS3DComment;

  TMS3DCommentList=class(TList)
  private
  protected
  public
    destructor Destroy; override;
    function NewComment: pMS3DComment;
  end;

  TMS3D_vertex_ex_t=record
    boneIds: array[0..2] of byte;
    weights: array[0..2] of byte;
    extra: cardinal;
    unknown: cardinal;
  end;
  pMS3D_vertex_ex_t=^TMS3D_vertex_ex_t;


  TVertexWeightList=class(TList)
  private
    FsubVersion: Integer;
    function GetWeight(idx: Integer): pMS3D_vertex_ex_t;
    procedure SetsubVersion(const Value: Integer);
  protected
  public
    function newWeight: pMS3D_vertex_ex_t;
    procedure Clear; override;
    destructor Destroy; override;
    property Weight[idx: Integer]: pMS3D_vertex_ex_t read GetWeight;
    property subVersion: Integer read FsubVersion write SetsubVersion;
  end;

type
  (* The MilkShape vector file.
     By Mattias Fagerlund, mattias@cambrianlabs.com. Yada yada. Eric rules! *)
  TGLMS3DVectorFile = class(TGLVectorFile)
  public
    class function Capabilities: TGLDataFileCapabilities; override;
    procedure LoadFromStream(aStream: TStream); override;
  end;


  {$A+}

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------


// -------------------------
//  TMS3DGroup 
// -------------------------

constructor TMS3DGroup.Create;
begin
  TriangleIndices := TList.Create;
end;

destructor TMS3DGroup.Destroy;
begin
  TriangleIndices.Free;
  inherited;
end;

// -------------------------
//  TMS3DCommentList 
// -------------------------

destructor TMS3DCommentList.destroy;
var
    i: integer;
    comment: pMS3DComment;
begin
    for i:=0 to count-1 do begin
        comment:=items[i];
        comment^.comment:=nil;
        dispose(comment);
    end;
    inherited;
end;

function TMS3DCommentList.NewComment: pMS3DComment;
begin
    new(result);
    add(result);
end;

// -------------------------
// TVertexWeightList 
// -------------------------

procedure TVertexWeightList.clear;
var
    i: integer;
begin
    for i:=0 to count-1 do Dispose(Weight[i]);
    inherited;
end;

destructor TVertexWeightList.destroy;
begin
    clear;
    inherited;
end;

function TVertexWeightList.GetWeight(idx: Integer): pMS3D_vertex_ex_t;
begin
    result:=pMS3D_vertex_ex_t(items[idx]);
end;

function TVertexWeightList.newWeight: pMS3D_vertex_ex_t;
var
    p: pMS3D_vertex_ex_t;
begin
    new(p);
    add(p);
    result:=p;
end;


procedure TVertexWeightList.SetsubVersion(const Value: Integer);
begin
  FsubVersion := Value;
end;

{ TGLMS3DVectorFile }

class function TGLMS3DVectorFile.Capabilities: TGLDataFileCapabilities;
begin
  Result := [dfcRead];
end;

procedure TGLMS3DVectorFile.LoadFromStream(aStream: TStream);
var
  // GLScene
  i, j, k: integer;
  itemp: Cardinal;
  wtemp: word;
  TexCoordID: integer;
  MO: TGLMeshObject;
  FaceGroup: TFGVertexNormalTexIndexList;
  Sk_MO: TGLSkeletonMeshObject;

  GroupList: TList;
  GLLibMaterial: TGLLibMaterial;

  // Milkshape 3d
  ms3d_header: TMS3DHeader;
  nNumVertices: word;

  ms3d_vertices: PMS3DVertexArray;
  nNumTriangles: word;

  ms3d_triangle: TMS3DTriangle;
  ms3d_triangle2: TMS3DTriangle;
  ms3d_triangles: PMS3DTriangleArray;

  nNumGroups: word;
  Group: TMS3DGroup;
  nNumMaterials: word;
  ms3d_material: TMS3DMaterial;

  fAnimationFPS: single;
  fCurrentTime: single;
  iTotalFrames: integer;

  nNumJoints: word;
  ms3d_joints: PMS3DJointArray;

  bonelist: TStringList;
  bone: TGLSkeletonBone;
  frame: TGLSkeletonFrame;
  rot, pos: TVector3f;

  //Tod
  subVersionComments: integer;
  subVersionVertexExtra: integer;

  nNumGroupComments: integer;
  nNumMaterialComments: integer;
  nNumJointComments: integer;
  nHasModelComment: integer;

  ms3d_comment: pMS3DComment;
  vertexWeight: pMS3D_vertex_ex_t;
  ms3d_norm_Array: array of TD3DVector;
  ms3d_norm: TD3DVector;

  path, libtexture: string;
  dotpos: Integer;

  //Helper classes for MS3D comments if you want to use them.

  groupCommentList: TMS3DCommentList;
  materialCommentList: TMS3DCommentList;
  jointCommentList: TMS3DCommentList;
  modelCommentList: TMS3DCommentList;

  procedure AddFaceVertex(ID: integer);
  begin
    // Add the texCoord
    TexCoordID := MO.TexCoords.Add(ms3d_triangle.s[ID], -ms3d_triangle.t[ID]);
    //Ok, here we add the vertex and the normal. We pass in the vertex index for both the vertex and the normal.
    // This is because we have already added the normals to the Mesh in the same order as the vertices.
    FaceGroup.Add(ms3d_triangle.vertexIndices[ID], ms3d_triangle.vertexIndices[ID], TexCoordID);

  end;

  function AddRotations(rot, baserot: TAffineVector): TAffineVector;
  var
    mat1, mat2, rmat: TGLMatrix;
    s, c: Single;
    Trans: TTransformations;
  begin
    mat1 := IdentityHMGMatrix;
    mat2 := IdentityHMGMatrix;

    SinCos(rot.X, s, c);
    rmat := CreateRotationMatrixX(s, c);
    mat1 := MatrixMultiply(mat1, rmat);
    SinCos(rot.Y, s, c);
    rmat := CreateRotationMatrixY(s, c);
    mat1 := MatrixMultiply(mat1, rmat);
    SinCos(rot.Z, s, c);
    rmat := CreateRotationMatrixZ(s, c);
    mat1 := MatrixMultiply(mat1, rmat);

    SinCos(baserot.X, s, c);
    rmat := CreateRotationMatrixX(s, c);
    mat2 := MatrixMultiply(mat2, rmat);
    SinCos(baserot.Y, s, c);
    rmat := CreateRotationMatrixY(s, c);
    mat2 := MatrixMultiply(mat2, rmat);
    SinCos(baserot.Z, s, c);
    rmat := CreateRotationMatrixZ(s, c);
    mat2 := MatrixMultiply(mat2, rmat);

    mat1 := MatrixMultiply(mat1, mat2);
    if MatrixDecompose(mat1, Trans) then
      SetVector(Result, Trans[ttRotateX], Trans[ttRotateY], Trans[ttRotateZ])
    else
      Result := NullVector;
  end;

begin
  GroupList := TList.Create;
  FaceGroup := nil;
  ms3d_vertices := nil;
  ms3d_triangles := nil;
  ms3d_joints := nil;
  ms3d_norm_Array := nil;
  groupCommentList := nil;
  materialCommentList := nil;
  jointCommentList := nil;
  modelCommentList := nil;

  try
    //Save the path of the MS3D so we can load the texture from that location instead of the EXE location.
    path := ExtractFilePath(ResourceName);
    if Length(path)>0 then
      path := IncludeTrailingPathDelimiter( path );

    // First comes the header.
    aStream.ReadBuffer(ms3d_header, sizeof(TMS3DHeader));
    Assert(ms3d_header.version = 4, Format('The MilkShape3D importer can only handle MS3D files of version 4, this is version ', [ms3d_header.id]));

    // Then comes the number of vertices
    aStream.ReadBuffer(nNumVertices, sizeof(nNumVertices));

    // Creates the vertex list
    if Owner is TGLActor then
    begin
      MO := TGLSkeletonMeshObject.CreateOwned(Owner.MeshObjects);
      TGLSkeletonMeshObject(MO).BonesPerVertex := 4;
    end
    else
      MO := TGLMeshObject.CreateOwned(Owner.MeshObjects);
    MO.Mode := momFaceGroups;

    // Then comes nNumVertices * sizeof (ms3d_vertex_t)
    ms3d_vertices := AllocMem(sizeof(TMS3DVertex) * nNumVertices);
    aStream.ReadBuffer(ms3d_vertices^, sizeof(TMS3DVertex) * nNumVertices);

    for i := 0 to nNumVertices - 1 do
      with ms3d_vertices^[i] do
      begin
        // Add the vertex to the vertexlist
        MO.Vertices.Add(vertex.v);
        if Owner is TGLActor then
          TGLSkeletonMeshObject(MO).AddWeightedBone(Byte(BoneID), 1);
      end;

    // number of triangles
    aStream.ReadBuffer(nNumTriangles, sizeof(nNumTriangles));

    // nNumTriangles * sizeof (ms3d_triangle_t)
    ms3d_triangles := AllocMem(sizeof(TMS3DTriangle) * nNumTriangles);
    aStream.ReadBuffer(ms3d_triangles^, sizeof(TMS3DTriangle) * nNumTriangles);

    // Now we have to match up the vertices with the normals that are in the triangles list
    // This is because Milkshape stores normals in the triangles group.  A vertex can be used
    // many times by different faces. We need to compress that down to 1 vertex = 1 normal
    ms3d_norm.X := 0;
    ms3d_norm.Y := 0;
    ms3d_norm.Z := 0;
    setLength(ms3d_norm_Array, nNumVertices);
    for i := 0 to nNumVertices - 1 do
      ms3d_norm_Array[i] := ms3d_norm;
    for i := 0 to nNumTriangles - 1 do
    begin
      ms3d_triangle2 := ms3d_triangles^[i];
      ms3d_norm_Array[ms3d_triangle2.VertexIndices[0]] := ms3d_triangle2.VertexNormals[0];
      ms3d_norm_Array[ms3d_triangle2.VertexIndices[1]] := ms3d_triangle2.VertexNormals[1];
      ms3d_norm_Array[ms3d_triangle2.VertexIndices[2]] := ms3d_triangle2.VertexNormals[2];
    end;

    // Now add the normals in the same order as the vertices to the mesh
    for i := 0 to nNumVertices - 1 do
    begin
      MO.Normals.Add(ms3d_norm_Array[i].v);
    end;
    ms3d_norm_Array := nil;

    // number of groups
    aStream.ReadBuffer(nNumGroups, sizeof(nNumGroups));

    // nNumGroups * sizeof (ms3d_group_t)
    for i := 0 to nNumGroups - 1 do
    begin
      // Read the first part of the group
      Group := TMS3DGroup.Create;
      GroupList.Add(Group);
      aStream.ReadBuffer(Group.Flags, sizeof(Group.Flags));
      aStream.ReadBuffer(Group.name, sizeof(Group.name));
      aStream.ReadBuffer(Group.numtriangles, sizeof(Group.numtriangles));

      for j := 0 to Group.numtriangles - 1 do
      begin
        aStream.ReadBuffer(wtemp, sizeof(wtemp));
        itemp := wtemp;
        Group.triangleIndices.Add(pointer(itemp));
      end;
      aStream.ReadBuffer(Group.materialIndex, sizeof(Group.materialIndex));

      // if materialindex=-1, then there is no material, and all faces should
      // be added to a base VIL
      if Group.materialIndex = -1 then
      begin
        // If there's no base VIL, create one!
        if FaceGroup = nil then
          FaceGroup := TFGVertexNormalTexIndexList.CreateOwned(MO.FaceGroups);

        for j := 0 to Group.numtriangles - 1 do
        begin
          ms3d_triangle := ms3d_triangles^[Cardinal(Group.triangleIndices[j])];
          AddFaceVertex(0);
          AddFaceVertex(1);
          AddFaceVertex(2);
        end;
      end;
    end;
    // number of materials
    aStream.ReadBuffer(nNumMaterials, sizeof(nNumMaterials));
    // nNumMaterials * sizeof (ms3d_material_t)
    for i := 0 to nNumMaterials - 1 do
    begin
      aStream.ReadBuffer(ms3d_material, sizeof(TMS3DMaterial));
      // Creates the material, if there's a materiallibrary!
      if Assigned(Owner.MaterialLibrary) then
      begin
        libtexture := string(ms3d_material.texture);
        dotpos := System.Pos('.', libtexture);
        Delete(libtexture, dotpos, Length(libtexture)-dotpos+1);
        GLLibMaterial := Owner.MaterialLibrary.LibMaterialByName(libtexture);
        if Assigned(GLLibMaterial) then
        begin
          GLLibMaterial.Material.Texture.Disabled := False;
        end
        else if FileStreamExists(path + string(ms3d_material.texture)) then
          GLLibMaterial := Owner.MaterialLibrary.AddTextureMaterial(libtexture, path + string(ms3d_material.texture))
        else
        begin
          if not Owner.IgnoreMissingTextures then
            Exception.Create('Texture file not found: ' + path + string(ms3d_material.texture));
          GLLibMaterial := Owner.MaterialLibrary.Materials.Add;
          GLLibMaterial.Name := string(ms3d_material.name);
        end;
        GLLibMaterial.Material.FrontProperties.Emission.Color := ms3d_material.emissive;
        GLLibMaterial.Material.FrontProperties.Ambient.Color := ms3d_material.ambient;
        GLLibMaterial.Material.FrontProperties.Diffuse.Color := ms3d_material.diffuse;
        GLLibMaterial.Material.FrontProperties.Specular.Color := ms3d_material.specular;

        // Shinintess is 0 to 128 in both MS3D and GLScene. Why not 0 to 127? Odd.
        GLLibMaterial.Material.FrontProperties.Shininess := round(ms3d_material.shininess);

        // ms3d_material.transparency is allready set as alpha channel on all
        // colors above
        if ms3d_material.transparency < 1 then
        begin
          GLLibMaterial.Material.BlendingMode := bmTransparency;
          GLLibMaterial.Material.FaceCulling := fcNoCull; //Make transparent materials two sided.
        end;
        GLLibMaterial.Material.Texture.TextureMode := tmModulate;

        // Creates a new face group and add all triangles for this material
        // here. We must cycle through all groups that have this material
        FaceGroup := TFGVertexNormalTexIndexList.CreateOwned(MO.FaceGroups);
        FaceGroup.MaterialName := GLLibMaterial.Name;

        for j := 0 to GroupList.Count - 1 do
        begin
          Group := TMS3DGroup(GroupList[j]);
          if Group.materialIndex = i then
            for k := 0 to Group.numtriangles - 1 do
            begin
              ms3d_triangle := ms3d_triangles^[Cardinal(Group.triangleIndices[k])];

              AddFaceVertex(0);
              AddFaceVertex(1);
              AddFaceVertex(2);
            end;
        end;
      end
      else
      begin
        Exception.Create(ResourceName + ' has materials but there is no material library assigned to the object loading this MS3D file.' + #10#13 + 
          'If this is what you want,  then you can safely ignore this exception.');
      end;
    end;

    // save some keyframer data
    aStream.ReadBuffer(fAnimationFPS, sizeof(fAnimationFPS));
    aStream.ReadBuffer(fCurrentTime, sizeof(fCurrentTime));
    aStream.ReadBuffer(iTotalFrames, sizeof(iTotalFrames));

    if Owner is TGLActor then
    begin
      TGLActor(Owner).Interval := trunc(1 / fAnimationFPS * 1000);
    end;

    // number of joints
    aStream.ReadBuffer(nNumJoints, sizeof(nNumJoints));

    // nNumJoints * sizeof (ms3d_joint_t)
    ms3d_joints := AllocMem(sizeof(TMS3DJoint) * nNumJoints);

    // We have to read the joints one by one!
    for i := 0 to nNumJoints - 1 do
    begin
      // Read the joint base
      aStream.ReadBuffer(ms3d_joints^[i].Base, sizeof(TMS3DJointBase));
      if (i = 0) then
        Assert(ms3d_joints^[i].base.numKeyFramesRot = iTotalFrames, 'This importer only works if the number of key frames = the number of total frames. i.e. Every frame must be a key frame');
      if ms3d_joints^[i].base.numKeyFramesRot > 0 then
      begin
        // Allocate memory for the rotations
        ms3d_joints^[i].keyFramesRot := AllocMem(sizeof(TMS3DKeyframeRotation) * ms3d_joints^[i].base.numKeyFramesRot);
        // Read the rotations
        aStream.ReadBuffer(ms3d_joints^[i].keyFramesRot^, sizeof(TMS3DKeyframeRotation) * ms3d_joints^[i].base.numKeyFramesRot);
      end
      else
        ms3d_joints^[i].keyFramesRot := nil;
      if ms3d_joints^[i].base.numKeyFramesTrans > 0 then
      begin
        // Allocate memory for the translations
        ms3d_joints^[i].keyFramesTrans := AllocMem(sizeof(TMS3DKeyframePosition) * ms3d_joints^[i].base.numKeyFramesTrans);
        // Read the translations
        aStream.ReadBuffer(ms3d_joints^[i].keyFramesTrans^, sizeof(TMS3DKeyframePosition) * ms3d_joints^[i].base.numKeyFramesTrans);
      end
      else
        ms3d_joints^[i].keyFramesTrans := nil;
    end;

    //Below is the Comments sections. We don't do anything with them at all.  Only read in for future use if needed
    aStream.ReadBuffer(subVersionComments, sizeof(subVersionComments));

    //*******************
    //*  now read in the Group Comments.
    //*******************
    aStream.ReadBuffer(nNumGroupComments, sizeof(nNumGroupComments));
    groupCommentList := TMS3DCommentList.Create;
    if (nNumGroupComments > 0) then
      groupCommentList.Capacity := nNumGroupComments;
    for i := 0 to nNumGroupComments - 1 do
    begin
      ms3d_comment := groupCommentList.NewComment;
      aStream.ReadBuffer(ms3d_comment^.index, sizeOf(ms3d_comment^.index));
      aStream.ReadBuffer(ms3d_comment^.commentLength, sizeOf(ms3d_comment^.commentLength));
      setLength(ms3d_comment^.comment, ms3d_comment^.commentLength);
      aStream.ReadBuffer(ms3d_comment^.comment[0], ms3d_comment^.commentLength);
    end;
    ////////

    //*******************
    //*  now read in the Material Comments.
    //*******************
    aStream.ReadBuffer(nNumMaterialComments, sizeof(nNumMaterialComments));
    MaterialCommentList := TMS3DCommentList.Create;
    if (nNumMaterialComments > 0) then
      MaterialCommentList.Capacity := nNumMaterialComments;
    for i := 0 to nNumMaterialComments - 1 do
    begin
      ms3d_comment := MaterialCommentList.NewComment;
      aStream.ReadBuffer(ms3d_comment^.index, sizeOf(ms3d_comment^.index));
      aStream.ReadBuffer(ms3d_comment^.commentLength, sizeOf(ms3d_comment^.commentLength));
      setLength(ms3d_comment^.comment, ms3d_comment^.commentLength);
      aStream.ReadBuffer(ms3d_comment^.comment[0], ms3d_comment^.commentLength);
    end;
    ////////

    //*******************
    //*  now read in the Joint Comments.
    //*******************
    aStream.ReadBuffer(nNumJointComments, sizeof(nNumJointComments));
    JointCommentList := TMS3DCommentList.Create;
    if (nNumJointComments > 0) then
      JointCommentList.Capacity := nNumJointComments;
    for i := 0 to nNumJointComments - 1 do
    begin
      ms3d_comment := JointCommentList.NewComment;
      aStream.ReadBuffer(ms3d_comment^.index, sizeOf(ms3d_comment^.index));
      aStream.ReadBuffer(ms3d_comment^.commentLength, sizeOf(ms3d_comment^.commentLength));
      setLength(ms3d_comment^.comment, ms3d_comment^.commentLength);
      aStream.ReadBuffer(ms3d_comment^.comment[0], ms3d_comment^.commentLength);
    end;
    ////////

    //*******************
    //*  now read in the Model Comment (if any).  The milkshape spec on this is somewhat wrong, as it does not use the same
    //* comments structure as the previous ones. Index is not included and I guess is assumed to always be 0 as there
    //* can only be one
    //*******************
    aStream.ReadBuffer(nHasModelComment, sizeof(nHasModelComment));
    ModelCommentList := TMS3DCommentList.Create;
    for i := 0 to nHasModelComment - 1 do
    begin
      ms3d_comment := ModelCommentList.NewComment;
      ms3d_comment^.index := 0;
      aStream.ReadBuffer(ms3d_comment^.commentLength, sizeOf(ms3d_comment^.commentLength));
      setLength(ms3d_comment^.comment, ms3d_comment^.commentLength);
      aStream.ReadBuffer(ms3d_comment^.comment[0], ms3d_comment^.commentLength);
    end;
    ////////

    //Read in the vertex weights
    //
    aStream.ReadBuffer(subVersionVertexExtra, sizeof(subVersionVertexExtra));
    Sk_MO := TGLSkeletonMeshObject(MO);
    if Owner is TGLActor then
    begin
      for i := 0 to nNumVertices - 1 do
      begin
        new(vertexWeight);
        aStream.ReadBuffer(vertexWeight^.boneIds[0], sizeOf(vertexWeight^.boneIds));
        aStream.ReadBuffer(vertexWeight^.weights[0], sizeOf(vertexWeight^.weights));
        aStream.ReadBuffer(vertexWeight^.extra, sizeOf(vertexWeight^.extra));
        if (subVersionVertexExtra = 3) then
          aStream.ReadBuffer(vertexWeight^.unknown, sizeOf(vertexWeight^.unknown));

        Sk_MO.VerticesBonesWeights^[i]^[0].Weight := 1;
        if (vertexWeight.boneIds[0] <> 255) then
        begin
          Sk_MO.VerticesBonesWeights^[i]^[0].Weight := vertexWeight.weights[0] / 100;
          Sk_MO.VerticesBonesWeights^[i]^[1].Weight := vertexWeight.weights[1] / 100;
          Sk_MO.VerticesBonesWeights^[i]^[1].BoneID := vertexWeight.boneIds[0];
        end
        else
        begin
          Sk_MO.VerticesBonesWeights^[i]^[1].Weight := 0;
          Sk_MO.VerticesBonesWeights^[i]^[1].BoneID := 0;
        end;
        if (vertexWeight.boneIds[1] <> 255) then
        begin
          Sk_MO.VerticesBonesWeights^[i]^[2].Weight := vertexWeight.weights[2] / 100;
          Sk_MO.VerticesBonesWeights^[i]^[2].BoneID := vertexWeight.boneIds[1];
        end
        else
        begin
          Sk_MO.VerticesBonesWeights^[i]^[2].Weight := 0;
          Sk_MO.VerticesBonesWeights^[i]^[2].BoneID := 0;
        end;
        if (vertexWeight.boneIds[2] <> 255) then
        begin
          Sk_MO.VerticesBonesWeights^[i]^[3].Weight := 1.0 - (vertexWeight.weights[0] + vertexWeight.weights[1] + vertexWeight.weights[2]) / 100;
          Sk_MO.VerticesBonesWeights^[i]^[3].BoneID := vertexWeight.boneIds[2];
        end
        else
        begin
          Sk_MO.VerticesBonesWeights^[i]^[3].Weight := 0;
          Sk_MO.VerticesBonesWeights^[i]^[3].BoneID := 0;
        end;
        dispose(vertexWeight);
      end;
    end;

    // ***
    // Mesh Transformation:
    //
    // 0. Build the transformation matrices from the rotation and position
    // 1. Multiply the vertices by the inverse of local reference matrix (lmatrix0)
    // 2. then translate the result by (lmatrix0 * keyFramesTrans)
    // 3. then multiply the result by (lmatrix0 * keyFramesRot)
    //
    // For normals skip step 2.
    //
    //
    //
    // NOTE:  this file format may change in future versions!
    //
    //
    // - Mete Ciragan
    // ***

    if (Owner is TGLActor) and (nNumJoints > 0) then
    begin
      // Bone names are added to a list initally to sort out parents
      bonelist := TStringList.Create;
      for i := 0 to nNumJoints - 1 do
        bonelist.Add(string(ms3d_joints^[i].Base.Name));
      // Find parent bones and add their children
      for i := 0 to nNumJoints - 1 do
      begin
        j := bonelist.IndexOf(string(ms3d_joints^[i].Base.ParentName));
        if j = -1 then
          bone := TGLSkeletonBone.CreateOwned(Owner.Skeleton.RootBones)
        else
          bone := TGLSkeletonBone.CreateOwned(Owner.Skeleton.RootBones.BoneByID(j));
        bone.Name := string(ms3d_joints^[i].Base.Name);
        bone.BoneID := i;
      end;
      bonelist.Free;
      // Set up the base pose
      frame := TGLSkeletonFrame.CreateOwned(Owner.Skeleton.Frames);
      for i := 0 to nNumJoints - 1 do
      begin
        pos := ms3d_joints^[i].Base.Position.V;
        rot := ms3d_joints^[i].Base.Rotation.V;
        frame.Position.Add(pos);
        frame.Rotation.Add(rot);
      end;

      // Now load the animations
      for i := 0 to nNumJoints - 1 do
      begin
        for j := 0 to ms3d_joints^[i].Base.NumKeyFramesRot - 1 do
        begin
          if (j + 1) = Owner.Skeleton.Frames.Count then
            frame := TGLSkeletonFrame.CreateOwned(Owner.Skeleton.Frames)
          else
            frame := Owner.Skeleton.Frames[j + 1];
          if ms3d_joints^[i].Base.ParentName = '' then
          begin
            pos := ms3d_joints^[i].KeyFramesTrans^[j].Position.V;
            //pos:=ms3d_joints^[i].Base.Position.V;
            rot := ms3d_joints^[i].KeyFramesRot^[j].Rotation.V;
          end
          else
          begin
            pos := ms3d_joints^[i].KeyFramesTrans^[0].Position.V; //Always read tranlation position from the first frame
            AddVector(pos, ms3d_joints^[i].Base.Position.V);
            rot := ms3d_joints^[i].KeyFramesRot^[j].Rotation.V;
            rot := AddRotations(rot, ms3d_joints^[i].Base.Rotation.V);
          end;
          frame.Position.Add(pos);
          frame.Rotation.Add(rot);
        end;
      end;
      Owner.Skeleton.RootBones.PrepareGlobalMatrices;
      TGLSkeletonMeshObject(MO).PrepareBoneMatrixInvertedMeshes;
      with TGLActor(Owner).Animations.Add do
      begin
        Reference := aarSkeleton;
        StartFrame := 0;
        EndFrame := Owner.Skeleton.Frames.Count;
      end;
    end;
  finally
    if Assigned(ms3d_vertices) then
      FreeMem(ms3d_vertices);
    if Assigned(ms3d_triangles) then
      FreeMem(ms3d_triangles);
    if Assigned(ms3d_joints) then
    begin
      // Free the internal storage of the joint
      for i := 0 to nNumJoints - 1 do
      begin
        if Assigned(ms3d_joints^[i].keyFramesRot) then
          FreeMem(ms3d_joints^[i].keyFramesRot);
        if Assigned(ms3d_joints^[i].keyFramesTrans) then
          FreeMem(ms3d_joints^[i].keyFramesTrans);
      end;
      FreeMem(ms3d_joints);
    end;

    // Finalize
    for i := 0 to GroupList.Count - 1 do
      TMS3DGroup(GroupList[i]).Free;
    GroupList.Free;
    if (assigned(groupCommentList)) then
      groupCommentList.free;
    if (assigned(materialCommentList)) then
      materialCommentList.free;
    if (assigned(jointCommentList)) then
      jointCommentList.free;
    if (assigned(modelCommentList)) then
      modelCommentList.free;
  end;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

  RegisterVectorFileFormat('ms3d', 'MilkShape3D files', TGLMS3DVectorFile);

end.

