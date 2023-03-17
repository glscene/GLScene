//
// Graphic Scene Engine, http://glscene.org
//
(*
	Vector file object loading of Ghoul2 model and animation files.
*)
unit GLX.FileGL2;

interface

uses
  System.Classes,
  System.SysUtils,

  GLX.VectorFileObjects,
  GLX.ApplicationFileIO,
  GLX.Material,
  Scena.VectorGeometry,
  Scena.VectorTypes,
  GLX.VectorLists,

  Formats.FileGL2;

type

  TgxGLMVectorFile = class (TgxVectorFile)
    public
      class function Capabilities : TDataFileCapabilities; override;
      procedure LoadFromStream(aStream : TStream); override;
  end;

  TgxGLAVectorFile = class (TgxVectorFile)
    public
      class function Capabilities : TDataFileCapabilities; override;
      procedure LoadFromStream(aStream : TStream); override;
  end;

var
  vGhoul2LevelOfDetail     : Integer = 0; // Highest detail level by default
  vGhoul2MaxBonesPerVertex : Integer = 4; // Ghoul2 supports up to 4 bones
                                          // per vertex. Use this global
                                          // variable to set a different limit.

//====================================================================
implementation
//====================================================================

// ------------------
// ------------------ TgxGLMVectorFile ------------------
// ------------------

class function TgxGLMVectorFile.Capabilities : TDataFileCapabilities;
begin
  Result:=[dfcRead];
end;

procedure TgxGLMVectorFile.LoadFromStream(aStream : TStream);
var
  GLMFile     : TFileGLM;
  i,j,k,s,c,d : integer;
  mesh        : TgxSkeletonMeshObject;
  fg          : TfgxVertexIndexList;
  VertOfs     : integer;
  shader      : string;
  vec2        : Tvector2f;
  numweights,
  boneref,
  boneid      : Integer;
  boneweight,
  weighttot   : Single;
  NumSurfVert : Integer;

  procedure AllocateMaterial(meshname,shader:string);
  var
    LibMat : TgxLibMaterial;
  begin
    if Assigned(Owner.MaterialLibrary) then with Owner.MaterialLibrary do begin
      if Assigned(Materials.GetLibMaterialByName(meshname)) then exit;
      LibMat:=Materials.Add;
      LibMat.name:=meshname;
      LibMat.Material.Texture.Disabled:=False;
    end;
  end;

begin
  GLMFile:=TFileGLM.Create;
  GLMFile.LoadFromStream(aStream);
  try
    // Need a way to import all levels of detail, but this global
    // variable will do for now.
    d:=vGhoul2LevelOfDetail;
    if d>=Length(GLMFile.LODs) then exit;
    for s:=0 to Length(GLMFile.SurfaceHeirachy)-1 do begin
      mesh:=TgxSkeletonMeshObject.CreateOwned(Owner.MeshObjects);
      mesh.Mode:=momFaceGroups;
      mesh.Name:=trim(GLMFile.SurfaceHeirachy[s].name);
      shader:=trim(GLMFile.SurfaceHeirachy[s].shader);
      AllocateMaterial(mesh.Name,shader);

      // Set size of VerticesBonesWeights
      NumSurfVert:=0;
      for c:=0 to GLMFile.SurfaceHeirachy[s].numChildren-1 do begin
        i:=GLMFile.SurfaceHeirachy[s].childIndices[c]-1;
        NumSurfVert:=NumSurfVert+GLMFile.LODs[d].Surfaces[i].SurfaceHeader.numVerts;
      end;
      mesh.BonesPerVertex:=vGhoul2MaxBonesPerVertex;
      mesh.VerticeBoneWeightCount:=NumSurfVert;
      for c:=0 to GLMFile.SurfaceHeirachy[s].numChildren-1 do begin
        i:=GLMFile.SurfaceHeirachy[s].childIndices[c]-1;
        with GLMFile.LODs[d].Surfaces[i] do begin
          VertOfs:=mesh.Vertices.Count;
          for j:=0 to Length(Vertices)-1 do begin
            // Add vertices and normals
            mesh.Vertices.Add(Vertices[j].vertex);
            mesh.Normals.Add(Vertices[j].normal);

            // Fix and then add the Texture coords
            vec2:=TexCoords[j];
            vec2.Y:=1-vec2.Y; // reverse the v coordinate
            mesh.TexCoords.Add(vec2);

            // Add weighted bones
            numweights:=G2_GetVertWeights(Vertices[j]);
            weighttot:=0;
            for k:=0 to mesh.BonesPerVertex-1 do
              if k<numweights then begin
                boneref:=G2_GetVertBoneIndex(Vertices[j],k);
                boneid:=BoneReferences[boneref];
                boneweight:=G2_GetVertBoneWeight(Vertices[j],k,weighttot,numweights);
                mesh.VerticesBonesWeights^[mesh.Vertices.Count-1]^[k].BoneID:=boneid;
                mesh.VerticesBonesWeights^[mesh.Vertices.Count-1]^[k].Weight:=boneweight;
              end else begin
                mesh.VerticesBonesWeights^[mesh.Vertices.Count-1]^[k].BoneID:=0;
                mesh.VerticesBonesWeights^[mesh.Vertices.Count-1]^[k].Weight:=0;
              end;
          end;
          fg:=TfgxVertexIndexList.CreateOwned(mesh.FaceGroups);
          fg.Mode:=fgmmTriangles;
          fg.MaterialName:=mesh.Name;
          for j:=0 to Length(Triangles)-1 do begin
            // The faces need to be wound in the opposite direction so...
            fg.VertexIndices.Add(Triangles[j].indices[0]+VertOfs);
            fg.VertexIndices.Add(Triangles[j].indices[2]+VertOfs);
            fg.VertexIndices.Add(Triangles[j].indices[1]+VertOfs);
          end;
        end;
      end;
    end;

  finally
    GLMFile.Free;
  end;
end;


// ------------------
// ------------------ TgxGLAVectorFile ------------------
// ------------------

// Capabilities
//
class function TgxGLAVectorFile.Capabilities : TDataFileCapabilities;
begin
  Result:=[dfcRead];
end;

// LoadFromStream
//
procedure TgxGLAVectorFile.LoadFromStream(aStream : TStream);
var
  GLAFile  : TFileGLA;
  i,j      : Integer;
  frame    : TgxSkeletonFrame;
  CompBone : TgxACompQuatBone;
  quat     : TQuaternion;
  pos      : TAffineVector;
  basepose : TgxSkeletonFrame;
  bonelist : TgxIntegerList;
  bone     : TgxSkeletonBone;

begin
  GLAFile:=TFileGLA.Create;
  GLAFile.LoadFromStream(aStream);
  try
    if not (Owner is TgxActor) then exit;

    TgxActor(Owner).Reference:=aarSkeleton;

    bonelist:=TgxIntegerList.Create;
    for i:=0 to GLAFile.AnimHeader.numBones-1 do
      bonelist.Add(i);
    while bonelist.count>0 do begin
      if GLAFile.Skeleton[bonelist[0]].parent = -1 then
        bone:=TgxSkeletonBone.CreateOwned(Owner.Skeleton.RootBones)
      else begin
        bone:=Owner.Skeleton.RootBones.BoneByID(GLAFile.Skeleton[bonelist[0]].parent);
        if Assigned(bone) then
          bone:=TgxSkeletonBone.CreateOwned(bone)
      end;
      if Assigned(bone) then begin
        bone.Name:=GLAFile.Skeleton[bonelist[0]].Name;
        bone.BoneID:=bonelist[0];
      end else
        bonelist.Add(bonelist[0]);
      bonelist.Delete(0);
    end;
    bonelist.Free;

    // Build the base pose
    basepose:=TgxSkeletonFrame.CreateOwned(TgxActor(Owner).Skeleton.Frames);
    basepose.Name:='basepose';
    basepose.TransformMode:=sftQuaternion;
    basepose.Position.AddNulls(GLAFile.AnimHeader.numBones);
    basepose.Quaternion.AddNulls(GLAFile.AnimHeader.numBones);

    // Load animation data
    for i:=0 to GLAFile.AnimHeader.numFrames-1 do begin
      // Create the frame
      frame:=TgxSkeletonFrame.CreateOwned(TgxActor(Owner).Skeleton.Frames);
      frame.Name:='Frame'+IntToStr(i);
      frame.TransformMode:=sftQuaternion;

      for j:=0 to GLAFile.AnimHeader.numBones-1 do begin
        // Uncompress bone and add to the frame
        CompBone:=GLAFile.GetCompressedMatrix(i,j);
        quat:=QuaternionMake([CompBone[1]-32726,CompBone[2]-32726,CompBone[3]-32726],CompBone[0]-32726);
        pos:=AffineVectorMake(CompBone[4]/64-512,CompBone[5]/64-512,CompBone[6]/64-512);
        frame.Quaternion.Add(quat);
        frame.Position.Add(pos);
      end;
    end;

    Owner.Skeleton.RootBones.PrepareGlobalMatrices;

    for i:=0 to Owner.MeshObjects.Count-1 do
      TgxSkeletonMeshObject(Owner.MeshObjects[i]).PrepareBoneMatrixInvertedMeshes;

  finally
    GLAFile.Free;
  end;
end;


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterVectorFileFormat('glm', 'Ghoul2 (GLM) model files', TgxGLMVectorFile);
   RegisterVectorFileFormat('glx', 'Ghoul2 (GLX) model files', TgxGLMVectorFile);
   RegisterVectorFileFormat('gla', 'Ghoul2 (GLA) animation files', TgxGLAVectorFile);

end.
