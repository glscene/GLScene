//
// The graphics rendering engine GLScene http://glscene.org
//
unit GLS.FileB3D;

(* B3D VectorFile class to load Blitz 3D model files *)

interface

uses
  System.Classes, 
  System.SysUtils,
   
  GLS.VectorFileObjects, 
  GLS.ApplicationFileIO, 
  GLS.Texture, 
  GLS.TextureFormat,
  GLS.Material, 
  GLS.VectorTypes, 
  GLS.VectorGeometry, 
  GLS.VectorLists,
  Formats.B3D;

type
  TGLB3DVectorFile = class(TGLVectorFile)
  public
    class function Capabilities: TGLDataFileCapabilities; override;
    procedure LoadFromStream(AStream: TStream); override;
  end;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

// ------------------------------ TGLB3DVectorFile ------------------------------

class function TGLB3DVectorFile.Capabilities: TGLDataFileCapabilities;
begin
  Result := [DfcRead];
end;


procedure TGLB3DVectorFile.LoadFromStream(AStream: TStream);

var
  B3d: TFileB3D;
  S: string;
  Mo: TMeshObject;
  I, J: Integer;
  FaceGroup: TFGVertexIndexList;
  // lightmapBmp : TBitmap;
  Node: PNODEChunk;
  B3DMat: TB3DMaterial;
  B3DTex: TB3DTexture;
  B3DLightTex: TB3DTexture;
  Vertex: PVertexData;
  Triangles: PTRISChunk;
  V, V1: TAffineVector;
  Matrix: TGLMatrix;
  MatLib: TGLMaterialLibrary;
  LightLib: TGLMaterialLibrary;
  RotQuat: TQuaternion;
  RotMat: TGLMatrix;

  function GetOrAllocateMaterial(MaterialNum: Integer; AMat: TB3DMaterial;
    ATex: TB3DTexture; ALightmap: TB3DTexture): string;
  var
    LibMat: TGLLibMaterial;
    TexName: string;
    LightName: string;
  begin
    if GetOwner is TGLBaseMesh then
    begin
      MatLib := TGLBaseMesh(GetOwner).MaterialLibrary;
      LightLib := TGLBaseMesh(GetOwner).LightmapLibrary;
      // got a linked material library?
      if Assigned(MatLib) then
      begin
        Result := AMat.GetMaterialName;
        // add base material
        LibMat := MatLib.Materials.GetLibMaterialByName(Result);
        if not Assigned(LibMat) then
        begin
          if Assigned(ATex) then
            TexName := ATex.GetTextureName
          else
            TexName := '';
          if not FileExists(TexName) then
            TexName := ExtractFileName(TexName);
          if TexName <> '' then
            LibMat := MatLib.AddTextureMaterial(Result + IntToStr(MaterialNum),
              TexName, False)
          else
          begin
            LibMat := MatLib.Materials.Add;
            LibMat.Name := Result + IntToStr(MaterialNum);
          end;
          Libmat.Material.FrontProperties.Diffuse.Red := AMat.MaterialData.Red;
          Libmat.Material.FrontProperties.Diffuse.Green :=
            AMat.MaterialData.Green;
          Libmat.Material.FrontProperties.Diffuse.Blue :=
            AMat.MaterialData.Blue;
          Libmat.Material.FrontProperties.Diffuse.Alpha :=
            AMat.MaterialData.Alpha;
          Libmat.Material.FrontProperties.Shininess :=
            Round(AMat.MaterialData.Shininess * 100.0);
          Libmat.Material.MaterialOptions := [MoNoLighting];
          if AMat.MaterialData.Alpha <> 1 then
          begin
            Libmat.Material.FaceCulling := FcNoCull;
            Libmat.Material.BlendingMode := BmTransparency;
          end;
          if Assigned(ATex) then
          begin
            LibMat.TextureOffset.AsAffineVector :=
              AffineVectorMake(ATex.TextureData.X_pos,
              ATex.TextureData.Y_pos, 0);
            LibMat.TextureScale.AsAffineVector :=
              AffineVectorMake(ATex.TextureData.X_scale,
              ATex.TextureData.Y_scale, 1);
            if ATex.TextureData.Flags = 2 then
            begin
              Libmat.Material.FaceCulling := FcNoCull;
              Libmat.Material.BlendingMode := BmTransparency;
            end;
            if AMat.MaterialData.Alpha <> 1 then
            begin
              Libmat.Material.Texture.ImageAlpha := TiaAlphaFromIntensity;
              Libmat.Material.Texture.TextureFormat := TfRGBA;
              Libmat.Material.Texture.TextureMode := TmModulate;
            end;
          end;
        end;
        // add lightmap material
        if (Assigned(LightLib)) and (Assigned(ALightmap)) then
        begin
          LightName := ALightmap.GetTextureName;
          // add base material
          LibMat := LightLib.Materials.GetLibMaterialByName(LightName
            { + IntToStr(MaterialNum) } );
          if not Assigned(LibMat) then
          begin
            if not FileExists(LightName) then
              LightName := ExtractFileName(LightName);

            LibMat := LightLib.AddTextureMaterial(LightName
              { + IntToStr(MaterialNum) } , LightName, False);
            LibMat.Material.Texture.TextureMode := TmReplace;
            if Assigned(ALightMap) then
            begin

              LibMat.TextureOffset.AsAffineVector :=
                AffineVectorMake(ALightMap.TextureData.X_pos,
                ALightMap.TextureData.Y_pos, 0);

              LibMat.TextureScale.AsAffineVector :=
                AffineVectorMake(ALightMap.TextureData.X_scale,
                ALightMap.TextureData.Y_scale, 1);
            end;
          end;
          // modify the material lightmap index
          AMat.MaterialData.Texture_id[1] := LibMat.Index;
        end;
      end
      else
        Result := '';
    end
    else
      Result := '';
  end;

begin
  B3d := TFileB3D.Create;
  try
    // first, load the b3d model sturctures from stream
    B3d.LoadFromStream(AStream);
    // then add all the materials and lightmaps from b3d structures
    for I := 0 to B3d.Materials.Count - 1 do
    begin
      B3DMat := TB3DMaterial(B3d.Materials.Objects[I]);
      B3DTex := nil;
      B3DLightTex := nil;
      // check if there is one texture layer
      if B3DMat.MaterialData.N_texs > 0 then
      begin
        if B3DMat.MaterialData.Texture_id[0] >= 0 then
          B3DTex := TB3DTexture(B3d.Textures.Objects
            [B3DMat.MaterialData.Texture_id[0]]);
        // check if there are two texture layer
        if B3DMat.MaterialData.N_texs > 1 then
          // why lightmap in some case on channel 2?
          if B3DMat.MaterialData.Texture_id[1] >= 0 then
            B3DLightTex :=
              TB3DTexture(B3d.Textures.Objects
              [B3DMat.MaterialData.Texture_id[1]])
          else
            { //check if there are three texture layer }
            if B3DMat.MaterialData.N_texs > 2 then
              if B3DMat.MaterialData.Texture_id[2] >= 0 then
                B3DLightTex :=
                  TB3DTexture(B3d.Textures.Objects
                  [B3DMat.MaterialData.Texture_id[2]]);
      end;
      GetOrAllocateMaterial(I, B3DMat, B3DTex, B3DLightTex);
    end;

    if GetOwner is TGLBaseMesh then
      (GetOwner as TGLBaseMesh).NormalsOrientation := MnoDefault;

    Node := B3d.Nodes.NodeData;
    while Node <> nil do
    begin
      if Node^.Meshes <> nil then
      begin
        Mo := TMeshObject.CreateOwned(Owner.MeshObjects);

        SetString(S, Node^.Name, Strlen(Node^.Name));
        // if Pos('16', s)>1 then
        // Pos('17', s);
        Mo.Name := S;
        Mo.Mode := MomFaceGroups;
        // add all the vertices, normals, colors and texture-coords(including the lightmap texture)
        Vertex := Node^.Meshes^.Vertices.Vertices;
        while Assigned(Vertex) do
        begin
          // W3D modif inversed z
          Mo.Vertices.Add(AffineVectorMake(Vertex^.Y, Vertex^.X, Vertex^.Z));
          if (Node^.Meshes^.Vertices.Flags and 1) > 0 then
            Mo.Normals.Add(VectorNormalize(AffineVectorMake(Vertex^.Ny,
              Vertex^.Nx, Vertex^.Nz)));

          if (Node^.Meshes^.Vertices.Flags and 2) > 0 then
          begin
            Mo.Colors.Add(VectorMake(Vertex^.Red, Vertex^.Green, Vertex^.Blue,
              Vertex^.Alpha));
          end;

          case Node^.Meshes^.Vertices.Tex_coord_sets of
            1:
              begin
                case Node^.Meshes^.Vertices.Tex_coord_set_size of
                  2:
                    Mo.TexCoords.Add(Vertex^.Tex_coords[0],
                      -Vertex^.Tex_coords[1], 0);
                  3:
                    Mo.TexCoords.Add(Vertex^.Tex_coords[0],
                      -Vertex^.Tex_coords[1], Vertex^.Tex_coords[2]);
                end;
              end;
            2: // lightmap tex_coord included
              begin
                case Node^.Meshes^.Vertices.Tex_coord_set_size of
                  2:
                    Mo.TexCoords.Add(Vertex^.Tex_coords[0],
                      -Vertex^.Tex_coords[1], 0);
                  3:
                    Mo.TexCoords.Add(Vertex^.Tex_coords[0],
                      -Vertex^.Tex_coords[1], Vertex^.Tex_coords[2]);
                end;
                Mo.LightMapTexCoords.Add
                  (Vertex^.Tex_coords
                  [Node^.Meshes^.Vertices.Tex_coord_set_size],
                  -Vertex^.Tex_coords
                  [Node^.Meshes^.Vertices.Tex_coord_set_size + 1]);
              end;
          end;
          Vertex := Vertex^.Next;
        end;
        // add facegroups
        Triangles := Node^.Meshes^.Triangles;
        while Assigned(Triangles) do
        begin
          FaceGroup := TFGVertexIndexList.CreateOwned(Mo.FaceGroups);
          if Triangles^.Brush_id >= 0 then
          begin
            FaceGroup.MaterialName := B3d.Materials[Triangles^.Brush_id] +
              InttoStr(Triangles^.Brush_id);
            FaceGroup.LightMapIndex :=
              TB3DMaterial(B3d.Materials.Objects[Triangles^.Brush_id])
              .MaterialData.Texture_id[1];
          end
          else
          begin
            FaceGroup.MaterialName := '';
            FaceGroup.LightMapIndex := -1;
          end;
          for J := 0 to Length(Triangles^.Vertex_id) - 1 do
            FaceGroup.VertexIndices.Add(Triangles^.Vertex_id[J]);
          while FaceGroup.VertexIndices.Count mod 3 <> 0 do
            FaceGroup.VertexIndices.Delete(FaceGroup.VertexIndices.Count - 1);
          Triangles := Triangles.Next;
          FaceGroup.Reverse;

        end;
        RotQuat := QuaternionMake([Node^.Rotation.Z, Node^.Rotation.Y,
          Node^.Rotation.W], Node^.Rotation.X);
        RotMat := QuaternionToMatrix(RotQuat);
        Mo.Vertices.TransformAsVectors(RotMat);
        (*
          mo.SetPosition( Node^.Position[1], Node^.Position[0], Node^.Position[2]);
          mo.SetScale( Node^.Scale[1], Node^.Scale[0], Node^.Scale[2]);
        *)
        if Pos('ENT_', UpperCase(Mo.Name)) = 0 then
          V := AffineVectorMake(Node^.Position.Y,
            Node^.Position.X, Node^.Position.Z)
        else
        begin
          V := AffineVectorMake(0.0, 0.0, 0.0);
        end;
        V1 := AffineVectorMake(Node^.Scale.Y, Node^.Scale.X, Node^.Scale.Z);
        Matrix := CreateScaleAndTranslationMatrix(VectorMake(V1), VectorMake(V));
        Mo.Vertices.TransformAsPoints(Matrix);
      end;
      Node := Node^.Next;
    end;
  finally
    B3d.Free;
  end;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

RegisterVectorFileFormat('b3d', 'Blitz 3D model files', TGLB3DVectorFile);

end.
