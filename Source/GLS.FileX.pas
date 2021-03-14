//
// The graphics rendering engine GLScene http://glscene.org
//
unit GLS.FileX;

(* Support of X format files for Microsoft's favorite format *)

interface

{$I GLScene.inc}

uses
  System.Classes,
  System.SysUtils,

  GLS.VectorTypes,
  GLS.VectorFileObjects,
  GLS.ApplicationFileIO,
  GLS.VectorGeometry,
  GLS.Texture,
  GLS.VectorLists,
  GLS.Material,

  Formats.X;

type
  TGLXVectorFile = class(TGLVectorFile)
  public
    class function Capabilities: TGLDataFileCapabilities; override;
    procedure LoadFromStream(aStream: TStream); override;
  end;

// -------------------------------------------------------------
implementation
// -------------------------------------------------------------

class function TGLXVectorFile.Capabilities: TGLDataFileCapabilities;
begin
  Result := [dfcRead];
end;

procedure TGLXVectorFile.LoadFromStream(aStream: TStream);
var
  DXFile: TDXFile;

  procedure RecursDXFile(DXNode: TDXNode);
  var
    i, j, k, l, vertcount: integer;
    mo: TMeshObject;
    mat: TGLMatrix;
    libmat: TGLLibMaterial;
    fg: TFGVertexNormalTexIndexList;
    str: String;
  begin
    mat := IdentityHMGMatrix;
    if Assigned(DXNode.Owner) then
      if DXNode.Owner is TDXFrame then
      begin
        mat := TDXFrame(DXNode.Owner).GlobalMatrix;
        TransposeMatrix(mat);
      end;

    if DXNode is TDXMesh then
    begin
      mo := TMeshObject.CreateOwned(Owner.MeshObjects);
      mo.Mode := momFaceGroups;
      mo.Vertices.Assign(TDXMesh(DXNode).Vertices);
      mo.Vertices.TransformAsPoints(mat);
      mo.Normals.Assign(TDXMesh(DXNode).Normals);
      mo.Normals.TransformAsVectors(mat);
      mo.TexCoords.Assign(TDXMesh(DXNode).TexCoords);

      if TDXMesh(DXNode).MaterialList.Count > 0 then
      begin
        // Add the materials
        if (Owner.UseMeshMaterials) and Assigned(Owner.MaterialLibrary) then
        begin
          for i := 0 to TDXMesh(DXNode).MaterialList.Count - 1 do
          begin
            str := TDXMesh(DXNode).MaterialList.Items[i].Texture;
            if FileExists(str) then
              libmat := Owner.MaterialLibrary.AddTextureMaterial('', str)
            else
              libmat := Owner.MaterialLibrary.Materials.Add;

            libmat.Name := Format('%s_%d', [DXNode.Name, i]);
            with libmat.Material.FrontProperties do
            begin
              Diffuse.Color := TDXMesh(DXNode).MaterialList.Items[i].Diffuse;
              Shininess := Round(TDXMesh(DXNode).MaterialList.Items[i]
                .SpecPower / 2);
              Specular.Color :=
                VectorMake(TDXMesh(DXNode).MaterialList.Items[i].Specular);
              Emission.Color :=
                VectorMake(TDXMesh(DXNode).MaterialList.Items[i].Emissive);
            end;
          end;
        end;

        // Add the facegroups (separate since material library
        // can be unassigned)
        for i := 0 to TDXMesh(DXNode).MaterialList.Count - 1 do
        begin
          fg := TFGVertexNormalTexIndexList.CreateOwned(mo.FaceGroups);
          fg.MaterialName := Format('%s_%d', [DXNode.Name, i]);
        end;

        // Now add the indices per material
        vertcount := 0;
        for i := 0 to TDXMesh(DXNode).VertCountIndices.Count - 1 do
        begin
          if (i < TDXMesh(DXNode).MaterialIndices.Count) then
          begin
            j := TDXMesh(DXNode).MaterialIndices[i];
            k := TDXMesh(DXNode).VertCountIndices[i];
            for l := vertcount to vertcount + k - 1 do
            begin
              TFGVertexNormalTexIndexList(mo.FaceGroups[j])
                .VertexIndices.Add(TDXMesh(DXNode).VertexIndices[l]);
              if mo.TexCoords.Count > 0 then
                TFGVertexNormalTexIndexList(mo.FaceGroups[j])
                  .TexCoordIndices.Add(TDXMesh(DXNode).VertexIndices[l]);
              if TDXMesh(DXNode).NormalIndices.Count > 0 then
                TFGVertexNormalTexIndexList(mo.FaceGroups[j])
                  .NormalIndices.Add(TDXMesh(DXNode).NormalIndices[l])
            end;
            vertcount := vertcount + k;
          end;
        end;
      end
      else
      begin
        fg := TFGVertexNormalTexIndexList.CreateOwned(mo.FaceGroups);
        fg.NormalIndices.Assign(TDXMesh(DXNode).NormalIndices);
        fg.VertexIndices.Assign(TDXMesh(DXNode).VertexIndices);
      end;

    end;

    for i := 0 to DXNode.Count - 1 do
      RecursDXFile(TDXNode(DXNode[i]));
  end;

begin
  DXFile := TDXFile.Create;
  try
    DXFile.LoadFromStream(aStream);
    RecursDXFile(DXFile.RootNode);
  finally
    DXFile.Free;
  end;
end;

//--------------------------------------------------------
initialization
//--------------------------------------------------------

RegisterVectorFileFormat('x', 'DirectX Model files', TGLXVectorFile);

end.
