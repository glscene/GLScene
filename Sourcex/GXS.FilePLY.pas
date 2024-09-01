//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.FilePLY;

(* PLY (Stanford Triangle Format) vector file format implementation *)

interface

{$I GXS.Scene.inc}

uses
  System.Classes,
  System.SysUtils,
  GXS.VectorFileObjects,
  GXS.ApplicationFileIO,
  GXS.Utils;

type
  (* The PLY vector file aka Stanford Triangle Format.
    This is a format for storing graphical objects that are described as a
    collection of polygons. The format is extensible, supports variations and
    subformats. This importer only works for the simplest variant (triangles
    without specified normals, and will ignore most header specifications. *)
  TgxPLYVectorFile = class(TgxVectorFile)
  public
    class function Capabilities: TDataFileCapabilities; override;
    procedure LoadFromStream(aStream: TStream); override;
  end;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

// ------------------
// ------------------ TgxPLYVectorFile ------------------
// ------------------

class function TgxPLYVectorFile.Capabilities: TDataFileCapabilities;
begin
  Result := [dfcRead];
end;

procedure TgxPLYVectorFile.LoadFromStream(aStream: TStream);
var
  i, nbVertices, nbFaces: Integer;
  sl: TStringList;
  mesh: TgxMeshObject;
  fg: TfgxVertexIndexList;
  p: PChar;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromStream(aStream{$IFDEF Unicode}, TEncoding.ASCII{$ENDIF});
    mesh := TgxMeshObject.CreateOwned(Owner.MeshObjects);
    mesh.Mode := momFaceGroups;
    if sl[0] <> 'ply' then
      raise Exception.Create('Not a valid ply file !');
    nbVertices := 0;
    nbFaces := 0;
    i := 0;
    while i < sl.Count do
    begin
      if sl[i] = 'end_header' then
        Break;
      if Copy(sl[i], 1, 14) = 'element vertex' then
        nbVertices := StrToIntDef(Copy(sl[i], 16, MaxInt), 0);
      if Copy(sl[i], 1, 12) = 'element face' then
        nbFaces := StrToIntDef(Copy(sl[i], 14, MaxInt), 0);
      Inc(i);
    end;
    Inc(i);
    // vertices
    mesh.Vertices.Capacity := nbVertices;
    while (i < sl.Count) and (nbVertices > 0) do
    begin
      p := PChar(sl[i]);
      mesh.Vertices.Add(ParseFloat(p), ParseFloat(p), ParseFloat(p));
      // AffineVectorMake(StrToFloatDef(tl[0]), StrToFloatDef(tl[1]), StrToFloatDef(tl[2])));}
      Dec(nbVertices);
      Inc(i);
    end;
    // faces
    fg := TfgxVertexIndexList.CreateOwned(mesh.FaceGroups);
    fg.Mode := fgmmTriangles;
    fg.VertexIndices.Capacity := nbFaces * 3;
    while (i < sl.Count) and (nbFaces > 0) do
    begin
      p := PChar(sl[i]);
      ParseInteger(p); // skip index
      fg.VertexIndices.Add(ParseInteger(p), ParseInteger(p), ParseInteger(p));
      Dec(nbFaces);
      Inc(i);
    end;
    mesh.BuildNormals(fg.VertexIndices, momTriangles);
  finally
    sl.Free;
  end;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

RegisterVectorFileFormat('ply', 'Stanford triangle format', TgxPLYVectorFile);

end.
