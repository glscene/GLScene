//
// The graphics rendering engine GLScene http://glscene.org
//
unit GLS.FilePLY;

(* PLY (Stanford Triangle Format) vector file format implementation *)

interface

{$I GLScene.inc}

uses
  System.Classes,
  System.SysUtils,

  GLS.VectorGeometry,
  GLS.VectorLists,
  GLS.VectorFileObjects,
  GLS.ApplicationFileIO,
  GLS.Utils;

type
  (* The PLY vector file aka Stanford Triangle Format.
    This is a format for storing graphical objects that are described as a
    collection of polygons. The format is extensible, supports variations and
    subformats. This importer only works for the simplest variant (triangles
    without specified normals, and will ignore most header specifications. *)
  TGLPLYVectorFile = class(TGLVectorFile)
  public
    class function Capabilities: TGLDataFileCapabilities; override;
    procedure LoadFromStream(aStream: TStream); override;
  end;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

// ------------------
// ------------------ TGLPLYVectorFile ------------------
// ------------------

class function TGLPLYVectorFile.Capabilities: TGLDataFileCapabilities;
begin
  Result := [dfcRead];
end;

procedure TGLPLYVectorFile.LoadFromStream(aStream: TStream);
var
  i, nbVertices, nbFaces: Integer;
  sl: TStringList;
  mesh: TGLMeshObject;
  fg: TFGVertexIndexList;
  p: PChar;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromStream(aStream{$IFDEF Unicode}, TEncoding.ASCII{$ENDIF});
    mesh := TGLMeshObject.CreateOwned(Owner.MeshObjects);
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
      // AffineVectorMake(GLStrToFloatDef(tl[0]), GLStrToFloatDef(tl[1]), GLStrToFloatDef(tl[2])));}
      Dec(nbVertices);
      Inc(i);
    end;
    // faces
    fg := TFGVertexIndexList.CreateOwned(mesh.FaceGroups);
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

RegisterVectorFileFormat('ply', 'Stanford triangle format', TGLPLYVectorFile);

end.
