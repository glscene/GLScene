//
// The graphics rendering engine GLScene http://glscene.org
//

unit GLS.FileNMF;

(*
  NormalMapper loading into GLScene FreeForms/Actors

  Notes:
  NormalMapper can be found at http://www.ati.com/developer/tools.html
*)

interface

uses
  System.Classes,

  GLS.VectorTypes,
  GLS.VectorFileObjects,
  GLS.PersistentClasses,
  GLS.VectorGeometry,
  GLS.VectorLists,
  GLS.ApplicationFileIO;

const
  NMF_HEADER_TAG = 'NMF ';
  NMF_TRIANGLE_TAG = 'TRIS';

type
  TNmHeader = record
    hdr: array [0 .. 3] of AnsiChar;
    size: cardinal;
  end;

  TNmRawTriangle = record
    vert, norm: array [0 .. 2] of TAffineVector;
    texCoord: array [0 .. 2] of TTexPoint;
  end;

  TFileNMF = class
  public
    FileHeader, TrisHeader: TNmHeader;
    NumTris: Integer;
    RawTriangles: array of TNmRawTriangle;

    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
  end;

type
  TGLNMFVectorFile = class(TGLVectorFile)
  public
    class function Capabilities: TGLDataFileCapabilities; override;
    procedure LoadFromStream(aStream: TStream); override;
    procedure SaveToStream(aStream: TStream); override;
  end;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

procedure TFileNMF.LoadFromStream(Stream: TStream);
var
  Done: Boolean;
begin
  Stream.Read(FileHeader, SizeOf(TNmHeader));
  if FileHeader.hdr <> NMF_HEADER_TAG then
    exit;

  Done := False;
  while not Done do
  begin
    Stream.Read(TrisHeader, SizeOf(TNmHeader));
    if TrisHeader.hdr = NMF_TRIANGLE_TAG then
    begin
      Stream.Read(NumTris, SizeOf(NumTris));
      if NumTris < 0 then
        exit;
      SetLength(RawTriangles, NumTris);
      Stream.Read(RawTriangles[0], SizeOf(TNmRawTriangle) * NumTris);
      Done := True;
    end;
  end;
end;

procedure TFileNMF.SaveToStream(Stream: TStream);
begin
  NumTris := Length(RawTriangles);
  TrisHeader.hdr := NMF_TRIANGLE_TAG;
  TrisHeader.size := SizeOf(TNmRawTriangle) * NumTris + SizeOf(FileHeader);
  FileHeader.hdr := NMF_HEADER_TAG;
  FileHeader.size := TrisHeader.size + SizeOf(TrisHeader);

  Stream.Write(FileHeader, SizeOf(TNmHeader));
  Stream.Write(TrisHeader, SizeOf(TNmHeader));
  NumTris := Length(RawTriangles);
  Stream.Write(NumTris, SizeOf(NumTris));
  Stream.Write(RawTriangles[0], SizeOf(TNmRawTriangle) * NumTris);
end;

// ------------------
// ------------------ TGLNMFVectorFile ------------------
// ------------------

class function TGLNMFVectorFile.Capabilities: TGLDataFileCapabilities;
begin
  Result := [dfcRead, dfcWrite];
end;

procedure TGLNMFVectorFile.LoadFromStream(aStream: TStream);
var
  i, j: Integer;
  mesh: TMeshObject;
  nmf: TFileNMF;
begin
  nmf := TFileNMF.Create;
  try
    nmf.LoadFromStream(aStream);
    mesh := TMeshObject.CreateOwned(Owner.MeshObjects);
    mesh.Mode := momTriangles;
    for i := 0 to nmf.NumTris - 1 do
    begin
      for j := 0 to 2 do
      begin
        mesh.Vertices.Add(nmf.RawTriangles[i].vert[j]);
        mesh.Normals.Add(nmf.RawTriangles[i].norm[j]);
        mesh.TexCoords.Add(nmf.RawTriangles[i].texCoord[j]);
      end;
    end;
  finally
    nmf.Free;
  end;
end;

procedure TGLNMFVectorFile.SaveToStream(aStream: TStream);
var
  i, j: Integer;
  nmf: TFileNMF;
  Vertices, TempVertices, Normals, TexCoords: TAffineVectorList;
begin
  nmf := TFileNMF.Create;
  Vertices := TAffineVectorList.Create;
  Normals := TAffineVectorList.Create;
  TexCoords := TAffineVectorList.Create;
  try
    for i := 0 to Owner.MeshObjects.Count - 1 do
    begin
      TempVertices := Owner.MeshObjects[i].ExtractTriangles(TexCoords, Normals);
      Vertices.Add(TempVertices);
      TempVertices.Free;
    end;

    nmf.NumTris := (Vertices.Count div 3);
    SetLength(nmf.RawTriangles, nmf.NumTris);
    for i := 0 to nmf.NumTris - 1 do
    begin
      for j := 0 to 2 do
      begin
        nmf.RawTriangles[i].vert[j] := Vertices[3 * i + j];
        nmf.RawTriangles[i].norm[j] := Normals[3 * i + j];
        nmf.RawTriangles[i].texCoord[j].S := TexCoords[3 * i + j].X;
        nmf.RawTriangles[i].texCoord[j].T := TexCoords[3 * i + j].Y;
      end;
    end;
    nmf.SaveToStream(aStream);
  finally
    Vertices.Free;
    Normals.Free;
    TexCoords.Free;
    nmf.Free;
  end;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

RegisterVectorFileFormat('nmf', 'NormalMapper files', TGLNMFVectorFile);

end.
