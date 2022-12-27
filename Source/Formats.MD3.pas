//
// The multimedia graphics platform GLScene https://github.com/glscene
//

unit Formats.MD3;

(* File loading methods for the MD3 file format *)

interface

uses
  System.Classes, 
  GLS.VectorTypes;

type
  // Quake3 MD3 structure types
  TMD3Tag = record
    strName: array[0..63] of AnsiChar;
    vPosition: TVector3f;
    rotation: TMatrix3f;
  end;

  (* This part of the MD3 structure called 2 things:
    A frame and a bone. It doesn't matter because we don't use it *)
  (*
    TMD3Frame = record
    min_bound,max_bounds,
    local_origin  : TVector3f;
    radius        : single;
    name          : array[0..15] of char;
  end;
  *)
  TMD3Bone = record
    mins,maxs,
    position: TVector3f;
    scale: single;
    creator: array[0..15] of AnsiChar;
  end;

  TMD3Triangle = record
    vertex: TVector3s; // value/64 to get real number position
    normal: TVector2b;     // Latitude,Longitude
  end;

  TMD3Face = record
    vertexIndices: TVector3i;
  end;

  TMD3TexCoord = record
    textureCoord: TVector2f;
  end;

  TMD3Skin = record
    strName : array[0..63] of AnsiChar;
    shaderIndex: Integer;
  end;

  TMD3Header = record
    fileID     : array[0..3] of AnsiChar;
    version    : integer;
    strFile    : array[0..63] of AnsiChar;
    flags,
    numFrames,
    numTags,
    numMeshes,
    numMaxSkins,
    headerSize,
    tagStart,
    tagEnd,
    fileSize   : integer;
  end;

  TMD3MeshHeader = record
    meshID        : array[0..3] of AnsiChar;
    strName       : array[0..63] of AnsiChar;
    flags,
    numMeshFrames,
    numSkins,
    numVertices,
    numTriangles,
    triStart,
    headerSize,
    uvStart,
    vertexStart,
    meshSize      : integer;
  end;

  TMD3MeshData = record
    MeshHeader : TMD3MeshHeader;
    Skins      : array of TMD3Skin;
    Triangles  : array of TMD3Face;
    TexCoords  : array of TMD3TexCoord;
    Vertices   : array of TMD3Triangle;
  end;

  // MD3 Main file class

  TFileMD3 = class
    public
      ModelHeader: TMD3Header;
      Bones    : array of TMD3Bone;
      Tags     : array of TMD3Tag;
      MeshData : array of TMD3MeshData;

      procedure LoadFromStream(aStream : TStream);
  end;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

// ------------------
// ------------------ TFileMD3 ------------------
// ------------------

procedure TFileMD3.LoadFromStream(aStream : TStream);
var
  i : Integer;
  meshOffset : LongInt;
begin
  aStream.Read(ModelHeader, sizeof(ModelHeader));

  // Test for correct file ID and version
  Assert(ModelHeader.fileID='IDP3','Incorrect MD3 file ID');
  Assert(ModelHeader.version=15,'Incorrect MD3 version number');

  // Read in the bones
  SetLength(Bones,ModelHeader.numFrames);
  aStream.Read(Bones[0],sizeof(TMD3Bone)*ModelHeader.numFrames);

  // Read in the Tags
  SetLength(Tags,ModelHeader.numFrames*ModelHeader.numTags);
  if ModelHeader.numTags > 0 then 
     aStream.Read(Tags[0],sizeof(TMD3Tag)*ModelHeader.numFrames*ModelHeader.numTags);

  // Read in the Mesh data
  meshOffset:=aStream.Position;
  SetLength(MeshData,ModelHeader.numMeshes);
  for i:=0 to ModelHeader.numMeshes-1 do begin
    with MeshData[i] do begin
      aStream.Position:=meshOffset;
      aStream.Read(MeshHeader,sizeof(MeshHeader));
      // Set up the dynamic arrays
      SetLength(Skins,MeshHeader.numSkins);
      SetLength(Triangles,MeshHeader.numTriangles);
      SetLength(TexCoords,MeshHeader.numVertices);
      SetLength(Vertices,MeshHeader.numVertices*MeshHeader.numMeshFrames);
      // Skins
      aStream.read(Skins[0],sizeof(TMD3Skin)*MeshHeader.numSkins);
      // Face data
      aStream.Position:=meshOffset+MeshHeader.triStart;
      aStream.read(Triangles[0],sizeof(TMD3Face)*MeshHeader.numTriangles);
      // Texture coordinates
      aStream.Position:=meshOffset+meshHeader.uvStart;
      aStream.read(TexCoords[0],sizeof(TMD3TexCoord)*meshHeader.numVertices);
      // Vertices
      aStream.Position:=meshOffset+meshHeader.vertexStart;
      aStream.read(Vertices[0],sizeof(TMD3Triangle)*MeshHeader.numMeshFrames*MeshHeader.numVertices);
      // Increase the offset
      meshOffset:=meshOffset+MeshHeader.meshSize;
    end;
  end;
end;

end.