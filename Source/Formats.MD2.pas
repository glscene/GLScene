//
// The graphics rendering engine GLScene http://glscene.org
//

unit Formats.MD2;

(* Loading methods for MD2 file format *)

interface

{$R-}
{$I GLScene.inc}

uses
  System.Classes,
  System.SysUtils,
   
  GLS.VectorGeometry, 
  GLS.VectorTypes;

const
  MAX_MD2_TRIANGLES = 4096;
  MAX_MD2_VERTICES = 2048;
  MAX_MD2_FRAMES = 512;
  MAX_MD2_SKINS = 32;
  MAX_MD2_SKINNAME = 64;

type
  PMD2VertexIndex = ^TMD2VertexIndex;
  TMD2VertexIndex = record
    A, B, C: integer;
    A_S, A_T,
    B_S, B_T,
    C_S, C_T: single;
  end;

  TMD2Triangle = record
    VertexIndex: TVector3s;
    TextureCoordIndex: TVector3s;
  end;

  TMD2TriangleVertex = record
    Vert: array[0..2] of byte;
    LightnormalIndex: byte;
  end;

  PMD2AliasFrame = ^TMD2AliasFrame;
  TMD2AliasFrame = record
    Scale: TVector3f;
    Translate: TVector3f;
    Name: array[0..15] of AnsiChar;
    Vertices: array[0..0] of TMD2TriangleVertex;
  end;

  TMD2Header = record
    Ident: integer;
    Version: integer;

    SkinWidth: integer;
    SkinHeight: integer;
    FrameSize: integer;

    Num_Skins: integer;
    Num_Vertices: integer;
    Num_TextureCoords: integer;
    Num_VertexIndices: integer;
    Num_GLCommdands: integer;
    Num_Frames: integer;

    Offset_skins: integer;
    Offset_st: integer;
    Offset_tris: integer;
    Offset_frames: integer;
    Offset_glcmds: integer;
    Offset_end: integer;
  end;

  TIndexList = array of TMD2VertexIndex;
  TGLVertexList = array of array of TVector3f;


type
  TFileMD2 = class
  private
    FiFrames: longint;
    FiVertices: longint;
    FiTriangles: longint;
    procedure FreeLists;
  public
    fIndexList : TIndexList;
    fVertexList : TGLVertexList;
    FrameNames : TStrings;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure LoadFromStream(aStream : TStream);
    property iFrames: longInt read FiFrames;
    property iVertices: longInt read FiVertices;
    property iTriangles: longInt read FiTriangles;

    property IndexList: TIndexList read fIndexList;
    property VertexList: TGLVertexList read fVertexList;
  end;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

// ------------------
// ------------------ TFileMD2 ------------------
// ------------------

constructor TFileMD2.Create;
begin
  inherited;
  FreeLists;
  FrameNames := TStringList.Create;
end;

destructor TFileMD2.Destroy;
begin
  FreeLists;
  FrameNames.Free;
  inherited;
end;

procedure TFileMD2.FreeLists;
begin
  SetLength(fIndexList,0);
  SetLength(fVertexList,0,0);
  FiFrames := 0;
  FiVertices := 0;
  FiTriangles := 0;
end;

procedure TFileMD2.LoadFromStream(aStream : TStream);
var
  Skins: array[0..MAX_MD2_SKINS - 1, 0..63] of AnsiChar;
  TextureCoords: array[0..MAX_MD2_VERTICES - 1] of TVector2s;
  Buffer: array[0..MAX_MD2_VERTICES * 4 + 127] of byte;
  Header: TMD2Header;
  Triangle: TMD2Triangle;
  I: integer;
  J: integer;
  Frame: PMD2AliasFrame;
  FrameName : String;

begin
  FreeLists;
  // read the modelinfo
  aStream.Read(Header, SizeOf(Header));
  FiFrames := Header.Num_Frames;
  FiVertices := Header.Num_Vertices;
  FiTriangles := Header.Num_VertexIndices;
  SetLength(fIndexList, FiTriangles);
  SetLength(fVertexList, FiFrames, FiVertices);
  // get the skins...
  aStream.Read(Skins, Header.Num_Skins * MAX_MD2_SKINNAME);
  // ...and the texcoords
  aStream.Read(TextureCoords, Header.Num_TextureCoords * SizeOf(TVector2s));
  for I := 0 to Header.Num_VertexIndices - 1 do begin
    aStream.Read(Triangle, SizeOf(TMD2Triangle));
     with fIndexList[I] do begin
      A := Triangle.VertexIndex.Z;
      B := Triangle.VertexIndex.Y;
      C := Triangle.VertexIndex.X;
      A_S := TextureCoords[Triangle.TextureCoordIndex.Z].X / Header.SkinWidth;
      A_T := TextureCoords[Triangle.TextureCoordIndex.Z].Y / Header.SkinHeight;
      B_S := TextureCoords[Triangle.TextureCoordIndex.Y].X / Header.SkinWidth;
      B_T := TextureCoords[Triangle.TextureCoordIndex.Y].Y / Header.SkinHeight;
      C_S := TextureCoords[Triangle.TextureCoordIndex.X].X / Header.SkinWidth;
      C_T := TextureCoords[Triangle.TextureCoordIndex.X].Y / Header.SkinHeight;
    end;
  end;
  for I := 0 to Header.Num_Frames - 1 do begin
    Frame := PMD2AliasFrame(@Buffer);
    // read animation / frame info
    aStream.Read(Frame^, Header.FrameSize);
    FrameName := Trim(String(Frame^.Name));
    if CharInSet(Copy(FrameName, Length(FrameName) - 1, 1)[1], ['0'..'9']) then
      FrameName := Copy(FrameName, 1, Length(FrameName) - 2)
    else
      FrameName := Copy(FrameName, 1, Length(FrameName) - 1);
    if FrameNames.IndexOf(FrameName) < 0 then
      FrameNames.AddObject(FrameName, TObject(Cardinal(I)));
    // fill the vertices list
     for J := 0 to FiVertices - 1 do begin
       fVertexList[i][J].X := Frame^.Vertices[J].Vert[0] * Frame^.Scale.X + Frame^.Translate.X;
       fVertexList[i][J].Y := Frame^.Vertices[J].Vert[1] * Frame^.Scale.Y + Frame^.Translate.Y;
       fVertexList[i][J].Z := Frame^.Vertices[J].Vert[2] * Frame^.Scale.Z + Frame^.Translate.Z;
     end;
  end;
end;

end.
