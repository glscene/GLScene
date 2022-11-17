//
// The graphics rendering engine GLScene http://glscene.org
//
unit GLS.FileVOR;
(*
  Supports to import TetGen files from http://wias-berlin.de/software/tetgen/fformats.html 
  combined in a VOR ASCII file for Voronoi polyhedralization.
*)
interface

uses
  System.Classes,
  System.SysUtils,
  GLS.ApplicationFileIO,

  GLS.VectorTypes,
  GLS.VectorGeometry,
  GLS.VectorLists,
  GLS.VectorFileObjects,
  GLS.Utils;

type
  TVORHeader = packed record
     dummy : array[0..79] of byte;
     nbFaces : Longint;
  end;

  TVORVertex = TAffineVector;

  TVORFace = packed record
	   normal : TVORVertex;	// facet surface normal
	   v1 : TVORVertex;	// vertex 1
	   v2 : TVORVertex;	// vertex 2
	   v3 : TVORVertex;	// vertex 3
      padding : array[0..1] of byte;
   end;

type

  TGLVORVectorFile = class(TGLVectorFile)
  public
    class function Capabilities: TGLDataFileCapabilities; override;
    procedure LoadFromStream(aStream: TStream); override;
    procedure SaveToStream(aStream: TStream); override;
  end;

{$IFDEF USE_MULTITHREAD}
  threadvar
{$ELSE}
  var
{$ENDIF}
  VORUseEmbeddedColors: Boolean;
  
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

const
  cNODE_LABEL = 'node';
  cEDGE_LABEL = 'edge';
  cFACE_LABEL = 'face';
  cCELL_LABEL = 'cell';

// ------------------
// ------------------ TGLVectorFile ------------------
// ------------------

class function TGLVORVectorFile.Capabilities: TGLDataFileCapabilities;
begin
   Result := [dfcRead, dfcWrite];
end;

procedure TGLVORVectorFile.LoadFromStream(aStream: TStream);
var
  Sl: TStringList;

  procedure _DecodeVORNormals(const aString: String; var aNormal: TVORVertex);
  begin
    Sl.CommaText := aString;
    if Sl.Count <> 5 then
      raise Exception.Create('Invalid Normal')
    else
    begin
      aNormal.X := GLStrToFloatDef(Sl[2], 0);
      aNormal.Y := GLStrToFloatDef(Sl[3], 0);
      aNormal.Z := GLStrToFloatDef(Sl[4], 0);
    end;
  end;

  procedure _DecodeVORVertex(const aString: String; var aVertex: TVORVertex);
  begin
    Sl.CommaText := aString;
    if (Sl.Count <> 4) or (CompareText(Sl[0], cNODE_LABEL) <> 0) then
      raise Exception.Create('Invalid Vertex')
    else
    begin
      aVertex.X := GLStrToFloatDef(Sl[1], 0);
      aVertex.Y := GLStrToFloatDef(Sl[2], 0);
      aVertex.Z := GLStrToFloatDef(Sl[3], 0);
    end;
  end;

var
  R: Byte;
  G: Byte;
  B: Byte;
  I: Integer;
  L: Integer;
  CurLine: string;
  Mesh: TGLMeshObject;
  DataFace: TVORFace;
  Header: TVORHeader;
  FileContent: TStringList;
  CalcNormal: TAffineVector;

  // NOTE ABOUT VOR:
  //  The minimum size of an empty ASCII file is 15 bytes.
  //  Each facet contains:
  //    - Normals: 3 floats (4 bytes)
  //    - Vertices: 3x floats (4 byte each, 12 bytes total)
  //    - AttributeCount: 1 short (2 bytes)
  //  Total: 50 bytes per facet

begin
  // create mesh object
  Mesh := TGLMeshObject.CreateOwned(Owner.MeshObjects);
  try
    Mesh.Mode := momTriangles;
    begin
      // ASCII VOR READER
      FileContent := TStringList.Create;
      Sl := TStringList.Create;
      try
        FileContent.LoadFromStream(AStream);
        I := 0;
        CurLine := Trim(UpperCase(FileContent[I]));
        if Pos(cNODE_LABEL, CurLine) = 1 then
        begin
          Mesh.Vertices.Capacity := (FileContent.Count - 2) div 7;
          Mesh.Normals.Capacity := (FileContent.Count -2) div 7;
          Inc(I);
          CurLine := Trim(UpperCase(FileContent[I]));
          while I < FileContent.Count do
          begin
            if Pos(cEDGE_LABEL, CurLine) = 1 then
            begin
              _DecodeVORNormals(CurLine, DataFace.normal);
              Inc(I);
              CurLine := Trim(UpperCase(FileContent[I]));
              if Pos(cFACE_LABEL, CurLine) = 1 then
              begin
                Inc(I);
                CurLine := Trim(FileContent[I]);
                _DecodeVORVertex(CurLine, DataFace.v1);
                Inc(I);
                CurLine := Trim(FileContent[I]);
                _DecodeVORVertex(CurLine, DataFace.v2);
                Inc(I);
                CurLine := Trim(FileContent[I]);
                _DecodeVORVertex(CurLine, DataFace.v3);
              end;
              Inc(I);
              CurLine := Trim(UpperCase(FileContent[I]));
              if Pos(cCELL_LABEL, CurLine) <> 1 then
                raise Exception.Create('End of Loop Not Found')
              else
              begin
                CalcNormal := CalcPlaneNormal(DataFace.v1, DataFace.v2, DataFace.v3);
                if VectorDotProduct(CalcNormal, DataFace.normal) > 0 then
                  Mesh.Vertices.Add(DataFace.v1, DataFace.v2, DataFace.v3)
                else
                  Mesh.Vertices.Add(DataFace.v3, DataFace.v2, DataFace.v1);
                Mesh.Normals.Add(DataFace.normal, DataFace.normal, DataFace.normal);
              end;
            end;
            Inc(I);
            CurLine := Trim(UpperCase(FileContent[I]));
            if Pos(cCELL_LABEL, curLine) <> 1 then
              raise Exception.Create('End of Facet Not found');
            Inc(I);
            CurLine := Trim(UpperCase(FileContent[I]));
            if Pos(cNODE_LABEL, curLine) = 1 then Break;
          end;
        end;
        finally
          Sl.Free;
          FileContent.Free;
        end;
      end;
   except
      on E: Exception do
         Mesh.Free;
   end;
end;

procedure TGLVORVectorFile.SaveToStream(aStream: TStream);
var
  I: Integer;
  DataFace: TVORFace;
  Header: TVORHeader;
  List: TGLAffineVectorList;
const
  cHeaderTag = 'VOR export';
begin
   List := Owner.MeshObjects.ExtractTriangles;
   try
      FillChar(Header.dummy[0], SizeOf(Header.dummy), 0);
      Move(cHeaderTag, Header.dummy[0], Length(cHeaderTag));
      Header.nbFaces := List.Count div 3;
      aStream.Write(Header, SizeOf(Header));
      I := 0;
      while I <list.Count do
      begin
         DataFace.normal := CalcPlaneNormal(List[I], List[I + 1], List[I + 2]);
         DataFace.v1 := List[I];
         DataFace.v2 := List[I + 1];
         DataFace.v3 := List[I + 2];
         aStream.Write(DataFace, SizeOf(DataFace));
         Inc(I, 3);
      end;
   finally
      list.Free;
   end;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

  VORUseEmbeddedColors := False;
  RegisterVectorFileFormat('voronoi', 'Voronoi files', TGLVORVectorFile);

end.