//
// The graphics rendering engine GLScene http://glscene.org
//
unit GLS.FileSTL;
(*
  Support-code to load STL Files into TGLFreeForm-Components in GLScene.
  Note that you must manually add this unit to one of your project's uses
  to enable support for STL files at run-time.

  The STL vector file (stereolithography format).
  It is a list of the triangular surfaces that describe a computer generated solid model.
  This is the standard input for most rapid prototyping machines.
  There are two flavors of STL, the "text" and the "binary".
  This class reads both, but exports only the "binary" version.
  Original Binary importer code by Paul M. Bearne, Text importer by Adem.
*)
interface

uses
  System.Classes,
  System.SysUtils,
  GLS.ApplicationFileIO,

  GLS.VectorTypes,
  GLS.VectorGeometry,
  GLS.VectorLists,
  GLS.VectorFileObjects;

type
  TSTLHeader = packed record
     dummy : array[0..79] of byte;
     nbFaces : Longint;
  end;

  TSTLVertex = TAffineVector;

  TSTLFace = packed record
	   normal : TSTLVertex;	// facet surface normal
	   v1 : TSTLVertex;	// vertex 1
	   v2 : TSTLVertex;	// vertex 2
	   v3 : TSTLVertex;	// vertex 3
      padding : array[0..1] of byte;
   end;

type

  TglSTLVectorFile = class(TglVectorFile)
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
  STLUseEmbeddedColors: Boolean;
  
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

const
  cSOLID_LABEL = 'SOLID';
  cFACETNORMAL_LABEL = 'FACET NORMAL ';
  cOUTERLOOP_LABEL = 'OUTER LOOP';
  cVERTEX_LABEL = 'VERTEX';
  cENDLOOP_LABEL = 'ENDLOOP';
  cENDFACET_LABEL = 'ENDFACET';
  cENDSOLID_LABEL = 'ENDSOLID';
  cFULL_HEADER_LEN = 84;

// ------------------
// ------------------ TGLSTLVectorFile ------------------
// ------------------

class function TGLSTLVectorFile.Capabilities: TGLDataFileCapabilities;
begin
   Result := [dfcRead, dfcWrite];
end;

procedure TGLSTLVectorFile.LoadFromStream(aStream: TStream);
var
  Sl: TStringList;

  procedure _DecodeSTLNormals(const aString: String; var aNormal: TSTLVertex);
  begin
    Sl.CommaText := aString;
    if Sl.Count <> 5 then
      raise Exception.Create('Invalid Normal')
    else
    begin
      aNormal.X := StrToFloatDef(Sl[2], 0);
      aNormal.Y := StrToFloatDef(Sl[3], 0);
      aNormal.Z := StrToFloatDef(Sl[4], 0);
    end;
  end;

  procedure _DecodeSTLVertex(const aString: String; var aVertex: TSTLVertex);
  begin
    Sl.CommaText := aString;
    if (Sl.Count <> 4) or (CompareText(Sl[0], cVERTEX_LABEL) <> 0) then
      raise Exception.Create('Invalid Vertex')
    else
    begin
      aVertex.X := StrToFloatDef(Sl[1], 0);
      aVertex.Y := StrToFloatDef(Sl[2], 0);
      aVertex.Z := StrToFloatDef(Sl[3], 0);
    end;
  end;

var
  R: Byte;
  G: Byte;
  B: Byte;
  I: Integer;
  L: Integer;
  CurLine: string;
  Mesh: TMeshObject;
  DataFace: TSTLFace;
  Header: TSTLHeader;
  FileContent: TStringList;
  CalcNormal: TAffineVector;

  // evaluates STL file header to detect if ascii or binary format
  //
  // NOTE ABOUT STL:
  //  The minimum size of an empty ASCII file is 15 bytes.
  //  In binary format each facet contains:
  //    - Normals: 3 floats (4 bytes)
  //    - Vertices: 3x floats (4 byte each, 12 bytes total)
  //    - AttributeCount: 1 short (2 bytes)
  //  Total: 50 bytes per facet
  function IsBinary: Boolean;
  const
    FACET_SIZE = 50;
  type
    TSTLHeader = packed record
      Dummy: array[0..79] of AnsiChar;
      Faces: Longint;
    end;
  var
    P: Int64;
    Header: TSTLHeader;
    PHeader: ^TSTLHeader;
  begin
    Result := True;
    try
      if (AStream.Size - AStream.Position) < SizeOf(TSTLHeader) then Abort;
      P := AStream.Position;
      PHeader := @Header;
      AStream.Read(PHeader^, SizeOf(Header));
      AStream.Position := P;
      if not
      (
        (Header.Dummy[0] = 's') and
        (Header.Dummy[1] = 'o') and
        (Header.Dummy[2] = 'l') and
        (Header.Dummy[3] = 'i') and
        (Header.Dummy[4] = 'd')
      ) then Exit;
      if AStream.Size <> (SizeOf(TSTLHeader) + (Header.Faces * FACET_SIZE)) then Abort;
      Result := True;
    except
      Result := False;
    end;
  end;

begin
  // create mesh object
  Mesh := TMeshObject.CreateOwned(Owner.MeshObjects);
  try
    Mesh.Mode := momTriangles;
    if IsBinary then
    begin
      // BINARY STL READER
      AStream.Read(Header, SizeOf(TSTLHeader));
      Mesh.Vertices.Capacity := Header.nbFaces * 3;
      Mesh.Normals.Capacity := Header.nbFaces * 3;
      Mesh.Colors.Capacity := Header.nbFaces * 3;
      for I := 0 to Header.nbFaces - 1 do
      begin
        aStream.Read(DataFace, SizeOf(TSTLFace));
        with DataFace do
        begin
          // STL faces have a normal, but do not necessarily follow the winding rule,
          // so we must first determine if the triangle is properly oriented and
          // rewind it properly if not...
          CalcNormal := CalcPlaneNormal(v1, v2, v3);
          if VectorDotProduct(CalcNormal, normal) > 0 then
            Mesh.Vertices.Add(v1, v2, v3)
          else
            Mesh.Vertices.Add(v3, v2, v1);
          Mesh.Normals.Add(normal, normal, normal);
          //
          // evaluates vertices colors for Fusion360 STL extended format
          //
          // https://en.wikipedia.org/wiki/STL_(file_format)#Color_in_binary_STL
          //
          if STLUseEmbeddedColors then
          begin
            L := padding[0] or (padding[1] shl 8);
            R := (L and $1F) shl 3;
            G := ((L shr 5) and $1F) shl 3;
            B := ((L shr 10) and $1F) shl 3;
            Mesh.Colors.Add(R / 255, G / 255, B / 255, 1.0);
            Mesh.Colors.Add(R / 255, G / 255, B / 255, 1.0);
            Mesh.Colors.Add(R / 255, G / 255, B / 255, 1.0);
          end;
        end;
      end;
      STLUseEmbeddedColors := False;
    end
    else
    begin
      // ASCII STL READER
      FileContent := TStringList.Create;
      Sl := TStringList.Create;
      try
        FileContent.LoadFromStream(AStream);
        I := 0;
        CurLine := Trim(UpperCase(FileContent[I]));
        if Pos(cSOLID_LABEL, CurLine) = 1 then
        begin
          Mesh.Vertices.Capacity := (FileContent.Count - 2) div 7;
          Mesh.Normals.Capacity := (FileContent.Count -2) div 7;
          Inc(I);
          CurLine := Trim(UpperCase(FileContent[I]));
          while I < FileContent.Count do
          begin
            if Pos(cFACETNORMAL_LABEL, CurLine) = 1 then
            begin
              _DecodeSTLNormals(CurLine, DataFace.normal);
              Inc(I);
              CurLine := Trim(UpperCase(FileContent[I]));
              if Pos(cOUTERLOOP_LABEL, CurLine) = 1 then
              begin
                Inc(I);
                CurLine := Trim(FileContent[I]);
                _DecodeSTLVertex(CurLine, DataFace.v1);
                Inc(I);
                CurLine := Trim(FileContent[I]);
                _DecodeSTLVertex(CurLine, DataFace.v2);
                Inc(I);
                CurLine := Trim(FileContent[I]);
                _DecodeSTLVertex(CurLine, DataFace.v3);
              end;
              Inc(I);
              CurLine := Trim(UpperCase(FileContent[I]));
              if Pos(cENDLOOP_LABEL, CurLine) <> 1 then
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
            if Pos(cENDFACET_LABEL, curLine) <> 1 then
              raise Exception.Create('End of Facet Not found');
            Inc(I);
            CurLine := Trim(UpperCase(FileContent[I]));
            if Pos(cENDSOLID_LABEL, curLine) = 1 then Break;
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

procedure TGLSTLVectorFile.SaveToStream(aStream: TStream);
var
  I: Integer;
  DataFace: TSTLFace;
  Header: TSTLHeader;
  List: TAffineVectorList;
const
  cHeaderTag = 'STL export';
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

  STLUseEmbeddedColors := False;
  RegisterVectorFileFormat('stl', 'Stereolithography files', TGLSTLVectorFile);

end.