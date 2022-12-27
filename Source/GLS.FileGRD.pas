//
// The multimedia graphics platform GLScene https://github.com/glscene
//
unit GLS.FileGRD;

(* GRD (Grid Format) text and binary vector file format implementation *)

interface

{$I GLScene.inc}

uses
  System.Classes,
  System.SysUtils,

  GLS.VectorGeometry,
  GLS.VectorTypes,
  GLS.VectorFileObjects,
  GLS.ApplicationFileIO,
  GLS.Graph,
  GLS.Utils;

type
  (* The GRD file represents ascii grid formats in 2D/3D.
    This is a format for storing regular grid values as a
    matrices of cell centers. The format supports variations and
    subformats. This importer works for Sutfer, ArcInfo and GMS formats *)
  TGLGRDVectorFile = class(TGLVectorFile)
  public
    GLHeightField: TGLHeightField;
    Nodes: array of TSingleArray;
    class function Capabilities: TGLDataFileCapabilities; override;
    procedure LoadFromStream(aStream: TStream); override;
  private
    StrVal: String;
    StrLine: String;
    MaxZ: Single;
    function ExtractWord(N: Integer; const S: string;
      const WordDelims: TSysCharSet): string;
    function WordPosition(const N: Integer; const S: string;
      const WordDelims: TSysCharSet): Integer;
  end;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

// ------------------
// ------------------ TGLGRDVectorFile ------------------
// ------------------

const
  dSURFBLANKVAL = 1.70141E38; // default value in Surfer for blanking
  NODATA_value = -9999; // default value in GIS ArcInfo for blanking

class function TGLGRDVectorFile.Capabilities: TGLDataFileCapabilities;
begin
  Result := [dfcRead];
end;

function TGLGRDVectorFile.WordPosition(const N: Integer; const S: string;
  const WordDelims: TSysCharSet): Integer;
var
  Count, I: Integer;
begin
  Count := 0;
  I := 1;
  Result := 0;
  while ((I <= Length(S)) and (Count <> N)) do
  begin
    // skip over delimiters
    while (I <= Length(S)) and CharInSet(S[I], WordDelims) do
      Inc(I);
    // if we're not beyond end of S, we're at the start of a word
    if I <= Length(S) then
      Inc(Count);
    // if not finished, find the end of the current word
    if Count <> N then
      while (I <= Length(S)) and not CharInSet(S[I], WordDelims) do
        Inc(I)
    else
      Result := I;
  end;
end;

function TGLGRDVectorFile.ExtractWord(N: Integer; const S: string;
  const WordDelims: TSysCharSet): string;

var
  I, Len: Integer;

begin
  Len := 0;
  I := WordPosition(N, S, WordDelims);
  if (I <> 0) then
    // find the end of the current word
    while (I <= Length(S)) and not CharInSet(S[I], WordDelims) do
    begin
      // add the I'th character to result
      Inc(Len);
      SetLength(Result, Len);
      Result[Len] := S[I];
      Inc(I);
    end;
  SetLength(Result, Len);
end;

procedure TGLGRDVectorFile.LoadFromStream(aStream: TStream);
var
  I, J, K: Integer;
  N: Integer; // N => counter to increment through file
  SL, TL: TStringList;

  Nx, Ny: Integer;
  Dx, Dy: Single;
  Xo, Xe, Yo, Ye, Zo, Ze: Single;
  NBlanks: Integer; // Number of blank nodes
  BlankVal, NoData: Double;

  function ReadLine: string;
  begin
    Result := SL[N];
    Inc(N);
  end;

begin
  SL := TStringList.Create;
  TL := TStringList.Create;
  try
    SL.LoadFromStream(aStream);
    if (Copy(SL[0], 1, 4) <> 'DSAA') and (Copy(SL[0], 1, 5) <> 'ncols') then
    begin
      raise Exception.Create('Not a valid grd file !');
      Exit;
    end;

    GLHeightField := TGLHeightField.Create(Owner);

    Zo := 3 * 10E38; // Low
    Ze := -3 * 10E38; // High
    TL.DelimitedText := Copy(ReadLine, 1, 4);

    if (TL[0] = 'DSAA') then // Surfer ASCII grid
    begin
      TL.DelimitedText := ReadLine;
      Nx := StrToInt(Tl[0]);
      Ny := StrToInt(Tl[1]);

      TL.DelimitedText := ReadLine;
      Xo := GLStrToFloatDef(Tl[0]);
      Xe := GLStrToFloatDef(Tl[1]);

      TL.DelimitedText := ReadLine;
      Yo := GLStrToFloatDef(Tl[0]);
      Ye := GLStrToFloatDef(Tl[1]);

      TL.DelimitedText := ReadLine;
      Zo := GLStrToFloatDef(Tl[0]);
      Ze := GLStrToFloatDef(Tl[1]);

      Dx := (Xe - Xo) / Nx;
      Dy := (Ye - Yo) / Ny;

      SetLength(Nodes, Ny, Nx);

      NBlanks := 0;
      BlankVal := dSURFBLANKVAL;
      NoData := BlankVal; // NoData value

      // loop over the Ny-1 Rows
      for I := 0 to Ny - 1 do
      begin
        J := 0;
        // reading lines until Nx-1 Cols entries have been obtained
        while J <= Nx - 1 do
        begin
          StrLine := ReadLine;
          K := 1;
          StrVal := ExtractWord(K, StrLine, [' ']);
          while (StrVal <> '') do
          begin
            if (J <= Nx - 1) then
              Nodes[I, J] := GLStrToFloatDef(StrVal);
            if Nodes[I, J] > MaxZ then
              MaxZ := Nodes[I, J];
            if (Nodes[I, J] >= BlankVal) then
              NBlanks := NBlanks + 1;

            Inc(J);
            Inc(K);
            StrVal := ExtractWord(K, StrLine, [' ']);
          end;
          if (J > Nx - 1) then
            Break;
        end;
      end
    end
    else // ArcInfo ASCII grid
    begin
      TL.DelimitedText := Sl[0];
      Ny := StrToInt(TL[1]); // ncols
      TL.DelimitedText := SL[1];
      Nx := StrToInt(TL[1]); // nrows
      TL.DelimitedText := SL[2];
      Xo := GLStrToFloatDef(Tl[1]); // xllcorner
      TL.DelimitedText := SL[3];
      Yo := GLStrToFloatDef(TL[1]); // yllcorner
      TL.DelimitedText := Sl[4];
      Dx := GLStrToFloatDef(TL[1]);
      Dy := Dx; // cellsize
      TL.DelimitedText := SL[5];
      NoData := GLStrToFloatDef(TL[1]); // NoData value

      MaxZ := -3 * 10E38;
      SetLength(Nodes, Nx, Ny);

      for I := 0 to Nx - 1 do
      begin
        TL.DelimitedText := SL[I + 6];
        for J := 0 to Ny - 1 do
        begin
          StrVal := TL[J];
          Nodes[I, J] := GLStrToFloatDef(StrVal);
          if Nodes[I, J] > MaxZ then
            MaxZ := Nodes[I, J];
        end;
      end;
    end;

    GLHeightField.XSamplingScale.Min := -(Nx div 2);
    GLHeightField.XSamplingScale.Max := (Nx div 2);
    GLHeightField.YSamplingScale.Min := -(Ny div 2);
    GLHeightField.YSamplingScale.Max := (Ny div 2);

  finally
    TL.Free;
    SL.Free;
  end;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

RegisterVectorFileFormat('grd', 'ArcInfo/Surfer grids', TGLGRDVectorFile);

end.
