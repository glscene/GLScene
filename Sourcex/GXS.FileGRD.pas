//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.FileGRD;

(* GRD (Grid Text Format) vector file format implementation *)

interface

{$I GXS.Scene.inc}

uses
  System.Classes,
  System.SysUtils,

  GXS.VectorGeometry,
  GXS.VectorTypes,
  GXS.VectorFileObjects,
  GXS.ApplicationFileIO,
  GXS.Graph;

type
  (*The GRD file represents ascii grid formats in 2D/3D.
    This is a format for storing regular grid values as a
    matrices of cell centers. The format supports variations and
    subformats. This importer works for Sutfer, ArcInfo and GMS formats *)
  TgxGRDVectorFile = class(TgxVectorFile)
   public
    HeightField: TgxHeightField;
    Nodes: array of TSingleArray;
    class function Capabilities: TDataFileCapabilities; override;
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

//=======================================================================
implementation
//=======================================================================

//-------------------
// ------------------ TgxGRDVectorFile ------------------
// ------------------

const
  dSURFBLANKVAL = 1.70141E38; // default value in Surfer for blanking
  NODATA_value =  -9999; //default value in GIS ArcInfo for blanking

class function TgxGRDVectorFile.Capabilities: TDataFileCapabilities;
begin
  Result := [dfcRead];
end;

function TgxGRDVectorFile.WordPosition(const N: Integer; const S: string;
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

function TgxGRDVectorFile.ExtractWord(N: Integer; const S: string;
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

procedure TgxGRDVectorFile.LoadFromStream(aStream: TStream);
var
  I, J, K: Integer;
  N : Integer; // N => counter to increment through file
  Sl, Tl: TStringList;

  Nx, Ny: Integer;
  Dx, Dy: Single;
  Xo, Xe, Yo, Ye, Zo, Ze: Single;
  NBlanks : Integer; // Number of blank nodes
  BlankVal, NoData : Double;

  { sub } function ReadLine: string;
  begin
    Result := Sl[N];
    Inc(N);
  end;

begin
  sl := TStringList.Create;
  tl := TStringList.Create;
  try
    sl.LoadFromStream(aStream);
    if (Copy(sl[0], 1, 4) <> 'DSAA') and (Copy(sl[0], 1, 5) <> 'ncols') then
    begin
      raise Exception.Create('Not a valid grd file !');
      Exit;
    end;

    HeightField := TgxHeightField.Create(Owner);

    Zo := 3 * 10E38; // Low
    Ze := -3 * 10E38; // High
    Tl.DelimitedText := Copy(ReadLine, 1, 4);

    if (Tl[0] = 'DSAA') then // Surfer ASCII grid
    begin
      Tl.DelimitedText := ReadLine;
      Nx := StrToInt(Tl[0]);
      Ny := StrToInt(Tl[1]);

      Tl.DelimitedText := ReadLine;
      Xo := StrToFloat(Tl[0]);
      Xe := StrToFloat(Tl[1]);

      Tl.DelimitedText := ReadLine;
      Yo := StrToFloat(Tl[0]);
      Ye := StrToFloat(Tl[1]);

      Tl.DelimitedText := ReadLine;
      Zo := StrToFloat(Tl[0]);
      Ze := StrToFloat(Tl[1]);

      Dx := (Xe - Xo) / Nx;
      Dy := (Ye - Yo) / Ny;

      SetLength(Nodes, Ny, Nx);

      Nblanks := 0;
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
              Nodes[I, J] := StrToFloat(StrVal);
            if Nodes[I, J] > MaxZ then
              MaxZ := Nodes[I, J];
            if (Nodes[I, J] >= BlankVal) then
              Nblanks := Nblanks + 1;

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
      Tl.DelimitedText := Sl[0];
      Ny := StrToInt(Tl[1]); // ncols
      Tl.DelimitedText := Sl[1];
      Nx := StrToInt(Tl[1]); // nrows
      Tl.DelimitedText := Sl[2];
      Xo := StrToFloat(Tl[1]); // xllcorner
      Tl.DelimitedText := Sl[3];
      Yo := StrToFloat(Tl[1]); // yllcorner
      Tl.DelimitedText := Sl[4];
      Dx := StrToFloat(Tl[1]);
      Dy := Dx;     // cellsize
      Tl.DelimitedText := Sl[5];
      NoData := StrToFloat(Tl[1]); // NoData value

      MaxZ := -3 * 10E38;
      SetLength(Nodes, Nx, Ny);

      for I := 0 to Nx - 1 do
      begin
        Tl.DelimitedText := Sl[I + 6];
        for J := 0 to Ny - 1 do
        begin
          StrVal := Tl[J];
          Nodes[I, J] := StrToFloat(StrVal);
          if Nodes[I, J] > MaxZ then
            MaxZ := Nodes[I, J];
        end;
      end;
    end;

    HeightField.XSamplingScale.Min := -(Nx div 2);
    HeightField.XSamplingScale.Max := (Nx div 2);
    HeightField.YSamplingScale.Min := -(Ny div 2);
    HeightField.YSamplingScale.Max := (Ny div 2);

  finally
    tl.Free;
    sl.Free;
  end;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

RegisterVectorFileFormat('grd', 'ArcInfo/Surfer grids', TgxGRDVectorFile);

end.
