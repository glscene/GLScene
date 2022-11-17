//
// The graphics rendering engine GLScene http://glscene.org
//

unit GLS.FileNurbs;

(* Nurbs surfaces vector file loading. *)

interface

uses
  System.Classes,
  System.SysUtils,

  GLS.VectorTypes,
  GLS.VectorFileObjects,
  GLS.VectorGeometry,
  GLS.VectorLists,
  GLS.ApplicationFileIO,
  GLS.ParametricSurfaces,
  GLS.Utils;

type

  TGLNurbsVectorFile = class(TGLVectorFile)
  public
    class function Capabilities: TGLDataFileCapabilities; override;
    procedure LoadFromStream(stream: TStream); override;
  end;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

// ------------------
// ------------------ TGLNurbsVectorFile ------------------
// ------------------

class function TGLNurbsVectorFile.Capabilities: TGLDataFileCapabilities;
begin
  Result := [dfcRead];
end;

procedure TGLNurbsVectorFile.LoadFromStream(stream: TStream);
  function CleanupLine(const line: String): String;
  var
    p: Integer;
  begin
    p := Pos('#', line);
    if p > 0 then
      Result := LowerCase(Trim(Copy(line, 1, p - 1)))
    else
      Result := LowerCase(Trim(line));
  end;

  function ReadSingleArray(sl: TStrings; idx: Integer;
    list: TGLSingleList): Integer;
  var
    k: Integer;
    buf: String;
    vals: TStringList;
  begin
    vals := TStringList.Create;
    try
      while idx < sl.Count do
      begin
        buf := CleanupLine(sl[idx]);
        if buf = ']' then
          Break;
        vals.CommaText := buf;
        for k := 0 to vals.Count - 1 do
          if vals[k] <> '' then
            list.Add(GLStrToFloatDef(vals[k], 0));
        Inc(idx);
      end;
      Result := idx;
    finally
      vals.Free;
    end;
  end;

  function ReadVectorArray(sl: TStrings; idx: Integer;
    list: TGLAffineVectorList): Integer;
  var
    buf: String;
    vals: TStringList;
  begin
    vals := TStringList.Create;
    try
      while idx < sl.Count do
      begin
        buf := CleanupLine(sl[idx]);
        if buf = ']' then
          Break;
        vals.CommaText := buf;
        if vals.Count >= 3 then
          list.Add(GLStrToFloatDef(vals[0], 0),
            GLStrToFloatDef(vals[1], 0),
            GLStrToFloatDef(vals[2], 0));
        Inc(idx);
      end;
      Result := idx;
    finally
      vals.Free;
    end;
  end;

var
  sl, buf: TStringList;
  ss: TStringStream;
  i, j: Integer;
  surface: TMOParametricSurface;
  invert: Boolean;
  invControlPoints: TGLAffineVectorList;
begin
  ss := TStringStream.Create('');
  sl := TStringList.Create;
  buf := TStringList.Create;

  surface := TMOParametricSurface.CreateOwned(Owner.MeshObjects);
  with surface do
  begin
    Name := 'Nurbs' + IntToStr(Owner.IndexOf(surface));
    Basis := psbBSpline;
    Renderer := psrOpenGL;
    AutoKnots := False;
  end;

  invert := False;

  try
    ss.CopyFrom(stream, stream.Size - stream.Position);
    sl.Text := ss.DataString;

    i := 0;
    while i < sl.Count do
    begin
      buf.CommaText := CleanupLine(sl[i]);
      if buf.Count > 1 then
      begin
        if buf[0] = 'uorder' then
          surface.OrderU := StrToIntDef(buf[1], 2)
        else if buf[0] = 'vorder' then
          surface.OrderV := StrToIntDef(buf[1], 2)
        else if buf[0] = 'uknot' then
          i := ReadSingleArray(sl, i + 1, surface.KnotsU)
        else if buf[0] = 'vknot' then
          i := ReadSingleArray(sl, i + 1, surface.KnotsV)
        else if buf[0] = 'weight' then
          i := ReadSingleArray(sl, i + 1, surface.Weights)
        else if buf[0] = 'udimension' then
          surface.CountU := StrToIntDef(buf[1], 0)
        else if buf[0] = 'vdimension' then
          surface.CountV := StrToIntDef(buf[1], 0)
        else if buf[0] = 'controlpoint' then
          i := ReadVectorArray(sl, i + 1, surface.ControlPoints)
        else if buf[0] = 'ccw' then
          invert := (buf[1] = 'false');
      end;
      Inc(i);
    end;

    if invert then
    begin
      invControlPoints := TGLAffineVectorList.Create;
      for i := surface.CountV - 1 downto 0 do
        for j := 0 to surface.CountU - 1 do
          invControlPoints.Add(surface.ControlPoints[i * surface.CountU + j]);
      surface.ControlPoints.Assign(invControlPoints);
      invControlPoints.Free;
    end;

  finally
    buf.Free;
    sl.Free;
    ss.Free;
  end;
end;

// ------------------------------------------------------------------
initialization

// ------------------------------------------------------------------

RegisterVectorFileFormat('nurbs', 'Nurbs model files', TGLNurbsVectorFile);

end.
