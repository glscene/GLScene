//
// The graphics rendering engine GLScene http://glscene.org
//
unit CUDA.Parser;
(*
   Helper unit for parsing CU modules and get information about.
   kernel's functions, textures, shared and constants memory.
*)
interface

uses
  System.Classes,
  System.SysUtils,
  CUDA.RunTime;

type

  TCUDAType =
    (
    customType,
    char1,
    uchar1,
    char2,
    uchar2,
    char3,
    uchar3,
    char4,
    uchar4,
    short1,
    ushort1,
    short2,
    ushort2,
    short3,
    ushort3,
    short4,
    ushort4,
    int1,
    uint1,
    int2,
    uint2,
    int3,
    uint3,
    int4,
    uint4,
    long1,
    ulong1,
    long2,
    ulong2,
    long3,
    ulong3,
    long4,
    ulong4,
    float1,
    float2,
    float3,
    float4,
    longlong1,
    ulonglong1,
    longlong2,
    ulonglong2,
    longlong3,
    ulonglong3,
    longlong4,
    ulonglong4,
    double1,
    double2,
    double3,
    double4,
    int8,
    int16,
    int32,
    uint8,
    uint16,
    uint32
    );

  TCUDATexRefInfo = record
    Name: string;
    DataType: TCUDAType;
    Dim: Byte;
    ReadMode: TcudaTextureReadMode;
  end;

  TCUDAFuncArgInfo = record
    Name: string;
    DataType: TCUDAType;
    CustomType: string;
    Ref: Boolean;
  end;

  TCUDAFuncInfo = record
    Name: string;
    KernelName: string;
    Args: array of TCUDAFuncArgInfo;
  end;

  TCUDAConstantInfo = record
    Name: string;
    DataType: TCUDAType;
    CustomType: string;
    Ref: Boolean;
    DefValue: Boolean;
  end;

  TCUDAModuleInfo = class(TObject)
  private
    ping, pong: TStrings;
    procedure Reset;
    procedure BreakStrings(inlist, outlist: TStrings);
    procedure RemoveComents(inlist, outlist: TStrings);
    procedure RemoveSpaces(inlist, outlist: TStrings);
    procedure ReplaceUnsigned(inlist, outlist: TStrings);
    procedure FindTexRef(inlist: TStrings);
    procedure FindConst(inlist: TStrings);
    procedure FindFunc(inlist: TStrings);
    procedure FindFuncKernelName(inlist: TStrings);
  public
    Owner: TComponent;
    TexRef: array of TCUDATexRefInfo;
    Func: array of TCUDAFuncInfo;
    Constant: array of TCUDAConstantInfo;
    constructor Create;
    destructor Destroy; override;
    procedure ParseModule(ASource, AProduct: TStrings);
  end;

//-------------------------------------------
implementation
//-------------------------------------------

uses
  GLS.Strings;

const
  WordDelimiters: set of AnsiChar = [#0..#255] - ['a'..'z','A'..'Z','1'..'9','0','_'];
  sCUDAType: array[TCUDAType] of string =
  (
    '',
    'char',
    'uchar',
    'char2',
    'uchar2',
    'char3',
    'uchar3',
    'char4',
    'uchar4',
    'short',
    'ushort',
    'short2',
    'ushort2',
    'short3',
    'ushort3',
    'short4',
    'ushort4',
    'int',
    'uint',
    'int2',
    'uint2',
    'int3',
    'uint3',
    'int4',
    'uint4',
    'long',
    'ulong',
    'long2',
    'ulong2',
    'long3',
    'ulong3',
    'long4',
    'ulong4',
    'float',
    'float2',
    'float3',
    'float4',
    'longlong',
    'ulonglong',
    'longlong2',
    'ulonglong2',
    'longlong3',
    'ulonglong3',
    'longlong4',
    'ulonglong4',
    'double',
    'double2',
    'double3',
    'double4',
    'int8',
    'int16',
    'int32',
    'uint8',
    'uint16',
    'uint32'
    );

function StrToCUDAType(const AToken: string): TCUDAType;
var
  T: TCUDAType;
begin
  for T := char1 to uint32 do
    if AToken = sCUDAType[T] then
    begin
      exit(T);
    end;
  Result := customType;
end;

procedure TCUDAModuleInfo.BreakStrings(inlist, outlist: TStrings);
var
  i: Integer;
  str, accum: string;
  c: Char;
begin
  str := inlist.Text;
  outlist.Clear;
  accum := '';

  for I := 1 to Length(str) do
  begin
    c := str[I];
    if CharInSet(c, WordDelimiters) then
    begin
      if Length(accum) > 0 then
      begin
        outlist.Add(accum);
        accum := '';
      end;
      outlist.Add(c);
    end
    else
      accum := accum + str[I];
  end;
end;

procedure TCUDAModuleInfo.RemoveComents(inlist, outlist: TStrings);
var
  bSkipToLineBreak: Boolean;
  bSkipToRemarkEnd: Boolean;
  i: Integer;
  str1, str2: string;
begin
  outlist.Clear;
  bSkipToLineBreak := False;
  bSkipToRemarkEnd := False;
  for I := 0 to inlist.Count - 2 do
  begin
    str1 := inlist[I];
    str2 := inlist[I+1];

    if bSkipToLineBreak then
    begin
      if (str1 = #13) then
        bSkipToLineBreak := False;
      continue;
    end;

    if bSkipToRemarkEnd then
    begin
      if (str1 = '*') and (str2 = '/')  then
        bSkipToRemarkEnd := False;
      continue;
    end;

    if (str1 = '/') and (str2 = '/') then
    begin
      bSkipToLineBreak := True;
      continue;
    end
    else if (str1 = '/') and (str2 = '*') then
    begin
      bSkipToRemarkEnd := True;
      continue;
    end;

    outlist.Add(str1);
  end;
end;

procedure TCUDAModuleInfo.RemoveSpaces(inlist, outlist: TStrings);
var
  i: Integer;
begin
  outlist.Clear;
  for I := 0 to inlist.Count - 2 do
    if inlist[i] > #32 then
      outlist.Add(inlist[i]);
end;

procedure TCUDAModuleInfo.ReplaceUnsigned(inlist, outlist: TStrings);
var
  I: Integer;
begin
  outlist.Clear;
  I := 0;
  repeat
    if (inlist[I] = 'unsigned') and (inlist[I+1] = 'int') then
    begin
      outlist.Add('uint32');
      Inc(I);
    end
    else
      outlist.Add(inlist[I]);
   Inc(I);
  until I >= inlist.Count;
end;

procedure TCUDAModuleInfo.FindTexRef(inlist: TStrings);
var
  i, p, e: Integer;
  texInfo: TCUDATexRefInfo;
begin
  for I := 0 to inlist.Count - 1 do
  begin
    if UpperCase(inlist[i]) = 'TEXTURE' then
    begin
      if inlist[i+1] <> '<' then
        continue;
      texInfo.DataType := StrToCUDAType(inlist[i+2]);
      if inlist[i+3] <> ',' then
        continue;
      Val(inlist[i+4], texInfo.Dim, e);
      if e <> 0 then
        Continue;

      p := 5;
      if inlist[i+5] = ',' then
      begin
        if inlist[i+6] = 'cudaReadModeElementType' then
          texInfo.ReadMode := cudaReadModeElementType
        else if inlist[i+6] = 'cudaReadModeNormalizedFloat' then
          texInfo.ReadMode := cudaReadModeNormalizedFloat
        else
          Continue;
        p := 7;
      end;
      if inlist[i+p] <> '>' then
        continue;
      texInfo.Name := inlist[i+p+1];
      SetLength(TexRef, Length(TexRef)+1);
      TexRef[High(TexRef)] := texInfo;
    end;
  end;
end;

constructor TCUDAModuleInfo.Create;
begin
  ping := TStringList.Create;
  pong := TStringList.Create;
end;

destructor TCUDAModuleInfo.Destroy;
begin
  ping.Destroy;
  pong.Destroy;
end;

procedure TCUDAModuleInfo.FindConst(inlist: TStrings);
var
  i, p: Integer;
  constInfo: TCUDAConstantInfo;
begin
  for I := 0 to inlist.Count - 1 do
  begin
    if UpperCase(inlist[i]) = '__CONSTANT__' then
    begin
      p := i+1;
      if inlist[p] = 'static' then
        Inc(p);
      constInfo.DataType := StrToCUDAType(inlist[p]);
      if constInfo.DataType = customType then
        constInfo.CustomType := inlist[p]
      else
        constInfo.CustomType := '';
      Inc(p);

      if inlist[p] = '*' then
      begin
        constInfo.Ref := True;
        Inc(p);
      end
      else
        constInfo.Ref := False;

      constInfo.Name := inlist[p];
      Inc(p);
      constInfo.DefValue := False;
      while p < inlist.Count do
      begin
        if inlist[p] = '=' then
        begin
          constInfo.DefValue := True;
          break;
        end
        else if inlist[p] = ';' then
          break;
        Inc(p);
      end;
      SetLength(Constant, Length(Constant)+1);
      Constant[High(Constant)] := constInfo;
    end;
  end;
end;

procedure TCUDAModuleInfo.FindFunc(inlist: TStrings);
var
  i, p: Integer;
  funcInfo: TCUDAFuncInfo;
  argInfo: TCUDAFuncArgInfo;
begin
  for I := 0 to inlist.Count - 1 do
  begin
    if UpperCase(inlist[i]) = '__GLOBAL__' then
    begin
      if inlist[i+1] <> 'void' then
        Continue;
      funcInfo.Name := inlist[i+2];
      funcInfo.KernelName := '';
      if inlist[i+3] <> '(' then
        Continue;

      p := 4;
      funcInfo.Args := nil;
      while inlist[i+p] <> ')' do
      begin
        if inlist[i+p] = ',' then
        begin
          inc(p);
          Continue;
        end;
        argInfo.DataType := StrToCUDAType(inlist[i+p]);
        if argInfo.DataType = customType then
          argInfo.CustomType := inlist[i+p]
        else
          argInfo.CustomType := '';
        Inc(p);

        if inlist[i+p] = '*' then
        begin
          argInfo.Ref := True;
          Inc(p);
        end
        else
          argInfo.Ref := False;

        argInfo.Name := inlist[i+p];
        SetLength(funcInfo.Args, Length(funcInfo.Args)+1);
        funcInfo.Args[High(funcInfo.Args)] := argInfo;
        inc(p);
      end;
      SetLength(Func, Length(Func)+1);
      Func[High(Func)] := funcInfo;
    end;
  end;
end;

procedure TCUDAModuleInfo.FindFuncKernelName(inlist: TStrings);
var
  I, J, P: Integer;
  LStr: string;
begin
  for J := 0 to inlist.Count - 1 do
  begin
    LStr := inlist[J];
    P := Pos('.entry', LStr);
    if P > 0 then
    begin
      Delete(LStr, 1, P+6);
      P := Pos(' ', LStr);
      if P < 1 then
        continue;
      LStr := Copy(LStr, 1, P-1);
      for I := 0 to High(Func) do
      begin
        if Pos(Func[I].Name, LStr) > 0 then
        begin
          if Length(Func[I].KernelName) > Length(LStr) then
            continue;
          Func[I].KernelName := LStr;
          break;
        end;
      end;
    end;
  end;
end;

procedure TCUDAModuleInfo.Reset;
var
  i: Integer;
begin
  TexRef := nil;
  Constant:= nil;
  for I := 0 to High(Func) do
    Func[I].Args := nil;
  Func := nil;
end;

procedure TCUDAModuleInfo.ParseModule(ASource, AProduct: TStrings);
begin
  Reset;
  BreakStrings(ASource, ping);
  RemoveComents(ping, pong);
  RemoveSpaces(pong, ping);
  ReplaceUnsigned(ping, pong);
  FindTexRef(pong);
  FindConst(pong);
  FindFunc(pong);
  // Double call to confidence
  FindFuncKernelName(AProduct);
  FindFuncKernelName(AProduct);
end;

end.

