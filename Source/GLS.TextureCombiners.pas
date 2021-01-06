//
// The graphics rendering engine GLScene http://glscene.org
//

unit GLS.TextureCombiners;

(* Texture combiners setup utility functions. *)

interface

{$I GLScene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.SysUtils,
  System.Classes,
   
  GLS.OpenGLTokens,
  GLS.Context;


type
  TCombinerCommand = record
    ActiveUnit: Integer;
    Arg1: Integer;
    Arg2: Integer;
  end;

  TCombinerCache = array of TCombinerCommand;

  ETextureCombinerError = class(Exception);

  (* Parses a TC text description and setups combiners accordingly. 
    *experimental*
    Knowledge of texture combiners is a requirement
    Syntax: pascal-like, one instruction per line, use '//' for comment. 

    Examples: 
      Tex1:=Tex0;   // replace texture 1 with texture 0
      Tex1:=Tex0+Tex1; // additive blending between textures 0 and 1
      Tex1:=Tex0-Tex1; // subtractive blending between textures 0 and 1
      Tex1:=Tex0*Tex1; // modulation between textures 0 and 1
      Tex1:=Tex0+Tex1-0.5; // signed additive blending between textures 0 and 1
      Tex1:=Interpolate(Tex0, Tex1, PrimaryColor); // interpolation between textures 0 and 1 using primary color as factor
      Tex1:=Dot3(Tex0, Tex1); // dot3 product between textures 0 and 1

     Accepted tokens:
      Tex0, Tex1, etc. : texture unit
      PrimaryColor, Col : the primary color
      ConstantColor, EnvCol : texture environment constant color
      
     Tokens can be qualified with '.a' or '.alpha' to specify the alpha channel
     explicitly, and '.rgb' to specify color channels (default). You cannot mix
     alpha and rgb tokens in the same line.
  *)
function GetTextureCombiners(const tcCode: TStringList): TCombinerCache;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

var
  vActiveUnit: Integer;
  vCommandCache: TCombinerCache;

procedure TCAssertCheck(const b: Boolean; const errMsg: string);
begin
  if not b then
    raise ETextureCombinerError.Create(errMsg);
end;

function RemoveSpaces(const str: string): string;
var
  c: Char;
  i, p, n: Integer;
begin
  n := Length(str);
  SetLength(Result, n);
  p := 1;
  for i := 1 to n do
  begin
    c := str[i];
    if c <> ' ' then
    begin
      Result[p] := c;
      Inc(p);
    end;
  end;
  SetLength(Result, p - 1);
end;

procedure ProcessTextureCombinerArgument(arg: string; sourceEnum, operandEnum: Integer;
  const dest: string);
var
  sourceValue, operandValue, n, p: Integer;
  origArg, qualifier: string;
  cmd: TCombinerCommand;
begin
  origArg := arg;
  p := Pos('.', arg);
  if p > 0 then
  begin
    qualifier := Copy(arg, p + 1, MaxInt);
    arg := Copy(arg, 1, p - 1);
  end
  else
    qualifier := 'rgb';
  if qualifier = 'rgb' then
  begin
    if Copy(arg, 1, 1) = '~' then
    begin
      operandValue := GL_ONE_MINUS_SRC_COLOR;
      arg := Copy(arg, 2, MaxInt);
    end
    else if Copy(arg, 1, 2) = '1-' then
    begin
      operandValue := GL_ONE_MINUS_SRC_COLOR;
      arg := Copy(arg, 3, MaxInt);
    end
    else
      operandValue := GL_SRC_COLOR;
  end
  else if Copy(qualifier, 1, 1) = 'a' then
  begin
    if Copy(arg, 1, 1) = '~' then
    begin
      operandValue := GL_ONE_MINUS_SRC_ALPHA;
      arg := Copy(arg, 2, MaxInt);
    end
    else if Copy(arg, 1, 2) = '1-' then
    begin
      operandValue := GL_ONE_MINUS_SRC_ALPHA;
      arg := Copy(arg, 3, MaxInt);
    end
    else
      operandValue := GL_SRC_ALPHA;
  end
  else
    operandValue := 0;
  sourceValue := 0;
  if (arg = 'tex') or (arg = dest) then
    sourceValue := GL_TEXTURE
  else if ((arg = 'tex0') and (dest = 'tex1')) or ((arg = 'tex1') and (dest = 'tex2'))
    or ((arg = 'tex2') and (dest = 'tex3')) then
    sourceValue := GL_PREVIOUS_ARB
  else if (arg = 'col') or (arg = 'col0') or (arg = 'primarycolor') then
    sourceValue := GL_PRIMARY_COLOR_ARB
  else if (arg = 'envcol') or (arg = 'constcol') or (arg = 'constantcolor') then
    sourceValue := GL_CONSTANT_COLOR_EXT
  else if Copy(arg, 1, 3) = 'tex' then
  begin
    TCAssertCheck(GL.ARB_texture_env_crossbar or GL.NV_texture_env_combine4,
      'Requires GL_ARB_texture_env_crossbar or NV_texture_env_combine4');
    n := StrToIntDef(Copy(arg, 4, MaxInt), -1);
    if n in [0..7] then
      sourceValue := GL_TEXTURE0_ARB + n;
  end;
  TCAssertCheck((operandValue > 0) and (sourceValue > 0),
    'invalid argument : "' + origArg + '"');

  SetLength(vCommandCache, Length(vCommandCache)+2);
  cmd.ActiveUnit := vActiveUnit;
  cmd.Arg1 := sourceEnum;
  cmd.Arg2 := sourceValue;
  vCommandCache[High(vCommandCache)-1] := cmd;
  cmd.ActiveUnit := vActiveUnit;
  cmd.Arg1 := operandEnum;
  cmd.Arg2 := operandValue;
  vCommandCache[High(vCommandCache)] := cmd;
end;

procedure ProcessTextureCombinerLine(const tcLine: string);
var
  line, dest, arg1, arg2, arg3, funcname: string;
  p: Integer;
  destEnum, operEnum: Integer;
  sourceBaseEnum, operandBaseEnum: Integer;
  sl: TStrings;
  cmd: TCombinerCommand;
begin
  // initial filtering
  line := LowerCase(RemoveSpaces(Trim(tcLine)));
  if Copy(line, 1, 2) = '//' then
    Exit;
  if line = '' then
    Exit;
  if line[Length(line)] = ';' then
  begin
    line := Trim(Copy(line, 1, Length(line) - 1));
    if line = '' then
      Exit;
  end;
  // Parse destination
  p := Pos(':=', line);
  dest := Copy(line, 1, p - 1);
  line := Copy(line, p + 2, MaxInt);
  p := Pos('.', dest);
  destEnum := GL_COMBINE_RGB_ARB;
  sourceBaseEnum := GL_SOURCE0_RGB_ARB;
  operandBaseEnum := GL_OPERAND0_RGB_ARB;
  if p > 0 then
  begin
    if Copy(dest, p + 1, 1) = 'a' then
    begin
      destEnum := GL_COMBINE_ALPHA_ARB;
      sourceBaseEnum := GL_SOURCE0_ALPHA_ARB;
      operandBaseEnum := GL_OPERAND0_ALPHA_ARB;
    end;
    dest := Copy(dest, 1, p - 1);
  end;
  if Copy(dest, 1, 3) = 'tex' then
  begin
    p := StrToIntDef(Copy(dest, 4, MaxInt), -1);
    TCAssertCheck(p >= 0, 'Invalid destination texture unit "' + dest + '"');
    vActiveUnit := p;
  end
  else
    TCAssertCheck(False, 'Invalid destination "' + dest + '"');
  // parse combiner operator
  operEnum := 0;
  arg1 := '';
  arg2 := '';
  arg3 := '';
  p := Pos('+', line);
  if p > 0 then
  begin
    // ADD & ADD_SIGNED operators
    if Copy(line, Length(line) - 3, 4) = '-0.5' then
    begin
      operEnum := GL_ADD_SIGNED_ARB;
      SetLength(line, Length(line) - 4);
    end
    else
      operEnum := GL_ADD;
    arg1 := Copy(line, 1, p - 1);
    arg2 := Copy(line, p + 1, MaxInt);
  end;
  p := Pos('*', line);
  if p > 0 then
  begin
    // MODULATE operator
    operEnum := GL_MODULATE;
    arg1 := Copy(line, 1, p - 1);
    arg2 := Copy(line, p + 1, MaxInt);
    line := '';
  end;
  p := Pos('(', line);
  if p > 0 then
  begin
    // function
    sl := TStringList.Create;
    try
      funcName := Copy(line, 1, p - 1);
      p := Pos('(', line);
      line := Copy(line, p + 1, MaxInt);
      p := Pos(')', line);
      sl.CommaText := Copy(line, 1, p - 1);
      if funcName = 'interpolate' then
      begin
        // INTERPOLATE operator
        TCAssertCheck(sl.Count = 3, 'Invalid parameter count');
        operEnum := GL_INTERPOLATE_ARB;
        arg1 := sl[0];
        arg2 := sl[1];
        arg3 := sl[2];
      end
      else if funcName = 'dot3' then
      begin
        // DOT3 operator
        TCAssertCheck(sl.Count = 2, 'Invalid parameter count');
        TCAssertCheck(GL.ARB_texture_env_dot3, 'Requires GL_ARB_texture_env_dot3');
        operEnum := GL_DOT3_RGB_ARB;
        arg1 := sl[0];
        arg2 := sl[1];
      end
      else
        TCAssertCheck(False, 'Invalid function "' + funcName + '"');
    finally
      sl.Free;
    end;
    line := '';
  end;
  p := Pos('-', line);
  if p > 0 then
  begin
    // SUBTRACT operator
    operEnum := GL_SUBTRACT_ARB;
    arg1 := Copy(line, 1, p - 1);
    arg2 := Copy(line, p + 1, MaxInt);
    line := '';
  end;
  if operEnum = 0 then
  begin
    // REPLACE by default
    operEnum := GL_REPLACE;
    arg1 := line;
  end;

  cmd.ActiveUnit := vActiveUnit;
  cmd.Arg1 := destEnum;
  cmd.Arg2 := operEnum;
  SetLength(vCommandCache, Length(vCommandCache)+1);
  vCommandCache[High(vCommandCache)] := cmd;
  // parse arguments
  if arg1 <> '' then
    ProcessTextureCombinerArgument(arg1, sourceBaseEnum, operandBaseEnum, dest);
  if arg2 <> '' then
    ProcessTextureCombinerArgument(arg2, sourceBaseEnum + 1, operandBaseEnum + 1, dest);
  if arg3 <> '' then
    ProcessTextureCombinerArgument(arg3, sourceBaseEnum + 2, operandBaseEnum + 2, dest);
end;

function GetTextureCombiners(const tcCode: TStringList): TCombinerCache;
var
  i: Integer;
  sl: TStringList;
begin
  vCommandCache := nil;
  TCAssertCheck(GL.ARB_texture_env_combine, 'Requires GL_ARB_texture_env_combine support');
  sl := TStringList.Create;
  try
    sl.Assign(tcCode);
    for i := 0 to sl.Count - 1 do
      ProcessTextureCombinerLine(sl[i]);
  finally
    sl.Free;
  end;
  Result := vCommandCache;
end;

end.

