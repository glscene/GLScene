//
// The graphics rendering engine GLScene http://glscene.org
//
unit DWS.VectorGeometry;

(* DelphiWebScript symbol creation for GLS.VectorGeometry types and functions *)

interface

uses
  System.Classes,
  dwsExprs,
  dwsSymbols,
  dwsComp,
  dwsFunctions,
  GLS.VectorGeometry;

type
  TdwsVectorGeometryUnit = class(TdwsUnitComponent)
  protected
    procedure AddUnitSymbols(SymbolTable: TSymbolTable); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

procedure Register;

// =========================================================
implementation
// =========================================================

type
  TVectorMakeFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TSetVectorFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TVectorAddFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TVectorSubtractFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TVectorScaleFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TCombineVectorFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TVectorCombineFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TVectorCombine3Function = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TVectorDotProductFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TVectorCrossProductFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TVectorNormalizeFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TVectorTransformFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TInvertMatrixFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TTransposeMatrixFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TMatrixMultiplyFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TCreateScaleMatrixFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TCreateTranslationMatrixFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TCreateScaleAndTranslationMatrixFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TCreateRotationMatrixXFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TCreateRotationMatrixYFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TCreateRotationMatrixZFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TCreateRotationMatrixFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

procedure Register;
begin
  RegisterComponents('GLScene DWS', [TdwsVectorGeometryUnit]);
end;

function GetVectorFromInfo(Info: IInfo): TGLVector;
begin
  Result := VectorMake(Info.Element([0]).Value, Info.Element([1]).Value,
    Info.Element([2]).Value, Info.Element([3]).Value);
end;

procedure SetInfoFromVector(Info: IInfo; vec: TGLVector);
var
  i: Integer;
begin
  for i := 0 to 3 do
    Info.Element([i]).Value := vec[i];
end;

function GetMatrixFromInfo(Info: IInfo): TGLMatrix;
var
  i: Integer;
begin
  for i := 0 to 3 do
    Result[i] := VectorMake(Info.Element([i]).Element([0]).Value,
      Info.Element([i]).Element([1]).Value, Info.Element([i]).Element([2])
      .Value, Info.Element([i]).Element([3]).Value);
end;

procedure SetInfoFromMatrix(Info: IInfo; mat: TGLMatrix);
var
  i, j: Integer;
begin
  for i := 0 to 3 do
    for j := 0 to 3 do
      Info.Element([i]).Element([j]).Value := mat[i][j];
end;

procedure TdwsVectorGeometryUnit.AddUnitSymbols(SymbolTable: TSymbolTable);
var
  FloatSymbol: TSymbol;
begin
  FloatSymbol := SymbolTable.FindSymbol('Float');

  // Array types
  SymbolTable.AddSymbol(TStaticArraySymbol.Create('TGLVector',
    FloatSymbol, 0, 3));
  SymbolTable.AddSymbol(TStaticArraySymbol.Create('TGLMatrix',
    SymbolTable.FindSymbol('TGLVector'), 0, 3));

  // Vector functions
  TVectorMakeFunction.Create(SymbolTable, 'VectorMake',
    ['x', 'Float', 'y', 'Float', 'z', 'Float', 'w', 'Float'], 'TGLVector');
  TSetVectorFunction.Create(SymbolTable, 'SetVector',
    ['@v', 'TGLVector', 'x', 'Float', 'y', 'Float', 'z', 'Float', 'w',
    'Float'], '');
  TVectorAddFunction.Create(SymbolTable, 'VectorAdd',
    ['v1', 'TGLVector', 'v2', 'TGLVector'], 'TGLVector');
  TVectorSubtractFunction.Create(SymbolTable, 'VectorSubtract',
    ['v1', 'TGLVector', 'v2', 'TGLVector'], 'TGLVector');
  TVectorScaleFunction.Create(SymbolTable, 'VectorScale',
    ['v', 'TGLVector', 'f', 'Float'], 'TGLVector');
  TCombineVectorFunction.Create(SymbolTable, 'CombineVector',
    ['@vr', 'TGLVector', 'v', 'TGLVector', '@f', 'Float'], '');
  TVectorCombineFunction.Create(SymbolTable, 'VectorCombine',
    ['v1', 'TGLVector', 'v2', 'TGLVector', 'f1', 'Float', 'f2', 'Float'],
    'TGLVector');
  TVectorCombine3Function.Create(SymbolTable, 'VectorCombine3',
    ['v1', 'TGLVector', 'v2', 'TGLVector', 'v3', 'TGLVector', 'f1', 'Float', 'f2',
    'Float', 'f3', 'Float'], 'TGLVector');
  TVectorDotProductFunction.Create(SymbolTable, 'VectorDotProduct',
    ['v1', 'TGLVector', 'v2', 'TGLVector'], 'Float');
  TVectorCrossProductFunction.Create(SymbolTable, 'VectorCrossProduct',
    ['v1', 'TGLVector', 'v2', 'TGLVector'], 'TGLVector');
  TVectorNormalizeFunction.Create(SymbolTable, 'VectorNormalize',
    ['v', 'TGLVector'], 'TGLVector');
  TVectorTransformFunction.Create(SymbolTable, 'VectorTransform',
    ['v', 'TGLVector', 'm', 'TGLMatrix'], 'TGLVector');

  // Matrix function
  TInvertMatrixFunction.Create(SymbolTable, 'InvertMatrix',
    ['@mat', 'TGLMatrix'], '');
  TTransposeMatrixFunction.Create(SymbolTable, 'TransposeMatrix',
    ['@mat', 'TGLMatrix'], '');
  TMatrixMultiplyFunction.Create(SymbolTable, 'MatrixMultiply',
    ['m1', 'TGLMatrix', 'm2', 'TGLMatrix'], 'TGLMatrix');
  TCreateScaleMatrixFunction.Create(SymbolTable, 'CreateScaleMatrix',
    ['v', 'TGLVector'], 'TGLMatrix');
  TCreateTranslationMatrixFunction.Create(SymbolTable,
    'CreateTranslationMatrix', ['v', 'TGLVector'], 'TGLMatrix');
  TCreateScaleAndTranslationMatrixFunction.Create(SymbolTable,
    'CreateScaleAndTranslationMatrix', ['scale', 'TGLVector', 'offset',
    'TGLVector'], 'TGLMatrix');
  TCreateRotationMatrixXFunction.Create(SymbolTable, 'CreateRotationMatrixX',
    ['angle', 'Float'], 'TGLMatrix');
  TCreateRotationMatrixYFunction.Create(SymbolTable, 'CreateRotationMatrixY',
    ['angle', 'Float'], 'TGLMatrix');
  TCreateRotationMatrixZFunction.Create(SymbolTable, 'CreateRotationMatrixZ',
    ['angle', 'Float'], 'TGLMatrix');
  TCreateRotationMatrixFunction.Create(SymbolTable, 'CreateRotationMatrix',
    ['anAxis', 'TGLVector', 'angle', 'Float'], 'TGLMatrix');
end;

constructor TdwsVectorGeometryUnit.Create(AOwner: TComponent);
begin
  inherited;
  FUnitName := 'GLS.VectorGeometry';
end;

procedure TVectorMakeFunction.Execute;
begin
  Info.Vars['Result'].Element([0]).Value := Info['x'];
  Info.Vars['Result'].Element([1]).Value := Info['y'];
  Info.Vars['Result'].Element([2]).Value := Info['z'];
  Info.Vars['Result'].Element([3]).Value := Info['w'];
end;

procedure TSetVectorFunction.Execute;
begin
  Info.Vars['v'].Element([0]).Value := Info['x'];
  Info.Vars['v'].Element([1]).Value := Info['y'];
  Info.Vars['v'].Element([2]).Value := Info['z'];
  Info.Vars['v'].Element([3]).Value := Info['w'];
end;

procedure TVectorAddFunction.Execute;
var
  v1, v2, vr: TGLVector;
begin
  v1 := GetVectorFromInfo(Info.Vars['v1']);
  v2 := GetVectorFromInfo(Info.Vars['v2']);
  VectorAdd(v1, v2, vr);
  SetInfoFromVector(Info.Vars['Result'], vr);
end;

procedure TVectorSubtractFunction.Execute;
var
  v1, v2, vr: TGLVector;
begin
  v1 := GetVectorFromInfo(Info.Vars['v1']);
  v2 := GetVectorFromInfo(Info.Vars['v2']);
  VectorSubtract(v1, v2, vr);
  SetInfoFromVector(Info.Vars['Result'], vr);
end;

procedure TVectorScaleFunction.Execute;
var
  v, vr: TGLVector;
  f: Single;
begin
  v := GetVectorFromInfo(Info.Vars['v']);
  f := Info['f'];
  VectorScale(v, f, vr);
  SetInfoFromVector(Info.Vars['Result'], vr);
end;

procedure TCombineVectorFunction.Execute;
var
  vr, v: TGLVector;
  f: Single;
begin
  vr := GetVectorFromInfo(Info.Vars['vr']);
  v := GetVectorFromInfo(Info.Vars['v']);
  f := Info['f'];
  CombineVector(vr, v, f);
  SetInfoFromVector(Info.Vars['Result'], vr);
  Info.Vars['f'].Value := f;
end;

procedure TVectorCombineFunction.Execute;
var
  v1, v2, vr: TGLVector;
  f1, f2: Single;
begin
  v1 := GetVectorFromInfo(Info.Vars['v1']);
  v2 := GetVectorFromInfo(Info.Vars['v2']);
  f1 := Info['f1'];
  f2 := Info['f2'];
  VectorCombine(v1, v2, f1, f2, vr);
  SetInfoFromVector(Info.Vars['Result'], vr);
end;

procedure TVectorCombine3Function.Execute;
var
  v1, v2, v3, vr: TGLVector;
  f1, f2, f3: Single;
begin
  v1 := GetVectorFromInfo(Info.Vars['v1']);
  v2 := GetVectorFromInfo(Info.Vars['v2']);
  v3 := GetVectorFromInfo(Info.Vars['v3']);
  f1 := Info['f1'];
  f2 := Info['f2'];
  f3 := Info['f3'];
  VectorCombine3(v1, v2, v3, f1, f2, f3, vr);
  SetInfoFromVector(Info.Vars['Result'], vr);
end;

procedure TVectorDotProductFunction.Execute;
var
  v1, v2: TGLVector;
begin
  v1 := GetVectorFromInfo(Info.Vars['v1']);
  v2 := GetVectorFromInfo(Info.Vars['v2']);
  Info.Result := VectorDotProduct(v1, v2);
end;

procedure TVectorCrossProductFunction.Execute;
var
  v1, v2, vr: TGLVector;
begin
  v1 := GetVectorFromInfo(Info.Vars['v1']);
  v2 := GetVectorFromInfo(Info.Vars['v2']);
  VectorCrossProduct(v1, v2, vr);
  SetInfoFromVector(Info.Vars['Result'], vr);
end;

procedure TVectorNormalizeFunction.Execute;
var
  v, vr: TGLVector;
begin
  v := GetVectorFromInfo(Info.Vars['v']);
  vr := VectorNormalize(v);
  SetInfoFromVector(Info.Vars['Result'], vr);
end;

procedure TVectorTransformFunction.Execute;
var
  v, vr: TGLVector;
  mat: TGLMatrix;
begin
  v := GetVectorFromInfo(Info.Vars['v']);
  mat := GetMatrixFromInfo(Info.Vars['mat']);
  vr := VectorTransform(v, mat);
  SetInfoFromVector(Info.Vars['Result'], vr);
end;

procedure TInvertMatrixFunction.Execute;
var
  mat: TGLMatrix;
begin
  mat := GetMatrixFromInfo(Info.Vars['mat']);
  InvertMatrix(mat);
  SetInfoFromMatrix(Info.Vars['Result'], mat);
end;

procedure TTransposeMatrixFunction.Execute;
var
  mat: TGLMatrix;
begin
  mat := GetMatrixFromInfo(Info.Vars['mat']);
  TransposeMatrix(mat);
  SetInfoFromMatrix(Info.Vars['Result'], mat);
end;

procedure TMatrixMultiplyFunction.Execute;
var
  m1, m2, mr: TGLMatrix;
begin
  m1 := GetMatrixFromInfo(Info.Vars['m1']);
  m2 := GetMatrixFromInfo(Info.Vars['m2']);
  MatrixMultiply(m1, m2, mr);
  SetInfoFromMatrix(Info.Vars['Result'], mr);
end;

procedure TCreateScaleMatrixFunction.Execute;
var
  v: TGLVector;
  mr: TGLMatrix;
begin
  v := GetVectorFromInfo(Info.Vars['v']);
  mr := CreateScaleMatrix(v);
  SetInfoFromMatrix(Info.Vars['Result'], mr);
end;

procedure TCreateTranslationMatrixFunction.Execute;
var
  v: TGLVector;
  mr: TGLMatrix;
begin
  v := GetVectorFromInfo(Info.Vars['v']);
  mr := CreateTranslationMatrix(v);
  SetInfoFromMatrix(Info.Vars['Result'], mr);
end;

procedure TCreateScaleAndTranslationMatrixFunction.Execute;
var
  scale, offset: TGLVector;
  mr: TGLMatrix;
begin
  scale := GetVectorFromInfo(Info.Vars['scale']);
  offset := GetVectorFromInfo(Info.Vars['offset']);
  mr := CreateScaleAndTranslationMatrix(scale, offset);
  SetInfoFromMatrix(Info.Vars['Result'], mr);
end;

procedure TCreateRotationMatrixXFunction.Execute;
var
  angle: Single;
  mr: TGLMatrix;
begin
  angle := Info['angle'];
  mr := CreateRotationMatrixX(angle);
  SetInfoFromMatrix(Info.Vars['Result'], mr);
end;

procedure TCreateRotationMatrixYFunction.Execute;
var
  angle: Single;
  mr: TGLMatrix;
begin
  angle := Info['angle'];
  mr := CreateRotationMatrixY(angle);
  SetInfoFromMatrix(Info.Vars['Result'], mr);
end;

procedure TCreateRotationMatrixZFunction.Execute;
var
  angle: Single;
  mr: TGLMatrix;
begin
  angle := Info['angle'];
  mr := CreateRotationMatrixZ(angle);
  SetInfoFromMatrix(Info.Vars['Result'], mr);
end;

procedure TCreateRotationMatrixFunction.Execute;
var
  angle: Single;
  anAxis: TGLVector;
  mr: TGLMatrix;
begin
  anAxis := GetVectorFromInfo(Info.Vars['anAxis']);
  angle := Info['angle'];
  mr := CreateRotationMatrix(anAxis, angle);
  SetInfoFromMatrix(Info.Vars['Result'], mr);
end;

end.
