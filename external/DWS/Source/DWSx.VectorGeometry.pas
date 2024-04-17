//
// The graphics rendering platform GLScene http://glscene.org
//
unit DWSx.VectorGeometry;

(* DelphiWebScript symbol creation for GXS.VectorGeometry types and functions *)

interface

uses
  System.Classes,
  dwsExprs,
  dwsSymbols,
  dwsComp,
  dwsFunctions,
  GXS.VectorGeometry;

type
  TdwxVectorGeometryUnit = class(TdwxUnitComponent)
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

function GetVectorFromInfo(Info: IInfo): TGXVector;
begin
  Result := VectorMake(Info.Element([0]).Value, Info.Element([1]).Value,
    Info.Element([2]).Value, Info.Element([3]).Value);
end;

procedure SetInfoFromVector(Info: IInfo; vec: TGXVector);
var
  i: Integer;
begin
  for i := 0 to 3 do
    Info.Element([i]).Value := vec[i];
end;

function GetMatrixFromInfo(Info: IInfo): TGXMatrix;
var
  i: Integer;
begin
  for i := 0 to 3 do
    Result[i] := VectorMake(Info.Element([i]).Element([0]).Value,
      Info.Element([i]).Element([1]).Value, Info.Element([i]).Element([2])
      .Value, Info.Element([i]).Element([3]).Value);
end;

procedure SetInfoFromMatrix(Info: IInfo; mat: TGXMatrix);
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
  SymbolTable.AddSymbol(TStaticArraySymbol.Create('TGXVector',
    FloatSymbol, 0, 3));
  SymbolTable.AddSymbol(TStaticArraySymbol.Create('TGXMatrix',
    SymbolTable.FindSymbol('TGXVector'), 0, 3));

  // Vector functions
  TVectorMakeFunction.Create(SymbolTable, 'VectorMake',
    ['x', 'Float', 'y', 'Float', 'z', 'Float', 'w', 'Float'], 'TGXVector');
  TSetVectorFunction.Create(SymbolTable, 'SetVector',
    ['@v', 'TGXVector', 'x', 'Float', 'y', 'Float', 'z', 'Float', 'w',
    'Float'], '');
  TVectorAddFunction.Create(SymbolTable, 'VectorAdd',
    ['v1', 'TGXVector', 'v2', 'TGXVector'], 'TGXVector');
  TVectorSubtractFunction.Create(SymbolTable, 'VectorSubtract',
    ['v1', 'TGXVector', 'v2', 'TGXVector'], 'TGXVector');
  TVectorScaleFunction.Create(SymbolTable, 'VectorScale',
    ['v', 'TGXVector', 'f', 'Float'], 'TGXVector');
  TCombineVectorFunction.Create(SymbolTable, 'CombineVector',
    ['@vr', 'TGXVector', 'v', 'TGXVector', '@f', 'Float'], '');
  TVectorCombineFunction.Create(SymbolTable, 'VectorCombine',
    ['v1', 'TGXVector', 'v2', 'TGXVector', 'f1', 'Float', 'f2', 'Float'],
    'TGXVector');
  TVectorCombine3Function.Create(SymbolTable, 'VectorCombine3',
    ['v1', 'TGXVector', 'v2', 'TGXVector', 'v3', 'TGXVector', 'f1', 'Float', 'f2',
    'Float', 'f3', 'Float'], 'TGXVector');
  TVectorDotProductFunction.Create(SymbolTable, 'VectorDotProduct',
    ['v1', 'TGXVector', 'v2', 'TGXVector'], 'Float');
  TVectorCrossProductFunction.Create(SymbolTable, 'VectorCrossProduct',
    ['v1', 'TGXVector', 'v2', 'TGXVector'], 'TGXVector');
  TVectorNormalizeFunction.Create(SymbolTable, 'VectorNormalize',
    ['v', 'TGXVector'], 'TGXVector');
  TVectorTransformFunction.Create(SymbolTable, 'VectorTransform',
    ['v', 'TGXVector', 'm', 'TGXMatrix'], 'TGXVector');

  // Matrix function
  TInvertMatrixFunction.Create(SymbolTable, 'InvertMatrix',
    ['@mat', 'TGXMatrix'], '');
  TTransposeMatrixFunction.Create(SymbolTable, 'TransposeMatrix',
    ['@mat', 'TGXMatrix'], '');
  TMatrixMultiplyFunction.Create(SymbolTable, 'MatrixMultiply',
    ['m1', 'TGXMatrix', 'm2', 'TGXMatrix'], 'TGXMatrix');
  TCreateScaleMatrixFunction.Create(SymbolTable, 'CreateScaleMatrix',
    ['v', 'TGXVector'], 'TGXMatrix');
  TCreateTranslationMatrixFunction.Create(SymbolTable,
    'CreateTranslationMatrix', ['v', 'TGXVector'], 'TGXMatrix');
  TCreateScaleAndTranslationMatrixFunction.Create(SymbolTable,
    'CreateScaleAndTranslationMatrix', ['scale', 'TGXVector', 'offset',
    'TGXVector'], 'TGXMatrix');
  TCreateRotationMatrixXFunction.Create(SymbolTable, 'CreateRotationMatrixX',
    ['angle', 'Float'], 'TGXMatrix');
  TCreateRotationMatrixYFunction.Create(SymbolTable, 'CreateRotationMatrixY',
    ['angle', 'Float'], 'TGXMatrix');
  TCreateRotationMatrixZFunction.Create(SymbolTable, 'CreateRotationMatrixZ',
    ['angle', 'Float'], 'TGXMatrix');
  TCreateRotationMatrixFunction.Create(SymbolTable, 'CreateRotationMatrix',
    ['anAxis', 'TGXVector', 'angle', 'Float'], 'TGXMatrix');
end;

constructor TdwsVectorGeometryUnit.Create(AOwner: TComponent);
begin
  inherited;
  FUnitName := 'GXS.VectorGeometry';
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
  v1, v2, vr: TGXVector;
begin
  v1 := GetVectorFromInfo(Info.Vars['v1']);
  v2 := GetVectorFromInfo(Info.Vars['v2']);
  VectorAdd(v1, v2, vr);
  SetInfoFromVector(Info.Vars['Result'], vr);
end;

procedure TVectorSubtractFunction.Execute;
var
  v1, v2, vr: TGXVector;
begin
  v1 := GetVectorFromInfo(Info.Vars['v1']);
  v2 := GetVectorFromInfo(Info.Vars['v2']);
  VectorSubtract(v1, v2, vr);
  SetInfoFromVector(Info.Vars['Result'], vr);
end;

procedure TVectorScaleFunction.Execute;
var
  v, vr: TGXVector;
  f: Single;
begin
  v := GetVectorFromInfo(Info.Vars['v']);
  f := Info['f'];
  VectorScale(v, f, vr);
  SetInfoFromVector(Info.Vars['Result'], vr);
end;

procedure TCombineVectorFunction.Execute;
var
  vr, v: TGXVector;
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
  v1, v2, vr: TGXVector;
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
  v1, v2, v3, vr: TGXVector;
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
  v1, v2: TGXVector;
begin
  v1 := GetVectorFromInfo(Info.Vars['v1']);
  v2 := GetVectorFromInfo(Info.Vars['v2']);
  Info.Result := VectorDotProduct(v1, v2);
end;

procedure TVectorCrossProductFunction.Execute;
var
  v1, v2, vr: TGXVector;
begin
  v1 := GetVectorFromInfo(Info.Vars['v1']);
  v2 := GetVectorFromInfo(Info.Vars['v2']);
  VectorCrossProduct(v1, v2, vr);
  SetInfoFromVector(Info.Vars['Result'], vr);
end;

procedure TVectorNormalizeFunction.Execute;
var
  v, vr: TGXVector;
begin
  v := GetVectorFromInfo(Info.Vars['v']);
  vr := VectorNormalize(v);
  SetInfoFromVector(Info.Vars['Result'], vr);
end;

procedure TVectorTransformFunction.Execute;
var
  v, vr: TGXVector;
  mat: TGXMatrix;
begin
  v := GetVectorFromInfo(Info.Vars['v']);
  mat := GetMatrixFromInfo(Info.Vars['mat']);
  vr := VectorTransform(v, mat);
  SetInfoFromVector(Info.Vars['Result'], vr);
end;

procedure TInvertMatrixFunction.Execute;
var
  mat: TGXMatrix;
begin
  mat := GetMatrixFromInfo(Info.Vars['mat']);
  InvertMatrix(mat);
  SetInfoFromMatrix(Info.Vars['Result'], mat);
end;

procedure TTransposeMatrixFunction.Execute;
var
  mat: TGXMatrix;
begin
  mat := GetMatrixFromInfo(Info.Vars['mat']);
  TransposeMatrix(mat);
  SetInfoFromMatrix(Info.Vars['Result'], mat);
end;

procedure TMatrixMultiplyFunction.Execute;
var
  m1, m2, mr: TGXMatrix;
begin
  m1 := GetMatrixFromInfo(Info.Vars['m1']);
  m2 := GetMatrixFromInfo(Info.Vars['m2']);
  MatrixMultiply(m1, m2, mr);
  SetInfoFromMatrix(Info.Vars['Result'], mr);
end;

procedure TCreateScaleMatrixFunction.Execute;
var
  v: TGXVector;
  mr: TGXMatrix;
begin
  v := GetVectorFromInfo(Info.Vars['v']);
  mr := CreateScaleMatrix(v);
  SetInfoFromMatrix(Info.Vars['Result'], mr);
end;

procedure TCreateTranslationMatrixFunction.Execute;
var
  v: TGXVector;
  mr: TGXMatrix;
begin
  v := GetVectorFromInfo(Info.Vars['v']);
  mr := CreateTranslationMatrix(v);
  SetInfoFromMatrix(Info.Vars['Result'], mr);
end;

procedure TCreateScaleAndTranslationMatrixFunction.Execute;
var
  scale, offset: TGXVector;
  mr: TGXMatrix;
begin
  scale := GetVectorFromInfo(Info.Vars['scale']);
  offset := GetVectorFromInfo(Info.Vars['offset']);
  mr := CreateScaleAndTranslationMatrix(scale, offset);
  SetInfoFromMatrix(Info.Vars['Result'], mr);
end;

procedure TCreateRotationMatrixXFunction.Execute;
var
  angle: Single;
  mr: TGXMatrix;
begin
  angle := Info['angle'];
  mr := CreateRotationMatrixX(angle);
  SetInfoFromMatrix(Info.Vars['Result'], mr);
end;

procedure TCreateRotationMatrixYFunction.Execute;
var
  angle: Single;
  mr: TGXMatrix;
begin
  angle := Info['angle'];
  mr := CreateRotationMatrixY(angle);
  SetInfoFromMatrix(Info.Vars['Result'], mr);
end;

procedure TCreateRotationMatrixZFunction.Execute;
var
  angle: Single;
  mr: TGXMatrix;
begin
  angle := Info['angle'];
  mr := CreateRotationMatrixZ(angle);
  SetInfoFromMatrix(Info.Vars['Result'], mr);
end;

procedure TCreateRotationMatrixFunction.Execute;
var
  angle: Single;
  anAxis: TGXVector;
  mr: TGXMatrix;
begin
  anAxis := GetVectorFromInfo(Info.Vars['anAxis']);
  angle := Info['angle'];
  mr := CreateRotationMatrix(anAxis, angle);
  SetInfoFromMatrix(Info.Vars['Result'], mr);
end;

end.
