unit Velthuis.Loggers;

interface

uses
  System.Classes;

type
  ILogger = interface
    ['{B6821CA6-64F8-48B0-89D0-A9A3E6304D82}']
    procedure Log(Msg: string); overload;
    procedure Log(Format: string; Args: array of const); overload;
  end;

  TLogger = class(TInterfacedObject, ILogger)
  private
    FStream: TStream;
    FWriter: TStreamWriter;
  public
    constructor Create(S: TStream); overload;
    constructor Create(LogFileName: string); overload;
    destructor Destroy; override;
    procedure Log(Msg: string); overload;
    procedure Log(Format: string; Args: array of const); overload;
  end;

implementation

uses
  System.SysUtils;

{ TLogger }

constructor TLogger.Create(S: TStream);
begin
  FStream := S;
  FWriter := TStreamWriter.Create(S);
end;

constructor TLogger.Create(LogFileName: string);
var
  F: TFileStream;
begin
  F := TFileStream.Create(LogFileName, fmCreate);
  Create(F);
end;

destructor TLogger.Destroy;
begin
  FWriter.Free;
  FStream.Free;
end;

procedure TLogger.Log(Msg: string);
begin
  FWriter.WriteLine(Msg);
end;

procedure TLogger.Log(Format: string; Args: array of const);
begin
  FWriter.WriteLine(System.SysUtils.Format(Format, Args));
end;

end.
