//
// The graphics rendering engine GLScene http://glscene.org
//
(*
  Frame combining a TrackBar and an Edit.
*)
unit FRTrackBarEdit;

interface

{$I GLScene.inc}

uses
  System.Classes,
  System.SysUtils,
  VCL.Forms,
  VCL.StdCtrls,
  VCL.ComCtrls,
  VCL.Controls;

type
  TRTrackBarEdit = class(TFrame)
    TrackBar1: TTrackBar;
    Edit1: TEdit;
    procedure TrackBarChange(Sender: TObject);
    procedure EditChange(Sender: TObject);
  private
    procedure SetValue(const val: Integer);
    function GetValue: Integer;
    procedure SetValueMin(const val: Integer);
    function GetValueMin: Integer;
    procedure SetValueMax(const val: Integer);
    function GetValueMax: Integer;
  public
    property Value: Integer read GetValue write SetValue;
    property ValueMin: Integer read GetValueMin write SetValueMin;
    property ValueMax: Integer read GetValueMax write SetValueMax;
  end;

  // ---------------------------------------------------------------------
implementation

// ---------------------------------------------------------------------

{$R *.dfm}

procedure TRTrackBarEdit.TrackBarChange(Sender: TObject);
begin
  Edit1.Text := IntToStr(TrackBar1.Position);
end;

procedure TRTrackBarEdit.EditChange(Sender: TObject);
var
  i: Integer;
begin
  try
    i := StrToInt(Edit1.Text);
    TrackBar1.Position := i;
  except
    // ignore
  end;
end;

procedure TRTrackBarEdit.SetValue(const val: Integer);
begin
  TrackBar1.Position := val;
  TrackBarChange(Self);
end;

function TRTrackBarEdit.GetValue: Integer;
begin
  Result := TrackBar1.Position;
end;

procedure TRTrackBarEdit.SetValueMax(const val: Integer);
begin
  TrackBar1.Max := val;
  TrackBarChange(Self);
end;

function TRTrackBarEdit.GetValueMax: Integer;
begin
  Result := TrackBar1.Max;
end;

procedure TRTrackBarEdit.SetValueMin(const val: Integer);
begin
  TrackBar1.Min := val;
  TrackBarChange(Self);
end;

function TRTrackBarEdit.GetValueMin: Integer;
begin
  Result := TrackBar1.Min;
end;

end.
