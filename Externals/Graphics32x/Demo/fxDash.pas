unit fxDash;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,

  GR32,
  GR32_Polygons,

  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, FMX.Layouts;

type
  TFormDash = class(TForm)
    Image1: TImage;
    TrackBar1: TTrackBar;
    DashChk: TCheckBox;
    ToolBar1: TToolBar;
    Panel1: TLayout;
    Button1: TButton;
    procedure TrackBar1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormDash: TFormDash;

implementation

{$R *.fmx}

procedure TFormDash.TrackBar1Change(Sender: TObject);
var b32: TBitmap32;
    Points: TArrayOfFloatPoint;
    Dashes: TArrayOfFloat;
    d: Single;
begin
     b32 := TBitmap32.Create;
     b32.SetSize(350, 350);

     SetLength(Points, 10);
     Points[0] := FloatPoint(128.57142 * 2, 155.21932 * 2);
     Points[1] := FloatPoint(82.877695 * 2, 135.39077 * 2);
     Points[2] := FloatPoint(37.202937 * 2, 155.26298 * 2);
     Points[3] := FloatPoint(41.940878 * 2, 105.6783  * 2);
     Points[4] := FloatPoint(8.9270057 * 2, 68.379869 * 2);
     Points[5] := FloatPoint(57.548943 * 2, 57.563411 * 2);
     Points[6] := FloatPoint(82.820007 * 2, 14.639505 * 2);
     Points[7] := FloatPoint(108.13208 * 2, 57.539246 * 2);
     Points[8] := FloatPoint(156.76432 * 2, 68.309239 * 2);
     Points[9] := FloatPoint(123.78611 * 2, 105.6392  * 2);

     if DashChk.IsChecked then
     begin
          SetLength(Dashes, 3);
          d := TrackBar1.Value;
          Dashes[0] := d;
          Dashes[1] := d;
          Dashes[2] := d;
          DashLineFS(b32, Points, Dashes, Color32(clRed32), True, TrackBar1.Value);
     end
     else PolyPolylineFS(b32, [Points], Color32(clRed32), True, TrackBar1.Value, TJoinStyle.jsRound, TEndStyle.esButt);

     Image1.Bitmap.Assign(b32);
     b32.Free;
end;

end.
