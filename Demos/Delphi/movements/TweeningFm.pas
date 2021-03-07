unit TweeningFm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.TypInfo,
  Vcl.Graphics,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Samples.Spin,
  
  GLS.Scene,
  GLS.VectorTypes,
  GLS.Objects,
  GLS.Coordinates,
  GLS.SceneViewer,
 
  GLS.BaseClasses,
  GLS.Cadencer,
  GLS.AnimationUtils,
  GLS.VectorGeometry,
  GLS.BitmapFont,
  GLS.WindowsFont;

type

  TSinglePoint = record
    X: single;
    Y: single;
  end;

  // You can customize TAnimationState for your own purpose, you can add states or rename them, etc.
  TAnimationState = (asWaiting, asGoToStateA, asGoToStateB);

  TAnimationTime = record
    Current: Double;
    Initial: Double;
    Diff: Double;
  end;

  // You can customize TAnimation for your own purpose, you can add InitialXValue for your custom data (Matrix or anything else).
  // Note that you will maybe have to create your own Tweener function to use your custom Current/Target type.
  TAnimation = record
    State: TAnimationState;
    Time: TAnimationTime;
    Init: Boolean;
    Initial1iValue: Integer;
    Initial1sValue: single;
    Initial3fValue: TAffineVector;
    Initial4fValue: TGLVector;
    InitialPtValue: TSinglePoint;
  end;

  TFormTweening = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLPlane1: TGLPlane;
    Panel1: TPanel;
    Button1: TButton;
    GLCadencer1: TGLCadencer;
    Button2: TButton;
    PointA: TGLPoints;
    GLFlatText1: TGLFlatText;
    GLWindowsBitmapFont1: TGLWindowsBitmapFont;
    PointB: TGLPoints;
    GLFlatText2: TGLFlatText;
    UseCurrentPosition: TCheckBox;
    EaseTypeA: TComboBox;
    EaseTypeB: TComboBox;
    TimeA: TSpinEdit;
    TimeB: TSpinEdit;
    Label2: TLabel;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    procedure GLPlane1Progress(Sender: TObject; const deltaTime, newTime: Double);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
  private
    FGLPlane1AnimationData: TAnimation;
  public
  end;

var
  FormTweening: TFormTweening;

implementation

{$R *.dfm}

procedure TFormTweening.Button1Click(Sender: TObject);
begin
  FGLPlane1AnimationData.State := asGoToStateA;
  FGLPlane1AnimationData.Init := True;
end;

procedure TFormTweening.Button2Click(Sender: TObject);
begin
  FGLPlane1AnimationData.State := asGoToStateB;
  FGLPlane1AnimationData.Init := True;
end;

procedure TFormTweening.FormCreate(Sender: TObject);
var
  EaseName: string;
  i: integer;
begin
  // Fill combobox with human readable data from TEaseType
  for i := Ord(etLinear) to Ord(etBounceOutIn) do
  begin
    EaseName := GetEnumName(TypeInfo(TEaseType), Ord(TEaseType(i)));
    EaseTypeA.Items.Add(EaseName);
    EaseTypeB.Items.Add(EaseName);
  end;

  EaseTypeA.ItemIndex := Ord(etLinear);
  EaseTypeB.ItemIndex := Ord(etLinear);
end;

procedure TFormTweening.GLPlane1Progress(Sender: TObject; const deltaTime, newTime: Double);
begin

  // Execute following code only if we are in state asGoToStateA
  if FGLPlane1AnimationData.State = asGoToStateA then
  begin
    // First we initialize value if requested
    if FGLPlane1AnimationData.Init = True then
      with FGLPlane1AnimationData do
      begin
        Init := False;
        Time.Initial := newTime;

        if UseCurrentPosition.Checked then
          Initial3fValue := GLPlane1.Position.AsAffineVector
        else
          Initial3fValue := PointB.Position.AsAffineVector;
      end;

    // Then we use the Tweener function to compute interpolation based on Initial values (time and position) and current ones
    with FGLPlane1AnimationData do
    begin
      Time.Diff := newTime - Time.Initial;
      GLPlane1.Position.AsAffineVector := Tweener(Initial3fValue, PointA.Position.AsAffineVector, Time.Diff, TimeA.Value/1000, TEaseType(EaseTypeA.ItemIndex));

      if Time.Diff >= TimeA.Value/1000 then
        State := asWaiting;
    end;
  end;

  // Execute following code only if we are in state asGoToStateB
  if FGLPlane1AnimationData.State = asGoToStateB then
  begin
    // First we initialize value if requested
    if FGLPlane1AnimationData.Init = True then
      with FGLPlane1AnimationData do
      begin
        Init := False;
        Time.Initial := newTime;

        if UseCurrentPosition.Checked then
          Initial3fValue := GLPlane1.Position.AsAffineVector
        else
          Initial3fValue := PointA.Position.AsAffineVector;
      end;

    // Then we use the Tweener function to compute interpolation based on Initial values (time and position) and current ones
    with FGLPlane1AnimationData do
    begin
      Time.Diff := newTime - Time.Initial;
      GLPlane1.Position.AsAffineVector := Tweener(Initial3fValue, PointB.Position.AsAffineVector, Time.Diff, TimeB.Value/1000, TEaseType(EaseTypeB.ItemIndex));

      if Time.Diff >= TimeB.Value/1000 then
        State := asWaiting;
    end;
  end;

end;

procedure TFormTweening.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if ssCtrl in Shift then
  begin
    if (Button = TMouseButton.mbLeft) then
    begin
      PointA.Position.X := X;
      PointA.Position.Y := GLSceneViewer1.Height-Y;
    end
    else if (Button = TMouseButton.mbRight) then
    begin
      PointB.Position.X := X;
      PointB.Position.Y := GLSceneViewer1.Height-Y;
    end;
  end;
end;

end.
