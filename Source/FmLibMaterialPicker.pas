//
// The multimedia graphics platform GLScene https://github.com/glscene
//
unit FmLibMaterialPicker;

(* Allows choosing a material in a material library *)

interface

{$I GLScene.inc}

uses
  System.Classes,
  System.Types,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.Controls,
  Vcl.Graphics,

  GLS.Scene,
  GLS.Objects,
  GLS.SceneViewer,
  GLS.Material,
  GLS.Color,
  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.HUDObjects,
  GLS.GeomObjects;

type
  TGLLibMaterialPickerForm = class(TForm)
    ListBoxMaterials: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    BitBtnOk: TBitBtn;
    BitBtnCancel: TBitBtn;
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    LightSource1: TGLLightSource;
    dcLight1: TGLDummyCube;
    dcWorld1: TGLDummyCube;
    Camera1: TGLCamera;
    Cube1: TGLCube;
    Sphere1: TGLSphere;
    FireSphere1: TGLSphere;
    Teapot1: TGLTeapot;
    GLMaterialLibrary1: TGLMaterialLibrary;
    ComboBoxObject1: TComboBox;
    ComboBoxBackGround1: TComboBox;
    BackGroundSprite1: TGLHUDSprite;
    LightSource2: TGLLightSource;
    procedure ListBoxMaterialsClick(Sender: TObject);
    procedure ListBoxMaterialsKeyPress(Sender: TObject; var Key: Char);
    procedure ListBoxMaterialsDblClick(Sender: TObject);
    procedure ComboBoxObject1Change(Sender: TObject);
    procedure ComboBoxBackGround1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint; var Handled: Boolean);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
  private
    mx, my: Integer;

    FLibMaterial1: TGLAbstractLibMaterial;
    function GetMaterial1: TGLMaterial;
    procedure SetMaterial1(const Value: TGLMaterial);
    function GetLibMaterial1: TGLAbstractLibMaterial;
    procedure SetLibMaterial1(const Value: TGLAbstractLibMaterial);

  public
    function Execute(var MaterialName: TGLLibMaterialName;
      MaterialLibrary: TGLAbstractMaterialLibrary): Boolean;
    property Material1: TGLMaterial read GetMaterial1 write SetMaterial1;
    property LibMaterial1: TGLAbstractLibMaterial read GetLibMaterial1 write SetLibMaterial1;

  end;

// For registration in GLScene
function GLLibMaterialPickerForm: TGLLibMaterialPickerForm;
procedure ReleaseLibMaterialPickerForm;

//-------------------------------------------------
implementation
//-------------------------------------------------

{$R *.dfm}

var
  vGLLibMaterialPickerForm: TGLLibMaterialPickerForm;

// ------------ GLLibMaterialPickerForm -----------------

function GLLibMaterialPickerForm: TGLLibMaterialPickerForm;
begin
  if not Assigned(vGLLibMaterialPickerForm) then
    vGLLibMaterialPickerForm := TGLLibMaterialPickerForm.Create(nil);
  Result := vGLLibMaterialPickerForm;
end;

// ----------- ReleaseLibMaterialPickerForm --------------

procedure ReleaseLibMaterialPickerForm;
begin
  if Assigned(vGLLibMaterialPickerForm) then
  begin
    vGLLibMaterialPickerForm.Free;
    vGLLibMaterialPickerForm := nil;
  end;
end;

//------------------- FormCreate --------------------------

procedure TGLLibMaterialPickerForm.FormCreate(Sender: TObject);
begin
  // BackGroundSprite positions and size
  BackGroundSprite1.Position.X := GLSceneViewer1.Width div 2;
  BackGroundSprite1.Position.Y := GLSceneViewer1.Height div 2;
  BackGroundSprite1.Width := GLSceneViewer1.Width;
  BackGroundSprite1.Height := GLSceneViewer1.Height;

  ComboBoxObject1.ItemIndex := 0;
  ComboBoxObject1Change(Self);
  ComboBoxBackground1.ItemIndex := 0;
  ComboBoxBackground1Change(Self);
end;

// ----------------------- MouseMove ----------------------

procedure TGLLibMaterialPickerForm.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

procedure TGLLibMaterialPickerForm.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if (ssRight in Shift) and (ssLeft in Shift) then
    Camera1.AdjustDistanceToTarget(1 - 0.01 * (my - Y))
  else if (ssRight in Shift) or (ssLeft in Shift) then
    Camera1.MoveAroundTarget(Y - my, X - mx);
  mx := X;
  my := Y;
end;

// ----------------------- MouseWheel ----------------------
procedure TGLLibMaterialPickerForm.GLSceneViewer1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Camera1.AdjustDistanceToTarget(1 - 0.1 * (Abs(WheelDelta) / WheelDelta));
end;


//------------------ Execute --------------------------

function TGLLibMaterialPickerForm.Execute(var MaterialName: TGLLibMaterialName;
  MaterialLibrary: TGLAbstractMaterialLibrary): Boolean;
begin
  MaterialLibrary.SetNamesToTStrings(ListBoxMaterials.Items);
  ListBoxMaterials.ItemIndex := ListBoxMaterials.Items.IndexOf(MaterialName);
  if ((ListBoxMaterials.ItemIndex < 0) and (ListBoxMaterials.Items.Count > 0)) then
    ListBoxMaterials.ItemIndex := 0;
  BitBtnOk.Enabled := (ListBoxMaterials.Items.Count > 0);

  ListBoxMaterialsClick(Self);
  Result := (ShowModal = mrOk);
  if Result then
  begin
    if ListBoxMaterials.ItemIndex >= 0 then
      MaterialName := ListBoxMaterials.Items[ListBoxMaterials.ItemIndex]
    else
      MaterialName := '';
  end;
end;

//-------------------------------------------------------------------

function TGLLibMaterialPickerForm.GetLibMaterial1: TGLAbstractLibMaterial;
begin
   Result := FLibMaterial1;
end;

function TGLLibMaterialPickerForm.GetMaterial1: TGLMaterial;
begin
   Result := GLMaterialLibrary1.Materials[0].Material;
end;


procedure TGLLibMaterialPickerForm.ListBoxMaterialsClick(Sender: TObject);
begin
  if ListBoxMaterials.ItemIndex >= 0 then
    LibMaterial1 := TGLAbstractLibMaterial(ListBoxMaterials.Items.Objects[ListBoxMaterials.ItemIndex]);
   Cube1.Material :=  Material1;
   Sphere1.Material := Material1;
   TeaPot1.Material := Material1;
end;

procedure TGLLibMaterialPickerForm.ListBoxMaterialsKeyPress(Sender: TObject;
  var Key: Char);
begin
  ListBoxMaterialsClick(Sender);
end;

procedure TGLLibMaterialPickerForm.SetLibMaterial1(const Value: TGLAbstractLibMaterial);
begin
  FLibMaterial1 := Value;
  if Assigned(FLibMaterial1) then
  begin
    GLMaterialLibrary1.Materials[0].Material.MaterialLibrary := FLibMaterial1.MaterialLibrary;
    GLMaterialLibrary1.Materials[0].Material.LibMaterialName := FLibMaterial1.Name
  end
  else
  begin
    GLMaterialLibrary1.Materials[0].Material.MaterialLibrary := nil;
    GLMaterialLibrary1.Materials[0].Material.LibMaterialName := '';
  end;
end;

procedure TGLLibMaterialPickerForm.SetMaterial1(const Value: TGLMaterial);
begin
  GLMaterialLibrary1.Materials[0].Material.Assign(Value.GetActualPrimaryMaterial);
end;

// ------------------- ComboBoxObject1Change -----------------------------

procedure TGLLibMaterialPickerForm.ComboBoxObject1Change(Sender: TObject);
begin
  var i: Integer := ComboBoxObject1.ItemIndex;
  Cube1.Visible := i = 0;
  Sphere1.Visible := i = 1;
  Teapot1.Visible := i = 2;
end;

// ------------------- ComboBoxBackGround1Change -----------------------------

procedure TGLLibMaterialPickerForm.ComboBoxBackGround1Change(Sender: TObject);
var
  bgColor: TColor;  // from Vcl.Graphics
begin
  case ComboBoxBackground1.ItemIndex of
    1: bgColor := clWhite;
    2: bgColor := clBlack;
    3: bgColor := clBlue;
    4: bgColor := clRed;
    5: bgColor := clGreen;
  else
    bgColor := clNone;   // 0 - pattern background
  end;
  BackGroundSprite1.Material.Texture.Disabled := (bgColor <> clNone);
  BackGroundSprite1.Material.FrontProperties.Diffuse.Color := ConvertWinColor(bgColor);
end;


procedure TGLLibMaterialPickerForm.ListBoxMaterialsDblClick(Sender: TObject);
begin
  BitBtnOk.Click;
end;


//-----------------------------------------------------------------
initialization
//-----------------------------------------------------------------

finalization

 ReleaseLibMaterialPickerForm;

end.
