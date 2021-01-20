//
// The graphics rendering engine GLScene http://glscene.org
//
unit FmGuiLayoutEditor;

interface

{$I GLScene.inc}

uses
  Winapi.Windows,
  Winapi.ShellApi,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Buttons,
  Vcl.ExtDlgs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Samples.Spin,
  Vcl.Grids,
  
  GLS.BaseClasses,
  GLS.Gui,
  GLS.Strings;

type
  TGLLayoutEditorForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    items_list: TListBox;
    x_label: TLabel;
    y_label: TLabel;
    open_image_button: TBitBtn;
    open_button: TBitBtn;
    save_button: TBitBtn;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    delete_item_button: TBitBtn;
    add_button: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    left_edit: TSpinEdit;
    top_edit: TSpinEdit;
    Label3: TLabel;
    Label4: TLabel;
    height_edit: TSpinEdit;
    width_edit: TSpinEdit;
    Label5: TLabel;
    name_edit: TEdit;
    elements_grid: TStringGrid;
    Panel3: TPanel;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    ScrollBox1: TScrollBox;
    Image2: TImage;
    PaintBox1: TPaintBox;
    Image1: TImage;
    BitBtn6: TBitBtn;
    BitBtn1: TBitBtn;
    GLGuiLayout1: TGLGuiLayout;
    procedure open_image_buttonClick(Sender: TObject);
    procedure open_buttonClick(Sender: TObject);
    procedure save_buttonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure add_buttonClick(Sender: TObject);
    procedure delete_item_buttonClick(Sender: TObject);
    procedure items_listClick(Sender: TObject);
    procedure name_editExit(Sender: TObject);
    procedure name_editKeyPress(Sender: TObject; var Key: Char);
    procedure elements_gridClick(Sender: TObject);
    procedure left_editChange(Sender: TObject);
    procedure top_editChange(Sender: TObject);
    procedure width_editChange(Sender: TObject);
    procedure height_editChange(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure elements_gridDblClick(Sender: TObject);
  private
     
    procedure SyncImages;
    procedure DrawCurrentElement;
    procedure RefreshComponentBox;
    function GetEnabledSpins: Boolean;
    procedure SetEnabledSpins(Value: Boolean);
  public
    procedure Execute(AGUILayout: TGLGuiLayout);
    property EnabledSpins: Boolean read GetEnabledSpins write SetEnabledSpins;
  end;

function GUILayoutEditorForm: TGLLayoutEditorForm;
procedure ReleaseGUILayoutEditorForm;

//-------------------------------------------------
implementation
//-------------------------------------------------

{$R *.dfm}

uses
  GLS.Utils;

var
  vGUILayoutEditorForm: TGLLayoutEditorForm;
  rect_point1, rect_point2: TPoint;
  zoom: integer = 1;
  sorted_elements: array[0..9] of TGLGuiElement;

function GUILayoutEditorForm: TGLLayoutEditorForm;
begin
  if not Assigned(vGUILayoutEditorForm) then
    vGUILayoutEditorForm := TGLLayoutEditorForm.Create(nil);
  Result := vGUILayoutEditorForm;
end;

procedure ReleaseGUILayoutEditorForm;
begin
  if Assigned(vGUILayoutEditorForm) then
  begin
    vGUILayoutEditorForm.Free;
    vGUILayoutEditorForm := nil;
  end;
end;

function SnapPoint(X, Y: integer): TPoint;
begin
  Result.X := (X div zoom) * zoom;
  Result.Y := (Y div zoom) * zoom;
end;

procedure TGLLayoutEditorForm.SetEnabledSpins(Value: Boolean);
begin
  left_edit.Enabled := Value;
  top_edit.Enabled := Value;
  height_edit.Enabled := Value;
  width_edit.Enabled := Value;
end;

procedure TGLLayoutEditorForm.SyncImages;
begin
  Image2.Width := Image1.Width;
  Image2.Height := Image1.Height;
  Image2.Picture.Bitmap.Width := Image1.Width;
  Image2.Picture.Bitmap.Height := Image1.Height;
  PaintBox1.Width := Image1.Width;
  PaintBox1.Height := Image1.Height;
  ScrollBox1.HorzScrollBar.Range := Image1.Width;
  ScrollBox1.VertScrollBar.Range := Image1.Height;
  PaintBox1.Canvas.CopyRect(PaintBox1.Canvas.ClipRect,
    Image1.Canvas, Image1.Canvas.ClipRect);
end;

procedure TGLLayoutEditorForm.DrawCurrentElement;
begin
  with elements_grid do
    if (items_list.ItemIndex > -1) and (sorted_elements[Col + 3 * Row] <> nil)
      then
      with sorted_elements[Col + 3 * Row], Image2.Canvas do
      begin
        FillRect(ClipRect);
        Rectangle(Rect(zoom * Round(TopLeft.X), zoom * Round(TopLeft.Y),
          zoom * Round(BottomRight.X), zoom * Round(BottomRight.Y)));
      end;
end;

procedure TGLLayoutEditorForm.open_image_buttonClick(Sender: TObject);
var
  LFileName: string;
begin
  LFileName := '';
  if OpenPictureDialog(LFileName) then
    try
      Image1.Stretch := false;
      Image1.AutoSize := true;
      Image1.Picture.LoadFromFile(LFileName);
      Image1.AutoSize := false;
      Image1.Stretch := true;
      Image2.Canvas.Pen.Width := 1;
      SyncImages;
      zoom := 1;
    except
      Application.MessageBox('Unable to load picture!', 'Error', MB_ICONERROR);
    end;
end;

procedure TGLLayoutEditorForm.RefreshComponentBox;
var
  i: integer;
begin
  items_list.Clear;
  for i := 0 to GLGuiLayout1.GuiComponents.Count - 1 do
    items_list.Items.Add(GLGuiLayout1.GuiComponents[i].Name);
  items_list.ItemIndex := 0;
  items_listClick(nil);
end;

procedure TGLLayoutEditorForm.open_buttonClick(Sender: TObject);

begin
  case Application.MessageBox('Save layout?',
    'Question', MB_ICONQUESTION + MB_YESNOCANCEL) of
    mrYes: save_buttonClick(nil);
    mrCancel: Exit;
  end;

  if OpenDialog1.Execute then
    try
      GLGuiLayout1.Clear;
      GLGuiLayout1.LoadFromFile(OpenDialog1.FileName);
      RefreshComponentBox;
    except
      Application.MessageBox('Unable to load layout!', 'Error', MB_ICONERROR);
    end;
end;

procedure TGLLayoutEditorForm.save_buttonClick(Sender: TObject);
begin
  if SaveDialog1.FileName = '' then
    if SaveDialog1.Execute then
      GLGuiLayout1.SaveToFile(SaveDialog1.FileName)
    else
  else
    GLGuiLayout1.SaveToFile(SaveDialog1.FileName);
end;

procedure TGLLayoutEditorForm.FormCreate(Sender: TObject);
begin
  rect_point1.X := -1;
  Image2.Canvas.FillRect(Image2.Canvas.ClipRect);
  Image2.Canvas.Pen.Color := clAqua;
end;

function TGLLayoutEditorForm.GetEnabledSpins: Boolean;
begin
  Result := left_edit.Enabled;
end;

procedure TGLLayoutEditorForm.Image1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  x_label.Caption := 'X: ' + IntToStr(X div zoom);
  y_label.Caption := 'Y: ' + IntToStr(Y div zoom);

  if not (ssRight in Shift) then
    Exit;
  if (X < 0) or (Y < 0) or (X > Image2.Width) or (Y > Image2.Height) then
    Exit;

  if rect_point1.X < 0 then
    rect_point1 := SnapPoint(X, Y)
  else
    with Image2.Canvas do
    begin
      FillRect(ClipRect);
      rect_point2 := SnapPoint(X, Y);
      Rectangle(rect_point1.x, rect_point1.y, X, Y);
    end;

  if items_list.ItemIndex = -1 then
    Exit;
  if rect_point1.X < rect_point2.X then
    left_edit.Value := rect_point1.X div zoom
  else
    left_edit.Value := rect_point2.X div zoom;
  if rect_point1.Y > rect_point2.Y then
    top_edit.Value := rect_point2.Y div zoom
  else
    top_edit.Value := rect_point1.Y div zoom;

  width_edit.Value := Abs(rect_point2.X - rect_point1.X) div zoom;
  height_edit.Value := Abs(rect_point2.Y - rect_point1.Y) div zoom;
end;

procedure TGLLayoutEditorForm.Image1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not (ssRight in Shift) then
    Exit;
  with Image2.Canvas do
  begin
    FillRect(ClipRect);
    rect_point1 := SnapPoint(X, Y);
    rect_point2 := rect_point1;
  end;
end;

procedure TGLLayoutEditorForm.Image1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not (ssRight in Shift) then
    Exit;
  rect_point1.X := -1;
end;

procedure TGLLayoutEditorForm.add_buttonClick(Sender: TObject);
var
  i: integer;
begin
  with TGLGuiComponent(GLGuiLayout1.GuiComponents.Add) do
  begin
    Name := InputBox('Question', 'Name of new region:', '');
    if Name = '' then
    begin
      Free;
      Exit;
    end;
    items_list.Items.Add(Name);
    items_list.ItemIndex := items_list.Count - 1;
    for i := 0 to 9 do
    begin
      sorted_elements[i] := TGLGuiElement(Elements.Add);
      sorted_elements[i].Align := TGUIAlignments(i);
    end;
    name_edit.Text := Name;
  end;
end;

procedure TGLLayoutEditorForm.delete_item_buttonClick(Sender: TObject);
begin
  if items_list.ItemIndex = -1 then
    Exit;
  GLGuiLayout1.GuiComponents.Delete(items_list.ItemIndex);
  items_list.DeleteSelected;
  if items_list.ItemIndex > -1 then
    name_edit.Text := GLGuiLayout1.GuiComponents[items_list.ItemIndex].Name
  else
    name_edit.Text := '';
end;

procedure TGLLayoutEditorForm.items_listClick(Sender: TObject);
var
  i, p: integer;
begin
  if items_list.ItemIndex = -1 then
    Exit;

  name_edit.Text := GLGuiLayout1.GuiComponents[
    items_list.ItemIndex].Name;
  elements_grid.Row := 0;
  elements_grid.Col := 0;
  for i := 0 to Length(sorted_elements) - 1 do
  begin
    sorted_elements[i] := nil;
    if I < 9 then
      elements_grid.Cells[i mod 3, i div 3] := #32;
  end;

  with GLGuiLayout1.GuiComponents[items_list.ItemIndex] do
    for i := 0 to Elements.Count - 1 do
    begin
      p := Integer(Elements[i].Align);
      sorted_elements[p] := Elements[i];
      elements_grid.Cells[p mod 3, p div 3] := '+';
    end;
  elements_gridClick(nil);
end;

procedure TGLLayoutEditorForm.name_editExit(Sender: TObject);
begin
  if items_list.ItemIndex > -1 then
  begin
    GLGuiLayout1.GuiComponents[items_list.
      ItemIndex].Name := name_edit.Text;
    items_list.Items[items_list.ItemIndex] := name_edit.Text;
    items_listClick(nil);
  end;
end;

procedure TGLLayoutEditorForm.name_editKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
    name_editExit(nil);
end;

procedure TGLLayoutEditorForm.elements_gridClick(Sender: TObject);
begin
  with elements_grid do
    if (items_list.ItemIndex > -1) and (sorted_elements[Col + 3 * Row] <> nil)
      then
      with sorted_elements[Col + 3 * Row] do
      begin
        EnabledSpins := True;
        left_edit.Value := Round(TopLeft.X);
        top_edit.Value := Round(TopLeft.Y);
        width_edit.Value := Round(BottomRight.X - TopLeft.X);
        height_edit.Value := Round(BottomRight.Y - TopLeft.Y);
        DrawCurrentElement;
      end
    else
    begin
      EnabledSpins := False;
      Image2.Canvas.FillRect(Image2.Canvas.ClipRect);
    end;
end;

procedure TGLLayoutEditorForm.elements_gridDblClick(Sender: TObject);
var
  I: Integer;
  E: TGLGuiElement;
begin
  if items_list.ItemIndex > -1 then
    with elements_grid do
    begin
      if Assigned(sorted_elements[Col + 3 * Row]) then
      begin
        I := GLGuiLayout1.GuiComponents[items_list.ItemIndex].Elements.IndexOf(sorted_elements[Col + 3 * Row]);
        GLGuiLayout1.GuiComponents[items_list.ItemIndex].Elements.Delete(I);
        sorted_elements[Col + 3 * Row] := nil;
        Cells[Col, Row] := #32;
        elements_gridClick(nil);
      end
      else begin
        E := TGLGuiElement(GLGuiLayout1.GuiComponents[items_list.ItemIndex].Elements.Add);
        E.Align := TGUIAlignments(Col + 3 * Row);
        sorted_elements[Col + 3 * Row] := E;
        Cells[Col, Row] := '+';
        elements_gridClick(nil);
      end;
    end;
end;

procedure TGLLayoutEditorForm.left_editChange(Sender: TObject);
begin
  if (items_list.ItemIndex = -1) or not EnabledSpins then
    Exit;
  if left_edit.Value + width_edit.Value > Image2.Width div zoom then
    left_edit.Value := (Image2.Width div zoom) - width_edit.Value;
  if left_edit.Value < 0 then
    left_edit.Value := 0;
  with elements_grid do
    sorted_elements[Col + 3 * Row].TopLeft.X := left_edit.Value;
  DrawCurrentElement;
end;

procedure TGLLayoutEditorForm.top_editChange(Sender: TObject);
begin
  if (items_list.ItemIndex = -1) or not EnabledSpins then
    Exit;
  if top_edit.Value + height_edit.Value > Image2.Height div zoom then
    top_edit.Value := (Image2.Height div zoom) - height_edit.Value;
  if top_edit.Value < 0 then
    top_edit.Value := 0;
  with elements_grid do
    sorted_elements[Col + 3 * Row].TopLeft.Y := top_edit.Value;
  DrawCurrentElement;
end;

procedure TGLLayoutEditorForm.width_editChange(Sender: TObject);
begin
  if (items_list.ItemIndex = -1) or not EnabledSpins then
    Exit;
  with elements_grid do
    sorted_elements[Col + 3 * Row].BottomRight.X := left_edit.Value +
      width_edit.Value;
  if left_edit.Value + width_edit.Value > Image2.Width div zoom then
    width_edit.Value := (Image2.Width div zoom) - left_edit.Value;
  if width_edit.Value < 0 then
    width_edit.Value := 0;
  DrawCurrentElement;
end;

procedure TGLLayoutEditorForm.height_editChange(Sender: TObject);
begin
  if (items_list.ItemIndex = -1) or not EnabledSpins  then
    Exit;
  with elements_grid do
    sorted_elements[Col + 3 * Row].BottomRight.Y := top_edit.Value +
      height_edit.Value;
  if top_edit.Value + height_edit.Value > Image2.Height div zoom then
    height_edit.Value := (Image2.Height div zoom) - top_edit.Value;
  if height_edit.Value < 0 then
    height_edit.Value := 0;
  DrawCurrentElement;
end;

procedure TGLLayoutEditorForm.BitBtn4Click(Sender: TObject);
begin
  if zoom + TBitBtn(Sender).Tag < 1 then
    Exit;
  Image1.Width := (Image1.Width div zoom) * (zoom + TBitBtn(Sender).Tag);
  Image1.Height := (Image1.Height div zoom) * (zoom + TBitBtn(Sender).Tag);
  SyncImages;
  zoom := zoom + TBitBtn(Sender).Tag;
  Image2.Canvas.Pen.Width := zoom;
  elements_gridClick(nil);
end;

procedure TGLLayoutEditorForm.BitBtn6Click(Sender: TObject);
{$IFDEF LINUX}
var
  lProcess: TProcess;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  ShellExecuteW(0, 'open', 'mspaint', '', '', SW_SHOW)
{$ENDIF}
{$IFDEF LINUX}
  lProcess := TProcess.Create(nil);
  lProcess.CommandLine := 'gimp';
  try
    lProcess.Execute;
  finally
    lProcess.Destroy;
  end;
{$ENDIF}
end;

procedure TGLLayoutEditorForm.Execute(AGUILayout: TGLGuiLayout);
begin
  GLGuiLayout1.Assign(AGUILayout);
  Image1.Stretch := false;
  Image1.AutoSize := true;
  Image1.Picture.Assign(AGUILayout.Material.GetActualPrimaryTexture.Image.GetBitmap32.Create32BitsBitmap);
  Image1.AutoSize := false;
  Image1.Stretch := true;
  Image2.Canvas.Pen.Width := 1;
  SyncImages;
  zoom := 1;

  RefreshComponentBox;
  if ShowModal = mrOk then
    AGUILayout.Assign(GLGuiLayout1);
end;

//-------------------------------------
initialization
//-------------------------------------

finalization

  ReleaseGUILayoutEditorForm;

end.

