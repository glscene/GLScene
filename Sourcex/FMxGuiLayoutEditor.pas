//
// The graphics engine GXScene https://github.com/glscene
//
unit FMxGuiLayoutEditor;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Rtti,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Layouts,
  FMX.ListBox,
  FMX.Grid,
  FMX.Controls.Presentation,
  FMX.Edit,
  FMX.EditBox,
  FMX.SpinBox,
  FMX.Objects,

  GXS.Gui, 
  FMX.Grid.Style, 
  FMX.ScrollBox;

type
  TGUILayoutForm = class(TForm)
    Panel1: TPanel;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Panel2: TPanel;
    Items_List: TListBox;
    Open_Image_Button: TButton;
    Open_Button: TButton;
    Save_Button: TButton;
    Done_Button: TButton;
    ButtonZoomIn: TButton;
    Add_Button: TButton;
    Delete_Button: TButton;
    Name_Edit: TEdit;
    LabelName: TLabel;
    Elements_Grid: TStringGrid;
    LabelX: TLabel;
    LabelY: TLabel;
    LabelHeight: TLabel;
    LabelWidth: TLabel;
    Left_Edit: TSpinBox;
    Top_Edit: TSpinBox;
    Width_Edit: TSpinBox;
    Height_Edit: TSpinBox;
    ScrollBox1: TScrollBox;
    PaintBox1: TPaintBox;
    Image1: TImage;
    Image2: TImage;
    X_Label: TLabel;
    Y_Label: TLabel;
    ImageZoomIn: TImage;
    ButtonZoomOut: TButton;
    ImageZoomOut: TImage;
    ButtonShow: TButton;
    ImageShow: TImage;
    ImageDone: TImage;
    ImageOpen: TImage;
    ImageSave: TImage;
    ImageLoadSkin: TImage;
    ImageAdd: TImage;
    ImageDelete: TImage;
    procedure Items_ListClick(Sender: TObject);
  private
    Rect_point1, Rect_point2: TPoint;
    Sorted_Elements: array[0..9] of TgxGuiElement;
    procedure SyncImages;
    procedure DrawCurrentElement;
    procedure RefreshComponentBox;
    function GetEnabledSpins: Boolean;
    procedure SetEnabledSpins(Value: Boolean);
  public
  end;

var
  GUILayoutForm: TGUILayoutForm;

//======================================================================
implementation
//======================================================================

{$R *.fmx}

var
  vGUILayoutEditor: TGUILayoutForm;
  Zoom: integer = 1;

//---------------------------------------------------------------
// TGUILayoutForm
//---------------------------------------------------------------
procedure TGUILayoutForm.DrawCurrentElement;
begin
  with Elements_Grid do
    if (Items_List.ItemIndex > -1) and (Sorted_elements[ColumnIndex + 3 * Selected] <> nil)
      then
      with Sorted_Elements[ColumnIndex + 3 * Selected], Image2.Canvas do
      begin
        { TODO : E2250 There is no overloaded version of 'FillRect' not enouph arguments }
        (*FillRect(ClipRect);*)
        Rect(zoom * Round(TopLeft.X), zoom * Round(TopLeft.Y),
          zoom * Round(BottomRight.X), zoom * Round(BottomRight.Y));
      end;
end;

function TGUILayoutForm.GetEnabledSpins: Boolean;
begin
  Result := Left_Edit.Enabled;
end;

procedure TGUILayoutForm.Items_ListClick(Sender: TObject);
var
  i, p: integer;
begin
  if Items_list.ItemIndex = -1 then
    Exit;
  { TODO : E2003 Undeclared identifier: 'GuiLayout1' - GXScene not installed}
  (*Name_edit.Text := GuiLayout1.GuiComponents[Items_list.ItemIndex].Name;*)
  Elements_grid.Selected := 0;    //in VCL Row
  Elements_grid.ColumnIndex := 0; //in VCL Col
  for i := 0 to Length(Sorted_elements) - 1 do
  begin
    sorted_elements[i] := nil;
    if I < 9 then
      Elements_grid.Cells[i mod 3, i div 3] := #32;
  end;

 { TODO : E2003 Undeclared identifier: 'GuiLayout1' - GLScene not installed}
 (*
  with GuiLayout1.GuiComponents[Items_list.ItemIndex] do
    for i := 0 to Elements.Count - 1 do
    begin
      p := Integer(Elements[i].Align);
      Sorted_elements[p] := Elements[i];
      Elements_grid.Cells[p mod 3, p div 3] := '+';
    end;
  Elements_gridClick(nil);
  *)
end;

procedure TGUILayoutForm.RefreshComponentBox;
var
  i: integer;
begin
  Items_List.Clear;
  { TODO : E2003 Undeclared identifier: 'GLGuiLayout1' - GLScene not installed}
  (*
  for i := 0 to GLGuiLayout1.GuiComponents.Count - 1 do
    Items_List.Items.Add(GLGuiLayout1.GuiComponents[i].Name);
  *)
  Items_List.ItemIndex := 0;
  Items_ListClick(nil);
end;

procedure TGUILayoutForm.SetEnabledSpins(Value: Boolean);
begin
  Left_Edit.Enabled := Value;
  Top_Edit.Enabled := Value;
  Height_Edit.Enabled := Value;
  Width_Edit.Enabled := Value;
end;

procedure TGUILayoutForm.SyncImages;
begin
  Image2.Width := Image1.Width;
  Image2.Height := Image1.Height;
  Image2.Bitmap.Width := Round(Image1.Width);
  Image2.Bitmap.Height := Round(Image1.Height);
  PaintBox1.Width := Image1.Width;
  PaintBox1.Height := Image1.Height;
  { TODO : E2003 Undeclared identifier: 'HorzScrollBar' }
  (*
  ScrollBox1.HorzScrollBar.Range := Image1.Width;
  ScrollBox1.VertScrollBar.Range := Image1.Height;
  PaintBox1.Canvas.CopyRect(PaintBox1.Canvas.ClipRect,
            Image1.Canvas, Image1.Canvas.ClipRect);
  *)
end;

end.
