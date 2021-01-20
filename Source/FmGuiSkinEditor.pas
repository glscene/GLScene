//
// The graphics rendering engine GLScene http://glscene.org
//
unit FmGuiSkinEditor;

(*  Editor for Gui skin. *)

interface

{$I GLScene.inc}

uses
  Winapi.Windows, 
  Winapi.Messages, 
  System.SysUtils, 
  System.Classes,
  VCL.Graphics, 
  VCL.Controls, 
  VCL.Forms, 
  VCL.Dialogs,
  VCL.StdCtrls, 
  VCL.ComCtrls, 
  VCL.ExtCtrls, 
  VCL.Menus,

  GLS.Texture, 
  GLS.Scene, 
  GLS.Objects, 
  GLS.Windows, 
  GLS.HudObjects,
  GLS.SceneViewer, 
  GLS.Gui, 
  GLS.Graphics, 
  GLS.Utils,
  GLS.Coordinates,
  GLS.BaseClasses, 
  GLS.Material;

type
  TGLSkinEditorForm = class(TForm)
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLPanel1: TGLPanel;
    HUDSprite1: TGLHUDSprite;
    GLMemoryViewer1: TGLMemoryViewer;
    GLLightSource1: TGLLightSource;
    StatusBar: TStatusBar;
    panBottom: TPanel;
    panZoomImage: TPanel;
    imgFull: TImage;
    sbarHorizontal: TScrollBar;
    sbarVertical: TScrollBar;
    Button5: TButton;
    Button6: TButton;
    panImageProperties: TPanel;
    Label5: TLabel;
    Label6: TLabel;
    Panel2: TPanel;
    imgPreview: TImage;
    Panel3: TPanel;
    Label2: TLabel;
    Label1: TLabel;
    Button3: TButton;
    Button4: TButton;
    CheckBox1: TCheckBox;
    WidthEdit: TEdit;
    HeightEdit: TEdit;
    panElements: TPanel;
    Bevel2: TBevel;
    Bevel1: TBevel;
    Label3: TLabel;
    Label4: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label14: TLabel;
    lbElements: TListBox;
    btnAdd: TButton;
    btnDelete: TButton;
    ComboBox1: TComboBox;
    LeftEdit: TEdit;
    TopEdit: TEdit;
    RightEdit: TEdit;
    BottomEdit: TEdit;
    ScaleXEdit: TEdit;
    ScaleYEdit: TEdit;
    popElements: TPopupMenu;
    mnuTopLeft: TMenuItem;
    mnuTop: TMenuItem;
    mnuTopRight: TMenuItem;
    mnuLeft: TMenuItem;
    mnuCenter: TMenuItem;
    mnuRight: TMenuItem;
    mnuBottomLeft: TMenuItem;
    mnuBottom: TMenuItem;
    mnuBottomRight: TMenuItem;
    N1: TMenuItem;
    mnuAddAll: TMenuItem;
    N2: TMenuItem;
    mnuAllTop: TMenuItem;
    mnuAllMiddle: TMenuItem;
    mnuAllBottom: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure ScrollbarChange(Sender: TObject);
    procedure WidthEditChange(Sender: TObject);
    procedure HeightEditChange(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure lbElementsClick(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure imgFullMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure imgFullMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure imgFullMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure lbElementsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CheckBox1Click(Sender: TObject);
    procedure ScaleXEditChange(Sender: TObject);
    procedure ScaleYEditChange(Sender: TObject);
    procedure LeftEditChange(Sender: TObject);
    procedure TopEditChange(Sender: TObject);
    procedure RightEditChange(Sender: TObject);
    procedure BottomEditChange(Sender: TObject);
    procedure EditKeyPress(Sender: TObject; var Key: Char);
    procedure FormResize(Sender: TObject);
    procedure MenuItemClick(Sender: TObject);
    procedure mnuAddAllClick(Sender: TObject);
    procedure mnuAllTopClick(Sender: TObject);
    procedure mnuAllMiddleClick(Sender: TObject);
    procedure mnuAllBottomClick(Sender: TObject);
    procedure imgPreviewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure imgPreviewMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    FOriginalWndProc: TWndMethod;
    FFocusRect: TRect;
    VisibleRect: TRect;
    PreviewMousePoint: TPoint;
    PreviewWidth,
      PreviewHeight: Integer;
    FullMousePoint: TPoint;
    MouseDown: Boolean;
    procedure ImageWndProc(var Message: TMessage);
    procedure DrawImageFocusRect(ARect: TRect);
    procedure AlignZoomPanel;
    procedure UpdateRegionEdits;
    procedure SetEditState(Parent: TControl; Enabled: Boolean);
    procedure AddElement(Index: Integer);
    procedure DrawCrossair(Point: TPoint);
  public
    TheGuiComponent: TGLGuiElementList;
    SelectedElement: TGLGUIElement;
    Tex: TGLTexture;
    Zoom: Single;
    Width: Integer;
    Height: Integer;
    function Edit(GuiComponent: TGLGuiElementList): Boolean;
    procedure Render;
    procedure SetMax(Scrollbar: TScrollbar; Val: Integer);
  end;

var
  GLSkinEditorForm: TGLSkinEditorForm;

function GUIComponentDialog(GuiComponent: TGLGuiElementList): Boolean;

//--------------------------------------------------------------------
implementation
//--------------------------------------------------------------------

{$R *.dfm}

function GUIComponentDialog(GuiComponent: TGLGuiElementList): Boolean;
var
  Editor: TGLSkinEditorForm;
begin
  Editor := TGLSkinEditorForm.Create(nil);
  Result := Editor.Edit(GuiComponent);
  Editor.Free;
end;

procedure TGLSkinEditorForm.FormCreate(Sender: TObject);
begin
  //override original WndProc to capture image mouse leave message
  FOriginalWndProc := imgFull.WindowProc;
  imgFull.WindowProc := ImageWndProc;

  Tex := TGLTexture.Create(Self);
  Tex.SetImageClassName('TGLPersistentImage');
  GLPanel1.RedrawAtOnce := True;

  StatusBar.Panels[0].Text := 'X : 0';
  StatusBar.Panels[1].Text := 'Y : 0';
  AlignZoomPanel;
  UpdateRegionEdits;
  DoubleBuffered := True;
  FullMousePoint := Point(-1, -1);

  //this Delphi bug shows all panels transparent
  //the code below is to avoid this bug in XP
  panElements.ParentBackground := False;
  panElements.ParentBackground := True;
  panElements.ParentBackground := False;

  panImageProperties.ParentBackground := False;
  panImageProperties.ParentBackground := True;
  panImageProperties.ParentBackground := False;

  panBottom.ParentBackground := False;
  panBottom.ParentBackground := True;
  panBottom.ParentBackground := False;

  panZoomImage.ParentBackground := False;
  panZoomImage.ParentBackground := True;
  panZoomImage.ParentBackground := False;
end;

procedure TGLSkinEditorForm.FormDestroy(Sender: TObject);
begin
  Tex.Free;
end;

function TGLSkinEditorForm.Edit(GuiComponent: TGLGuiElementList): Boolean;

var
  Mat: TGLMaterial;
  GuiLayout: TGLGuiLayout;
  XC: Integer;

begin
  TheGuiComponent := GuiComponent;
  GuiLayout := (GuiComponent.GetOwner as TGLGuiComponent).Owner.GetOwner as
    TGLGuiLayout;
  Mat := GuiLayout.Material;
  GLPanel1.Visible := True;
  GLPanel1.GuiLayout := GuiLayout;
  GLPanel1.GuiLayoutName := (GuiComponent.GetOwner as TGLGuiComponent).Name;
  Zoom := 1.0;

  if (Assigned(mat.MaterialLibrary)
    and (mat.MaterialLibrary is TGLMaterialLibrary)
    and (Mat.LibMaterialName <> '')) then
  begin
    mat :=
      TGLMaterialLibrary(mat.MaterialLibrary).Materials.GetLibMaterialByName(Mat.LibMaterialName).Material;
  end;
  Width := Mat.Texture.Image.Width;
  Height := Mat.Texture.Image.Height;
  WidthEdit.Text := IntToStr(Mat.Texture.Image.Width);
  HeightEdit.Text := IntToStr(Mat.Texture.Image.Height);

  GLPanel1.GuiLayout.Material.Assign(Mat);

  Tex.Assign(mat.Texture);
  imgPreview.Picture.Bitmap.Canvas.StretchDraw(imgPreview.ClientRect, (Tex.Image
    as TGLPersistentImage).Picture.Graphic);
  PreviewWidth := (Tex.Image as TGLPersistentImage).Picture.Width;
  Previewheight := (Tex.Image as TGLPersistentImage).Picture.Height;

  lbElements.Clear;
  for XC := 0 to TheGuiComponent.Count - 1 do
  begin
    lbElements.Items.Add(TheGuiComponent.Items[XC].Name);
  end;

  if TheGuiComponent.Count > 0 then
  begin
    SelectedElement := TheGuiComponent.Items[0];
    lbElements.ItemIndex := 0;
  end
  else
    SelectedElement := nil;
  Render;
  Result := ShowModal = mrOk;
end;

procedure TGLSkinEditorForm.Button3Click(Sender: TObject);
begin
  Zoom := Zoom + 0.5;
  Label2.Caption := FormatFloat('####0.0', Zoom);

{$IFDEF MSWINDOWS}
  sbarVertical.PageSize := Round(256 / Zoom);
  sbarHorizontal.PageSize := Round(256 / Zoom);
{$ENDIF}
  Render;
end;

procedure TGLSkinEditorForm.Button4Click(Sender: TObject);
begin
  if Abs(Zoom - 0.5) > 0.001 then
    Zoom := Zoom - 0.5;
  Label2.Caption := FormatFloat('####0.0', Zoom);
  //  panel3.Invalidate;

{$IFDEF MSWINDOWS}
  sbarVertical.PageSize := Round(256 / Zoom);
  sbarHorizontal.PageSize := Round(256 / Zoom);
{$ENDIF}
  Render;
end;

procedure TGLSkinEditorForm.Render;
var
  BitMap: TBitmap;
  Image: TGLBitmap32;
begin
  if CheckBox1.Checked then
  begin
    GLPanel1.Width := Width;
    GLPanel1.Height := Height;
    GLPanel1.Left := 1 - sbarHorizontal.position;
    GLPanel1.Top := 1 - sbarVertical.position;

    GLMemoryViewer1.Render;
    Image := GLMemoryViewer1.Buffer.CreateSnapShot;
    ;
    Bitmap := Image.Create32BitsBitmap;
    try
      imgFull.Canvas.Brush.Color := clBlack;
      imgFull.Canvas.FillRect(imgFull.Canvas.ClipRect);
      imgFull.Canvas.StretchDraw(Rect(0, 0, Round(((Tex.Image as
        TGLPersistentImage).Width) * Zoom), Round(((Tex.Image as
        TGLPersistentImage).Height) * Zoom)), Bitmap); {}
    finally
      Bitmap.Free;
      Image.Free;
    end;

    //    imgFull.Canvas.StretchDraw(Rect(Round((1-sbarHorizontal.position)*Zoom),Round((1-sbarVertical.position)*Zoom),Round((1-sbarHorizontal.position+(Tex.Image as TGLPersistentImage).Width)*Zoom),Round((1-sbarVertical.position+(Tex.Image as TGLPersistentImage).Height)*Zoom)),Bitmap);{}
  end
  else
  begin
    imgFull.Canvas.Brush.Color := clBlack;
    imgFull.Canvas.FillRect(imgFull.Canvas.ClipRect);
    imgFull.Canvas.StretchDraw(
      Rect(
      Round((1 - sbarHorizontal.position) * Zoom),
      Round((1 - sbarVertical.position) * Zoom),
      Round((1 - sbarHorizontal.position + (Tex.Image as
        TGLPersistentImage).Width) * Zoom),
      Round((1 - sbarVertical.position + (Tex.Image as
        TGLPersistentImage).Height) * Zoom)
      ),
      (Tex.Image as TGLPersistentImage).Picture.Graphic
      );

    if Assigned(SelectedElement) then
    begin
      imgFull.Canvas.Brush.Style := bsClear;
      imgFull.Canvas.Pen.Color := clWhite;
      imgFull.Canvas.Pen.Style := psSolid;
      imgFull.Canvas.Pen.Mode := pmXor;

      imgFull.Canvas.Rectangle(
        Rect(Round((1 - sbarHorizontal.position + SelectedElement.TopLeft.X) *
          Zoom),
        Round((1 - sbarVertical.position + SelectedElement.TopLeft.Y) * Zoom),
        Round((1 - sbarHorizontal.position + SelectedElement.BottomRight.X) *
          Zoom),
        Round((1 - sbarVertical.position + SelectedElement.BottomRight.Y) * Zoom)
        )
        );
    end;

    //rectangle the part that is visible in the preview
    imgPreview.Canvas.DrawFocusRect(VisibleRect);
    if (PreviewWidth = 0) or (PreviewHeight = 0) then
    begin
      PreviewWidth := 2;
      PreviewHeight := 2;
    end;
    if Zoom = 0 then
      Zoom := 0.5;
    //    {$R-}
    VisibleRect := Rect(
      Round(sbarHorizontal.Position / PreviewWidth * imgPreview.Width),
      Round(sbarVertical.Position / PreviewHeight * imgPreview.Height),
      Round((sbarHorizontal.Position + (imgFull.Width - 1) / Zoom) / PreviewWidth
        * imgPreview.Width),
      Round((sbarVertical.Position + (imgFull.Height - 1) / Zoom) / PreviewHeight
        * imgPreview.Height)
      );
    imgPreview.Canvas.DrawFocusRect(VisibleRect);
  end;

  DrawCrossair(FullMousePoint);
end;

procedure TGLSkinEditorForm.SetMax(Scrollbar: TScrollbar; Val: Integer);
begin
{$IFDEF MSWINDOWS}
  if Scrollbar.Position + Scrollbar.PageSize >= val then
  begin
    Scrollbar.Position := val - Scrollbar.PageSize + 1;
  end;
{$ENDIF}
  Scrollbar.Max := val;
end;

procedure TGLSkinEditorForm.ScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
{$IFDEF MSWINDOWS}
  if ScrollPos + (Sender as TScrollBar).PageSize > (Sender as TScrollBar).Max
    then
    ScrollPos := (Sender as TScrollBar).Max - (Sender as TScrollBar).PageSize +
      1;
{$ENDIF}
end;

procedure TGLSkinEditorForm.ScrollbarChange(Sender: TObject);
begin
  Render;
end;

procedure TGLSkinEditorForm.WidthEditChange(Sender: TObject);
var
  Val: Integer;
begin
  val := StrToIntDef(WidthEdit.Text, 0);
  if Val > 0 then
  begin
    Width := Val;
    GLPanel1.Width := Val;
    GLPanel1.ReBuildGui := True;
    GLPanel1.GUIRedraw := True;
    if Val > 256 then
    begin
      SetMax(sbarHorizontal, Val);
    end
    else
    begin
      SetMax(sbarHorizontal, 256);
    end;
    Render;
  end;
end;

procedure TGLSkinEditorForm.HeightEditChange(Sender: TObject);
var
  Val: Integer;
begin
  val := StrToIntDef(HeightEdit.Text, 0);
  if Val > 0 then
  begin
    Height := Val;
    GLPanel1.Height := Val;
    GLPanel1.ReBuildGui := True;
    GLPanel1.GUIRedraw := True;
    if Val > 256 then
    begin
      SetMax(sbarVertical, Val);
    end
    else
    begin
      SetMax(sbarVertical, 256);
    end;
    Render;
  end;
end;

procedure TGLSkinEditorForm.btnAddClick(Sender: TObject);
var
  PopupPoint: TPoint;
begin
  PopupPoint := btnAdd.Parent.ClientToScreen(Point(btnAdd.Left, btnAdd.Top +
    btnAdd.Height));
  popElements.Popup(PopupPoint.X, PopupPoint.Y);
end;

procedure TGLSkinEditorForm.lbElementsClick(Sender: TObject);
begin
  UpdateRegionEdits;
end;

procedure TGLSkinEditorForm.ComboBox1Change(Sender: TObject);
var
  S: string;
  Count: Integer;
begin
  if Assigned(SelectedElement) then
  begin
    if SelectedElement.Align = TGUIAlignments(ComboBox1.ItemIndex) then
      Exit;

    SelectedElement.Align := TGUIAlignments(ComboBox1.ItemIndex);
    GLPanel1.ReBuildGui := True;
    GLPanel1.GUIRedraw := True;

    Count := 1;
    repeat
      S := ComboBox1.Items[ComboBox1.ItemIndex] + IntToStr(Count);
      inc(Count);
    until lbElements.Items.IndexOf(S) = -1;

    lbElements.Items[lbElements.Itemindex] := S;

    Render;
  end;
end;

procedure TGLSkinEditorForm.btnDeleteClick(Sender: TObject);
var
  Index: Integer;
begin
  if (lbElements.ItemIndex >= 0) and (lbElements.ItemIndex <
    lbElements.Items.Count) then
  begin
    Index := lbElements.ItemIndex;

    TheGuiComponent.Delete(Index);
    lbElements.Items.Delete(Index);

    if Index > lbElements.Items.Count - 1 then
      while (Index > -1) and (Index > lbElements.Items.Count - 1) do
        Dec(Index);

    if Index > -1 then
      lbElements.ItemIndex := Index;

    Render;
  end;

  UpdateRegionEdits;
end;

procedure TGLSkinEditorForm.imgFullMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if SelectedElement = nil then
    exit;

  if Button = TMouseButton(mbLeft) then
  begin
    MouseDown := True;

    if not CheckBox1.Checked then
      if Assigned(SelectedElement) then
      begin
        SelectedElement.TopLeft.X := (sbarHorizontal.Position - 1) + Int(x /
          Zoom);
        SelectedElement.TopLeft.Y := (sbarVertical.Position - 1) + Int(y /
          Zoom);
      end;
  end;
  FFocusRect.TopLeft := Point(Round(Round(X / Zoom - 0.5) * Zoom), Round(Round(Y
    / Zoom - 0.5) * Zoom));
  FFocusRect.BottomRight := FFocusRect.TopLeft;
  DrawImageFocusRect(FFocusRect);
end;

procedure TGLSkinEditorForm.imgFullMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if SelectedElement = nil then
    exit;

  if Button = TMouseButton(mbLeft) then
  begin
    MouseDown := False;
    if not CheckBox1.Checked then
      if Assigned(SelectedElement) then
      begin
        SelectedElement.BottomRight.X := (sbarHorizontal.Position) + Int(x /
          Zoom);
        SelectedElement.BottomRight.Y := (sbarVertical.Position) + Int(y /
          Zoom);
        Render;
      end;
  end;
  StatusBar.Panels[2].Text := 'dx : ';
  StatusBar.Panels[3].Text := 'dy : ';
  DrawImageFocusRect(FFocusRect);
  UpdateRegionEdits;
end;

procedure TGLSkinEditorForm.imgFullMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  DrawCrossair(FullMousePoint);
  FullMousePoint := Point(X, Y);
  DrawCrossair(FullMousePoint);

  if ssLeft in Shift then
  begin
    if SelectedElement <> nil then
    begin
      DrawImageFocusRect(FFocusRect);
      FFocusRect.BottomRight := Point(Round(Round(X / Zoom + 0.5) * Zoom),
        Round(Round(Y / Zoom + 0.5) * Zoom));
      DrawImageFocusRect(FFocusRect);

      StatusBar.Panels[0].Text := 'X : ' + FormatFloat('###0',
        Round(sbarHorizontal.Position + (x - 1) / Zoom));
      StatusBar.Panels[1].Text := 'Y : ' + FormatFloat('###0',
        Round(sbarVertical.Position + (y - 1) / Zoom));
      StatusBar.Panels[2].Text := 'dx : ' + FormatFloat('###0',
        Round(sbarHorizontal.Position + (x - 1) / Zoom -
        SelectedElement.TopLeft.X));
      StatusBar.Panels[3].Text := 'dy : ' + FormatFloat('###0',
        Round(sbarVertical.Position + (y - 1) / Zoom -
        SelectedElement.TopLeft.Y));
    end;
  end
  else
  begin
    StatusBar.Panels[0].Text := 'X : ' + FormatFloat('###0',
      Trunc(sbarHorizontal.Position + (x - 1) / Zoom));
    StatusBar.Panels[1].Text := 'Y : ' + FormatFloat('###0',
      Trunc(sbarVertical.Position + (y - 1) / Zoom));
  end;
end;

procedure TGLSkinEditorForm.lbElementsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = VK_LEFT then
  begin
    if lbElements.ItemIndex > 0 then
      lbElements.ItemIndex := lbElements.ItemIndex - 1;
    key := VK_CANCEL;
  end;
  if key = VK_RIGHT then
  begin
    if lbElements.ItemIndex + 1 < lbElements.Items.Count then
      lbElements.ItemIndex := lbElements.ItemIndex + 1;
    key := VK_CANCEL;
  end;

end;

procedure TGLSkinEditorForm.CheckBox1Click(Sender: TObject);
begin
  GLPanel1.ReBuildGui := True;
  GLPanel1.GUIRedraw := True;
  Render;
end;

procedure TGLSkinEditorForm.ScaleXEditChange(Sender: TObject);
var
  res: Single;
begin
  if Assigned(SelectedElement) then
  begin
    res := StrToFloatDef(ScaleXEdit.Text, 0);
    if res > 0 then
    begin
      SelectedElement.Scale.X := Res;
      GLPanel1.ReBuildGui := True;
      GLPanel1.GUIRedraw := True;
      Render;
    end;
  end;
end;

procedure TGLSkinEditorForm.ScaleYEditChange(Sender: TObject);
var
  res: Single;
begin
  if Assigned(SelectedElement) then
  begin
    res := StrToFloatDef(ScaleYEdit.Text, 0);
    if res > 0 then
    begin
      SelectedElement.Scale.Y := Res;
      GLPanel1.ReBuildGui := True;
      GLPanel1.GUIRedraw := True;
      Render;
    end;
  end;
end;

procedure TGLSkinEditorForm.LeftEditChange(Sender: TObject);
var
  res: Single;
begin
  if Assigned(SelectedElement) then
  begin
    GLPanel1.BlockRender;
    try
      res := StrToFloatDef(LeftEdit.Text, -1);
      if res >= 0 then
      begin
        SelectedElement.TopLeft.X := Res;
      end;
    finally
      GLPanel1.UnBlockRender;
    end;
    GLPanel1.ReBuildGui := True;
    GLPanel1.GUIRedraw := True;
    Render;
  end;
end;

procedure TGLSkinEditorForm.TopEditChange(Sender: TObject);
var
  res: Single;
begin
  if Assigned(SelectedElement) then
  begin
    GLPanel1.BlockRender;
    try
      res := StrToFloatDef(TopEdit.Text, -1);
      if res >= 0 then
      begin
        SelectedElement.TopLeft.Y := Res;
      end;
    finally
      GLPanel1.UnBlockRender;
    end;
    GLPanel1.ReBuildGui := True;
    GLPanel1.GUIRedraw := True;
    Render;
  end;
end;

procedure TGLSkinEditorForm.RightEditChange(Sender: TObject);
var
  res: Single;
begin
  if Assigned(SelectedElement) then
  begin
    GLPanel1.BlockRender;
    try
      res := StrToFloatDef(RightEdit.Text, -1);
      if res >= 0 then
      begin
        SelectedElement.BottomRight.X := Res;
      end;
    finally
      GLPanel1.UnBlockRender;
    end;
    GLPanel1.ReBuildGui := True;
    GLPanel1.GUIRedraw := True;
    Render;
  end;
end;

procedure TGLSkinEditorForm.BottomEditChange(Sender: TObject);
var
  res: Single;
begin
  if Assigned(SelectedElement) then
  begin
    GLPanel1.BlockRender;
    try
      res := StrToFloatDef(BottomEdit.Text, -1);
      if res >= 0 then
      begin
        SelectedElement.BottomRight.Y := Res;
      end;
    finally
      GLPanel1.UnBlockRender;
    end;
    GLPanel1.ReBuildGui := True;
    GLPanel1.GUIRedraw := True;
    Render;
  end;
end;

procedure TGLSkinEditorForm.DrawImageFocusRect(ARect: TRect);
begin
  imgFull.Canvas.Brush.Style := bsClear;
  imgFull.Canvas.Pen.Color := clRed;
  imgFull.Canvas.Pen.Style := psSolid;
  imgFull.Canvas.Pen.Mode := pmXor;

  imgFull.Canvas.Rectangle(ARect);
end;

procedure TGLSkinEditorForm.EditKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key > #32) and not CharInSet(Key, ['0'..'9',
    FormatSettings.DecimalSeparator]) then
    Key := #0;
end;

procedure TGLSkinEditorForm.AlignZoomPanel;
begin
  imgFull.Left := 0;
  imgFull.Top := 0;
  imgFull.Width := panZoomImage.Width - sbarVertical.Width - 1;
  imgFull.Height := panZoomImage.Height - sbarHorizontal.Height - 1;
  imgFull.Picture.Bitmap.Width := imgFull.Width;
  imgFull.Picture.Bitmap.Height := imgFull.Height;

  sbarVertical.Left := imgFull.Width;
  sbarVertical.Top := 0;
  sbarVertical.Height := imgFull.Height;

  sbarHorizontal.Left := 0;
  sbarHorizontal.Top := imgFull.Height;
  sbarHorizontal.Width := imgFull.Width;
  Render;
end;

procedure TGLSkinEditorForm.UpdateRegionEdits;
begin
  if (lbElements.ItemIndex >= 0) and (lbElements.ItemIndex <
    lbElements.Items.Count) then
  begin
    SelectedElement := TheGuiComponent.Items[lbElements.ItemIndex];
    ComboBox1.ItemIndex := Integer(SelectedElement.Align);
    ScaleXEdit.Text := FloatToStr(SelectedElement.Scale.X);
    ScaleYEdit.Text := FloatToStr(SelectedElement.Scale.Y);
    LeftEdit.Text := FloatToStr(SelectedElement.TopLeft.X);
    TopEdit.Text := FloatToStr(SelectedElement.TopLeft.Y);
    RightEdit.Text := FloatToStr(SelectedElement.BottomRight.X);
    BottomEdit.Text := FloatToStr(SelectedElement.BottomRight.Y);

    SetEditState(Self, True);
    Render;
  end
  else
  begin
    SelectedElement := nil;
    SetEditState(Self, False);
  end;
end;

procedure TGLSkinEditorForm.SetEditState(Parent: TControl; Enabled: Boolean);
var
  i: Integer;
begin
  for i := 0 to Parent.ComponentCount - 1 do
  begin
    if Parent.Components[i].Tag <> 1 then
      continue;

    if Parent.Components[i] is TLabel then
    begin
      (Parent.Components[i] as TLabel).Enabled := Enabled;
    end
    else if Parent.Components[i] is TEdit then
    begin
      (Parent.Components[i] as TEdit).Enabled := Enabled;
      if Enabled then
      begin
        (Parent.Components[i] as TEdit).Color := clWindow;
      end
      else
      begin
        (Parent.Components[i] as TEdit).Text := '';
        (Parent.Components[i] as TEdit).Color := clBtnFace;
      end;
    end
    else if Parent.Components[i] is TComboBox then
    begin
      (Parent.Components[i] as TComboBox).Enabled := Enabled;
      if Enabled then
      begin
        (Parent.Components[i] as TComboBox).Color := clWindow;
      end
      else
      begin
        (Parent.Components[i] as TComboBox).ItemIndex := -1;
        (Parent.Components[i] as TComboBox).Color := clBtnFace;
      end;
    end
    else if Parent.Components[i] is TButton then
    begin
      (Parent.Components[i] as TButton).Enabled := Enabled;
    end;
  end;
end;

procedure TGLSkinEditorForm.FormResize(Sender: TObject);
begin
  AlignZoomPanel;
end;

procedure TGLSkinEditorForm.MenuItemClick(Sender: TObject);
begin
  AddElement((Sender as TMenuItem).Tag);
end;

procedure TGLSkinEditorForm.mnuAddAllClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to 8 do
  begin
    AddElement(i);
  end;
end;

procedure TGLSkinEditorForm.AddElement(Index: Integer);
var
  S: string;
  Count: Integer;
  NewElement: TGLGuiElement;
begin
  Count := 1;
  repeat
    S := ComboBox1.Items[Index] + IntToStr(Count);
    inc(Count);
  until lbElements.Items.IndexOf(S) = -1;

  NewElement := TheGuiComponent.Add as TGLGuiElement;
  NewElement.Name := S;
  NewElement.Align := TGUIAlignments(Index);
  NewElement.BottomRight.SetPoint2D(0, 0);
  NewElement.TopLeft.SetPoint2D(0, 0);
  lbElements.ItemIndex := lbElements.Items.Add(S);
  UpdateRegionEdits;
end;

procedure TGLSkinEditorForm.mnuAllTopClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to 2 do
  begin
    AddElement(i);
  end;
end;

procedure TGLSkinEditorForm.mnuAllMiddleClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 3 to 5 do
  begin
    AddElement(i);
  end;
end;

procedure TGLSkinEditorForm.mnuAllBottomClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 6 to 8 do
  begin
    AddElement(i);
  end;
end;

procedure TGLSkinEditorForm.imgPreviewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  PreviewMousePoint := Point(X, Y);
end;

procedure TGLSkinEditorForm.imgPreviewMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if PtInRect(VisibleRect, Point(X, Y)) then
  begin
    imgPreview.Cursor := crHandPoint;
  end
  else
  begin
    imgPreview.Cursor := crDefault;
  end;

  if ssLeft in Shift then
  begin
    if imgPreview.Cursor <> crDefault then
    begin
      sbarVertical.Position := sbarVertical.Position + Round((Y -
        PreviewMousePoint.Y) * PreviewWidth / imgPreview.Width);
      sbarHorizontal.Position := sbarHorizontal.Position + Round((X -
        PreviewMousePoint.X) * PreviewHeight / imgPreview.Height);
    end;

    PreviewMousePoint := Point(X, Y);

    VisibleRect := Rect(
      Round(sbarHorizontal.Position / PreviewWidth * imgPreview.Width),
      Round(sbarVertical.Position / PreviewHeight * imgPreview.Height),
      Round((sbarHorizontal.Position + (imgFull.Width - 1) / Zoom) / PreviewWidth
        * imgPreview.Width),
      Round((sbarVertical.Position + (imgFull.Height - 1) / Zoom) / PreviewHeight
        * imgPreview.Height)
      );
  end;
end;

procedure TGLSkinEditorForm.DrawCrossair(Point: TPoint);
begin
  if Point.X < 0 then
    Exit;

  imgFull.Canvas.Pen.Color := clWhite;
  imgFull.Canvas.Pen.Style := psDot;
  imgFull.Canvas.Pen.Mode := pmXor;

  imgFull.Canvas.MoveTo(Point.X, 0);
  imgFull.Canvas.LineTo(Point.X, imgFull.Height);

  imgFull.Canvas.MoveTo(0, Point.Y);
  imgFull.Canvas.LineTo(imgFull.Width, Point.Y);
end;

procedure TGLSkinEditorForm.ImageWndProc(var Message: TMessage);
begin
  if (Message.Msg = CM_MOUSELEAVE) then
  begin
    DrawCrossair(FullMousePoint);
    FullMousePoint := Point(-1, -1);
  end;

  FOriginalWndProc(Message);
end;

end.

