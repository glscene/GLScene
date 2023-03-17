//
// The graphics platform GLArena https://github.com/glscene
//
unit FMxPlugInManagerEditor;

(* Need a short description of what it does here *)

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Layouts,
  FMX.ListBox,
  FMX.Objects,
  FMX.Memo,
  FMX.ScrollBox,
  FMX.Controls.Presentation,

  GLX.PlugInInterface,
  GLX.PlugInManager, FMX.Memo.Types;

type
  TgxPlugInManagerEditor = class(TForm)
    OpenDialog: TOpenDialog;
    ListBox: TListBox;
    Label1: TLabel;
    GroupBox1: TGroupBox;
    DescriptionMemo: TMemo;
    Label2: TLabel;
    Label3: TLabel;
    DateLabel: TLabel;
    SizeLabel: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    ServiceBox: TComboBox;
    NameBox: TComboBox;
    ToolBar1: TToolBar;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    procedure OKButtonClick(Sender: TObject);
    procedure LoadButtonClick(Sender: TObject);
    procedure ListBoxClick(Sender: TObject);
    procedure UnloadButtonClick(Sender: TObject);
    procedure ServiceBoxChange(Sender: TObject);
  private
    FManager: TgxPlugInManager;
  public
    class procedure EditPlugIns(AManager: TgxPlugInManager);
  end;

var
  PlugInManagerEditor: TgxPlugInManagerEditor;

//=======================================================================
implementation
//=======================================================================

{$R *.fmx}

procedure TgxPlugInManagerEditor.OKButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TgxPlugInManagerEditor.LoadButtonClick(Sender: TObject);
var
  I, Index: Integer;
begin
  with OpenDialog do
    if Execute then
      for I := 0 to Files.Count - 1 do
      begin
        Index := FManager.AddPlugIn(Files[I]);
        if Index > -1 then
          if Index >= ListBox.Items.Count then
          begin
            FManager.PlugIns.Objects[Index];
            ListBox.Items.Add(FManager.PlugIns.Strings[I]);
          end
          else
        else
          MessageDlg(Format('Error while loading %s' + #13 +
            'not a valid plug-in', [Files[I]]),
            TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
      end;
end;

class procedure TgxPlugInManagerEditor.EditPlugIns(AManager: TgxPlugInManager);
begin
  // ensure only one instance
  if assigned(PlugInManagerEditor) then
    PlugInManagerEditor.Free;
  PlugInManagerEditor := TgxPlugInManagerEditor.Create(Application);
  with PlugInManagerEditor do
  begin
    ListBox.Items := AManager.PlugIns;
    FManager := AManager;
    ShowModal;
    Free;
  end;
  PlugInManagerEditor := nil;
end;

procedure TgxPlugInManagerEditor.ListBoxClick(Sender: TObject);
var
  Entry: Integer;
  Service: TPIServiceType;
  Services: TPIServices;

begin
  Entry := ListBox.ItemIndex;
  if Entry > -1 then
  begin
    SizeLabel.Text := Format('%n KB',
      [FManager.PlugIns[Entry].FileSize / 1000]);
    SizeLabel.Enabled := True;
    DateLabel.Text := DateToStr(FManager.PlugIns[Entry].FileDate);
    DateLabel.Enabled := True;
    DescriptionMemo.Lines.Text :=
      string(FManager.PlugIns[Entry].GetDescription);
    ServiceBox.Items.Clear;
    ServiceBox.Enabled := True;
    Services := FManager.PlugIns[Entry].GetServices;
    for Service := Low(TPIServiceType) to High(TPIServiceType) do
      if Service in Services then
        case Service of
          stRaw:
            begin
              Entry := ServiceBox.Items.Add('Raw');
              ServiceBox.Items.Objects[Entry] := Pointer(stRaw);
            end;
          stObject:
            begin
              Entry := ServiceBox.Items.Add('Object');
              ServiceBox.Items.Objects[Entry] := Pointer(stObject);
            end;
          stBitmap:
            begin
              Entry := ServiceBox.Items.Add('Bitmap');
              ServiceBox.Items.Objects[Entry] := Pointer(stBitmap);
            end;
          stTexture:
            begin
              Entry := ServiceBox.Items.Add('Texture');
              ServiceBox.Items.Objects[Entry] := Pointer(stTexture);
            end;
          stImport:
            begin
              Entry := ServiceBox.Items.Add('Import');
              ServiceBox.Items.Objects[Entry] := Pointer(stImport);
            end;
          stExport:
            begin
              Entry := ServiceBox.Items.Add('Export');
              ServiceBox.Items.Objects[Entry] := Pointer(stExport);
            end;
        end;
    ServiceBox.ItemIndex := 0;
    ServiceBox.OnChange(ServiceBox);
  end;
end;

procedure TgxPlugInManagerEditor.UnloadButtonClick(Sender: TObject);

var
  I: Integer;
begin
  for I := 0 to ListBox.Items.Count - 1 do
//    if ListBox.Selected then
    begin
      FManager.RemovePlugIn(I);
      ListBox.Items.Delete(I);
    end;
  DescriptionMemo.Lines.Clear;
  DateLabel.Text := '???';
  DateLabel.Enabled := False;
  SizeLabel.Text := '???';
  SizeLabel.Enabled := False;
  ServiceBox.ItemIndex := -1;
  ServiceBox.Enabled := False;
  NameBox.ItemIndex := -1;
  NameBox.Enabled := False;
end;

// ------------------------------------------------------------------------------

procedure NameCallback(Name: PAnsiChar); stdcall;

begin
  PlugInManagerEditor.NameBox.Items.Add(string(StrPas(Name)));
end;

// ------------------------------------------------------------------------------

procedure TgxPlugInManagerEditor.ServiceBoxChange(Sender: TObject);
begin
  NameBox.Items.Clear;
  with ServiceBox, Items do
    FManager.PlugIns[ListBox.ItemIndex].EnumResourceNames
      (TPIServiceType(Objects[ItemIndex]), NameCallback);
  NameBox.ItemIndex := 0;
  NameBox.Enabled := True;
end;

end.
