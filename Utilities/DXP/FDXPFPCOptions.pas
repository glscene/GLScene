unit FDXPFPCOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls;

type
  TDXPFPCOptions = class(TForm)
    Panel1: TPanel;
    BUOk: TButton;
    BUCancel: TButton;
    PageControl: TPageControl;
    TSCompiler: TTabSheet;
    TSCompilerMessages: TTabSheet;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    CheckBox1: TCheckBox;
    GroupBox4: TGroupBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    Panel2: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    Label1: TLabel;
    Panel3: TPanel;
    Label2: TLabel;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    CheckBox6: TCheckBox;
    Panel4: TPanel;
    Label3: TLabel;
    RadioButton6: TRadioButton;
    RadioButton7: TRadioButton;
    RadioButton8: TRadioButton;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    CheckBox10: TCheckBox;
    CheckBox11: TCheckBox;
    CheckBox12: TCheckBox;
    GroupBox5: TGroupBox;
    CBShowWarnings: TCheckBox;
    CBShowNotes: TCheckBox;
    CBShowHints: TCheckBox;
    TabSheet1: TTabSheet;
    GroupBox6: TGroupBox;
    RadioButton9: TRadioButton;
    CheckBox16: TCheckBox;
    RadioButton10: TRadioButton;
    RadioButton11: TRadioButton;
  private
    { Déclarations privées }
    procedure LoadOptions(options : TStrings);
    procedure SaveOptions(options : TStrings);
  public
    { Déclarations publiques }
    function Execute(options : TStrings) : Boolean;
  end;

implementation

{$R *.dfm}

// Execute
//
function TDXPFPCOptions.Execute(options : TStrings) : Boolean;
begin
   PageControl.ActivePage:=TSCompiler;
   LoadOptions(options);
   Result:=(ShowModal=mrOk);
   if Result then
      SaveOptions(options);
end;

// LoadOptions
//
procedure TDXPFPCOptions.LoadOptions(options : TStrings);
var
   i : Integer;
   comp : TComponent;
   msg : String;
begin
   for i:=0 to ComponentCount-1 do begin
      comp:=Components[i];
      if comp is TRadioButton then
         TRadioButton(comp).Checked:=(options.IndexOf(TRadioButton(comp).Hint)>=0)
      else if comp is TCheckBox then
         TCheckBox(comp).Checked:=(options.IndexOf(TCheckBox(comp).Hint)>=0);
   end;
   msg:='';
   for i:=0 to options.Count-1 do if Copy(options[i], 1, 2)='-v' then begin
      msg:=Copy(options[i], 3, MaxInt);
      Break;
   end;
   CBShowWarnings.Checked:=(Pos('w', msg)>0);
   CBShowNotes.Checked:=(Pos('n', msg)>0);
   CBShowHints.Checked:=(Pos('h', msg)>0);
end;

// SaveOptions
//
procedure TDXPFPCOptions.SaveOptions(options : TStrings);
var
   i : Integer;
   comp : TComponent;
   msg : String;
begin
   options.Clear;
   for i:=0 to ComponentCount-1 do begin
      comp:=Components[i];
      if comp is TRadioButton then begin
         if TRadioButton(comp).Checked then
            options.Add(TRadioButton(comp).Hint);
      end else if comp is TCheckBox then begin
         if TCheckBox(comp).Checked then
            options.Add(TCheckBox(comp).Hint);
      end;
   end;
   msg:='-ve';
   if CBShowWarnings.Checked then msg:=msg+'w';
   if CBShowNotes.Checked then msg:=msg+'n';
   if CBShowHints.Checked then msg:=msg+'h';
   options.Add(msg);
end;

end.
