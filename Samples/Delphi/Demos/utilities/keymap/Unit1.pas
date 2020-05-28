unit Unit1;

interface

uses
  System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Buttons,
  
  GLKeyboard;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    PAUp: TPanel;
    Label1: TLabel;
    PALeft: TPanel;
    PARight: TPanel;
    Label2: TLabel;
    procedure Timer1Timer(Sender: TObject);
    procedure PAUpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
     
  public
     
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
   Label1.Caption:='Hit one/any of the keys below to light up the panel...';
end;

procedure TForm1.Timer1Timer(Sender: TObject);

   procedure CheckPanel(panel : TPanel);
   begin
      // check if key associated to current panel's caption is down
      if IsKeyDown(KeyNameToVirtualKeyCode(panel.Caption)) then
         panel.Color:=clRed         // down, set panel to red
      else panel.Color:=clBtnFace;  // up, set panel to default color
   end;

begin
   // check all keys
   CheckPanel(PALeft);
   CheckPanel(PAUp);
   CheckPanel(PARight);
end;

procedure TForm1.PAUpClick(Sender: TObject);
var
   keyCode : Integer;
begin
   Label1.Caption:='Type key to map...';
   // wait for a key to be pressed
   repeat
      Application.ProcessMessages;  // let other messages happen
      Sleep(1);                     // relinquish time for other processes
      keyCode:=KeyPressed;
   until keyCode>=0;
   // retrieve keyname and adjust panel caption
   TPanel(Sender).Caption:=VirtualKeyCodeToKeyName(keyCode);
   TPanel(Sender).Tag:=keyCode;
   // restore default label1.caption
   FormCreate(Self);
end;

end.
