program gui;

{$APPTYPE gui}

uses Forms, Controls, StdCtrls {$IFDEF FPC}, Buttons, Interfaces{$ENDIF};

var
   frm : TForm;
   button : TButton;
begin
   Application.Initialize;

   frm:=TForm.Create(nil);
   frm.Caption:='Hello World!';
   frm.Position:=poScreenCenter;
   frm.Width:=200;
   frm.Height:=100;

   button:=TButton.Create(frm);
   button.Left:=60;
   button.Top:=20;
   button.Caption:='Ok!';
   button.ModalResult:=mrOk;
   button.Parent:=frm;

   frm.ShowModal;

   frm.Free;
end.
