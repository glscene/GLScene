{: A "pawn"-like revolution solid.

   Allows playing with a few settings for a revolution solid and see the visual
   (and triangle count) impact they have.
}
program pawn;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
