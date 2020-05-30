{: Demo for color modulation and fade-in/out for bitmap fonts.

   The bitmap Font used in this demo is obtained from
   http://www.algonet.se/~guld1/freefont.htm
   and was modified by me to have a red background so that I can have the
   character itself in black.
   Nelson Chu
   cpegnel@ust.hk
}
program Fontcolor;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
