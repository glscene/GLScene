(* Demo for color modulation and fade-in/out for bitmap fonts.
   The bitmap Font used in this demo is obtained from
   http://www.algonet.se/~guld1/freefont.htm
   and was modified by me to have a red background so that I can have the
   character itself in black.
   Nelson Chu
   cpegnel@ust.hk
*)
program FontcolorD;

uses
  Forms,
  fFontColorD in 'fFontColorD.pas' {FormFontColor};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormFontColor, FormFontColor);
  Application.Run;
end.
