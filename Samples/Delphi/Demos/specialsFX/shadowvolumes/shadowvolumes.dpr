{: Shadow volumes demo.

   This demo is under construction...

   246 (what?)

   <b>History : </b><font size=-1><ul>
      <li>29/11/03 - MF - Items now self shadow, and a new cylinder was added.
        Both changes are intended to demonstrate the problems of darkening.
      <li>?/?/03 - EG - Creation (based on code from Mattias Fagerlund)
   </ul></font>
}
program shadowvolumes;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
