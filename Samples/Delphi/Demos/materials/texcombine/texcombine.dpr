{: TexCombineShader demo / mini-lab.

   This is an advanced demo, which showcases use and setup of extra texture
   units, along with texture combination possibilities.

   The texture combiner allows to declare how each texture unit should
   be used, and how each should be combined with the others. Basicly,
   a texture combiner "code" defines how each texture should be combined,
   knowing that the result of the last texture unit (the one with the higher
   index) defines the final output.<br>
   Note that if the code allows you to declare the combiners in any order,
   the hardware will evaluate them in their index order, and will only accept
   one combiner assignement for each texture unit.
}
program texcombine;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
