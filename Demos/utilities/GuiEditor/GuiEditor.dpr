{: This is more than a sample which demonstrates the use of the Gui system,
   its an editor for combining several GUI-Component into one layout, using
   the same texture for them all.

   The default layout image, is a modification based on Jan Horn's image in his
   windows (opengl) release...  

   Be aware that for HUD purposes mip mapping should allways be disabled as the
   result might become blurred by the mipmap... Reason unknown.

	<b>History : </b><font size=-1><ul>
      <li>17/01/07 - DaStr - Fixed calls to GUIComponentDialog function (thanks Andreas)
      <li>19/09/02 - JAJ - Submitted to GLScene. Open/Save/Import + Edit/Preview.
	</ul></font>
}
program GuiEditor;

uses
  Forms,
  MainFormUnit in 'MainFormUnit.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
