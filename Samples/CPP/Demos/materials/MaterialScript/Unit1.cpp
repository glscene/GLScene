//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>

#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLMaterial"
#pragma link "GLMaterialScript"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLWin32Viewer"

#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
   SetGLSceneMediaDir();
   GLMaterialScripter1->DebugMemo = Memo2;;
   GLCube1->Material->MaterialLibrary = GLMaterialLibrary1;
   SetCurrentDir(ExtractFilePath(Application->ExeName));
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Button1Click(TObject *Sender)
{
   OpenDialog1->InitialDir = ExtractFilePath(Application->ExeName);
   if (OpenDialog1->Execute())
   {
	  if (FileExists(OpenDialog1->FileName))
	  Memo1->Lines->LoadFromFile(OpenDialog1->FileName);
   }
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Button2Click(TObject *Sender)
{
   GLMaterialLibrary1->Materials->Clear();
   GLCube1->Material->MaterialLibrary = GLMaterialLibrary1;
   GLMaterialScripter1->Script = Memo1->Lines;
   GLMaterialScripter1->CompileScript();
   GLCube1->Material->LibMaterialName = "TestMat";
}
//---------------------------------------------------------------------------

