//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>

#pragma hdrstop

#include "fMatScriptC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"

#pragma link "GLS.Material"
#pragma link "GLS.MaterialScript"
#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLS.SceneViewer"

#pragma link "GLS.SimpleNavigation"
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
   PathToData = GetCurrentAssetPath(); // not use ExtractFilePath(Application->ExeName)
   SetCurrentDir(PathToData);     // ..glscene\assets
   GLMaterialLibrary1->TexturePaths = PathToData;
   GLMaterialScripter1->DebugMemo = Memo2;
   GLCube1->Material->MaterialLibrary = GLMaterialLibrary1;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::ButtonLoadScriptClick(TObject *Sender)
{
   OpenDialog1->InitialDir = PathToData  + "\\script";
   if (OpenDialog1->Execute())
   {
	  if (FileExists(OpenDialog1->FileName))
	  Memo1->Lines->LoadFromFile(OpenDialog1->FileName);
   }
   SetCurrentDir(PathToData);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::ButtonExecuteScriptClick(TObject *Sender)
{
   GLMaterialLibrary1->Materials->Clear();
   GLCube1->Material->MaterialLibrary = GLMaterialLibrary1;
   GLMaterialScripter1->Script = Memo1->Lines;
   GLMaterialScripter1->CompileScript();
   GLCube1->Material->LibMaterialName = "TestMat";
}
//---------------------------------------------------------------------------

