//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLFileSMD"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent * Owner):TForm(Owner)
{
  const float cSpacing = 1, cRadius = 0.3;
  const int cNb = 1;

  int x, y, z;
  TGLSphere *sphere;


  String MediaPath = ExtractFilePath(ParamStr(0));
  String SubStr = "Samples";
  int I = MediaPath.Pos(SubStr);
  if (I != 0) {
	MediaPath.Delete(I+8,MediaPath.Length()-I);
	SetCurrentDir(MediaPath+"Media\\");
  }

  // Dynamically construct an array of spheres, and make them shadow casters
  // Note that as the spheres are children of the shadowvolume component,
  // they are thus also shadow receivers. If they were created as child of
  // another object (not under the shadow volume), they would not receive
  // shadows (which can sometimes be interesting).
  for(x = -cNb; x <= cNb; x++)
	for(y = -cNb; y <= cNb; y++)
	  for(z = -cNb; z <= cNb; z++)
		if((x && y && z) != 0)
		{
		  sphere = (TGLSphere *) DCSpheres->AddNewChild(__classid(TGLSphere));
		  sphere->Position->SetPoint(x * cSpacing, y * cSpacing, z * cSpacing);
		  sphere->Radius = cRadius;
		  GLShadowVolume->Occluders->AddCaster(sphere, 0, scmParentVisible);
        }
  DCSpheres->MoveTo(GLShadowVolume);
  GLFreeForm->LoadFromFile("trinityrage.smd");
  GLFreeForm->BuildSilhouetteConnectivityData();
  GLShadowVolume->Occluders->AddCaster(GLFreeForm,0,scmRecursivelyVisible);
}

//---------------------------------------------------------------------------

void __fastcall TForm1::GLCadencer1Progress(TObject * Sender,
                                            const double deltaTime,
                                            const double newTime)
{
  DCLight2->TurnAngle = newTime * 45;
  DCLight3->RollAngle = newTime * 50;
  GLSceneViewer->Invalidate();
}

//---------------------------------------------------------------------------
void __fastcall TForm1::CBShowVolumesClick(TObject * Sender)
{
  if(CBShowVolumes->Checked)
    GLShadowVolume->Options = GLShadowVolume->Options << svoShowVolumes;
  else
    GLShadowVolume->Options = GLShadowVolume->Options >> svoShowVolumes;
}

//---------------------------------------------------------------------------
void __fastcall TForm1::RBZFailClick(TObject * Sender)
{
// this event handles all the radio buttons
  if(RBDarkening->Checked)
    GLShadowVolume->Mode = svmDarkening;
  else if(RBNoShadows->Checked)
    GLShadowVolume->Mode = svmOff;
  else
  {
    GLShadowVolume->Mode = svmAccurate;
    if(RBZFail->Checked)
      GLShadowVolume->Capping = svcAlways;
    else
      GLShadowVolume->Capping = svcNever;
  }
}

//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewerMouseDown(TObject * Sender,
                                               TMouseButton Button,
                                               TShiftState Shift, int X, int Y)
{
  mx = X;
  my = Y;
}

//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewerMouseMove(TObject * Sender,
                                               TShiftState Shift, int X, int Y)
{
  if(Shift.Contains(ssLeft))
  {
    GLCamera->MoveAroundTarget((float)(my - Y) * 0.5, (float)(mx - X) * 0.5);
    GLCadencer1->Progress();
  }
  else if(Shift.Contains(ssRight))
  {
    DCLight1Turn->Turn((float)(mx - X) * 0.5);
    DCLight1Pitch->Pitch((float)(my - Y) * 0.5);
    GLLightSource1->TransformationChanged();
    GLCadencer1->Progress();
  }
  mx = X;
  my = Y;
}

//---------------------------------------------------------------------------
void __fastcall TForm1::Timer1Timer(TObject * Sender)
{
  LabelFPS->Caption = GLSceneViewer->FramesPerSecondText(1);
  GLSceneViewer->ResetPerformanceMonitor();
}

//---------------------------------------------------------------------------
void __fastcall TForm1::FormResize(TObject * Sender)
{
  GLCamera->SceneScale = GLSceneViewer->Width / 450;
}

//---------------------------------------------------------------------------
void __fastcall TForm1::CBMainLightClick(TObject * Sender)
{
  GLLightSource1->Shining = CBMainLight->Checked;
}

//---------------------------------------------------------------------------
void __fastcall TForm1::CBBlueLightClick(TObject * Sender)
{
  GLLightSource2->Shining = CBBlueLight->Checked;
}

//---------------------------------------------------------------------------
void __fastcall TForm1::CBRedLightClick(TObject * Sender)
{
  GLLightSource3->Shining = CBRedLight->Checked;
}

//---------------------------------------------------------------------------
void __fastcall TForm1::ScrollBar_ShadowResolutionChange(TObject * Sender)
{
  GLSphere_Shadow->Stacks = ScrollBar_ShadowResolution->Position;
  GLSphere_Shadow->Slices = ScrollBar_ShadowResolution->Position;
  GLShadowVolume->FlushSilhouetteCache();
}

//---------------------------------------------------------------------------
void __fastcall TForm1::Button_GenerateSilhouetteClick(TObject * Sender)
{
  TGLSilhouetteParameters silhouetteParameters;
  TGLSilhouette *Silhouette;
  int i;
  TGLSceneObject *Target = GLSphere4;

  silhouetteParameters.CappingRequired = false;
  SetVector(silhouetteParameters.SeenFrom,
            GLLines1->AbsoluteToLocal(GLCamera->AbsolutePosition));

  silhouetteParameters.Style = ssOmni;

  Silhouette = Target->GenerateSilhouette(silhouetteParameters);

  GLLines1->Nodes->Clear();

  for(i = 0; i <= Silhouette->Indices->Count - 1; i++)
    GLLines1->Nodes->AddNode(GLLines1->
                             AbsoluteToLocal(Target->
                                             LocalToAbsolute(Silhouette->
                                                             Vertices->
                                                             Items[Silhouette->
                                                                   Indices->
                                                                   Items[i]])));

  delete Silhouette;
}

//---------------------------------------------------------------------------

