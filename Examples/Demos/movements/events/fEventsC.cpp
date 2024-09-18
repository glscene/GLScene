//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fEventsC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"

#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLS.TimeEventsMgr"
#pragma link "GLS.SceneViewer"
#pragma resource "*.dfm"
TFormEvents *FormEvents;
//---------------------------------------------------------------------------
__fastcall TFormEvents::TFormEvents(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFormEvents::Timer1Timer(TObject *Sender)
{
	StatusBar->Panels->Items[0]->Text = "Time: ";
	StatusBar->Panels->Items[1]->Text = FloatToStrF(GLCadencer1->CurrentTime,ffFixed, 10, 2);
	GLSceneViewer1->ResetPerformanceMonitor();
}
//---------------------------------------------------------------------------
void __fastcall TFormEvents::GLTimeEventsMGR1Events0Event(TTimeEvent *event)
{
   Cube1->RollAngle = (float)event->ElapsedTime*180/3;
}
//---------------------------------------------------------------------------
void __fastcall TFormEvents::GLTimeEventsMGR1Events1Event(TTimeEvent *event)
{
   Cube2->RollAngle = (float)event->TickCount/499*180;
}
//---------------------------------------------------------------------------
void __fastcall TFormEvents::GLTimeEventsMGR1Events2Event(TTimeEvent *event)
{
  Cube3->RollAngle = 90;
}
//---------------------------------------------------------------------------
void __fastcall TFormEvents::GLTimeEventsMGR1Events3Event(TTimeEvent *event)
{
 Cube1->RollAngle = (float)event->TickCount/4*90;
}
//---------------------------------------------------------------------------
void __fastcall TFormEvents::GLTimeEventsMGR1Events4Event(TTimeEvent *event)
{
 Cube2->RollAngle = (float)event->TickCount/20*90;
}
//---------------------------------------------------------------------------
void __fastcall TFormEvents::GLTimeEventsMGR1Events5Event(TTimeEvent *event)
{
  Cube3->RollAngle = (float)event->TickCount/200*90;
}
//---------------------------------------------------------------------------
