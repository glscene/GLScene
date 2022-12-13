//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>
#include <system.hpp>
#include <sysutils.hpp>

#pragma hdrstop

#include "fConsoleC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Behaviours"
#pragma link "GLS.BitmapFont"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"

#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLS.SimpleNavigation"
#pragma link "GLS.SceneViewer"
#pragma link "GLS.WindowsFont"
#pragma resource "*.dfm"
TForm1 *Form1;

//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::OnHelloCommand(const TGLConsoleCommand *ConsoleCommand,
		  const TGLCustomConsole *Console, TGLUserInputCommand &Command)
{
 /// Console->AddLine("Hi, dude!");    // yet not works!
}

//---------------------------------------------------------------------------
void __fastcall TForm1::OnCommand(const TGLConsoleCommand *ConsoleCommand,
		  const TGLCustomConsole *Console, TGLUserInputCommand &Command)
{
  int i;
  String Str;

  if (Command.CommandCount == 0)
	exit;
  Command.Strings[0] = LowerCase(Command.Strings[0]);
  if (Command.Strings[0] == "echo")
  {
	for (i = 1; i< (Command.CommandCount - 1); i++)
	  Str = Str + Command.Strings[i];
///	Console->AddLine("You just typed: " + Str);
	Command.UnknownCommand = false;
  }
  else
  if (Command.Strings[0] == "exit")
  {
	Application->Terminate();
	Command.UnknownCommand = false; // user won't see it anyway, but you should
									 // get used to puting this line in every
									 // command you recognize :)
  }
///  if (Command.UnknownCommand)
///	GLConsole1->AddLine(" - Current supported external commands are: \r\
///					 echo and exit!");
}

//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
//  GLConsole1 = (TGLConsole*)(Scene->Objects->AddNewChild(__classid(TGLConsole)));

  GLConsole1->Visible = false;
  GLConsole1->SceneViewer = Viewer;
  GLConsole1->Font = Font1;

  //optional stuff:
  TFileName Path = GetCurrentAssetPath();
  GLConsole1->HudSprite->Material->Texture->Image->LoadFromFile("GLScene.bmp");
  GLConsole1->AddLine("Console started");
  GLConsole1->HUDSpriteColor = clWhite;
  GLConsole1->FontColor = clBlue;

  //two ways of processing commands:
	 //1) manual
///  GLConsole1->OnCommandIssued = OnCommand;
	 //2)using built-in objects (prefered)
	GLConsole1->Commands->Add()->CommandName = "hello";
	GLConsole1->Commands->Add()->ShortHelp = "Says hi to you too";
	GLConsole1->Commands->Add()->LongHelp->Add("Well, the console really does say - Hi, dude - to you, because");
	GLConsole1->Commands->Add()->LongHelp->Add("it is roude not to greet someone, when he says - hello - to you ;)");
///	GLConsole1->Commands->Add()->OnCommand = OnHelloCommand;

  //register additional commands to enable auto-completion function
  GLConsole1->AdditionalCommands->Add("echo");
  GLConsole1->AdditionalCommands->Add("exit");
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
		  const double newTime)
{
  Viewer->Invalidate();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormKeyPress(TObject *Sender, System::WideChar &Key)
{
  GLConsole1->ProcessKeyPress(Key);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormKeyDown(TObject *Sender, WORD &Key, TShiftState Shift)

{
  GLConsole1->ProcessKeyDown(Key);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::ViewerMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
		  int X, int Y)
{
  GLConsole1->Visible = !GLConsole1->Visible;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormResize(TObject *Sender)
{
  GLConsole1->RefreshHudSize();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CheckBox1Click(TObject *Sender)
{
  if (CheckBox1->Checked)
	GLConsole1->Options = GLConsole1->Options << coAutoCompleteCommandsOnKeyPress;
  else
	GLConsole1->Options = GLConsole1->Options >> coAutoCompleteCommandsOnKeyPress;
  Viewer->SetFocus();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CheckBox2Click(TObject *Sender)
{
  if (CheckBox1->Checked)
	GLConsole1->Options = GLConsole1->Options << coAutoCompleteCommandsOnEnter;
  else
	GLConsole1->Options = GLConsole1->Options >> coAutoCompleteCommandsOnEnter;
  Viewer->SetFocus();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CheckBox3Click(TObject *Sender)
{
  if (CheckBox1->Checked)
	GLConsole1->Options = GLConsole1->Options << coShowConsoleHelpIfUnknownCommand;
  else
	GLConsole1->Options = GLConsole1->Options >> coShowConsoleHelpIfUnknownCommand;
  Viewer->SetFocus();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button1Click(TObject *Sender)
{
  GLConsole1->TypedCommands->SaveToFile("saved_typed_commands.ini");
  Viewer->SetFocus();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button2Click(TObject *Sender)
{
  GLConsole1->ColsoleLog->SaveToFile("saved_console_output.ini");
  Viewer->SetFocus();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button6Click(TObject *Sender)
{
  GLConsole1->TypedCommands->LoadFromFile("saved_typed_commands.ini");
  Viewer->SetFocus();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button7Click(TObject *Sender)
{
  GLConsole1->ColsoleLog->LoadFromFile("saved_console_output.ini");
  GLConsole1->RefreshHudSize();
  Viewer->SetFocus();
}
//---------------------------------------------------------------------------
