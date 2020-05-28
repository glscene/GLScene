//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>
#include <system.hpp>
#include <sysutils.hpp>

#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLBehaviours"
#pragma link "GLBitmapFont"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLSimpleNavigation"
#pragma link "GLWin32Viewer"
#pragma link "GLWindowsFont"
#pragma resource "*.dfm"
TForm1 *Form1;
TGLConsole *Console;

//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::OnHelloCommand(const TGLConsoleCommand *ConsoleCommand,
		  const TGLCustomConsole *Console, TGLUserInputCommand &Command)
{
///  Console->AddLine("Hi, dude!");    // yet not works!
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
///	Console->AddLine(" - Current supported external commands are: \r\
///					 echo and exit!");
}

//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  Console = (TGLConsole *)(Scene->Objects->AddNewChild(__classid(TGLConsole)));

  Console->Visible = false;
  Console->SceneViewer = Viewer;
  Console->Font = Font1;

  //optional stuff:
  SetGLSceneMediaDir();
  Console->HudSprite->Material->Texture->Image->LoadFromFile("GLScene.bmp");
  Console->AddLine("Console started");
  Console->HUDSpriteColor = clWhite;
  Console->FontColor = clBlue;

  //two ways of processing commands:
	 //1) manual
///  Console->OnCommandIssued = OnCommand;
	 //2)using built-in objects (prefered)
	Console->Commands->Add()->CommandName = "hello";
	Console->Commands->Add()->ShortHelp = "Says hi to you too";
	Console->Commands->Add()->LongHelp->Add("Well, the console really does say - Hi, dude - to you, because");
	Console->Commands->Add()->LongHelp->Add("it is roude not to greet someone, when he says - hello - to you ;)");
///	Console->Commands->Add()->OnCommand = OnHelloCommand;

  //register additional commands to enable auto-completion function
  Console->AdditionalCommands->Add("echo");
  Console->AdditionalCommands->Add("exit");
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
  Console->ProcessKeyPress(Key);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormKeyDown(TObject *Sender, WORD &Key, TShiftState Shift)

{
  Console->ProcessKeyDown(Key);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::ViewerMouseDown(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y)
{
  Console->Visible = !Console->Visible;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormResize(TObject *Sender)
{
  Console->RefreshHudSize();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CheckBox1Click(TObject *Sender)
{
  if (CheckBox1->Checked)
	Console->Options = Console->Options << coAutoCompleteCommandsOnKeyPress;
  else
	Console->Options = Console->Options >> coAutoCompleteCommandsOnKeyPress;
  Viewer->SetFocus();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CheckBox2Click(TObject *Sender)
{
  if (CheckBox1->Checked)
	Console->Options = Console->Options << coAutoCompleteCommandsOnEnter;
  else
	Console->Options = Console->Options >> coAutoCompleteCommandsOnEnter;
  Viewer->SetFocus();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CheckBox3Click(TObject *Sender)
{
  if (CheckBox1->Checked)
	Console->Options = Console->Options << coShowConsoleHelpIfUnknownCommand;
  else
	Console->Options = Console->Options >> coShowConsoleHelpIfUnknownCommand;
  Viewer->SetFocus();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button1Click(TObject *Sender)
{
  Console->TypedCommands->SaveToFile("saved_typed_commands.ini");
  Viewer->SetFocus();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button2Click(TObject *Sender)
{
  Console->ColsoleLog->SaveToFile("saved_console_output.ini");
  Viewer->SetFocus();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button6Click(TObject *Sender)
{
  Console->TypedCommands->LoadFromFile("saved_typed_commands.ini");
  Viewer->SetFocus();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button7Click(TObject *Sender)
{
  Console->ColsoleLog->LoadFromFile("saved_console_output.ini");
  Console->RefreshHudSize();
  Viewer->SetFocus();
}
//---------------------------------------------------------------------------
