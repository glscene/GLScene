(* Setting up a simple scripted scene.

   This demo shows how to manipulate GLScene objects though
   a couple of basic scripts. The TGLDWS2ActiveBehaviour is
   a behaviour style component that wraps a DelphiWebScriptII
   program and script. Once compiled and executed it will
   remain active until it is deactivated, destroyed or
   recompiled.

   There are a couple of procedures that the active script will
   recognize and execute while active. The OnBeginProgram and
   OnProgress procedures. If these procedures are found in the
   compiled program it will call them from the behaviour. The
   OnBeginProgram is called when the program starts, just after
   compiling. The OnProgress procedure is called when the
   behaviour progresses. The OnBeginProgram event is used here
   to grab the instance of the object being scripted and the
   OnProgress is used to manipulate the GLScene object.

   The DWS2Program property is the compiled program, this can
   be used to call on internal variables or functions from
   Delphi. This can be used to create other custom events.

   InvalidateScript is called after the script text is altered
   to alert the behaviour that the program needs to be
   recompiled. OnBeginProgram will be called again once the
   program is compiled and restarted. The Active property can
   be used to halt and start DWS2Program's execution.
*)
program ScriptBasics;

uses
  Vcl.Forms,
  ScriptBasicsFm in 'ScriptBasicsFm.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
