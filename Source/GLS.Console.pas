//
// The graphics rendering engine GLScene http://glscene.org
//

unit GLS.Console;

(*
   The console is a popdown window that appears on a game for text output/input.

   What is different compared to the original component?
     1) Can be aded to any object, not just the root one
     2) Has a *wide* range of built-in commands
     3) TGLConsoleCommand.UnknownCommand added
         it is set to True, if no internal command recognized
     4) Internal console help added
     5) By default does not remove quotes ("), but this option can be
         turned on (property RemoveQuotes)
     6) Command list added. All user commands are saved there
     7) All previously typed commands can be accessed in a usual way (arrow up/down)
     8) Commands can be auto-completed by pressing TConsoleControls.AutoCompleteCommand key,
        or setting AutoCompleteCommandsOnKeyPress, AutoCompleteCommandsOnEnter to True
        Dbl-pressing the key, defined in the TConsoleControls.AutoCompleteCommand
        property, gives you a list  of all posible internal-external commands that
        start with your letters
     9) Batch command execution support added
    10) Short help is shown when user calls the global 'help' function
        Long help is shown elsewhere
    11) Show command help by "/?", "-?", "--?" etc
    12) Assign() added for every class

    TODO:
      [new command] Redirection with the | operator, like in any othe console (optional)
      [new command] File browser stuff... (this one's optional ;)

      Blinking cursor, "Delete" key support
      Allow long lines to continue on the next line
      May be SceneViewer should be a TControl to support the FullScreenViewer...
*)

interface

{$I GLScene.inc}

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  System.TypInfo,
  Vcl.Graphics,

  GLS.Scene,
  GLS.Coordinates,
  GLS.Objects,
  GLS.HudObjects,
  GLS.SceneViewer,
  GLS.BitmapFont,
  GLS.PersistentClasses,
  GLS.Context,
  GLS.Texture,
  GLS.Utils,
  GLS.Strings,
  GLS.Material,
  GLS.VectorTypes;

const
  CONSOLE_MAX_COMMANDS = 120;

type
  EGLConsoleException = class(Exception);

  TGLConsoleOption = (coAutoCompleteCommandsOnKeyPress,
    //commands are auto-completed as user types them
    coAutoCompleteCommandsOnEnter, //commands are auto-completed when user presses the "Enter" key
    coShowConsoleHelpIfUnknownCommand, //take a wild guess ;)
    coRemoveQuotes); //remove quotes when a command line is parsed

  TGLConsoleOptions = set of TGLConsoleOption;

  TGLCustomConsole = class;
  TGLConsoleCommandList = class;
  TGLConsoleCommand = class;

  (* Stores info on a command. A command is a parsed input line.
    Should be transformed into a class, I think...*)
  TGLUserInputCommand = record
    CommandCount: Integer;
    Strings: array of string;
    UnknownCommand: Boolean;
      //if user identifies a command, he must set this to  "True"
  end;

  // Event called when used presses the "Enter"
  TGLlConsoleEvent = procedure(const ConsoleCommand: TGLConsoleCommand;
    const Console: TGLCustomConsole;
    var Command: TGLUserInputCommand) of object;

  TGLConsoleMatchList = set of 0..CONSOLE_MAX_COMMANDS {Byte};

  // A class that checks for duplicates. 
  TGLConsoleStringList = class(TStringList)
  private
    FConsole: TGLCustomConsole;
  protected
    procedure Changed; override;
    function GetOwner: TPersistent; override;
  public
    function CommandExists(const Command: string): Boolean;
    constructor Create(const Owner: TGLCustomConsole);
  end;

  // A wrapper for a console command. 
  TGLConsoleCommand = class(TCollectionItem)
  private
    FVisible: Boolean;
    FEnabled: Boolean;
    FSilentDisabled: Boolean;
    FCommandList: TGLConsoleCommandList;
    FCommandName: string;
    FShortHelp: string;
    FLongHelp: TStringList;
    FOnCommand: TGLlConsoleEvent;
    FOnHelp: TNotifyEvent;
    procedure SetCommandName(const Value: string);
  protected
    procedure ShowInvalidUseOfCommandError; virtual;
    procedure ShowInvalidNumberOfArgumentsError(const ShowHelpAfter: Boolean =
      True); virtual;
    procedure DoOnCommand(var UserInputCommand: TGLUserInputCommand); virtual;
    function GetDisplayName: string; override;
  public
    procedure ShowHelp; virtual;
    procedure ShowShortHelp; virtual;
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property CommandName: string read FCommandName write SetCommandName;
    property ShortHelp: string read FShortHelp write FShortHelp;
    property LongHelp: TStringList read FLongHelp;
    property OnCommand: TGLlConsoleEvent read FOnCommand write FOnCommand;
    property OnHelp: TNotifyEvent read FOnHelp write FOnHelp;
    // Disabled commands won't execute
    property Enabled: Boolean read FEnabled write FEnabled default True;
    (* If command is disabled and user calls it, no error report will be
       generated if SilentDisabled is enabled *)
    property SilentDisabled: Boolean read FSilentDisabled write FSilentDisabled
      default False;
    (* Hidden commands won't show when user requests command list
      or uses auto-complete *)
    property Visible: Boolean read FVisible write FVisible default True;
  end;

  TGLConsoleCommandList = class(TCollection)
  private
    FConsole: TGLCustomConsole;
    function GetItems(const Index: Integer): TGLConsoleCommand;
  protected
    function GetOwner: TPersistent; override;
  public
    procedure SortCommands(const Ascending: Boolean = True);
    function CommandExists(const Command: string): Boolean;
    function GetCommandIndex(const Command: string): Integer;
    // General list stuff.
    function LastConsoleCommand: TGLConsoleCommand;
    function Add: TGLConsoleCommand; overload;
    // Standard stuff.
    constructor Create(const AOwner: TGLCustomConsole);
    destructor Destroy; override;
    property Items[const Index: Integer]: TGLConsoleCommand read GetItems;
      default;
  end;

  TGLConsoleControls = class(TPersistent)
  private
    FOwner: TPersistent;
    FNavigatePageUp: Byte;
    FAutoCompleteCommand: Byte;
    FPreviousCommand: Byte;
    FNextCommand: Byte;
    FNavigateUp: Byte;
    FNavigatePageDown: Byte;
    FNavigateDown: Byte;
    FDblClickDelay: Integer;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent);
    procedure Assign(Source: TPersistent); override;
  published
    property NavigateUp: Byte read FNavigateUp write FNavigateUp default VK_HOME;
    property NavigateDown: Byte read FNavigateDown write FNavigateDown default VK_END;
    property NavigatePageUp: Byte read FNavigatePageUp write FNavigatePageUp default VK_PRIOR;
    property NavigatePageDown: Byte read FNavigatePageDown write FNavigatePageDown default VK_NEXT;
    property NextCommand: Byte read FNextCommand write FNextCommand default VK_DOWN;
    property PreviousCommand: Byte read FPreviousCommand write FPreviousCommand default VK_UP;
    property AutoCompleteCommand: Byte read FAutoCompleteCommand write FAutoCompleteCommand default VK_CONTROL;
    property DblClickDelay: Integer read FDblClickDelay write FDblClickDelay default 300;
  end;

  TGLCustomConsole = class(TGLBaseSceneObject)
  private
    FHudSprite: TGLHudSprite;
    FHudText: TGLHudText;
    FSceneViewer: TGLSceneViewer;
    FInputLine: string;
    FStartLine: Integer;
    FCurrentCommand: Integer;
    FPreviousTickCount: Integer;
    FSize: Single;
    FColsoleLog: TStringList;
    FCommands: TGLConsoleCommandList;
    FAdditionalCommands: TGLConsoleStringList;
    FTypedCommands: TStringList;
    FControls: TGLConsoleControls;
    FOnCommandIssued: TGLlConsoleEvent;
    FOptions: TGLConsoleOptions;
    FHint: string;
    procedure SetSize(const Value: Single);
    procedure SetSceneViewer(const Value: TGLSceneViewer);
    function GetFont: TGLCustomBitmapFont;
    procedure SetFont(const Value: TGLCustomBitmapFont);
  protected
    // Misc 
    procedure DoOnCommandIssued(var UserInputCommand: TGLUserInputCommand); virtual;
    procedure SetFontColor(const Color: TColor); virtual;
    function GetFontColor: TColor;
    procedure SetHUDSpriteColor(const Color: TColor); virtual;
    function GetHUDSpriteColor: TColor;
    function NumLines: Integer;
    procedure ShowConsoleHelp; virtual;
    procedure HandleUnknownCommand(const Command: string); virtual;
    // Auto Complete Command 
    procedure AutoCompleteCommand; overload; virtual;
    procedure AutoCompleteCommand(var MatchCount: Integer; var
      AdditionalCommandsMatchList: TGLConsoleMatchList; var CommandsMatchList:
      TGLConsoleMatchList); overload;
    // Command interpreters 
    procedure CommandIssued(var UserInputCommand: TGLUserInputCommand); virtual;
    procedure FixCommand(var UserInputCommand: TGLUserInputCommand); virtual;
    function ParseString(str, caract: string): TGLUserInputCommand;
    procedure ProcessInput; virtual;
    // Refreshes the Hud (clip lines outside the visible console). 
    procedure RefreshHud; virtual;
    // Register built-in commands (onCreate)
    procedure RegisterBuiltInCommands; virtual;
    // Internal command handlers:
    procedure ProcessInternalCommandHelp(const ConsoleCommand:
      TGLConsoleCommand; const Console: TGLCustomConsole; var Command:
      TGLUserInputCommand); virtual;
    procedure ProcessInternalCommandClearScreen(const ConsoleCommand:
      TGLConsoleCommand; const Console: TGLCustomConsole; var Command:
      TGLUserInputCommand); virtual;
    procedure ProcessInternalCommandConsoleHide(const ConsoleCommand:
      TGLConsoleCommand; const Console: TGLCustomConsole; var Command:
      TGLUserInputCommand); virtual;
    procedure ProcessInternalCommandConsoleColor(const ConsoleCommand:
      TGLConsoleCommand; const Console: TGLCustomConsole; var Command:
      TGLUserInputCommand); virtual;
    procedure ProcessInternalCommandConsoleRename(const ConsoleCommand:
      TGLConsoleCommand; const Console: TGLCustomConsole; var Command:
      TGLUserInputCommand); virtual;
    procedure ProcessInternalCommandConsoleClearTypedCommands(const
      ConsoleCommand: TGLConsoleCommand; const Console: TGLCustomConsole; var
      Command: TGLUserInputCommand); virtual;

    procedure ProcessInternalCommandSystemTime(const ConsoleCommand:
      TGLConsoleCommand; const Console: TGLCustomConsole; var Command:
      TGLUserInputCommand); virtual;
    procedure ProcessInternalCommandSystemDate(const ConsoleCommand:
      TGLConsoleCommand; const Console: TGLCustomConsole; var Command:
      TGLUserInputCommand); virtual;

    procedure ProcessInternalCommandViewerFPS(const ConsoleCommand:
      TGLConsoleCommand; const Console: TGLCustomConsole; var Command:
      TGLUserInputCommand); virtual;
    procedure ProcessInternalCommandViewerResetPerformanceMonitor(const
      ConsoleCommand: TGLConsoleCommand; const Console: TGLCustomConsole; var
      Command: TGLUserInputCommand); virtual;
    procedure ProcessInternalCommandViewerVSync(const ConsoleCommand:
      TGLConsoleCommand; const Console: TGLCustomConsole; var Command:
      TGLUserInputCommand); virtual;
    procedure ProcessInternalCommandViewerAntiAliasing(const ConsoleCommand:
      TGLConsoleCommand; const Console: TGLCustomConsole; var Command:
      TGLUserInputCommand); virtual;
    // Internal command help handlers:
    procedure GetHelpInternalCommandRename(Sender: TObject); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure SetName(const Value: TComponentName); override;
  public
    (* Methods: User *must* call these methods in his code. *)
    procedure ProcessKeyPress(const c: Char); virtual;
    procedure ProcessKeyDown(const key: word); virtual;
    // Navigation through code from outside
    procedure NavigateUp;
    procedure NavigateDown;
    procedure NavigatePageUp;
    procedure NavigatePageDown;
    (* Refreshes the size of the hud to reflect changes on the viewer.
       Should be called whenever the viewer's size changes. *)
    procedure RefreshHudSize; virtual;
    // Adds a line (which is not treated as a command). 
    procedure AddLine(const str: string);
    // TypedCommands are cleared and current command index is reset. 
    procedure ClearTypedCommands;
    procedure ExecuteCommand(const Command: string);
    procedure ExecuteCommands(const Commands: TStrings);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Changes the console font color. 
    property FontColor: TColor read GetFontColor write SetFontColor stored False;
    property HUDSpriteColor: TColor read GetHUDSpriteColor write SetHUDSpriteColor stored False;
    // Where user enters his commands.
    property InputLine: string read FInputLine write FInputLine;
    // List of commands that user typed.
    property TypedCommands: TStringList read FTypedCommands;
    // Commands have events that are called when user types a sertauin command
    property Commands: TGLConsoleCommandList read FCommands;
    (* Aditional commands can be registered to participate in command auto-completion.
     They can be interpreted in the global OnCommandIssued event handler. *)
    property AdditionalCommands: TGLConsoleStringList read FAdditionalCommands;
    // User controls. 
    property Controls: TGLConsoleControls read FControls;
    // List of commands that user typed and console's responces. 
    property ColsoleLog: TStringList read FColsoleLog;
    // Allows to change consol's height from 0 to 1. 
    property Size: Single read FSize write SetSize;
    // Visual stuff. 
    property SceneViewer: TGLSceneViewer read FSceneViewer write SetSceneViewer;
    property HudSprite: TGLHudSprite read FHudSprite;
    property HudText: TGLHudText read FHudText;
    property Font: TGLCustomBitmapFont read GetFont write SetFont stored False;
    property Options: TGLConsoleOptions read FOptions write FOptions;
    (* Main event of the console. Happens whenever the enter key is pressed.
      First the input line is compared to all registered commands, then everything
      is parsed into a TGLUserInputCommand record and  sent to the event.
      Empty lines are  not  ignored (i.e. they also trigger events) *)
    property OnCommandIssued: TGLlConsoleEvent read FOnCommandIssued write
      FOnCommandIssued;
    // Standard stuff 
    property Hint: string read FHint write FHint;
    property Visible default False;
  end;

  TGLConsole = class(TGLCustomConsole)
  published
    property FontColor;
    property HUDSpriteColor;
    property InputLine;
    property TypedCommands;
    property Commands;
    property AdditionalCommands;
    property Controls;
    property ColsoleLog;
    property SceneViewer;
    property HudSprite;
    property HudText;
    property Font;
    property Options;
    property OnCommandIssued;
    property Hint;
    property Tag;
    property ObjectsSorting;
    property Visible;
    property OnProgress;
  end;

//-------------------------------------------------------------------
implementation
//-------------------------------------------------------------------

const
  STR_NO_DUPLICATE_NAMES_ALLOWED = 'Duplicate names not allowed!';
  STR_UNRECOGNIZED_PARAMETER = 'Unrecognized parameter: ';

  conDefaultFontCharHeight = 15;
  conDefaultConsoleWidth = 400;
  conDefaultConsoleHeight = 100;

//------------------------------
//  TGLCustomConsole 
//------------------------------

procedure TGLCustomConsole.ProcessInternalCommandClearScreen(const
  ConsoleCommand: TGLConsoleCommand; const Console: TGLCustomConsole; var Command:
  TGLUserInputCommand);
begin
  Console.FInputLine := '';
  Console.ColsoleLog.Clear;
end;

procedure TGLCustomConsole.ProcessInternalCommandConsoleHide(const
  ConsoleCommand: TGLConsoleCommand; const Console: TGLCustomConsole; var Command:
  TGLUserInputCommand);
begin
  Console.Visible := False;
  AddLine(' - Console hidden');
end;

procedure TGLCustomConsole.ProcessInternalCommandConsoleColor(const
  ConsoleCommand: TGLConsoleCommand; const Console: TGLCustomConsole; var Command:
  TGLUserInputCommand);
var
  NewColor: TColor;
begin
  with Console, ConsoleCommand do
  begin
    if Command.CommandCount = 1 then
      AddLine(' - Current console font color is ' +
        ColorToString(FHudText.ModulateColor.AsWinColor))
    else if Command.CommandCount = 2 then
    begin
      if TryStringToColorAdvanced(Command.Strings[1], NewColor) then
      begin
        //color changed successfully
        SetFontColor(NewColor);
        AddLine(' - Current console font changed to ' +
          ColorToString(NewColor));
      end
      else
      begin
        //color unidentified!
        AddLine(' - Color unidentified!');
      end;
    end
    else
      ConsoleCommand.ShowInvalidNumberOfArgumentsError;
  end;
end;

procedure TGLCustomConsole.ProcessInternalCommandConsoleRename(const
  ConsoleCommand: TGLConsoleCommand; const Console: TGLCustomConsole; var Command:
  TGLUserInputCommand);
var
  CommandIndex: Integer;
begin
  if (Command.CommandCount <> 3) then
    ConsoleCommand.ShowInvalidNumberOfArgumentsError
  else
  begin
    CommandIndex :=
      ConsoleCommand.FCommandList.GetCommandIndex(Command.Strings[1]);

    if CommandIndex = -1 then
    begin
      AddLine(' - Could not rename  command +"' + Command.Strings[1] + '" to "'
        +
        Command.Strings[2] + '": such command was not found!');
      ConsoleCommand.ShowHelp;
    end
    else if ConsoleCommand.FCommandList.CommandExists(Command.Strings[2]) or
      Console.FAdditionalCommands.CommandExists(Command.Strings[2]) then
    begin
      AddLine(' - Could not rename  command +"' + Command.Strings[1] + '" to "'
        +
        Command.Strings[2] + '": command "' + Command.Strings[2] +
          '" already exists!');
      ConsoleCommand.ShowHelp;
    end
    else
    begin
      ConsoleCommand.FCommandName := Command.Strings[2];
      AddLine(' - Command "' + Command.Strings[1] + '" successfully renamed to "'
        +
        Command.Strings[2] + '"!');
    end;
  end;
end;

procedure TGLCustomConsole.ProcessInternalCommandConsoleClearTypedCommands(const
  ConsoleCommand: TGLConsoleCommand; const Console: TGLCustomConsole; var Command:
  TGLUserInputCommand);
begin
  if (Command.CommandCount = 1) then
    Console.ClearTypedCommands
  else
    ConsoleCommand.ShowInvalidNumberOfArgumentsError;
end;

procedure TGLCustomConsole.ProcessInternalCommandSystemDate(const
  ConsoleCommand: TGLConsoleCommand; const Console: TGLCustomConsole; var Command:
  TGLUserInputCommand);
begin
  if (Command.CommandCount = 1) then
    AddLine(' - Current system date is: ' + DateToStr(now))
  else
    ConsoleCommand.ShowInvalidNumberOfArgumentsError;
end;

procedure TGLCustomConsole.ProcessInternalCommandHelp(const ConsoleCommand:
  TGLConsoleCommand; const Console: TGLCustomConsole; var Command:
  TGLUserInputCommand);
var
  MainCommand: string;
  I: Integer;
begin
  if Command.CommandCount = 1 then
    Console.ShowConsoleHelp
  else if (Command.CommandCount = 2) then
  begin
    MainCommand := LowerCase(Command.Strings[1]);

    if FCommands.Count <> 0 then
      for I := 0 to FCommands.Count - 1 do
        if MainCommand = LowerCase(FCommands[I].FCommandName) then
        begin
          FCommands[I].ShowHelp;
          Exit;
        end;

    if FAdditionalCommands.Count <> 0 then
      for I := 0 to FAdditionalCommands.Count - 1 do
        if MainCommand = LowerCase(FAdditionalCommands[I]) then
        begin
          AddLine(' - Command "' + Command.Strings[1] +
            '" exists, but help is unavaible,');
          AddLine(' - because it is an external command');
          Exit;
        end;

    HandleUnknownCommand(Command.Strings[1]);
  end
  else
    ConsoleCommand.ShowInvalidNumberOfArgumentsError;
end;

procedure TGLCustomConsole.ProcessInternalCommandSystemTime(const
  ConsoleCommand: TGLConsoleCommand; const Console: TGLCustomConsole; var Command:
  TGLUserInputCommand);
begin
  if Command.CommandCount = 1 then
    AddLine(' - Current system time is: ' + TimeToStr(now))
  else
    ConsoleCommand.ShowInvalidNumberOfArgumentsError;
end;

procedure TGLCustomConsole.GetHelpInternalCommandRename(Sender: TObject);
begin
  with TGLConsoleCommand(Sender) do
  begin
    Addline(' - The "' + FCommandName + '" command can rename any command');
    AddLine(' - Usage:');
    AddLine(' - ' + FCommandName + ' [old_command_name] [new_command_name]');
  end;
end;

procedure TGLCustomConsole.ProcessInternalCommandViewerFPS(const ConsoleCommand:
  TGLConsoleCommand; const Console: TGLCustomConsole; var Command:
  TGLUserInputCommand);
begin
  if Command.CommandCount = 1 then
  begin
    if Console.FSceneViewer <> nil then
      AddLine(' - Current SceneViewer has ' +
        Console.FSceneViewer.FramesPerSecondText)
    else
      AddLine(' - ' + strErrorEx + strSceneViewerNotDefined);
  end
  else
    ConsoleCommand.ShowInvalidNumberOfArgumentsError;
end;

procedure
  TGLCustomConsole.ProcessInternalCommandViewerResetPerformanceMonitor(const
  ConsoleCommand: TGLConsoleCommand; const Console: TGLCustomConsole; var Command:
  TGLUserInputCommand);
begin
  if Command.CommandCount = 1 then
  begin
    if Console.FSceneViewer <> nil then
    begin
      Console.FSceneViewer.ResetPerformanceMonitor;
      AddLine(' - ResetPerformanceMonitor for Current SceneViewer completed');
    end
    else
      AddLine(' - ' + strErrorEx + strSceneViewerNotDefined);
  end
  else
    ConsoleCommand.ShowInvalidNumberOfArgumentsError;
end;

procedure TGLCustomConsole.ProcessInternalCommandViewerVSync(const
  ConsoleCommand: TGLConsoleCommand; const Console: TGLCustomConsole; var Command:
  TGLUserInputCommand);
const
  ON_OFF: array[Boolean] of string = ('Off', 'On');
begin
  if Console.FSceneViewer <> nil then
  begin
    if Command.CommandCount = 1 then
    begin
      AddLine(' - Current SceneViewer VSync is ' +
        ON_OFF[Console.FSceneViewer.VSync = vsmSync]);
    end
    else if (Command.CommandCount = 2) then
    begin
      if Command.Strings[1] = ON_OFF[False] then
        Console.FSceneViewer.VSync := vsmNoSync
      else if Command.Strings[1] = ON_OFF[True] then
        Console.FSceneViewer.VSync := vsmSync
      else
      begin
        AddLine(' - ' + STR_UNRECOGNIZED_PARAMETER + Command.Strings[1]);
        Exit;
      end;

      AddLine(' - Current SceneViewer VSync was changed to ' +
        ON_OFF[Console.FSceneViewer.VSync = vsmSync]);
    end
    else
      HandleUnknownCommand(Command.Strings[1]);
  end
  else
    AddLine(' - ' + strErrorEx + strSceneViewerNotDefined);
end;

procedure TGLCustomConsole.ProcessInternalCommandViewerAntiAliasing(
  const ConsoleCommand: TGLConsoleCommand;
  const Console: TGLCustomConsole; var Command: TGLUserInputCommand);
var
  Temp: Integer;
begin
  if Console.FSceneViewer <> nil then
  begin
    if Command.CommandCount = 1 then
      AddLine(' - Current SceneViewer AntiAliasing = ' +
        GetEnumName(TypeInfo(TGLAntiAliasing),
        Ord(Console.FSceneViewer.Buffer.AntiAliasing)))
    else if (Command.CommandCount = 2) then
    begin
      Temp := GetEnumValue(TypeInfo(TGLAntiAliasing), Command.Strings[1]);
      if Temp = -1 then
      begin
        AddLine(' - ' + STR_UNRECOGNIZED_PARAMETER + Command.Strings[1]);
      end
      else
      begin
        Console.FSceneViewer.Buffer.AntiAliasing := TGLAntiAliasing(Temp);
        AddLine(' - Current SceneViewer AntiAliasing was changed to  ' +
          GetEnumName(TypeInfo(TGLAntiAliasing),
          Ord(Console.FSceneViewer.Buffer.AntiAliasing)))
      end;
    end
    else
      ConsoleCommand.ShowInvalidNumberOfArgumentsError;
  end
  else
    AddLine(' - ' + strErrorEx + strSceneViewerNotDefined);
end;

function TGLCustomConsole.ParseString(str, caract: string): TGLUserInputCommand;
var
  p1: Integer;
begin
  Result.CommandCount := 0;
  while True do
  begin
    p1 := pos(caract, str);
    if (p1 = 0) or (p1 = -1) then
      break;
    SetLength(Result.Strings, Result.CommandCount + 1);
    Result.Strings[Result.CommandCount] := copy(str, 1, p1 - 1);
    str := copy(str, p1 + 1, Length(str));
    Result.CommandCount := Result.CommandCount + 1;
  end;
  if Length(str) > 0 then
  begin
    setlength(Result.Strings, Result.CommandCount + 1);
    Result.Strings[Result.CommandCount] := str;
    Result.CommandCount := Result.CommandCount + 1;
  end;
end;

procedure TGLCustomConsole.FixCommand(var UserInputCommand:
  TGLUserInputCommand);
var
  nCount, I: Integer;
  openq: Boolean;
begin
  for I := 0 to UserInputCommand.CommandCount - 1 do
    UserInputCommand.Strings[I] := trim(UserInputCommand.Strings[I]);

  nCount := 0;
  I := 0;
  openq := False;
  while nCount < UserInputCommand.CommandCount do
  begin
    if UserInputCommand.Strings[I] = '' then
    begin
      if UserInputCommand.Strings[nCount] <> '' then
        UserInputCommand.Strings[I] := UserInputCommand.Strings[nCount];
    end
    else if openq then
      UserInputCommand.Strings[I] := UserInputCommand.Strings[I] + ' ' +
        UserInputCommand.Strings[nCount];

    if (Length(UserInputCommand.Strings[I]) > 0) then
    begin
      if coRemoveQuotes in FOptions then
      begin
        if (UserInputCommand.Strings[I][1] = '"') and
          (UserInputCommand.Strings[I][Length(UserInputCommand.Strings[I])] =
            '"') then
          UserInputCommand.Strings[I] := copy(UserInputCommand.Strings[I], 2,
            Length(UserInputCommand.Strings[I]) - 2);

        if (UserInputCommand.Strings[I][1] = '"') and not openq then
        begin
          openq := True;
          UserInputCommand.Strings[I] := copy(UserInputCommand.Strings[I], 2,
            Length(UserInputCommand.Strings[I]));
        end;

        if (UserInputCommand.Strings[I][Length(UserInputCommand.Strings[I])] =
          '"') and openq then
        begin
          openq := False;
          UserInputCommand.Strings[I] := copy(UserInputCommand.Strings[I], 1,
            Length(UserInputCommand.Strings[I]) - 1);
        end;
      end;

      if not openq then
        Inc(I);
    end;
    Inc(nCount);
  end;

  if I < UserInputCommand.CommandCount then
  begin
    setLength(UserInputCommand.Strings, I);
    UserInputCommand.CommandCount := I;
  end;
end;

constructor TGLCustomConsole.Create(AOwner: TComponent);
begin
  inherited;
  FColsoleLog := TStringList.Create;
  FTypedCommands := TStringList.Create;
  FCommands := TGLConsoleCommandList.Create(Self);
  FAdditionalCommands := TGLConsoleStringList.Create(Self);
  FControls := TGLConsoleControls.Create(Self);

  FHudSprite := TGLHudSprite.Create(Self);
  MakeSubComponent(FHudSprite, True);
  AddChild(FHudSprite);
  FHudSprite.FreeNotification(Self);
  with FHudSprite.Material do
  begin
    BlendingMode := bmTransparency;
    FrontProperties.Diffuse.Alpha := 0.5;
    Texture.TextureMode := tmModulate;
    Texture.Enabled := True;
  end;

  FHudText := TGLHudText.Create(Self);
  MakeSubComponent(FHudText, True);
  AddChild(FHUDText);
  FHudText.FreeNotification(Self);
  FHudText.Position.Y := 2;
  FHudText.Position.X := 3;

  FSize := 0.35;

  RegisterBuiltIncommands;
  SetVisible(False);
  SetHUDSpriteColor(clWhite);
  SetFontColor(clBlue);
end;

destructor TGLCustomConsole.Destroy;
begin
  Controls.Destroy;
  FCommands.Destroy;
  FAdditionalCommands.Destroy;
  FTypedCommands.Destroy;
  FColsoleLog.Destroy;
  FreeAndNil(FHudSprite);
  FreeAndNil(FHudText);
  inherited;
end;

procedure TGLCustomConsole.ProcessKeyPress(const c: Char);
begin
  if not Visible then
    Exit;

  if c = #8 then //VK_BACK
    FInputLine := copy(FInputLine, 1, Length(FInputLine) - 1)
  else if c = #13 then //VK_RETURN
  begin
    if coAutoCompleteCommandsOnEnter in FOptions then
      AutoCompleteCommand;

    //remmember the current entered command
    if (FInputLine <> '') and (FInputLine <> #13) then
    begin
      if FTypedCommands.Count = 0 then
        FCurrentCommand := FTypedCommands.Add(FInputLine) + 1
      else
      begin
        if FTypedCommands[FTypedCommands.Count - 1] <> FInputLine then
          FCurrentCommand := FTypedCommands.Add(FInputLine) + 1;
      end;
    end;

    ProcessInput;
  end
  else
    FInputLine := FinputLine + c;

  if coAutoCompleteCommandsOnKeyPress in FOptions then
    AutoCompleteCommand;

  RefreshHud;
end;

procedure TGLCustomConsole.ProcessKeyDown(const key: word);
var
  MatchCount: Integer;
  AdditionalCommandsMatchList: TGLConsoleMatchList;
  CommandsMatchList: TGLConsoleMatchList;
  CurrentTickCount: Integer;
  I: Integer;
begin
  if not Visible then
    Exit;

  if (key = FControls.NextCommand) then
    if FCurrentCommand <> FTypedCommands.Count then
    begin
      if FCurrentCommand <> FTypedCommands.Count - 1 then
        Inc(FCurrentCommand);
      FinputLine := FTypedCommands[FCurrentCommand];
    end;

  if (key = FControls.PreviousCommand) then
    if FTypedCommands.Count <> 0 then
    begin
      if FCurrentCommand <> 0 then
        Dec(FCurrentCommand);
      FinputLine := FTypedCommands[FCurrentCommand];
    end;

  if (key = FControls.AutoCompleteCommand) then
  begin
    CurrentTickCount := GetTickCount;
    AutoCompleteCommand(MatchCount, AdditionalCommandsMatchList,
      CommandsMatchList);
    if MatchCount = 0 then
      Beep;

    if CurrentTickCount - FPreviousTickCount < Controls.FDblClickDelay then
      if MatchCount > 1 then
      begin
        if CommandsMatchList <> [] then
        begin
          AddLine(' - Registered commands:');
          for I := 0 to CONSOLE_MAX_COMMANDS do
            if I in CommandsMatchList then
              AddLine('   - ' + FCommands[I].FCommandName);
        end;

        if AdditionalCommandsMatchList <> [] then
        begin
          AddLine(' - Additional registered commands:');
          for I := 0 to CONSOLE_MAX_COMMANDS do
            if I in AdditionalCommandsMatchList then
              AddLine('   - ' + FAdditionalCommands[I]);
        end;
      end;

    FPreviousTickCount := CurrentTickCount;
  end;

  if (key = FControls.NavigateUp) then
    Dec(FStartLine);
  if (key = FControls.NavigateDown) then
    Inc(FStartLine);
  if (key = FControls.NavigatePageUp) then
    Dec(FStartLine, NumLines);
  if key = FControls.NavigatePageDown then
    Inc(FStartLine, NumLines);

  RefreshHud;
end;

procedure TGLCustomConsole.RefreshHud;
var
  outStr: string;
  endLine, I: Integer;
begin
  //beware! This stuf is messy

  if FStartLine > FColsoleLog.Count - numlines then
    FStartLine := FColsoleLog.Count - numlines;
  if FStartLine < 0 then
    FStartLine := 0;

  endLine := FStartLine + numlines - 1;

  if FColsoleLog.Count < numLines then
    outStr := FColsoleLog.Text
  else
  begin
    for I := FStartLine to endLine do
      outStr := outStr + FColsoleLog[I] + #13;
  end;

  FHudText.Text := outStr + '> ' + FInputLine;
end;

function TGLCustomConsole.NumLines: Integer;
begin
  if GetFont = nil then
    Result := Trunc(FHudSprite.Height / conDefaultFontCharHeight - 1.7)
  else
    Result := Trunc(FHudSprite.Height / GetFont.CharHeight - 1.7);
end;

procedure TGLCustomConsole.ProcessInput;
var
  info: TGLUserInputCommand;
begin
  //Add the current line
  AddLine(FInputLine);

  //Get everything between spaces
  info := ParseString(FInputLine, ' ');
  info.UnknownCommand := True;
  //Remove empty strings and " sequences
  FixCommand(info);
  //Execute the command
  CommandIssued(info);

  //Clear the current line
  FinputLine := '';
end;

procedure TGLCustomConsole.ExecuteCommands(const Commands: TStrings);
var
  I: Integer;
begin
  if Commands.Count = 0 then
    Exit;
  for I := 0 to Commands.Count - 1 do
    ExecuteCommand(Commands[I]);
end;

procedure TGLCustomConsole.ExecuteCommand(const Command: string);
begin
  FInputLine := Command;
  ProcessInput;
end;

procedure TGLCustomConsole.AddLine(const str: string);
begin
  FColsoleLog.Text := FColsoleLog.Text + str + #10;
  FStartLine := FColsoleLog.Count - numLines;
  RefreshHud;
end;

procedure TGLCustomConsole.CommandIssued(var UserInputCommand:
  TGLUserInputCommand);
var
  MainCommand: string;
  I: Integer;
begin
  if UserInputCommand.CommandCount = 0 then
    Exit;

  MainCommand := LowerCase(UserInputCommand.Strings[0]);

  if FCommands.Count <> 0 then
    for I := 0 to FCommands.Count - 1 do
      if MainCommand = LowerCase(FCommands[I].FCommandName) then
      begin
        //show help
        if UserInputCommand.CommandCount > 1 then
        begin
          //I hope I didn't forget anything ;)
          if (UserInputCommand.Strings[1] = '/?') or
            (UserInputCommand.Strings[1] = '\?') or
            (UserInputCommand.Strings[1] = '-?') or
            (UserInputCommand.Strings[1] = '--?') or
            (UserInputCommand.Strings[1] = '/help') or
            (UserInputCommand.Strings[1] = '\help') or
            (UserInputCommand.Strings[1] = '-help') or
            (UserInputCommand.Strings[1] = '--help') then
            FCommands[I].ShowHelp
          else
            //or execute the asosiated event
            FCommands[I].DoOnCommand(UserInputCommand);
        end
        else
          //or execute the asosiated event
          FCommands[I].DoOnCommand(UserInputCommand);

        //recognize the command
        UserInputCommand.UnknownCommand := False;
        break;
      end;

  //external command processing event
  DoOnCommandIssued(UserInputCommand);

  if UserInputCommand.UnknownCommand then
    HandleUnknownCommand(UserInputCommand.Strings[0]);
end;

procedure TGLCustomConsole.RefreshHudSize;
begin
  if FSceneViewer <> nil then
  begin
    FHudSprite.Width := FSceneViewer.Width;
    FHudSprite.Height := FSceneViewer.Height * FSize;
  end
  else
  begin
    FHudSprite.Width := conDefaultConsoleWidth;
    FHudSprite.Height := conDefaultConsoleHeight;
  end;

  FHudSprite.Position.X := FHudSprite.Width / 2;
  FHudSprite.Position.Y := FHudSprite.Height / 2;
  RefreshHud;
end;

procedure TGLCustomConsole.SetFontColor(const Color: TColor);
begin
  FHUDText.ModulateColor.AsWinColor := Color;
  FHUDText.Material.FrontProperties.Ambient.AsWinColor := Color;
end;

procedure TGLCustomConsole.ShowConsoleHelp;
var
  I: Integer;

begin
  if (FCommands.Count = 0) and (FAdditionalCommands.Count = 0) then
    AddLine(' - There are no registered commands!')
  else
  begin
    if FCommands.Count <> 0 then
    begin
      AddLine(' - List of registered console commands:');
      for I := 0 to FCommands.Count - 1 do
        FCommands[I].ShowShortHelp;
    end;

    if FAdditionalCommands.Count <> 0 then
    begin
      AddLine(' - List of additional console commands:');
      for I := 0 to FAdditionalCommands.Count - 1 do
        AddLine('   - ' + FAdditionalCommands[I]);
    end;
  end;
end;

procedure TGLCustomConsole.ClearTypedCommands;
begin
  FTypedCommands.Clear;
  FCurrentCommand := 0;
end;

{$WARNINGS off}

procedure TGLCustomConsole.AutoCompleteCommand(var MatchCount: Integer;
  var AdditionalCommandsMatchList: TGLConsoleMatchList;
  var CommandsMatchList: TGLConsoleMatchList);
var
  I: Integer;
  HasEnterKey: Boolean;
  NewInputLine, FirstMatch: string;
  NewMatchCount, FirstMatchIndex: Integer;
begin
  MatchCount := 0;
  AdditionalCommandsMatchList := [];
  CommandsMatchList := [];
  if FInputLine <> '' then
  begin
    //delete the last "Enter" key, if there is any
    if FInputLine[Length(FInputLine)] = #13 then
    begin
      Delete(FInputLine, Length(FInputLine), 1);
      HasEnterKey := True;
    end;

    //find all the matches
    if FAdditionalCommands.Count <> 0 then
      for I := 0 to FAdditionalCommands.Count - 1 do
        if AnsiStartsText(FInputLine, FAdditionalCommands[I]) then
        begin
          Inc(MatchCount);
          AdditionalCommandsMatchList := AdditionalCommandsMatchList + [I];
        end;

    if FCommands.Count <> 0 then
      for I := 0 to FCommands.Count - 1 do
        if FCommands[I].FVisible then
          if AnsiStartsText(FInputLine, FCommands[I].FCommandName) then
          begin
            Inc(MatchCount);
            CommandsMatchList := CommandsMatchList + [I];
          end;

    //if there is only one, fill it up!
    if MatchCount = 1 then
    begin
      if AdditionalCommandsMatchList <> [] then
        for I := 0 to CONSOLE_MAX_COMMANDS do
          if I in AdditionalCommandsMatchList then
          begin
            FInputLine := FAdditionalCommands[I];
            break;
          end;
      if CommandsMatchList <> [] then
        for I := 0 to CONSOLE_MAX_COMMANDS do
          if I in CommandsMatchList then
          begin
            FInputLine := FCommands[I].FCommandName;
            break;
          end;
    end
    else
      {//if more than 1, try to complete other letters} if MatchCount > 1 then
      begin
        NewInputLine := FInputLine;
        //find 1st match
        if AdditionalCommandsMatchList <> [] then
          for I := 0 to CONSOLE_MAX_COMMANDS do
            if I in AdditionalCommandsMatchList then
            begin
              FirstMatch := FAdditionalCommands[I];
              FirstMatchIndex := I;
              break;
            end;

        if AdditionalCommandsMatchList = [] then
          for I := 0 to CONSOLE_MAX_COMMANDS do
            if I in CommandsMatchList then
            begin
              FirstMatch := FCommands[I].FCommandName;
              FirstMatchIndex := I;
              break;
            end;

        NewMatchCount := MatchCount;
        while (NewMatchCount = MatchCount) and (Length(NewInputLine) <>
          Length(FirstMatch)) do
        begin
          NewInputLine := NewInputLine + FirstMatch[Length(NewInputLine) + 1];
          NewMatchCount := 0;

          if AdditionalCommandsMatchList <> [] then
            for I := FirstMatchIndex to FAdditionalCommands.Count - 1 do
              if AnsiStartsText(NewInputLine, FAdditionalCommands[I]) then
                Inc(NewMatchCount);

          if AdditionalCommandsMatchList = [] then
          begin
            for I := FirstMatchIndex to FCommands.Count - 1 do
              if AnsiStartsText(NewInputLine, FCommands[I].FCommandName) then
                Inc(NewMatchCount);
          end
          else if CommandsMatchList <> [] then
          begin
            for I := 0 to FCommands.Count - 1 do
              if AnsiStartsText(NewInputLine, FCommands[I].FCommandName) then
                Inc(NewMatchCount);
          end;
        end;

        FInputLine := NewInputLine;

        if NewMatchCount <> MatchCount then
          Delete(FInputLine, Length(NewInputLine), 1);
      end;

    //Restore the #13 key
    if HasEnterKey then
      FInputLine := FInputLine + #13;
  end;
end;
{$WARNINGS on}

procedure TGLCustomConsole.AutoCompleteCommand;
var
  MatchCount: Integer;
  AdditionalCommandsMatchList: TGLConsoleMatchList;
  CommandsMatchList: TGLConsoleMatchList;
begin
  AutoCompleteCommand(MatchCount, AdditionalCommandsMatchList,
    CommandsMatchList);
end;

procedure TGLCustomConsole.RegisterBuiltInCommands;
begin
  { Special commands }
  with FCommands.Add do
  begin
    FCommandName := '?';
    FShortHelp := 'displays help for a single command or all commands';
    FLongHelp.Add(FShortHelp);
    FOnCommand := ProcessInternalCommandHelp;
  end;

  with FCommands.Add do
  begin
    FCommandName := 'Help';
    FShortHelp := 'displays help for a single command or all commands';
    FLongHelp.Add(FShortHelp);
    FOnCommand := ProcessInternalCommandHelp;
  end;

  with FCommands.Add do
  begin
    FCommandName := 'cls';
    FShortHelp := 'clears screen';
    FLongHelp.Add(FShortHelp);
    FOnCommand := ProcessInternalCommandClearScreen;
  end;

  // Console commands 
  with FCommands.Add do
  begin
    FCommandName := 'Console.Hide';
    FShortHelp := 'hides the console';
    FLongHelp.Add(FShortHelp);
    FOnCommand := ProcessInternalCommandConsoleHide;
  end;

  with FCommands.Add do
  begin
    FCommandName := 'Console.Color';
    FShortHelp := 'displays and allows to change the color of the console';
    FLongHelp.Add(FShortHelp);
    FOnCommand := ProcessInternalCommandConsoleColor;
  end;

  with FCommands.Add do
  begin
    FCommandName := 'Console.Ren';
    FShortHelp := 'renames any command';
    // FLongHelp.Add('') not needed here, because is has an OnHelp event
    FOnCommand := ProcessInternalCommandConsoleRename;
    FOnHelp := GetHelpInternalCommandRename;
  end;

  with FCommands.Add do
  begin
    FCommandName := 'Console.ClearTypedCommands';
    FShortHelp := 'clears Typed Commands list';
    FLongHelp.Add(FShortHelp);
    FOnCommand := ProcessInternalCommandConsoleClearTypedCommands;
  end;

  // System commands 
  with FCommands.Add do
  begin
    FCommandName := 'System.Time';
    FShortHelp := 'displays current system time';
    FLongHelp.Add(FShortHelp);
    FOnCommand := ProcessInternalCommandSystemTime;
  end;

  with FCommands.Add do
  begin
    FCommandName := 'System.Date';
    FShortHelp := 'displays current system date';
    FLongHelp.Add(FShortHelp);
    FOnCommand := ProcessInternalCommandSystemDate;
  end;

  // Viewer commands 
  with FCommands.Add do
  begin
    FCommandName := 'Viewer.FPS';
    FShortHelp := 'displays GLSceneViewer FPS';
    FLongHelp.Add(FShortHelp);
    FOnCommand := ProcessInternalCommandViewerFPS;
  end;

  with FCommands.Add do
  begin
    FCommandName := 'Viewer.ResetPerformanceMonitor';
    FShortHelp := 'resets GLSceneViewer FPS monitor';
    FLongHelp.Add(FShortHelp);
    FOnCommand := ProcessInternalCommandViewerResetPerformanceMonitor;
  end;

  with FCommands.Add do
  begin
    FCommandName := 'Viewer.VSync';
    FShortHelp := 'displays and allows to change GLSceneViewer VSync';
    FLongHelp.Add(FShortHelp);
    FOnCommand := ProcessInternalCommandViewerVSync;
  end;

  with FCommands.Add do
  begin
    FCommandName := 'Viewer.AntiAliasing';
    FShortHelp := 'displays and allows to change GLSceneViewer AntiAliasing';
    FLongHelp.Add(FShortHelp);
    FOnCommand := ProcessInternalCommandViewerAntiAliasing;
  end;
end;

procedure TGLCustomConsole.HandleUnknownCommand(const Command: string);
begin
  AddLine(' - Command "' + Command + '" not recognized!');
  if coShowConsoleHelpIfUnknownCommand in FOptions then
    ShowConsoleHelp;
end;

procedure TGLCustomConsole.NavigateDown;
begin
  Inc(FStartLine);
  if FStartLine > FColsoleLog.Count - numlines then
    FStartLine := FColsoleLog.Count - numlines;
  if FStartLine < 0 then
    FStartLine := 0;
end;

procedure TGLCustomConsole.NavigatePageDown;
begin
  Inc(FStartLine, NumLines);
  if FStartLine > FColsoleLog.Count - numlines then
    FStartLine := FColsoleLog.Count - numlines;
  if FStartLine < 0 then
    FStartLine := 0;
end;

procedure TGLCustomConsole.NavigatePageUp;
begin
  Dec(FStartLine, NumLines);
  if FStartLine > FColsoleLog.Count - numlines then
    FStartLine := FColsoleLog.Count - numlines;
  if FStartLine < 0 then
    FStartLine := 0;
end;

procedure TGLCustomConsole.NavigateUp;
begin
  Dec(FStartLine);
  if FStartLine > FColsoleLog.Count - numlines then
    FStartLine := FColsoleLog.Count - numlines;
  if FStartLine < 0 then
    FStartLine := 0;
end;

function TGLCustomConsole.GetFontColor: TColor;
begin
  Result := FHUDText.ModulateColor.AsWinColor;
end;

function TGLCustomConsole.GetHUDSpriteColor: TColor;
begin
  if Assigned(HUDSprite.Material.MaterialLibrary)
    and (HUDSprite.Material.MaterialLibrary is TGLMaterialLibrary)
    and (HUDSprite.Material.LibMaterialName <> '') then
    Result :=
      TGLMaterialLibrary(HUDSprite.Material.MaterialLibrary).LibMaterialByName(HUDSprite.Material.LibMaterialName).Material.FrontProperties.Ambient.AsWinColor
  else
    Result := HUDSprite.Material.FrontProperties.Ambient.AsWinColor;
end;

procedure TGLCustomConsole.SetHUDSpriteColor(const Color: TColor);
begin
  if Assigned(HUDSprite.Material.MaterialLibrary)
    and (HUDSprite.Material.MaterialLibrary is TGLMaterialLibrary)
    and (HUDSprite.Material.LibMaterialName <> '') then
      TGLMaterialLibrary(HUDSprite.Material.MaterialLibrary).LibMaterialByName(HUDSprite.Material.LibMaterialName).Material.FrontProperties.Ambient.AsWinColor := Color
  else
    HUDSprite.Material.FrontProperties.Ambient.AsWinColor := Color;
end;

procedure TGLCustomConsole.SetSize(const Value: Single);
begin
  if (Value <= 0) or (Value > 1) then
    raise EGLConsoleException.Create('Size must be between 0 and 1!')
  else
  begin
    FSize := Value;
    RefreshHudSize;
  end;
end;

procedure TGLCustomConsole.DoOnCommandIssued(var UserInputCommand:
  TGLUserInputCommand);
begin
  if Assigned(FOnCommandIssued) then
    FOnCommandIssued(nil, Self, UserInputCommand);
end;

procedure TGLCustomConsole.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FSceneViewer then
      FSceneViewer := nil;

    if AComponent = FHudSprite then
      FHudSprite := nil;

    if AComponent = FHudText then
      FHudText := nil;
  end;
end;

procedure TGLCustomConsole.SetSceneViewer(
  const Value: TGLSceneViewer);
begin
  if FSceneViewer <> nil then
    FSceneViewer.RemoveFreeNotification(Self);

  FSceneViewer := Value;

  if FSceneViewer <> nil then
  begin
    FSceneViewer.FreeNotification(Self);
    RefreshHudSize;
  end;
end;

function TGLCustomConsole.GetFont: TGLCustomBitmapFont;
begin
  Result := FHudText.BitmapFont;
end;

procedure TGLCustomConsole.SetFont(const Value: TGLCustomBitmapFont);
begin
  FHudText.BitmapFont := Value;
end;

procedure TGLCustomConsole.SetName(const Value: TComponentName);
begin
  inherited;
  FHudSprite.Name := Value + 'HudSprite';
  FHudText.Name := Value + 'HudText';
end;

//--------------------------------
// TGLConsoleControls 
//--------------------------------

procedure TGLConsoleControls.Assign(Source: TPersistent);
begin
  if Source is TGLConsoleControls then
  begin
    FNavigateUp := TGLConsoleControls(Source).FNavigateUp;
    FNavigateDown := TGLConsoleControls(Source).FNavigateDown;
    FNavigatePageUp := TGLConsoleControls(Source).FNavigatePageUp;
    FNavigatePageDown := TGLConsoleControls(Source).FNavigatePageDown;
    FNextCommand := TGLConsoleControls(Source).FNextCommand;
    FPreviousCommand := TGLConsoleControls(Source).FPreviousCommand;
    FAutoCompleteCommand := TGLConsoleControls(Source).FAutoCompleteCommand;
    FDblClickDelay := TGLConsoleControls(Source).FDblClickDelay;
  end;
end;

constructor TGLConsoleControls.Create(AOwner: TPersistent);
begin
  FOwner := AOwner;

  FNavigateUp := VK_HOME;
  FNavigateDown := VK_END;
  FNavigatePageUp := VK_PRIOR;
  FNavigatePageDown := VK_NEXT;
  FNextCommand := VK_DOWN;
  FPreviousCommand := VK_UP;
  FAutoCompleteCommand := VK_CONTROL;

  FDblClickDelay := 300;
end;

function TGLConsoleControls.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

//---------------------------
// TGLConsoleCommand 
//---------------------------

procedure TGLConsoleCommand.Assign(Source: TPersistent);
begin
  Assert(Source <> nil);
  inherited;
  SetCommandName(TGLConsoleCommand(Source).FCommandName);
  FShortHelp := TGLConsoleCommand(Source).FShortHelp;
  FLongHelp.Assign(TGLConsoleCommand(Source).FLongHelp);

  FVisible := TGLConsoleCommand(Source).FVisible;
  FEnabled := TGLConsoleCommand(Source).FEnabled;
  FSilentDisabled := TGLConsoleCommand(Source).FSilentDisabled;
end;

constructor TGLConsoleCommand.Create(Collection: TCollection);
begin
  inherited;
  Assert((Collection is TGLConsoleCommandList) or (Collection = nil));

  FCommandList := TGLConsoleCommandList(Collection);
  FLongHelp := TStringList.Create;

  FVisible := True;
  FEnabled := True;
end;

destructor TGLConsoleCommand.Destroy;
begin
  FLongHelp.Destroy;
  inherited;
end;

procedure TGLConsoleCommand.ShowInvalidUseOfCommandError;
begin
  FCommandList.FConsole.AddLine('   - Invalid use of command!');
end;

procedure TGLConsoleCommand.ShowInvalidNumberOfArgumentsError(const
  ShowHelpAfter: Boolean);
begin
  FCommandList.FConsole.AddLine('   - Invalid number of arguments!');
  if ShowHelpAfter then
    ShowHelp;
end;

procedure TGLConsoleCommand.SetCommandName(const Value: string);
begin
  //the name must be unique
  if FCommandList.CommandExists(Value) or
    FCommandList.FConsole.FAdditionalCommands.CommandExists(Value) then
  begin
    raise EGLConsoleException.Create(STR_NO_DUPLICATE_NAMES_ALLOWED);
    Exit;
  end;

  FCommandName := Value;
end;

procedure TGLConsoleCommand.ShowHelp;
var
  I: Integer;
begin
  if Assigned(FOnHelp) then
    FOnHelp(Self)
  else if FLongHelp.Count <> 0 then
    for I := 0 to FLongHelp.Count - 1 do
      FCommandList.FConsole.AddLine('   - ' + FLongHelp[I]);
end;

procedure TGLConsoleCommand.DoOnCommand(var UserInputCommand:
  TGLUserInputCommand);
begin
  Assert(Assigned(FOnCommand));
  if FEnabled then
    FOnCommand(Self, FCommandList.FConsole, UserInputCommand)
  else
  begin
    if not FSilentDisabled then
      FCommandList.FConsole.AddLine(' - Command "' + FCommandName +
        '" has been disabled!');
  end;
end;

procedure TGLConsoleCommand.ShowShortHelp;
begin
  if FVisible then
    FCommandList.FConsole.AddLine('   - ' + FCommandName + ' - ' + FShortHelp);
end;

function TGLConsoleCommand.GetDisplayName: string;
begin
  if FCommandName = '' then
    Result := inherited GetDisplayName
  else
    Result := FCommandName;
end;

//---------------------------
// TGLConsoleCommandList
//---------------------------

function TGLConsoleCommandList.Add: TGLConsoleCommand;
begin
  Result := TGLConsoleCommand(inherited Add);
end;

constructor TGLConsoleCommandList.Create(const AOwner: TGLCustomConsole);
begin
  Assert(AOwner <> nil);
  FConsole := TGLCustomConsole(AOwner);
  inherited Create(TGLConsoleCommand);
end;

destructor TGLConsoleCommandList.Destroy;
begin
  Clear;
  inherited;
end;

function TGLConsoleCommandList.GetItems(const Index: Integer):
  TGLConsoleCommand;
begin
  Result := TGLConsoleCommand(inherited Items[Index]);
end;

function TGLConsoleCommandList.LastConsoleCommand: TGLConsoleCommand;
begin
  Result := GetItems(Count - 1);
end;

procedure TGLConsoleCommandList.SortCommands(const Ascending: Boolean);
begin
  Assert(False, 'Not implemented yet....');
end;

function TGLConsoleCommandList.CommandExists(const Command: string): Boolean;
var
  I: Integer;
begin
  Result := True;
  if Count <> 0 then
    for I := 0 to Count - 1 do
      if GetItems(I).FCommandName = Command then
        Exit;
  Result := False;
end;

function TGLConsoleCommandList.GetCommandIndex(const Command: string): Integer;
begin
  if Count <> 0 then
    for Result := 0 to Count - 1 do
      if GetItems(Result).FCommandName = Command then
        Exit;

  Result := -1;
end;

function TGLConsoleCommandList.GetOwner: TPersistent;
begin
  Result := FConsole;
end;

//---------------------------
// TGLConsoleStringList
//---------------------------

procedure TGLConsoleStringList.Changed;
begin
  inherited;
  //we'll just assume that user added a command and check it,
  //other cases are not dealt with

  if Count = 0 then
    Exit;

  //check if this command does not duplicate any existing
  if FConsole.FCommands.CommandExists(Strings[Count - 1]) then
    Delete(Count - 1);
end;

function TGLConsoleStringList.CommandExists(const Command: string): Boolean;
begin
  Result := IndexOf(Command) <> -1;
end;

constructor TGLConsoleStringList.Create(const Owner: TGLCustomConsole);
begin
  Assert(Owner <> nil);
  Duplicates := dupError;
  FConsole := Owner;
end;

function TGLConsoleStringList.GetOwner: TPersistent;
begin
  Result := FConsole;
end;

//---------------------------
initialization
//---------------------------

  RegisterClasses([TGLCustomConsole, TGLConsole, TGLConsoleStringList,
    TGLConsoleCommand, TGLConsoleCommandList, TGLConsoleControls]);

end.

