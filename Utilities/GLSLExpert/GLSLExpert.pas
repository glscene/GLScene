// GLSLExpert
{: Egg<p>

   GLSLExpert expert registration and bindings<p>

   <b>Historique : </b><font size=-1><ul>
      <li>14/02/05 - Egg - Creation
   </ul></font>
}
unit GLSLExpert;

interface

uses Windows, Classes, ToolsAPI, Menus, SysUtils;

type

   // TGLSLWizard
   //
   TGLSLWizard = class(TNotifierObject, IOTAMenuWizard, IOTAWizard)
      public
         // IOTAWizard
         function GetIDString : String;
         function GetName : String;
         function GetState : TWizardState;
         procedure Execute;

         // IOTAMenuWizard
         function GetMenuText : String;
   end;

   // TGLSLKeyBinding
   //
   TGLSLKeyBinding = class (TNotifierObject, IOTAKeyboardBinding)
      procedure KeyProcValidate(const Context: IOTAKeyContext; KeyCode: TShortcut; var BindingResult: TKeyBindingResult);

      procedure BindKeyboard(const BindingServices: IOTAKeyBindingServices);
      function GetBindingType : TBindingType;
      function GetDisplayName : String;
      function GetName : String;
   end;

procedure Register;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses Dialogs;

var
   vKeyBindingIndex : Integer;
   vValidateShortCut : TShortCut = 24662; // Ctrl+Shift+V

procedure Register;
begin
   RegisterPackageWizard(TGLSLWizard.Create);
   vKeyBindingIndex:=(BorlandIDEServices as IOTAKeyboardServices).AddKeyboardBinding(TGLSLKeyBinding.Create);
end;

procedure TGLSLWizard.Execute;
begin
   ShowMessage( 'GLSL Expert'#13#10#13#10
               +'Copyright 2005 - Eric Grange / GLScene.org');
end;

function TGLSLWizard.GetIDString: string;
begin
  Result := 'GLScene.GLSLExpert';
end;

function TGLSLWizard.GetMenuText: string;
begin
   Result := 'GLSL Expert';
end;

function TGLSLWizard.GetName : string;
begin
   Result := 'GLSLExpert';
end;

function TGLSLWizard.GetState : TWizardState;
begin
   Result:=[wsEnabled];
end;

procedure TGLSLKeyBinding.BindKeyboard(const BindingServices : IOTAKeyBindingServices);
begin
   BindingServices.AddKeyBinding([vValidateShortCut], KeyProcValidate, nil);
end;

function TGLSLKeyBinding.GetBindingType : TBindingType;
begin
   Result:=btPartial;
end;

function TGLSLKeyBinding.GetDisplayName : String;
begin
   Result:='GLSLExpert KeyBinding';
end;

function TGLSLKeyBinding.GetName : String;
begin
   Result:=GetDisplayName;
end;

// KeyProcValidate
//
procedure TGLSLKeyBinding.KeyProcValidate(const Context: IOTAKeyContext; KeyCode: TShortcut;
                                          var BindingResult: TKeyBindingResult);
var
   editorServices : IOTAEditorServices;
   msgServices : IOTAMessageServices;
   msgGroup : IOTAMessageGroup;
   editBuffer : IOTAEditBuffer;
   editPosition : IOTAEditPosition;
   i, p, colNb, lineNb : Integer;
   currentDirBackup : String;
   fileName, cmdLine, logFileName : String;
   errPrefix, errLine : String;
   lineRef : Pointer;
   log : TStringList;
begin
   BindingResult:=krHandled;
   editorServices:=(BorlandIDEServices as IOTAEditorServices);
   msgServices:=(BorlandIDEServices as IOTAMessageServices);
   editBuffer:=editorServices.TopBuffer;
   if Assigned(editBuffer) and (not editBuffer.IsReadOnly) then begin
      editPosition:=editBuffer.EditPosition;
      fileName:=editBuffer.FileName;
      cmdLine:='"C:\Program Files\3Dlabs\GLSL Validate\glslvalidate.exe" ';
      if CompareText(ExtractFileExt(fileName), '.glsl')<>0 then
         Exit // not a GLSL file
      else if Pos('_fp.glsl', LowerCase(fileName))>0 then begin
         cmdLine:=cmdLine+'/f ';
         logFileName:='fragment.log';
      end else if Pos('_vp.glsl', LowerCase(fileName))>0 then begin
         cmdLine:=cmdLine+'/v ';
         logFileName:='vertex.log';
      end else begin
         ShowMessage( 'Couldn''t determine if this was a vertex or fragment program.'#13#10
                     +'Please terminate your filename with "_fp.glsl" or "_vp.glsl",'#13#10
                     +'or contribute better detection logic code ;)');
         Exit;
      end;

      if editBuffer.IsModified then
         (BorlandIDEServices as IOTAActionServices).SaveFile(fileName);

      cmdLine:=cmdLine+'"'+fileName+'"';
      currentDirBackup:=GetCurrentDir;
      SetCurrentDir(ExtractFilePath(fileName));
      WinExec(PAnsiChar(AnsiString(cmdLine)), SW_HIDE);
      SetCurrentDir(currentDirBackup);
      logFileName:=ExtractFilePath(fileName)+logFileName;
      if FileExists(logFileName) then begin
         log:=TStringList.Create;
         try
            msgGroup:=msgServices.GetGroup('GLSL');
            if msgGroup=nil then
               msgGroup:=msgServices.AddMessageGroup('GLSL')
            else msgServices.ClearToolMessages(msgGroup);

            log.LoadFromFile(logFileName);
            if log.Count=0 then
               msgServices.AddToolMessage(fileName, 'Validation failed for some undetermined reason...',
                                          'GLSL', 0, 0, nil, lineRef, msgGroup)
            else if CompareText(log[0], 'Success.')=0 then
               msgServices.AddToolMessage(fileName, 'Validation passed successfully!',
                                          'GLSL', 0, 0, nil, lineRef, msgGroup)
            else begin
               // we got error dude
               for i:=1 to log.Count-1 do begin
                  p:=Pos(':', log[i]);
                  if p>0 then begin
                     errPrefix:=Copy(log[i], 1, p-1);
                     errLine:=Copy(log[i], p+1, MaxInt);
                     p:=Pos(':', errLine);
                     colNb:=StrToIntDef(Copy(errLine, 1, p-1), 0);
                     errLine:=Copy(errLine, p+1, MaxInt);
                     p:=Pos(':', errLine);
                     lineNb:=StrToIntDef(Copy(errLine, 1, p-1), 0);
                     errLine:=Copy(errLine, p+1, MaxInt);
                     msgServices.AddToolMessage(fileName, errLine, errPrefix, lineNb, colNb,
                                                nil, lineRef, msgGroup);
                  end;
               end;
            end;
            msgServices.ShowMessageView(msgGroup);
         finally
            log.Free;
         end;
         DeleteFile(logFileName);
      end else ShowMessage('Couldn''t find validation log file!');
   end;
end;

end.
