unit uGBEOptionsUtils;

interface

uses
  FMX.Types3D,
  System.IniFiles,
  System.SysUtils,
  System.Classes;

type
  TKeyboardType = (QWERTY, AZERTY);

  TGBEOptions = record
    afficherLignes, activerMusiques, activerSons, activerVagues, activerHerbe,
      activerHerbeVent, activerNuages, afficherFPS, utilisationTasks,
      pleinEcran: boolean;
    volumeSons, volumeMusiques: single;
    detailsHeightmap, nbNuages, nbHerbe, detailsVagues: integer;
    Filter: TMultisample;
    Keyboard: TKeyboardType;

    procedure WriteConfig(configFile: string);
    procedure SaveOption(configFile, section, option, value: string);
    procedure ReadConfig(configFile: string);
    function ReadOption(configFile, section, option: string): string;
  end;

implementation // ----------------------------------------------------------

// TGBEOptions

procedure TGBEOptions.ReadConfig(configFile: string);
var
  IniFile: TIniFile;
begin
  if fileexists(configFile) then
  begin
    IniFile := TInifile.create(configFile);
    afficherLignes := IniFile.ReadBool('OPTIONS', 'showLines', false);
    activerMusiques := IniFile.ReadBool('OPTIONS', 'musics', false);
    activerSons := IniFile.ReadBool('OPTIONS', 'sounds', false);
    activerVagues := IniFile.ReadBool('OPTIONS', 'activeWaves', false);
    activerHerbe := IniFile.ReadBool('OPTIONS', 'activeGrass', true);
    activerHerbeVent := IniFile.ReadBool('OPTIONS', 'activeGrassWind', true);
    activerNuages := IniFile.ReadBool('OPTIONS', 'activeClouds', true);
    afficherFPS := IniFile.ReadBool('OPTIONS', 'showFPS', false);
    utilisationTasks := IniFile.ReadBool('OPTIONS', 'useTasks', false);
    pleinEcran := IniFile.ReadBool('OPTIONS', 'fullScreen', false);
    detailsHeightmap := IniFile.ReadInteger('OPTIONS', 'detailsHeightmap', 0);
    nbNuages := IniFile.ReadInteger('OPTIONS', 'nbNuages', 15);
    nbHerbe := IniFile.ReadInteger('OPTIONS', 'nbHerbe', 50);
    volumeSons := IniFile.ReadFloat('OPTIONS', 'volumeSons', 1);
    volumeMusiques := IniFile.ReadFloat('OPTIONS', 'volumeMusiques', 1);
    detailsVagues := IniFile.ReadInteger('OPTIONS', 'detailsWaves', 1);
    case IniFile.ReadInteger('OPTIONS', 'Filter', 0) of
      0:
        Filter := TMultisample.None;
      1:
        Filter := TMultisample.TwoSamples;
      2:
        Filter := TMultisample.FourSamples;
    end;
    case IniFile.ReadInteger('OPTIONS', 'Keyboard', 0) of
      0:
        Keyboard := TKeyboardType.AZERTY;
      1:
        Keyboard := TKeyboardType.QWERTY;
    end;
    IniFile.Free;
  end
  else
  begin
    afficherLignes := false;
    activerMusiques := false;
    activerSons := false;
    activerVagues := true;
    activerHerbe := true;
    activerHerbeVent := true;
    activerNuages := true;
    afficherFPS := false;
    utilisationTasks := false;
    pleinEcran := false;
    detailsHeightmap := 0;
    nbNuages := 15;
    nbHerbe := 50;
    volumeSons := 1;
    volumeMusiques := 1;
    detailsVagues := 1;
    Filter := TMultisample.None;
  end;
end;

procedure TGBEOptions.WriteConfig(configFile: string);
var
  IniFile: TInifile;
begin
  IniFile := TInifile.create(configFile);
  IniFile.writeBool('OPTIONS', 'showLines', afficherLignes);
  IniFile.writeBool('OPTIONS', 'musics', activerMusiques);
  IniFile.writeBool('OPTIONS', 'sounds', activerSons);
  IniFile.writeBool('OPTIONS', 'activeWaves', activerVagues);
  IniFile.writeBool('OPTIONS', 'activeGrass', activerHerbe);
  IniFile.writeBool('OPTIONS', 'activeGrassWind', activerHerbeVent);
  IniFile.writeBool('OPTIONS', 'activeClouds', activerNuages);
  IniFile.writeBool('OPTIONS', 'showFPS', afficherFPS);
  IniFile.writeBool('OPTIONS', 'useTasks', utilisationTasks);
  IniFile.writeBool('OPTIONS', 'fullScreen', pleinEcran);
  IniFile.writeInteger('OPTIONS', 'detailsHeightmap', detailsHeightmap);
  IniFile.writeInteger('OPTIONS', 'nbNuages', nbNuages);
  IniFile.writeInteger('OPTIONS', 'nbHerbe', nbHerbe);
  IniFile.writefloat('OPTIONS', 'volumeSons', volumeSons);
  IniFile.writefloat('OPTIONS', 'volumeMusiques', volumeMusiques);
  IniFile.writeInteger('OPTIONS', 'detailsWaves', detailsVagues);
  case Filter of
    TMultisample.None:
      IniFile.writeInteger('OPTIONS', 'Filter', 0);
    TMultisample.TwoSamples:
      IniFile.writeInteger('OPTIONS', 'Filter', 1);
    TMultisample.FourSamples:
      IniFile.writeInteger('OPTIONS', 'Filter', 2);
  end;
  case Keyboard of
    QWERTY:
      IniFile.writeInteger('OPTIONS', 'Keyboard', 1);
    AZERTY:
      IniFile.writeInteger('OPTIONS', 'Keyboard', 0);
  end;
  IniFile.Free;
end;

procedure TGBEOptions.SaveOption(configFile, section, option, value: string);
var
  IniFile: TInifile;
begin
  IniFile := TInifile.create(configFile);
  IniFile.writeString(section, option, value);
  IniFile.Free;
end;

function TGBEOptions.ReadOption(configFile, section, option: string): string;
var
  IniFile: TInifile;
begin
  IniFile := TInifile.create(configFile);
  Result := IniFile.ReadString(section, option, '');
  IniFile.Free;
end;

end.
