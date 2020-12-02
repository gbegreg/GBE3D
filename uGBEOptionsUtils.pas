unit uGBEOptionsUtils;

interface

uses FMX.Types3D, System.IniFiles, System.SysUtils, System.classes;

type
  TGBEOptions = record
    afficherLignes, activerMusiques, activerSons, activerVagues, activerHerbe, activerHerbeVent,
    activerNuages, afficherFPS, utilisationTasks, pleinEcran : boolean;
    volumeSons, volumeMusiques : single;
    detailsHeightmap, nbNuages, nbHerbe, detailsVagues : integer;
    filtre : TMultisample;

    procedure sauverConfig(configFile : string);
    procedure sauverOption(configFile, section, option, value: string);
    procedure chargerConfig(configFile : string);
    function chargerOption(configFile, section, option: string): string;
  end;

implementation

{ TGBEOptions }

procedure TGBEOptions.chargerConfig(configFile : string);
var
  ficini : TInifile;
begin
  if fileexists(configFile) then begin
    ficini := TInifile.create(configFile);
    afficherLignes := ficini.ReadBool('OPTIONS','showLines',false);
    activerMusiques := ficini.ReadBool('OPTIONS','musics',false);
    activerSons := ficini.ReadBool('OPTIONS','sounds',false);
    activerVagues := ficini.ReadBool('OPTIONS','activeWaves',false);
    activerHerbe := ficini.ReadBool('OPTIONS','activeGrass',true);
    activerHerbeVent := ficini.ReadBool('OPTIONS','activeGrassWind',true);
    activerNuages := ficini.ReadBool('OPTIONS','activeClouds',true);
    afficherFPS := ficini.ReadBool('OPTIONS','showFPS',false);
    utilisationTasks := ficini.ReadBool('OPTIONS','useTasks',false);
    pleinEcran := ficini.ReadBool('OPTIONS','fullScreen',false);
    detailsHeightmap := ficini.ReadInteger('OPTIONS','detailsHeightmap',0);
    nbNuages := ficini.ReadInteger('OPTIONS','nbNuages',15);
    nbHerbe := ficini.ReadInteger('OPTIONS','nbHerbe',50);
    volumeSons := ficini.ReadFloat('OPTIONS','volumeSons',1);
    volumeMusiques := ficini.ReadFloat('OPTIONS','volumeMusiques',1);
    detailsVagues := ficini.ReadInteger('OPTIONS','detailsWaves',1);
    case ficini.ReadInteger('OPTIONS', 'filtre', 0) of
      0: filtre := TMultisample.None;
      1: filtre := TMultisample.TwoSamples;
      2: filtre := TMultisample.FourSamples;
    end;
    ficini.Free;
  end else begin
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
    filtre := TMultisample.None;
  end;
end;

procedure TGBEOptions.sauverConfig(configFile : string);
var
  ficini : TInifile;
begin
  ficini := TInifile.create(configFile);
  ficini.writeBool('OPTIONS','showLines',afficherLignes);
  ficini.writeBool('OPTIONS','musics',activerMusiques);
  ficini.writeBool('OPTIONS','sounds',activerSons);
  ficini.writeBool('OPTIONS','activeWaves',activerVagues);
  ficini.writeBool('OPTIONS','activeGrass',activerHerbe);
  ficini.writeBool('OPTIONS','activeGrassWind',activerHerbeVent);
  ficini.writeBool('OPTIONS','activeClouds',activerNuages);
  ficini.writeBool('OPTIONS','showFPS',afficherFPS);
  ficini.writeBool('OPTIONS','useTasks',utilisationTasks);
  ficini.writeBool('OPTIONS','fullScreen',pleinEcran);
  ficini.writeInteger('OPTIONS','detailsHeightmap',detailsHeightmap);
  ficini.writeInteger('OPTIONS','nbNuages',nbNuages);
  ficini.writeInteger('OPTIONS','nbHerbe',nbHerbe);
  ficini.writefloat('OPTIONS','volumeSons',volumeSons);
  ficini.writefloat('OPTIONS','volumeMusiques',volumeMusiques);
  ficini.writeInteger('OPTIONS','detailsWaves',detailsVagues);
  case filtre of
    TMultisample.None : ficini.WriteInteger('OPTIONS','filtre',0);
    TMultisample.TwoSamples : ficini.WriteInteger('OPTIONS','filtre',1);
    TMultisample.FourSamples : ficini.WriteInteger('OPTIONS','filtre',2);
  end;
  ficini.Free;
end;

procedure TGBEOptions.sauverOption(configFile, section, option, value: string);
var
  ficini : TInifile;
begin
  ficini := TInifile.create(configFile);
  ficini.writeString(section,option,value);
  ficini.Free;
end;

function TGBEOptions.chargerOption(configFile, section, option: string): string;
var
  ficini : TInifile;
begin
  ficini := TInifile.create(configFile);
  result := ficini.readString(section,option,'');
  ficini.Free;
end;

end.
