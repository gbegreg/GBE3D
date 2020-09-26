unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Objects, FMX.Viewport3D, GBEViewport3D, FMX.Types3D,
  System.Math.Vectors, FMX.Objects3D, FMX.Controls3D, FMX.Ani,
  FMX.MaterialSources, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Effects, uUtils,
  GBEJoystick, GBEPlayerPosition, GBEHeightmap, GBEPlaneExtend,
  GBECubeExtend, FMX.Filter.Effects, System.ImageList, FMX.ImgList, Generics.Collections,
  GBECubemap, dateUtils, FMX.Media, FMX.ListBox, system.IOUtils, uGBEOptionsUtils, inifiles;

type
  TfMain = class(TForm)
    layIntro: TLayout;
    layJeu: TLayout;
    imgIntro: TImage;
    layIntroGauche: TLayout;
    layIntroDroite: TLayout;
    viewportIntro: TGBEViewport3D;
    Light1: TLight;
    sVirus: TSphere;
    Cylinder1: TCylinder;
    Cylinder2: TCylinder;
    Cylinder3: TCylinder;
    Cylinder4: TCylinder;
    aniRotationIntro: TFloatAnimation;
    Cylinder5: TCylinder;
    Cylinder6: TCylinder;
    Cylinder7: TCylinder;
    Cylinder8: TCylinder;
    lmsVirus: TLightMaterialSource;
    lmsVirusAntennes: TLightMaterialSource;
    Cylinder9: TCylinder;
    Cylinder10: TCylinder;
    layMenu: TLayout;
    layMenuCentre: TLayout;
    rJouer: TRectangle;
    lblJouer: TLabel;
    GlowEffect2: TGlowEffect;
    aniSautIntro: TFloatAnimation;
    rAide: TRectangle;
    lblAide: TLabel;
    GlowEffect1: TGlowEffect;
    aniPrincipale: TFloatAnimation;
    Rectangle1: TRectangle;
    lblInfo: TLabel;
    GBEViewport3D1: TGBEViewport3D;
    layDroit: TLayout;
    joyDeplacement: TGBEJoystick;
    Light2: TLight;
    lmsTextureSol: TLightMaterialSource;
    textureOcean: TLightMaterialSource;
    dmyMonde: TDummy;
    sol: TGBEHeightmap;
    ocean: TGBEPlaneExtend;
    PlayerPosition: TGBEPlayerPosition;
    cameraCarte: TCamera;
    layGauche: TLayout;
    joyOrientation: TGBEJoystick;
    Image1: TImage;
    layEcran: TLayout;
    imgViseur: TImage;
    ImageListButtons: TImageList;
    recIHM: TRectangle;
    btnQuitter: TButton;
    cmsBalle: TColorMaterialSource;
    layInfo: TLayout;
    lblMunition: TLabel;
    MediaPlayerSons: TMediaPlayer;
    recCadreVie: TRectangle;
    recVie: TRectangle;
    layGameOver: TLayout;
    recGameOver: TRectangle;
    Image2: TImage;
    Layout3: TLayout;
    Layout4: TLayout;
    rRetour: TRectangle;
    Label2: TLabel;
    GlowEffect4: TGlowEffect;
    dmyEnnemis: TDummy;
    Virus1: TSphere;
    Cylinder11: TCylinder;
    Cylinder12: TCylinder;
    Cylinder13: TCylinder;
    Cylinder14: TCylinder;
    Cylinder15: TCylinder;
    Cylinder16: TCylinder;
    Cylinder17: TCylinder;
    Cylinder18: TCylinder;
    Cylinder19: TCylinder;
    Cylinder20: TCylinder;
    GlowEffect5: TGlowEffect;
    imgCarte: TImage;
    lmsVirusTouche1: TLightMaterialSource;
    lmsVirusTouche2: TLightMaterialSource;
    dmyBonus: TDummy;
    lmsMasque: TLightMaterialSource;
    layVictoire: TLayout;
    recVictoire: TRectangle;
    Image3: TImage;
    Layout2: TLayout;
    Layout5: TLayout;
    Rectangle3: TRectangle;
    Label1: TLabel;
    GlowEffect7: TGlowEffect;
    bonusMasque: TCube;
    lmsMunition: TLightMaterialSource;
    Pie1: TPie;
    tmsCubemap: TTextureMaterialSource;
    layAide: TLayout;
    recAide: TRectangle;
    Image6: TImage;
    Layout6: TLayout;
    Layout7: TLayout;
    recRetourAide: TRectangle;
    Label3: TLabel;
    GlowEffect8: TGlowEffect;
    layAideConsigne: TLayout;
    Label4: TLabel;
    layChrono: TLayout;
    lblChrono: TLabel;
    layEnnemi: TLayout;
    Image7: TImage;
    lblEnnemi: TLabel;
    Image8: TImage;
    layVictoireMessage: TLayout;
    lblVictoireMessage: TLabel;
    aniMortVirus: TFloatAnimation;
    MediaPlayerMusique: TMediaPlayer;
    Image5: TImage;
    tFPS: TTimer;
    lblFPS: TLabel;
    layConfig: TLayout;
    rConfig: TRectangle;
    Image4: TImage;
    Layout8: TLayout;
    Layout9: TLayout;
    Rectangle4: TRectangle;
    Label5: TLabel;
    GlowEffect3: TGlowEffect;
    Rectangle2: TRectangle;
    recConfig: TRectangle;
    Label6: TLabel;
    GlowEffect6: TGlowEffect;
    Label7: TLabel;
    Label8: TLabel;
    cbFiltre: TComboBox;
    cbDetailsSol: TComboBox;
    Label9: TLabel;
    cbAnimerMer: TCheckBox;
    Label10: TLabel;
    cbMusique: TCheckBox;
    Label11: TLabel;
    cbFPS: TCheckBox;
    Label12: TLabel;
    cbAfficherLignes: TCheckBox;
    cmsLignes: TColorMaterialSource;
    procedure FormCreate(Sender: TObject);
    procedure aniPrincipaleProcess(Sender: TObject);
    procedure rJouerClick(Sender: TObject);
    procedure btnQuitterClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure FormDestroy(Sender: TObject);
    procedure rRetourClick(Sender: TObject);
    procedure aniVirus1Process(Sender: TObject);
    procedure Rectangle3Click(Sender: TObject);
    procedure rAideClick(Sender: TObject);
    procedure joyOrientationMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure aniMortVirusProcess(Sender: TObject);
    procedure aniMortVirusFinish(Sender: TObject);
    procedure Image5Click(Sender: TObject);
    procedure tFPSTimer(Sender: TObject);
    procedure Rectangle4Click(Sender: TObject);
    procedure recConfigClick(Sender: TObject);
  private
    procedure afficherJeu;
    procedure afficherMenu;
    procedure CreerMonde;
    procedure GestionDeplacementJoueur;
    procedure ChargerMer;
    procedure tir;
    procedure GestionTir;
    procedure afficherNiveauVie;
    procedure afficherFinJeu;
    procedure ChargerEnnemis;
    function collisionEnnemi(balle: TSphere): boolean;
    procedure AfficherCarte;
    procedure ChargerBonus;
    procedure afficherVictoire;
    procedure collisionBonus;
    function Aleatoire: single;
    procedure afficherAide;
    procedure DeplacementVirus(Sender: TObject);
    procedure collisionEnnemis;
    procedure afficherOptions;
    procedure MasquerLayout;
    procedure PlaySound(mediaplayer: TMediaPlayer; son: string);
    { Déclarations privées }
  public
    { Déclarations publiques }
    scene : TSceneJeu;
    vitesse, limiteZoneJeuX, limiteZoneJeuY, demiHauteur, niveauVie, hauteurEnnemi : single;
    listeBalles : TTirList;
    nbBalles, nbEnnemisRestant, FPS : integer;
    toucheAppuyee : boolean;
    heureFin, heureDebut, heureTerminee, meilleurTemps : TTime;
    listeAnimation : TList<TFloatAnimation>;
    optionsJeu : TGBEOptions;
    configFile, repSons : string;
  end;

var
  fMain: TfMain;

implementation
uses uGBEUtils3D, uGBEImageUtils;

{$R *.fmx}

procedure TfMain.aniMortVirusFinish(Sender: TObject);
begin
  (aniMortVirus.parent as TControl3D).Visible := false;
  aniMortVirus.Parent := fMain;
end;

procedure TfMain.aniMortVirusProcess(Sender: TObject);
begin
  (aniMortVirus.parent as TControl3D).Scale.X := (aniMortVirus.parent as TControl3D).Scale.X - 0.05;
  (aniMortVirus.parent as TControl3D).Scale.Y := (aniMortVirus.parent as TControl3D).Scale.Y - 0.05;
  (aniMortVirus.parent as TControl3D).Scale.Z := (aniMortVirus.parent as TControl3D).Scale.Z - 0.05;
end;

procedure TfMain.aniPrincipaleProcess(Sender: TObject);
begin
  case scene of
    menu: afficherMenu;
    jeu: afficherJeu;
    gameover: afficherFinJeu;
    victoire: afficherVictoire;
    aide: afficherAide;
    options: afficherOptions;
  end;
  GBEViewport3D1.Repaint;
end;

procedure TfMain.btnQuitterClick(Sender: TObject);
begin
  scene := TSceneJeu.menu;
end;

procedure TfMain.Rectangle3Click(Sender: TObject);
begin
  scene := TSceneJeu.menu;
end;

procedure TfMain.Rectangle4Click(Sender: TObject);
begin
  case cbFiltre.ItemIndex of
    0: GBEViewport3D1.Multisample := TMultisample.None;
    1: GBEViewport3D1.Multisample := TMultisample.TwoSamples;
    2: GBEViewport3D1.Multisample := TMultisample.FourSamples;
  end;
  optionsJeu.afficherLignes := cbAfficherLignes.IsChecked;
  optionsJeu.activerMusiques := cbMusique.IsChecked;
  optionsJeu.activerVagues := cbAnimerMer.IsChecked;
  optionsJeu.afficherFPS := cbFPS.IsChecked;
  optionsJeu.detailsHeightmap := cbDetailsSol.ItemIndex;
  optionsJeu.filtre := GBEViewport3D1.Multisample;
  optionsJeu.sauverConfig(configFile);
  scene := TSceneJeu.menu;
end;

procedure TfMain.rAideClick(Sender: TObject);
begin
  scene := TSceneJeu.aide;
end;

procedure TfMain.recConfigClick(Sender: TObject);
begin
  scene := TSceneJeu.options;
end;

procedure TfMain.aniVirus1Process(Sender: TObject);
begin
  Virus1.Position.y := sol.GetHeight(Virus1.Position.Point) + hauteurEnnemi;
end;

procedure TfMain.FormCreate(Sender: TObject);
var
  ficini : TInifile;
begin
  scene := TSceneJeu.menu;
  configFile := TPath.GetHomePath + PathDelim + 'stopcovid10.cfg';
  repSons := '.'+PathDelim+'sons'+PathDelim;
  if not(FileExists(configFile)) then begin
    cbFiltre.ItemIndex := 2;
    cbDetailsSol.ItemIndex := 0;
    cbAnimerMer.IsChecked := true;
    cbFPS.IsChecked := false;
    cbAfficherLignes.IsChecked := false;
    cbMusique.IsChecked := true;
    meilleurTemps := incMinute(now, 10) - now;
    Rectangle4Click(sender);
  end else begin
    optionsJeu.chargerConfig(configFile);
    case optionsJeu.filtre of
      TMultisample.None: cbFiltre.ItemIndex := 0;
      TMultisample.TwoSamples: cbFiltre.ItemIndex := 1;
      TMultisample.FourSamples: cbFiltre.ItemIndex := 2;
    end;
    cbDetailsSol.ItemIndex := optionsJeu.detailsHeightmap;
    cbAnimerMer.IsChecked := optionsJeu.activerVagues;
    cbMusique.IsChecked := optionsJeu.activerMusiques;
    cbFPS.IsChecked := optionsJeu.afficherFPS;
    cbAfficherLignes.IsChecked := optionsJeu.afficherLignes;
    ficini := TInifile.Create(configFile);
    meilleurTemps := ficini.ReadTime('RECORD','meilleurTemps', incMinute(now, 10) - now);
    ficini.Free;
  end;;

  FPS := 0;
  listeAnimation := TList<TFloatAnimation>.create;
  masquerLayout;
  joyDeplacement.Width := 0;
  joyOrientation.Width := 0;
  GBEViewport3D1.HitTest := false;
  with TGBECubeMap.Create(dmyMonde) do begin
    MaterialSource := tmsCubemap;
    width := Sol.Width*1.5;
    height := width;
    depth := width;
    parent := dmyMonde;
    TwoSide := true;
    hittest := false;
    position.X := 0;
    position.Y := 0;
    position.Z := 0;
    rotationangle.Y := 180;
  end;
  cameraCarte.Position.Z := 0;
  GBEViewport3D1.DoAddView(cameraCarte);
  listeBalles := TTirList.Create;
  joyDeplacement.deplacement := Point3D(-1,1,1);
  aniPrincipale.Start;
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  listeBalles.Free;
  listeAnimation.free;
end;

procedure TfMain.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if toucheAppuyee then exit;

  toucheAppuyee := true;
  GBEViewport3D1.BeginUpdate;
  if scene = TSceneJeu.jeu then begin
    if keyChar = ' ' then tir;
    if keyChar = 'a' then PlayerPosition.getDummyOrientation.RotationAngle.X := PlayerPosition.getDummyOrientation.RotationAngle.X + 0.5;;
    if keyChar = 'e' then PlayerPosition.getDummyOrientation.RotationAngle.X := PlayerPosition.getDummyOrientation.RotationAngle.X - 0.5;;
    if keyChar = 'q' then PlayerPosition.getDummyOrientation.RotationAngle.Y := PlayerPosition.getDummyOrientation.RotationAngle.Y - 0.5;
    if key = 27 then vitesse := 0;
    if keyChar = 'z' then begin
      if abs(vitesse) <= vitesseMax then vitesse := vitesse - 0.05
      else vitesse := -vitesseMax;
    end;
    if keyChar = 'd' then PlayerPosition.getDummyOrientation.RotationAngle.Y := PlayerPosition.getDummyOrientation.RotationAngle.Y + 0.5;
    if keyChar = 's' then begin
      if vitesse <= vitesseMax then vitesse := vitesse + 0.05
      else vitesse := vitesseMax;
    end;

    interactionIHM(GBEViewport3D1);
  end;
  GBEViewport3D1.EndUpdate;
  toucheAppuyee := false;
end;

procedure TfMain.rJouerClick(Sender: TObject);
begin
  scene := TSceneJeu.jeu;
end;

procedure TfMain.rRetourClick(Sender: TObject);
begin
  scene := TSceneJeu.menu;
end;

procedure TfMain.afficherMenu;
begin
  if not(layIntro.Visible) then begin
    tFPS.Enabled := false;
    MasquerLayout;
    layIntro.Visible := true;
    aniRotationIntro.Start;
    aniSautIntro.Start;
    PlaySound(MediaPlayerMusique, repSons+'Deus Ex Tempus.mp3');
  end;
end;

procedure TfMain.afficherJeu;
begin
  if not(layJeu.Visible) then begin
    randomize;
    tFPS.Enabled := cbFPS.IsChecked;
    lblFPS.Text := '';
    nbBalles := 10;
    niveauVie := 100;
    vitesse := 0;
    sol.ShowLines := cbAfficherLignes.IsChecked;
    ocean.ShowLines := cbAfficherLignes.IsChecked;
    hauteurEnnemi := (virus1.Height+2) * 0.5;
    MasquerLayout;
    layJeu.Visible := true;
    toucheAppuyee := false;
    creerMonde;
    nbEnnemisRestant := dmyEnnemis.ChildrenCount;
    heureFin := incMinute(now,10);
    heureDebut := now;
    PlaySound(MediaPlayerMusique, repSons+'Deus Ex Tempus.mp3');
    aniRotationIntro.Stop;
    aniSautIntro.Stop;
  end;

  if MediaPlayerMusique.State = TMediaState.Stopped then begin
    MediaPlayerMusique.Stop;
    MediaPlayerMusique.CurrentTime := 0;
    MediaPlayerMusique.Play;
  end;

  inc(FPS);
  gestionDeplacementJoueur;
  collisionBonus;
  collisionEnnemis;
  afficherNiveauVie;
  if niveauVie <= 0 then begin
    PlaySound(MediaPlayerSons, repSons+'Powerup3.mp3');
    scene := TSceneJeu.gameover;
  end;
  if nbEnnemisRestant = 0  then begin
    heureTerminee := now;
    scene := TSceneJeu.victoire;
  end
  else lblEnnemi.Text := 'X '+ nbEnnemisRestant.ToString;

  if imgCarte.Visible then afficherCarte;
end;

procedure TfMain.afficherAide;
begin
  label4.Text := 'Vous devez désinfecter l''île des virus covid-10.'+#13#10+
                 'Pour ce faire votre seringue vous permet de tirer un vaccin sur les virus. La seringue dispose d''un réservoir de 10 doses. Pour venir à bout d''un virus, vous devrez lui envoyer 3 doses de vaccin.'+#13#10+#13#10+
                 'Attention : si vous restez immobiles dans ce lieu infecté, vous perdrez de l''énergie. Il faut donc se déplacer pour rester en forme.'+#13#10+#13#10+
                 'Toutefois des bonus représentés sous forme de caisses vous permettront de refaire le plein de la seringue (les caisses bleues) ou de refaire le plein d''énergie (les caisses roses).'+#13#10+#13#10+
                 'Vous serez victorieux si vous réussissez à éliminer tous les virus de l''ile en moins de 10 minutes.'+#13#10+#13#10+
                 'Pour vous déplacer, vous pouvez utiliser les joysticks à la souris ou en tactile ou les touches du clavier :'+#13#10+
                 ' - touches Q et D : pour tourner;'#13#10+
                 ' - Z et S : pour accélérer/freiner;'#13#10+
                 ' - ESCAPE : pour s''arrêter;'#13#10+
                 ' - ESPACE : pour tirer;'#13#10+
                 ' - A : pour orienter le viseur vers le haut;'+#13#10+
                 ' - E : pour orienter le viseur vers le bas.';
  MasquerLayout;
  layAide.Visible := true;
end;

procedure TfMain.afficherOptions;
begin
  if not(layConfig.Visible) then begin
    MasquerLayout;
    layConfig.Visible := true;
  end;
end;

procedure TfMain.afficherNiveauVie;
begin
  recVie.Width :=  niveauVie * maxVie /100;
  if niveauVie < 30 then begin
    if (round(niveauVie) mod 3) = 0 then GlowEffect5.Enabled := true
    else GlowEffect5.Enabled := false;
  end
  else GlowEffect5.Enabled := false;
  lblMunition.Text := 'Munitions : '+ nbBalles.ToString;
  lblChrono.Text := formatdatetime('nn:ss', heureFin -now);
  if heurefin-now < 0 then begin
    PlaySound(MediaPlayerSons, repSons+'Powerup3.mp3');
    scene := TSceneJeu.gameover;
  end;
end;

procedure TfMain.afficherFinJeu;
begin
  MasquerLayout;
  tFPS.Enabled := false;
  layGameOver.Visible := true;
end;

procedure TfMain.afficherVictoire;
var
  ficini : Tinifile;
begin
  MasquerLayout;
  tFPS.Enabled := false;
  if (heureTerminee - heureDebut) < meilleurTemps then begin
    meilleurTemps := heureTerminee - heureDebut;
    ficini := TInifile.Create(configFile);
    ficini.WriteTime('RECORD','meilleurTemps', meilleurTemps);
    ficini.Free;
  end;
  lblVictoireMessage.Text := 'Félicitations !!! Vous avez réussi en ' + formatdatetime('nn:ss', heureTerminee - heureDebut) +#13#10+
                             'Votre record est de '+formatDateTime('nn:ss',meilleurTemps)+'.';
  layVictoire.Visible := true;
end;

procedure TfMain.CreerMonde;
begin
  GBEViewport3D1.Camera := PlayerPosition.getCamera;
  GBEViewport3D1.UsingDesignCamera := false;
  limiteZoneJeuX := ocean.Width * 0.5;
  limiteZoneJeuY := ocean.height * 0.5;
  demiHauteur := PlayerPosition.Height * 0.5;
  case cbDetailsSol.ItemIndex of
    0: sol.loadHeightmapFromResource('heightmap32');
    1: sol.loadHeightmapFromResource('heightmap64');
    2: sol.loadHeightmapFromResource('heightmap128');
    3: sol.loadHeightmapFromResource('heightmap256');
  end;
  ChargerMer;
  ChargerEnnemis;
  ChargerBonus;
  PlayerPosition.Position.Point := Point3D(170,sol.GetHeight(Point3D(170, 0, 210)) + tailleJoueur + demiHauteur, 210);
end;

procedure TfMain.tFPSTimer(Sender: TObject);
begin
  lblFPS.Text := FPS.ToString;
  FPS := 0;
end;

procedure TfMain.tir;
var balle : TTir;
begin
  if nbBalles > 0 then begin
    PlaySound(MediaPlayerSons, repSons+'tir.mp3');
    balle := TTir.Create;
    balle.Balle := TSphere.Create(nil);
    balle.Balle.Parent := sol;
    balle.VitesseTir := 1.2;
    balle.DistanceTir := 100;
    balle.balle.MaterialSource := cmsBalle;
    balle.Balle.RotationAngle.X := 90;
    balle.Balle.Width := 0.5;
    balle.Balle.Depth := 0.5;
    balle.Balle.height := 0.5;
    balle.PositionDepart := PlayerPosition.Position.Point;
    balle.Direction := joyDeplacement.direction * balle.VitesseTir + joyOrientation.direction * balle.VitesseTir;
    balle.Balle.Position.Point := PlayerPosition.Position.point;
    listeBalles.Add(balle);
    dec(nbBalles);
    lblMunition.Text := 'Munitions : '+ nbBalles.ToString;
  end else begin
    PlaySound(MediaPlayerSons, repSons+'Blip.mp3');
  end;
end;

procedure TfMain.GestionDeplacementJoueur;
var resultat : TGBECollisionRetour;
begin
  GBEViewport3D1.BeginUpdate;
  if abs(vitesse) <= vitesseMax then vitesse := vitesse + joyDeplacement.Acceleration/2000;

  PlayerPosition.NextPosition.Position.point := PlayerPosition.Position.Point - joyDeplacement.direction * vitesse;
  PlayerPosition.NextPosition.position.Y := sol.GetHeight(PlayerPosition.NextPosition.Position.point) + tailleJoueur + demiHauteur;

  if (abs(PlayerPosition.NextPosition.position.Point.x) < limiteZoneJeuX) and
     (abs(PlayerPosition.NextPosition.position.Point.z) < limiteZoneJeuY) then begin
    resultat := DetectionCollisionObstacle(sol, PlayerPosition.NextPosition);
    if not(resultat.bool) then begin
       PlayerPosition.Position.point := PlayerPosition.NextPosition.position.Point;
    end;
  end
  else vitesse := 0;

  gestionTir;
  if vitesse = 0 then niveauVie := niveauVie - 0.5;
  GBEViewport3D1.EndUpdate;
end;

procedure TfMain.ChargerMer;
begin
  ocean.Origine := Point3D(ocean.Width/2,0,ocean.height/2);
  ocean.Opacity := 0.6;
  ocean.Amplitude := 5;
  ocean.Longueur := 0.5;
  ocean.ActiveWaves := cbAnimerMer.IsChecked;
  ocean.UseTasks := false;
end;

procedure TfMain.GestionTir;
var balle : TTir;
    i : integer;
    balleADetruire : boolean;
begin
  for i := listeBalles.Count -1 downto 0 do begin
    balleADetruire := false;
    balle := listeBalles[i];
    balle.Balle.Position.Point := balle.Balle.Position.Point + balle.Direction;
    if (balle.Balle.Position.Point.X > (balle.PositionDepart.X + balle.DistanceTir)) or
       (balle.Balle.Position.Point.X < (balle.PositionDepart.X - balle.DistanceTir)) or
       (balle.Balle.Position.Point.Z > (balle.PositionDepart.Z + balle.DistanceTir)) or
       (balle.Balle.Position.Point.Z < (balle.PositionDepart.Z - balle.DistanceTir)) then balleADetruire := true;

    if balle.Balle.Position.Y < sol.GetHeight(balle.Balle.Position.Point) then balleADetruire := true;

    if collisionEnnemi(balle.Balle) then begin
      balleADetruire := true;
    end;
    if balleADetruire then begin
      balle.Balle.Visible := false;
      listeBalles.Delete(i);
    end;
  end;
end;

procedure TfMain.Image5Click(Sender: TObject);
begin
  if imgCarte.Visible then begin
    imgCarte.Visible := false;
    Pie1.Visible := false;
  end
  else afficherCarte;
end;

procedure TfMain.joyOrientationMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if button = TMouseButton.mbRight then tir;
end;

function TfMain.collisionEnnemi(balle : TSphere): boolean;
var retour : TGBECollisionRetour;
    i : integer;
begin
  result := false;
  retour := collisionDummyChilds(dmyEnnemis, balle);

  if retour.bool then begin
    result := true;
    (retour.objet as TSphere).Tag := (retour.objet).Tag - 1;
    PlaySound(MediaPlayerSons, repSons+'Explosion.mp3');
    case (retour.objet as TSphere).tag of
      2 : (retour.objet as TSphere).MaterialSource := lmsVirusTouche1;
      1 : (retour.objet as TSphere).MaterialSource := lmsVirusTouche2;
      0 : begin
            for I := 0 to listeAnimation.Count-1 do begin
              if listeAnimation[i].Parent.Name = retour.objet.Name then begin
                listeAnimation[i].StopAtCurrent;
                break;
              end;
            end;
            aniMortVirus.Parent := (retour.objet as TSphere);
            aniMortVirus.Start;
            dec(nbEnnemisRestant);
          end;
    end;
  end;
end;

procedure TfMain.collisionBonus;
var retour : TGBECollisionRetour;
begin
  retour := collisionDummyChilds(dmyBonus, PlayerPosition);
  if retour.bool then begin
     PlaySound(MediaPlayerSons, repSons+'Powerup3.mp3');
     retour.objet.visible := false;
     case retour.objet.tag of
       1: niveauVie := 100;
       2: nbBalles := 10;
     end;
  end;
end;

procedure TfMain.collisionEnnemis;
var retour : TGBECollisionRetour;
begin
  retour := collisionDummyChilds(dmyEnnemis, PlayerPosition);
  if retour.bool then begin
    niveauVie := niveauVie - 0.7;
  end;
end;

procedure TfMain.ChargerBonus;
var i : integer;
begin
  bonusMasque.Position.Point := Point3D(random(360)-180,0, random(400)-200);
  bonusMasque.Position.Y := sol.GetHeight(bonusMasque.Position.Point) + bonusMasque.Height * 0.4;
  bonusMasque.RotationAngle.Y := random(360);
  bonusMasque.Parent := sol;
  dmyBonus.DeleteChildren;
  bonusMasque.Parent := dmyBonus;

  for i := 0 to nbBonus do begin
    with TCube(bonusMasque.Clone(nil)) do begin
      Position.Point := Point3D(random(360)-180,0, random(400)-200);
      Position.Y := sol.GetHeight(Position.Point) + Height * 0.4;
      RotationAngle.Y := random(360);
      parent := dmyBonus;
      tag := random(2)+1;
      case tag of
        1: MaterialSource := lmsMasque;
        else MaterialSource := lmsMunition;
      end;
    end;
  end;
end;

procedure TfMain.ChargerEnnemis;
var I: Integer;
    monAnimation : TFloatanimation;
begin
  Virus1.Position.Point := Point3D(random(360)-180,0, random(400)-200);
  Virus1.Position.Y := sol.GetHeight(Virus1.Position.Point) + hauteurEnnemi;
  Virus1.RotationAngle.Y := random(360);
  Virus1.Visible := true;
  virus1.Scale.X := 1;
  virus1.Scale.Y := 1;
  virus1.Scale.Z := 1;
  virus1.MaterialSource := lmsVirus;
  virus1.Tag := 3;
  virus1.Parent := Sol;
  dmyEnnemis.DeleteChildren;
  virus1.Parent := dmyEnnemis;

  for I := 2 to MaxEnnemis do begin
    with (Virus1.Clone(self) as TSphere) do begin
      parent := dmyEnnemis;
      Position.Point := Point3D(random(360)-180,0, random(400)-200);
      Position.Y := sol.GetHeight(Position.Point) + hauteurEnnemi;
      RotationAngle.Y := random(360);
      Visible := true;
      Scale.X := 1;
      Scale.Y := 1;
      Scale.Z := 1;
      MaterialSource := lmsVirus;
      name := 'virus'+i.ToString;
      Tag := 3;
    end;
  end;

  listeAnimation.Clear;
  for I := 0 to dmyEnnemis.ChildrenCount-1 do begin
    monAnimation := TFloatAnimation.Create(dmyEnnemis.Children[i]);
    monAnimation.Interpolation := TInterpolationType.Sinusoidal;
    monAnimation.AutoReverse := true;
    monAnimation.Loop := true;
    monAnimation.Duration := Aleatoire;
    monAnimation.OnProcess := DeplacementVirus;
    monAnimation.Parent := dmyEnnemis.Children[i];
    if i mod 2 = 0 then  monAnimation.propertyName := 'Position.X'
    else monAnimation.propertyName := 'Position.Z';
    monAnimation.StartValue := (dmyEnnemis.Children[i] as TSphere).Position.Point.X;
    monAnimation.StopValue := random(360)-180;
    monAnimation.Start;
    listeAnimation.Add(monAnimation);
  end;

  PlayerPosition.RotationAngle.X := 180;
  PlayerPosition.RotationAngle.Y := 0;
  PlayerPosition.RotationAngle.Z := 0;
end;

function TfMain.Aleatoire: single;
begin
  result := random(10)*6;
  while result = 0 do
    result := random(10)*6;
end;

procedure TfMain.AfficherCarte;
begin
  imgCarte.Bitmap := GBEViewport3D1.getBitmapFromView(cameraCarte);
  imgCarte.Visible := true;
  Pie1.Visible := true;
end;

procedure TfMain.DeplacementVirus(Sender: TObject);
begin
  if (sender is TFloatAnimation) then begin
    ((sender as TFloatAnimation).Parent as TSphere).Position.y := sol.GetHeight(((sender as TFloatAnimation).Parent as TSphere).Position.Point) + hauteurEnnemi;
  end;
end;

procedure TfMain.MasquerLayout;
begin
  layIntro.Visible := false;
  layConfig.Visible := false;
  layJeu.Visible := false;
  layGameOver.Visible := false;
  layVictoire.Visible := false;
  layAide.Visible := false;
end;

procedure TfMain.PlaySound(mediaplayer: TMediaPlayer; son : string);
begin
  if cbMusique.IsChecked then begin
    if mediaplayer.State = TMediaState.Playing then mediaplayer.Stop;
    mediaplayer.FileName := son;
    mediaplayer.Play;
  end else begin
    mediaplayer.Stop;
  end;
end;

end.
