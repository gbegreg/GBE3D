unit main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.ImageList, FMX.ImgList, FMX.Controls.Presentation, System.Math,
  FMX.StdCtrls, FMX.Layouts, FMX.Viewport3D, GBEViewport3D, FMX.Ani,
  FMX.ListBox, System.Math.Vectors, FMX.Controls3D, FMX.Objects3D,
  GBEJoystick, GBEPlayerPosition, FMX.Objects, FMX.Types3D, System.IOUtils, inifiles,
  GBEHeightmap, FMX.MaterialSources, GBEPlaneExtend, GBEClouds, FMX.Edit,
  FMX.EditBox, FMX.SpinBox, FMX.Effects, FMX.TabControl, frmSmartphone,
  FMX.Filter.Effects, GBEGrass;

type
  TSceneJeu = (intro, jeu);
  TfrmMain = class(TForm)
    GBEViewport3D: TGBEViewport3D;
    aniPrincipale, aniTexte: TFloatAnimation;
    StyleBook: TStyleBook;
    ImageListButtons: TImageList;
    joyDeplacement, joyOrientation: TGBEJoystick;
    GBEPlayerPosition: TGBEPlayerPosition;
    tFPS: TTimer;
    btnPlayPause, btnSmartphone, btnQuitter: TButton;
    GBEHeightmap1: TGBEHeightmap;
    lSoleil: TLight;
    GBEPlaneExtend1: TGBEPlaneExtend;
    GBEClouds1: TGBEClouds;
    TextureMaterialSource1, TextureMaterialSource2, TextureMaterialSource3: TTextureMaterialSource;
    recIHM: TRectangle;
    palmierModele, modelJoueur: TModel3D;
    ColorMaterialSource1: TColorMaterialSource;
    textureSol, textureOcean, palmierModeleMat21, palmierModeleMat01, palmierModeleMat11, modelJoueurMat01 : TLightMaterialSource;
    Image1, Image2, Image3, Image4: TImage;
    Label9, lblCollision: TLabel;
    GlowEffect1: TGlowEffect;
    dmyMonde, dmyObstacles, dmyArbreForCollisionDetection, dmySoleil: TDummy;
    cameraCarte: TCamera;
    layBas, layGauche, layIHM: TLayout;
    lblFPS: TText;
    FillRGBEffect1: TFillRGBEffect;
    modelMaison: TModel3D;
    dmyMaisonForCollisionDetection: TDummy;
    modelMaisonMat11: TLightMaterialSource;
    modelMaisonMat01: TLightMaterialSource;
    TextureMaterialSource: TTextureMaterialSource;
    dmyHerbe: TDummy;
    procedure aniPrincipaleProcess(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnOptionAppliquerClick(Sender: TObject);
    procedure tFPSTimer(Sender: TObject);
    procedure btnPlayPauseClick(Sender: TObject);
    procedure Image2Click(Sender: TObject);
    procedure btnSmartphoneClick(Sender: TObject);
    procedure btnQuitterClick(Sender: TObject);
  private
    procedure initialisation;
    procedure ReprendreJeu;
    procedure SauverConfig;
    procedure AppliquerConfig;
    procedure ChargerConfig;
    procedure PauseJeu;
    procedure SceneIntroduction;
    procedure SceneGame;
    procedure ChargerNuages;
    procedure ChargerMer;
    procedure ChargerVegetation;
    procedure ChargerHabitation;
    procedure preparationSceneGame;
    procedure preparationSceneIntroduction;
    procedure AfficherCarte;
    procedure SupprimerObstacles;
    procedure MettreAJourHauteurs;
    procedure PrendrePhoto(Sender: TObject);
    procedure genererHerbe(nombre : integer; premiereGeneration: boolean);
    procedure gestionHerbe;
    { Déclarations privées }
  public
    { Déclarations publiques }
    configFile : string;
    FPS, oldDetailsSol, distanceAffichageHerbe : integer;
    vitesse, demiDistanceAffichageHerbe : single;
    sceneJeu : TSceneJeu;
    preparationAFaire, gererHerbe : boolean;
    limiteZoneJeuX, limiteZoneJeuY : single;
    smartphone : TfSmartphone;
  end;

const
  tailleJoueur = 0.75; // Taille du joueur pour la vue FirstPerson
  vitesseMax = 0.2;    // Vitesse maxi de déplacement
  maxHerbe = 100;

var
  frmMain: TfrmMain;

implementation

uses uGBEUtils3D, uGBEImageUtils;

{$R *.fmx}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  {$IFDEF ANDROID}
     FullScreen := true; // Forcer en plein pour Android
  {$ENDIF}
  initialisation;
end;

procedure TfrmMain.Image2Click(Sender: TObject);
begin
  preparationAFaire := true;
  sceneJeu := TSceneJeu.jeu;
end;

procedure TfrmMain.aniPrincipaleProcess(Sender: TObject); // Boucle principale du jeu
begin
  if smartphone.cbFPS.IsChecked then inc(FPS);
  case sceneJeu of
    intro: SceneIntroduction;
    jeu: SceneGame;
  end;
end;

procedure TfrmMain.tFPSTimer(Sender: TObject);  // Pour calculer le FPS
begin
  if btnPlayPause.Tag = 0 then lblFPS.text := 'Pause'
  else lblFPS.Text := FPS.ToString;
  FPS := 0;
end;

procedure TfrmMain.initialisation;
begin
  randomize;
  smartphone := TfSmartphone.Create(layGauche);
  smartphone.Parent := layGauche;
  smartphone.Align := TAlignLayout.Center;
  smartphone.lblHour.Text := FormatDateTime('hh:nn', now);
  smartphone.btnOptionAppliquer.OnClick := btnOptionAppliquerClick;
  smartphone.imgPhoto.OnClick := PrendrePhoto;
  smartphone.recGPS.visible := false;
  smartphone.recAide.visible := false;
  smartphone.recOptions.visible := false;
  preparationAFaire := true;
  distanceAffichageHerbe := 30;
  demiDistanceAffichageHerbe := distanceAffichageHerbe * 0.5;
  vitesse := 0;
  joyDeplacement.height := 0;
  joyOrientation.height := 0;
  FPS := 0;
  configFile := TPath.GetHomePath + PathDelim + 'demogbe_island.cfg';
  sceneJeu := TSceneJeu.intro;
  preparationAFaire := true;
  chargerConfig;
  supprimerObstacles;
  ChargerVegetation;
  ChargerHabitation;
  GBEViewport3D.Camera := GBEPlayerPosition.getCamera;
  GBEViewport3D.UsingDesignCamera := false;
  GBEViewport3D.DoAddView(cameraCarte);
  GBEPlayerPosition.Position.Y := GBEHeightmap1.GetHeight(GBEPlayerPosition.Position.Point)-1;
  textureOcean.Texture := tileImage(textureOcean.Texture, 7, 7);
  textureSol.Texture := tileImage(textureSol.Texture, 10, 10);
  aniPrincipale.Start;
end;

procedure TfrmMain.ReprendreJeu;
begin
  btnPlayPause.ImageIndex := 6;
  btnPlayPause.Tag := 1;
  if sceneJeu = TSceneJeu.intro then image2.HitTest := true;
  aniPrincipale.StartFromCurrent := true;
  aniPrincipale.Start;
end;

procedure TfrmMain.PauseJeu;
begin
  btnPlayPause.ImageIndex := 5;
  btnPlayPause.Tag := 0;
  aniPrincipale.StopAtCurrent;
end;

procedure TfrmMain.btnOptionAppliquerClick(Sender: TObject);
begin
  appliquerConfig;
  reprendreJeu;
end;

procedure TfrmMain.PrendrePhoto(Sender: TObject);
var
  b : TBitmap;
begin
  b := TBitmap.Create(width, height);  // Création du TBitmap
  GBEViewport3D.Context.CopyToBitmap(b,Rect(0,0,width, Height));  // Permet de copier dans le TBitmap ce qui est affiché dans le viewport
  smartphone.image1.Visible := true;
  smartphone.recPhoto.Visible := true;
  smartphone.image3.Bitmap.Width := b.Width;
  smartphone.image3.Bitmap.height := b.Height;
  smartphone.image3.Bitmap.CopyFromBitmap(b);
  b.free;
end;

procedure TfrmMain.btnPlayPauseClick(Sender: TObject);
begin
  case btnPlayPause.tag of
    1: pauseJeu;
    0: reprendreJeu;
  end;
end;

procedure TfrmMain.btnQuitterClick(Sender: TObject);
begin
  close;
end;

procedure TfrmMain.btnSmartphoneClick(Sender: TObject);
begin
  if smartphone.Position.X = -layGauche.Width -layGauche.Margins.left then
  begin
    TAnimator.AnimateFloat(smartphone, 'Position.X', 0, 0.5);
    FillRGBEffect1.enabled := true;
  end
  else
  begin
    TAnimator.AnimateFloat(smartphone, 'Position.X', -layGauche.Width -layGauche.Margins.left, 0.5);
    FillRGBEffect1.Enabled := false;
  end;
end;

procedure TfrmMain.ChargerConfig;
var
  ficini : TInifile;
  I: Integer;
begin
  if fileExists(configFile) then begin
    ficini := TIniFile.Create(configFile);
    smartphone.cbAliasing.ItemIndex := ficini.ReadInteger('OPTIONS','aliasing',2);
    smartphone.cbFPS.IsChecked := ficini.ReadBool('OPTIONS','showFPS', false);
    if smartphone.cbFPS.IsChecked then recIHM.Width := 192
    else recIHM.Width := 144;

    smartphone.cbAfficherLignes.IsChecked := ficini.ReadBool('OPTIONS','showLines', false);
    case smartphone.cbAliasing.ItemIndex of
      0: GBEViewport3D.Multisample := TMultisample.None;
      1: GBEViewport3D.Multisample := TMultisample.TwoSamples;
      2: GBEViewport3D.Multisample := TMultisample.FourSamples;
    end;
    smartphone.sbNuages.Value := ficini.ReadFloat('OPTIONS', 'nbnuages', 15);
    GBEPlaneExtend1.SubdivisionsWidth := ficini.ReadInteger('OPTIONS','detailsOcean',30);;
    GBEPlaneExtend1.SubdivisionsHeight := ficini.ReadInteger('OPTIONS','detailsOcean',30);;
    case GBEPlaneExtend1.SubdivisionsWidth of
      10: smartphone.cbDetailsOcean.ItemIndex := 0;
      30: smartphone.cbDetailsOcean.ItemIndex := 1;
      90: smartphone.cbDetailsOcean.ItemIndex := 2;
    end;

    smartphone.cbTasks.IsChecked := ficini.ReadBool('OPTIONS','multitaches', true);
    smartphone.cbDetailsSol.ItemIndex := ficini.ReadInteger('OPTIONS','detailsSol', 0);
    case smartphone.cbDetailsSol.ItemIndex of
      0: GBEHeightmap1.loadHeightmapFromResource('heightmap32');
      1: GBEHeightmap1.loadHeightmapFromResource('heightmap64');
      2: GBEHeightmap1.loadHeightmapFromResource('heightmap128');
      3: GBEHeightmap1.loadHeightmapFromResource('heightmap256');
      4: GBEHeightmap1.loadHeightmapFromResource('heightmap512');
    end;
    smartphone.cbHerbe.IsChecked := ficini.ReadBool('OPTIONS','herbe', true);
    ficini.Free;
  end
  else begin
    smartphone.cbAliasing.ItemIndex := 2;
    smartphone.cbFPS.IsChecked := false;
    smartphone.cbAfficherLignes.IsChecked := false;
    GBEPlaneExtend1.SubdivisionsHeight := 30;
    GBEPlaneExtend1.SubdivisionsWidth := 30;
    smartphone.sbNuages.Value := 15;
    smartphone.cbDetailsOcean.ItemIndex := 1;
    smartphone.cbDetailsSol.ItemIndex := 0;
    GBEHeightmap1.loadHeightmapFromResource('heightmap32');
    GBEHeightmap1.Flou := 1;
    smartphone.cbTasks.IsChecked := true;
    smartphone.cbHerbe.IsChecked := true;
  end;

  if not(smartphone.cbFPS.IsChecked) then lblFPS.Text := '';
  tFPS.Enabled := smartphone.cbFPS.IsChecked;
  GBEHeightmap1.ShowLines := smartphone.cbAfficherLignes.IsChecked;
  GBEPlaneExtend1.ShowLines := smartphone.cbAfficherLignes.IsChecked;
  GBEPlaneExtend1.ActiveWaves := smartphone.cbDetailsOcean.ItemIndex > 0;
  GBEPlaneExtend1.UseTasks := smartphone.cbTasks.IsChecked;
  GBEClouds1.UseTasks := smartphone.cbTasks.IsChecked;
  oldDetailsSol := smartphone.cbDetailsSol.ItemIndex;
  gererHerbe := smartphone.cbHerbe.IsChecked;
  if not(gererHerbe) then begin
    for I := dmyHerbe.ChildrenCount-1 downto 0 do
       dmyHerbe.Children[i].Free;
  end;
  ChargerNuages;
  ChargerMer;
end;

procedure TfrmMain.SauverConfig;
var
  ficini : TInifile;
begin
  ficini := TIniFile.Create(configFile);
  ficini.WriteInteger('OPTIONS','aliasing', smartphone.cbAliasing.ItemIndex);
  ficini.WriteBool('OPTIONS','showFPS',smartphone.cbFPS.IsChecked);
  ficini.WriteBool('OPTIONS','showLines',smartphone.cbAfficherLignes.IsChecked);
  ficini.WriteFloat('OPTIONS','nbnuages',smartphone.sbNuages.Value);
  ficini.WriteBool('OPTIONS','multitaches',smartphone.cbTasks.IsChecked);
  ficini.WriteBool('OPTIONS','herbe',smartphone.cbHerbe.IsChecked);

  case smartphone.cbDetailsOcean.ItemIndex of
    0: ficini.WriteInteger('OPTIONS','detailsOcean', 10);
    1: ficini.WriteInteger('OPTIONS','detailsOcean', 30);
    2: ficini.WriteInteger('OPTIONS','detailsOcean', 90);
  end;

  ficini.WriteInteger('OPTIONS','detailsSol', smartphone.cbDetailsSol.ItemIndex);
  ficini.Free;
end;

procedure TfrmMain.AppliquerConfig;
var
  changerDetailSol : boolean;
begin
  changerDetailSol := smartphone.cbDetailsSol.ItemIndex <> oldDetailsSol;
  SauverConfig;
  ChargerConfig;
  if changerDetailSol then MettreAJourHauteurs;
end;

procedure TfrmMain.MettreAJourHauteurs;
var
  i : integer;
begin
  for I := 0 to dmyObstacles.ChildrenCount -1 do  begin// Mise à jour des obstacles
    if TControl3D(dmyObstacles.Children[i]).Name = 'dmyMaisonForCollisionDetection' then
       TControl3D(dmyObstacles.Children[i]).Position.Y := GBEHeightmap1.GetHeight(TControl3D(dmyObstacles.Children[i]).Position.Point) + 2.5
    else TControl3D(dmyObstacles.Children[i]).Position.Y := GBEHeightmap1.GetHeight(TControl3D(dmyObstacles.Children[i]).Position.Point);
  end;

  for I := 0 to dmyHerbe.ChildrenCount -1 do  // Mise à jour de l'herbe
    TControl3D(dmyHerbe.Children[i]).Position.Y := GBEHeightmap1.GetHeight(TControl3D(dmyHerbe.Children[i]).Position.Point) + TControl3D(dmyHerbe.Children[i]).Height * 0.5;

  GBEPlayerPosition.Position.y := GBEHeightmap1.GetHeight(GBEPlayerPosition.Position.Point) + tailleJoueur;
  modelJoueur.Position.point := GBEPlayerPosition.Position.point;
end;

procedure TfrmMain.ChargerNuages;
begin
  GBEClouds1.deleteTexturesClouds;
  GBEClouds1.addTextureCloud(TextureMaterialSource1);
  GBEClouds1.addTextureCloud(TextureMaterialSource2);
  GBEClouds1.addTextureCloud(TextureMaterialSource3);
  GBEClouds1.NbClouds := Round(smartphone.sbNuages.Value);
  GBEClouds1.ActiveWind := true;
end;

procedure TfrmMain.ChargerMer;
begin
  GBEPlaneExtend1.Origine := Point3D(GBEPlaneExtend1.Width/2,0,GBEPlaneExtend1.height/2);
  GBEPlaneExtend1.Opacity := 0.6;
  GBEPlaneExtend1.Amplitude := 5;
  GBEPlaneExtend1.Longueur := 1;
end;

procedure TfrmMain.ChargerVegetation;
var
  pxObject : TProxyObject;
  i: integer;
begin
  dmyArbreForCollisionDetection.Position.point := Point3D(180, GBEHeightmap1.GetHeight(Point3D(180, 0, 190)), 190);

  for I := 0 to 8 do begin
    pxObject := TProxyObject.Create(nil);
    GBEHeightmap1.AddObject(pxObject);
    pxObject.SourceObject := dmyArbreForCollisionDetection;
    pxObject.width := dmyArbreForCollisionDetection.Width;
    pxObject.Height := dmyArbreForCollisionDetection.Height;
    pxObject.depth := dmyArbreForCollisionDetection.Depth;
    pxObject.Locked:=true;
    pxObject.HitTest:=false;
    pxObject.Position.Point:= Point3D(dmyArbreForCollisionDetection.Position.X-random(20), 0,dmyArbreForCollisionDetection.Position.Z-random(20));
    pxObject.Position.Y := GBEHeightmap1.GetHeight(pxObject.Position.Point);
    pxObject.RotationAngle.y := random(360);
    pxObject.Parent := dmyObstacles;
    pxObject.Name := 'dmyArbre'+i.ToString;
    pxObject.Visible := true;
  end;
end;

procedure TfrmMain.genererHerbe(nombre : integer; premiereGeneration: boolean);
var
  i : integer;
  p : TPoint3D;
begin
  for i := 1 to nombre do
  begin
    with TGBEGrass.Create(nil) do
    begin
      visible := false;
      parent := dmyHerbe;
      if premiereGeneration then begin
        p.x := GBEPlayerPosition.Position.X + demiDistanceAffichageHerbe - random(distanceAffichageHerbe);
        p.z := GBEPlayerPosition.Position.Z + demiDistanceAffichageHerbe - random(distanceAffichageHerbe);
      end else begin
        p.x := GBEPlayerPosition.Position.X + joyDeplacement.direction.x * demiDistanceAffichageHerbe + demiDistanceAffichageHerbe - random(distanceAffichageHerbe);
        p.z := GBEPlayerPosition.Position.Z + joyDeplacement.direction.z * demiDistanceAffichageHerbe + demiDistanceAffichageHerbe - random(distanceAffichageHerbe);
      end;

      rotationangle.y := random(360);
      rotationangle.X := 180;
      MaterialSource := TextureMaterialSource;
      width := 0.5;
      height := 0.25;
      depth := 0;
      p.Y := GBEHeightmap1.GetHeight(p) + height * 0.5;
      position.Point := p;
      zwrite := true;
      visible := true;
      temps := 0.1;
    end;
  end;
end;

procedure TfrmMain.gestionHerbe;
var
  i : integer;
  distance : TPoint3D;
  aFaire : boolean;
begin
  gererHerbe := false;
  aFaire := dmyHerbe.ChildrenCount = 0;
  if smartphone.cbHerbe.IsChecked then begin
    for I := dmyHerbe.ChildrenCount-1 downto 0 do begin // Recherche et suppression des herbes trop loin du joueur
      distance := TGBEGrass(dmyHerbe.Children[i]).Position.Point - GBEPlayerPosition.Position.Point;
      if (abs(distance.X) > distanceAffichageHerbe)
         or (abs(distance.Y) > distanceAffichageHerbe)
         or (abs(distance.Z) > distanceAffichageHerbe) then
           dmyHerbe.Children[i].Free;
    end;
    genererHerbe(maxHerbe - dmyHerbe.ChildrenCount, aFaire); // On génére nouvelles herbes
  end;
  gererHerbe := smartphone.cbHerbe.IsChecked;
end;

procedure TfrmMain.SupprimerObstacles;
begin
  dmyArbreForCollisionDetection.Parent := GBEHeightmap1;   // Pour conserver le modelPalmier
  dmyMaisonForCollisionDetection.Parent := GBEHeightmap1;  // Pour conserver le modelMaison
  dmyObstacles.DeleteChildren;            // On supprime tous les obstacles
  dmyArbreForCollisionDetection.Parent := dmyObstacles; // On réaffecte les obstacles "vegetation"
  dmyMaisonForCollisionDetection.Parent := dmyObstacles; // On réaffecte les obstacles "Habitation"
end;

procedure TfrmMain.ChargerHabitation;
begin
  dmyMaisonForCollisionDetection.Position.point := Point3D(170, GBEHeightmap1.GetHeight(Point3D(170, 0, 200))+2.5, 200);
end;

procedure TfrmMain.AfficherCarte;
begin
  smartphone.imgCarte.Bitmap := GBEViewport3D.getBitmapFromView(cameraCarte);
  smartphone.imgCarte.Bitmap.Canvas.BeginScene;
  smartphone.imgCarte.Bitmap.Canvas.FillEllipse(RectF((smartphone.imgCarte.Bitmap.Width-30)/2, (smartphone.imgCarte.Bitmap.Height-30)/2, (smartphone.imgCarte.Bitmap.Width+30)/2, (smartphone.imgCarte.Bitmap.Height+30)/2), 0.8);
  smartphone.imgCarte.Bitmap.Canvas.EndScene;
end;

procedure TfrmMain.SceneIntroduction;
begin
  if preparationAFaire then preparationSceneIntroduction;
  if smartphone.sbNuages.Value > 0 then GBEClouds1.moveClouds;
  Image3.Position.X := frmMain.Width - Image3.Width - 25;
  Image3.Position.Y := frmMain.Height - Image3.Height - 50;
end;

procedure TfrmMain.SceneGame;
var
  resultat : TRetour;
begin
  if preparationAFaire then preparationSceneGame;
  if smartphone.sbNuages.Value > 0 then GBEClouds1.moveClouds;

  if gererHerbe then gestionHerbe;

  lblCollision.text := '';
  smartphone.lblHour.Text := FormatDateTime('hh:nn', now);
  if smartphone.recGPS.visible then afficherCarte;

  if joyDeplacement.Acceleration = 0 then vitesse := 0
  else begin
    if abs(vitesse) <= vitesseMax then vitesse := vitesse + joyDeplacement.Acceleration/5000;

    GBEPlayerPosition.NextPosition.Position.point := GBEPlayerPosition.Position.Point - joyDeplacement.direction * vitesse;
    GBEPlayerPosition.NextPosition.position.Y := GBEHeightmap1.GetHeight(GBEPlayerPosition.Position.Point) + tailleJoueur;

    // On controle que la prochaine position est dans l'aire de jeu
    if (abs(GBEPlayerPosition.NextPosition.position.Point.x) < limiteZoneJeuX) and
       (abs(GBEPlayerPosition.NextPosition.position.Point.z) < limiteZoneJeuY) then begin
      resultat := DetectionCollisionObstacle(GBEHeightmap1, GBEPlayerPosition.NextPosition);
      if not(resultat.bool) then begin
         GBEPlayerPosition.Position.point := GBEPlayerPosition.NextPosition.position.Point;
         modelJoueur.Position.point := GBEPlayerPosition.Position.point;
         modelJoueur.RotationAngle.y := 180-GBEPlayerPosition.RotationAngle.Y;
      end
      else lblCollision.text := 'Collision avec ' + resultat.libelle;
    end
    else vitesse := 0; // sinon on ne déplace pas le joueur et on réinitialise sa vitesse de déplacement
  end;
end;

procedure TfrmMain.preparationSceneGame;
begin
  layBas.Visible := true;
  layGauche.Visible := true;
  smartphone.Position.x := -layGauche.Width -layGauche.Margins.left;
  Image2.Visible := false;
  Image3.Visible := false;
  Image2.HitTest := false;
  aniTexte.Stop;
  GBEPlayerPosition.RotationAngle.X := 180;
  GBEPlayerPosition.Parent := GBEHeightmap1;
  GBEPlayerPosition.TypePosition := TGBETypePosition.thirdPerson;
  GBEPlayerPosition.getCamera.Position.Y := -1;
  GBEPlayerPosition.getCamera.Position.Z := -4;
  GBEPlayerPosition.Position.Point := Point3D(180,GBEHeightmap1.GetHeight(Point3D(180, 0, 220)) + tailleJoueur, 220);
  modelJoueur.Position.Point := GBEPlayerPosition.Position.Point;
  joyDeplacement.deplacement := Point3D(-1,0,1);
  limiteZoneJeuX := GBEPlaneExtend1.width*0.5;
  limiteZoneJeuY := GBEPlaneExtend1.height*0.5;
  preparationAFaire := false;
end;

procedure TfrmMain.preparationSceneIntroduction;
begin
  layBas.Visible := false;
  layGauche.Visible := false;
  GBEPlayerPosition.Parent := dmyMonde;
  GBEPlayerPosition.Position.Point := Point3D(0,GBEHeightmap1.GetHeight(Point3D(0, 0, 0)) ,0);
  GBEPlayerPosition.TypePosition := TGBETypePosition.thirdPerson;
  GBEPlayerPosition.getCamera.Position.Y := -30;
  GBEPlayerPosition.getCamera.Position.Z := -320;
  Image2.Visible := true;
  Image3.Visible := true;
  Image2.HitTest := true;
  aniTexte.Start;
  preparationAFaire := false;
end;

end.
