with Ada.Text_Io; use Ada.Text_Io;

--Acteurs utilises pour la partie graphique
with Gtk.Main;          use Gtk.Main;
with Gtk.Window;        use Gtk.Window;
with Gtk.Enums;         use Gtk.Enums;
with Gtk.Button;        use Gtk.Button;
with Gtk.Label;         USE Gtk.Label;
with Gtk.Box;           use Gtk.Box;
with Gtk.Widget;        use Gtk.Widget;
with Gtk.Table;         use Gtk.Table;
with Gtk.Image;         use Gtk.Image;
with Gtk.Alignment;     use Gtk.Alignment;
with Gtk.GEntry;        use Gtk.GEntry;
with gdk.event;         use gdk.event;
with glib;              use glib;
with Ada.Finalization;   use Ada.Finalization;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window ;
with Gtk.Handlers;

procedure Kakuro is


   --**Debut partie Structure**--

   --Declaration des types

   type Tab_Boolean is array (Integer range 1..9) of Boolean;

   type Carac is ('N','L','H','V','D'); --type des cases
   --'N' = rien, 'L' = case à remplir, 'H' = contrainte horizontale
   --'V' = contrainte verticale, 'D' = double contrainte

   type Cases is record
      Car : Carac := 'L'; --type de la case (intialisé à 'L')
      Valeur_Test : Integer := 0; --Valeur testée par l'algorithme
      --prenant ses valeurs parmi les valeurs de Tab_Valeurs_Possibles à "true"
      Contrainte_H : Integer := 0; --contrainte horizontale (à zero si aucune contrainte)
      Contrainte_V : Integer := 0; --contrainte verticale (à zero si aucune contrainte)
      Tab_Valeurs_Possibles : Tab_Boolean := (others => false);
   end record;

   type Une_Grille is array (Integer range <>,Integer range <>) of Cases;
   --Definit la grille de Kakuro

   type Coord is record --Coordonnees d'une case
      X : Integer; --Indice de la ligne
      Y : Integer; --Indice de la colonne
   end record;

   type Tab_Chemin is array (Integer range <>) of Coord;

   --**Fin partie Structure**--


   --**Début partie Exception**--

   package P_Callback is new Gtk.Handlers.Callback(Gtk_Widget_Record);
   use P_Callback;

   --Permet de stopper le processus lors de la fermeture d'une fenêtre
   PROCEDURE Stop_Program(Emetteur : access Gtk_widget_Record'class) IS
   PRAGMA Unreferenced (Emetteur );
   begin
      Main_Quit;
   end Stop_Program ;

   --Permet de fermer une fenetre
   procedure Stop_Windows(Emetteur : access Gtk_widget_Record'class) is
      --pragma Unreferenced(Emetteur);
   begin
      destroy(Emetteur);
   end Stop_Windows;


   --Ouvre une fenêtre pour afficher l'exception levée
   procedure Dialogue(erreur : in string) is
      Win : Gtk_Window;
      Boite : Gtk_VBox;
      Message : Gtk_Label;
   begin
      Init;
      Gtk_New(Win); --Permet de créer une nouvelle fenêtre
      Win.Set_Title("Fatal Error"); --Définit le titre de la fenêtre
      Win.set_Default_Size(300,100); --Définit la taille de la fenêtre
      if not Win.Set_Icon_From_File("interdit.png")then
         --en cas d'erreur lors du chargement de l'icône
         Put("chargement de l'icone impossible");
      end if;
      Gtk_New_VBox(Boite,true,3); --Une VBox joue le rôle de conteneur
      --pour pouvoir afficher dans la fenêtre le message d'erreur
      Win.Add(Boite); --ajout de la Vbox à la fenêtre
      Gtk_New(Message,erreur); --crée une étiquette Message
      --contenant la chaîne de caractères erreur
      Boite.Pack_End(Message,true,false); --attache le message
      --à la boîte crée précédemment
      connect(Win,"destroy",Stop_Program'Access);
      Win.show_all; --Affiche à l'écran le contenu de la fenêtre
      Main;
   end Dialogue;


   --parcours de la grille afin de détecter puis de lever une potentielle exception
   procedure Lever_exception(Grille : in Une_Grille) is
      Type_Non_Trouve : exception; --type inexistant (pas 'L','N','H','V' ou 'D')
      Contrainte_H_Fausse, Contrainte_V_Fausse : exception; --contraintes non comprises entre 3 et 45
      Aucune_possibilite : exception; --Aucune valeur n'est possible dans une case
      Trouve : Boolean := false;
      Ligne, Colonne : Integer;
   begin
      for Li in Grille'range(1) loop
         for Col in Grille'range(2) loop

            if not ((Grille(Li,Col).car /= 'D') or else (Grille(Li,Col).car /= 'L') or else (Grille(Li,Col).car /= 'V') or else (Grille(Li,Col).car /= 'H') or else (Grille(Li,Col).car /= 'N')) then
               --Vérifie le type de la case (appartenance au type énuméré carac)
               Ligne := Li;
               Colonne := Col;
               raise Type_Non_Trouve;
            end if;

            if (Grille(Li,Col).Car = 'D' or Grille(Li,Col).Car = 'H') and not (Grille(Li,Col).Contrainte_H in 3..45) then
               ---Vérifie que la contrainte horizontale appartient bien à l'intervalle [3,45]
               Ligne := Li;
               Colonne := Col;
               raise Contrainte_H_Fausse;
            end if;

            if (Grille(Li,Col).Car = 'D' or Grille(Li,Col).Car = 'V') and not (Grille(Li,Col).Contrainte_V in 3..45) then
               --Vérifie que la contrainte verticale appartient bien à l'intervalle [3,45]
               Ligne := Li;
               Colonne := Col;
               raise Contrainte_V_Fausse;
            end if;

            if Grille(Li,Col).Car = 'L' and Grille(LI,Col).Valeur_Test = 0 then
               --vérifie qu'il y est au moins une valeur possible dans une case
               for I in Grille(Li,Col).Tab_Valeurs_Possibles'range loop
                  if Grille(Li,Col).Tab_Valeurs_Possibles(I) then
                     Trouve := true;
                  end if;
               end loop;

               if not Trouve then
                  Ligne := Li;
                  Colonne := Col;
                  raise Aucune_possibilite;
               end if;

            end if;

         end loop;
      end loop;

   exception --traitement des exceptions
      when Type_Non_Trouve =>
         Dialogue ("Case "&Integer'Image(Ligne)&","&Integer'Image(Colonne)&"  : "&"Le type de case détecté n'est pas défini => Grille Invalide");
      when Contrainte_H_Fausse =>
         Dialogue ("Case "&Integer'Image(Ligne)&","&Integer'Image(Colonne)& " : "&"Mauvaise Contrainte Horizontale => Grille Invalide");
      when Contrainte_V_Fausse =>
         Dialogue ("Case "&Integer'Image(Ligne)&","&Integer'Image(Colonne)&"  : "&"Mauvaise Contrainte Verticale => Grille Invalide");
      when Aucune_possibilite =>
         Dialogue ("Case "&Integer'Image(Ligne)&","&Integer'Image(Colonne)&"  : "&"Aucune valeurs possibles=> Grille Non Resolvable");
   end Lever_exception;

   --**Fin partie exception**--


   --**Debut partie Creation**--

   --Recuperation des dimensions de la grille de jeu à l'aide d'un fichier grille
   procedure Recuperation_Max (Li_Max : out Integer; C_Max : out Integer; fichier : in File_type) is
      Ligne : constant String := Get_Line (fichier); --Récupère la première ligne du fichier
      Pos_Tiret : Integer := Ligne'First; --Repère la position du caractère '-'
      Trouver : Boolean := False;
   begin
      while not Trouver and Pos_Tiret<=Ligne'Last loop
         --cherche le caractère '-' dans la ligne
         if Ligne(Pos_Tiret)= '-' then
            Trouver := True;
         else
            Pos_Tiret := Pos_Tiret + 1;
         end if;
      end loop;
      Li_Max := Integer'Value(Ligne(Ligne'First..Pos_Tiret-1));
      C_Max := Integer'Value(Ligne(Pos_Tiret+1..Ligne'Last));
   end Recuperation_Max;


   --Récupération des données d'un fichier grille pour définir une case
   procedure Recuperation_Donnee (fichier : in File_type; Mat_Grille : in out Une_Grille) is
      Ligne : constant String := Get_Line(fichier); --Récupère une ligne du fichier
      Num : Integer := Ligne'First;
      Trouver : Boolean := False;
      Pos_Virgule : Integer; --
      Pos_Carac : Integer;   -- repérage des différents caractères délimiteurs
      Pos_Tiret : Integer;   --
      Coord_Case : Coord;
      Lettre : Carac;
      --pour rappel, voici des exemples de lignes d'un fichier grille
      -- 2,2:D4-8  2,3:H6  1,1:N
   begin
      while not Trouver and Num <= Ligne'Last loop
         case Ligne(Num) is
            when ',' => Pos_Virgule := Num;
            when 'N' =>
               Pos_Carac := Num;
               Trouver := True;
               Lettre := 'N';
            when 'V' =>
               Pos_Carac := Num;
               Trouver := True;
               Lettre := 'V';
            when 'H' =>
               Pos_Carac := Num;
               Trouver := True;
               Lettre := 'H';
            when 'D' =>
               Pos_Carac := Num;
               Lettre := 'D';
            when '-' =>
               Pos_Tiret := Num;
               Trouver := True;
            when others => NULL;
         end case;
         Num := Num + 1;
      end loop;

      Coord_Case.X := Integer'Value(Ligne(Ligne'First..Pos_Virgule-1));
      Coord_Case.Y := Integer'Value(Ligne(Pos_Virgule+1..Pos_Carac-2));
      Mat_Grille(Coord_Case.X,Coord_Case.Y).Car := Lettre;
      case Mat_Grille(Coord_Case.X,Coord_Case.Y).Car is
         when 'V' => Mat_Grille(Coord_Case.X,Coord_Case.Y).Contrainte_V := Integer'Value(Ligne(Pos_Carac+1..Ligne'Last));
         when 'H' => Mat_Grille(Coord_Case.X,Coord_Case.Y).Contrainte_H := Integer'Value(Ligne(Pos_Carac+1..Ligne'Last));
         when 'D' =>
            Mat_Grille(Coord_Case.X,Coord_Case.Y).Contrainte_H := Integer'Value(Ligne(Pos_Carac+1..Pos_Tiret-1));
            Mat_Grille(Coord_Case.X,Coord_Case.Y).Contrainte_V := Integer'Value(Ligne(Pos_Tiret+1..Ligne'Last));
         when others => NULL;
      end case;
   end Recuperation_Donnee;


   --Lit le fichier pour créer la grille
   procedure Creer_Grille (Mat_Grille : in out Une_Grille; fichier : in File_type) is
   begin
      while not End_of_File(fichier) loop --permet de parcourir le fichier en déplaçant un curseur ligne par ligne
         Recuperation_Donnee (fichier, Mat_Grille); --permet de récupérer les informations pour la ligne où le curseur est placé
      end loop;
   end Creer_Grille;

   --**Fin partie Creation**--


   --**Début partie Combinaison**--

   --Analyse une ligne de BD.txt pour savoir si elle correspond à celle du couple contrainte-nombre de case recherché
   procedure Trouver_Ligne (Contrainte : in Integer; Nb_Cases : in Integer; fichier_BD : in File_Type; Num_Ligne : in out Positive_Count; Trouver : in out Boolean) is
      Ligne : constant String := Get_Line (fichier_BD);
      Pos_Tiret : Integer;
      Pos_Carac : Integer;
      Trouver_Carac : Boolean := False;
      Num : Integer := Ligne'First;
   begin
      while not Trouver_Carac and Num <= Ligne'Last loop
         case Ligne(Num) is
            when '-' => Pos_Tiret := Num;
            when ':' => Pos_Carac := Num;
               Trouver_Carac := True;
            when others => NULL;
         end case;
         Num := Num + 1;
      end loop;

      if Contrainte = Integer'value(Ligne(Ligne'First..Pos_Tiret-1)) and Nb_Cases = Integer'value (Ligne(Pos_Tiret+1..Pos_Carac-1)) then
         Trouver := True;
      else
         Num_Ligne := Num_Ligne + 1;
      end if;
   end Trouver_Ligne;

   --Permet de parcourir le fichier BD.txt jusqu'à trouver la ligne contenant le couple contrainte/nombre de cases libres voulu
   procedure Parcourir_fichier (Contrainte : in Integer; Nb_Cases : in Integer; fichier_BD : in File_Type; Num_Ligne : out Positive_Count) is
      Trouver : Boolean := False;
   begin
      Num_Ligne := 1;
      while not End_Of_File(fichier_BD) and not Trouver loop --analyse de la ligne
           Trouver_Ligne(Contrainte,Nb_Cases,fichier_BD,Num_Ligne,Trouver);
      end loop;
   end Parcourir_fichier;


   -- récupère les possibilités associées au couple contraintes-nombre de cases
   procedure Recuperer_chiffres (fichier_BD : in File_type; Tab_Combi : out Tab_Boolean) is
      Ligne : constant String := Get_Line(fichier_BD);
      Num : Integer := Ligne'First;
   begin
      while not (Ligne(Num) = ':') loop--exemple de ligne de BD.txt->16-3:123456789
         Num := Num + 1;
      end loop;

      for i in Num+1..Ligne'Last loop
         Tab_Combi (Character'Pos(Ligne(i))-48) := True; --conversion avec le code ASCII
      end loop;
   end Recuperer_chiffres;


   --trouve le nombre de case dédiés au remplissage d'une contrainte
   --la distinction de 'D' en 'V' ou 'H' est faite lors de l'appel de la fonction
   function Nb_Cases_Disponibles (Grille : in Une_Grille; Ligne, Colonne : in Integer; Char : Character) return Integer is
      Col : Integer := Colonne;
      Li : Integer := Ligne;
      Nb_Cases : Integer := 0;
      --stop : Boolean := False;
   begin
      if Char = 'H' then
         while not (Col+1 > Grille'Last(2)) and then (Grille(Li,Col+1).Car = 'L') loop
            Nb_Cases := Nb_Cases +1;--incrémenté que si la case suivante est une case libre.
            Col := Col +1;
         end loop;
      else
         while not (Li+1 > Grille'Last(1)) and then (Grille(Li+1,Col).Car = 'L') loop
            Nb_Cases := Nb_Cases +1;--incrémenté que si la case suivante est une case libre.
            Li := Li +1;
         end loop;
      end if;
      return Nb_Cases;
   end Nb_Cases_Disponibles;


   --charge les combinaisons de chiffres possibles
   procedure Trouver_Combinaisons (Contrainte : in Integer; Nb_Cases : in Integer; Tab_Combi : out Tab_Boolean) is
      fichier_BD : File_type;
      Num_Lig : Positive_Count := 1;
   begin
      Tab_Combi := (others => false);
      open(fichier_BD,In_File,"BD.txt");
      Parcourir_fichier(Contrainte,Nb_Cases,fichier_BD,Num_Lig);
      Reset (fichier_BD);
      Set_Line(fichier_BD,Num_Lig);
      Recuperer_chiffres (fichier_BD,Tab_Combi);
      close(fichier_BD);
      exception
      when end_Error =>
         --Exception déclenchée si aucune combinaison existe pour le couple contrainte/nombre de cases
         Dialogue("La contrainte "&Integer'Image(contrainte)&" n'est pas réalisable en "&Integer'Image(Nb_Cases)&" cases => Grille Invalide");
   end Trouver_Combinaisons;

   --**Fin partie Combinaison**--


   --**Début partie Balayage**--


    --Calcul le nombre de valeurs possibles dans une case après les deux balayages
   function Nb_True (Grille : in Une_Grille; Ligne,Colonne : in Integer) return Integer is
      Tableau : constant Tab_boolean := Grille(Ligne,Colonne).Tab_Valeurs_Possibles;
      Nbr_True : Integer := 0;
   begin
      for A in Tableau'Range loop
         if Tableau(A) then
            Nbr_True := Nbr_True + 1;
         end if;
      end loop;
      return Nbr_True;
   end Nb_True;

   -- premier balayage effectué (balayage vertical)
   procedure Balayage_V ( Grille : in out Une_Grille) is
      Nb_Cases : Integer;
      Tableau : Tab_boolean := (others => false);
   begin
      for Li in Grille'Range(1) loop
         for Col in Grille'Range(2) loop
            if Grille(Li,Col).Car = 'V' or Grille(Li,Col).Car = 'D' then
               Nb_Cases := Nb_Cases_Disponibles(Grille,Li,Col,'V');
               Trouver_Combinaisons(Grille(Li,Col).Contrainte_V,Nb_Cases,Tableau);

               for A in Li+1..Li+Nb_Cases loop --cases libres verticalement
                  Grille(A,Col).Tab_Valeurs_Possibles := Tableau ; --charge les chiffres possibles de chaque case
               end loop;
            end if;
         end loop;
      end loop;
   end Balayage_V;


   -- après le balayage_V, croise les valeurs possibles avec celles chargées lors
   -- de balayage_V pour réduire le nombre de possibilités dans chaque case libre
   procedure Balayage_H (Grille : in out Une_Grille) is
      Nb_Cases : Integer;
      Tableau : Tab_boolean := (others => false);
   begin
      for Li in Grille'Range(1) loop
         for Col in Grille'Range(2) loop
            if Grille(Li,Col).Car = 'H' or Grille(Li,Col).Car = 'D' then
               Nb_Cases := Nb_Cases_Disponibles(Grille,Li,Col,'H');
               Trouver_Combinaisons(Grille(Li,Col).Contrainte_H,Nb_Cases,Tableau);

               for A in Col+1..Col+Nb_Cases loop --cases libres horizontalement
                  if Nb_True(Grille,Li,A) = 0 then
                     for I in Tableau'Range loop
                        Grille(Li,A).Tab_Valeurs_Possibles(I) := Tableau(I);
                     end loop;
                  else
                     for I in Tableau'Range loop
                        Grille(Li, A).Tab_Valeurs_Possibles(I) := Grille(Li,A).Tab_Valeurs_Possibles(I) and Tableau(I);
                        --une valeur n'est possible que si elle possible pour remplir la contrainte V ET la contrainte H
                     end loop;
                  end if;
               end loop;

            end if;
         end loop;
      end loop;
   end Balayage_H;

   procedure Propagation_Contrainte (Grille : in out Une_Grille; L,C,Num : Integer) is
      --Num : Integer;
      col,lig : Integer;
   begin
      col := C;
      --gauche
      while Col >= Grille'First(2) and then grille(L,Col).car = 'L' loop
         grille(L,Col).tab_valeurs_possibles(Num) := false;
         col := col -1;
      end loop;
      col := C;
      --droite
      while Col <= Grille'Last(2) and then grille(L,Col).car = 'L' loop
         grille(L,Col).tab_valeurs_possibles(Num) := false;
         col := col +1;
      end loop;
      lig := L;
      --haut
      while Lig >= Grille'First(1) and then grille(Lig,C).car = 'L' loop
         grille(Lig,C).tab_valeurs_possibles(Num) := false;
         lig := lig -1;
      end loop;
      lig := L;
      --bas
      while Lig <= Grille'Last(1) and then grille(Lig,C).car = 'L' loop
         grille(Lig,C).tab_valeurs_possibles(Num) := false;
         lig := lig +1;
      end loop;
   end Propagation_Contrainte;

   procedure Diminution_L (Grille : in out Une_Grille; Fin : in out Boolean) is
      Num : Integer;
   begin
      for L in Grille'Range(1) loop
         for C in Grille'Range(2) loop
            if Grille(L,C).Car = 'L' then
               if Nb_True(grille,L,C) = 1 then
                  Num := 1;
                  while not grille(L,C).Tab_Valeurs_Possibles(Num) loop
                     Num := Num +1;
                  end loop;
                  Grille(L,C).Valeur_Test := Num;
                  Grille(L,C).Tab_Valeurs_Possibles(Num) := False;
                  Propagation_Contrainte(Grille,L,C,Num);
                  Fin := False;
               end if;
            end if;
         end loop;
      end loop;
   end Diminution_L;

   procedure Complete_Somme (Grille : in out Une_Grille; Fin : in out Boolean) is
      Num_Libre,I,Reste,Nb_Cases : Integer;
      Somme : Integer := 0;
      Contrainte_H, Contrainte_V : Integer;
      Trouver : Boolean := False;
   begin
      for L in Grille'range(1) loop
         for C in Grille'range(2) loop
            case Grille(L,C).Car is
            when 'H' =>
               Contrainte_H := Grille(L,C).Contrainte_H;
               Somme := 0;
               Num_Libre := 0;
               Nb_Cases := Nb_Cases_Disponibles(Grille,L,C,'H'); --function Nb_Cases_Disponibles (Grille : in Une_Grille; Ligne, Colonne : in Integer; Char : Character) return Integer is
               --Put_Line("H:" & Integer'Image(L) & "," & Integer'Image(C) & Integer'Image(Nb_Cases));
               for I in 1..Nb_Cases loop
                  Somme := Somme + Grille(L,C+I).Valeur_Test;
                  if Nb_True(Grille,L,C+I) >= 1 then
                     Num_Libre := Num_Libre +1;
                  end if;
               end loop;

               if Num_Libre = 1 then
                  Fin := False;
                  Trouver := False;
                  Reste := Contrainte_H - Somme;
                  I := 1;
                  if Reste <= 9 and Reste > 0 then
                     while not Trouver and I <= Nb_Cases loop
                        if Nb_True(Grille,L,C+I) >= 1 then
                           Grille(L,C+I).Valeur_Test := Reste;
                           Propagation_Contrainte(Grille,L,C+I,Reste);
                           for J in 1..9 loop
                              Grille(L,C+I).Tab_Valeurs_Possibles(J) := False;
                           end loop;
                           Trouver := True;
                        else
                           I := I +1;
                        end if;
                     end loop;
                  end if;
               end if;
            when 'V' =>
               Contrainte_V := Grille(L,C).Contrainte_V;
               Somme := 0;
               Num_Libre := 0;
               Nb_Cases := Nb_Cases_Disponibles(Grille,L,C,'V');
               --Put_Line("V:" & Integer'Image(L) & "," & Integer'Image(C) & Integer'Image(Nb_Cases));
               for I in 1..Nb_Cases loop
                  Somme := Somme + Grille(L+I,C).Valeur_Test;
                  if Nb_True(Grille,L+I,C) >= 1 then
                     Num_Libre := Num_Libre +1;
                  end if;
               end loop;

               if Num_Libre = 1 then
                  Fin := False;
                  Trouver := False;
                  Reste := Contrainte_V - Somme;
                  I := 1;
                  if Reste <= 9 and Reste > 0 then
                     while not Trouver and I <= Nb_Cases loop
                        if Nb_True(Grille,L+I,C) >= 1 then
                           Grille(L+I,C).Valeur_Test := Reste;
                           Propagation_Contrainte(Grille,L+I,C,Reste);
                           for J in 1..9 loop
                              Grille(L+I,C).Tab_Valeurs_Possibles(J) := False;
                           end loop;
                           Trouver := True;
                        else
                           I := I +1;
                        end if;
                     end loop;
                  end if;
               end if;
            when 'D' =>
               --H
               Contrainte_H := Grille(L,C).Contrainte_H;
               Somme := 0;
               Num_Libre := 0;
               Nb_Cases := Nb_Cases_Disponibles(Grille,L,C,'H'); --function Nb_Cases_Disponibles (Grille : in Une_Grille; Ligne, Colonne : in Integer; Char : Character) return Integer is
               --Put_Line("H:" & Integer'Image(L) & "," & Integer'Image(C) & Integer'Image(Nb_Cases));
               for I in 1..Nb_Cases loop
                  Somme := Somme + Grille(L,C+I).Valeur_Test;
                  if Nb_True(Grille,L,C+I) >= 1 then
                     Num_Libre := Num_Libre +1;
                  end if;
               end loop;

               if Num_Libre = 1 then
                  Fin := False;
                  Trouver := False;

                  Reste := Contrainte_H - Somme;
                  I := 1;
                  if Reste <= 9 and Reste > 0 then
                     while not Trouver and I <= Nb_Cases loop
                        if Nb_True(Grille,L,C+I) > 1 then
                           Grille(L,C+I).Valeur_Test := Reste;
                           Propagation_Contrainte(Grille,L,C+I,Reste);
                           for J in 1..9 loop
                              Grille(L,C+I).Tab_Valeurs_Possibles(J) := False;
                           end loop;
                           Trouver := True;
                        else
                           I := I +1;
                        end if;
                     end loop;
                  end if;
               end if;
               --V
               Contrainte_V := Grille(L,C).Contrainte_V;
               Somme := 0;
               Num_Libre := 0;
               Nb_Cases := Nb_Cases_Disponibles(Grille,L,C,'V');
               --Put_Line("V:" & Integer'Image(L) & "," & Integer'Image(C) & Integer'Image(Nb_Cases));
               for I in 1..Nb_Cases loop
                  Somme := Somme + Grille(L+I,C).Valeur_Test;
                  if Nb_True(Grille,L+I,C) >= 1 then
                     Num_Libre := Num_Libre +1;
                  end if;
               end loop;

               if Num_Libre = 1 then
                  Fin := False;
                  Trouver := False;

                  Reste := Contrainte_V - Somme;
                  I := 1;
                  if Reste <= 9 and Reste > 0 then
                     while not Trouver and I <= Nb_Cases loop
                        if Nb_True(Grille,L+I,C) > 1  then
                           Grille(L+I,C).Valeur_Test := Reste;
                           Propagation_Contrainte(Grille,L+I,C,Reste);
                           for J in 1..9 loop
                              Grille(L+I,C).Tab_Valeurs_Possibles(J) := False;
                           end loop;
                           Trouver := True;
                        else
                           I := I +1;
                        end if;
                     end loop;
                  end if;
               end if;
            when others =>
               null;
            end case;
         end loop;
      end loop;
   end Complete_Somme;


   --Optimisation des possibilités de chaque case
   --toute case ne contenant qu'une unique valeur possible est fixée
   --les contraintes H et V utilisant cette valeur sont parcourues
   --pour mettre à false la veleur fixée
   procedure Opti (Grille : in out Une_Grille) is
      Fin : Boolean := False;
   begin
      while not Fin loop --tant qu'il existe une case à une unique valeur possible
         Fin := True;
         Diminution_L(Grille,Fin);
         Complete_Somme(Grille,Fin);
      end loop;
   end Opti;

   --**Fin partie Balayage**--


   --**Début partie Chemin**--

--     type Dispo is record
--        Nb_Cases_Dispo_H : Integer := 0;
--        --permet de charger dans chaque case les bonnes valeurs possibles
--        --en connaissant à n'importe quel endroit le nombre de cases disponibles
--        --pour atteindre une certaine contrainte
--        Nb_Cases_Dispo_V : Integer := 0;
--        Ligne_Remplie : Boolean := False; --A voir !!*** URGENT *********EEEEEEE PBBKJBFFFFFFFJDSSFSDHVFUFVSF
--        Colonne_Remplie : Boolean := False; --A voir !!*** URGENT*************** JDUQGKHDPIQDOPIFDJPISAPODPIJIPOHOISFHOUFHUOSHOUFHUSFHUSFGSUUHFUSF
--     end record ;
--
--     type Mat_Cases_Dispo is array( Integer range <>,Integer range <>) of Dispo;
--
--
--     --init la matrice des Nb_cases_Dispo
--     function Remplir_Mat_Dispo (Grille : in Une_Grille; Tab : in Tab_Chemin) return Mat_Cases_Dispo is
--        Mat_Cases : Mat_Cases_Dispo (Grille'Range(1),Grille'Range(2));
--        Nb_Cases : Integer;
--        A : Integer := Tab'First;
--     begin
--        for i in Grille'Range(1) loop
--           for j in Grille'Range(2) loop
--              if Grille(i,j).car = 'V' or Grille(i,j).car = 'D' then
--                 Nb_Cases := Nb_Cases_Disponibles(Grille,i,j,'V');
--                 for k in i+1..i+Nb_Cases loop
--                    Mat_Cases(k,j).Nb_Cases_Dispo_V := Nb_Cases;
--                 end loop;
--
--                 while Tab(A) /= (0,0) loop
--                    Mat_Cases(Tab(A).X,Tab(A).Y).Colonne_Remplie := True;
--                    A := A+1;
--                 end loop;
--
--              elsif Grille(i,j).car = 'H' or Grille(i,j).car = 'D' then
--                 Nb_Cases := Nb_Cases_Disponibles(Grille,i,j,'H');
--                 for k in j+1..j+Nb_Cases loop
--                    Mat_Cases(i,k).Nb_Cases_Dispo_H := Nb_Cases;
--                 end loop;
--
--                 while Tab(A) /= (0,0) loop
--                    Mat_Cases(Tab(A).X,Tab(A).Y).Ligne_Remplie := True;
--                    A := A+1;
--                 end loop;
--              end if;
--           end loop;
--        end loop;
--
--        return Mat_Cases;
--     end Remplir_Mat_Dispo;

--     --vérifie que la case n'a pas déjÃ  été traversée
--     function Verif_Chemin (Coord_Case : Coord; Tab : Tab_Chemin) return Boolean is
--        Trouver : Boolean := False;
--        Num_Etape : Integer := Tab'First;
--     begin
--        while not Trouver and then (Num_Etape <= Tab'Last) and then (Tab(Num_Etape) /= (0,0)) loop
--           if Tab(Num_Etape) = Coord_Case then
--              Trouver := True;
--           end if;
--           Num_Etape := Num_Etape +1;
--        end loop;
--        return not Trouver;
--     end Verif_Chemin;

   --calcule le nombre de cases 'L' (libre, à remplir par l'algorithme) dans la grille
   function Calculer_Nb_L (Grille : in Une_Grille) return Integer is
      Nb_L : Integer := 0;
   begin
      for i in Grille'Range(1) loop
         for j in Grille'Range(2) loop
            if Grille(i,j).Car = 'L' and then Grille(i,j).Valeur_Test = 0 then
               Nb_L := Nb_L +1;
            end if;
         end loop;
      end loop;
      return Nb_L;
   end Calculer_Nb_L;

   --un type de chemin : ligne par ligne (toutes contraintes H existantes)
   function Creer_Chemin_Brut_Ligne (Grille : in Une_Grille) return Tab_chemin is
      T_Chemin : Tab_Chemin(1..Calculer_Nb_L(Grille));
      Int : Integer := 1;
   begin
      for Lig in Grille'Range(1) loop
         for Col in Grille'Range(2) loop
            if Grille(Lig,Col).car = 'L' and then Grille(Lig,Col).Valeur_Test = 0 then
               T_Chemin(Int) := (Lig,Col);
               Int := Int + 1;
            end if;
         end loop;
      end loop;
      return T_Chemin;
   end Creer_Chemin_Brut_Ligne;

   --même principe que Creer_chemin_brut_ligne mais en parcourant colonne par colonne
   function Creer_Chemin_Brut_Colonne (Grille : in Une_Grille) return Tab_chemin is
      T_Chemin : Tab_Chemin(1..Calculer_Nb_L(Grille));
      Int : Integer := 1;
   begin
      for Col in Grille'Range(2) loop
         for Lig in Grille'Range(1) loop
            if Grille(Lig,Col).car = 'L' and then Grille(Lig,Col).Valeur_Test = 0 then
               T_Chemin(Int) := (Lig,Col);
               Int := Int + 1;
            end if;
         end loop;
      end loop;
      return T_Chemin;
   end Creer_Chemin_Brut_Colonne;

      type Memoire is record --utilisé pour construire les chemins V2C et V2L
      Fin : Boolean := False;
      Colonne : Integer := 1 ;
      ligne : Integer :=1;
   end record;


   type Tab_Memoire is array (Integer range <>) of Memoire;

      --utilisé lors de la création des deux prochains types de chemin
   function Fin_L(Tab_Mem: Tab_Memoire) return Boolean is
      Fini : Boolean := True;
      Num : Integer := Tab_Mem'First;
   begin
     while Fini and Tab_Mem'Last >= Num loop
    if not Tab_Mem(Num).Fin then
       Fini:=False;
    end if;
    Num := Num +1;
    end loop;
     return Fini;
   end Fin_L;

   --autre type de chemin : parcours ligne par ligne mais revient à la ligne suivante dès qu'une contrainte horizontale est finie. Une fois la dernière ligne atteinte on remonte à la deuxième ligne et on réitère le processus
   function Creer_Chemin_BrutV2L (Grille : in Une_Grille) return Tab_chemin is
      T_Chemin : Tab_Chemin(1..Calculer_Nb_L(Grille)):=(others =>(0,0));
      Tab_Mem : Tab_Memoire(2..Grille'Last(1));
      Lig : Integer := 2;
      Col : Integer := 1;
      Nb_Cases_Dispo : Integer;
      Etape : Integer := T_Chemin'First;
   begin
      while not Fin_L(Tab_Mem) and T_Chemin(T_Chemin'last) = (0,0) Loop
         While (Lig <= Grille'Last(1) and Col <= Grille'Last(2)) and T_Chemin(T_Chemin'last) = (0,0) Loop
            while (T_Chemin(T_Chemin'last) = (0,0) and Col<=Grille'Last(2)) and then (Grille(Lig,Col).car /= 'H' and Grille(Lig,Col).car /= 'D') loop --or -> and
               Col := Col +1 ;
            end loop;

            if Col < Grille'Last(2) then
               Nb_Cases_Dispo := Nb_Cases_Disponibles(Grille,Lig,Col,'H');
               Tab_Mem(Lig).Colonne := Col + Nb_Cases_Dispo;
               Col := Col + 1;
               for I in Etape..(Etape+Nb_Cases_Dispo-1) loop
                  if Col <= Grille'Last(2) and then Grille(Lig,Col).Valeur_Test = 0 then
                     T_Chemin(Etape) := (Lig,Col);
                     Etape := Etape + 1;
                  end if;
                  Col := Col + 1;
               end loop;
            elsif Col = Grille'Last(2) then
               Tab_Mem(Lig).Fin := True;
            end if;

            if Lig = Tab_Mem'Last then
               Lig := 2;
            else
               Lig := Lig + 1;
            end if;
            Col := Tab_Mem(Lig).Colonne;
         end loop;
      end loop;
      return T_Chemin;
   end Creer_Chemin_BrutV2L;

   --même principe que V2L : parcours colonne par colonne mais revient à la colonne suivante dès qu'une contrainte verticale est finie. Une fois la dernière colonne atteinte
   --on se décale à la deuxième colonne et on réitère le processus
   function Creer_Chemin_BrutV2C(Grille : in Une_Grille) return Tab_chemin is
      T_Chemin : Tab_Chemin(1..Calculer_Nb_L(Grille)):= (others => (0,0));
      Tab_Mem : Tab_Memoire(2..Grille'Last(2));
      Lig : Integer := 1;
      Col : Integer := 2;
      Nb_Cases_Dispo : Integer;
      Etape : Integer := T_Chemin'First;
   begin
      while not Fin_L(Tab_Mem) and T_Chemin(T_Chemin'last) = (0,0) Loop
         While (Lig <= Grille'Last(1) and Col <= Grille'Last(2)) and T_Chemin(T_Chemin'last) = (0,0) Loop
            while (T_Chemin(T_Chemin'last) = (0,0) and Lig<=Grille'Last(1)) and then (Grille(Lig,Col).car /= 'V' and Grille(Lig,Col).car /= 'D') loop --or -> and
               Lig := lig+1 ;
            end loop;

            if Lig < Grille'Last(1) then
               Nb_Cases_Dispo := Nb_Cases_Disponibles(Grille,Lig,Col,'V');
               Tab_Mem(Col).Ligne := Lig + Nb_Cases_Dispo;
               Lig := Lig + 1;
               for I in Etape..(Etape+Nb_Cases_Dispo-1) loop
                  if Lig <= Grille'Last(1) and then Grille(Lig,Col).Valeur_Test = 0 then
                     T_Chemin(Etape) := (Lig,Col);
                     Etape := Etape + 1;
                  end if;
                  Lig := Lig + 1;
               end loop;
            elsif Lig = Grille'Last(1) then
               Tab_Mem(Col).Fin := True;
            end if;

            if col = Tab_Mem'Last then
               col := 2;
            else
               col := col + 1;
            end if;
            Lig := Tab_Mem(Col).Ligne;

         end loop;
      end loop;
      return T_Chemin;
   end Creer_Chemin_BrutV2C;

      -- fonction de vérification utilisée dans le programme principal(backtrack)
   -- vérifie si les contraintes verticales et horizontales sont atteintes
   function Verif_Somme_V2 (Grille : Une_Grille; Coords : Coord) return Boolean is
      Ok : Boolean := true;
      Coord_Act : Coord := Coords;
      Somme : Integer := 0;
      Contr_H : Integer := 0;
      Compteur_H : Integer := 0;
      Contr_V : Integer := 0;
      Compteur_V : Integer := 0;
      Nb_Cases_Dispo : Integer := 0;
   begin
      --H
      while Grille(Coord_Act.X,Coord_Act.Y-1).Car = 'L' loop
         Coord_Act.Y := Coord_Act.Y-1;
      end loop;

      if Grille(Coord_Act.X,Coord_Act.Y-1).Car = 'H' or Grille(Coord_Act.X,Coord_Act.Y-1).Car = 'D' then
         Contr_H := Grille(Coord_Act.X,Coord_Act.Y-1).Contrainte_H;
         Nb_Cases_Dispo := Nb_Cases_Disponibles(Grille,Coord_Act.X,Coord_Act.Y-1,'H');
         for I in 1..Nb_Cases_Dispo loop
            if Grille(Coord_Act.X,Coord_Act.Y).Valeur_test /= 0 then
               Compteur_H := Compteur_H+1;
               Somme := Somme + Grille(Coord_Act.X,Coord_Act.Y).Valeur_Test;
               Coord_Act.Y := Coord_Act.Y+1;
            end if;
         end loop;

         if (Compteur_H = Nb_Cases_Dispo and Somme /= Contr_H) or (Compteur_H /= Nb_Cases_Dispo and Somme >= Contr_H) then
            Ok := False;
         end if;
      end if;

      Somme := 0;
      Coord_Act := Coords;

      --V
      while Grille(Coord_Act.X-1,Coord_Act.Y).Car = 'L' loop
         Coord_Act.X := Coord_Act.X-1;
      end loop;

      if Grille(Coord_Act.X-1,Coord_Act.Y).Car = 'V' or Grille(Coord_Act.X-1,Coord_Act.Y).Car = 'D' then
         Contr_V := Grille(Coord_Act.X-1,Coord_Act.Y).Contrainte_V;
         Nb_Cases_Dispo := Nb_Cases_Disponibles(Grille,Coord_Act.X-1,Coord_Act.Y,'V');
         for I in 1..Nb_Cases_Dispo  loop
            if Grille(Coord_Act.X,Coord_Act.Y).Valeur_test /= 0 then
               Compteur_V := Compteur_V + 1;
               Somme := Somme + Grille(Coord_Act.X,Coord_Act.Y).Valeur_Test;
               Coord_Act.X := Coord_Act.X+1;
            end if;
         end loop;

         if (Compteur_V = Nb_Cases_Dispo and Somme /= Contr_V) or (Compteur_V /= Nb_Cases_Dispo and Somme >= Contr_V) then
            Ok := False;
         end if;
      end if;

      return Ok;
   end Verif_Somme_V2;


   -- fonction de vérification utilisée dans backtrack, renvoie faux lorsque backtrack essaie une valeur déjÃ  présente dans la même contrainte en ligne ou en colonne
   function Verif_Aucun_Doublons (Grille : Une_Grille; Coords : Coord ; Valeur : Integer ) return Boolean is
      Doublon : Boolean := False;
      Coord_Proche : Coord := Coords;
   begin
      --droite
      while (not Doublon and Coord_Proche.Y <= Grille'Last(2)) and then (Grille(Coord_Proche.X,Coord_Proche.Y).car = 'L') loop
         if (not (Coord_Proche = Coords) and Coord_Proche.Y < Grille'Last(2)) and then (Grille(Coord_Proche.X,Coord_Proche.Y).Valeur_Test = Valeur) then
            Doublon := True;
         else
            Coord_Proche.Y := Coord_Proche.Y+1;
         end if;
      end loop;
      Coord_Proche := Coords;

      --gauche
      while (not Doublon and Coord_Proche.Y >= Grille'First(2)) and then (Grille(Coord_Proche.X,Coord_Proche.Y).Car = 'L') loop
         if not (Coord_Proche = Coords) and then Grille(Coord_Proche.X,Coord_Proche.Y).Valeur_Test = Valeur then
            Doublon := True;
         else
            Coord_Proche.Y := Coord_Proche.Y-1;
         end if;
      end loop;
      Coord_Proche := Coords;

      --bas
      while (not Doublon and Coord_Proche.X <= Grille'Last(1)) and then (Grille(Coord_Proche.X,Coord_Proche.Y).Car = 'L') loop
         if (not (Coord_Proche = Coords) and Coord_Proche.X < Grille'Last(1)) and then Grille(Coord_Proche.X,Coord_Proche.Y).Valeur_Test = Valeur then
            Doublon := True;
         else
            Coord_Proche.X := Coord_Proche.X+1;
         end if;
      end loop;
      Coord_Proche := Coords;

      --haut
      while (not Doublon and Coord_Proche.X >= Grille'First(1)) and then (Grille(Coord_Proche.X,Coord_Proche.Y).Car = 'L') loop
         if not (Coord_Proche = Coords) and then Grille(Coord_Proche.X,Coord_Proche.Y).Valeur_Test = Valeur then
            Doublon := True;
         else
            Coord_Proche.X := Coord_Proche.X-1;
         end if;
      end loop;

      return not Doublon;
   end Verif_Aucun_Doublons;


   --Vérifie si toute une ligne (peut y avoir plusieurs contraintes H) est bonne ou non
   function Ligne_Valide (Grille : in Une_Grille; Coord_L : in Coord) return Boolean is
      L_Valide : Boolean := True;
      Somme : Integer := 0;
      Contrainte_H : Integer := 0;
   begin
      for I in 1..Grille'Last(2) loop
         case Grille(Coord_L.X,I).car is
            when 'D' | 'H' => Contrainte_H := Grille(Coord_L.X,I).Contrainte_H;
               Somme := 0;
            when 'L' =>
               if Contrainte_H /= 0 then
                  if Grille(Coord_L.X, I).Valeur_test /= 0 then
                     Somme := Somme + Grille(Coord_L.X, I).Valeur_test;
                  else
                     L_Valide := False;
                  end if;
               end if;
            when others => Contrainte_H := 0;
               Somme := 0;
               --null;
         end case;

         if I < Grille'Last(2) and then (Grille(Coord_L.X,I+1).car /= 'L') and then (Somme /= Contrainte_H) then
            L_Valide := False;
         elsif I = Grille'Last (2) and then (Grille(Coord_L.X,I).car = 'L') and then (Somme /= Contrainte_H) then
            L_Valide := False;
         end if;
      end loop;

      return L_Valide;
   end Ligne_Valide;

   --Vérifie si toute une colonne (peut y avoir plusieurs contraintes V) est bonne ou pas
   function Colonne_Valide (Grille : in Une_Grille; Coord_C : in Coord) return Boolean is
      C_Valide : Boolean := true;
      Somme : Integer := 0;
      Contrainte_V : Integer := 0;
   begin
      for I in 1..Grille'Last(1) loop
         case Grille(I,Coord_C.Y).car is
            when 'D' | 'V' => Contrainte_V := Grille(I,Coord_C.Y).Contrainte_V;
               Somme := 0;
            when 'L' =>
               if Contrainte_V /= 0 then
                  if Grille(I,Coord_C.Y).Valeur_test /= 0 then
                     Somme := Somme + Grille(I,Coord_C.Y).Valeur_test;
                  else
                     C_Valide := False;
                  end if;
               end if;
            when others => Contrainte_V := 0;
               Somme := 0;
               --null;
         end case;


         if I < Grille'Last(1) and then (Grille(I+1,Coord_C.Y).car /= 'L') and then (Somme /= Contrainte_V) then
            C_Valide := False;
         elsif I = Grille'Last (1) and then (Grille(I,Coord_C.Y).car /= 'L') and then (Somme /= Contrainte_V) then
            C_Valide := False;
         end if;
      end loop;

      return C_Valide;
   end Colonne_Valide;

-- Vérif finale : la grille n'est pas valide si au moins une des colonnes ou des lignes ne l'est pas
   function Grille_Pas_Valide (Grille : in Une_Grille) return Boolean is
      G_Pas_Valide : Boolean := False;
      Ligne : Integer := Grille'First(1);
      Colonne : Integer := Grille'First(2);
   begin
      while not G_Pas_Valide and Ligne <= Grille'Last(1) loop
         if Ligne_Valide(Grille,(Ligne,Grille'First(2))) then
            Ligne := Ligne +1;
         else
            G_Pas_Valide := True;
         end if;
      end loop;

      while not G_Pas_Valide and Colonne <= Grille'Last(2) loop
         if Colonne_Valide(Grille,(Grille'First(1),Colonne)) then
            Colonne := Colonne +1;
         else
            G_Pas_Valide := True;
         end if;
      end loop;

      return G_Pas_Valide;
   end Grille_Pas_Valide;

     -- utilisée dans backtrack, renvoie la valeur Ã  tester après Valeur_test (la valeur Ã  true dans le tableau de booléens qui suit valeur_test)
  -- renvoie 0 lorsque la valeur_test est le sup des valeurs possibles de tab
   function Valeur_Suivante (Tab : Tab_Boolean ; Valeur_Test : Integer) return Integer is
      Val_Suiv : Integer := 0;
      Temp : Integer := Valeur_Test+1;
      Trouver : Boolean := False;
   begin
      while not Trouver and Temp <= 9 loop
         if Tab(Temp)  then
            Trouver := True;
            Val_Suiv := Temp;
         else
            Temp := Temp +1;
         end if;
      end loop;

      return Val_Suiv;
   end Valeur_Suivante;

   --fonction récursive
   procedure Backtrack_V2 (Grille : in out Une_Grille; Chemin : in Tab_Chemin; Etape: in out Integer; appels : in out Integer) is
      Val_Suiv : Integer := 0;
      Fin : Boolean := False;
      Coord_Case : Coord;
      Overflow : exception;
   begin
      appels := appels +1; --permet d'avoir le nombre de récursions effectué pour résoudre une grille

      if appels > 101000 then
         raise Overflow;
      end if;

      while (not Fin and then Grille_Pas_Valide(Grille)) and appels <= 101000  loop
         if Etape > Chemin'Last then
            Etape := Chemin'Last;
         elsif Etape < Chemin'First then
            Etape := Chemin'First;
         end if;
         Coord_Case := Chemin(Etape);

         --on cherche la prochaine valeur supérieure dans la case
         Val_Suiv := Valeur_Suivante(Grille(Chemin(Etape).X,Chemin(Etape).Y).Tab_Valeurs_Possibles, Grille(Chemin(Etape).X,Chemin(Etape).Y).Valeur_Test);

         Grille(Chemin(Etape).X,Chemin(Etape).Y).Valeur_Test := Val_suiv;
         while (not (Verif_Aucun_Doublons(Grille,Chemin(Etape),Val_Suiv) and (Verif_Somme_V2(Grille,Coord_Case)))) and Val_Suiv /= 0  loop
            Val_Suiv := Valeur_Suivante(Grille(Chemin(Etape).X,Chemin(Etape).Y).Tab_Valeurs_Possibles, Grille(Chemin(Etape).X,Chemin(Etape).Y).Valeur_Test);

            Grille(Chemin(Etape).X,Chemin(Etape).Y).Valeur_Test := Val_suiv;
         end loop;

         if Val_Suiv = 0 then --retour en arriêre car valeur_test est au max donc toutes les valeurs possibles ont été testées et aucune ne convient
            Grille(Chemin(Etape).X,Chemin(Etape).Y).Valeur_Test := 0;
            if Etape > Chemin'First then
               Fin := True;
            end if;
         else  --on a trouvé une valeur possible donc on avance d'une case
            Grille(Chemin(Etape).X,Chemin(Etape).Y).Valeur_Test := Val_Suiv;
            Etape := Etape +1;
            Backtrack_V2(Grille,Chemin,Etape,appels);
            Etape := Etape -1;
         end if;
      end loop;

   exception
      when Overflow =>
         Dialogue("Overflow : trop d'appels récursifs");
   end Backtrack_V2;

      -- utilisé comme critère de choix du chemin V2L
   function Egal_nb_contH (grille : in Une_Grille) return boolean is
      autant : boolean := true;
      ref : Integer := 0;
      ligne_deux : constant Integer := grille'first(1)+1;
      nb : Integer := 0;
   begin
      for L in grille'first(1)..grille'last(1) loop
         for C in grille'range(2) loop
            if L = ligne_deux and (grille(L,C).car ='H' or grille(L,C).car = 'D') then
               ref  := ref +1;
            elsif L /= ligne_deux and (grille(L,C).car ='H' or grille(L,C).car = 'D') then
               nb := nb +1;
            elsif L /= ligne_deux and C = grille'last(2) and nb /= ref then
               autant := false;
            elsif L/= ligne_deux and C = grille'last(2) and nb = ref then
               nb := 0;
            end if;
         end loop;
      end loop;
      return autant;
   end Egal_nb_contH;

   -- utilisé comme critère de choix du chemin V2L
   function Egal_nb_contV (grille : in Une_Grille) return boolean is
      autant : boolean := true;
      ref : Integer := 0;
      col_deux : constant Integer := grille'first(2)+1;
      nb : Integer := 0;
   begin
      for C in grille'first(2)+1..grille'last(2) loop
         for L in grille'range(1) loop
            if C = col_deux and (grille(L,C).car ='V' or grille(L,C).car = 'D') then
               ref  := ref +1;
            elsif C /= col_deux and (grille(L,C).car ='V' or grille(L,C).car = 'D') then
               nb := nb +1;
            elsif C /= col_deux and L = grille'last(1) and nb /= ref then
               autant := false;
            elsif C /= col_deux and L = grille'last(1) and nb = ref then
               nb := 0;
            end if;
         end loop;
      end loop;
      return autant;
   end Egal_nb_contV;

   --critère pour le choix automatique du meilleur chemin Ã  utiliser
   -- renvoie vrai si moins de contraintes H Ã  remplir que de contraintes V
   function Hinf_ou_egal (Grille : in Une_Grille) return boolean is
      nb_H : Integer := 0;
      nb_V : Integer := 0;
      Hinf : boolean := false;
   begin
      for L in grille'range(1) loop
         for C in grille'range(2) loop
            case grille(L,C).car is
               when 'V' => nb_V := nb_V +1;
               when 'H' => nb_H := nb_H +1;
               when 'D' => nb_V := nb_V +1;
                  nb_H := nb_H +1;
               when others => null;
            end case;
         end loop;
      end loop;
      if Nb_H <= nb_V then  --sinon Hinf reste Ã   l'init false
         Hinf := true;
      end if;
      return Hinf;
   end Hinf_ou_egal;

-- fonction renvoyant le chemin permettant de résoudre la grille en moins de récursions possibles d'après nos critères
   function choix_auto (Mat_grille : in Une_Grille) return Tab_chemin is
      Tab   : Tab_Chemin(1..Calculer_Nb_L(Mat_Grille)) := (others => (0,0));
   begin
      if Egal_nb_contH(Mat_grille) then      --V2 prioritaire
         Tab := Creer_Chemin_BrutV2L(Mat_grille);
         --Put("1");

      elsif Egal_nb_contV(Mat_grille) then   --V2 prioritaire
         Tab := Creer_Chemin_BrutV2C(Mat_grille);
         --Put("2");

      else
         if (Mat_grille'last(1) - Mat_grille'first(1)+1) = (Mat_grille'last(2) - Mat_grille'first(2)+1) then --grille carrée
            if not(Hinf_ou_egal(Mat_grille)) then
               Tab := Creer_Chemin_Brut_Colonne(Mat_grille);
               --Put("3");

            else
               Tab := Creer_Chemin_Brut_Ligne(Mat_grille);
               --Put("4");

            end if;
         else --grille rectangulaire
            if (Mat_grille'last(2) - Mat_grille'first(2)+1) > (Mat_grille'last(1) - Mat_grille'first(1)+1) then --Nb_Col > nb_Lig
               Tab := Creer_Chemin_Brut_Colonne(Mat_grille);
               --Put("3");

            else --Nb_Lig > nb_Col
               Tab := Creer_Chemin_Brut_Ligne(Mat_grille);
               --Put("4");

            end if;
         end if;
      end if;
      return Tab;
   end choix_auto;

--Lancer backrack avec le bon chemin
   procedure Backtracking (Mat_Grille : in out Une_Grille; Appels : out Integer) is
      Tab   : Tab_Chemin(1..Calculer_Nb_L(Mat_Grille)) := (others => (0,0));
      Etape : Integer := 1;
   begin
      Appels := 0;
      Tab := choix_auto(Mat_Grille);
      Backtrack_V2(Mat_Grille,Tab,Etape,Appels);
      if Appels <= 101000 then
         Put_line("Nb de recursions :" & Integer'Image(Appels));
      end if;
   end Backtracking;

   type Nombre is record
      --utilisé dans la conversion de manière à suppprimer l'espace automatique de Integer'image
      Prem : Integer := 0;
      Deux : Integer := 0;
      Seul : Integer := 0;
   end record;

  -- converti un nombre dans le code ASCII
  function Convert ( Dec : in Integer ) return Nombre is
          Nb : Nombre;
          Dec2 : Integer := Dec;
      begin
      if Dec <= 9 then
             Nb.Seul := Dec + 48;
      else
             Nb.Prem := Dec/10 + 48;
         while Dec2 >= 10 loop
                Dec2 := Dec2 - 10;
         end loop;
             Nb.Deux := Dec2 + 48;
      end if;

      return Nb;
   end Convert;

   function Integer_Image(I : in Nombre) return String is
    begin
      if I.Prem = 0 then
         return Character'Val(I.Seul)&"" ;
      else
         return Character'Val(I.Prem)&Character'Val(I.Deux);
      end if;
   end Integer_Image;




   --****AFFICHAGE DE GRILLE DANS LA CONSOLE****
   --plus utilisé depuis le passage en mode graphique avec gtkada

   --type Mat is array (Integer range 1..5,Integer range 1..8) of Character;

   --utilisé pour l'affichage console développé (une case est de dimension 5x8 caractères)
--        type Tab_Carac is array (Integer range 0..9) of Character;
--
--     --utilisé dans affichage final pour afficher la matrice de matrice de caractères, plus simplement
--     procedure Affiche_Ligne (Matr : in Mat; Ligne : Integer) is
--     begin
--        for Col in Matr'Range(2) loop
--           Put(Matr(Ligne,Col));
--        end loop;
--     end Affiche_Ligne;

   --affichage simpliste de la grille finale
--     procedure Affichage_Primaire (Mat_Grille : Une_Grille) is
--     begin
--        for I in Mat_Grille'Range(1) loop
--           for J in Mat_Grille'Range(2) loop
--              case Mat_Grille(I,J).Car is
--                 when 'N' => Put(" N");
--                 when 'L' => Put(Integer'Image(Mat_Grille(I,J).Valeur_Test));
--                 when 'V' => Put(" V");
--                 when 'H' => Put(" H");
--                 when 'D' => Put(" D");
--              end case;
--           end loop;
--           New_Line;
--        end loop;
--     end Affichage_Primaire;

   --Affichage complet de la grille--
   --procedure Affichage_Final (Grille : Une_Grille) is
--        Mat_N : Mat := (('#','#','#','#','#','#','#','#'),
--                        ('#','#','#','#','#','#','#','#'),
--                        ('#','#','#','#','#','#','#','#'),
--                        ('#','#','#','#','#','#','#','#'),
--                        ('#','#','#','#','#','#','#','#'));
--        X : Integer;
--        Mat_L : Mat := (('#','#','#','#','#','#','#','#'),
--                        ('#',' ',' ',' ',' ',' ',' ','#'),
--                        ('#',' ',' ',' ',' ',' ',' ','#'),
--                        ('#',' ',' ',' ',' ',' ',' ','#'),
--                        ('#','#','#','#','#','#','#','#'));
--        Mat_Cont : Mat :=  (('#','#','#','#','#','#','#','#'),
--                            ('#','\','_',' ',' ',' ',' ','#'),
--                            ('#',' ',' ','\','_',' ',' ','#'),
--                            ('#',' ',' ',' ',' ','\','_','#'),
--                            ('#','#','#','#','#','#','#','#'));
--        Tab_C : Tab_Carac := ('0','1','2','3','4','5','6','7','8','9');
--     begin
--        for Lig in Grille'Range(1) loop
--           for I in 1..5 loop
--              for Col in Grille'Range(2) loop
--                 case Grille(Lig,Col).Car is
--                    when 'N' => Affiche_Ligne(Mat_N,I);
--
--                    when 'L' =>
--                       X := Grille(Lig,Col).Valeur_Test;
--                       Mat_L(3,4) := Tab_C(X);
--                       Affiche_Ligne(Mat_L,I);
--                    when 'H' =>
--                       X := Grille(Lig,Col).Contrainte_H;
--                       if X < 10 then
--                          Mat_Cont(2,6) := Tab_C(X);
--                          Affiche_Ligne(Mat_Cont,I);
--                       else
--                          Mat_Cont(2,5) := Tab_C(X/10);
--                          while X >= 10 loop
--                             X := X -10;
--                          end loop;
--                          Mat_Cont(2,6) := Tab_C(X);
--                          Affiche_Ligne(Mat_Cont,I);
--                       end if;
--                    when 'V' =>
--                       X := Grille(Lig,Col).Contrainte_V;
--                       if X < 10 then
--                          Mat_Cont(4,3) := Tab_C(X);
--                          Affiche_Ligne(Mat_Cont,I);
--                       else
--                          Mat_Cont(4,3) := Tab_C(X/10);
--                          while X >= 10 loop
--                             X := X -10;
--                          end loop;
--                          Mat_Cont(4,4) := Tab_C(X);
--                          Affiche_Ligne(Mat_Cont,I);
--                       end if;
--                    when 'D' =>
--                       X := Grille(Lig,Col).Contrainte_H;
--                       if X < 10 then
--                          Mat_Cont(2,6) := Tab_C(X);
--                       else
--                          Mat_Cont(2,5) := Tab_C(X/10);
--                          while X >= 10 loop
--                             X := X -10;
--                          end loop;
--                          Mat_Cont(2,6) := Tab_C(X);
--                       end if;
--                       X := Grille(Lig,Col).Contrainte_V;
--                       if X < 10 then
--                          Mat_Cont(4,3) := Tab_C(X);
--                          Affiche_Ligne(Mat_Cont,I);
--                       else
--                          Mat_Cont(4,3) := Tab_C(X/10);
--                          while X >= 10 loop
--                             X := X -10;
--                          end loop;
--                          Mat_Cont(4,4) := Tab_C(X);
--                          Affiche_Ligne(Mat_Cont,I);
--                       end if;
--                    --when others => null;
--                 end case;
--                 Mat_Cont(4,3) := ' ';
--                 Mat_Cont(4,4) := ' ';
--                 Mat_Cont(2,5) := ' ';
--                 Mat_Cont(2,6) := ' ';
--              end loop;
--              New_Line;
--           end  loop;
--        end loop;
--     end Affichage_Final;

   type T_Fenetre is new Controlled with record
      Win : Gtk_Window; --fenêtre
      win2 : gtk_window;
      Btn : Gtk_Button; --bouton
      Lbl,lbl2 : Gtk_Label; --étiquette
      Tab,Tab2 : gtk_table; --tableau
      img : gtk_image; --image
      Saisie_Nom : Gtk_Entry; --zone de saisie
   end record;


   type T2_fenetre is record
      W1 : T_fenetre;
      W2 : T_fenetre;
   end record;

   --SOUS PACKAGES POUR MANIPULER LES CALLBACKS

   package P_UCallback is new Gtk.Handlers.User_Callback(Gtk_Widget_Record, Gtk_Label);
   use P_Ucallback;
   package P_UCallback2 is new Gtk.Handlers.User_Callback(Gtk_Widget_Record, Gtk_window);
   use P_Ucallback2;
   package P_UCallback3 is new Gtk.Handlers.User_Callback(Gtk_Widget_Record, T_Fenetre);
   use P_Ucallback3;
   package P_UCallback4 is new Gtk.Handlers.User_Callback(Gtk_Widget_Record, T2_Fenetre);
   use P_Ucallback4;
   package P_UCallback5 is new Gtk.Handlers.User_Callback(Gtk_Widget_Record, gtk_entry);
   use P_Ucallback5;
   package P_UCallback6 is new Gtk.Handlers.User_Callback(Gtk_Widget_Record, Une_grille);
   use P_UCallback6;
   PACKAGE P_UHandlers IS NEW Gtk.Handlers.User_Return_Callback(Gtk_Widget_Record, Boolean, T_Fenetre) ;
   USE P_UHandlers ;


  --crée la fenêtre dans laquelle sera affichée la grille avec redimensionnement de la fenêtre en fonction du nombre de ligne et de colonne de la grille.
   procedure Affichage_graphique(Grille : in Une_Grille; Lig,Col : Integer; Win : in out Gtk_Window) is
      Tab :Gtk_Table;
      Img,Img1,Img2 : Gtk_Image;
      Txt : Gtk_Label;
      Defilement  : GTK_Scrolled_Window ;
   begin
      Init;
      Gtk_New(Win);
      Win.Set_Title("Kakuro");
      if Lig < 11 and col < 11 then
         Win.Set_Default_Size(gint(Col)*55,Gint(Lig)*55);
      else
         Win.set_default_size(500,460);
      end if;

      Gtk_New(Defilement) ;
      Win.Add(Defilement) ;
      Gtk_New(Tab,Guint(Lig),Guint(Col),false);
      Defilement.Add_With_Viewport(tab);
      for L in Grille'range(1) loop
         for C in Grille'range(2) loop
            case Grille(L,C).car is
               when 'N' => Gtk_New(Img,"N.png");
                  Set_Row_Spacing(tab,guint(L)-1,4);
                  Set_Col_Spacing(tab,guint(C)-1,4);
                  Tab.Attach(Img,Guint(C)-1,Guint(C),Guint(L)-1,Guint(L));
               when 'L' => Gtk_New(txt,Integer'image(Grille(L,C).Valeur_Test)); --affiche valeur
                  Tab.attach(Txt,Guint(C)-1,Guint(C),Guint(L)-1,Guint(L));
                  Gtk_New(Img1, "L.png");
                  Set_Row_Spacing(tab,guint(L)-1,4);
                  Set_Col_Spacing(tab,guint(C)-1,4);
                  Tab.Attach(Img1,Guint(C)-1,Guint(C),Guint(L)-1,Guint(L)) ;
               when 'H' =>  Gtk_New(txt,"        "&Integer'image(Grille(L,C).Contrainte_H));
                  Tab.Attach(txt,Guint(C)-1,Guint(C),Guint(L)-1,Guint(L));
                  Gtk_New(Img2, "H.png");
                  Set_Row_Spacing(tab,guint(L)-1,4);
                  Set_Col_Spacing(tab,guint(C)-1,4);
                  Tab.Attach(Img2,Guint(C)-1,Guint(C),Guint(L)-1,Guint(L));
               when 'V' =>  Gtk_New(txt,Integer'image(Grille(L,C).Contrainte_V)&"        ");
                  Tab.Attach(txt,Guint(C)-1,Guint(C),Guint(L)-1,Guint(L));
                  Gtk_New(Img2, "V.png");
                  Set_Row_Spacing(tab,guint(L)-1,4);
                  Set_Col_Spacing(tab,guint(C)-1,4);
                  Tab.Attach(Img2,Guint(C)-1,Guint(C),Guint(L)-1,Guint(L));
               when 'D' =>  Gtk_New(txt,Integer'image(Grille(L,C).Contrainte_V)&"        ");
                  Tab.Attach(txt,Guint(C)-1,Guint(C),Guint(L)-1,Guint(L));
                  Gtk_New(txt,"        "&Integer'image(Grille(L,C).Contrainte_H));
                  Tab.Attach(txt,Guint(C)-1,Guint(C),Guint(L)-1,Guint(L));
                  Gtk_New(Img2, "D.png");
                  Set_Row_Spacing(tab,guint(L)-1,4);
                  Set_Col_Spacing(tab,guint(C)-1,4);
                  Tab.Attach(Img2,Guint(C)-1,Guint(C),Guint(L)-1,Guint(L));
            end case;
         end loop;
      end loop;

      connect(Win,"destroy",Stop_Program'Access);
      Win.show_all;
      Main;
   end Affichage_graphique;

   --routine du programme
   procedure Resolution (Li_Max,C_Max : in Integer; Mat_Grille : in out Une_Grille) is
      Win : Gtk_window;
      appels : Integer;
   begin
      Balayage_V(Mat_Grille);
      Balayage_H(Mat_grille);
      Opti(Mat_Grille);
      Lever_exception(Mat_Grille);
      if Grille_Pas_Valide(Mat_Grille) then
         Backtracking(Mat_Grille,appels);
      end if;
      if appels <= 101000 then
         Affichage_graphique(Mat_Grille,Li_Max,C_Max,Win);
      end if;
   end Resolution;

   --lancement des modules de résolution
   procedure Lancement (Emetteur : access Gtk_widget_Record'class;Label : Gtk_Label) is
      pragma Unreferenced (Emetteur);
      Li_Max : Integer;
      C_Max : Integer;
      fichier : File_type;
      fichier_rep : File_type;
   begin
      open(fichier_rep,In_File,"Repertoire.txt");
      if Get_Text(Label) = "1" then
         open(fichier,In_File,Get_Line(fichier_rep));
      else
         for I in 1..Integer'Value(Get_Text(Label))-1 loop
            Skip_Line(fichier_rep,1);
         end loop;
         open(fichier,In_File,Get_Line(fichier_rep));
      end if;
      close(fichier_rep);
      Recuperation_Max (Li_Max,C_Max,fichier);
      declare
         Mat_Grille : Une_Grille(1..Li_Max,1..C_Max);
      begin
         Creer_Grille (Mat_Grille,fichier);
         close(fichier);
         Resolution (Li_Max,C_Max,Mat_Grille);
      exception
         when use_error => Dialogue("Tentative de résoudre 2 fois la même grille impossible");
      end;
   end Lancement;



  --pour le mode jeu
  --détruire la fenêtre de choix et changer la valeur du label du bouton de la grille
   procedure destroy (Emetteur : access Gtk_widget_Record'class ; W: T2_Fenetre) is
      pragma Unreferenced (Emetteur);
      file1 : File_Type;
   begin
      Set_Label(W.W1.Btn,get_text(W.W2.Lbl)); --on récupère le label du bouton cliqué dans val_jeu1
      open(file1,Append_File,"jeu.txt"); --fichier permettant la vérification de la grille
      Put(file1,Get_name(W.W1.btn)&get_text(W.W2.Lbl));
      close(file1);
      destroy(W.W2.Win);
      W.W1.Win.show_all;
   end destroy;

   --fenêtre constituée de 9 boutons étiquetés de 1 Ã  9 (choix valeur case)
   function val_jeu1 (Emetteur : access Gtk_widget_Record'class ;evenement : gdk_event ;W: T_Fenetre) return boolean is
      pragma Unreferenced (Emetteur);
      num : Integer := 1;
      T2 : T2_fenetre;
      file1 : File_Type;
   begin
      T2.W1 := W;
      case get_button(evenement) is
         when 1 =>   -- clic gauche
            Gtk_New(T2.W2.Win);
            T2.W2.Win.Set_Title("Valeur case");
            T2.W2.Win.set_default_size(260,260);
            Gtk_new(T2.W2.Tab,3,3,false);
            T2.W2.win.add(T2.W2.Tab);
            for L in 1..3 loop
               for C in 1..3 loop
                  gtk_new(T2.W2.Lbl,Integer_image(convert(Num)));
                  gtk_new(T2.W2.btn,Integer'image(Num));
                  Num := num +1;
                  T2.W2.Tab.attach(T2.W2.btn,Guint(C)-1,Guint(C),Guint(L)-1,Guint(L));
                  connect(T2.W2.btn,"clicked",destroy'access,T2);
               end loop;
            end loop;
            T2.W2.Win.show_all;
         when 3 => set_label(W.Btn,""); -- clic droit
            open(file1,Append_File,"jeu.txt"); --fichier permettant la vérification de la grille
            Put(file1,Get_name(T2.W1.btn)& "0");
            close(file1);
         when others => null;
      end case;

      return false;
   end val_jeu1;


   --procédure éxécutée lors du clic sur "vérifier" dans le mode jeu
   --vérifie toutes les sommes (utilise Grille_Pas_Valide) en récupérant toutes les données utiles dans jeu.txt
   procedure  verifier_jeu1(Emetteur : access Gtk_widget_Record'class ;grille : Une_grille) is
      pragma Unreferenced (Emetteur);
      W : T_Fenetre;
      file : file_type;
      grille2 : une_grille := grille;
   begin
      open(file,in_file,"jeu.txt");
      reset(file);
       while not End_Of_File(File) loop
         declare
            ligne : constant string := get_line(File);
            Fin : Boolean := false;
            Num : Integer := Ligne'First;
            lig : Integer := 0;
            col : Integer := 0;
            virgule,deux_points : Integer := 0;
         begin
            while Num <= Ligne'Last and not Fin loop
               if Ligne(Num) = ',' then
                  Virgule := Num;
                  Num := Num +1;
               elsif Ligne(Num) = ':' then
                  Deux_Points := Num;
                  Fin := True;
               else
                  Num := Num +1;
               end if;
            end loop;
            lig := Integer'value(Ligne(ligne'first..virgule-1));
            col := Integer'value(Ligne(virgule+1..deux_points-1));
            Grille2(Lig,Col).Valeur_Test := Integer'value(Ligne(deux_points+1..ligne'last));
         end;
      end loop;
      close(file);
      gtk_new(W.win2);
      W.win2.set_title("Resultat");
      W.Win2.set_default_size(200,200);
      Gtk_new(W.Tab2,1,1,true);
      if Grille_Pas_Valide(Grille2) then
         gtk_new(W.img,"grille_pas_valide.png");
         w.Tab2.attach(W.img,0,1,0,1);
      else
         gtk_new(W.img,"grille_valide.png");
         W.Tab2.attach(W.img,0,1,0,1);
      end if;


      W.win2.add(W.Tab2);
      W.win2.show_all;
      connect(W.win2,"destroy",Stop_Windows'access);
   end verifier_jeu1;

   --principe d'affichage graphique mais en remplaçat les images des cases 'L' par des boutons connectés à une fenêtre de choix de valeurs (1..9)
   procedure Afficher_Jeu (Grille : in Une_Grille; Lig,Col : Integer) is
      Img,Img2 : Gtk_Image;
      Txt : Gtk_Label;
      T : T_Fenetre;
      Defilement : GTK_Scrolled_Window ;
      btn1 : gtk_button;
      file : File_Type;
   begin
      create(File,in_file,"jeu.txt");
      close(file);
      Init;
      Gtk_New(T.Win);
      T.Win.set_title("Remplir");
      if Lig < 11 and col < 11 then
         T.Win.Set_Default_Size(gint(Col)*55,Gint(Lig)*55+60);
      else
         T.Win.set_default_size(500,460);
      end if;
      Gtk_New(T.Tab,Guint(Lig)+1,Guint(Col),true);
      gtk_new(btn1,"Verifier");
      T.tab.attach(btn1,0,Guint(Col),Guint(Lig),Guint(Lig)+1);
      connect(btn1,"clicked",verifier_jeu1'access,grille);
      Gtk_New(Defilement);
      T.Win.Add(Defilement);
      Defilement.Add_With_Viewport(T.tab);

      for L in Grille'range(1) loop
         for C in Grille'range(2) loop
            case Grille(L,C).car is
               when 'N' => Gtk_New(Img,"N.png");
                  Set_Row_Spacing(T.tab,guint(L)-1,4);
                  Set_Col_Spacing(T.tab,guint(C)-1,4);
                  T.Tab.Attach(Img,Guint(C)-1,Guint(C),Guint(L)-1,Guint(L));
               when 'L' =>
                  Gtk_New(T.btn,"");
                  T.Tab.Attach(T.btn,Guint(C)-1,Guint(C),Guint(L)-1,Guint(L));
                  Connect(T.Btn, "button_press_event", to_Marshaller(val_jeu1'ACCESS), T) ;
                  set_name(T.btn,Integer_image(convert(L))&","&Integer_image(convert(C))&":");
               when 'H' =>  Gtk_New(txt,"        "&Integer'image(Grille(L,C).Contrainte_H));
                  T.Tab.Attach(txt,Guint(C)-1,Guint(C),Guint(L)-1,Guint(L));
                  Gtk_New(Img2, "H.png");
                  Set_Row_Spacing(T.tab,guint(L)-1,4);
                  Set_Col_Spacing(T.tab,guint(C)-1,4);
                  T.Tab.Attach(Img2,Guint(C)-1,Guint(C),Guint(L)-1,Guint(L));
               when 'V' =>  Gtk_New(txt,Integer'image(Grille(L,C).Contrainte_V)&"        ");
                  T.Tab.Attach(txt,Guint(C)-1,Guint(C),Guint(L)-1,Guint(L));
                  Gtk_New(Img2, "V.png");
                  Set_Row_Spacing(T.tab,guint(L)-1,4);
                  Set_Col_Spacing(T.tab,guint(C)-1,4);
                  T.Tab.Attach(Img2,Guint(C)-1,Guint(C),Guint(L)-1,Guint(L));
               when 'D' =>  Gtk_New(txt,Integer'image(Grille(L,C).Contrainte_V)&"        ");
                  T.Tab.Attach(txt,Guint(C)-1,Guint(C),Guint(L)-1,Guint(L));
                  Gtk_New(txt,"        "&Integer'image(Grille(L,C).Contrainte_H));
                  T.Tab.Attach(txt,Guint(C)-1,Guint(C),Guint(L)-1,Guint(L));
                  Gtk_New(Img2, "D.png");
                  Set_Row_Spacing(T.tab,guint(L)-1,4);
                  Set_Col_Spacing(T.tab,guint(C)-1,4);
                  T.Tab.Attach(Img2,Guint(C)-1,Guint(C),Guint(L)-1,Guint(L));
            end case;
         end loop;
      end loop;

      connect(T.win,"destroy",Stop_Program'access);
      T.Win.show_all;
      main;
   end Afficher_Jeu;


   --éxécutée lors du choix du mode "visualiser une grille"
   --affiche une grille graphique (choisie par l'utilisateur) non résolue
   procedure Afficher_visu (Grille : in Une_Grille; Lig,Col : Integer) is
      Img,Img1,Img2 : Gtk_Image;
      Txt : Gtk_Label;
      T : T_Fenetre;
      Defilement  : GTK_Scrolled_Window ;
   begin
      Init;
      Gtk_New(T.Win);
      T.Win.Set_Title("Visuel");
      if Lig < 11 and col < 11 then
         T.Win.Set_Default_Size(gint(Col)*55,Gint(Lig)*55);
      else
         T.Win.set_default_size(500,460);
      end if;
      Gtk_New(T.Tab,Guint(Lig),Guint(Col),false);

      Gtk_New(Defilement) ;
      T.Win.Add(Defilement) ;
      Defilement.Add_With_Viewport(T.tab);
      for L in Grille'range(1) loop
         for C in Grille'range(2) loop
            case Grille(L,C).car is
               when 'N' => Gtk_New(Img,"N.png");
                  Set_Row_Spacing(T.tab,guint(L)-1,4);
                  Set_Col_Spacing(T.tab,guint(C)-1,4);
                  T.Tab.Attach(Img,Guint(C)-1,Guint(C),Guint(L)-1,Guint(L));
               when 'L' => Gtk_New(Img1, "L.png");
                  Set_Row_Spacing(T.tab,guint(L)-1,4);
                  Set_Col_Spacing(T.tab,guint(C)-1,4);
                  T.Tab.Attach(Img1,Guint(C)-1,Guint(C),Guint(L)-1,Guint(L));
               when 'H' =>  Gtk_New(txt,"        "&Integer'image(Grille(L,C).Contrainte_H));
                  T.Tab.Attach(txt,Guint(C)-1,Guint(C),Guint(L)-1,Guint(L));
                  Gtk_New(Img2, "H.png");
                  Set_Row_Spacing(T.tab,guint(L)-1,4);
                  Set_Col_Spacing(T.tab,guint(C)-1,4);
                  T.Tab.Attach(Img2,Guint(C)-1,Guint(C),Guint(L)-1,Guint(L));
               when 'V' =>  Gtk_New(txt,Integer'image(Grille(L,C).Contrainte_V)&"        ");
                  T.Tab.Attach(txt,Guint(C)-1,Guint(C),Guint(L)-1,Guint(L));
                  Gtk_New(Img2, "V.png");
                  Set_Row_Spacing(T.tab,guint(L)-1,4);
                  Set_Col_Spacing(T.tab,guint(C)-1,4);
                  T.Tab.Attach(Img2,Guint(C)-1,Guint(C),Guint(L)-1,Guint(L));
               when 'D' =>  Gtk_New(txt,Integer'image(Grille(L,C).Contrainte_V)&"        ");
                  T.Tab.Attach(txt,Guint(C)-1,Guint(C),Guint(L)-1,Guint(L));
                  Gtk_New(txt,"        "&Integer'image(Grille(L,C).Contrainte_H));
                  T.Tab.Attach(txt,Guint(C)-1,Guint(C),Guint(L)-1,Guint(L));
                  Gtk_New(Img2, "D.png");
                  Set_Row_Spacing(T.tab,guint(L)-1,4);
                  Set_Col_Spacing(T.tab,guint(C)-1,4);
                  T.Tab.Attach(Img2,Guint(C)-1,Guint(C),Guint(L)-1,Guint(L));
            end case;
         end loop;
      end loop;

      connect(T.win,"destroy",Stop_Program'access);
      T.Win.show_all;
      main;
   end Afficher_visu;

   procedure visuel_connect (emetteur : access Gtk_widget_Record'class;Label : Gtk_Label) is
      pragma Unreferenced (Emetteur);
      Li_Max : Integer;
      C_Max : Integer;
      fichier : File_type;
      fichier_rep : File_type;
      lbl : constant string := get_text(label);
   begin
      open(fichier_rep,In_File,"Repertoire.txt");
      if Integer'image(lbl'first) = "1" then
         open(fichier,In_File,Get_Line(fichier_rep));
      else
         if lbl(lbl'last) = 'J' then
            for I in 1..Integer'Value(lbl(lbl'first..lbl'last-1))-1 loop
               Skip_Line(fichier_rep,1);
            end loop;
         else
            for I in 1..Integer'Value(lbl)-1 loop
               Skip_Line(fichier_rep,1);
            end loop;
         end if;
         open(fichier,In_File,Get_Line(fichier_rep));
      end if;
      close(fichier_rep);
      Recuperation_Max (Li_Max,C_Max,fichier);

      declare
         Mat_Grille : Une_Grille(1..Li_Max,1..C_Max);
         fichier_jeu : File_type;
      begin
         Creer_Grille (Mat_Grille,fichier);
         close(fichier);
         if lbl(lbl'last) = 'J' then
            open(fichier_jeu,In_File,"jeu.txt");
            Dialogue("Une grille de jeu a déjà  été ouverte");
            close(fichier_jeu);
         else
            Balayage_V(Mat_Grille);
            Balayage_H(Mat_grille);
            Opti(Mat_Grille);
            Afficher_visu(Mat_Grille,Li_Max,C_Max);
         end if;


      exception
         when use_error => Dialogue("Tentative de résoudre 2 fois la même grille impossible");
         when name_error => Afficher_Jeu(Mat_Grille,Li_max,C_max);
            open(fichier_jeu,In_File,"jeu.txt");
            Delete(fichier_jeu);
      end;
   end visuel_connect;


     ----crée un label pour le put dans le fichier grille
     --utilisé pour construire un gtk_label à partir d'une chaîne de caractères
   function Creer_Label_Fichier (Nom1,Nom2 : String) return Gtk_Label is
      Label_Fichier : Gtk_Label;
   begin
      Gtk_New(Label_Fichier, Nom1 & Nom2);
      return Label_Fichier;
   end Creer_Label_Fichier;

   --affiche le contenu du répertoire dans une nouvelle fenetre
   --chaque fichier est alors sélectionnable par clic
   --fenêtre utilisée dans 3 modes : visualiser, résoudre et jouer
   procedure Afficher_contenu (Emetteur : access Gtk_widget_Record'class;Lbl : gtk_label) is
      pragma Unreferenced (Emetteur);
      fichier_repert : File_type;
      Win_Rep : Gtk_Window;
      Tab : Gtk_Table;
      Num : Integer := 1;
      Btn : Gtk_Button;
      Label : Gtk_Label;
      Defilement  : GTK_Scrolled_Window ;
   begin
      Init;
      Gtk_New(Win_Rep);
      Win_Rep.Set_Title("Repertoire");
      Win_Rep.Set_Default_Size(500,500);
      Gtk_New(Defilement) ;
      Win_rep.Add(Defilement) ;
      Gtk_New(Tab,1,1,True);
      Defilement.Add_With_Viewport(tab);
      open(fichier_repert,In_File,"Repertoire.txt");
      while not End_Of_File(fichier_repert) loop
         Gtk_New_With_Mnemonic(Btn,Integer'Image(Num) & "-" & Get_Line(fichier_repert)) ;
         Tab.attach(btn,0,1,Guint(Num)-1,Guint(Num));
         Gtk_New(Label,Integer'Image(Num));
         if Get_text(Lbl) = "1" then
            Connect(Btn,"clicked", Lancement'ACCESS, Label);
         elsif Get_text(Lbl) = "4" then
            Connect(Btn,"clicked", visuel_connect'ACCESS,Creer_Label_Fichier(Get_text(Label),"J"));
         else
            Connect(Btn,"clicked", visuel_connect'ACCESS, Label);
         end if;
         Num := Num + 1;
      end loop;
      close(fichier_repert);

      connect(win_rep,"destroy",Stop_Program'access);
      Win_Rep.show_all;
      Main;
   end Afficher_contenu;

   --
   -- CREATION DE GRILLE
   --

--récupérer le nom de la grille venant d'être crée (dernière ligne du fichier
   function Recuperer_Nom (fichier_repert : in out File_type) return String is
      Num : Integer := 1;
   begin
      while not End_Of_File(fichier_repert) loop
         Skip_Line(fichier_repert,1);
         Num := Num +1;
      end loop;
      Reset(fichier_repert);
      for I in 1..Num-2 loop
         Skip_Line(fichier_repert,1);
      end loop;
      return Get_Line(fichier_repert); --nom de la grille créée
   end Recuperer_Nom;

   procedure Ecrire_Fichier2(Emetteur : access Gtk_widget_Record'class; T2 :T2_fenetre) is
      pragma Unreferenced (Emetteur);
      fichier_repert : File_type;
      fichier : File_type;
      Texte : constant String := Get_label(T2.W1.lbl);
      txt : gtk_label;
      virgule,point,tiret : Integer;
      trouver_V,trouver_P,trouver_T : boolean := false;
      num : Integer := texte'first;
      ligne,colonne : guint;
      img : gtk_image;
      car : character;

   begin
      while not trouver_V loop
         if Texte(Num) = ',' then
            trouver_V := true;
            virgule := num;
         else
            Num := num +1;
         end if;
      end loop;
      Num := virgule;
      while not trouver_P loop
         if Texte(Num) = ':' then
            trouver_P := true;
            point := num;
         else
            Num := num +1;
         end if;
      end loop;
      car := texte(point+1);
      Get_size(T2.W2.Tab2,Ligne,Colonne);
      case car is
         when 'H' => Set_Row_Spacing(T2.W2.tab2,guint'value(Texte(texte'first..virgule-1))-1,4);
            Set_Col_Spacing(T2.W2.Tab2,guint'value(Texte(virgule+1..point-1))-1,4);
            gtk_new(txt,"        "&Texte(point+2..texte'last));
            T2.W2.Tab2.attach(txt,guint(Integer'value(Texte(virgule+1..point-1)))-1,guint(Integer'value(Texte(virgule+1..point-1))),guint(Integer'value(Texte(texte'first..virgule-1)))-1,guint(Integer'value(Texte(texte'first..virgule-1))));
            gtk_new(img,"H.png");
            T2.W2.tab2.attach(img,guint(Integer'value(Texte(virgule+1..point-1)))-1,guint(Integer'value(Texte(virgule+1..point-1))),guint(Integer'value(Texte(texte'first..virgule-1)))-1,guint(Integer'value(Texte(texte'first..virgule-1))));
            if Guint'value(Texte(texte'first..virgule-1)) = ligne and Guint'value(Texte(virgule+1..point-1)) = colonne then

               destroy(T2.W2.win2);
            end if;
            T2.W2.win2.show_all;

         when 'V' => Set_Row_Spacing(T2.W2.Tab2,guint'value(Texte(texte'first..virgule-1))-1,4);
            Set_Col_Spacing(T2.W2.Tab2,guint'value(Texte(virgule+1..point-1))-1,4);
            gtk_new(txt,Texte(point+2..texte'last)&"       "); --point + 1
            T2.W2.Tab2.attach(txt,guint(Integer'value(Texte(virgule+1..point-1)))-1,guint(Integer'value(Texte(virgule+1..point-1))),guint(Integer'value(Texte(texte'first..virgule-1)))-1,guint(Integer'value(Texte(texte'first..virgule-1))));
            gtk_new(img,"V.png");
            T2.W2.tab2.attach(img,guint(Integer'value(Texte(virgule+1..point-1)))-1,guint(Integer'value(Texte(virgule+1..point-1))),guint(Integer'value(Texte(texte'first..virgule-1)))-1,guint(Integer'value(Texte(texte'first..virgule-1))));
            if Guint'value(Texte(texte'first..virgule-1)) = ligne and Guint'value(Texte(virgule+1..point-1)) = colonne then

               destroy(T2.W2.win2);
            end if;
            T2.W2.win2.show_all;
         when 'D' => Set_Row_Spacing(T2.W2.Tab2,guint'value(Texte(texte'first..virgule-1))-1,4);
            Set_Col_Spacing(T2.W2.Tab2,guint'value(Texte(virgule+1..point-1))-1,4);
            while not trouver_T loop
               if Texte(Num) = '-' then
                  trouver_T := true;
                  tiret := num;
               else
                  Num := num +1;
               end if;
            end loop;
            gtk_new(txt,Texte(Tiret+1..texte'last)&"      "&Texte(point+2..tiret-1)); --point + 1
            T2.W2.Tab2.attach(txt,guint(Integer'value(Texte(virgule+1..point-1)))-1,guint(Integer'value(Texte(virgule+1..point-1))),guint(Integer'value(Texte(texte'first..virgule-1)))-1,guint(Integer'value(Texte(texte'first..virgule-1))));
            gtk_new(img,"D.png");
            T2.W2.tab2.attach(img,guint(Integer'value(Texte(virgule+1..point-1)))-1,guint(Integer'value(Texte(virgule+1..point-1))),guint(Integer'value(Texte(texte'first..virgule-1)))-1,guint(Integer'value(Texte(texte'first..virgule-1))));
            if Guint'value(Texte(texte'first..virgule-1)) = ligne and Guint'value(Texte(virgule+1..point-1)) = colonne then

               destroy(T2.W2.win2);
            end if;
            T2.W2.win2.show_all;
         when others => null;
      end case;

      open(fichier_repert,In_File,"Repertoire.txt");
      declare
         Nom : constant String := Recuperer_Nom(fichier_repert);
      begin
         close(fichier_repert);
         open(fichier,Append_File,Nom);
         Put_Line(fichier,Texte);
         Destroy(T2.W1.Win);
         close(fichier);
      end;
   end Ecrire_fichier2;




--fenêtre permettant de choisir la deuxième contrainte ('V') d'une case contrainte double
   procedure DeuxiemeD (Emetteur : access Gtk_widget_Record'class;T2 : T2_Fenetre) is
      pragma Unreferenced (Emetteur);
      Num : Integer := 1;
      W : T_Fenetre;
      T2_2 : T2_fenetre;
   begin
      destroy(T2.W1.win);
      gtk_new(W.Win,Window_Toplevel);
      W.win.Set_Title("Contrainte Verticale");
      W.win.Set_default_size(300,300);
      Gtk_New(W.Tab,5,9,True);
      W.win.add(W.Tab);
      for L in 1..5 loop
         for C in 1..9 loop
            if Num > 2 then
               Gtk_New(W.Btn,Integer'Image(Num));
               Gtk_New(W.lbl,Get_Label(T2.W1.lbl)&"-"&Integer_image(convert(Num)));
               W.Tab.Attach(W.Btn,Guint(C)-1,Guint(C),Guint(L)-1,Guint(L)); -- Nom1 & "-" & Nom2
               T2_2 := (W,T2.W2);
              connect(W.btn,"clicked",Ecrire_Fichier2'access,T2_2);
             else
               Gtk_New(W.img,"N.png");
               W.Tab.Attach(W.Img,Guint(C)-1,Guint(C),Guint(L)-1,Guint(L));
               null;
            end if;
            Num := Num+1;
         end loop;
      end loop;
      W.Win.show_all;

   end DeuxiemeD;


   --OUVRE UNE NOUVELLE FENETRE POUR CHOISIR LA CONTRAINTE
   procedure Window_Choix(Car : character; Li,Co : Integer; wind : t_fenetre) is -- wind.win2 => visuel avec son Tab2
       W : T_fenetre;
      Num : Integer := 1;
      T2 : T2_fenetre;
      img : gtk_image;
      Ligne,colonne : guint;
   begin
    case car is
         when 'H' | 'D'  =>  Gtk_New(W.Win,Window_Toplevel);
            W.Win.Set_default_size(300,300);
            W.Win.Set_Title("Contrainte Horizontale");
            Gtk_New(W.Tab,5,9,True);
            W.Win.add(W.Tab);
         when 'V' => Gtk_New(W.Win,Window_Toplevel);
            W.Win.Set_default_size(300,300);
            W.Win.Set_Title("Contrainte Verticale");
            Gtk_New(W.Tab,5,9,True);
            W.Win.add(W.Tab);
         when others => null;
      end case;
      Get_size(Wind.Tab2,Ligne,Colonne);
      case car is
         when 'L' => Set_Row_Spacing(Wind.tab2,guint(Li)-1,4);
            Set_Col_Spacing(Wind.tab2,guint(Co)-1,4);
            gtk_new(img,"L.png");
            Wind.tab2.attach(img,Guint(Co)-1,Guint(Co),Guint(Li)-1,Guint(Li));
            wind.win2.show_all;
            if Guint(Li) = ligne and Guint(Co) = colonne then

               destroy(Wind.win2);
            end if;
            when 'N' => Set_Row_Spacing(wind.tab2,guint(Li)-1,4);
            Set_Col_Spacing(Wind.tab2,guint(Co)-1,4);
            gtk_new(img,"N.png");
            Wind.tab2.attach(img,Guint(Co)-1,Guint(Co),Guint(Li)-1,Guint(Li));
            wind.win2.show_all;
            if Guint(Li) = ligne and Guint(Co) = colonne then

               destroy(Wind.win2);
            end if;
         when others => null;
      end case;


      if car = 'V' or car = 'D' or car = 'H' then
         for L in 1..5 loop
            for C in 1..9 loop
               if Num > 2 then
                  Gtk_New(W.Btn,Integer'Image(Num));
                  W.Tab.Attach(W.Btn,Guint(C)-1,Guint(C),Guint(L)-1,Guint(L));
                  case Car is
                  when 'H' => Gtk_new(W.lbl,Integer_Image(convert(Li))&","&Integer_image(convert(Co))&":"&Car&Integer_Image(Convert(Num)));

                     T2 := (W,wind);
                     Connect(W.Btn,"clicked", Ecrire_Fichier2'ACCESS,T2);
          when 'D' =>  Gtk_new(W.lbl,Integer_Image(convert(Li))&","&Integer_image(convert(Co))&":"&Car&Integer_Image(Convert(Num)));
                     T2 := (W,wind);
                     Connect(W.Btn,"clicked", DeuxiemeD'ACCESS,T2); --ouvre une deuxième fenêtre pour la deuxième contrainte
                  when 'V' =>  Gtk_new(W.lbl,Integer_Image(convert(Li))&","&Integer_image(convert(Co))&":"&Car&Integer_Image(Convert(Num)));
                     T2 := (W,wind);
                     Connect(W.Btn,"clicked", Ecrire_fichier2'ACCESS,T2);
                  when others => null;
                  end case;
               else
                  Gtk_New(W.img,"N.png");
                  W.Tab.Attach(W.Img,Guint(C)-1,Guint(C),Guint(L)-1,Guint(L));
                  null;
               end if;
               Num := Num+1;
            end loop;
         end loop;

         W.Win.show_all;
      end if;
   end Window_Choix;


   PROCEDURE Finish(Emetteur : access Gtk_widget_Record'class;win : gtk_window) IS
      PRAGMA Unreferenced (Emetteur );
   BEGIN
      destroy(win);
   END finish ;


   --mise à jour => fenêtre constituée des 5 boutons (N,L,H,V,D) avec pour titre les coordonnées de la case à renseigner
   --(lors de la création d'une grille) et de la taille de la grille
   procedure Maj (wind : T_Fenetre; L_Max,C_Max : Integer; Car : character) is
      Texte : constant string := Get_title(Wind.win);
      virg : Integer;
      trouver : boolean := false;
      Num : Integer := Texte'first;
      Trouver_S : boolean := false;
      slash : Integer ;
      fichier,fichier_repert : file_type;
      win1 : gtk_window;
      label1 : gtk_label;
      btn1 : gtk_button;
      tab : gtk_table;
   begin
      while not trouver  loop
         if texte(num) = ',' then
            trouver := true;
            virg := num;
         else
            Num := Num +1;
         end if;
      end loop;
      Num := virg;
      while not Trouver_S loop
         if Texte(Num) = '/' then
            Trouver_S := true;
            slash := num;
         else
            Num := Num +1;
         end if;
      end loop;

      if Integer'value(Texte(virg+1..slash-1)) = C_Max and then Integer'value(Texte(Texte'first..Virg-1)) /= L_Max then
         Wind.win.Set_title(Integer_image(convert(Integer'value(Texte(Texte'first..Virg-1))+1))&","&Integer_image(convert(1))&"/"&Integer_image(convert(L_Max))&"-"&Integer_image(convert(C_Max)));
      elsif Integer'value(Texte(virg+1..slash-1)) = C_Max and then Integer'value(Texte(Texte'first..Virg-1)) = L_Max then
         open(fichier_repert,In_File,"Repertoire.txt");
            declare
            Nom : constant String := Recuperer_Nom(fichier_repert);
            begin
            Gtk_new(Win1,window_toplevel);
            Win1.set_default_size(300,300);
            Win1.set_title("Message de fin");
            gtk_new(Tab,2,1,True);
            Win1.add(Tab);
            Gtk_new(label1,"Le fichier "&Nom&" a ete genere.");
            Tab.attach(label1,0,1,0,1);
            Gtk_new_with_mnemonic(btn1," Terminer ");
             Tab.attach(btn1,0,1,1,2);
            connect(btn1,"clicked",finish'access,win1);
            Win1.show_all;
            destroy(Wind.win);
            end;
         close(fichier_repert);
      else
         Wind.Win.Set_title(Integer_image(convert(Integer'value(Texte(Texte'first..Virg-1))))&","&Integer_image(convert(Integer'value(Texte(Virg+1..slash-1))+1))&"/"&Integer_image(convert(L_Max))&"-"&Integer_image(convert(C_Max)));
      end if;

      case car is
         when 'L' => null;
            Window_Choix('L',Integer'value(Texte(Texte'first..Virg-1)),Integer'value(Texte(Virg+1..slash-1)),wind);
         when 'N' =>
            Window_Choix('N',Integer'value(Texte(Texte'first..Virg-1)),Integer'value(Texte(Virg+1..slash-1)),wind);
            open(fichier_repert,In_File,"Repertoire.txt");
            declare
               Nom : constant String := Recuperer_Nom(fichier_repert);
            begin
               open(fichier,Append_File,Nom);
               Put_Line(fichier,Integer_image(convert(Integer'value(Texte(Texte'first..Virg-1))))&","&Integer_image(convert(Integer'value(Texte(Virg+1..slash-1))))&":N");
               close(fichier);
               close(fichier_repert);
            end;

         when 'V' => Window_Choix('V',Integer'value(Texte(Texte'first..Virg-1)),Integer'value(Texte(Virg+1..slash-1)),wind);
         when 'H' => Window_Choix('H',Integer'value(Texte(Texte'first..Virg-1)),Integer'value(Texte(Virg+1..slash-1)),wind);
         when 'D' => Window_Choix('D',Integer'value(Texte(Texte'first..Virg-1)),Integer'value(Texte(Virg+1..slash-1)),wind);
         when others => null;
      end case;
      end Maj;


   procedure WindowD(Emetteur : access Gtk_Widget_Record'Class; window : T_fenetre) is
      pragma Unreferenced (Emetteur);
      slash,tiret : Integer ;
      Texte : constant string := Get_title(Window.win);
      Num : Integer := Texte'first;
      Trouver_S,Trouver_T : boolean := false;
      intermediaire : constant string := Get_label(window.lbl)&"";
   begin

      while not Trouver_S loop
         if Texte(Num) = '/' then
            Trouver_S := true;
            slash := num;
         else
            Num := Num +1;
         end if;
      end loop;
      Num := slash;
      --Put_Line(Integer'image(Num));
      while not Trouver_T loop
         if Texte(Num) = '-' then
            Trouver_T := true;
            tiret := num;
         else
            Num := Num +1;
         end if;

      end loop;
      Maj(Window,Integer'value(Texte(Slash+1..tiret-1)),Integer'value(Texte(Tiret+1..Texte'last)),intermediaire(1));
   end WindowD;


   --OUVRE UNE FENETRE POUR CHOISIR LE TYPE DE CASE
   procedure Window_type(Emetteur : access Gtk_Widget_Record'Class; wind : t_fenetre) is
      pragma Unreferenced (Emetteur);
      Texte : constant string := Get_Label(wind.lbl);
      Nom_Grille : constant String := Get_Label(wind.Lbl2);
      Trouver : Boolean := False;
      tiret : Integer := 1; --Label.Get_Text'First
      NUm : Integer := 1;
      Num2 : Integer :=1;
      fichier,fichier_repert : file_type;
      L_Max,C_Max : Integer;
      F : t_fenetre;
      Defilement : Gtk_Scrolled_Window;
   begin

      open(fichier_repert,In_File,"Repertoire.txt");
      reset(fichier_repert);
      while not End_Of_File(fichier_repert) loop
         Skip_Line(fichier_repert,1);
         Num2 := Num2 +1;
      end loop;
      close(fichier_repert);
      open(fichier_repert,append_File,"Repertoire.txt");

      Put_Line(fichier_repert,Nom_Grille & ".txt");
      close(fichier_repert);

       while Num /= Texte'Last and not Trouver and Texte(Texte'first) = '%' loop
         if Texte(Num) = '-' then
            tiret := Num;
            Trouver := true;
         else
            Num:=Num+1;
         end if;
      end loop;

      if Texte(Texte'first) = '%' then

         create(fichier,Out_File,Nom_Grille & ".txt");
         Put_Line(fichier, Integer_Image(Convert(Integer'Value(Texte(Texte'first+1..Tiret-1)))) & "-" & Integer_Image(Convert(Integer'Value(texte(Tiret+1..Texte'Last)))));
         Close(fichier);

         L_Max := Integer'Value(Texte(Texte'first+2..Tiret-1));
         C_Max := Integer'Value(texte(Tiret+2..Texte'Last));

         Gtk_New(F.Win,Window_Toplevel);
         F.Win.Set_Title("1,1/"&Integer_Image(convert(L_Max))&"-"&Integer_image(convert(C_Max)));
         F.Win.set_default_size(250,100);
         move(F.win,800,150);
         Gtk_New(F.Win2,Window_Toplevel);
         F.Win2.Set_Title("Visuel : ne pas toucher");
         F.Win2.set_default_size(200,200);
         move(F.win2,800,400);
      end if;


      --Visuel
      gtk_new(Defilement);
      F.Win2.add(Defilement);
      Gtk_new(F.Tab2,Guint(L_Max),Guint(C_Max),true);
      Defilement.Add_With_Viewport(F.Tab2);


      Gtk_New(F.Tab,1,5,True);
      F.Win.add(F.Tab);

      Gtk_New(F.btn,"N");
      Gtk_New(F.lbl,"N");
      F.Tab.Attach(F.btn,0,1,0,1);
      Connect(F.btn,"clicked",WindowD'ACCESS,F); -- incrémentation des coords + écriture N fichier.
      Gtk_New(F.btn,"H");
      Gtk_New(F.Lbl,"H");
      F.Tab.Attach(F.btn,2,3,0,1);
      Connect(F.btn,"clicked",WindowD'ACCESS,F);
      Gtk_New(F.btn,"V");
      Gtk_New(F.lbl,"V");
      F.Tab.Attach(F.btn,3,4,0,1);
      Connect(F.btn,"clicked",WindowD'ACCESS,F);
      Gtk_New(F.btn,"D");
      Gtk_New(F.lbl,"D");
      F.Tab.Attach(F.btn,1,2,0,1);
      Connect(F.btn,"clicked",WindowD'ACCESS,F);
      Gtk_New(F.btn,"L");
      Gtk_New(F.lbl,"L");
      F.Tab.Attach(F.btn,4,5,0,1);
      Connect(F.btn,"clicked",WindowD'ACCESS,F);
      F.Win.show_all;
      F.Win2.show_all;
      destroy(wind.win);

   end Window_type;


-- ouvre une nouvelle fenêtre (nb de col au début de la création d'une grille)
   procedure Colonne_Window(Emetteur : access Gtk_widget_Record'class; wind : T_fenetre) is
      pragma Unreferenced (Emetteur);
      Num : Integer := 1;
      W : t_fenetre;
   begin

      Gtk_New(W.Win,Window_Toplevel);
      W.win.Set_Title("Nb de col");
      W.Win.set_default_size(300,300);
      Gtk_New(W.Tab,5,9,True);
      W.Win.add(W.Tab);
      for L in 1..5 loop
         for C in 1..9 loop
            if num > 2 then
               Gtk_New(W.btn,Integer'image(Num));
               Gtk_New(W.Lbl,"%"&Get_label(Wind.lbl)&"-"&Integer'image(num));
               Gtk_New(W.lbl2,Get_Label(Wind.lbl2));
               W.Tab.Attach(W.btn,Guint(C)-1,Guint(C),Guint(L)-1,Guint(L));
               Connect(W.Btn,"clicked", Window_type'ACCESS,W);
            else
               Gtk_New(W.img,"N.png");
               W.Tab.Attach(w.Img,Guint(C)-1,Guint(C),Guint(L)-1,Guint(L));
               --show(img);              null;
            end if;
            Num := Num+1;
         end loop;
      end loop;
      W.Win.show_all;
      destroy(wind.win);
   end Colonne_Window;



   --OUVRE UNE NOUVELLE FENETRE (nb de ligne)
   procedure Ligne_Window(Emetteur : access Gtk_widget_Record'class; F : t_fenetre) is
      pragma Unreferenced (Emetteur);
      Num : Integer := 1;
      W : T_fenetre;
   begin
      Gtk_New(W.Win,Window_Toplevel);
      W.Win.Set_Title("Nb de lignes");
      W.win.Set_default_size(300,300);
      Gtk_New(W.Tab,5,9,True);
      W.Win.add(W.Tab);
      for L in 1..5 loop
         for C in 1..9 loop
            if num > 2 then
               Gtk_New(W.Btn,Integer'Image(Num));
               Gtk_New(W.Lbl,Integer'Image(Num));
               Gtk_New(W.lbl2,Get_Text(F.Saisie_Nom));
               W.Tab.Attach(W.btn,Guint(C)-1,Guint(C),Guint(L)-1,Guint(L));
               Connect(W.Btn,"clicked", Colonne_Window'ACCESS,W);
            else
               Gtk_New(W.img,"N.png");
               W.Tab.Attach(W.Img,Guint(C)-1,Guint(C),Guint(L)-1,Guint(L));
               --show(img);
            end if;
            Num := Num+1;
         end loop;
      end loop;
      W.Win.show_all;
      destroy(F.Win);
   end Ligne_Window;

   --demande Ã  l'utilisateur un nom de grille
   procedure Saisie_Window (Emetteur : access Gtk_widget_Record'class; S : t_fenetre) is
      pragma Unreferenced (Emetteur);

      W : t_fenetre;
      Label_Nom,lbl_extension : Gtk_Label;
      VBoxP : Gtk_Vbox;
      HBox_Nom : Gtk_Hbox;
      Alig_Nom : Gtk_Alignment;
   begin

      destroy(S.Win);
      Gtk_New(W.Win,Window_Toplevel) ;
      W.Win.Set_Title("Nom de la grille") ;
      W.win.Set_default_size(300,150) ;

      Gtk_New(Label_Nom, "Nom de la grille:");
      gtk_new(lbl_extension,".txt");
      Gtk_New(W.Saisie_Nom);

      Gtk_New(W.Btn, "Valider");

      Gtk_New(Alig_Nom,0.5,0.5,0.0,0.0);
      Alig_Nom.Add(W.Saisie_Nom);

      Gtk_New_Hbox(HBox_Nom);
      HBox_Nom.Pack_Start(Label_Nom);
      HBox_Nom.Pack_Start(Alig_Nom);
      HBox_Nom.pack_end(lbl_extension);
      Gtk_New_Vbox(VBoxP);
      VBoxP.Pack_Start(HBox_Nom);
      VBoxP.Pack_Start(W.btn);
      W.Win.Add(VBoxP);

      connect(W.Btn,"clicked",Ligne_Window'access,W);
      W.Win.show_all;
   end Saisie_Window;

   --affiche une fenêtre expliquant la signification des carac(N,V,H,D,L) Ã   rentrer lors de la crÃÂ ÂÃÂ Â©ation d'une grille.
   procedure Info(Emetteur : access Gtk_widget_Record'class) is
      pragma Unreferenced (Emetteur);
      W : t_fenetre;
   begin
      gtk_new(W.Win,window_toplevel);
      W.Win.set_default_size(350,300);
      W.Win.set_title("Informations");
      Gtk_new(W.tab,6,1,True);
      W.WIn.add(W.Tab);
      Gtk_new(W.lbl,"N : represente une case vide, ni a contraindre ni a remplir");
      W.Tab.Attach(W.lbl,0,1,0,1);
      Gtk_new(W.lbl,"H : represente une case contenant une unique contrainte (horizontale)");
      W.Tab.Attach(W.lbl,0,1,1,2);
      Gtk_new(W.lbl,"V : represente une case contenant une unique contrainte (verticale)");
      W.Tab.Attach(W.lbl,0,1,2,3);
      Gtk_new(W.lbl,"D : represente une case contenant deux contraintes, une verticale et une horizontale");
      W.Tab.Attach(W.lbl,0,1,3,4);
      Gtk_new(W.lbl,"L : represente une case que l'algorithme devra remplir");
      W.Tab.Attach(W.lbl,0,1,4,5);
      Gtk_new(W.btn," Ok ");
      W.Tab.Attach(W.btn,0,1,5,6);
      connect(W.btn,"clicked",Saisie_Window'access,W);
      W.Win.show_all;
   end Info;
   --
   --Affichage du menu d'accueil
   --
   procedure affichage_Accueil is
      Win : Gtk_window;
      Bouton_Creer, Bouton_Resoudre,bouton_visuel,bouton_jouer : Gtk_Button;
      label : Gtk_Label;
      img : gtk_image;
     loading_error : exception;

      tab : gtk_table;

   begin
      Init;
      Gtk_New(Win);
      Win.Set_Title("Projet 2MIC : Kakuro");
      Win.Set_Default_Size(450,350);
      if not Win.Set_Icon_From_File("square.png")then
         raise loading_error;
      end if;
      gtk_new(tab,10,4,true);
      win.add(tab);

      Gtk_New_With_Mnemonic(Bouton_Creer, "     _Creer une grille     ");
      Gtk_New_With_Mnemonic(Bouton_Resoudre, "    _Resoudre une grille    ");
      Gtk_New_With_Mnemonic(bouton_visuel, "    _Visualiser une grille    ");
      Gtk_New_With_Mnemonic(bouton_jouer, "    _Jouer    ");

      tab.attach(Bouton_Creer,0,1,8,9);
      tab.attach(Bouton_Resoudre,3,4,8,9);
      tab.attach(bouton_visuel,1,2,8,9);
      tab.attach(bouton_jouer,2,3,8,9);

      connect(win,"destroy",Stop_Program'access);
      Gtk_New(img,"option.png");

      tab.attach(img,1,3,3,4);
      gtk_new(img,"ban_.png");
      tab.attach(img,0,4,9,10);
      gtk_new(img,"noms.png");
      tab.attach(img,0,1,0,5);
      Gtk_New(Label,"1");
      Connect(Bouton_Resoudre,"clicked", Afficher_contenu'ACCESS,Label);
      Gtk_New(Label,"2");
      Connect(Bouton_Creer,"clicked", Info'ACCESS);
      gtk_new(label,"3");
      connect(bouton_visuel,"clicked",Afficher_contenu'ACCESS,Label);
      gtk_new(label,"4");
      connect(bouton_jouer,"clicked",Afficher_contenu'ACCESS,Label);
      Win.show_all;
      Main;

   exception
      when loading_error => Dialogue("chargement de l'icone impossible : S");
   end affichage_Accueil;

   --Main
   begin
     Affichage_Accueil;

end Kakuro;


