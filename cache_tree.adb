with Ada.Text_IO; use Ada.Text_IO;
with routeur_exceptions; use routeur_exceptions;
with Ada.Unchecked_Deallocation;

package body cache_tree is

	procedure Free is
		new Ada.Unchecked_Deallocation(Object => T_Arbre_Cellule, Name => T_Arbre);

	procedure Initialiser_Arbre(Arbre : out T_Arbre) is
	begin
		Arbre := Null;
	end Initialiser_Arbre;

	procedure Initialiser_Cache(Cache: out T_Cache; Taille_Max : in Integer; Politique : in T_Politique) is
	begin
		Cache.Taille_Max := Taille_Max;
		Cache.Taille := 0; 
		Cache.Defauts := 0;
		Cache.Demandes := 0;
		Cache.Enregistrement := 0;
		Cache.Politique := Politique;
	end Initialiser_Cache;

	function Est_Vide(Arbre : in T_Arbre) return Boolean is
	begin
		return (Arbre = Null); 
	end;

    procedure Vider(Arbre : in out T_Arbre) is
	begin
        if not Est_Vide(Arbre) then
            -- Si le cache n'est pas vide
			Vider(Arbre.All.Gauche);
            Vider(Arbre.All.Droite);
            Free(Arbre);
        else
			-- Si le cache est vide
            null;
        end if;
	end Vider;

	function Taille_Cache(Cache : in T_Cache) return Integer is
	begin
		return Cache.Taille;
	end Taille_Cache;

	function Frequence_Arbre(Arbre : in T_Arbre) return Integer is
	begin
		return Arbre.All.Frequence;
	end Frequence_Arbre;

	function Demandes_Cache(Cache : in T_Cache) return Integer is
	begin
		return Cache.Demandes;
	end Demandes_Cache;

	function Defauts_Cache(Cache : in T_Cache) return Integer is
	begin
		return Cache.Defauts;
	end Defauts_Cache;

	function Enregistrement_Cache(Cache : in T_Cache) return Integer is
	begin
		return Cache.Enregistrement;
	end Enregistrement_Cache;

	procedure Enregistrer(Arbre : in out T_Arbre; Cache : in out T_Cache; Adresse : in T_Adresse_IP; Masque : in T_Adresse_IP; Sortie : in Unbounded_String; Politique : in T_Politique) is
		Taille_Masque : constant Integer := Get_taille_binaire_masque(Masque);
		Enregistreur : T_Arbre;
	begin
		-- On regarde si l'arbre est vide
		if Est_Vide(Arbre) then
			Arbre := new T_Arbre_Cellule'(0, 0, To_Unbounded_String(""), null, null, 0, False, 0, 0);
		else
			null;
		end if;

		-- On regarde si on arrive au niveau d'une feuille
		if (Arbre.All.Hauteur - Taille_Masque = 0) then
			-- On devrait être au niveau d'une feuille à présent
			-- Il reste à stocker toutes les informations nécessaires
			Arbre.All.Adresse := Adresse;
			Arbre.All.Masque := Masque;
			Arbre.All.Sortie := Sortie;
			Arbre.All.Feuille := True;

			case Politique is
				when FIFO => Arbre.All.Identifiant := Cache.Enregistrement; -- FIFO
				when LRU => Arbre.All.Identifiant := 0; -- LRU
				when LFU => Arbre.All.Identifiant := 0; -- LFU
				-- when others => raise Politique_non_valide_exception;
			end case;

			Cache.Taille := Cache.Taille + 1;
			Cache.Enregistrement := Cache.Enregistrement + 1;
		elsif (Adresse AND 2 ** (31 - Arbre.All.Hauteur)) = 0 then
			-- On regarde si le bit vaut 0 et si la cellule de gauche est vide ou non
            if Est_Vide(Arbre.All.Gauche) then
				-- Cas où le bit vaut 0 et que la cellule à gauche est nulle
                Initialiser_Arbre(Enregistreur);
				Enregistreur := new T_Arbre_Cellule'(0, 0, To_Unbounded_String(""), null, null, 0, False, 0, 0);

				-- On augmente la hauteur dans l'arbre
                Enregistreur.All.Hauteur := Arbre.All.Hauteur + 1;

				-- On raccroche l'arbre de gauche à la cellule créée
                Arbre.All.Gauche := Enregistreur;

				-- On supprime le cache temporaire
				Enregistreur := null;

				-- On procède par récursivité
                Enregistrer(Arbre.All.Gauche, Cache, Adresse, Masque, Sortie, Politique);
            else
				-- Cas où le bit vaut 0 et que la cellule à gauche n'est pas nulle
                Enregistrer(Arbre.All.Gauche, Cache, Adresse, Masque, Sortie, Politique);     
            end if;
		else
			-- On regarde si le bit vaut 0 et si la cellule de droite est vide ou non
            if Arbre.All.Droite = Null then
				-- Cas où le bit vaut 1 et que la cellule à droite est nulle
                Initialiser_Arbre(Enregistreur);
				Enregistreur := new T_Arbre_Cellule'(0, 0, To_Unbounded_String(""), null, null, 0, False, 0, 0);

				-- On augmente la hauteur dans l'arbre
                Enregistreur.All.Hauteur := Arbre.All.Hauteur + 1;

				-- On raccroche l'arbre de gauche à la cellule créée
                Arbre.All.Droite := Enregistreur;

				-- On supprime le cache temporaire
				Enregistreur := null;

				-- On procède par récursivité
                Enregistrer(Arbre.All.Droite, Cache, Adresse, Masque, Sortie, Politique);
            else
				-- Cas où le bit vaut 1 et que la cellule à droite n'est pas nulle
                Enregistrer(Arbre.All.Droite, Cache, Adresse, Masque, Sortie, Politique);     
            end if;
		end if;

	end Enregistrer;

	procedure Supprimer(Arbre : in out T_Arbre; Cache : in out T_Cache) is

		Politique : constant T_Politique := Cache.Politique;

		function Adresse_Identifiant_Min(Arbre : in T_Arbre; Min : Integer) return T_Adresse_IP is
			Recherche_Adresse : T_Arbre;
			Adresse : T_Adresse_IP;
		begin
			-- On fait pointer les pointeurs sur la racine
			Recherche_Adresse := Arbre;

			-- Cas de base
			if Est_Vide(Recherche_Adresse) then
				return 0;
			else
				null;
			end if;

			-- On met à jour l'adresse correspondante au min
			if Min = Recherche_Adresse.All.Identifiant and Recherche_Adresse.All.Feuille then
				Adresse := Recherche_Adresse.All.Adresse;
			else
				null;
			end if;

			-- On applique la fonction de manière récursive à gauche et à droite
			if not Est_Vide(Recherche_Adresse.All.Gauche) then
				Adresse := Adresse_Identifiant_Min(Recherche_Adresse.All.Gauche, Min);
			else
				null;
			end if;

			if not Est_Vide(Recherche_Adresse.All.Droite) then
				Adresse := Adresse_Identifiant_Min(Recherche_Adresse.All.Droite, Min);
			else
				null;
			end if;

			return Adresse;
		end Adresse_Identifiant_Min;

		procedure Supprimer_FIFO(Arbre : in T_Arbre) is
			Min : Integer := 10000000;
			Adresse : constant T_Adresse_IP := Adresse_Identifiant_Min(Arbre, Min);
		begin
			-- On initialise le minimum
			Min := Recherche_Identifiant_Min(Arbre, Min);

			-- On regarde si on arrive jusqu'à la feuille
        	if Arbre = Null then 
        	    raise Arbre_Vide_Exception;
        	elsif Adresse = Arbre.All.Adresse and Arbre.All.Feuille then
        	    Arbre.All.Feuille := False;
         	    Arbre.All.Adresse := 0;
				Arbre.All.Frequence := 0;
				Arbre.All.Identifiant := 0;
				Arbre.All.Masque := 0;
				Arbre.All.Sortie := To_Unbounded_String("");
			else
				null;
        	end if;

        	Supprimer_FIFO(Arbre.All.Gauche);
        	Supprimer_FIFO(Arbre.All.Droite);

		exception
			when Arbre_Vide_Exception => null;
		end Supprimer_FIFO;

		procedure Supprimer_LRU(Arbre : in T_Arbre) is
			Min : Integer := 10000000;
			Suppresseur : T_Arbre;
			Adresse : constant T_Adresse_IP := Adresse_Identifiant_Min(Arbre, Min);
		begin
			-- On initialise le minimum
			Min := Recherche_Identifiant_Min(Arbre, Min);

			-- On initialise le pointeur temporaire
			Suppresseur := Arbre;

			-- On regarde si on arrive à la taille du masque
			if Min = Suppresseur.All.Identifiant then
				-- On devrait être au niveau de la feuille correspondante à présent
				Arbre.All.Adresse := 0;
				Arbre.All.Frequence := 0;
				Arbre.All.Identifiant := 0;
				Arbre.All.Masque := 0;
				Arbre.All.Sortie := To_Unbounded_String("");
				Arbre.All.Feuille := False;
			elsif (Adresse AND 2 ** (31 - Suppresseur.All.Hauteur)) = 0 then
				-- Cas où le bit vaut 0
        	    Supprimer_LRU(Arbre.All.Gauche);
			else
				-- Cas où le bit vaut 1
				Supprimer_LRU(Arbre.All.Droite);
			end if;
		end Supprimer_LRU;

		function Recherche_Frequence_Min(Arbre : in T_Arbre) return T_Adresse_IP is
			Recherche_Frequence : T_Arbre;
			Min : Integer;
			Adresse : T_Adresse_IP;
		begin
			-- On fait pointer les pointeurs sur la racine
			Recherche_Frequence := Arbre;
			Min := 100000; -- on n'utilisera en pratique jamais plus de 100,000 fois une adresse, à changer

			-- On met à jour la plus petite fréquence
			if Min > Recherche_Frequence.All.Frequence and Recherche_Frequence.All.Feuille then
				Min := Recherche_Frequence.All.Frequence;
				Adresse := Recherche_Frequence.All.Adresse;
			else
				null;
			end if;

			-- On applique la fonction de manière récursive à gauche et à droite
			if not Est_Vide(Recherche_Frequence.All.Gauche) then
				Adresse := Recherche_Frequence_Min(Recherche_Frequence.All.Gauche);
			else
				null;
			end if;

			if not Est_Vide(Recherche_Frequence.All.Droite) then
				Adresse := Recherche_Frequence_Min(Recherche_Frequence.All.Droite);
			else
				null;
			end if;

			return Adresse;
		end Recherche_Frequence_Min;

		procedure Supprimer_LFU(Arbre : in T_Arbre) is
			Adresse : T_Adresse_IP;
			Suppresseur : T_Arbre;
		begin
			-- Il faut faire la recherche du minimum en terme de fréquence et noter son adresse (= le parcours) ainsi que créer un pointeur temporaire
			Adresse := Recherche_Frequence_Min(Arbre); -- pas d'erreur retournée étant donné que le cache est plein (il existe au moins une adresse)
			Suppresseur := Arbre;

			-- On regarde pour chaque bit de l'adresse si il vaut 0 ou 1 pour savoir quelle direction prendre
			for i in 1..32 loop
				if ((Adresse AND (2 ** (32 - i))) = 0) then
					--  Cas où le bit vaut 0
						Suppresseur := Suppresseur.All.Gauche;
				else
					-- Cas où le bit vaut 1
						Suppresseur := Suppresseur.All.Droite;
				end if;
			end loop;

			-- Il ne reste plus qu'à supprimer cette cellule
			Suppresseur.All.Adresse := 0;
			Suppresseur.All.Frequence := 0;
			Suppresseur.All.Identifiant := 0;
			Suppresseur.All.Masque := 0;
			Suppresseur.All.Sortie := To_Unbounded_String("");
			Suppresseur.All.Feuille := False;
		end Supprimer_LFU;

	begin
		-- On regarde quelle est la procédure 
		case Politique is
			when FIFO => Supprimer_FIFO(Arbre); -- FIFO
			when LRU => Supprimer_LRU(Arbre); -- LRU
			when LFU => Supprimer_LFU(Arbre); -- LFU
			-- when others => raise Politique_non_valide_exception;
		end case;

		Cache.Taille := Cache.Taille - 1;

	exception
		when Politique_non_valide_exception => Put("La politique demandée n'est pas valide.");
	end Supprimer;

	function Est_Plein(Cache : in T_Cache) return Boolean is
		Est_Plein : Boolean;
	begin
		if Cache.Taille >= Cache.Taille_Max then
			Est_Plein := True;
		else
			Est_Plein := False;
		end if;

		return Est_Plein;
	end Est_Plein;

	procedure Afficher_Arbre(Arbre : in T_Arbre) is
		Afficheur : T_Arbre;
	begin
		-- Initialisation des pointeurs qui servent à afficher l'arbre
		Afficheur := Arbre;

		-- On propage l'exception si le cache est vide
		if Est_Vide(Afficheur) then
			raise Affichage_Exception;
		else
			null;
		end if;

		-- On regarde si on est au niveau d'une feuille
		if Afficheur.All.Feuille then
			Put_Line("Adresse :" & T_Adresse_IP'Image(Afficheur.All.Adresse));
			Put_Line("Masque :" & T_Adresse_IP'Image(Afficheur.All.Masque));
			Put_Line(To_String(Afficheur.All.Sortie));
			Put_Line("Identifiant :" & Integer'Image(Afficheur.All.Identifiant));
			Put_Line("Fréquence :" & Integer'Image(Afficheur.All.Frequence));
			New_Line;
		else
			null;
		end if;

		-- Récursivité à gauche et à droite

		Afficher_Arbre(Afficheur.All.Gauche);
		Afficher_Arbre(Afficheur.All.Droite);

	exception
		when Affichage_Exception => null;
	end Afficher_Arbre;

	procedure Afficher_Statistiques_Cache(Cache : in T_Cache) is
		Taux_Defauts : Float;
	begin
		Put_Line("Le nombre de défauts de cache est de :" & Integer'Image(Cache.Defauts));
		Put_Line("Le nombre de demandes de route au cache est de :" & Integer'Image(Cache.Demandes));

		Taux_Defauts := Float(Cache.Defauts) / Float(Cache.Demandes);
		Put_Line("Le taux de défauts de cache est de :" & Float'Image(Taux_Defauts));
	end Afficher_Statistiques_Cache;

	-- pré-condition : not Est_Vide(Arbre)
	function Recherche_Identifiant_Max(Arbre : in T_Arbre; Max : in out Integer) return Integer is
			Recherche_Max : T_Arbre;
			Max_Gauche : Integer;
			Max_Droite : Integer;
		begin
			-- On initialise le pointeur temporaire
			Recherche_Max := Arbre;

			-- On regarde si le cache est vide ou non
			if not Est_Vide(Recherche_Max) then
				-- On traite la racine
				if Recherche_Max.All.Feuille and then Recherche_Max.All.Identifiant > Max then
					Max := Recherche_Max.All.Identifiant;
				else
					null;
				end if;

				-- On traite la partie gauche de l'arbre
				Max_Gauche := Recherche_Identifiant_Max(Recherche_Max.All.Gauche, Max);

				-- On traite la partie droite de l'arbre
				Max_Droite := Recherche_Identifiant_Max(Recherche_Max.All.Droite, Max);

				-- On compare le minimum de gauche et de droite
				if Max_Gauche > Max_Droite then
					Max := Max_Gauche;
				else
					Max := Max_Droite;
				end if;

			else
				raise Arbre_Vide_Exception;
			end if;

			return Max;

		exception
			when Arbre_Vide_Exception => return Max;
		end Recherche_Identifiant_Max;

		function Recherche_Identifiant_Min(Arbre : in T_Arbre; Min : in out Integer) return Integer is
			Recherche_Min : T_Arbre;
			Min_Gauche : Integer;
			Min_Droite : Integer;
		begin
			-- On initialise le pointeur temporaire
			Recherche_Min := Arbre;

			-- On regarde si le cache est vide ou non
			if not Est_Vide(Recherche_Min) then
				-- On traite la racine
				if Recherche_Min.All.Feuille and then Recherche_Min.All.Identifiant < Min then
					Min := Recherche_Min.All.Identifiant;
				else
					null;
				end if;

				-- On traite la partie gauche de l'arbre
				Min_Gauche := Recherche_Identifiant_Min(Recherche_Min.All.Gauche, Min);

				-- On traite la partie droite de l'arbre
				Min_Droite := Recherche_Identifiant_Min(Recherche_Min.All.Droite, Min);

				-- On compare le minimum de gauche et de droite
				if Min_Gauche < Min_Droite then
					Min := Min_Gauche;
				else
					Min := Min_Droite;
				end if;

			else
				raise Arbre_Vide_Exception;
			end if;

			return Min;
		
		exception
			when Arbre_Vide_Exception => return Min;
		end Recherche_Identifiant_Min;

	function Chercher_Arbre(Arbre : in T_Arbre; Cache : in out T_Cache; Adresse : in T_Adresse_IP) return Unbounded_string is
		Sortie : Unbounded_String;
		Recherche_Adresse : T_Arbre;
		Max : Integer := 0;
		Politique : constant T_Politique := Cache.Politique;
		Compteur : Integer := 1;
    begin
		Cache.Demandes := Cache.Demandes + 1;

		-- On fait pointer le pointeur temporaire sur la racine
		Recherche_Adresse := Arbre; 
		
		-- On se déplace jusqu'à l'adresse (si elle existe)
		while Compteur /= 32 and Adresse /= Recherche_Adresse.All.Adresse loop
			if ((Adresse AND (2 ** (32 - Compteur))) = 0) then
				--  Cas où le bit vaut 0
				if Est_Vide(Recherche_Adresse.Gauche) then
					-- Cas où le cache à gauche est vide
					Put_Line("L'adresse" & T_Adresse_IP'Image(Adresse) & " n'a pas été trouvée");
					Cache.Defauts := Cache.Defauts + 1;
					raise Adresse_Absente_Exception;
				else
					Recherche_Adresse := Recherche_Adresse.All.Gauche;
				end if;
			else
				-- Cas où le bit vaut 1
				if Est_Vide(Recherche_Adresse.Droite) then
					-- Cas où le cache à droite est vide
					Put_Line("L'adresse" & T_Adresse_IP'Image(Adresse) & " n'a pas été trouvée");
					Cache.Defauts := Cache.Defauts + 1;
					raise Adresse_Absente_Exception;
				else
					Recherche_Adresse := Recherche_Adresse.All.Droite;
				end if;
			end if;
			Compteur := Compteur + 1;
		end loop;

		-- Cas où on ne trouve pas l'adresse à la fin
		if Compteur = 32 and Adresse /= Recherche_Adresse.All.Adresse then
			Put_Line("L'adresse" & T_Adresse_IP'Image(Adresse) & " n'a pas été trouvée");
			Cache.Defauts := Cache.Defauts + 1;
			raise Adresse_Absente_Exception;
		else
			null;
		end if;

		-- On devrait être au niveau de l'adresse en question
		Sortie := Recherche_Adresse.All.Sortie;
		Recherche_Adresse.All.Frequence := Recherche_Adresse.All.Frequence + 1;
		if Politique = LRU then -- LRU
			Max := Recherche_Identifiant_Max(Arbre, Max);
			if Max = 0 then
				Recherche_Adresse.All.Identifiant := Max + 1;
			elsif Recherche_Adresse.All.Identifiant /= Max then
				Recherche_Adresse.All.Identifiant := Max + 1;
			else
				null;
			end if;
		else
			null;
		end if;

    	return Sortie;

    exception
    	when Adresse_Absente_Exception => return To_Unbounded_String("L'adresse" & T_Adresse_IP'Image(Adresse) & " n'a pas été trouvée");
    end Chercher_Arbre;

end cache_tree;
