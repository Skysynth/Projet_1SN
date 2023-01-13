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
		Taille_Masque : Integer;
	begin
		-- Cas où le cache est vide
		if Est_Vide(Arbre) then
			Arbre := new T_Arbre_Cellule'(0, 0, To_Unbounded_String(""), null, null, 0, False, 0);
		else
			null;
		end if;

		-- On récupère la taille du masque
		Taille_Masque := Get_taille_binaire_masque(Masque);

		-- On regarde pour chaque bit de l'adresse si il vaut 0 ou 1 pour savoir quelle direction prendre
		for i in 1..Taille_Masque loop
			if ((Adresse AND (2 ** (32 - i))) = 0) then
				--  Cas où le bit vaut 0
				if Est_Vide(Arbre.All.Gauche) then
				-- Cas où le cache à gauche est vide
					Arbre.Gauche := new T_Arbre_Cellule'(0, 0, To_Unbounded_String(""), null, null, 0, False, 0);
					Arbre := Arbre.All.Gauche;
				else
					Arbre := Arbre.All.Gauche;
				end if;
			else
				-- Cas où le bit vaut 1
				if Est_Vide(Arbre.All.Droite) then
				-- Cas où le cache à droite est vide
					Arbre.Droite := new T_Arbre_Cellule'(0, 0, To_Unbounded_String(""), null, null, 0, False, 0);
					Arbre := Arbre.All.Droite;
				else
					Arbre := Arbre.All.Droite;
				end if;
			end if;
		end loop;

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
	end Enregistrer;

	procedure Supprimer(Arbre : in out T_Arbre; Cache : in out T_Cache; Masque : in T_Adresse_IP) is

		Politique : constant T_Politique := Cache.Politique;

		function Recherche_Identifiant_Min(Arbre : in T_Arbre; Politique : in T_Politique) return T_Adresse_IP is
			Recherche_Identifiant1 : T_Arbre;
			Recherche_Identifiant2 : T_Arbre;
			Min : Integer;
			Adresse : T_Adresse_IP;
		begin
			-- On fait pointer les pointeurs sur la racine
			Recherche_Identifiant1 := Arbre;
			Recherche_Identifiant2 := Arbre;
			Min := 100000; -- on n'utilisera en pratique jamais plus de 100,000 fois une adresse, à changer
			Adresse := 0; -- par défaut
			
			-- On recherche le minimum à droite et à gauche
			if not Est_Vide(Recherche_Identifiant1) and then not Est_Vide(Recherche_Identifiant1.Gauche) then
				if Min > Recherche_Identifiant1.All.Identifiant then
					Min := Recherche_Identifiant1.All.Identifiant;
					Adresse := Recherche_Identifiant1.All.Adresse;
				else
					null; -- il ne ne passe rien
				end if;

				Adresse := Recherche_Identifiant_Min(Recherche_Identifiant1.All.Gauche, Politique); -- on procède par récursivité (on se dédouble à chaque fois, un peu comme le calcul de la FFT)
			else
				-- On regarde les cas où on sort des if à cause des deuxièmes conditions
				if not Est_Vide(Recherche_Identifiant1) then
					if Min > Arbre.All.Identifiant then
						Min := Recherche_Identifiant1.All.Identifiant;
						Adresse := Recherche_Identifiant1.All.Adresse;
					else
						null; -- il ne ne passe rien
					end if;
				end if;
			end if;

			if not Est_Vide(Recherche_Identifiant2) and then not Est_Vide(Recherche_Identifiant2.Droite) then
				if Min > Recherche_Identifiant2.All.Identifiant then
					Min := Recherche_Identifiant2.All.Identifiant;
					Adresse := Recherche_Identifiant2.All.Adresse;
				else
					null; -- il ne se passe rien
				end if;

				Adresse := Recherche_Identifiant_Min(Recherche_Identifiant2.All.Droite, Politique); -- on procède par récursivité
			else
				-- On regarde les cas où on sort des if à cause des deuxièmes conditions
				if not Est_Vide(Recherche_Identifiant2) then
					if Min > Arbre.All.Identifiant then
						Min := Recherche_Identifiant2.All.Identifiant;
						Adresse := Recherche_Identifiant2.All.Adresse;
					else
						null; -- il ne ne passe rien
					end if;
				else
					null; -- il ne se passe rien
				end if;
			end if;

			return Adresse;
		end Recherche_Identifiant_Min;

		procedure Supprimer_FIFO(Arbre : in T_Arbre; Masque : in T_Adresse_IP) is
			Adresse : T_Adresse_IP;
			Suppresseur : T_Arbre;
			Taille_Masque : Integer;
		begin
			-- Il faut faire la recherche du minimum en terme d'identifiant et noter son adresse (= le parcours) ainsi que créer un pointeur temporaire
			Adresse := Recherche_Identifiant_Min(Arbre, Politique); -- pas d'erreur retournée étant donné que le cache est plein (il existe au moins une adresse)
			Suppresseur := Arbre;
			Taille_Masque := Get_taille_binaire_masque(Masque);

			-- On regarde pour chaque bit de l'adresse si il vaut 0 ou 1 pour savoir quelle direction prendre
			for i in 1..Taille_Masque loop
				if ((Adresse AND (2 ** (32 - i))) = 0) then
					--  Cas où le bit vaut 0
						if not Est_Vide(Suppresseur.All.Gauche) then
							Suppresseur := Suppresseur.All.Gauche;
						else
							raise Suppression_Exception;
						end if;
				else
					-- Cas où le bit vaut 1
						if not Est_Vide(Suppresseur.All.droite) then
							Suppresseur := Suppresseur.All.Droite;
						else
							raise Suppression_Exception;
						end if;
				end if;
			end loop;

			-- Il ne reste plus qu'à supprimer cette cellule
			Free(Suppresseur);

		exception
			when Suppression_Exception => Put_Line("L'élément à supprimer n'existe pas.");
		end Supprimer_FIFO;

		procedure Supprimer_LRU(Arbre : in T_Arbre; Masque : in T_Adresse_IP) is
			Adresse : T_Adresse_IP;
			Suppresseur : T_Arbre;
			Taille_Masque : Integer;
		begin
			-- Il faut faire la recherche du minimum en terme d'identifiant et noter son adresse (= le parcours) ainsi que créer un pointeur temporaire
			Adresse := Recherche_Identifiant_Min(Arbre, Politique); -- pas d'erreur retournée étant donné que le cache est plein (il existe au moins une adresse)

			-- On regarde pour chaque bit de l'adresse si il vaut 0 ou 1 pour savoir quelle direction prendre
			Suppresseur := Arbre;
			Taille_Masque := Get_taille_binaire_masque(Masque);
			for i in 1..Taille_Masque loop
				if ((Adresse AND (2 ** (32 - i))) = 0) then
					--  Cas où le bit vaut 0
						Suppresseur := Suppresseur.All.Gauche;
				else
					-- Cas où le bit vaut 1
						Suppresseur := Suppresseur.All.Droite;
				end if;
			end loop;

			-- Il ne reste plus qu'à supprimer cette cellule
			Free(Suppresseur);
		end Supprimer_LRU;

		function Recherche_Frequence_Min(Arbre : in T_Arbre; Politique : in T_Politique) return T_Adresse_IP is
			Recherche_Frequence1 : T_Arbre;
			Recherche_Frequence2 : T_Arbre;
			Min : Integer;
			Adresse : T_Adresse_IP;
		begin
			-- On fait pointer les pointeurs sur la racine
			Recherche_Frequence1 := Arbre;
			Recherche_Frequence2 := Arbre;
			Min := 100000; -- on n'utilisera en pratique jamais plus de 100,000 fois une adresse
			Adresse := 0; -- par défaut
			
			-- On recherche le minimum à droite et à gauche
			if not Est_Vide(Recherche_Frequence1) and then not Est_Vide(Recherche_Frequence1.Gauche) then
				if Min > Recherche_Frequence1.All.Frequence then
					Min := Recherche_Frequence1.All.Frequence;
					Adresse := Recherche_Frequence1.All.Adresse;
				else
					null; -- il ne ne passe rien
				end if;

				Recherche_Frequence1 := Recherche_Frequence1.All.Gauche;

				Adresse := Recherche_Frequence_Min(Recherche_Frequence1, Politique); -- on procède par récursivité (on se dédouble à chaque fois, un peu comme le calcul de la FFT)
			else
				-- On regarde les cas où on sort des if à cause des premières conditions
				if not Est_Vide(Recherche_Frequence1) then
					if Min > Arbre.All.Frequence then
						Min := Recherche_Frequence1.All.Frequence;
						Adresse := Recherche_Frequence1.All.Adresse;
					else
						null; -- il ne ne passe rien
					end if;
				else
					null; -- il ne se passe rien
				end if;
			end if;

			if not Est_Vide(Recherche_Frequence2) and then not Est_Vide(Recherche_Frequence2.Droite) then
				if Min > Recherche_Frequence2.All.Frequence then
					Min := Recherche_Frequence2.All.Frequence;
					Adresse := Recherche_Frequence1.All.Adresse;
				else
					null; -- il ne se passe rien
				end if;

				Recherche_Frequence2 := Recherche_Frequence2.All.Droite;

				Adresse := Recherche_Frequence_Min(Recherche_Frequence2, Politique); -- on procède par récursivité
			else
				-- On regarde les cas où on sort des if à cause des premières conditions
				if not Est_Vide(Recherche_Frequence2) then
					if Min > Arbre.All.Frequence then
						Min := Recherche_Frequence2.All.Frequence;
						Adresse := Recherche_Frequence2.All.Adresse;
					else
						null; -- il ne ne passe rien
					end if;
				else
					null; -- il ne se passe rien
				end if;
			end if;

			return Adresse;
		end Recherche_Frequence_Min;

		procedure Supprimer_LFU(Arbre : in T_Arbre; Masque : in T_Adresse_IP; Politique: in T_Politique) is
			Adresse : T_Adresse_IP;
			Suppresseur : T_Arbre;
			Taille_Masque : Integer;
		begin
			-- Il faut faire la recherche du minimum en terme de fréquence et noter son adresse (= le parcours) ainsi que créer un pointeur temporaire
			Adresse := Recherche_Frequence_Min(Arbre, Politique); -- pas d'erreur retournée étant donné que le cache est plein (il existe au moins une adresse)
			Suppresseur := Arbre;
			Taille_Masque := Get_taille_binaire_masque(Masque);

			-- On regarde pour chaque bit de l'adresse si il vaut 0 ou 1 pour savoir quelle direction prendre
			for i in 1..Taille_Masque loop
				if ((Adresse AND (2 ** (32 - i))) = 0) then
					--  Cas où le bit vaut 0
						Suppresseur := Suppresseur.All.Gauche;
				else
					-- Cas où le bit vaut 1
						Suppresseur := Suppresseur.All.Droite;
				end if;
			end loop;

			-- Il ne reste plus qu'à supprimer cette cellule
			Free(Suppresseur);
		end Supprimer_LFU;

	begin
		-- On regarde quelle est la procédure
		case Politique is
			when FIFO => Supprimer_FIFO(Arbre, Masque); -- FIFO
			when LRU => Supprimer_LRU(Arbre, Masque); -- LRU
			when LFU => Supprimer_LFU(Arbre, Masque, Politique); -- LFU
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
		Afficheur1 : T_Arbre;
		Afficheur2 : T_Arbre;
		Compteur : Integer := 0; -- pour compter les feuilles
	begin
		-- Initialisation des pointeurs qui servent à afficher l'arbre
		Afficheur1 := Arbre;
		Afficheur2 := Arbre;

		if Est_Vide(Arbre) then
			raise Arbre_Vide_Exception;
		else
			null;
		end if;

		-- Le parcours est en profondeur, on explore tout ce qu'il y a à gauche
		if not Est_Vide(Afficheur1.All.Gauche) and then not Afficheur1.All.Feuille then
			-- Tant que le chemin gauche de l'arbre n'est pas nul, on avance
			Afficher_Arbre(Afficheur1.All.Gauche);
		else
			-- Sinon on affiche car la cellule est une feuille
			if not Est_Vide(Afficheur1) then
				Compteur := Compteur + 1;

				Put_Line("Adresse" & Integer'Image(Compteur) & " :" & T_Adresse_IP'Image(Afficheur1.All.Adresse));
				Put_Line("Masque" & Integer'Image(Compteur)& " :" & T_Adresse_IP'Image(Afficheur1.All.Masque));
				Put_Line(To_String(Afficheur1.All.Sortie));
				New_Line;
			else
				null;
			end if;
		end if;

		-- Le parcours est en profondeur, on explore tout ce qu'il y a à droite
		if not Est_Vide(Afficheur2.All.Droite) and then not Afficheur2.All.Feuille then
			-- Tant que le chemin gauche de l'arbre n'est pas nul, on avance
			Afficher_Arbre(Afficheur2.All.Droite);
		else
			-- Cas où la cellule est une feuille
			if not Est_Vide(Afficheur2) then
				Compteur := Compteur + 1;

				Put_Line("Adresse" & Integer'Image(Compteur) & " :" & T_Adresse_IP'Image(Afficheur2.All.Adresse));
				Put_Line("Masque" & Integer'Image(Compteur) & " :" & T_Adresse_IP'Image(Afficheur2.All.Masque));
				Put_Line(To_String(Afficheur2.All.Sortie));
				New_Line;
			else
				null;
			end if;
		end if;

	exception
		when Arbre_Vide_Exception => Put_Line("L'arbre est vide.");
	end Afficher_Arbre;

	procedure Afficher_Statistiques_Cache(Cache : in T_Cache) is
		Taux_Defauts : Float;
	begin
		Put_Line("Le nombre de défauts de cache est de :" & Integer'Image(Cache.Defauts));
		Put_Line("Le nombre de demandes de route au cache est de :" & Integer'Image(Cache.Demandes));

		Taux_Defauts := Float(Cache.Defauts) / Float(Cache.Demandes);
		Put_Line("Le taux de défauts de cache est de :" & Float'Image(Taux_Defauts));
	end Afficher_Statistiques_Cache;

	function Recherche_Identifiant_Max(Arbre : in T_Arbre) return Integer is
			Recherche_Identifiant1 : T_Arbre;
			Recherche_Identifiant2 : T_Arbre;
			Max : Integer;
		begin
			-- On fait pointer les pointeurs sur la racine
			Recherche_Identifiant1 := Arbre;
			Recherche_Identifiant2 := Arbre;
			Max := 0; -- par défaut
			
			-- On recherche le minimum à droite et à gauche
			if not Est_Vide(Recherche_Identifiant1) and then not Est_Vide(Recherche_Identifiant1.Gauche) then
				if Max < Recherche_Identifiant1.All.Identifiant then
					Max := Recherche_Identifiant1.All.Identifiant;
				else
					null; -- il ne ne passe rien
				end if;

				Recherche_Identifiant1 := Recherche_Identifiant1.All.Gauche;

				Max := Recherche_Identifiant_Max(Recherche_Identifiant1); -- on procède par récursivité (on se dédouble à chaque fois, un peu comme le calcul de la FFT)
			else
				-- On regarde les cas où on sort des if à cause des premières conditions
				if not Est_Vide(Recherche_Identifiant1) then
					if Max < Arbre.All.Identifiant then
						Max := Recherche_Identifiant1.All.Identifiant;
					else
						null; -- il ne ne passe rien
					end if;
				else
					null;
				end if;
			end if;
			
			if not Est_Vide(Recherche_Identifiant2) and then not Est_Vide(Recherche_Identifiant2.Droite) then
				if Max < Recherche_Identifiant2.All.Identifiant then
					Max := Recherche_Identifiant2.All.Identifiant;
				else
					null; -- il ne se passe rien
				end if;

				Recherche_Identifiant2 := Recherche_Identifiant2.All.Droite;

				Max := Recherche_Identifiant_Max(Recherche_Identifiant2); -- on procède par récursivité
			else
				-- On regarde les cas où on sort des if à cause des premières conditions
				if not Est_Vide(Recherche_Identifiant2) then
					if Max < Arbre.All.Identifiant then
						Max := Recherche_Identifiant2.All.Identifiant;
					else
						null; -- il ne ne passe rien
					end if;
				else
					null; -- il ne se passe rien
				end if;
			end if;

			return Max;
		end Recherche_Identifiant_Max;

	function Chercher_Arbre(Arbre : in T_Arbre; Cache : in out T_Cache; Adresse : in T_Adresse_IP) return Unbounded_string is
		Sortie : Unbounded_String;
		Recherche_Adresse : T_Arbre;
		Max : Integer;
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
		if Compteur = 32 then
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
			Max := Recherche_Identifiant_Max(Arbre);
			if Recherche_Adresse.All.Identifiant /= Max then
					Recherche_Adresse.All.Identifiant := Max + 1;
			else
				null;
			end if;
		else
			null;
		end if;

    	return Sortie;

    exception
    	when Adresse_Absente_Exception => return To_Unbounded_String("L'adresse n'a pas été trouvée dans le cache.");
    end Chercher_Arbre;

end cache_tree;
