with Ada.Text_IO; use Ada.Text_IO;
with routeur_exceptions; use routeur_exceptions;
with Ada.Unchecked_Deallocation;

package body cache_tree is

	procedure Free is
		new Ada.Unchecked_Deallocation(Object => T_Arbre_Cellule, Name => T_Arbre);

	procedure Initialiser(Cache : out T_Cache_Arbre; Taille : in Integer) is
	begin
		Cache.Arbre := Null;
		Cache.Taille := Taille;
	end Initialiser;

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
            Put_Line("Le cache est pas vide. Pas besoin de le vider.");
        end if;
	end Vider;

	function Taille_Cache(Cache : in T_Cache_Arbre) return Integer is
	begin
		return Cache.Taille;
	end Taille_Cache;

	function Arbre_Cache(Cache : in T_Cache_Arbre) return T_Arbre is
	begin
		return Cache.Arbre;
	end Arbre_Cache;

	function Frequence_Arbre(Arbre : in T_Arbre) return Integer is
	begin
		return Arbre.All.Frequence;
	end Frequence_Arbre;

	function Demandes_Cache(Cache : in T_Cache_Arbre) return Integer is
	begin
		return Cache.Demandes;
	end Demandes_Cache;

	function Defauts_Cache(Cache : in T_Cache_Arbre) return Integer is
	begin
		return Cache.Defauts;
	end Defauts_Cache;

	function Enregistrement_Cache(Cache : in T_Cache_Arbre) return Integer is
	begin
		return Cache.Enregistrement;
	end Enregistrement_Cache;

	procedure Enregistrer(Arbre : in out T_Arbre; Cache : in out T_Cache_Arbre; Adresse : in T_Adresse_IP; Masque : in T_Adresse_IP; Sortie : in Unbounded_String; Politique : in T_Politique) is
		Taille_Masque : Integer;
	begin
		-- Cas où le cache est vide
		if Est_Vide(Arbre) then
			Arbre := new T_Arbre_Cellule'(Adresse, Masque, Sortie, null, null, 0, False, 0);
		else
			Put_Line("Le cache n'est pas vide. On peut continuer.");
		end if;

		Taille_Masque := Get_taille_binaire(Masque);

		-- On regarde pour chaque bit de l'adresse si il vaut 0 ou 1 pour savoir quelle direction prendre
		for i in 0..(Taille_Masque - 1) loop
			if ((Adresse AND (2 ** (Taille_Masque - 1 - i))) = 0) then
				--  Cas où le bit vaut 0
				if Est_Vide(Arbre.All.Gauche) then
				-- Cas où le cache à gauche est vide
					Arbre.All.Gauche := new T_Arbre_Cellule'(Adresse, Masque, Sortie, null, null, 0, False, 0);
					Arbre := Arbre.All.Gauche;
				else
					Arbre := Arbre.All.Gauche;
				end if;
			else
				-- Cas où le bit vaut 1
				if Est_Vide(Arbre.All.Droite) then
				-- Cas où le cache à droite est vide
					Arbre.All.Droite := new T_Arbre_Cellule'(Adresse, Masque, Sortie, null, null, 0, False, 0);
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

		case T_Politique'Pos(Politique) is
			when 1 => Arbre.All.Identifiant := Cache.Enregistrement; -- FIFO
			when 2 => Arbre.All.Identifiant := 0; -- LRU
			when 3 => Arbre.All.Identifiant := 0; -- LFU
			when others => raise Politique_non_valide_exception;
		end case;

		Cache.Taille := Cache.Taille + 1;
		Cache.Enregistrement := Cache.Enregistrement + 1;
	end Enregistrer;

	procedure Ajouter_Frequence(Arbre : in out T_Arbre; Adresse : in T_Adresse_IP; Masque : in T_Adresse_IP) is
		Taille_Masque : Integer;
	begin
		if Est_Vide(Arbre) then
		-- Cas où le cache est vide
			raise Adresse_Absente_Exception;
		else
			Put_Line("Le cache n'est pas vide. On peut continuer.");
		end if;

		Taille_Masque := Get_taille_binaire(Masque);

		-- On regarde pour chaque bit de l'adresse si il vaut 0 ou 1 pour savoir quelle direction prendre
		for i in 0..(Taille_Masque - 1) loop
			if ((Adresse AND (2 ** (Taille_Masque - 1 - i))) = 0) then
				--  Cas où le bit vaut 0
				if Est_Vide(Arbre.Gauche) then
				-- Cas où le cache à gauche est vide
					raise Adresse_Absente_Exception;
				else
					Arbre := Arbre.All.Gauche;
				end if;
			else
				-- Cas où le bit vaut 1
				if Est_Vide(Arbre.Droite) then
				-- Cas où le cache à droite est vide
					raise Adresse_Absente_Exception;
				else
					Arbre := Arbre.All.Droite;
				end if;
			end if;
		end loop;

		-- On devrait être au niveau de la feuille correspondante à l'adresse désormais
		Arbre.All.Frequence := Arbre.All.Frequence + 1;
	exception
		when Adresse_Absente_Exception => Put("L'adresse demandée n'est pas présente.");
	end Ajouter_Frequence;

	procedure Supprimer(Arbre : in out T_Arbre; Cache : in out T_Cache_Arbre; Politique : in T_Politique; Masque : in T_Adresse_IP) is

		function Recherche_Identifiant_Min(Arbre : in T_Arbre) return T_Adresse_IP is
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
			if Recherche_Identifiant1 /= null and then Recherche_Identifiant1.Gauche /= null then
				if Min > Recherche_Identifiant1.All.Identifiant then
					Min := Recherche_Identifiant1.All.Identifiant;
					Adresse := Recherche_Identifiant1.All.Adresse;
				else
					null; -- il ne ne passe rien
				end if;

				Adresse := Recherche_Identifiant_Min(Recherche_Identifiant1.All.Gauche); -- on procède par récursivité (on se dédouble à chaque fois, un peu comme le calcul de la FFT)
			elsif Recherche_Identifiant2 /= null and then Recherche_Identifiant2.Droite /= null then
				if Min > Recherche_Identifiant2.All.Identifiant then
					Min := Recherche_Identifiant2.All.Identifiant;
					Adresse := Recherche_Identifiant1.All.Adresse;
				else
					null; -- il ne se passe rien
				end if;

				Adresse := Recherche_Identifiant_Min(Recherche_Identifiant2.All.Droite); -- on procède par récursivité
			else
				-- On regarde les cas où on sort des if à cause des premières conditions
				if Recherche_Identifiant1 /= null then
					if Min > Arbre.All.Identifiant then
						Min := Recherche_Identifiant1.All.Identifiant;
						Adresse := Recherche_Identifiant1.All.Adresse;
					else
						null; -- il ne ne passe rien
					end if;
				elsif Recherche_Identifiant2 /= null then
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
			Adresse := Recherche_Identifiant_Min(Arbre); -- pas d'erreur retournée étant donné que le cache est plein (il existe au moins une adresse)
			Suppresseur := Arbre;
			Taille_Masque := Get_taille_binaire(Masque);

			-- On regarde pour chaque bit de l'adresse si il vaut 0 ou 1 pour savoir quelle direction prendre
			for i in 0..(Taille_Masque - 1) loop
				if ((Adresse AND (2 ** (Taille_Masque - i))) = 0) then
					--  Cas où le bit vaut 0
						Suppresseur := Suppresseur.All.Gauche;
				else
					-- Cas où le bit vaut 1
						Suppresseur := Suppresseur.All.Droite;
				end if;
			end loop;

			-- Il ne reste plus qu'à supprimer cette cellule
			Free(Suppresseur);
		end Supprimer_FIFO;

		procedure Supprimer_LRU(Arbre : in T_Arbre; Masque : in T_Adresse_IP) is
			Adresse : T_Adresse_IP;
			Suppresseur : T_Arbre;
			Taille_Masque : Integer;
		begin
			-- Il faut faire la recherche du minimum en terme d'identifiant et noter son adresse (= le parcours) ainsi que créer un pointeur temporaire
			Adresse := Recherche_Identifiant_Min(Arbre); -- pas d'erreur retournée étant donné que le cache est plein (il existe au moins une adresse)
			Suppresseur := Arbre;
			Taille_Masque := Get_taille_binaire(Masque);

			-- On regarde pour chaque bit de l'adresse si il vaut 0 ou 1 pour savoir quelle direction prendre
			for i in 0..(Taille_Masque - 1) loop
				if ((Adresse AND (2 ** (Taille_Masque - 1 - i))) = 0) then
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

		function Recherche_Frequence_Min(Arbre : in T_Arbre) return T_Adresse_IP is
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
			if Recherche_Frequence1 /= null and then Recherche_Frequence1.Gauche /= null then
				if Min > Recherche_Frequence1.All.Frequence then
					Min := Recherche_Frequence1.All.Frequence;
					Adresse := Recherche_Frequence1.All.Adresse;
				else
					null; -- il ne ne passe rien
				end if;

				Recherche_Frequence1 := Recherche_Frequence1.All.Gauche;

				Adresse := Recherche_Frequence_Min(Recherche_Frequence1); -- on procède par récursivité (on se dédouble à chaque fois, un peu comme le calcul de la FFT)
			elsif Recherche_Frequence2 /= null and then Recherche_Frequence2.Droite /= null then
				if Min > Recherche_Frequence2.All.Frequence then
					Min := Recherche_Frequence2.All.Frequence;
					Adresse := Recherche_Frequence1.All.Adresse;
				else
					null; -- il ne se passe rien
				end if;

				Recherche_Frequence2 := Recherche_Frequence2.All.Droite;

				Adresse := Recherche_Frequence_Min(Recherche_Frequence2); -- on procède par récursivité
			else
				-- On regarde les cas où on sort des if à cause des premières conditions
				if Recherche_Frequence1 /= null then
					if Min > Arbre.All.Frequence then
						Min := Recherche_Frequence1.All.Frequence;
						Adresse := Recherche_Frequence1.All.Adresse;
					else
						null; -- il ne ne passe rien
					end if;
				elsif Recherche_Frequence2 /= null then
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

		procedure Supprimer_LFU(Arbre : in T_Arbre; Masque : in T_Adresse_IP) is
			Adresse : T_Adresse_IP;
			Suppresseur : T_Arbre;
			Taille_Masque : Integer;
		begin
			-- Il faut faire la recherche du minimum en terme de fréquence et noter son adresse (= le parcours) ainsi que créer un pointeur temporaire
			Adresse := Recherche_Frequence_Min(Arbre); -- pas d'erreur retournée étant donné que le cache est plein (il existe au moins une adresse)
			Suppresseur := Arbre;
			Taille_Masque := Get_taille_binaire(Masque);

			-- On regarde pour chaque bit de l'adresse si il vaut 0 ou 1 pour savoir quelle direction prendre
			for i in 0..(Taille_Masque - 1) loop
				if ((Adresse AND (2 ** (Taille_Masque - 1 - i))) = 0) then
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
		case T_Politique'Pos(Politique) is
			when 1 => Supprimer_FIFO(Arbre, Masque); -- FIFO
			when 2 => Supprimer_LRU(Arbre, Masque); -- LRU
			when 3 => Supprimer_LFU(Arbre, Masque); -- LFU
			when others => raise Politique_non_valide_exception;
		end case;

		Cache.Taille := Cache.Taille - 1;

	exception
		when Politique_non_valide_exception => Put("La politique demandée n'est pas valide.");
	end Supprimer;

	function Est_Plein(Cache : in T_Cache_Arbre; Taille : in Integer) return Boolean is
		Est_Plein : Boolean;
	begin
		if Cache.Taille >= Taille then
			Est_Plein := True;
		else
			Est_Plein := False;
		end if;

		return Est_Plein;
	end Est_Plein;

	procedure Afficher_Arbre(Arbre : in T_Arbre) is
		Afficheur1 : T_Arbre;
		Afficheur2 : T_Arbre;
		Compteur : Integer := 0; -- pour compter les feuilles, ce qui doit correspondre à la taille du cache
	begin
		-- Initialisation des pointeurs qui servent à afficher l'arbre
		Afficheur1 := Arbre;
		Afficheur2 := Arbre;

		-- Le parcours est en profondeur, on explore tout ce qu'il y a à gauche
		if not Afficheur1.Gauche.All.Feuille then
			-- Tant que le chemin gauche de l'arbre n'est pas nul, on avance
			Afficher_Arbre(Afficheur1.All.Gauche);
		else
			-- Sinon on affiche car la cellule est une feuille
			Afficheur1 := Afficheur1.All.Gauche;
			Compteur := Compteur + 1;

			Put_Line("Feuille" & Integer'Image(Compteur));
			Put_Line(T_Adresse_IP'Image(Afficheur1.All.Adresse));
			Put_Line(T_Adresse_IP'Image(Afficheur1.All.Adresse));
			New_Line;
		end if;

		-- Le parcours est en profondeur, on explore tout ce qu'il y a à droite
		if not Afficheur2.Droite.All.Feuille then
			-- Tant que le chemin gauche de l'arbre n'est pas nul, on avance
			Afficher_Arbre(Afficheur2.All.Droite);
		else
			-- Cas où la cellule est une feuille
			Afficheur2 := Afficheur2.All.Droite;
			Compteur := Compteur + 1;

			Put_Line("Feuille" & Integer'Image(Compteur));
			Put_Line(T_Adresse_IP'Image(Afficheur2.All.Adresse));
			Put_Line(T_Adresse_IP'Image(Afficheur2.All.Adresse));
			New_Line;
		end if;
	end Afficher_Arbre;

	procedure Afficher_Statistiques_Cache(Cache : in T_Cache_Arbre) is
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
			if Recherche_Identifiant1 /= null and then Recherche_Identifiant1.Gauche /= null then
				if Max < Recherche_Identifiant1.All.Identifiant then
					Max := Recherche_Identifiant1.All.Identifiant;
				else
					null; -- il ne ne passe rien
				end if;

				Recherche_Identifiant1 := Recherche_Identifiant1.All.Gauche;

				Max := Recherche_Identifiant_Max(Recherche_Identifiant1); -- on procède par récursivité (on se dédouble à chaque fois, un peu comme le calcul de la FFT)
			elsif Recherche_Identifiant2 /= null and then Recherche_Identifiant2.Droite /= null then
				if Max < Recherche_Identifiant2.All.Identifiant then
					Max := Recherche_Identifiant2.All.Identifiant;
				else
					null; -- il ne se passe rien
				end if;

				Recherche_Identifiant2 := Recherche_Identifiant2.All.Droite;

				Max := Recherche_Identifiant_Max(Recherche_Identifiant2); -- on procède par récursivité
			else
				-- On regarde les cas où on sort des if à cause des premières conditions
				if Recherche_Identifiant1 /= null then
					if Max < Arbre.All.Identifiant then
						Max := Recherche_Identifiant1.All.Identifiant;
					else
						null; -- il ne ne passe rien
					end if;
				elsif Recherche_Identifiant2 /= null then
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

		function Chercher_Cache(Cache : in out T_Cache_Arbre; Adresse : in T_Adresse_IP; Politique : in T_Politique; Masque : in T_Adresse_IP) return Unbounded_string is
        	Max : Integer;
        	Arbre : T_Arbre := Arbre_Cache(Cache);
        	Taille_Masque : Integer;
    	begin

			if Est_Vide(Arbre) then
			-- Cas où le cache est vide
				raise Adresse_Absente_Exception;
			else
				Put_Line("Le cache n'est pas vide. On peut continuer.");
			end if;

			Taille_Masque := Get_taille_binaire(Masque);

			-- On regarde pour chaque bit de l'adresse si il vaut 0 ou 1 pour savoir quelle direction prendre
			for i in 0..(Taille_Masque - 1) loop
				if ((Adresse AND (2 ** (Taille_Masque - 1 - i))) = 0) then
					--  Cas où le bit vaut 0
					if Est_Vide(Arbre.Gauche) then
					-- Cas où le cache à gauche est vide
						raise Adresse_Absente_Exception;
					else
						Arbre := Arbre.All.Gauche;
					end if;
				else
					-- Cas où le bit vaut 1
					if Est_Vide(Arbre.Droite) then
					-- Cas où le cache à droite est vide
						raise Adresse_Absente_Exception;
					else
						Arbre := Arbre.All.Droite;
					end if;
				end if;
			end loop;

			-- On devrait être au niveau de la feuille correspondante à l'adresse désormais
			Arbre.All.Frequence := Arbre.All.Frequence + 1;
        	Max := Recherche_Identifiant_Max(Arbre);
        	Cache.Demandes := Cache.Demandes + 1;

       		-- Je mets à jour la politique LRU
        	if (T_Politique'Pos(Politique) = 2) then -- LRU
        	    if Arbre.All.Identifiant = Max then
        	        null; -- rien à faire
        	    else
        	        Arbre.All.Identifiant := Max + 1; -- devient le plus récemment utilisé
        	    end if;
        	else
        	    null; -- rien à faire
        	end if;

        	return Arbre.All.Sortie;

    	exception
        	when Adresse_Absente_Exception => Put_Line("L'adresse n'a pas été trouvée");
    	end Chercher_Cache;

end cache_tree;
