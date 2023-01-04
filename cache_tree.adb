with Ada.Text_IO; use Ada.Text_IO;
with routeur_exceptions; use routeur_exceptions;
with Ada.Unchecked_Deallocation;

package body cache_tree is

	procedure Free is
		new Ada.Unchecked_Deallocation(Object => T_Cache_Cellule, Name => T_Cache_Arbre);

	procedure Initialiser(Cache : out T_Cache_Arbre) is
	begin
		Cache := Null;
	end Initialiser;

	function Est_Vide(Cache : in T_Cache_Arbre) return Boolean is
	begin
		return (Cache = Null);
	end;

    procedure Vider(Cache : in out T_Cache_Arbre) is
	begin
        if not Est_Vide(Cache) then
            -- Si le cache n'est pas vide
			Vider(Cache.All.Gauche);
            Vider(Cache.All.Droite);
            Free(Cache);
        else
			-- Si le cache est vide
            Put_Line("Le cache est pas vide. Pas besoin de le vider.");
        end if;
	end Vider;

	procedure Enregistrer(Cache : in out T_Cache_Arbre; Adresse : in T_Adresse_IP; Masque : T_Adresse_IP; Sortie : Unbounded_String; Taille : Integer) is
		Compteur_Taille : T_Cache_Arbre;
	begin
		-- On initialise le compteur pour la taille
		Compteur_Taille := Cache;


		-- Cas où le cache est vide
		if Est_Vide(Cache) then
			Cache := new T_Cache_Cellule'(Taille, Adresse, Masque, Sortie, null, null, 0, False);
			Compteur_Taille.All.Taille := Compteur_Taille.All.Taille + 1;
		else
			Put_Line("Le cache n'est pas vide. On peut continuer.");
		end if;

		-- On regarde pour chaque bit de l'adresse si il vaut 0 ou 1 pour savoir quelle direction prendre
		for i in 0..31 loop
			if ((Adresse AND (2 ** (31 - i))) = 0) then
				--  Cas où le bit vaut 0
				if Est_Vide(Cache.All.Gauche) then
				-- Cas où le cache à gauche est vide
					Cache.All.Gauche := new T_Cache_Cellule'(Taille, Adresse, Masque, Sortie, null, null, 0, False);
					Compteur_Taille.All.Taille := Compteur_Taille.All.Taille + 1;
					Cache := Cache.All.Gauche;
				else
					Cache := Cache.All.Gauche;
				end if;
			else
				-- Cas où le bit vaut 1
				if Est_Vide(Cache.All.Droite) then
				-- Cas où le cache à droite est vide
					Cache.All.Droite := new T_Cache_Cellule'(Taille, Adresse, Masque, Sortie, null, null, 0, False);
					Compteur_Taille.All.Taille := Compteur_Taille.All.Taille + 1;
					Cache := Cache.All.Droite;
				else
					Cache := Cache.All.Droite;
				end if;
			end if;
		end loop;

		-- On devrait être au niveau d'une feuille à présent
		-- Il reste à stocker toutes les informations nécessaires
		Cache.All.Adresse := Adresse;
		Cache.All.Masque := Masque;
		Cache.All.Sortie := Sortie;
		Cache.All.Active := True;

		-- On détruit le compteur pour la taille
		Free(Compteur_Taille);
	end Enregistrer;

	procedure Ajouter_Frequence(Cache : in out T_Cache_Arbre; Adresse : in T_Adresse_IP) is
	begin
		if Est_Vide(Cache) then
		-- Cas où le cache est vide
			raise Adresse_Absente_Exception;
		else
			Put_Line("Le cache n'est pas vide. On peut continuer.");
		end if;

		-- On regarde pour chaque bit de l'adresse si il vaut 0 ou 1 pour savoir quelle direction prendre
		for i in 0..(31 - 1) loop
			if ((Adresse AND (2 ** (31 - i))) = 0) then
				--  Cas où le bit vaut 0
				if Est_Vide(Cache.Gauche) then
				-- Cas où le cache à gauche est vide
					raise Adresse_Absente_Exception;
				else
					Cache := Cache.All.Gauche;
				end if;
			else
				-- Cas où le bit vaut 1
				if Est_Vide(Cache.Droite) then
				-- Cas où le cache à droite est vide
					raise Adresse_Absente_Exception;
				else
					Cache := Cache.All.Droite;
				end if;
			end if;
		end loop;

		-- On devrait être au niveau de la feuille correspondante à l'adresse désormais
		Cache.All.Frequence := Cache.All.Frequence + 1;
	exception
		when Adresse_Absente_Exception => Put("L'adresse demandée n'est pas présente.");
	end Ajouter_Frequence;

	procedure Supprimer(Cache : in out T_Cache_Arbre; Politique : in T_Politique; Taille : in Integer) is
		Compteur_Taille : T_Cache_Arbre;

		procedure Supprimer_FIFO(Cache : in out T_Cache_Arbre) is
		begin
			null; -- à compléter
			Cache.All.Taille := Cache.All.Taille - 1;
		end Supprimer_FIFO;

		procedure Supprimer_LRU(Cache : in out T_Cache_Arbre) is
		begin
			null; -- à compléter
			Cache.All.Taille := Cache.All.Taille - 1;
		end Supprimer_LRU;

		function Recherche_Frequence_Min(Cache : in T_Cache_Arbre) return T_Adresse_IP is
			Recherche_Frequence1 : T_Cache_Arbre;
			Recherche_Frequence2 : T_Cache_Arbre;
			Min : Integer;
			Adresse : T_Adresse_IP;
		begin
			-- On fait pointer les pointeurs sur la racine
			Recherche_Frequence1 := Cache;
			Recherche_Frequence2 := Cache;
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
					if Min > Cache.All.Frequence then
						Min := Recherche_Frequence1.All.Frequence;
						Adresse := Recherche_Frequence1.All.Adresse;
					else
						null; -- il ne ne passe rien
					end if;
				elsif Recherche_Frequence2 /= null then
					if Min > Cache.All.Frequence then
						Min := Recherche_Frequence2.All.Frequence;
						Adresse := Recherche_Frequence1.All.Adresse;
					else
						null; -- il ne ne passe rien
					end if;
				else
					null; -- il ne se passe rien
				end if;
			end if;

			return Adresse;
		end Recherche_Frequence_Min;

		procedure Supprimer_LFU(Cache : in out T_Cache_Arbre) is
			Adresse : T_Adresse_IP;
			Suppresseur : T_Cache_Arbre;
		begin
			-- Il faut faire la recherche du minimum en terme de fréquence et noter son adresse (= le parcours) ainsi que créer un pointeur temporaire
			Adresse := Recherche_Frequence_Min(Cache); -- pas d'erreur retournée étant donné que le cache est plein (il existe au moins une adresse)
			Suppresseur := Cache;

			-- On regarde pour chaque bit de l'adresse si il vaut 0 ou 1 pour savoir quelle direction prendre
			for i in 0..(31 - 1) loop
				if ((Adresse AND (2 ** (31 - i))) = 0) then
					--  Cas où le bit vaut 0
						Suppresseur := Suppresseur.All.Gauche;
				else
					-- Cas où le bit vaut 1
						Suppresseur := Suppresseur.All.Droite;
				end if;
			end loop;

			-- Il ne reste plus qu'à supprimer cette cellule et diminiuer la taille de 1
			Cache.All.Taille := Cache.All.Taille - 1;
			Free(Suppresseur);
		end Supprimer_LFU;

	begin
		-- On regarde quelle est la procédure
		case T_Politique'Pos(Politique) is
			-- when 1 => Supprimer_FIFO(Cache); -- FIFO : à faire (peut être)
			-- when 2 => Supprimer_LRU(Cache); -- LRU : à faire (peut être)
			when 3 => Supprimer_LFU(Cache); -- LFU
			when others => raise Politique_non_valide_exception;
		end case;

	exception
		when Politique_non_valide_exception => Put("La politique demandée n'est pas valide.");
	end Supprimer;

	function Est_Plein(Cache : in T_Cache_Arbre; Taille : in Integer) return Boolean is
		Est_Plein : Boolean;
	begin
		if Cache.All.Taille >= Taille then
			Est_Plein := True;
		else
			Est_Plein := False;
		end if;

		return Est_Plein;
	end Est_Plein;

	procedure Afficher_Cache(Cache : in T_Cache_Arbre) is
		Afficheur : T_Cache_Arbre;
	begin
		null ; -- à compléter
	end Afficher_Cache;

end cache_tree;
