with Ada.Text_IO; use Ada.Text_IO;
with routeur_exceptions; use routeur_exceptions;
with Ada.Unchecked_Deallocation;

package body cache_tree is

	procedure Free is
		new Ada.Unchecked_Deallocation(Object => T_Cache_Cellule, Name => T_Arbre);

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

	procedure Enregistrer(Arbre : in out T_Arbre; Cache : in out T_Cache_Arbre; Adresse : in T_Adresse_IP; Masque : in T_Adresse_IP; Sortie : in Unbounded_String) is
	begin
		-- Cas où le cache est vide
		if Est_Vide(Arbre) then
			Arbre := new T_Cache_Cellule'(Adresse, Masque, Sortie, null, null, 0, False);
			Cache.Taille := Cache.Taille + 1;
		else
			Put_Line("Le cache n'est pas vide. On peut continuer.");
		end if;

		-- On regarde pour chaque bit de l'adresse si il vaut 0 ou 1 pour savoir quelle direction prendre
		for i in 0..31 loop
			if ((Adresse AND (2 ** (31 - i))) = 0) then
				--  Cas où le bit vaut 0
				if Est_Vide(Arbre.All.Gauche) then
				-- Cas où le cache à gauche est vide
					Arbre.All.Gauche := new T_Cache_Cellule'(Adresse, Masque, Sortie, null, null, 0, False);
					Cache.Taille := Cache.Taille + 1;
					Arbre := Arbre.All.Gauche;
				else
					Arbre := Arbre.All.Gauche;
				end if;
			else
				-- Cas où le bit vaut 1
				if Est_Vide(Arbre.All.Droite) then
				-- Cas où le cache à droite est vide
					Arbre.All.Droite := new T_Cache_Cellule'(Adresse, Masque, Sortie, null, null, 0, False);
					Cache.Taille := Cache.Taille + 1;
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
		Arbre.All.Active := True;
	end Enregistrer;

	procedure Ajouter_Frequence(Arbre : in out T_Arbre; Adresse : in T_Adresse_IP) is
	begin
		if Est_Vide(Arbre) then
		-- Cas où le cache est vide
			raise Adresse_Absente_Exception;
		else
			Put_Line("Le cache n'est pas vide. On peut continuer.");
		end if;

		-- On regarde pour chaque bit de l'adresse si il vaut 0 ou 1 pour savoir quelle direction prendre
		for i in 0..(31 - 1) loop
			if ((Adresse AND (2 ** (31 - i))) = 0) then
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

	procedure Supprimer(Arbre : in out T_Arbre; Cache : in out T_Cache_Arbre; Politique : in T_Politique) is

		procedure Supprimer_FIFO(Arbre : in T_Arbre) is
		begin
			null; -- à compléter
		end Supprimer_FIFO;

		procedure Supprimer_LRU(Arbre : in T_Arbre) is
		begin
			null; -- à compléter
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

		procedure Supprimer_LFU(Arbre : in T_Arbre) is
			Adresse : T_Adresse_IP;
			Suppresseur : T_Arbre;
		begin
			-- Il faut faire la recherche du minimum en terme de fréquence et noter son adresse (= le parcours) ainsi que créer un pointeur temporaire
			Adresse := Recherche_Frequence_Min(Arbre); -- pas d'erreur retournée étant donné que le cache est plein (il existe au moins une adresse)
			Suppresseur := Arbre;

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
			Free(Suppresseur);
		end Supprimer_LFU;

	begin
		-- On regarde quelle est la procédure
		case T_Politique'Pos(Politique) is
			when 1 => Supprimer_FIFO(Arbre); -- FIFO : à faire (peut être)
			when 2 => Supprimer_LRU(Arbre); -- LRU : à faire (peut être)
			when 3 => Supprimer_LFU(Arbre); -- LFU
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

	procedure Afficher_Cache(Cache : in T_Cache_Arbre) is
		Afficheur : T_Arbre;
	begin
		null ; -- à compléter
	end Afficher_Cache;

end cache_tree;
