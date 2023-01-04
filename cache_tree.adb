-- with Ada.Text_IO; use Ada.Text_IO;
with SDA_Exceptions; use SDA_Exceptions;
with Ada.Unchecked_Deallocation;

package body cache_tree is

	procedure Free is
		new Ada.Unchecked_Deallocation(Object => T_Cache_Cellule, Name => T_Cache);

	procedure Initialiser(Sda : out T_Cache) is
	begin
		Cache := Null;
	end Initialiser;


	function Est_Vide(Cache : in T_Cache) return Boolean is
	begin
		return (Cache = Null);
	end;

    procedure Vider(Cache : in out T_Cache) is
	begin
        if not Est_Vide(Cache) then
            -- Si le cache n'est pas vide
			Vider(Cache.All.Gauche);
            Vider(Cache.All.Droit);
            Free(Cache.All);
        else
			-- Si le cache est vide
            Put_Line("Le cache est pas vide. Pas besoin de le vider.");
        end if;
	end Vider;

	procedure Enregistrer(Cache : in out T_Cache_Arbre; Adresse : in T_Adresse_IP; Masque : T_Adresse_IP; Sortie : Unbounded_String) is
		Compteur_Taille : T_Cache_Arbre;
	begin
		-- On initialise le compteur pour la taille
		Compteur_Taille := Cache;

		-- Cas où le cache est vide
		if Est_Vide(Cache) then
			Cache := new T_Cache_Cellule'(Taille, Adresse, Masque, Sortie, null, null, Frequence, Active);
			Compteur_Taille.All.Taille := Compteur_Taille.All.Taille + 1;
		else
			Put_Line("Le cache n'est pas vide. On peut continuer.");
		end if;

		-- On stocke la taille binaire de l'adresse
		Taille_Adresse := Get_taille_binaire(Adresse);

		-- On convertit l'adresse IP en binaire ainsi que le cache
		-- On regarde pour chaque bit si il vaut 0 ou 1 pour savoir quelle direction prendre
		for i in 0..(Taille_Adresse - 1) loop
			if (Adresse AND (2 ** (Taille_Adresse - i)) = 0) then
				--  Cas où le bit vaut 0
				if Est_Vide(Cache.Gauche) then
				-- Cas où le cache à gauche est vide
					Cache.Gauche := new T_Cache_Cellule'(Taille, Adresse, Masque, Sortie, null, null, Frequence, Active);
					Compteur_Taille.All.Taille := Compteur_Taille.All.Taille + 1;
					Cache := Cache.All.Gauche;
				else
					Cache := Cache.All.Gauche;
				end if;
			else
				-- Cas où le bit vaut 1
				if Est_Vide(Cache.Droite) then
				-- Cas où le cache à droite est vide
					Cache.Droite := new T_Cache_Cellule'(Taille, Adresse, Masque, Sortie, null, null, Frequence, Active);
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

	procedure Ajouter_Frequence(Cache : in out T_Cache; Adresse : in T_Adresse_IP) is
	begin
		if Est_Vide(Cache) then
		-- Cas où le cache est vide
			Cache := new T_Cache_Cellule'(Taille, Adresse, Masque, Sortie, null, null, Frequence, Active);
			Compteur_Taille.All.Taille := Compteur_Taille.All.Taille + 1;
		else
			Put_Line("Le cache n'est pas vide. On peut continuer.");
		end if;

		-- On stocke la taille binaire de l'adresse
		Taille_Adresse := Get_taille_binaire(Adresse);

		-- On convertit l'adresse IP en binaire ainsi que le cache
		-- On regarde pour chaque bit si il vaut 0 ou 1 pour savoir quelle direction prendre
		for i in 0..(Taille_Adresse - 1) loop
			if (Adresse AND (2 ** (Taille_Adresse - i)) = 0) then
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

	procedure Supprimer(Cache : in out T_Cache; Politique : in T_Politique) is
		Compteur_Taille : T_Cache;

		procedure Supprimer_FIFO(Cache : in out T_Cache) is
		begin
			null; -- à compléter
		end Supprimer_FIFO;

		procedure Supprimer_LRU(Cache : in out T_Cache) is
		begin
			null; -- à compléter
		end Supprimer_FIFO;

		procedure Supprimer_LFU(Cache : in out T_Cache) is
		begin
			null; -- à compléter
		end Supprimer_FIFO;

	begin
		-- On initialise le compteur pour la taille
		Compteur_Taille := Cache;

		-- On regarde quelle est la procédure
		case Politique is
			when (Politique'Val = FIFO) => Supprimer_FIFO(Cache);
			when (Politique'Val = LRU) => Supprimer_LRU(Cache);
			when (Politique'Val = LFU) => Supprimer_LFU(Cache);
		end case;
	end Supprimer;

	function Est_Plein(Cache : in T_Cache) return Boolean is
	begin
	end Est_Plein;

end cache_tree;
