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


	procedure Taille_Cache(Cache : in T_Cache; Taille: in Integer) return Integer is
	begin
		if Est_Vide(Cache) then
			Cache := new T_Cache_Cellule(Taille, Adresse, Masque, Interface, Null, Null);
		else
			Null; -- à compléter
		end if;

        Taille_Cache(Cache.Gauche, Taille - 1);
        Taille_Cache(Cache.Droit, Taille - 1);
	end Taille_Cache;

    procedure Vider(Cache : in out T_Cache) is
	begin
        if Est_Vide(Cache) then
            Null;
        else
            Vider(Cache.All.Gauche);
            Vider(Cache.All.Droit);
            Free(Cache.All);
        end if;
	end Vider;

	procedure Enregistrer(Cache : in out T_Cache_Arbre; Adresse : in T_Adresse_IP; Masque : T_Adresse_IP; Sortie : Unbounded_String) is
		bit : Integer; -- soit 0 ou 1 (voir pour mettre un modulo 1)
	begin
		-- Cas où le cache est vide
		if Est_Vide(Cache) then
			Cache := new T_Cache_Cellule'(Taille, Adresse, Masque, Sortie, null, null, Frequence, Active);
		else
			null;
		end if;

		-- On convertit l'adresse IP en binaire ainsi que le cache
		-- On regarde pour chaque bit si il vaut 0 ou 1 pour savoir quelle direction prendre
		for i in 1..Get_taille_binaire(Adresse) loop
			if bit = 0 then
				--  Cas où le bit vaut 0
				if Est_Vide(Cache.Gauche) then
				-- Cas où le cache à gauche est vide
					Cache.Gauche := new T_Cache_Cellule'(Taille, Adresse, Masque, Sortie, null, null, Frequence, Active);
					Cache := Cache.All.Gauche;
				else
					Cache := Cache.All.Gauche;
				end if;
			else
				-- Cas où le bit vaut 1
				if Est_Vide(Cache.Droite) then
				-- Cas où le cache à droite est vide
					Cache.Droite := new T_Cache_Cellule'(Taille, Adresse, Masque, Sortie, null, null, Frequence, Active);
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
	end Enregistrer;


end cache_tree;
