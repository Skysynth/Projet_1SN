-- with Ada.Text_IO; use Ada.Text_IO;
with SDA_Exceptions; use SDA_Exceptions;
with Ada.Unchecked_Deallocation;

package body cache_tree is

	procedure Free is
		new Ada.Unchecked_Deallocation (Object => T_Cache_Cellule, Name => T_Cache);

	procedure Initialiser (Sda : out T_Cache) is
	begin
		Cache := Null;
	end Initialiser;


	function Est_Vide (Cache : in T_Cache) return Boolean is
	begin
		return (Cache = Null);
	end;


	procedure Taille_Cache (Cache : in T_Cache; Taille: in Integer) return Integer is
	begin
		if Est_Vide (Cache) then
			Cache := new T_Cache_Cellule(Taille, Adresse, Masque, Interface, Null, Null);
		else
			Null;
		end if;

        Taille_Cache(Cache.Gauche, Taille - 1);
        Taille_Cache(Cache.Droit, Taille - 1);
	end Taille_Cache;


end cache_tree;
