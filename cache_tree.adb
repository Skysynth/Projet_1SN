-- with Ada.Text_IO; use Ada.Text_IO;
with SDA_Exceptions; use SDA_Exceptions;
with Ada.Unchecked_Deallocation;

package body cache_tree is

	procedure Free is
		new Ada.Unchecked_Deallocation (Object => T_Cellule, Name => T_Cache);

	procedure Initialiser (Sda: out T_Cache) is
	begin
		Cache := Null;
	end Initialiser;


	function Est_Vide (Sda : Cache) return Boolean is
	begin
		return (Cache = Null);
	end;


	function Taille (Sda : in T_LCA) return Integer is
		Result: Integer;
	begin
		if Est_Vide (Sda) then
			-- Le pointeur est vide donc sa taille vaut 0
		 	Result := 0;
		else
			-- Récursivité sur la taille en ajoutant + 1 à chaque itération
			Result := Taille (Sda.All.Suivant) + 1;
		end if;

		return Result;
	end Taille;


end cache_tree;
