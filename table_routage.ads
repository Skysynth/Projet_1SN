
-- Definition de structures de donnees associatives sous forme d'une liste
-- chaine associative
generic
	type T_Cle is private;
	type T_Donnee is private;
	type T_Adresse_IP is private;


package Table_Routage is

	type T_Table_Routage is limited private;

	-- Initialiser une table de routage.  La table de routage est vide.
	procedure Initialiser(table_routage: out T_Table_Routage) with
		Post => Est_Vide (table_routage);


	-- Est-ce qu'une Table_Routage est vide ?
	function Est_Vide (Table_Routage : T_Table_Routage) return Boolean;


	-- Obtenir le nombre d'�l�ments d'une Table_Routage. 
	function Taille (Table_Routage : in T_Table_Routage) return Integer with
		Post => Taille'Result >= 0
			and (Taille'Result = 0) = Est_Vide (Table_Routage);


	-- Enregistrer une Donnee associee a une Cle dans une Table_Routage.
	-- Si la cle est deja presente dans la Table_Routage, sa donnee est changee.
	procedure Enregistrer (Table_Routage : in out T_Table_Routage ; Cle : in T_Cle ; Donnee : in T_Donnee) with
		Post => Cle_Presente (Table_Routage, Cle) and (La_Donnee (Table_Routage, Cle) = Donnee)   -- donn�e ins�r�e
				and (not (Cle_Presente (Table_Routage, Cle)'Old) or Taille (Table_Routage) = Taille (Table_Routage)'Old)
				and (Cle_Presente (Table_Routage, Cle)'Old or Taille (Table_Routage) = Taille (Table_Routage)'Old + 1);

	-- Supprimer la Donn�e associ�e � une Cl� dans une Table_Routage.
	-- Exception : Cle_Absente_Exception si Cl� n'est pas utilis�e dans la Table_Routage
	procedure Supprimer (Table_Routage : in out T_Table_Routage ; Cle : in T_Cle) with
		Post =>  Taille (Table_Routage) = Taille (Table_Routage)'Old - 1 -- un �l�ment de moins
			and not Cle_Presente (Table_Routage, Cle);         -- la cl� a �t� supprim�e


	-- Savoir si une Cl� est pr�sente dans une Table_Routage.
	function Cle_Presente (Table_Routage : in T_Table_Routage ; Cle : in T_Cle) return Boolean;


	-- Obtenir la donn�e associ�e � une Cle dans la Table_Routage.
	-- Exception : Cle_Absente_Exception si Cl� n'est pas utilis�e dans l'Table_Routage
	function La_Donnee (Table_Routage : in T_Table_Routage ; Cle : in T_Cle) return T_Donnee;


	-- Supprimer tous les �l�ments d'une Table_Routage.
	procedure Vider (Table_Routage : in out T_Table_Routage) with
		Post => Est_Vide (Table_Routage);


	-- Appliquer un traitement (Traiter) pour chaque couple d'une Table_Routage.
	generic
		with procedure Traiter (Cle : in T_Cle; Donnee: in T_Donnee);
	procedure Pour_Chaque (Table_Routage : in T_Table_Routage);


private

	type T_Cellule;

	type T_Table_Routage is access T_Cellule;

	type T_Cellule is
		record
			Adresse : T_Adresse_IP;
    		Masque : T_Adresse_IP;
			Sortie : Unbounded_String;
			Suivant : T_Table_Routage;
		end record;

end Table_Routage;


