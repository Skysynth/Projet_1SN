with tools; use tools;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

-- Definition de structures de donnees associatives sous forme d'une liste
-- chaine associative

package Table_Routage is

    type T_Table_Routage is limited private;
    
    function Convert_Unbounded_String_To_T_Adresse_IP(ligne : Unbounded_String) return T_Adresse_IP;
    
    procedure Afficher(Table_Routage : in T_Table_Routage);
    
    -- Initialiser une table de routage.  La table de routage est vide.
    procedure Initialiser(param : in T_Param; Table_routage: out T_Table_Routage);
    
    function Get_taille_binaire(adresse : T_Adresse_IP) return Integer;
        
    function Get_Interface(Adresse_IP: in T_Adresse_IP; Table_Routage: in T_Table_Routage) return Unbounded_String;

    -- Est-ce qu'une Table_Routage est vide ?
    function Est_Vide (Table_Routage : in T_Table_Routage) return Boolean;


	-- Obtenir le nombre d'elements d'une Table_Routage. 
	function Taille (Table_Routage : in T_Table_Routage) return Integer with
		Post => Taille'Result >= 0
			and (Taille'Result = 0) = Est_Vide (Table_Routage);


    -- Enregistrer une Donnee associee a une Cle dans une Table_Routage.
    -- Si la cle est deja presente dans la Table_Routage, sa donnee est changee.
    procedure Enregistrer (Table_Routage : in out T_Table_Routage ; Adresse : in T_Adresse_IP; Masque : in T_Adresse_IP; Sortie : in Unbounded_String );

	-- Supprimer la Donnee associee a une Cle dans une Table_Routage.
	-- Exception : Cle_Absente_Exception si Cle n'est pas utilisee dans la Table_Routage
	procedure Supprimer (Table_Routage : in out T_Table_Routage ; adresse : in T_Adresse_IP) with
		Post =>  Taille (Table_Routage) = Taille (Table_Routage)'Old - 1 -- un element de moins
			and not Adresse_Presente(Table_Routage, adresse);         -- la cle a ete supprimee


    function Adresse_Presente (Table_Routage : in T_Table_Routage ; adresse : in T_Adresse_IP) return Boolean;

	-- Supprimer tous les elements d'une Table_Routage.
	procedure Vider (Table_Routage : in out T_Table_Routage) with
		Post => Est_Vide (Table_Routage);

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


