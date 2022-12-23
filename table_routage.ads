with tools; use tools;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

-- Definition de structures de donnees associatives sous forme d'une liste
-- chaine associative

-- R1 : Concevoir et initialiser un routeur
package Table_Routage is

    type T_Table_Routage is private;
    
    -- R2 : Initialiser une table de routage.  La table de routage est vide.
    procedure Initialiser(param : in T_Param; Table_routage: out T_Table_Routage);

    
    
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
    
	-- Supprimer tous les elements d'une Table_Routage.
	procedure Vider (Table_Routage : in out T_Table_Routage) with
		Post => Est_Vide (Table_Routage);

    
    procedure Afficher(Table_Routage : in T_Table_Routage; file : File_Type);

    
    function Get_Interface(Adresse_IP: in T_Adresse_IP; Table_Routage: in T_Table_Routage) return Unbounded_String;
    
    -- fonction qui permettent d acceder aux differentes valeur ou pointe les pointeurs
    function Get_Adresse(Table_Routage: in T_Table_Routage) return T_Adresse_IP;
    function Get_Masque(Table_Routage: in T_Table_Routage) return T_Adresse_IP;
    function Get_Sortie(Table_Routage: in T_Table_Routage) return Unbounded_String;
    function Get_Suivant(Table_Routage: in T_Table_Routage) return T_Table_Routage;
    
    function Is_Command_And_Then_Execute(ligne : String; tr : T_Table_Routage; file_output : File_Type; num_ligne : Integer) return Boolean;
    
   
    function Adresse_Presente (Table_Routage : in T_Table_Routage ; adresse : in T_Adresse_IP) return Boolean;

    
    
    
private

    -- R2 : Concevoir et initialiser un routeur

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


