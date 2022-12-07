with tools; use tools;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

-- Definition de structures de donnees associatives sous forme d'une liste
-- chaine associative

package Table_Routage is

    type T_Table_Routage is limited private;
    
    function Convert_Unbounded_String_To_T_Adresse_IP(ligne : Unbounded_String) return T_Adresse_IP;
    
    procedure Afficher(Table_Routage : in T_Table_Routage);
    
    -- Initialiser une table de routage.  La table de routage est vide.
    procedure Initialiser(param : in T_Param; table_routage: out T_Table_Routage);


    -- Est-ce qu'une Table_Routage est vide ?
    function Est_Vide (Table_Routage : in T_Table_Routage) return Boolean;


    -- Obtenir le nombre d'�l�ments d'une Table_Routage. 
    function Taille (Table_Routage : in T_Table_Routage) return Integer with
        Post => Taille'Result >= 0
        and (Taille'Result = 0) = Est_Vide (Table_Routage);


    -- Enregistrer une Donnee associee a une Cle dans une Table_Routage.
    -- Si la cle est deja presente dans la Table_Routage, sa donnee est changee.
    procedure Enregistrer (Table_Routage : in out T_Table_Routage ; adresse : T_Adresse_IP);

    -- Supprimer la Donnee associee a une Cle dans une Table_Routage.
    -- Exception : Cle_Absente_Exception si Cle n'est pas utilisee dans la Table_Routage
    procedure Supprimer (Table_Routage : in out T_Table_Routage ; adresse : T_Adresse_IP);


    -- Savoir si une Cle est presente dans une Table_Routage.
    function Cle_Presente (Table_Routage : in T_Table_Routage ; adresse : T_Adresse_IP) return Boolean;


    -- Obtenir la donnee associee a une Cle dans la Table_Routage.
    function La_Donnee (Table_Routage : in T_Table_Routage ; adresse : T_Adresse_IP) return T_Adresse_IP;


    -- Supprimer tous les elements d'une Table_Routage.
    procedure Vider (Table_Routage : in out T_Table_Routage) with
        Post => Est_Vide (Table_Routage);


    -- Appliquer un traitement (Traiter) pour chaque couple d'une Table_Routage
        with procedure Traiter (Donnee: in T_Adresse_IP);
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


