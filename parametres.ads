with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package PARAMETRES is

    type T_Politique is limited private;

    type T_Param is limited private;

    type T_Adresse_IP is private;

    -- function is_Param_Valid(Parametre : Character) return Boolean;

    -- function is_File_Valid(file : String) return Boolean;

    function Initialiser return T_Param;

    procedure Afficher_Param(param : T_Param);

private

    type T_Politique is (FIFO, LRU, LFU);

    type T_Param is record

        taille_cache : Integer; -- parametre -c
        afficher_stats : Boolean; -- parametre -s et -S

        file_table_routage : Unbounded_String; -- parametre -t
        file_paquets : Unbounded_String; -- parametre -p
        file_resultats : Unbounded_String; -- parametre -r

        politique : T_Politique; -- parametre -P

    end record;

    type T_Adresse_IP is mod 2 ** 32;

end PARAMETRES;
