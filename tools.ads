with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package tools is

    type T_Politique is (FIFO, LRU, LFU);

    type T_Param is record

        taille_cache : Integer; -- Parametre [-c]
        afficher_stats : Boolean; -- Parametre [-s] et [-S]
        file_table_routage : Unbounded_String; -- Parametre [-t]
        file_paquets : Unbounded_String; -- Parametre [-p]
        file_resultats : Unbounded_String; -- Parametre [-r]
        politique : T_Politique; -- Parametre [-P]

    end record;

    type T_Adresse_IP is mod 2 ** 32;

    -- function is_Param_Valid(parametre : Character) return Boolean;

    -- function is_File_Valid(file : String) return Boolean;

    function Initialiser return T_Param;

    procedure Afficher_Param(param : T_Param);

end tools;
