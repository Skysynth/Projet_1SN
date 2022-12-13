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

    function Initialiser_Param return T_Param;

    procedure Afficher_Param(param : T_Param);

    procedure Remplir_Param(param : out T_Param);

    function Get_taille_binaire(adresse : T_Adresse_IP) return Integer;

    function Unbounded_String_To_Adresse_IP(ligne : Unbounded_String) return T_Adresse_IP;

    function Adresse_IP_To_String(adresse : T_Adresse_IP) return String;

end tools;
