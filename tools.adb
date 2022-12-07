with Ada.Text_IO;            use Ada.Text_IO;
with routeur_exceptions;         use routeur_exceptions;

package body tools is

    function Initialiser return T_Param is

        param : T_Param;

    begin
        param.taille_cache := 10;
        param.afficher_stats := True;
        param.file_table_routage := To_Unbounded_String("table.txt");
        param.file_paquets := To_Unbounded_String("paquets.txt");
        param.file_resultats := To_Unbounded_String("resultats.txt");
        param.politique := FIFO;

        return param;
    end Initialiser;

    procedure Afficher_Param(param : T_Param) is

    begin
        Put_Line("Affichage du fichier param :");
        Put_Line("   Taille du cache : " & Integer'Image(param.taille_cache));

        if param.afficher_stats then
            Put_Line("   Afficher les statistiques");
        else
            Put_Line("   Ne pas afficher les statistiques");
        end if;

        Put_Line("   Politique : " & T_Politique'Image(param.politique));
        Put_Line("   Fichier table de routage : " & To_String(param.file_table_routage));
        Put_Line("   Fichier paquets : " & To_String(param.file_paquets));
        Put_Line("   Fichier résultats : " & To_String(param.file_resultats));

    end;



end PARAMETRES;
