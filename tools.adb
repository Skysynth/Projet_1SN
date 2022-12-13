with Ada.Text_IO; use Ada.Text_IO;
with routeur_exceptions; use routeur_exceptions;
with Ada.Command_Line; use Ada.Command_Line;

package body tools is

    function Initialiser_Param return T_Param is

        param : T_Param;

    begin
        param.taille_cache := 10;
        param.afficher_stats := True;
        param.file_table_routage := To_Unbounded_String("table.txt");
        param.file_paquets := To_Unbounded_String("paquets.txt");
        param.file_resultats := To_Unbounded_String("resultats.txt");
        param.politique := FIFO;

        return param;
    end Initialiser_Param;

    procedure Remplir_Param(param : out T_Param) is

        iterateur : Integer := 1;

    begin
        while iterateur <= Argument_Count loop

            if Argument(iterateur)(1) = '-' and Argument(iterateur)'Length = 2 then

                case Argument(iterateur)(2) is

                    when 'c' =>
                        param.taille_cache := Integer'Value(Argument(iterateur + 1));
                        iterateur := iterateur + 1;

                    when 'P' =>
                        param.politique := T_Politique'Value(Argument(iterateur + 1));
                        iterateur := iterateur + 1;

                    when 's' => param.afficher_stats := True;

                    when 'S' => param.afficher_stats := False;

                    when 't' =>
                        param.file_table_routage := To_Unbounded_String(Argument(iterateur + 1));
                        iterateur := iterateur + 1;

                    when 'p' =>
                        param.file_paquets := To_Unbounded_String(Argument(iterateur + 1));
                        iterateur := iterateur + 1;

                    when 'r' =>
                        param.file_resultats := To_Unbounded_String(Argument(iterateur + 1));
                        iterateur := iterateur + 1;

                    when others => raise Option_non_valide_exception;

                end case;

                iterateur := iterateur + 1;

            else
                raise Option_non_valide_exception;
            end if;

        end loop;
    end Remplir_Param;



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

end tools;
