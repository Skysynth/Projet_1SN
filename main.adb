with Ada.Text_IO;           use Ada.Text_IO;
-- with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with routeur_exceptions;    use routeur_exceptions;
with PARAMETRES;            use PARAMETRES;

procedure Main is

    iterateur : Integer;
    is_opt : Boolean := True;

    opt : Unbounded_String;
    arg : Unbounded_String;

    param : T_Param;

    -- Afficher l'usage.
    procedure Afficher_Usage is
    begin
        New_Line;
        Put_Line ("Usage : " & Command_Name & " [parametres]");
        New_Line;
        Put_Line ("   Voici la liste des paramètres possibles :");
        Put_Line ("   -c <taille> : Défini la taille du cache. Valeur par défaut = 10");
        Put_Line ("   -p FIFO|LRU|LFU : Défini la politique utilisée pour le cache. Valeur par défaut = FIFO");
        Put_Line ("   -s : Afficher les statistiques. Activée par défaut");
        Put_Line ("   -S : Ne pas afficher les statistiques. Désactivée par défaut");
        Put_Line ("   -t <fichier> : Définit le nom du fichier contenant les routes de la table de routage. Valeur par défaut = table.txt");
        Put_Line ("   -p <fichier> : Définit le nom du fichier contenant les paquets à router. Valeur par défaut = paquets.txt");
        Put_Line ("   -r <fichier> : Définit le nom du fichier contenant les résultats. Valeur par défaut = resultats.txt");
        New_Line;
    end Afficher_Usage;


begin


    param := Initialiser;
    iterateur := 1;

    begin

        while iterateur <= Argument_Count loop

            if is_opt then

                if Argument(iterateur)(1) = '-' and Argument(iterateur)'length = 2 then

                    case Argument(iterateur)(2) is

                        when 'c' =>
                            param.taille_cache := Integer'value(Argument(iterateur+1));
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


            end if;

        end loop;

        Afficher_Param(param);

    exception
        when others => Afficher_Usage;

    end;




end Main;
