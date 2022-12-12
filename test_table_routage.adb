with Ada.Text_IO; use Ada.Text_IO;
-- with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with routeur_exceptions; use routeur_exceptions;
with tools; use tools;
with Table_Routage; use Table_Routage;

procedure Test_Table_Routage is

    iterateur : Integer;
    table : T_Table_Routage;
    param : T_Param;

    -- Afficher l'usage
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


    function Convert_Unbounded_String_To_T_Adresse_IP(ligne : Unbounded_String) return T_Adresse_IP is
        Adresse_Converti : T_Adresse_IP := 0;
        mot : Unbounded_String;
        N : Integer;
        j : Integer := 1;
    begin
            
        for i in 0..3 loop 
            mot := Null_Unbounded_String;
                
            N := length(ligne);
                
            while j <= N and then Element(ligne, j) /= '.' loop
                
                mot := mot & Element(ligne, j);
                j := j+1;
                    
            end loop;
                
            -- Enregistrer la destination et le masque une fois converti dans le routeur et enregistrer l'interface dans le routeur
              
            Adresse_Converti := Adresse_Converti + T_Adresse_IP'Value(To_String(mot)) * (2 ** (24-8*i));
            
            j := j + 1;
            
        end loop;
            
        return Adresse_Converti;
            
    end Convert_Unbounded_String_To_T_Adresse_IP;


    procedure Tester_Initialiser() is 
        Table_Routage : T_Table_Routage;
        begin 
        pragma Assert (Est_Vide(Table_Routage));
        pragma Assert (Taille(Table_Routage) = 0); 
        Initialiser(param, Table_Routage); 
        pragma Assert (not(Est_Vide(Table_Routage)));




        end Tester_Initialiser; 



begin
    Tester_Initialiser();

    param := Initialiser;
    iterateur := 1;

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

        Afficher_Param(param);

        Put_Line("Initialisation de la table de routage");

        Initialiser(param         =>  param,
                    table_routage => table);

        Afficher(Table_Routage => table);

        --    exception

        --      when others => Afficher_Usage;
    end;




end Test_Table_Routage;
