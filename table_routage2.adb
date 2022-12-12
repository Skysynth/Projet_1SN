with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with tools; use tools;

-- R1 : Concevoir et initialiser un routeur
package body Table_Routage is

    procedure Free is
        new Ada.Unchecked_Deallocation (Object => T_Cellule, Name => T_Table_Routage);

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

    end;

    procedure Afficher(Table_Routage : in T_Table_Routage) is

        table_temp : T_Table_Routage := Table_Routage;
    begin

        while table_temp /= null loop

            Put_Line("Adresse : " & T_Adresse_IP'Image(table_temp.all.Adresse)
                     & "   Masque : " & T_Adresse_IP'Image(table_temp.all.Masque)
                     & "   Interface : " & To_String(table_temp.all.Sortie));

            table_temp := table_temp.all.Suivant;
        end loop;

    end;

    -- R2 : Initialiser un routeur
    procedure Initialiser(param : in T_Param; Table_Routage: out T_Table_Routage) is
        File : File_Type;
        Donnee : array(1..3) of Unbounded_String;

        -- Concevoir le tableau de taille 3
        iterateur : Integer;
        ligne : Unbounded_String;

        mot : Unbounded_String;
        N : Integer;

        table_routage_temp : T_Table_Routage;

    begin
        Open (File => File,
              Mode => In_File,
              Name => To_String(param.file_table_routage));

        -- Concevoir une nouvelle cellule dans le routeur pour stocker les donnees de la prochaine ligne
        Table_Routage := new T_Cellule;
        table_routage_temp := Table_Routage;

        While not End_Of_File (File) Loop

            ligne := To_Unbounded_String(Get_Line(File));
            N := length(ligne);
            iterateur := 1;

            -- Concevoir un tableau de taille 3 qui va stocker des chaines de caracteres

            for j in 1..3 loop

                Donnee(j) := Null_Unbounded_String;


                while iterateur <= N and then Element(ligne, iterateur) /= ' ' loop

                    Donnee(j) := Donnee(j) & Element(ligne, iterateur);

                    iterateur := iterateur + 1;

                end loop;
                iterateur := iterateur + 1;

            end loop;

            -- Convertir l'adresse IP de la destination et du masque
            table_routage_temp.all.Adresse := Convert_Unbounded_String_To_T_Adresse_IP(Donnee(1));
            table_routage_temp.all.Masque := Convert_Unbounded_String_To_T_Adresse_IP(Donnee(2));
            table_routage_temp.all.Sortie := Donnee(3);

            if not End_Of_File (File) then
                table_routage_temp.Suivant := new T_Cellule;
                table_routage_temp := table_routage_temp.Suivant;
            else
                null;
            end if;

        end loop;

        Close (File);

    end Initialiser;


    function Get_taille_binaire(adresse : T_Adresse_IP) return Integer is

        compteur : Integer := 0;

        exposant : Integer := 31;

        resultat := 1;

    begin

        while resultat /= 0 loop

            resultat := (2 ** exposant) and adresse;

            compteur := compteur + 1;
            exposant := exposant - 1;

        end loop;

        return compteur;

    end Get_taille_binaire;


    -- retourne l'interface associée à cette adresse IP
    function get_Interface(Adresse_IP: in T_Adresse_IP; Table_Routage: in T_Table_Routage) return Unbounded_String is

        table_temp : T_Table_Routage;

        interface_max : Unbounded_String;
        masque_max : T_Adresse_IP := 0;

    begin

        table_temp := Table_Routage;

        -- Parcourir tous les potentiels candidats
        while not Est_Vide(table_temp) loop

            -- Adresse correspond ?
            if adresse_Correspond(Adresse_IP, table_temp.all.Adresse, table_temp.all.Masque) then

                -- Masque plus grand que l'ancien ?
                if get_taille_binaire(table_temp.all.Masque) > masque_max then

                end if;



            end if;

            table_temp := table_temp.all.Suivant;

        end loop;




        -- Choisir l'interface correspondant avec le plus grand masque


    end get_Interface;




    function Est_Vide (Table_Routage : in T_Table_Routage) return Boolean is
    begin

        return Table_Routage = null;
    end;

    function Taille (Table_Routage : in T_Table_Routage) return Integer is

    begin

        if Table_Routage = null then
            return 0;
        else
            return Taille(Table_Routage.all.Suivant) + 1;
        end if;

	end Enregistrer;



    procedure Enregistrer (Table_Routage : in out T_Table_Routage ; adresse : T_Adresse_IP) is

    begin

        null;
    end Enregistrer;


    function La_Donnee (Table_Routage : in T_Table_Routage ; adresse : T_Adresse_IP) return T_Adresse_IP is

    begin
        return 0;

    end La_Donnee;


    procedure Supprimer (Table_Routage : in out T_Table_Routage ; adresse : T_Adresse_IP) is
    begin
        null;
    end Supprimer;


	procedure Vider (Sda : in out T_LCA) is
	begin
		if not(Est_Vide(Sda)) then
			Vider(Sda.all.Suivant); -- .all permet d'acceder au contenu de l'adresse que pointe le pointeur
			Free (Sda);
		else
			Null;
		end if;

	end Vider;

end Table_Routage;
