with Ada.Unchecked_Deallocation;
with Routeur_Exceptions; use Routeur_Exceptions;

-- R1 : Concevoir et initialiser un routeur
package body Table_Routage is
    
    procedure Free is
        new Ada.Unchecked_Deallocation (Object => T_Cellule, Name => T_Table_Routage);
    
    -- R2 : Initialiser un routeur 
    procedure Initialiser(param : in T_Param; Table_Routage: out T_Table_Routage) is
        File : File_Type;
        Donnee : array(1..3) of Unbounded_String;
    
        -- Concevoir le tableau de taille 3
        iterateur : Integer;
        ligne : Unbounded_String;
        
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
            table_routage_temp.all.Adresse := Unbounded_String_To_Adresse_IP(Donnee(1));
            table_routage_temp.all.Masque := Unbounded_String_To_Adresse_IP(Donnee(2));
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

    end Taille;

    
    procedure Enregistrer (Table_Routage : in out T_Table_Routage ; Adresse : in T_Adresse_IP ; Masque : in T_Adresse_IP; Sortie : in Unbounded_String) is

    begin
        if Est_Vide(Table_Routage) then
            Table_Routage := new T_Cellule'(Adresse, Masque,Sortie, Table_Routage);
        else
            Enregistrer(Table_Routage.all.Suivant, Adresse, Masque, Sortie);
        end if;

    end Enregistrer;

    procedure Supprimer (Table_Routage : in out T_Table_Routage ; adresse : in T_Adresse_IP) is
        Table_A_Supp : T_Table_Routage;

    begin
        if Est_Vide(Table_Routage) then
            raise Adresse_Absente_Exception;
        elsif Table_Routage.all.Adresse = adresse then
            Table_A_Supp := Table_Routage;
            Table_Routage:= Table_Routage.all.Suivant;
            Free(Table_A_Supp);
        else
            Supprimer(Table_Routage.all.Suivant, adresse);
        end if;
    end Supprimer;



    procedure Vider (Table_Routage : in out T_Table_Routage) is
    begin
        if not(Est_Vide(Table_Routage)) then
            Vider(Table_Routage.all.Suivant); -- .all permet d'acceder au contenu de l'adresse que pointe le pointeur  
            Free (Table_Routage);
        else
            Null;
        end if;
		
    end Vider;
    
    procedure Afficher(Table_Routage : in T_Table_Routage; file : File_Type) is
        
        table_temp : T_Table_Routage := Table_Routage;
    
    begin
        
        while table_temp /= null loop
            
            Put_Line(file, Adresse_IP_To_String(table_temp.all.Adresse) & 
                         " " & Adresse_IP_To_String(table_temp.all.Masque) &
                         " " & To_String(table_temp.all.Sortie)); 
            
            table_temp := table_temp.all.Suivant;
        end loop;
        
    end;

    
    function Adresse_Correspond(adresse1 : T_Adresse_IP; adresse2 : T_Adresse_IP; Masque: T_Adresse_IP) return Boolean is
    begin    
        return (adresse1 AND Masque) = (adresse2 AND Masque);
    end Adresse_Correspond;
    
    -- retourne l'interface associée à cette adresse IP
    function Get_Interface(Adresse_IP: in T_Adresse_IP; Table_Routage: in T_Table_Routage) return Unbounded_String is
        
        table_temp : T_Table_Routage;
        
        interface_max : Unbounded_String;
        taille_masque_max : Integer := -1;
        taille_masque : Integer;
        
    begin
        
        table_temp := Table_Routage;
        
        -- Parcourir les adresses ip
        while not Est_Vide(table_temp) loop
            
            if Adresse_Correspond(Adresse_IP, table_temp.all.Adresse, table_temp.all.Masque) then
                
                taille_masque := Get_taille_binaire(table_temp.all.Masque);
                
                if taille_masque > taille_masque_max then
                    taille_masque_max := taille_masque;
                    interface_max := table_temp.all.Sortie;
                else
                    null;
                end if;
            else
                null;
            end if;
            
            table_temp := table_temp.all.Suivant;
                
        end loop;
        
        return interface_max;

    end Get_Interface;


    
    function Is_Command_And_Then_Execute(ligne : in String; tr : in T_Table_Routage; file_output : File_Type; num_ligne : Integer) return Boolean is
        type Commandes is (Table, Cache, Stat, Fin); 
    begin
        
        case Commandes'Value(ligne) is
        when Table =>
            Put_Line(file_output, "Table (" & Integer'Image(num_ligne) & " )");
            Afficher(Table_Routage => tr,
                     file          => file_output);
        when Cache =>
            Put_Line(file_output, "Cache (" & Integer'Image(num_ligne) & " )");
            Put_Line(file_output, "prochainement...");
        when Stat =>
            Put_Line(file_output, "Stat (" & Integer'Image(num_ligne) & " )");
            Put_Line(file_output, "prochainement...");
        when Fin =>
            Put_Line(file_output, "Fin (" & Integer'Image(num_ligne) & " )");
            raise COMMAND_FIN_CALLED;
        end case;
        
        return True;
        
    exception
        when Constraint_Error => return False;
    end Is_Command_And_Then_Execute;
    
    function Adresse_Presente (Table_Routage : in T_Table_Routage ; adresse : in T_Adresse_IP) return Boolean is
    begin	
        if not(Est_Vide(Table_Routage)) then 
            if Table_Routage.all.Adresse = adresse then 
                return true;
            else 
                return Adresse_Presente(Table_Routage.all.Suivant, adresse);
            end if;
        else 
            return false;
        end if; 
    end Adresse_Presente;
    
end Table_Routage;
