with Ada.Text_IO;            use Ada.Text_IO;
with SDA_Exceptions;         use SDA_Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

package body Table_Routage is

	procedure Free is
		new Ada.Unchecked_Deallocation (Object => T_Cellule, Name => T_LCA);


	procedure Initialiser(Sda: out T_LCA) is
	begin
		Sda:=null;
			
	end Initialiser;


	function Est_Vide (Sda : T_LCA) return Boolean is
	begin
	
		return Sda = null;	
	end;


	function Taille (Sda : in T_LCA) return Integer is
	
	begin
		
		if Sda = null then 
			return 0;
		else 
			return Taille(Sda.all.Suivant) + 1;
		end if;
	

	end Taille;


	procedure Enregistrer (Sda : in out T_LCA ; Cle : in T_Cle ; Donnee : in T_Donnee) is

	

	begin
		
		if Est_Vide(Sda) then
            Sda := new T_Cellule'(Cle, Donnee, Sda);
        elsif Sda.all.Cle=Cle then
            Sda.all.Donnee := Donnee;
        else
            Enregistrer(Sda.all.Suivant, Cle, Donnee);
        end if;

	end Enregistrer;

--Savoir si une cle est présente dans une SDA.
	function Cle_Presente (Sda : in T_LCA ; Cle : in T_Cle) return Boolean is

	
	begin	
		

		if not(Est_Vide(Sda)) then 
			if Sda.all.Cle = Cle then 
				return true;
			else 
				return Cle_Presente(Sda.all.Suivant, Cle);
			
			end if;

		else 
			return false;


		end if; 


	end Cle_Presente;


	function La_Donnee (Sda : in T_LCA ; Cle : in T_Cle) return T_Donnee is
	
	begin
		if not( Est_Vide(Sda)) then 
			if Sda.all.Cle = Cle then 
				return Sda.all.Donnee;
			
			else 
				return La_Donnee(Sda.all.Suivant, Cle);
			end if;
		else 
			raise Cle_Absente_Exception;

		end if;
	
	end La_Donnee;


	procedure Supprimer (Sda : in out T_LCA ; Cle : in T_Cle) is
	Sda0 : T_LCA;
	begin
		if Est_Vide(Sda) then
            raise Cle_Absente_Exception;
        elsif Sda.all.Cle = Cle then
            Sda0 := Sda;
            Sda:= Sda.all.Suivant;
            Free(Sda0);
        else
            Supprimer(Sda.all.Suivant, Cle);
        end if;
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


	procedure Pour_Chaque (Sda : in T_LCA) is
	
	begin
		if not( Est_Vide(Sda)) then 
			begin 
				Traiter(Sda.all.Cle, Sda.all.Donnee);
			exception 
				when others => 
					put_line(" erreur de traitement ");
			end;
			
			Pour_Chaque(Sda.all.Suivant);
			
		else 
			null;
		end if;
		
		
		
			
	end Pour_Chaque;


end Table_Routage;
