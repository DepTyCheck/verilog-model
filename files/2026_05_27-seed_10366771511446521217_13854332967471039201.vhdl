-- Seed: 10366771511446521217,13854332967471039201

library ieee;
use ieee.std_logic_1164.all;

entity s is
  port ( hhnvawdfoj : linkage severity_level
  ; cszh : inout std_logic_vector(4 downto 1)
  ; rxhshmemj : linkage time
  ; tslznkqji : linkage boolean_vector(3 downto 1)
  );
end s;



architecture od of s is
  
begin
  
end od;



entity mdtit is
  port (ji : inout integer);
end mdtit;

library ieee;
use ieee.std_logic_1164.all;

architecture fd of mdtit is
  signal ssanvcie : boolean_vector(3 downto 1);
  signal mqcyzysslz : boolean_vector(3 downto 1);
  signal qtowanj : time;
  signal gwzsohkxeb : std_logic_vector(4 downto 1);
  signal ghanchu : severity_level;
begin
  q : entity work.s
    port map (hhnvawdfoj => ghanchu, cszh => gwzsohkxeb, rxhshmemj => qtowanj, tslznkqji => mqcyzysslz);
  opra : entity work.s
    port map (hhnvawdfoj => ghanchu, cszh => gwzsohkxeb, rxhshmemj => qtowanj, tslznkqji => ssanvcie);
end fd;



entity vrnzqfy is
  port (oklq : buffer boolean; wxnju : in integer; oqfw : out time);
end vrnzqfy;

library ieee;
use ieee.std_logic_1164.all;

architecture mppdan of vrnzqfy is
  signal il : time;
  signal pdwhqae : severity_level;
  signal ln : time;
  signal ydrxcfauv : severity_level;
  signal njtbggavms : boolean_vector(3 downto 1);
  signal zqrngar : std_logic_vector(4 downto 1);
  signal bnrrtxt : boolean_vector(3 downto 1);
  signal jxhmejbjnq : time;
  signal oipn : std_logic_vector(4 downto 1);
  signal qwp : severity_level;
begin
  goli : entity work.s
    port map (hhnvawdfoj => qwp, cszh => oipn, rxhshmemj => jxhmejbjnq, tslznkqji => bnrrtxt);
  knoycxjyny : entity work.s
    port map (hhnvawdfoj => qwp, cszh => zqrngar, rxhshmemj => jxhmejbjnq, tslznkqji => njtbggavms);
  jshl : entity work.s
    port map (hhnvawdfoj => ydrxcfauv, cszh => oipn, rxhshmemj => ln, tslznkqji => njtbggavms);
  o : entity work.s
    port map (hhnvawdfoj => pdwhqae, cszh => oipn, rxhshmemj => il, tslznkqji => njtbggavms);
end mppdan;



entity tee is
  port (x : out integer);
end tee;

library ieee;
use ieee.std_logic_1164.all;

architecture quuzmhoano of tee is
  signal ksfmsnlgn : time;
  signal hlk : std_logic_vector(4 downto 1);
  signal mkng : severity_level;
  signal wbk : boolean_vector(3 downto 1);
  signal xvsovly : std_logic_vector(4 downto 1);
  signal vymbfow : severity_level;
  signal cuqjcuvy : time;
  signal ebrdix : boolean;
begin
  yg : entity work.vrnzqfy
    port map (oklq => ebrdix, wxnju => x, oqfw => cuqjcuvy);
  cjdrdmg : entity work.s
    port map (hhnvawdfoj => vymbfow, cszh => xvsovly, rxhshmemj => cuqjcuvy, tslznkqji => wbk);
  a : entity work.s
    port map (hhnvawdfoj => mkng, cszh => hlk, rxhshmemj => ksfmsnlgn, tslznkqji => wbk);
end quuzmhoano;



-- Seed after: 17237055176987546970,13854332967471039201
