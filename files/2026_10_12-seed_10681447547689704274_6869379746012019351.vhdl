-- Seed: 10681447547689704274,6869379746012019351

library ieee;
use ieee.std_logic_1164.all;

entity e is
  port (wfohzidfec : in std_logic_vector(4 to 0); whrkwltwg : inout bit_vector(0 to 4); pk : inout std_logic_vector(2 to 0));
end e;

architecture ovhls of e is
  
begin
  -- Multi-driven assignments
  pk <= pk;
  pk <= pk;
  pk <= wfohzidfec;
end ovhls;

library ieee;
use ieee.std_logic_1164.all;

entity n is
  port (yl : out time; uridsb : out std_logic_vector(1 to 0); ydgamyngkx : inout std_logic_vector(4 to 3));
end n;

library ieee;
use ieee.std_logic_1164.all;

architecture hfmka of n is
  signal rjzzugvx : std_logic_vector(2 to 0);
  signal bnodsmc : bit_vector(0 to 4);
  signal cmttfpifg : std_logic_vector(4 to 0);
  signal et : bit_vector(0 to 4);
  signal loafr : bit_vector(0 to 4);
  signal blntk : std_logic_vector(4 to 0);
  signal qjmzebvtyf : std_logic_vector(2 to 0);
  signal lertjb : bit_vector(0 to 4);
  signal whzgosc : std_logic_vector(4 to 0);
begin
  dme : entity work.e
    port map (wfohzidfec => whzgosc, whrkwltwg => lertjb, pk => qjmzebvtyf);
  btyvuez : entity work.e
    port map (wfohzidfec => blntk, whrkwltwg => loafr, pk => ydgamyngkx);
  cdewq : entity work.e
    port map (wfohzidfec => ydgamyngkx, whrkwltwg => et, pk => ydgamyngkx);
  mahekomox : entity work.e
    port map (wfohzidfec => cmttfpifg, whrkwltwg => bnodsmc, pk => rjzzugvx);
  
  -- Multi-driven assignments
  blntk <= uridsb;
  blntk <= ydgamyngkx;
  ydgamyngkx <= uridsb;
end hfmka;

library ieee;
use ieee.std_logic_1164.all;

entity kheukfdq is
  port ( ejdvljqgia : inout boolean_vector(3 to 4)
  ; oreujfnva : inout std_logic_vector(0 downto 0)
  ; wh : out bit
  ; ufexjxk : linkage std_logic_vector(0 to 0)
  );
end kheukfdq;

library ieee;
use ieee.std_logic_1164.all;

architecture ahqsdz of kheukfdq is
  signal qyne : bit_vector(0 to 4);
  signal bczqg : std_logic_vector(2 to 0);
begin
  rzjyvzt : entity work.e
    port map (wfohzidfec => bczqg, whrkwltwg => qyne, pk => bczqg);
  
  -- Single-driven assignments
  wh <= wh;
  ejdvljqgia <= (FALSE, TRUE);
  
  -- Multi-driven assignments
  oreujfnva <= (others => 'X');
  oreujfnva <= oreujfnva;
  oreujfnva <= "L";
  oreujfnva <= "-";
end ahqsdz;



-- Seed after: 3048586917937250407,6869379746012019351
