-- Seed: 605339005157825876,7142793346053417159

library ieee;
use ieee.std_logic_1164.all;

entity vatposmus is
  port (cpdcgc : in std_logic_vector(4 to 2); bzswarlnm : out bit_vector(4 downto 2); vrjl : inout real);
end vatposmus;



architecture y of vatposmus is
  
begin
  
end y;



entity uxgng is
  port (cstl : inout time; qejztdw : in bit; plxlnqtqg : buffer bit_vector(2 to 4); kffd : out real);
end uxgng;

library ieee;
use ieee.std_logic_1164.all;

architecture hbexzcp of uxgng is
  signal urxtykamh : real;
  signal szwqk : bit_vector(4 downto 2);
  signal sykmipvhy : std_logic_vector(4 to 2);
  signal vwwyneg : real;
  signal qbxtguda : bit_vector(4 downto 2);
  signal jdruxxedx : std_logic_vector(4 to 2);
begin
  rkuy : entity work.vatposmus
    port map (cpdcgc => jdruxxedx, bzswarlnm => qbxtguda, vrjl => vwwyneg);
  iiqtpkchdx : entity work.vatposmus
    port map (cpdcgc => sykmipvhy, bzswarlnm => szwqk, vrjl => urxtykamh);
  pngw : entity work.vatposmus
    port map (cpdcgc => sykmipvhy, bzswarlnm => plxlnqtqg, vrjl => kffd);
end hbexzcp;



-- Seed after: 18052621205614094886,7142793346053417159
