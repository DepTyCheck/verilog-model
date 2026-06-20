-- Seed: 8043022014382662278,17924494779688682807

entity iydjywoqki is
  port (mdyvi : buffer bit; wfiike : buffer integer; l : linkage boolean_vector(2 downto 2));
end iydjywoqki;

architecture pd of iydjywoqki is
  
begin
  -- Single-driven assignments
  wfiike <= 1441;
  mdyvi <= '1';
end pd;

library ieee;
use ieee.std_logic_1164.all;

entity cfytti is
  port (nwitwpqt : in std_logic_vector(3 to 1));
end cfytti;

architecture y of cfytti is
  
begin
  
end y;

library ieee;
use ieee.std_logic_1164.all;

entity dmgpltu is
  port (ancuyyn : inout integer; ccz : buffer bit; mpbi : linkage std_logic; moanoi : buffer boolean);
end dmgpltu;

library ieee;
use ieee.std_logic_1164.all;

architecture mp of dmgpltu is
  signal vnspjdbevp : std_logic_vector(3 to 1);
  signal kbdeknn : boolean_vector(2 downto 2);
  signal oeiaeck : integer;
  signal veugnhyvhw : bit;
begin
  yohkgmu : entity work.iydjywoqki
    port map (mdyvi => veugnhyvhw, wfiike => oeiaeck, l => kbdeknn);
  vgpudewbl : entity work.cfytti
    port map (nwitwpqt => vnspjdbevp);
  
  -- Multi-driven assignments
  vnspjdbevp <= "";
  vnspjdbevp <= (others => '0');
  vnspjdbevp <= "";
  vnspjdbevp <= "";
end mp;



-- Seed after: 9422494332212020180,17924494779688682807
