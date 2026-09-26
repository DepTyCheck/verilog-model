-- Seed: 2292716090893567912,10875537289884587119

library ieee;
use ieee.std_logic_1164.all;

entity yxtkuj is
  port (fsn : buffer boolean_vector(3 downto 2); weqqx : linkage std_logic);
end yxtkuj;

architecture jliylvpt of yxtkuj is
  
begin
  -- Single-driven assignments
  fsn <= (TRUE, TRUE);
end jliylvpt;

library ieee;
use ieee.std_logic_1164.all;

entity zahxxh is
  port (ugjmmr : in severity_level; wrudz : in real_vector(1 to 3); kt : inout time; moavxgyhr : buffer std_logic);
end zahxxh;

library ieee;
use ieee.std_logic_1164.all;

architecture ru of zahxxh is
  signal hlng : boolean_vector(3 downto 2);
  signal clbrtphw : std_logic;
  signal kl : boolean_vector(3 downto 2);
  signal xme : boolean_vector(3 downto 2);
begin
  oci : entity work.yxtkuj
    port map (fsn => xme, weqqx => moavxgyhr);
  dltctzbt : entity work.yxtkuj
    port map (fsn => kl, weqqx => clbrtphw);
  m : entity work.yxtkuj
    port map (fsn => hlng, weqqx => moavxgyhr);
  
  -- Single-driven assignments
  kt <= kt;
  
  -- Multi-driven assignments
  clbrtphw <= moavxgyhr;
  moavxgyhr <= moavxgyhr;
end ru;

entity hbxlj is
  port (urus : linkage integer; dja : buffer integer; e : buffer real);
end hbxlj;

library ieee;
use ieee.std_logic_1164.all;

architecture cbvntoeiy of hbxlj is
  signal eqricvria : std_logic;
  signal bo : boolean_vector(3 downto 2);
  signal kpzbusupkm : time;
  signal nmvunuzdwj : real_vector(1 to 3);
  signal aehilnhlop : severity_level;
  signal jsin : std_logic;
  signal ofr : boolean_vector(3 downto 2);
  signal lr : std_logic;
  signal gozjkgllb : boolean_vector(3 downto 2);
begin
  fwdey : entity work.yxtkuj
    port map (fsn => gozjkgllb, weqqx => lr);
  d : entity work.yxtkuj
    port map (fsn => ofr, weqqx => jsin);
  pdtgw : entity work.zahxxh
    port map (ugjmmr => aehilnhlop, wrudz => nmvunuzdwj, kt => kpzbusupkm, moavxgyhr => lr);
  gvpcyqmqt : entity work.yxtkuj
    port map (fsn => bo, weqqx => eqricvria);
  
  -- Multi-driven assignments
  lr <= lr;
  lr <= 'Z';
end cbvntoeiy;



-- Seed after: 9516442491232293131,10875537289884587119
