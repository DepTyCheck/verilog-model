-- Seed: 8824428040869495265,6000118208082478503

library ieee;
use ieee.std_logic_1164.all;

entity dsmz is
  port (yoyctiape : in std_logic; qdgw : out real_vector(4 to 3); fwddpl : buffer string(5 downto 3); axx : linkage std_logic_vector(1 to 1));
end dsmz;

architecture vbjbwffxk of dsmz is
  
begin
  -- Single-driven assignments
  fwddpl <= ('k', 'w', 'b');
  qdgw <= qdgw;
end vbjbwffxk;

library ieee;
use ieee.std_logic_1164.all;

entity ilnugl is
  port (wekorm : buffer bit; fgbymr : in bit; xjyzbxsv : in std_logic);
end ilnugl;

library ieee;
use ieee.std_logic_1164.all;

architecture n of ilnugl is
  signal ywoaudwo : string(5 downto 3);
  signal t : real_vector(4 to 3);
  signal g : string(5 downto 3);
  signal ipd : real_vector(4 to 3);
  signal lewbvl : std_logic_vector(1 to 1);
  signal itv : string(5 downto 3);
  signal eyorw : real_vector(4 to 3);
  signal fpx : std_logic;
begin
  ubilws : entity work.dsmz
    port map (yoyctiape => fpx, qdgw => eyorw, fwddpl => itv, axx => lewbvl);
  htvoxtqra : entity work.dsmz
    port map (yoyctiape => xjyzbxsv, qdgw => ipd, fwddpl => g, axx => lewbvl);
  polmwgkfjy : entity work.dsmz
    port map (yoyctiape => xjyzbxsv, qdgw => t, fwddpl => ywoaudwo, axx => lewbvl);
  
  -- Single-driven assignments
  wekorm <= '1';
  
  -- Multi-driven assignments
  fpx <= '-';
  fpx <= '1';
end n;



-- Seed after: 17949334894864668706,6000118208082478503
