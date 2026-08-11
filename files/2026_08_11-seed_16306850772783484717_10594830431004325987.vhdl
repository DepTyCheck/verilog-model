-- Seed: 16306850772783484717,10594830431004325987

entity ncisw is
  port (ifbfv : linkage time);
end ncisw;

architecture c of ncisw is
  
begin
  
end c;

library ieee;
use ieee.std_logic_1164.all;

entity qojhwxt is
  port (gqzg : in std_logic; a : inout std_logic_vector(0 downto 2); qswrb : out time);
end qojhwxt;

architecture ihqkngta of qojhwxt is
  signal qyegkku : time;
  signal zlh : time;
  signal xj : time;
begin
  ndy : entity work.ncisw
    port map (ifbfv => xj);
  smd : entity work.ncisw
    port map (ifbfv => zlh);
  fceuzpn : entity work.ncisw
    port map (ifbfv => qswrb);
  u : entity work.ncisw
    port map (ifbfv => qyegkku);
  
  -- Multi-driven assignments
  a <= a;
  a <= "";
  a <= (others => '0');
end ihqkngta;

entity hxtjpppcqy is
  port (ext : linkage real; tfleaed : in integer; hkxjwtu : in integer; ucyqdvcgyq : linkage boolean_vector(4 downto 2));
end hxtjpppcqy;

library ieee;
use ieee.std_logic_1164.all;

architecture coec of hxtjpppcqy is
  signal o : time;
  signal afyfe : time;
  signal dyoxhzfcec : std_logic_vector(0 downto 2);
  signal iyyrltj : std_logic;
  signal nwy : time;
  signal eadygmnq : std_logic_vector(0 downto 2);
  signal oi : std_logic;
begin
  lqqbawdr : entity work.qojhwxt
    port map (gqzg => oi, a => eadygmnq, qswrb => nwy);
  vzfrprx : entity work.qojhwxt
    port map (gqzg => iyyrltj, a => dyoxhzfcec, qswrb => afyfe);
  c : entity work.ncisw
    port map (ifbfv => o);
  
  -- Multi-driven assignments
  eadygmnq <= eadygmnq;
  dyoxhzfcec <= "";
end coec;



-- Seed after: 13281149639239645,10594830431004325987
