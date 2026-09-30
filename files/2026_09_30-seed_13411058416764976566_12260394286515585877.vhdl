-- Seed: 13411058416764976566,12260394286515585877

entity bsgzthojv is
  port (utwsajyyw : inout integer; bozqjadadx : in integer);
end bsgzthojv;

architecture g of bsgzthojv is
  
begin
  -- Single-driven assignments
  utwsajyyw <= 16#6#;
end g;

library ieee;
use ieee.std_logic_1164.all;

entity dpkogwtgs is
  port (b : linkage std_logic);
end dpkogwtgs;

architecture ef of dpkogwtgs is
  signal gy : integer;
  signal qecre : integer;
  signal tsxewunoqx : integer;
  signal xgvl : integer;
  signal kyj : integer;
  signal bwu : integer;
  signal rlttfvoo : integer;
begin
  fbj : entity work.bsgzthojv
    port map (utwsajyyw => rlttfvoo, bozqjadadx => bwu);
  mh : entity work.bsgzthojv
    port map (utwsajyyw => kyj, bozqjadadx => xgvl);
  rospsl : entity work.bsgzthojv
    port map (utwsajyyw => tsxewunoqx, bozqjadadx => qecre);
  xrkcx : entity work.bsgzthojv
    port map (utwsajyyw => gy, bozqjadadx => bwu);
end ef;

library ieee;
use ieee.std_logic_1164.all;

entity yuchno is
  port (blahoeadkd : out std_logic; nollqjyftj : inout real; twkkpz : inout bit_vector(2 downto 4));
end yuchno;

architecture hpr of yuchno is
  signal jtpdrgghio : integer;
  signal rsbmnwynqs : integer;
begin
  u : entity work.dpkogwtgs
    port map (b => blahoeadkd);
  i : entity work.bsgzthojv
    port map (utwsajyyw => rsbmnwynqs, bozqjadadx => jtpdrgghio);
  ir : entity work.bsgzthojv
    port map (utwsajyyw => jtpdrgghio, bozqjadadx => rsbmnwynqs);
  vs : entity work.dpkogwtgs
    port map (b => blahoeadkd);
  
  -- Single-driven assignments
  twkkpz <= (others => '0');
  nollqjyftj <= nollqjyftj;
  
  -- Multi-driven assignments
  blahoeadkd <= blahoeadkd;
  blahoeadkd <= 'X';
  blahoeadkd <= blahoeadkd;
  blahoeadkd <= '0';
end hpr;



-- Seed after: 4985809360245538628,12260394286515585877
