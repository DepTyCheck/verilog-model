-- Seed: 14524676232359551059,15306146742274979323

entity hlnr is
  port (br : buffer time_vector(4 downto 4));
end hlnr;

architecture rcwdavmrpk of hlnr is
  
begin
  -- Single-driven assignments
  br <= (others => 0 sec);
end rcwdavmrpk;

library ieee;
use ieee.std_logic_1164.all;

entity kgffdm is
  port (mfsqn : buffer string(4 downto 2); oaudy : out std_logic_vector(3 downto 4); phkfkocld : in std_logic);
end kgffdm;

architecture zczyoc of kgffdm is
  signal oirwwmaddq : time_vector(4 downto 4);
  signal qf : time_vector(4 downto 4);
  signal iieaac : time_vector(4 downto 4);
  signal vzz : time_vector(4 downto 4);
begin
  trkb : entity work.hlnr
    port map (br => vzz);
  bx : entity work.hlnr
    port map (br => iieaac);
  ptied : entity work.hlnr
    port map (br => qf);
  wj : entity work.hlnr
    port map (br => oirwwmaddq);
  
  -- Single-driven assignments
  mfsqn <= mfsqn;
  
  -- Multi-driven assignments
  oaudy <= "";
end zczyoc;



-- Seed after: 1886543278117646043,15306146742274979323
