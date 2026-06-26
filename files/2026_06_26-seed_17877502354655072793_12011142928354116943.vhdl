-- Seed: 17877502354655072793,12011142928354116943

library ieee;
use ieee.std_logic_1164.all;

entity cwhx is
  port (qbvwl : out integer; obehmvxyj : buffer std_logic);
end cwhx;

architecture poiriaf of cwhx is
  
begin
  -- Single-driven assignments
  qbvwl <= 16#E_2_B_4#;
end poiriaf;

library ieee;
use ieee.std_logic_1164.all;

entity rgnkkykqc is
  port (fxholm : out std_logic_vector(3 downto 2); bnuv : inout time; mecifvomnk : inout integer);
end rgnkkykqc;

library ieee;
use ieee.std_logic_1164.all;

architecture xzetut of rgnkkykqc is
  signal nciicf : integer;
  signal si : integer;
  signal itugfb : std_logic;
  signal adihyxbs : integer;
begin
  okjkl : entity work.cwhx
    port map (qbvwl => adihyxbs, obehmvxyj => itugfb);
  lh : entity work.cwhx
    port map (qbvwl => si, obehmvxyj => itugfb);
  cjbcnax : entity work.cwhx
    port map (qbvwl => nciicf, obehmvxyj => itugfb);
  
  -- Single-driven assignments
  bnuv <= 2#111# fs;
  mecifvomnk <= 4302;
  
  -- Multi-driven assignments
  fxholm <= "W0";
  fxholm <= "X0";
  fxholm <= ('0', 'W');
  fxholm <= "LW";
end xzetut;



-- Seed after: 3046434101569035379,12011142928354116943
