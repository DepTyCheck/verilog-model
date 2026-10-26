-- Seed: 261009065649588883,6562834012285671529

library ieee;
use ieee.std_logic_1164.all;

entity sq is
  port (nugt : in character; lmcltfmit : inout std_logic; lyawyyycdx : out std_logic_vector(0 downto 4));
end sq;

architecture ypxjmnbli of sq is
  
begin
  -- Multi-driven assignments
  lmcltfmit <= '0';
  lmcltfmit <= '1';
end ypxjmnbli;

entity psjv is
  port (ccqrpgmkrh : inout real_vector(3 downto 0));
end psjv;

architecture nphs of psjv is
  
begin
  -- Single-driven assignments
  ccqrpgmkrh <= (16#16.B90EB#, 1314.010, 2#10010.0_1_1_1#, 16#6_7.4_F_4#);
end nphs;

library ieee;
use ieee.std_logic_1164.all;

entity ooscefrhq is
  port (cfoamw : in integer; iphrloy : inout std_logic_vector(3 downto 4));
end ooscefrhq;

library ieee;
use ieee.std_logic_1164.all;

architecture pekrc of ooscefrhq is
  signal lfenq : std_logic_vector(0 downto 4);
  signal w : std_logic;
  signal b : character;
  signal plkhui : real_vector(3 downto 0);
begin
  aluyoxlhbu : entity work.psjv
    port map (ccqrpgmkrh => plkhui);
  lfxlzhaalq : entity work.sq
    port map (nugt => b, lmcltfmit => w, lyawyyycdx => iphrloy);
  yfkdlpemko : entity work.sq
    port map (nugt => b, lmcltfmit => w, lyawyyycdx => lfenq);
  
  -- Single-driven assignments
  b <= b;
  
  -- Multi-driven assignments
  iphrloy <= iphrloy;
  w <= w;
  lfenq <= "";
end pekrc;



-- Seed after: 6868099374059324095,6562834012285671529
