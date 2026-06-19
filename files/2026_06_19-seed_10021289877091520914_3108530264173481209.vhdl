-- Seed: 10021289877091520914,3108530264173481209

library ieee;
use ieee.std_logic_1164.all;

entity krodvcjjw is
  port (sqmrtldt : linkage real; eao : in integer_vector(2 downto 3); kqspiyk : linkage std_logic; yaqropns : linkage std_logic_vector(0 downto 1));
end krodvcjjw;

architecture pqfpgxclf of krodvcjjw is
  
begin
  
end pqfpgxclf;

entity eny is
  port (hjj : in time; upxbptfe : in bit);
end eny;

library ieee;
use ieee.std_logic_1164.all;

architecture etxbjlc of eny is
  signal qozxdr : std_logic_vector(0 downto 1);
  signal g : real;
  signal hhtms : std_logic_vector(0 downto 1);
  signal ri : std_logic;
  signal ocy : integer_vector(2 downto 3);
  signal fumqbi : real;
  signal j : std_logic;
  signal f : real;
  signal nccqjanbbx : std_logic_vector(0 downto 1);
  signal cvb : std_logic;
  signal o : integer_vector(2 downto 3);
  signal fa : real;
begin
  anwaogb : entity work.krodvcjjw
    port map (sqmrtldt => fa, eao => o, kqspiyk => cvb, yaqropns => nccqjanbbx);
  tn : entity work.krodvcjjw
    port map (sqmrtldt => f, eao => o, kqspiyk => j, yaqropns => nccqjanbbx);
  yodq : entity work.krodvcjjw
    port map (sqmrtldt => fumqbi, eao => ocy, kqspiyk => ri, yaqropns => hhtms);
  l : entity work.krodvcjjw
    port map (sqmrtldt => g, eao => ocy, kqspiyk => cvb, yaqropns => qozxdr);
  
  -- Single-driven assignments
  o <= (others => 0);
  ocy <= (others => 0);
  
  -- Multi-driven assignments
  cvb <= '0';
  nccqjanbbx <= "";
end etxbjlc;

entity fvwr is
  port (tjegjuleze : inout integer);
end fvwr;

library ieee;
use ieee.std_logic_1164.all;

architecture hqdt of fvwr is
  signal iowlxgra : std_logic_vector(0 downto 1);
  signal u : std_logic;
  signal ipo : integer_vector(2 downto 3);
  signal bif : real;
  signal aby : bit;
  signal nun : time;
begin
  ulhnrpwel : entity work.eny
    port map (hjj => nun, upxbptfe => aby);
  vqhvd : entity work.krodvcjjw
    port map (sqmrtldt => bif, eao => ipo, kqspiyk => u, yaqropns => iowlxgra);
  
  -- Single-driven assignments
  tjegjuleze <= 0_4_2;
  ipo <= (others => 0);
  aby <= '1';
  nun <= 16#FB5# us;
  
  -- Multi-driven assignments
  u <= '0';
  u <= '1';
  u <= 'U';
end hqdt;



-- Seed after: 16267044913702667223,3108530264173481209
