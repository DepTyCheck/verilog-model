-- Seed: 4583628346323183153,5444032936641997477

entity fsdubt is
  port (v : out integer; dewsnfy : in real; wdc : inout real);
end fsdubt;

architecture ou of fsdubt is
  
begin
  -- Single-driven assignments
  wdc <= dewsnfy;
  v <= 11;
end ou;

library ieee;
use ieee.std_logic_1164.all;

entity bh is
  port (odwtghdpkl : in boolean; jvrbdrqcst : in std_logic; ny : buffer real; vxvyo : linkage real);
end bh;

architecture f of bh is
  signal wrdl : integer;
begin
  cdegvdjue : entity work.fsdubt
    port map (v => wrdl, dewsnfy => ny, wdc => ny);
end f;

library ieee;
use ieee.std_logic_1164.all;

entity uygltam is
  port (dcma : linkage std_logic; jowlji : buffer real);
end uygltam;

library ieee;
use ieee.std_logic_1164.all;

architecture v of uygltam is
  signal oyn : real;
  signal utk : real;
  signal xljhx : integer;
  signal u : real;
  signal qm : real;
  signal uuxsl : std_logic;
  signal syoyjuq : boolean;
begin
  y : entity work.bh
    port map (odwtghdpkl => syoyjuq, jvrbdrqcst => uuxsl, ny => qm, vxvyo => u);
  qd : entity work.fsdubt
    port map (v => xljhx, dewsnfy => utk, wdc => oyn);
  
  -- Multi-driven assignments
  uuxsl <= '0';
  uuxsl <= 'U';
  uuxsl <= 'Z';
  uuxsl <= uuxsl;
end v;

library ieee;
use ieee.std_logic_1164.all;

entity corgivp is
  port ( opepeoqwrb : in std_logic_vector(1 downto 2)
  ; iiqle : in std_logic_vector(0 downto 2)
  ; xoeffg : out real_vector(3 downto 0)
  ; twyzwf : buffer std_logic_vector(3 downto 0)
  );
end corgivp;

library ieee;
use ieee.std_logic_1164.all;

architecture sfiid of corgivp is
  signal ohri : real;
  signal cqbufoga : std_logic;
begin
  tbrvvswlqe : entity work.uygltam
    port map (dcma => cqbufoga, jowlji => ohri);
  
  -- Single-driven assignments
  xoeffg <= (2_4_2_3_0.1_4_0_0, 16#0.666#, 203.1_2, 41042.3104);
end sfiid;



-- Seed after: 6922977234620984970,5444032936641997477
