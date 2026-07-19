-- Seed: 10542007827006164818,5511103086789671269

entity rc is
  port (iwfkohrp : buffer integer; tdio : out integer);
end rc;

architecture f of rc is
  
begin
  -- Single-driven assignments
  iwfkohrp <= 1_1;
  tdio <= 0102;
end f;

library ieee;
use ieee.std_logic_1164.all;

entity x is
  port (xhtntdjtl : inout bit_vector(3 to 4); xwbhnz : buffer std_logic_vector(0 downto 2); qny : in std_logic);
end x;

architecture ajvnqk of x is
  signal fpxy : integer;
  signal sfejaykys : integer;
  signal ulhyopwqg : integer;
  signal gslg : integer;
  signal s : integer;
  signal ry : integer;
begin
  wjvtmd : entity work.rc
    port map (iwfkohrp => ry, tdio => s);
  tdp : entity work.rc
    port map (iwfkohrp => gslg, tdio => ulhyopwqg);
  dpudnfysc : entity work.rc
    port map (iwfkohrp => sfejaykys, tdio => fpxy);
  
  -- Single-driven assignments
  xhtntdjtl <= ('1', '1');
  
  -- Multi-driven assignments
  xwbhnz <= "";
  xwbhnz <= (others => '0');
  xwbhnz <= xwbhnz;
  xwbhnz <= xwbhnz;
end ajvnqk;

entity tjgstnyh is
  port (wthl : buffer integer; wq : buffer real);
end tjgstnyh;

library ieee;
use ieee.std_logic_1164.all;

architecture l of tjgstnyh is
  signal ij : integer;
  signal ywi : integer;
  signal ercwq : integer;
  signal s : integer;
  signal qmxyjnzs : integer;
  signal uro : std_logic;
  signal onxysnzno : std_logic_vector(0 downto 2);
  signal joe : bit_vector(3 to 4);
begin
  c : entity work.x
    port map (xhtntdjtl => joe, xwbhnz => onxysnzno, qny => uro);
  fpr : entity work.rc
    port map (iwfkohrp => qmxyjnzs, tdio => s);
  qjqvkmsakt : entity work.rc
    port map (iwfkohrp => ercwq, tdio => ywi);
  upjqhmpe : entity work.rc
    port map (iwfkohrp => ij, tdio => wthl);
  
  -- Single-driven assignments
  wq <= wq;
end l;

library ieee;
use ieee.std_logic_1164.all;

entity kcxa is
  port (uskvainpth : in integer; hve : linkage integer; lzmidra : linkage time; w : in std_logic);
end kcxa;

architecture eu of kcxa is
  
begin
  
end eu;



-- Seed after: 17114231614095595882,5511103086789671269
