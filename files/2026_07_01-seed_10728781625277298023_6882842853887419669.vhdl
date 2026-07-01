-- Seed: 10728781625277298023,6882842853887419669

library ieee;
use ieee.std_logic_1164.all;

entity ihvtdtb is
  port (cckneeb : in std_logic; atqdnyduku : linkage boolean_vector(1 downto 2); v : buffer std_logic_vector(3 downto 1));
end ihvtdtb;

architecture guhbgwchhw of ihvtdtb is
  
begin
  -- Multi-driven assignments
  v <= ('L', '1', '-');
  v <= ('L', '1', 'W');
end guhbgwchhw;

entity v is
  port (cakwwqgk : buffer real; dthfprjtwp : buffer real; s : buffer integer; yyf : inout integer);
end v;

architecture zudfetzfoa of v is
  
begin
  
end zudfetzfoa;

entity vczlgsnw is
  port (zvp : in bit; e : out bit_vector(0 downto 1); zzleud : out integer; tdhtpfdh : in boolean);
end vczlgsnw;

library ieee;
use ieee.std_logic_1164.all;

architecture dweqvblvfa of vczlgsnw is
  signal wqxnexqw : boolean_vector(1 downto 2);
  signal bcmrppljip : std_logic_vector(3 downto 1);
  signal uicchiefi : boolean_vector(1 downto 2);
  signal mgcnyh : std_logic;
begin
  zc : entity work.ihvtdtb
    port map (cckneeb => mgcnyh, atqdnyduku => uicchiefi, v => bcmrppljip);
  bmdk : entity work.ihvtdtb
    port map (cckneeb => mgcnyh, atqdnyduku => wqxnexqw, v => bcmrppljip);
  
  -- Single-driven assignments
  zzleud <= 4_4_1_1;
  e <= (others => '0');
  
  -- Multi-driven assignments
  mgcnyh <= 'W';
  mgcnyh <= 'L';
end dweqvblvfa;

library ieee;
use ieee.std_logic_1164.all;

entity rtdexpdghx is
  port (zlikdir : inout boolean; yzkmusg : buffer std_logic_vector(0 to 1); jmazui : linkage boolean);
end rtdexpdghx;

architecture sjmxonw of rtdexpdghx is
  
begin
  -- Multi-driven assignments
  yzkmusg <= ('1', 'U');
  yzkmusg <= ('Z', 'L');
  yzkmusg <= ('L', 'Z');
end sjmxonw;



-- Seed after: 4470358250837071546,6882842853887419669
