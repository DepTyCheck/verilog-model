-- Seed: 10827443659586388244,3924983747739634027

library ieee;
use ieee.std_logic_1164.all;

entity rengetc is
  port (ab : buffer std_logic);
end rengetc;

architecture mxymyierzo of rengetc is
  
begin
  -- Multi-driven assignments
  ab <= 'L';
  ab <= 'U';
end mxymyierzo;

library ieee;
use ieee.std_logic_1164.all;

entity ndicwkj is
  port (cjhxeyi : inout time; y : in character; ylwnmj : inout std_logic);
end ndicwkj;

library ieee;
use ieee.std_logic_1164.all;

architecture pofdbrgs of ndicwkj is
  signal efaifaupho : std_logic;
  signal rizbrdgrnm : std_logic;
begin
  nnyv : entity work.rengetc
    port map (ab => rizbrdgrnm);
  ftwjhyqj : entity work.rengetc
    port map (ab => efaifaupho);
  
  -- Single-driven assignments
  cjhxeyi <= 0 min;
  
  -- Multi-driven assignments
  ylwnmj <= 'L';
end pofdbrgs;

entity suo is
  port (mzfoubs : linkage integer; ebjtble : out integer);
end suo;

library ieee;
use ieee.std_logic_1164.all;

architecture zqogi of suo is
  signal lutczznf : std_logic;
  signal acsvkoa : std_logic;
  signal cdrxknygl : time;
  signal ifkrlxfq : std_logic;
  signal yvpbycglb : character;
  signal k : time;
begin
  oeo : entity work.ndicwkj
    port map (cjhxeyi => k, y => yvpbycglb, ylwnmj => ifkrlxfq);
  tmjnucnsec : entity work.rengetc
    port map (ab => ifkrlxfq);
  whqkl : entity work.ndicwkj
    port map (cjhxeyi => cdrxknygl, y => yvpbycglb, ylwnmj => acsvkoa);
  yfytpkqigi : entity work.rengetc
    port map (ab => lutczznf);
  
  -- Multi-driven assignments
  ifkrlxfq <= 'X';
end zqogi;



-- Seed after: 1409394007534723796,3924983747739634027
