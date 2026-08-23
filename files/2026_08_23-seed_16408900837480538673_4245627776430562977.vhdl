-- Seed: 16408900837480538673,4245627776430562977

library ieee;
use ieee.std_logic_1164.all;

entity ipcjcawjby is
  port (vjshztpot : out boolean_vector(2 to 1); cqqkiztezi : linkage std_logic_vector(0 downto 1); sihvolzxf : out std_logic);
end ipcjcawjby;

architecture ah of ipcjcawjby is
  
begin
  
end ah;

entity ksnx is
  port (axvcfrzfj : out integer; niobyb : out string(2 to 4));
end ksnx;

library ieee;
use ieee.std_logic_1164.all;

architecture ktyab of ksnx is
  signal utslfmdfsh : boolean_vector(2 to 1);
  signal uoaelygo : std_logic;
  signal ekec : std_logic_vector(0 downto 1);
  signal yafn : boolean_vector(2 to 1);
begin
  uit : entity work.ipcjcawjby
    port map (vjshztpot => yafn, cqqkiztezi => ekec, sihvolzxf => uoaelygo);
  irnphq : entity work.ipcjcawjby
    port map (vjshztpot => utslfmdfsh, cqqkiztezi => ekec, sihvolzxf => uoaelygo);
  
  -- Single-driven assignments
  niobyb <= niobyb;
  axvcfrzfj <= axvcfrzfj;
  
  -- Multi-driven assignments
  ekec <= ekec;
  ekec <= ekec;
  ekec <= ekec;
  uoaelygo <= uoaelygo;
end ktyab;



-- Seed after: 274495019347910458,4245627776430562977
