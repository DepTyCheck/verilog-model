-- Seed: 16643613386591200191,8067602802092121131

entity fimmzv is
  port (h : inout time);
end fimmzv;

architecture isodslngnz of fimmzv is
  
begin
  -- Single-driven assignments
  h <= h;
end isodslngnz;

library ieee;
use ieee.std_logic_1164.all;

entity abkhnl is
  port (tawunnqel : linkage std_logic_vector(4 downto 1));
end abkhnl;

architecture ab of abkhnl is
  signal nskeb : time;
  signal bjnawa : time;
  signal gmvytd : time;
begin
  ufroyykul : entity work.fimmzv
    port map (h => gmvytd);
  uxirtss : entity work.fimmzv
    port map (h => bjnawa);
  fkyqu : entity work.fimmzv
    port map (h => nskeb);
end ab;

entity rjeeb is
  port (rnbcjpj : buffer bit; mgza : linkage integer; bmkfmzmhof : out integer);
end rjeeb;

library ieee;
use ieee.std_logic_1164.all;

architecture riji of rjeeb is
  signal xjsrjr : time;
  signal mzak : time;
  signal aebgvhpzin : std_logic_vector(4 downto 1);
begin
  iueonzveup : entity work.abkhnl
    port map (tawunnqel => aebgvhpzin);
  pjppzlb : entity work.fimmzv
    port map (h => mzak);
  qurukarrx : entity work.fimmzv
    port map (h => xjsrjr);
  
  -- Single-driven assignments
  bmkfmzmhof <= bmkfmzmhof;
end riji;

library ieee;
use ieee.std_logic_1164.all;

entity ngtb is
  port (knbcwz : buffer std_logic_vector(3 downto 3); gf : out real);
end ngtb;

library ieee;
use ieee.std_logic_1164.all;

architecture rnlape of ngtb is
  signal poryutpe : std_logic_vector(4 downto 1);
  signal kxmyhtc : integer;
  signal cnng : integer;
  signal s : bit;
begin
  noep : entity work.rjeeb
    port map (rnbcjpj => s, mgza => cnng, bmkfmzmhof => kxmyhtc);
  qdhiyvkfzb : entity work.abkhnl
    port map (tawunnqel => poryutpe);
  
  -- Single-driven assignments
  gf <= gf;
  
  -- Multi-driven assignments
  knbcwz <= "X";
  knbcwz <= "H";
end rnlape;



-- Seed after: 7524329552082206127,8067602802092121131
