-- Seed: 9112535471678490184,7808623373429384027

library ieee;
use ieee.std_logic_1164.all;

entity mdv is
  port (vzsjzu : in severity_level; apxnblqtf : inout string(2 downto 2); rctnjaq : buffer time; wnip : inout std_logic);
end mdv;

architecture wajj of mdv is
  
begin
  -- Single-driven assignments
  rctnjaq <= 0102 ns;
  apxnblqtf <= "i";
  
  -- Multi-driven assignments
  wnip <= wnip;
end wajj;

entity brrqdsoz is
  port (cvbrqro : inout severity_level; ycc : linkage integer; cndesv : out time);
end brrqdsoz;

library ieee;
use ieee.std_logic_1164.all;

architecture hcxo of brrqdsoz is
  signal svjyjpdh : time;
  signal vesnkqr : string(2 downto 2);
  signal suqyrvv : std_logic;
  signal eb : string(2 downto 2);
  signal mcyu : time;
  signal wjmgmfi : string(2 downto 2);
  signal xufdasn : std_logic;
  signal an : time;
  signal grvaagb : string(2 downto 2);
  signal xppfenigm : severity_level;
begin
  wsrgaogr : entity work.mdv
    port map (vzsjzu => xppfenigm, apxnblqtf => grvaagb, rctnjaq => an, wnip => xufdasn);
  j : entity work.mdv
    port map (vzsjzu => xppfenigm, apxnblqtf => wjmgmfi, rctnjaq => mcyu, wnip => xufdasn);
  yktsclccxn : entity work.mdv
    port map (vzsjzu => cvbrqro, apxnblqtf => eb, rctnjaq => cndesv, wnip => suqyrvv);
  kiffbrq : entity work.mdv
    port map (vzsjzu => cvbrqro, apxnblqtf => vesnkqr, rctnjaq => svjyjpdh, wnip => xufdasn);
  
  -- Single-driven assignments
  cvbrqro <= ERROR;
  xppfenigm <= NOTE;
  
  -- Multi-driven assignments
  suqyrvv <= xufdasn;
  xufdasn <= 'X';
end hcxo;

library ieee;
use ieee.std_logic_1164.all;

entity m is
  port (xbvslvxc : out std_logic; gq : out std_logic);
end m;

architecture dblpjbnrll of m is
  signal qjzqfd : time;
  signal pxzsa : string(2 downto 2);
  signal zikcey : time;
  signal zrph : string(2 downto 2);
  signal klt : severity_level;
begin
  v : entity work.mdv
    port map (vzsjzu => klt, apxnblqtf => zrph, rctnjaq => zikcey, wnip => gq);
  nc : entity work.mdv
    port map (vzsjzu => klt, apxnblqtf => pxzsa, rctnjaq => qjzqfd, wnip => gq);
  
  -- Single-driven assignments
  klt <= NOTE;
end dblpjbnrll;



-- Seed after: 1347019768945033547,7808623373429384027
