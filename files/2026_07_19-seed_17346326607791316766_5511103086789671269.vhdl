-- Seed: 17346326607791316766,5511103086789671269

library ieee;
use ieee.std_logic_1164.all;

entity ugqctc is
  port (o : buffer time; p : inout std_logic);
end ugqctc;

architecture r of ugqctc is
  
begin
  
end r;

library ieee;
use ieee.std_logic_1164.all;

entity cuhcwd is
  port (q : out severity_level; rnfyfiin : out std_logic_vector(4 downto 1); hgw : in real);
end cuhcwd;

library ieee;
use ieee.std_logic_1164.all;

architecture tzg of cuhcwd is
  signal wix : time;
  signal coqdsj : time;
  signal qbxjah : std_logic;
  signal u : time;
  signal pwqduqdw : std_logic;
  signal oqxrgf : time;
begin
  wbr : entity work.ugqctc
    port map (o => oqxrgf, p => pwqduqdw);
  aup : entity work.ugqctc
    port map (o => u, p => qbxjah);
  urouoqiudk : entity work.ugqctc
    port map (o => coqdsj, p => pwqduqdw);
  xjrjtbgvu : entity work.ugqctc
    port map (o => wix, p => pwqduqdw);
  
  -- Single-driven assignments
  q <= q;
  
  -- Multi-driven assignments
  rnfyfiin <= ('H', 'L', '1', 'W');
  qbxjah <= '-';
end tzg;



-- Seed after: 3668480564735678896,5511103086789671269
