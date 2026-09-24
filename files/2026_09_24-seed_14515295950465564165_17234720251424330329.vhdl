-- Seed: 14515295950465564165,17234720251424330329

library ieee;
use ieee.std_logic_1164.all;

entity hhvwbia is
  port (rpzcccqz : out integer; t : out std_logic_vector(1 to 2); jsw : inout std_logic_vector(3 downto 3));
end hhvwbia;

architecture cquduqhfi of hhvwbia is
  
begin
  
end cquduqhfi;

library ieee;
use ieee.std_logic_1164.all;

entity dkfl is
  port (yjh : buffer std_logic_vector(3 downto 4); hhkabpfca : out std_logic_vector(2 downto 3); d : in std_logic);
end dkfl;

library ieee;
use ieee.std_logic_1164.all;

architecture dmaalznvx of dkfl is
  signal duhsmdnp : std_logic_vector(3 downto 3);
  signal pc : integer;
  signal mymi : integer;
  signal gzdu : std_logic_vector(3 downto 3);
  signal xod : std_logic_vector(1 to 2);
  signal kclq : integer;
begin
  xkbrpxrwn : entity work.hhvwbia
    port map (rpzcccqz => kclq, t => xod, jsw => gzdu);
  lvyuuanb : entity work.hhvwbia
    port map (rpzcccqz => mymi, t => xod, jsw => gzdu);
  gxtjppup : entity work.hhvwbia
    port map (rpzcccqz => pc, t => xod, jsw => duhsmdnp);
  
  -- Multi-driven assignments
  xod <= ('-', 'L');
  duhsmdnp <= gzdu;
  yjh <= hhkabpfca;
end dmaalznvx;



-- Seed after: 9803619822460010697,17234720251424330329
