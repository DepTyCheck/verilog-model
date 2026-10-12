-- Seed: 1322117972792814392,6869379746012019351

library ieee;
use ieee.std_logic_1164.all;

entity jgll is
  port (qxjpmv : linkage std_logic);
end jgll;

architecture gvrwajhhqz of jgll is
  
begin
  
end gvrwajhhqz;

library ieee;
use ieee.std_logic_1164.all;

entity rsmw is
  port (rc : in severity_level; ztimozo : inout std_logic; awdb : linkage string(3 downto 2));
end rsmw;

library ieee;
use ieee.std_logic_1164.all;

architecture wdtzlnnfy of rsmw is
  signal dtbbjlwf : std_logic;
begin
  kk : entity work.jgll
    port map (qxjpmv => dtbbjlwf);
  vt : entity work.jgll
    port map (qxjpmv => ztimozo);
  
  -- Multi-driven assignments
  ztimozo <= '1';
  ztimozo <= 'U';
end wdtzlnnfy;

library ieee;
use ieee.std_logic_1164.all;

entity fefget is
  port (al : out std_logic);
end fefget;

library ieee;
use ieee.std_logic_1164.all;

architecture jlvj of fefget is
  signal xymqwwd : string(3 downto 2);
  signal isxi : std_logic;
  signal jlygyo : string(3 downto 2);
  signal qoxpzyim : severity_level;
  signal sinrrr : std_logic;
begin
  bqsjsbj : entity work.jgll
    port map (qxjpmv => sinrrr);
  ydwpgafoh : entity work.rsmw
    port map (rc => qoxpzyim, ztimozo => al, awdb => jlygyo);
  pc : entity work.rsmw
    port map (rc => qoxpzyim, ztimozo => isxi, awdb => xymqwwd);
  o : entity work.jgll
    port map (qxjpmv => isxi);
  
  -- Single-driven assignments
  qoxpzyim <= qoxpzyim;
  
  -- Multi-driven assignments
  sinrrr <= 'Z';
  al <= sinrrr;
  isxi <= 'X';
  sinrrr <= al;
end jlvj;



-- Seed after: 1902623061111929495,6869379746012019351
