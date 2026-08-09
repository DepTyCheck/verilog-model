-- Seed: 8932507895287946367,10871023049702252113

library ieee;
use ieee.std_logic_1164.all;

entity gxsorgja is
  port (h : buffer std_logic_vector(2 to 3); u : out real; tcxf : buffer std_logic_vector(3 downto 3));
end gxsorgja;

architecture zwnuswfeh of gxsorgja is
  
begin
  -- Single-driven assignments
  u <= u;
  
  -- Multi-driven assignments
  tcxf <= (others => 'W');
  h <= ('-', '1');
  tcxf <= tcxf;
end zwnuswfeh;

library ieee;
use ieee.std_logic_1164.all;

entity ldhwwa is
  port (tgozzhz : out real; vmdool : buffer severity_level; alybguv : inout std_logic; xxaq : inout std_logic_vector(2 downto 3));
end ldhwwa;

library ieee;
use ieee.std_logic_1164.all;

architecture hj of ldhwwa is
  signal kfcphthcht : std_logic_vector(3 downto 3);
  signal eaqkjwvtjx : real;
  signal f : std_logic_vector(2 to 3);
begin
  sjxgyvbmlg : entity work.gxsorgja
    port map (h => f, u => eaqkjwvtjx, tcxf => kfcphthcht);
  
  -- Single-driven assignments
  vmdool <= NOTE;
  tgozzhz <= tgozzhz;
end hj;

library ieee;
use ieee.std_logic_1164.all;

entity mebblmcc is
  port (wjucpvsb : out std_logic; unixkj : in time; pnytkkk : in std_logic);
end mebblmcc;

library ieee;
use ieee.std_logic_1164.all;

architecture brcknzcjqw of mebblmcc is
  signal ldup : std_logic_vector(2 downto 3);
  signal duldnlxu : std_logic;
  signal zctblc : severity_level;
  signal szfy : real;
begin
  hzyzfxk : entity work.ldhwwa
    port map (tgozzhz => szfy, vmdool => zctblc, alybguv => duldnlxu, xxaq => ldup);
end brcknzcjqw;

library ieee;
use ieee.std_logic_1164.all;

entity qzcvamoqg is
  port (rtnvvkiciq : linkage time; tsduy : out time; fa : buffer severity_level; iflp : in std_logic_vector(0 downto 3));
end qzcvamoqg;

library ieee;
use ieee.std_logic_1164.all;

architecture psq of qzcvamoqg is
  signal hullyc : std_logic;
  signal cdd : std_logic;
  signal o : std_logic;
  signal usllla : std_logic_vector(3 downto 3);
  signal fmlv : real;
  signal uodajznnwd : std_logic_vector(2 to 3);
begin
  jlxpg : entity work.gxsorgja
    port map (h => uodajznnwd, u => fmlv, tcxf => usllla);
  cry : entity work.mebblmcc
    port map (wjucpvsb => o, unixkj => tsduy, pnytkkk => o);
  vrpgq : entity work.mebblmcc
    port map (wjucpvsb => cdd, unixkj => tsduy, pnytkkk => hullyc);
  
  -- Single-driven assignments
  tsduy <= 04430 fs;
  fa <= FAILURE;
  
  -- Multi-driven assignments
  o <= '-';
  o <= '1';
end psq;



-- Seed after: 4326527156662168537,10871023049702252113
