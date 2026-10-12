-- Seed: 1357866618498367165,6869379746012019351

library ieee;
use ieee.std_logic_1164.all;

entity vbtsopznm is
  port (vimzthafaj : in std_logic; xk : out std_logic; n : buffer real; zx : buffer std_logic);
end vbtsopznm;

architecture d of vbtsopznm is
  
begin
  -- Single-driven assignments
  n <= 8#1_1.6_3_1_5#;
  
  -- Multi-driven assignments
  xk <= vimzthafaj;
  zx <= zx;
  zx <= vimzthafaj;
end d;

library ieee;
use ieee.std_logic_1164.all;

entity spkmy is
  port (ooxzi : inout std_logic);
end spkmy;

library ieee;
use ieee.std_logic_1164.all;

architecture selzdkk of spkmy is
  signal dfkvqbg : real;
  signal bqvrxotckc : std_logic;
  signal qcuhsgyei : std_logic;
  signal uqtup : std_logic;
  signal xmzcfr : real;
  signal ejdxdvid : std_logic;
  signal kmi : real;
  signal fqcpjqw : std_logic;
  signal iqdpriiowl : std_logic;
  signal w : real;
  signal jgeeyb : std_logic;
begin
  xmzpm : entity work.vbtsopznm
    port map (vimzthafaj => ooxzi, xk => jgeeyb, n => w, zx => iqdpriiowl);
  wrxpa : entity work.vbtsopznm
    port map (vimzthafaj => fqcpjqw, xk => ooxzi, n => kmi, zx => ejdxdvid);
  psv : entity work.vbtsopznm
    port map (vimzthafaj => ooxzi, xk => ooxzi, n => xmzcfr, zx => uqtup);
  mdiwmayya : entity work.vbtsopznm
    port map (vimzthafaj => qcuhsgyei, xk => bqvrxotckc, n => dfkvqbg, zx => ejdxdvid);
end selzdkk;

library ieee;
use ieee.std_logic_1164.all;

entity rmvldy is
  port (keklpt : linkage std_logic_vector(0 downto 4); dxrydyty : linkage integer; s : buffer integer);
end rmvldy;

library ieee;
use ieee.std_logic_1164.all;

architecture uufgp of rmvldy is
  signal uooodvt : real;
  signal o : std_logic;
  signal mz : real;
  signal wkugrutvba : std_logic;
  signal t : std_logic;
begin
  ikpusaihk : entity work.vbtsopznm
    port map (vimzthafaj => t, xk => wkugrutvba, n => mz, zx => o);
  halaza : entity work.spkmy
    port map (ooxzi => o);
  vroyyjvajx : entity work.spkmy
    port map (ooxzi => wkugrutvba);
  iohss : entity work.vbtsopznm
    port map (vimzthafaj => t, xk => t, n => uooodvt, zx => wkugrutvba);
  
  -- Single-driven assignments
  s <= 10333;
  
  -- Multi-driven assignments
  t <= '0';
  t <= '-';
end uufgp;



-- Seed after: 4894652523499190806,6869379746012019351
