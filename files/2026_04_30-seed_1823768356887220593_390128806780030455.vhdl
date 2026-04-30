-- Seed: 1823768356887220593,390128806780030455



entity shvymdjfu is
  port (yrlnclsn : in integer);
end shvymdjfu;



architecture bpa of shvymdjfu is
  
begin
  
end bpa;



entity tsyqn is
  port (wqucleyqg : inout bit);
end tsyqn;



architecture sm of tsyqn is
  signal zqstadh : integer;
begin
  reb : entity work.shvymdjfu
    port map (yrlnclsn => zqstadh);
end sm;

library ieee;
use ieee.std_logic_1164.all;

entity rom is
  port (qhg : in std_logic; pcmfew : in std_logic; kvjv : buffer real);
end rom;



architecture ugbrpguybf of rom is
  signal qouijr : integer;
  signal sslvyu : integer;
begin
  memmtecpsw : entity work.shvymdjfu
    port map (yrlnclsn => sslvyu);
  xpd : entity work.shvymdjfu
    port map (yrlnclsn => sslvyu);
  iacbfdzn : entity work.shvymdjfu
    port map (yrlnclsn => qouijr);
end ugbrpguybf;



entity o is
  port (wmktz : buffer integer);
end o;

library ieee;
use ieee.std_logic_1164.all;

architecture uzf of o is
  signal jiasuc : real;
  signal sktv : std_logic;
begin
  tuo : entity work.rom
    port map (qhg => sktv, pcmfew => sktv, kvjv => jiasuc);
end uzf;



-- Seed after: 12317100851801773456,390128806780030455
