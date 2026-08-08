-- Seed: 11060797275495094467,8927267689619684183

library ieee;
use ieee.std_logic_1164.all;

entity a is
  port (wdnr : inout std_logic);
end a;

architecture ltjikp of a is
  
begin
  -- Multi-driven assignments
  wdnr <= wdnr;
  wdnr <= 'U';
  wdnr <= wdnr;
end ltjikp;

library ieee;
use ieee.std_logic_1164.all;

entity prebxlg is
  port (v : in std_logic; dfn : inout std_logic_vector(1 downto 3); aaoz : out character);
end prebxlg;

library ieee;
use ieee.std_logic_1164.all;

architecture x of prebxlg is
  signal hizodnist : std_logic;
  signal eu : std_logic;
  signal rwf : std_logic;
begin
  kixzhkyr : entity work.a
    port map (wdnr => rwf);
  p : entity work.a
    port map (wdnr => eu);
  t : entity work.a
    port map (wdnr => hizodnist);
  
  -- Single-driven assignments
  aaoz <= 'n';
  
  -- Multi-driven assignments
  hizodnist <= v;
  dfn <= dfn;
end x;



-- Seed after: 7926647472877084437,8927267689619684183
