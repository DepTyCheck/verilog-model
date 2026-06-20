-- Seed: 10187419468977369675,17924494779688682807

library ieee;
use ieee.std_logic_1164.all;

entity nmk is
  port (zrqfx : in std_logic; vvmo : inout std_logic_vector(3 downto 3));
end nmk;

architecture d of nmk is
  
begin
  -- Multi-driven assignments
  vvmo <= "-";
end d;

library ieee;
use ieee.std_logic_1164.all;

entity qe is
  port (rlwkkwinu : inout time_vector(4 to 0); e : out std_logic_vector(1 downto 3); hymh : out std_logic; zhrv : out severity_level);
end qe;

library ieee;
use ieee.std_logic_1164.all;

architecture vn of qe is
  signal yecrnbs : std_logic_vector(3 downto 3);
  signal dzacnyl : std_logic_vector(3 downto 3);
  signal i : std_logic;
begin
  oqxbwc : entity work.nmk
    port map (zrqfx => i, vvmo => dzacnyl);
  ybvjp : entity work.nmk
    port map (zrqfx => hymh, vvmo => dzacnyl);
  iew : entity work.nmk
    port map (zrqfx => hymh, vvmo => yecrnbs);
end vn;



-- Seed after: 6886995690589207713,17924494779688682807
