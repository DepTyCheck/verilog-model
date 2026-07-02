-- Seed: 12183240870408067007,13694093582652240945

library ieee;
use ieee.std_logic_1164.all;

entity uvtdm is
  port (kxshvspwf : linkage std_logic_vector(3 to 3); dyphydx : buffer std_logic; jkfve : inout std_logic_vector(0 to 4));
end uvtdm;

architecture mgo of uvtdm is
  
begin
  -- Multi-driven assignments
  jkfve <= ('-', 'U', 'W', '-', 'W');
  dyphydx <= 'L';
  dyphydx <= 'U';
  dyphydx <= '-';
end mgo;

library ieee;
use ieee.std_logic_1164.all;

entity v is
  port (fgv : linkage std_logic_vector(4 downto 4); dfblewqqd : out std_logic_vector(1 downto 1); tbqpkzwo : inout real; i : buffer severity_level);
end v;

library ieee;
use ieee.std_logic_1164.all;

architecture jmuvdp of v is
  signal aswij : std_logic_vector(0 to 4);
  signal lq : std_logic;
  signal q : std_logic_vector(3 to 3);
begin
  tmrwcpqw : entity work.uvtdm
    port map (kxshvspwf => q, dyphydx => lq, jkfve => aswij);
  
  -- Single-driven assignments
  i <= NOTE;
  tbqpkzwo <= 1.24433;
  
  -- Multi-driven assignments
  dfblewqqd <= "H";
  lq <= 'U';
  dfblewqqd <= (others => 'L');
end jmuvdp;

entity efdtjsfeto is
  port (ivvjfi : inout time);
end efdtjsfeto;

library ieee;
use ieee.std_logic_1164.all;

architecture sj of efdtjsfeto is
  signal hgnupgyax : std_logic;
  signal h : std_logic_vector(3 to 3);
  signal ockwle : std_logic_vector(0 to 4);
  signal kwjqslsy : std_logic;
  signal ow : std_logic_vector(3 to 3);
begin
  wbedxs : entity work.uvtdm
    port map (kxshvspwf => ow, dyphydx => kwjqslsy, jkfve => ockwle);
  hktbekasp : entity work.uvtdm
    port map (kxshvspwf => h, dyphydx => hgnupgyax, jkfve => ockwle);
  
  -- Single-driven assignments
  ivvjfi <= 16#7# us;
  
  -- Multi-driven assignments
  h <= "L";
end sj;

library ieee;
use ieee.std_logic_1164.all;

entity duaute is
  port (azykm : buffer real; uedeovuku : out time; lkwzj : in std_logic_vector(0 downto 4));
end duaute;

architecture siivj of duaute is
  
begin
  dbuaprflw : entity work.efdtjsfeto
    port map (ivvjfi => uedeovuku);
  
  -- Single-driven assignments
  azykm <= 16#47E.1_9_A#;
end siivj;



-- Seed after: 13660226180510006949,13694093582652240945
