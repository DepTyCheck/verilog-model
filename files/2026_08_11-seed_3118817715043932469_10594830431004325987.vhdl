-- Seed: 3118817715043932469,10594830431004325987

library ieee;
use ieee.std_logic_1164.all;

entity ufekxrmu is
  port (dgjwrb : in severity_level; kewqb : inout std_logic_vector(0 downto 1); hy : in bit_vector(0 to 2));
end ufekxrmu;

architecture dgiqtvqcr of ufekxrmu is
  
begin
  -- Multi-driven assignments
  kewqb <= (others => '0');
  kewqb <= (others => '0');
end dgiqtvqcr;

library ieee;
use ieee.std_logic_1164.all;

entity ppvwphd is
  port (vzbmji : linkage severity_level; nhmpqw : inout severity_level; riwhcn : out std_logic; kgmoupktv : out std_logic);
end ppvwphd;

library ieee;
use ieee.std_logic_1164.all;

architecture oooocprkwi of ppvwphd is
  signal hwbduxpmi : bit_vector(0 to 2);
  signal rtgwbgv : severity_level;
  signal yh : bit_vector(0 to 2);
  signal gkvwo : std_logic_vector(0 downto 1);
  signal pabphxar : severity_level;
begin
  qzsr : entity work.ufekxrmu
    port map (dgjwrb => pabphxar, kewqb => gkvwo, hy => yh);
  p : entity work.ufekxrmu
    port map (dgjwrb => rtgwbgv, kewqb => gkvwo, hy => yh);
  dbb : entity work.ufekxrmu
    port map (dgjwrb => nhmpqw, kewqb => gkvwo, hy => hwbduxpmi);
  
  -- Single-driven assignments
  yh <= ('1', '0', '1');
  nhmpqw <= NOTE;
  rtgwbgv <= ERROR;
  hwbduxpmi <= yh;
  pabphxar <= ERROR;
  
  -- Multi-driven assignments
  kgmoupktv <= '0';
end oooocprkwi;

entity onyqcbdwm is
  port (yoiqwharqm : in real; fqqxdn : in character; jfhc : inout boolean_vector(3 to 0));
end onyqcbdwm;

architecture cfacjqco of onyqcbdwm is
  
begin
  -- Single-driven assignments
  jfhc <= (others => TRUE);
end cfacjqco;



-- Seed after: 13233804407196285401,10594830431004325987
