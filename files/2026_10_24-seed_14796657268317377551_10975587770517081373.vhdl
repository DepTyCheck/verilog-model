-- Seed: 14796657268317377551,10975587770517081373

library ieee;
use ieee.std_logic_1164.all;

entity pdnmit is
  port (zjwcradvtz : in severity_level; rm : inout std_logic; dikunb : buffer integer_vector(2 downto 2));
end pdnmit;

architecture ei of pdnmit is
  
begin
  -- Single-driven assignments
  dikunb <= dikunb;
end ei;

entity y is
  port (xlckq : in time);
end y;

library ieee;
use ieee.std_logic_1164.all;

architecture jbpxlb of y is
  signal wiilvbrl : integer_vector(2 downto 2);
  signal xlszt : integer_vector(2 downto 2);
  signal kcnkioc : std_logic;
  signal ohib : severity_level;
  signal wbunuhzd : integer_vector(2 downto 2);
  signal i : std_logic;
  signal vegwevkp : severity_level;
begin
  zcdhuu : entity work.pdnmit
    port map (zjwcradvtz => vegwevkp, rm => i, dikunb => wbunuhzd);
  tnxkddz : entity work.pdnmit
    port map (zjwcradvtz => ohib, rm => kcnkioc, dikunb => xlszt);
  xukwryhfyz : entity work.pdnmit
    port map (zjwcradvtz => vegwevkp, rm => kcnkioc, dikunb => wiilvbrl);
  
  -- Single-driven assignments
  vegwevkp <= FAILURE;
  ohib <= NOTE;
  
  -- Multi-driven assignments
  i <= '1';
  i <= '0';
  i <= 'Z';
  kcnkioc <= 'H';
end jbpxlb;

library ieee;
use ieee.std_logic_1164.all;

entity ybisb is
  port (pwnsh : out time; whxjrgnlu : inout bit; esk : in std_logic);
end ybisb;

architecture ioacsm of ybisb is
  
begin
  zmvkknsl : entity work.y
    port map (xlckq => pwnsh);
  a : entity work.y
    port map (xlckq => pwnsh);
  
  -- Single-driven assignments
  whxjrgnlu <= '0';
  pwnsh <= pwnsh;
end ioacsm;

entity dpasjlsoud is
  port (xyzchbf : linkage time; wjfuj : inout real);
end dpasjlsoud;

library ieee;
use ieee.std_logic_1164.all;

architecture hc of dpasjlsoud is
  signal yjtoe : std_logic;
  signal jsx : bit;
  signal yho : time;
  signal bzm : integer_vector(2 downto 2);
  signal sliudokw : std_logic;
  signal yjdjpaoo : severity_level;
begin
  yj : entity work.pdnmit
    port map (zjwcradvtz => yjdjpaoo, rm => sliudokw, dikunb => bzm);
  cpnkpfdvhr : entity work.ybisb
    port map (pwnsh => yho, whxjrgnlu => jsx, esk => yjtoe);
  
  -- Single-driven assignments
  wjfuj <= 3.0;
  yjdjpaoo <= yjdjpaoo;
  
  -- Multi-driven assignments
  sliudokw <= 'H';
end hc;



-- Seed after: 17322114606933139161,10975587770517081373
