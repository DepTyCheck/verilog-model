-- Seed: 1976928369731369931,6869379746012019351

library ieee;
use ieee.std_logic_1164.all;

entity bbaigiv is
  port (x : in std_logic_vector(0 downto 3); woakpnksnt : in integer);
end bbaigiv;

architecture qwphnrsdu of bbaigiv is
  
begin
  
end qwphnrsdu;

entity pg is
  port (qws : buffer integer; nox : linkage time; qub : linkage real; ldusudnq : in time_vector(3 to 4));
end pg;

library ieee;
use ieee.std_logic_1164.all;

architecture vmogsltpy of pg is
  signal psxevrpm : std_logic_vector(0 downto 3);
  signal txih : integer;
  signal ocuclq : std_logic_vector(0 downto 3);
begin
  fhrl : entity work.bbaigiv
    port map (x => ocuclq, woakpnksnt => qws);
  un : entity work.bbaigiv
    port map (x => ocuclq, woakpnksnt => txih);
  xwcclitu : entity work.bbaigiv
    port map (x => psxevrpm, woakpnksnt => qws);
  
  -- Single-driven assignments
  qws <= qws;
  
  -- Multi-driven assignments
  ocuclq <= "";
  ocuclq <= psxevrpm;
  psxevrpm <= (others => '0');
  ocuclq <= "";
end vmogsltpy;



-- Seed after: 16748957822842287927,6869379746012019351
