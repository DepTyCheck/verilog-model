-- Seed: 15437243895470077159,14629254427735353553

library ieee;
use ieee.std_logic_1164.all;

entity tcv is
  port (lnu : linkage real; vtq : in std_logic);
end tcv;

architecture rcddqxnu of tcv is
  
begin
  
end rcddqxnu;

entity ezmds is
  port (uahhe : buffer time_vector(1 to 0));
end ezmds;

library ieee;
use ieee.std_logic_1164.all;

architecture xtgztd of ezmds is
  signal o : real;
  signal ipdxuyrcgx : std_logic;
  signal d : real;
  signal jvckbh : real;
  signal nogkui : std_logic;
  signal rjy : real;
begin
  exirb : entity work.tcv
    port map (lnu => rjy, vtq => nogkui);
  ktikdfrrrm : entity work.tcv
    port map (lnu => jvckbh, vtq => nogkui);
  gorce : entity work.tcv
    port map (lnu => d, vtq => ipdxuyrcgx);
  y : entity work.tcv
    port map (lnu => o, vtq => nogkui);
  
  -- Single-driven assignments
  uahhe <= (others => 0 ns);
  
  -- Multi-driven assignments
  nogkui <= '1';
  nogkui <= '-';
  nogkui <= '0';
end xtgztd;

entity ponrlnue is
  port (lkqavvkq : linkage real; wcfmfrowm : out character);
end ponrlnue;

library ieee;
use ieee.std_logic_1164.all;

architecture xb of ponrlnue is
  signal lmbzxfjvbz : std_logic;
begin
  v : entity work.tcv
    port map (lnu => lkqavvkq, vtq => lmbzxfjvbz);
  
  -- Single-driven assignments
  wcfmfrowm <= 'e';
  
  -- Multi-driven assignments
  lmbzxfjvbz <= 'X';
  lmbzxfjvbz <= '0';
  lmbzxfjvbz <= 'W';
  lmbzxfjvbz <= 'W';
end xb;



-- Seed after: 14280083761069565966,14629254427735353553
