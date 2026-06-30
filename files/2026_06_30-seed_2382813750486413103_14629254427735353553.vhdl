-- Seed: 2382813750486413103,14629254427735353553

library ieee;
use ieee.std_logic_1164.all;

entity mapdbkl is
  port (aznvu : in std_logic; pendpjfmj : out std_logic_vector(3 downto 2); kdfjikk : in std_logic_vector(3 downto 1); bbor : in std_logic);
end mapdbkl;

architecture sxbtvpz of mapdbkl is
  
begin
  -- Multi-driven assignments
  pendpjfmj <= "LZ";
  pendpjfmj <= ('Z', 'U');
  pendpjfmj <= "ZW";
  pendpjfmj <= ('1', 'X');
end sxbtvpz;

library ieee;
use ieee.std_logic_1164.all;

entity qzws is
  port (cxlqwgid : in integer; ovxvtdwxh : buffer std_logic_vector(4 downto 2); pikglxo : linkage std_logic_vector(1 to 1));
end qzws;

library ieee;
use ieee.std_logic_1164.all;

architecture skqctnqfvo of qzws is
  signal epjhbsx : std_logic;
  signal dxliqiq : std_logic_vector(3 downto 1);
  signal waxignmce : std_logic_vector(3 downto 2);
  signal atgqwmn : std_logic;
  signal itukyxl : std_logic_vector(3 downto 1);
  signal dozxx : std_logic_vector(3 downto 2);
  signal wcqjt : std_logic;
  signal ekjikpzib : std_logic;
  signal btcemg : std_logic_vector(3 downto 2);
  signal h : std_logic_vector(3 downto 1);
  signal l : std_logic_vector(3 downto 2);
  signal zmzlbti : std_logic;
begin
  x : entity work.mapdbkl
    port map (aznvu => zmzlbti, pendpjfmj => l, kdfjikk => h, bbor => zmzlbti);
  nvftfisw : entity work.mapdbkl
    port map (aznvu => zmzlbti, pendpjfmj => btcemg, kdfjikk => ovxvtdwxh, bbor => ekjikpzib);
  mvuxiq : entity work.mapdbkl
    port map (aznvu => wcqjt, pendpjfmj => dozxx, kdfjikk => itukyxl, bbor => atgqwmn);
  hiypn : entity work.mapdbkl
    port map (aznvu => zmzlbti, pendpjfmj => waxignmce, kdfjikk => dxliqiq, bbor => epjhbsx);
  
  -- Multi-driven assignments
  wcqjt <= 'L';
  ovxvtdwxh <= ('X', 'U', 'H');
  l <= ('H', '-');
end skqctnqfvo;

library ieee;
use ieee.std_logic_1164.all;

entity wcpt is
  port (xgnpwmxnpx : buffer std_logic_vector(1 to 3); k : out bit; bgz : buffer std_logic_vector(0 to 0); oniflxglo : buffer bit);
end wcpt;

library ieee;
use ieee.std_logic_1164.all;

architecture qwi of wcpt is
  signal dumkmwhq : std_logic;
  signal yycw : std_logic_vector(3 downto 2);
  signal a : std_logic;
begin
  txgzihboyz : entity work.mapdbkl
    port map (aznvu => a, pendpjfmj => yycw, kdfjikk => xgnpwmxnpx, bbor => dumkmwhq);
  wvqw : entity work.mapdbkl
    port map (aznvu => a, pendpjfmj => yycw, kdfjikk => xgnpwmxnpx, bbor => a);
  
  -- Single-driven assignments
  oniflxglo <= '0';
  k <= '1';
  
  -- Multi-driven assignments
  dumkmwhq <= '-';
end qwi;



-- Seed after: 12435619279648628030,14629254427735353553
