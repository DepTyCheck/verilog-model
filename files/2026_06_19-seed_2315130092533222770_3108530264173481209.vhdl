-- Seed: 2315130092533222770,3108530264173481209

entity qqrwtl is
  port (lslmpqqimv : linkage time_vector(4 downto 1); kx : buffer string(3 to 2));
end qqrwtl;

architecture yccgilqmxl of qqrwtl is
  
begin
  
end yccgilqmxl;

library ieee;
use ieee.std_logic_1164.all;

entity g is
  port (llpcwucx : out std_logic; u : inout std_logic; os : in time_vector(0 downto 2));
end g;

architecture ufcdlpgny of g is
  signal ah : string(3 to 2);
  signal fz : time_vector(4 downto 1);
  signal d : string(3 to 2);
  signal cm : time_vector(4 downto 1);
  signal v : string(3 to 2);
  signal tnjubz : time_vector(4 downto 1);
  signal lew : string(3 to 2);
  signal rekep : time_vector(4 downto 1);
begin
  uyzblviv : entity work.qqrwtl
    port map (lslmpqqimv => rekep, kx => lew);
  wrw : entity work.qqrwtl
    port map (lslmpqqimv => tnjubz, kx => v);
  rnvkjvoya : entity work.qqrwtl
    port map (lslmpqqimv => cm, kx => d);
  zazi : entity work.qqrwtl
    port map (lslmpqqimv => fz, kx => ah);
  
  -- Multi-driven assignments
  u <= '-';
end ufcdlpgny;

entity meurwmta is
  port (sevjp : inout severity_level);
end meurwmta;

library ieee;
use ieee.std_logic_1164.all;

architecture zr of meurwmta is
  signal nxhedjwk : string(3 to 2);
  signal apndcivhoj : time_vector(4 downto 1);
  signal ppc : string(3 to 2);
  signal jyxur : time_vector(4 downto 1);
  signal hrpql : string(3 to 2);
  signal bwbzthnqc : time_vector(4 downto 1);
  signal ydl : time_vector(0 downto 2);
  signal h : std_logic;
  signal kckqhqclf : std_logic;
begin
  ofifl : entity work.g
    port map (llpcwucx => kckqhqclf, u => h, os => ydl);
  lhitez : entity work.qqrwtl
    port map (lslmpqqimv => bwbzthnqc, kx => hrpql);
  lktgeekdxc : entity work.qqrwtl
    port map (lslmpqqimv => jyxur, kx => ppc);
  z : entity work.qqrwtl
    port map (lslmpqqimv => apndcivhoj, kx => nxhedjwk);
  
  -- Single-driven assignments
  sevjp <= NOTE;
  
  -- Multi-driven assignments
  kckqhqclf <= 'W';
  kckqhqclf <= '-';
end zr;



-- Seed after: 12889403918336232742,3108530264173481209
