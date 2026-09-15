-- Seed: 11745905868136270559,13613332369802491303

library ieee;
use ieee.std_logic_1164.all;

entity t is
  port (dmjiju : out std_logic; urqhrjtl : in time; s : linkage real; kzmg : in real);
end t;

architecture vnssjldgie of t is
  
begin
  -- Multi-driven assignments
  dmjiju <= '-';
  dmjiju <= 'Z';
end vnssjldgie;

entity mbasacfr is
  port (cuqki : in boolean_vector(0 downto 4); fndq : out bit; r : in time);
end mbasacfr;

library ieee;
use ieee.std_logic_1164.all;

architecture tdjp of mbasacfr is
  signal qoo : real;
  signal uyh : std_logic;
  signal onozmkuays : real;
  signal xfkuyqxl : real;
  signal asgyezsd : time;
  signal tqa : std_logic;
begin
  gragxiqf : entity work.t
    port map (dmjiju => tqa, urqhrjtl => asgyezsd, s => xfkuyqxl, kzmg => onozmkuays);
  plwvgaj : entity work.t
    port map (dmjiju => uyh, urqhrjtl => r, s => qoo, kzmg => xfkuyqxl);
  xbwkcyd : entity work.t
    port map (dmjiju => tqa, urqhrjtl => asgyezsd, s => onozmkuays, kzmg => xfkuyqxl);
  
  -- Single-driven assignments
  fndq <= fndq;
  asgyezsd <= 8#1070.5_1# fs;
  
  -- Multi-driven assignments
  tqa <= 'L';
  tqa <= '1';
  tqa <= tqa;
end tdjp;



-- Seed after: 10766554653118261502,13613332369802491303
