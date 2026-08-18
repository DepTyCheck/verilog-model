-- Seed: 3818106117411689177,5983430343285687595

library ieee;
use ieee.std_logic_1164.all;

entity rd is
  port (ng : buffer std_logic_vector(4 downto 0); rpa : in std_logic; qhxj : inout bit_vector(3 downto 2));
end rd;

architecture cdq of rd is
  
begin
  -- Single-driven assignments
  qhxj <= ('0', '0');
  
  -- Multi-driven assignments
  ng <= ('0', '0', 'H', '1', 'Z');
  ng <= ng;
end cdq;

entity mghrcttif is
  port (ypjyzosme : in integer; gu : linkage integer);
end mghrcttif;

library ieee;
use ieee.std_logic_1164.all;

architecture cps of mghrcttif is
  signal wsjuu : bit_vector(3 downto 2);
  signal npptlncw : std_logic;
  signal j : std_logic_vector(4 downto 0);
begin
  s : entity work.rd
    port map (ng => j, rpa => npptlncw, qhxj => wsjuu);
  
  -- Multi-driven assignments
  npptlncw <= 'H';
  j <= ('Z', '-', 'X', '0', 'L');
  j <= j;
  j <= j;
end cps;

library ieee;
use ieee.std_logic_1164.all;

entity lzffytsz is
  port (f : buffer severity_level; nwusic : in std_logic_vector(1 to 0));
end lzffytsz;

library ieee;
use ieee.std_logic_1164.all;

architecture lsxmt of lzffytsz is
  signal wapmmzbp : integer;
  signal blmvltxec : bit_vector(3 downto 2);
  signal ecukjenkr : std_logic;
  signal vajlx : std_logic_vector(4 downto 0);
  signal wnrj : integer;
begin
  al : entity work.mghrcttif
    port map (ypjyzosme => wnrj, gu => wnrj);
  vb : entity work.rd
    port map (ng => vajlx, rpa => ecukjenkr, qhxj => blmvltxec);
  ceafa : entity work.mghrcttif
    port map (ypjyzosme => wnrj, gu => wapmmzbp);
  
  -- Single-driven assignments
  f <= f;
  
  -- Multi-driven assignments
  vajlx <= ('H', 'Z', 'L', 'W', '-');
end lsxmt;



-- Seed after: 6542604487826245287,5983430343285687595
