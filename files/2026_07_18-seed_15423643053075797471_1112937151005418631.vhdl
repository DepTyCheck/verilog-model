-- Seed: 15423643053075797471,1112937151005418631

library ieee;
use ieee.std_logic_1164.all;

entity vs is
  port (hejktl : inout integer; qnx : in integer; yfkrp : buffer std_logic_vector(1 to 3));
end vs;

architecture f of vs is
  
begin
  -- Single-driven assignments
  hejktl <= 16#8#;
  
  -- Multi-driven assignments
  yfkrp <= yfkrp;
  yfkrp <= yfkrp;
  yfkrp <= ('L', 'Z', 'H');
  yfkrp <= yfkrp;
end f;

entity bezpjbhnep is
  port (m : in real; gdah : in integer; kstsemfs : inout bit_vector(2 downto 1));
end bezpjbhnep;

library ieee;
use ieee.std_logic_1164.all;

architecture icbsukp of bezpjbhnep is
  signal kvrmhpj : std_logic_vector(1 to 3);
  signal jvhenombox : integer;
  signal wgzb : integer;
begin
  qgmy : entity work.vs
    port map (hejktl => wgzb, qnx => jvhenombox, yfkrp => kvrmhpj);
  jre : entity work.vs
    port map (hejktl => jvhenombox, qnx => wgzb, yfkrp => kvrmhpj);
  
  -- Single-driven assignments
  kstsemfs <= kstsemfs;
  
  -- Multi-driven assignments
  kvrmhpj <= kvrmhpj;
end icbsukp;

library ieee;
use ieee.std_logic_1164.all;

entity i is
  port (wzu : out std_logic; qybkplofy : in time; phdpl : out time);
end i;

architecture lfscdold of i is
  signal upuimnjhy : bit_vector(2 downto 1);
  signal egzpqhfni : integer;
  signal yopsgxv : real;
begin
  qumc : entity work.bezpjbhnep
    port map (m => yopsgxv, gdah => egzpqhfni, kstsemfs => upuimnjhy);
  
  -- Multi-driven assignments
  wzu <= 'H';
end lfscdold;



-- Seed after: 3241879737700094234,1112937151005418631
