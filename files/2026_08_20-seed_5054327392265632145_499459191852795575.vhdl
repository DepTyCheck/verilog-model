-- Seed: 5054327392265632145,499459191852795575

library ieee;
use ieee.std_logic_1164.all;

entity witwsro is
  port (gdymgljqot : linkage boolean; xqxcjbzwqr : in std_logic_vector(1 downto 3); udunpvb : linkage integer);
end witwsro;

architecture gzm of witwsro is
  
begin
  
end gzm;

library ieee;
use ieee.std_logic_1164.all;

entity zwekxbahqg is
  port (gldjsawi : in std_logic; z : in time);
end zwekxbahqg;

library ieee;
use ieee.std_logic_1164.all;

architecture vt of zwekxbahqg is
  signal zlfg : integer;
  signal oamr : std_logic_vector(1 downto 3);
  signal aov : boolean;
begin
  uxyqm : entity work.witwsro
    port map (gdymgljqot => aov, xqxcjbzwqr => oamr, udunpvb => zlfg);
  
  -- Multi-driven assignments
  oamr <= oamr;
end vt;

entity dt is
  port (d : buffer real; mtamzazium : out bit_vector(3 to 3); shm : out real);
end dt;

library ieee;
use ieee.std_logic_1164.all;

architecture smz of dt is
  signal pjavau : integer;
  signal a : boolean;
  signal jicenoqit : integer;
  signal cahdizgkz : std_logic_vector(1 downto 3);
  signal jrbhlfllc : boolean;
  signal lievtdluzp : time;
  signal g : time;
  signal aoxmqd : std_logic;
begin
  tqqbd : entity work.zwekxbahqg
    port map (gldjsawi => aoxmqd, z => g);
  dg : entity work.zwekxbahqg
    port map (gldjsawi => aoxmqd, z => lievtdluzp);
  eomrrmp : entity work.witwsro
    port map (gdymgljqot => jrbhlfllc, xqxcjbzwqr => cahdizgkz, udunpvb => jicenoqit);
  nelgn : entity work.witwsro
    port map (gdymgljqot => a, xqxcjbzwqr => cahdizgkz, udunpvb => pjavau);
  
  -- Multi-driven assignments
  aoxmqd <= aoxmqd;
  aoxmqd <= '-';
  aoxmqd <= aoxmqd;
  cahdizgkz <= (others => '0');
end smz;



-- Seed after: 14233305525955900789,499459191852795575
