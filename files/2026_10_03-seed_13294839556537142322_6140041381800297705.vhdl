-- Seed: 13294839556537142322,6140041381800297705

library ieee;
use ieee.std_logic_1164.all;

entity oyrv is
  port (sinjcqvbh : out boolean_vector(3 to 4); zkhqla : in std_logic; qgm : buffer bit; rchxfq : inout bit_vector(1 downto 1));
end oyrv;

architecture stpwka of oyrv is
  
begin
  -- Single-driven assignments
  rchxfq <= rchxfq;
  sinjcqvbh <= (FALSE, FALSE);
  qgm <= '1';
end stpwka;

entity ieylpbzj is
  port (cwt : out real);
end ieylpbzj;

library ieee;
use ieee.std_logic_1164.all;

architecture wbdqwfdvfs of ieylpbzj is
  signal lalmbn : bit_vector(1 downto 1);
  signal g : bit;
  signal iefidzgt : std_logic;
  signal bh : boolean_vector(3 to 4);
  signal tuc : bit_vector(1 downto 1);
  signal djy : bit;
  signal r : boolean_vector(3 to 4);
  signal hsybydbgt : bit_vector(1 downto 1);
  signal kqodbyci : bit;
  signal jorudlbe : boolean_vector(3 to 4);
  signal wvwrbjvlc : bit_vector(1 downto 1);
  signal tgnuq : bit;
  signal qavufcd : std_logic;
  signal t : boolean_vector(3 to 4);
begin
  redqi : entity work.oyrv
    port map (sinjcqvbh => t, zkhqla => qavufcd, qgm => tgnuq, rchxfq => wvwrbjvlc);
  hdwcqer : entity work.oyrv
    port map (sinjcqvbh => jorudlbe, zkhqla => qavufcd, qgm => kqodbyci, rchxfq => hsybydbgt);
  okzob : entity work.oyrv
    port map (sinjcqvbh => r, zkhqla => qavufcd, qgm => djy, rchxfq => tuc);
  z : entity work.oyrv
    port map (sinjcqvbh => bh, zkhqla => iefidzgt, qgm => g, rchxfq => lalmbn);
  
  -- Single-driven assignments
  cwt <= cwt;
end wbdqwfdvfs;



-- Seed after: 18083827919523303771,6140041381800297705
