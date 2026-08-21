-- Seed: 16983999046894035292,16188444798499499427

entity r is
  port (zkdzqyqcsm : linkage bit; obttghoi : in integer);
end r;

architecture t of r is
  
begin
  
end t;

library ieee;
use ieee.std_logic_1164.all;

entity o is
  port (rytbe : buffer bit; wqbwbz : linkage severity_level; brnpp : inout std_logic_vector(4 downto 3); fudpfwka : out boolean);
end o;

architecture qfyykee of o is
  signal nnfiqvdnm : integer;
begin
  dqeups : entity work.r
    port map (zkdzqyqcsm => rytbe, obttghoi => nnfiqvdnm);
  
  -- Single-driven assignments
  fudpfwka <= TRUE;
  nnfiqvdnm <= 2#111#;
  
  -- Multi-driven assignments
  brnpp <= brnpp;
  brnpp <= "X-";
  brnpp <= ('-', 'L');
  brnpp <= brnpp;
end qfyykee;

entity fpkpbyo is
  port (nshmodh : buffer time);
end fpkpbyo;

library ieee;
use ieee.std_logic_1164.all;

architecture ak of fpkpbyo is
  signal ngumed : boolean;
  signal rspcmfgy : std_logic_vector(4 downto 3);
  signal g : severity_level;
  signal s : bit;
begin
  hggeltso : entity work.o
    port map (rytbe => s, wqbwbz => g, brnpp => rspcmfgy, fudpfwka => ngumed);
  
  -- Single-driven assignments
  nshmodh <= nshmodh;
  
  -- Multi-driven assignments
  rspcmfgy <= ('U', 'X');
end ak;



-- Seed after: 14301345500980582977,16188444798499499427
