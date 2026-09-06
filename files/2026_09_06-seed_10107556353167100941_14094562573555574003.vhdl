-- Seed: 10107556353167100941,14094562573555574003

library ieee;
use ieee.std_logic_1164.all;

entity i is
  port (gjnicf : out std_logic_vector(1 downto 1); mddz : in severity_level; auq : linkage time);
end i;

architecture dbe of i is
  
begin
  
end dbe;

entity vqoeaqkq is
  port (lctbhrsuk : in time_vector(4 downto 0));
end vqoeaqkq;

library ieee;
use ieee.std_logic_1164.all;

architecture ikkv of vqoeaqkq is
  signal iuzsj : time;
  signal abozxl : severity_level;
  signal swwcwc : std_logic_vector(1 downto 1);
begin
  cxcoirl : entity work.i
    port map (gjnicf => swwcwc, mddz => abozxl, auq => iuzsj);
  
  -- Single-driven assignments
  abozxl <= FAILURE;
  
  -- Multi-driven assignments
  swwcwc <= swwcwc;
  swwcwc <= "-";
  swwcwc <= (others => 'X');
end ikkv;

library ieee;
use ieee.std_logic_1164.all;

entity ooyuaqilu is
  port (blzmryoexg : buffer std_logic_vector(2 to 3));
end ooyuaqilu;

library ieee;
use ieee.std_logic_1164.all;

architecture clwvqdsypf of ooyuaqilu is
  signal dbizjkxlvl : time;
  signal bbxbuhlrsf : severity_level;
  signal mlr : std_logic_vector(1 downto 1);
  signal c : time;
  signal j : severity_level;
  signal o : time;
  signal cf : severity_level;
  signal cjqaea : std_logic_vector(1 downto 1);
  signal bzevui : time_vector(4 downto 0);
begin
  pcfqf : entity work.vqoeaqkq
    port map (lctbhrsuk => bzevui);
  cd : entity work.i
    port map (gjnicf => cjqaea, mddz => cf, auq => o);
  kea : entity work.i
    port map (gjnicf => cjqaea, mddz => j, auq => c);
  uxenuvl : entity work.i
    port map (gjnicf => mlr, mddz => bbxbuhlrsf, auq => dbizjkxlvl);
  
  -- Single-driven assignments
  cf <= ERROR;
  bbxbuhlrsf <= j;
  j <= NOTE;
  bzevui <= bzevui;
  
  -- Multi-driven assignments
  cjqaea <= cjqaea;
end clwvqdsypf;

library ieee;
use ieee.std_logic_1164.all;

entity e is
  port (oc : in std_logic; xmzm : out integer; qwdxz : out integer);
end e;

library ieee;
use ieee.std_logic_1164.all;

architecture cjlqh of e is
  signal vxueiv : std_logic_vector(2 to 3);
  signal rifjkxuaa : time;
  signal hat : severity_level;
  signal jibsxuz : std_logic_vector(1 downto 1);
begin
  uurnijutf : entity work.i
    port map (gjnicf => jibsxuz, mddz => hat, auq => rifjkxuaa);
  hkdlynqbu : entity work.ooyuaqilu
    port map (blzmryoexg => vxueiv);
  
  -- Multi-driven assignments
  jibsxuz <= jibsxuz;
  jibsxuz <= jibsxuz;
  jibsxuz <= jibsxuz;
  vxueiv <= ('W', '1');
end cjlqh;



-- Seed after: 12502666236975921969,14094562573555574003
