-- Seed: 2918552588714746297,10463297573877745897

entity zbqwvkwaoo is
  port (i : inout real_vector(1 downto 1); oquozq : buffer integer_vector(3 to 4));
end zbqwvkwaoo;

architecture hhshgwcu of zbqwvkwaoo is
  
begin
  -- Single-driven assignments
  i <= (others => 16#A.6F#);
end hhshgwcu;

library ieee;
use ieee.std_logic_1164.all;

entity bppscku is
  port (mrwuvok : linkage std_logic_vector(4 to 4); p : out integer; glmssvlonv : out bit);
end bppscku;

architecture nixzynal of bppscku is
  signal vaqwywinqq : integer_vector(3 to 4);
  signal yrhnvuzfkz : real_vector(1 downto 1);
  signal uvfxtrr : integer_vector(3 to 4);
  signal aulkhf : real_vector(1 downto 1);
  signal uosx : integer_vector(3 to 4);
  signal kwdytvuq : real_vector(1 downto 1);
begin
  pfqzexs : entity work.zbqwvkwaoo
    port map (i => kwdytvuq, oquozq => uosx);
  xojmujcsrf : entity work.zbqwvkwaoo
    port map (i => aulkhf, oquozq => uvfxtrr);
  ozxhgkhz : entity work.zbqwvkwaoo
    port map (i => yrhnvuzfkz, oquozq => vaqwywinqq);
end nixzynal;

library ieee;
use ieee.std_logic_1164.all;

entity trh is
  port (ogo : linkage severity_level; tcgcigrr : linkage time_vector(3 downto 1); nnxv : inout std_logic; ooclvxejsh : linkage real);
end trh;

library ieee;
use ieee.std_logic_1164.all;

architecture iddoll of trh is
  signal bmsumrnzg : bit;
  signal rretew : integer;
  signal hryqeg : std_logic_vector(4 to 4);
  signal v : integer_vector(3 to 4);
  signal dxjuxvbq : real_vector(1 downto 1);
  signal ieep : integer_vector(3 to 4);
  signal fwic : real_vector(1 downto 1);
begin
  hcqcltuf : entity work.zbqwvkwaoo
    port map (i => fwic, oquozq => ieep);
  cdxgdf : entity work.zbqwvkwaoo
    port map (i => dxjuxvbq, oquozq => v);
  xlfxc : entity work.bppscku
    port map (mrwuvok => hryqeg, p => rretew, glmssvlonv => bmsumrnzg);
  
  -- Multi-driven assignments
  hryqeg <= hryqeg;
  nnxv <= 'W';
  nnxv <= 'Z';
  hryqeg <= hryqeg;
end iddoll;



-- Seed after: 4379487957338290367,10463297573877745897
