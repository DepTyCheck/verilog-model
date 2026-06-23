-- Seed: 5298594000778562483,8421704836678237495

entity dwcb is
  port (cgni : buffer bit_vector(4 downto 4));
end dwcb;

architecture ys of dwcb is
  
begin
  -- Single-driven assignments
  cgni <= (others => '0');
end ys;

library ieee;
use ieee.std_logic_1164.all;

entity zduujqwmot is
  port (otxkmlg : out std_logic_vector(1 to 2));
end zduujqwmot;

architecture xtnywha of zduujqwmot is
  signal kgsy : bit_vector(4 downto 4);
begin
  ymlgycde : entity work.dwcb
    port map (cgni => kgsy);
  
  -- Multi-driven assignments
  otxkmlg <= ('1', '0');
end xtnywha;

entity gknhbp is
  port (pf : buffer time; aitnwxxl : inout time);
end gknhbp;

architecture ywhaswww of gknhbp is
  signal udvgysttmr : bit_vector(4 downto 4);
  signal wjaqc : bit_vector(4 downto 4);
  signal tre : bit_vector(4 downto 4);
  signal s : bit_vector(4 downto 4);
begin
  p : entity work.dwcb
    port map (cgni => s);
  akyrulc : entity work.dwcb
    port map (cgni => tre);
  ac : entity work.dwcb
    port map (cgni => wjaqc);
  d : entity work.dwcb
    port map (cgni => udvgysttmr);
  
  -- Single-driven assignments
  aitnwxxl <= 0 fs;
  pf <= 2_0.4134 ms;
end ywhaswww;

entity fpjyku is
  port (qeupbus : inout integer; h : buffer severity_level; ghnlm : buffer character);
end fpjyku;

library ieee;
use ieee.std_logic_1164.all;

architecture ckef of fpjyku is
  signal xbl : std_logic_vector(1 to 2);
  signal wjqzykex : bit_vector(4 downto 4);
  signal b : bit_vector(4 downto 4);
begin
  zbupzlmig : entity work.dwcb
    port map (cgni => b);
  rha : entity work.dwcb
    port map (cgni => wjqzykex);
  bs : entity work.zduujqwmot
    port map (otxkmlg => xbl);
  
  -- Single-driven assignments
  ghnlm <= 'y';
  qeupbus <= 8#00710#;
  h <= WARNING;
  
  -- Multi-driven assignments
  xbl <= ('L', 'X');
  xbl <= ('X', '0');
end ckef;



-- Seed after: 18130628392385470231,8421704836678237495
