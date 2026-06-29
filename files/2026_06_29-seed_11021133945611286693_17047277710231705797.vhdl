-- Seed: 11021133945611286693,17047277710231705797

library ieee;
use ieee.std_logic_1164.all;

entity hy is
  port (veocjpudo : in std_logic; yfoezdyv : inout severity_level; iquqv : in std_logic_vector(4 to 2); nc : linkage time_vector(4 downto 2));
end hy;

architecture exhltqojst of hy is
  
begin
  -- Single-driven assignments
  yfoezdyv <= FAILURE;
end exhltqojst;

entity u is
  port (tmpsd : linkage time);
end u;

library ieee;
use ieee.std_logic_1164.all;

architecture izrofsk of u is
  signal crpz : time_vector(4 downto 2);
  signal ejbovrkh : severity_level;
  signal lwq : time_vector(4 downto 2);
  signal vltoicy : severity_level;
  signal zf : std_logic;
  signal zvigszxxd : time_vector(4 downto 2);
  signal acgeks : std_logic_vector(4 to 2);
  signal wa : severity_level;
  signal c : std_logic;
begin
  p : entity work.hy
    port map (veocjpudo => c, yfoezdyv => wa, iquqv => acgeks, nc => zvigszxxd);
  clcgca : entity work.hy
    port map (veocjpudo => zf, yfoezdyv => vltoicy, iquqv => acgeks, nc => lwq);
  dvovk : entity work.hy
    port map (veocjpudo => zf, yfoezdyv => ejbovrkh, iquqv => acgeks, nc => crpz);
  
  -- Multi-driven assignments
  c <= 'Z';
  zf <= '0';
  zf <= 'W';
  zf <= 'Z';
end izrofsk;

library ieee;
use ieee.std_logic_1164.all;

entity tisixa is
  port (wd : in std_logic; pwqmns : buffer real; d : buffer real);
end tisixa;

architecture hmpqjfwkp of tisixa is
  
begin
  -- Single-driven assignments
  pwqmns <= 2#1.010#;
  d <= 16#F83.C#;
end hmpqjfwkp;

entity qzfnlgtd is
  port (uargogya : buffer real; rmslhzbb : linkage real; iigio : inout boolean_vector(4 to 1));
end qzfnlgtd;

library ieee;
use ieee.std_logic_1164.all;

architecture nfpglgxmb of qzfnlgtd is
  signal r : time_vector(4 downto 2);
  signal l : std_logic_vector(4 to 2);
  signal jgc : severity_level;
  signal s : std_logic;
begin
  sjgpkiks : entity work.hy
    port map (veocjpudo => s, yfoezdyv => jgc, iquqv => l, nc => r);
  
  -- Single-driven assignments
  iigio <= (others => TRUE);
  uargogya <= 21.2;
  
  -- Multi-driven assignments
  l <= (others => '0');
  l <= "";
  l <= (others => '0');
  s <= 'X';
end nfpglgxmb;



-- Seed after: 2570306273137882814,17047277710231705797
