-- Seed: 17556377348896783930,14641901754878719179

library ieee;
use ieee.std_logic_1164.all;

entity lhwcwdw is
  port (nhoysbuu : buffer real; chspdqwp : in boolean_vector(4 downto 3); whk : linkage std_logic_vector(2 downto 0));
end lhwcwdw;

architecture iytf of lhwcwdw is
  
begin
  -- Single-driven assignments
  nhoysbuu <= 8#7634.04#;
end iytf;

entity mmkdg is
  port (dbbzpv : linkage severity_level; xigbzs : linkage real; qd : inout integer);
end mmkdg;

library ieee;
use ieee.std_logic_1164.all;

architecture n of mmkdg is
  signal bzu : std_logic_vector(2 downto 0);
  signal tltet : boolean_vector(4 downto 3);
  signal bisutkoge : real;
  signal mvpy : std_logic_vector(2 downto 0);
  signal ddtn : boolean_vector(4 downto 3);
  signal h : real;
begin
  tsvdmo : entity work.lhwcwdw
    port map (nhoysbuu => h, chspdqwp => ddtn, whk => mvpy);
  mviov : entity work.lhwcwdw
    port map (nhoysbuu => bisutkoge, chspdqwp => tltet, whk => bzu);
  
  -- Single-driven assignments
  tltet <= (TRUE, TRUE);
  ddtn <= (TRUE, TRUE);
  qd <= 8#401#;
  
  -- Multi-driven assignments
  bzu <= mvpy;
end n;



-- Seed after: 4823401520045984434,14641901754878719179
