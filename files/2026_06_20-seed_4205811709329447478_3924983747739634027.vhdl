-- Seed: 4205811709329447478,3924983747739634027

entity iup is
  port (smicgbzw : out real);
end iup;

architecture aidm of iup is
  
begin
  -- Single-driven assignments
  smicgbzw <= 1.4_4;
end aidm;

library ieee;
use ieee.std_logic_1164.all;

entity bjvpo is
  port (rgqmolzor : out std_logic_vector(1 downto 1); ra : in std_logic; ymjc : linkage std_logic_vector(1 to 0); nmvacv : buffer real);
end bjvpo;

architecture samvj of bjvpo is
  signal nbdoaj : real;
begin
  u : entity work.iup
    port map (smicgbzw => nbdoaj);
  
  -- Multi-driven assignments
  rgqmolzor <= "1";
end samvj;

entity byg is
  port (gnapym : linkage boolean);
end byg;

library ieee;
use ieee.std_logic_1164.all;

architecture aoj of byg is
  signal nve : real;
  signal ey : real;
  signal alx : std_logic_vector(1 to 0);
  signal msoda : std_logic;
  signal tywjfndf : std_logic_vector(1 downto 1);
  signal jqjs : real;
begin
  ncthcncn : entity work.iup
    port map (smicgbzw => jqjs);
  ont : entity work.bjvpo
    port map (rgqmolzor => tywjfndf, ra => msoda, ymjc => alx, nmvacv => ey);
  cbl : entity work.iup
    port map (smicgbzw => nve);
  
  -- Multi-driven assignments
  alx <= (others => '0');
  tywjfndf <= (others => 'U');
  msoda <= 'Z';
  alx <= (others => '0');
end aoj;



-- Seed after: 7686524712147038758,3924983747739634027
