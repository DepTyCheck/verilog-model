-- Seed: 4591389196301142987,1834764876137802293

entity zznqyls is
  port (lcmvtuyc : out bit_vector(3 to 4); uokwwbekb : out real);
end zznqyls;

architecture b of zznqyls is
  
begin
  
end b;

library ieee;
use ieee.std_logic_1164.all;

entity cylocimwg is
  port (v : buffer real; awo : out std_logic_vector(4 to 4));
end cylocimwg;

architecture jtov of cylocimwg is
  signal ftufglo : bit_vector(3 to 4);
  signal cmjkmzwm : real;
  signal cubscsq : bit_vector(3 to 4);
begin
  lnorp : entity work.zznqyls
    port map (lcmvtuyc => cubscsq, uokwwbekb => cmjkmzwm);
  njmkcyxdtk : entity work.zznqyls
    port map (lcmvtuyc => ftufglo, uokwwbekb => v);
end jtov;

library ieee;
use ieee.std_logic_1164.all;

entity pyzxdhkx is
  port (qspqw : inout integer; djoiits : linkage integer; iaylfb : linkage time_vector(4 to 3); raw : inout std_logic_vector(4 to 4));
end pyzxdhkx;

architecture sphf of pyzxdhkx is
  signal evsoh : real;
  signal xhjxaemp : bit_vector(3 to 4);
  signal cnwquyj : real;
  signal swycxxgl : bit_vector(3 to 4);
begin
  toddkfnvfm : entity work.zznqyls
    port map (lcmvtuyc => swycxxgl, uokwwbekb => cnwquyj);
  sca : entity work.zznqyls
    port map (lcmvtuyc => xhjxaemp, uokwwbekb => evsoh);
  
  -- Single-driven assignments
  qspqw <= 1_3_3;
  
  -- Multi-driven assignments
  raw <= (others => '-');
  raw <= "U";
  raw <= "Z";
end sphf;



-- Seed after: 3195050313616060294,1834764876137802293
