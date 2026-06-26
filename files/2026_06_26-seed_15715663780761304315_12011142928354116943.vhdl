-- Seed: 15715663780761304315,12011142928354116943

library ieee;
use ieee.std_logic_1164.all;

entity titoxfb is
  port (ohpdjub : in std_logic; fkcmk : buffer string(5 to 2));
end titoxfb;

architecture l of titoxfb is
  
begin
  
end l;

entity pnt is
  port (akobqozwdj : in bit);
end pnt;

library ieee;
use ieee.std_logic_1164.all;

architecture wju of pnt is
  signal yzdaepfbf : string(5 to 2);
  signal wocuu : std_logic;
  signal rzt : string(5 to 2);
  signal yx : std_logic;
begin
  pejiplea : entity work.titoxfb
    port map (ohpdjub => yx, fkcmk => rzt);
  jumpwvddp : entity work.titoxfb
    port map (ohpdjub => wocuu, fkcmk => yzdaepfbf);
  
  -- Multi-driven assignments
  yx <= 'U';
end wju;

library ieee;
use ieee.std_logic_1164.all;

entity ueu is
  port (fbcdspsdz : inout std_logic; rhumoa : inout string(3 downto 1));
end ueu;

architecture prbbm of ueu is
  signal kxh : string(5 to 2);
  signal uwvexovdxj : string(5 to 2);
  signal mdxizxw : bit;
  signal kad : bit;
begin
  qstyicw : entity work.pnt
    port map (akobqozwdj => kad);
  uoawixiin : entity work.pnt
    port map (akobqozwdj => mdxizxw);
  mwx : entity work.titoxfb
    port map (ohpdjub => fbcdspsdz, fkcmk => uwvexovdxj);
  ogaxjvczs : entity work.titoxfb
    port map (ohpdjub => fbcdspsdz, fkcmk => kxh);
  
  -- Multi-driven assignments
  fbcdspsdz <= '0';
  fbcdspsdz <= '0';
end prbbm;

entity b is
  port (kzo : inout bit_vector(4 downto 1); k : buffer severity_level);
end b;

library ieee;
use ieee.std_logic_1164.all;

architecture u of b is
  signal buwbogaky : string(3 downto 1);
  signal oyoc : std_logic;
begin
  nanconh : entity work.ueu
    port map (fbcdspsdz => oyoc, rhumoa => buwbogaky);
  
  -- Single-driven assignments
  k <= WARNING;
  kzo <= ('0', '0', '1', '0');
  
  -- Multi-driven assignments
  oyoc <= 'L';
end u;



-- Seed after: 4025709450159779853,12011142928354116943
