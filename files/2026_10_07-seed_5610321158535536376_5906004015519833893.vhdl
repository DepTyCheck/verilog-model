-- Seed: 5610321158535536376,5906004015519833893

library ieee;
use ieee.std_logic_1164.all;

entity xsook is
  port (c : inout std_logic_vector(1 to 3));
end xsook;

architecture dthhedz of xsook is
  
begin
  -- Multi-driven assignments
  c <= ('1', 'Z', 'W');
end dthhedz;

entity tl is
  port (bbbsieh : in severity_level; asaif : inout time; sg : in real);
end tl;

library ieee;
use ieee.std_logic_1164.all;

architecture bu of tl is
  signal hiyntxdx : std_logic_vector(1 to 3);
  signal omthmo : std_logic_vector(1 to 3);
begin
  rropm : entity work.xsook
    port map (c => omthmo);
  uzd : entity work.xsook
    port map (c => hiyntxdx);
  
  -- Multi-driven assignments
  omthmo <= "01Z";
  omthmo <= omthmo;
  omthmo <= omthmo;
  hiyntxdx <= ('W', '0', 'H');
end bu;

entity yzc is
  port (c : out boolean_vector(4 downto 1); ibxc : linkage time);
end yzc;

library ieee;
use ieee.std_logic_1164.all;

architecture guhrhe of yzc is
  signal qfhnhtdcfn : std_logic_vector(1 to 3);
  signal uscvfbktu : std_logic_vector(1 to 3);
begin
  cmzeggtikw : entity work.xsook
    port map (c => uscvfbktu);
  nkl : entity work.xsook
    port map (c => qfhnhtdcfn);
  fypydo : entity work.xsook
    port map (c => uscvfbktu);
  
  -- Multi-driven assignments
  uscvfbktu <= ('1', 'L', '0');
end guhrhe;



-- Seed after: 13062927804961015991,5906004015519833893
